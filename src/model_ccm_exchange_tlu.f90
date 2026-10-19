!===============================================================
! model_ccm_exchange_tlu.f90
! CCMExchangeTLU: money-conserving CCM exchange with threshold-logic-unit
! (TLU) saving-propensity policies. Retains the pair-selection and 2x2
! wealth-exchange rule of CCMExchange; only the source of each agent's
! saving propensity changes (fixed, random, heuristic, fixed-TLU, or
! online-learning stochastic-TLU policies).
!
! Timestep order per exchange step:
!   1. Pairing: shuffle indices into disjoint pairs (odd agent unpaired).
!   2. Randomness: one exchange epsilon per pair plus separate policy
!      uniforms, all drawn before any decision (no shared mutable RNG
!      state inside the pair loop).
!   3. Observe and decide from pre-trade wealth and prior reward_ema only
!      (no look-ahead); cache pre-trade features for learning.
!   4. Exchange with the unchanged CCM 2x2 transition.
!   5. Reward and update (tlu_learning only updates weights; every mode
!      maintains the reward EMA diagnostic).
!   6. Diagnostics: rejected-trade and mean-absolute-reward aggregates.
!
! RNG streams: matchmaking uses the intrinsic RNG (seeded by init_rng,
! exactly as in CCMExchange); exchange epsilons use this%rng/this%prng
! (same seeding as CCMExchange, so ccm_fixed trajectories match the
! reference model); policy initialization and exploration use a separate
! policy_rng instance derived deterministically from the nonzero seed,
! so policy draws never consume the matchmaking/epsilon sequences.
!===============================================================
module model_ccm_exchange_tlu_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use sim_base_m
    use abm_metrics_m
    use omp_lib

    implicit none
    public ! public by default, as in the other model modules

    ! Number of TLU features (bias + own wealth + partner wealth + memory)
    integer, parameter :: TLU_NFEATURES = 4
    ! Clip bound for the sigmoid argument (numerical stability)
    real(rk), parameter :: TLU_SIGMOID_CLIP = 30.0_rk

    ! Single agent with a threshold-logic-unit saving policy
    type :: Agent
        real(rk) :: cash = 0.0_rk
        ! Saving propensity actually used in the most recent exchange
        real(rk) :: saving_propensity = 0.0_rk
        ! TLU weights (weights(1) is the learnable bias weight)
        real(rk) :: weights(TLU_NFEATURES) = 0.0_rk
        ! Exponentially weighted average of prior normalized trade rewards
        real(rk) :: reward_ema = 0.0_rk
        ! 0 = low saving, 1 = high saving (-1 = inapplicable, ccm_fixed only)
        integer :: last_action = 0
        real(rk) :: last_policy_probability = 0.5_rk
        real(rk) :: last_reward = 0.0_rk
        ! Fixed baseline values (used by ccm_fixed / binary_fixed)
        real(rk) :: fixed_saving_propensity = 0.0_rk
        integer :: fixed_action = 0
    end type Agent

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: CCMExchangeTLU
        type(Agent), allocatable :: pop(:)
        character(len=32), allocatable :: names(:)  ! Metric names
        real(rk) :: debt_limit = 0.0_rk
        real(rk) :: exchange_delta = 1.0_rk
        real(rk) :: min_saving_propensity = 0.1_rk
        real(rk) :: max_saving_propensity = 0.9_rk
        character(len=32) :: policy_mode = 'tlu_learning'
        real(rk) :: lambda_low = 0.2_rk
        real(rk) :: lambda_high = 0.8_rk
        real(rk) :: learning_rate = 0.01_rk
        real(rk) :: policy_temperature = 1.0_rk
        real(rk) :: weight_init_scale = 0.1_rk
        real(rk) :: weight_clip = 5.0_rk
        real(rk) :: reward_clip = 2.0_rk
        real(rk) :: memory_decay = 0.9_rk
        logical :: use_partner_wealth = .true.
        logical :: use_memory = .true.
        ! Constant population mean cash (conserved by the exchange rule)
        real(rk) :: mean_cash = 1.0_rk
        ! Most-recent-step diagnostics reported through compute_metrics()
        real(rk) :: last_rejected_fraction = 0.0_rk
        real(rk) :: last_mean_abs_reward = 0.0_rk
        ! Separated RNG stream for policy init/exploration (see header)
        type(RNG_t) :: policy_rng
    contains
        ! Override the abstract methods
        procedure :: init
        procedure :: step
        procedure :: write_population_csv
        procedure :: compute_metrics
        procedure :: get_metric_names
    end type CCMExchangeTLU

contains

    !------------------------------------------------------------
    ! Pure TLU / exchange helpers (unit-tested directly).
    !------------------------------------------------------------

    ! Clip a value into [lo, hi]
    pure elemental function tlu_clip(v, lo, hi) result(c)
        real(rk), intent(in) :: v, lo, hi
        real(rk) :: c

        c = min(max(v, lo), hi)
    end function tlu_clip

    ! Logistic sigmoid with a clipped argument for numerical stability
    pure function tlu_sigmoid(z) result(p)
        real(rk), intent(in) :: z
        real(rk) :: p

        p = 1.0_rk / (1.0_rk + exp(-tlu_clip(z, -TLU_SIGMOID_CLIP, TLU_SIGMOID_CLIP)))
    end function tlu_sigmoid

    ! Build the pre-trade TLU feature vector for agent i paired with j.
    ! x(1) = 1 (bias); x(2) = own-wealth deviation; x(3) = partner-wealth
    ! deviation (zeroed when use_partner_wealth is off); x(4) = memory
    ! (zeroed when use_memory is off). reward_ema_in must hold only
    ! information from earlier exchanges (no look-ahead).
    pure subroutine tlu_build_features(cash_i, cash_j, mean_cash, reward_ema_in, &
                                       use_partner_wealth, use_memory, x)
        real(rk), intent(in) :: cash_i, cash_j, mean_cash, reward_ema_in
        logical, intent(in) :: use_partner_wealth, use_memory
        real(rk), intent(out) :: x(TLU_NFEATURES)

        x(1) = 1.0_rk
        x(2) = tanh((cash_i - mean_cash) / mean_cash)
        if (use_partner_wealth) then
            x(3) = tanh((cash_j - mean_cash) / mean_cash)
        else
            x(3) = 0.0_rk
        end if
        if (use_memory) then
            x(4) = tanh(reward_ema_in)
        else
            x(4) = 0.0_rk
        end if
    end subroutine tlu_build_features

    ! Deterministic hard-threshold TLU action: 1 if z >= 0, else 0
    pure function tlu_hard_action(weights, x) result(a)
        real(rk), intent(in) :: weights(TLU_NFEATURES), x(TLU_NFEATURES)
        integer :: a

        if (dot_product(weights, x) >= 0.0_rk) then
            a = 1
        else
            a = 0
        end if
    end function tlu_hard_action

    ! Stochastic-threshold (Bernoulli) policy probability p = sigmoid(z/T)
    pure function tlu_action_probability(weights, x, temperature) result(p)
        real(rk), intent(in) :: weights(TLU_NFEATURES), x(TLU_NFEATURES)
        real(rk), intent(in) :: temperature
        real(rk) :: p

        p = tlu_sigmoid(dot_product(weights, x) / temperature)
    end function tlu_action_probability

    ! Sample a Bernoulli action: 1 with probability p given uniform u
    pure function tlu_sample_action(p, u) result(a)
        real(rk), intent(in) :: p, u
        integer :: a

        if (u < p) then
            a = 1
        else
            a = 0
        end if
    end function tlu_sample_action

    ! CCM pairwise wealth update (pool form, algebraically the 2x2
    ! transition used by CCMExchange):
    !   pool = (1-li)*mi + (1-lj)*mj
    !   mi' = li*mi + eps*pool ; mj' = lj*mj + (1-eps)*pool
    pure subroutine ccm_pair_update(m_i, m_j, lambda_i, lambda_j, eps, m_i_new, m_j_new)
        real(rk), intent(in) :: m_i, m_j, lambda_i, lambda_j, eps
        real(rk), intent(out) :: m_i_new, m_j_new
        real(rk) :: pool

        pool = (1.0_rk - lambda_i) * m_i + (1.0_rk - lambda_j) * m_j
        m_i_new = lambda_i * m_i + eps * pool
        m_j_new = lambda_j * m_j + (1.0_rk - eps) * pool
    end subroutine ccm_pair_update

    ! Bernoulli score-function (REINFORCE) update for the logistic
    ! threshold policy with a reward baseline, weights clipped each:
    !   w = clip_each(w + lr*advantage*(a - p)*x/T, -wclip, +wclip)
    pure subroutine tlu_reinforce_update(weights, x, action, prob, advantage, &
                                         learning_rate, temperature, weight_clip)
        real(rk), intent(inout) :: weights(TLU_NFEATURES)
        real(rk), intent(in) :: x(TLU_NFEATURES)
        integer, intent(in) :: action
        real(rk), intent(in) :: prob, advantage, learning_rate, temperature, weight_clip
        integer :: k

        do k = 1, TLU_NFEATURES
            weights(k) = tlu_clip(weights(k) + learning_rate * advantage * &
                                  (real(action, rk) - prob) * x(k) / temperature, &
                                  -weight_clip, weight_clip)
        end do
    end subroutine tlu_reinforce_update

    ! Top wealth share of the richest fraction frac of the population.
    ! Always defined (at least the single richest agent); for very small
    ! populations the top-1% share coincides with the top-10% share.
    function top_wealth_share(cash_values, frac) result(share)
        real(rk), intent(in) :: cash_values(:)
        real(rk), intent(in) :: frac
        real(rk) :: share
        real(rk), allocatable :: cash_sorted(:)
        real(rk) :: total
        integer :: n, k

        n = size(cash_values)
        allocate (cash_sorted(n))
        cash_sorted = cash_values
        call sort_real_array(cash_sorted)
        total = sum(cash_sorted)
        if (total <= 0.0_rk) then
            share = 0.0_rk
            return
        end if
        k = max(1, nint(frac * real(n, rk)))
        k = min(k, n)
        share = sum(cash_sorted(n - k + 1:n)) / total
    end function top_wealth_share

    !------------------------------------------------------------
    subroutine init(this, cfg)
        class(CCMExchangeTLU), intent(out) :: this
        class(AbstractConfig), intent(in)  :: cfg
        integer(int64) :: i
        real(rk) :: u, w_init(TLU_NFEATURES)
        real(rk) :: x_init(TLU_NFEATURES), p_init
        integer :: a_init, k

        select type (cfg)
        type is (Config_CCMExchangeTLU)
            ! Fail early on unknown policy modes / invalid combinations
            call cfg % validate()

            ! Intrinsic-RNG stream: matchmaking + baseline sampling, exactly
            ! as in CCMExchange so ccm_fixed reproduces the reference model.
            call init_rng(cfg % seed)
            allocate (this % pop(cfg % n_agents))

            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % debt_limit = cfg % debt_limit
            this % exchange_delta = cfg % exchange_delta
            this % min_saving_propensity = cfg % min_saving_propensity
            this % max_saving_propensity = cfg % max_saving_propensity
            this % policy_mode = cfg % policy_mode
            this % lambda_low = cfg % lambda_low
            this % lambda_high = cfg % lambda_high
            this % learning_rate = cfg % learning_rate
            this % policy_temperature = cfg % policy_temperature
            this % weight_init_scale = cfg % weight_init_scale
            this % weight_clip = cfg % weight_clip
            this % reward_clip = cfg % reward_clip
            this % memory_decay = cfg % memory_decay
            this % use_partner_wealth = cfg % use_partner_wealth
            this % use_memory = cfg % use_memory
            this % mean_cash = cfg % init_cash
            this % last_rejected_fraction = 0.0_rk
            this % last_mean_abs_reward = 0.0_rk

            ! Separated policy stream, derived deterministically from the
            ! nonzero seed; never touches the matchmaking/epsilon sequences.
            if (cfg % seed == 0) then
                call this % policy_rng % set_random_seed()
            else
                call this % policy_rng % set_seed( &
                    [ieor(cfg % seed, 5276461642469775544_int64), &
                     ieor(cfg % seed, -7046029254386353131_int64)])
            end if

            ! Exchange-epsilon stream, seeded exactly as in CCMExchange.
            if (cfg % seed == 0) then
                call this % rng % set_random_seed()
            else
                call this % rng % set_seed( &
                    [cfg % seed, ieor(cfg % seed, -7046029254386353131_int64)])
            end if
#ifdef _OPENMP
            call this % prng % init_parallel(omp_get_max_threads(), this % rng)
#else
            call this % prng % init_parallel(1, this % rng)
#endif

            ! First pass: cash, diagnostics, and TLU weights. All weight
            ! draws happen here so tlu_fixed and tlu_learning start from
            ! identical weights under the same seed and policy stream.
            do i = 1, cfg % n_agents
                this % pop(i) % cash = cfg % init_cash
                this % pop(i) % reward_ema = 0.0_rk
                this % pop(i) % last_reward = 0.0_rk
                this % pop(i) % weights = 0.0_rk
                if (trim(this % policy_mode) == 'tlu_fixed' .or. &
                    trim(this % policy_mode) == 'tlu_learning') then
                    do k = 1, TLU_NFEATURES
                        u = this % policy_rng % unif_01()
                        w_init(k) = (2.0_rk * u - 1.0_rk) * this % weight_init_scale
                    end do
                    this % pop(i) % weights = w_init
                end if
            end do

            ! Second pass: initial saving propensity / action per policy.
            ! ccm_fixed samples exactly like CCMExchange (one intrinsic
            ! uniform per agent in agent order).
            do i = 1, cfg % n_agents
                select case (trim(this % policy_mode))
                case ('ccm_fixed')
                    call random_number(u)
                    this % pop(i) % saving_propensity = &
                        u * (cfg % max_saving_propensity - cfg % min_saving_propensity) + &
                        cfg % min_saving_propensity
                    this % pop(i) % fixed_saving_propensity = this % pop(i) % saving_propensity
                    this % pop(i) % fixed_action = -1
                    this % pop(i) % last_action = -1
                    this % pop(i) % last_policy_probability = -1.0_rk
                case ('binary_fixed')
                    call random_number(u)
                    if (u < 0.5_rk) then
                        a_init = 1
                    else
                        a_init = 0
                    end if
                    this % pop(i) % fixed_action = a_init
                    this % pop(i) % last_action = a_init
                    this % pop(i) % last_policy_probability = real(a_init, rk)
                    this % pop(i) % saving_propensity = &
                        this % lambda_low + real(a_init, rk) * (this % lambda_high - this % lambda_low)
                    this % pop(i) % fixed_saving_propensity = this % pop(i) % saving_propensity
                case ('binary_random')
                    this % pop(i) % last_action = 0
                    this % pop(i) % last_policy_probability = 0.5_rk
                    this % pop(i) % saving_propensity = this % lambda_low
                case ('heuristic')
                    ! At equal initial wealth nobody is below the mean.
                    this % pop(i) % last_action = 0
                    this % pop(i) % last_policy_probability = 0.0_rk
                    this % pop(i) % saving_propensity = this % lambda_low
                case ('tlu_fixed')
                    call tlu_build_features(cfg % init_cash, cfg % init_cash, this % mean_cash, &
                                            0.0_rk, this % use_partner_wealth, this % use_memory, x_init)
                    a_init = tlu_hard_action(this % pop(i) % weights, x_init)
                    this % pop(i) % last_action = a_init
                    this % pop(i) % last_policy_probability = real(a_init, rk)
                    this % pop(i) % saving_propensity = &
                        this % lambda_low + real(a_init, rk) * (this % lambda_high - this % lambda_low)
                case ('tlu_learning')
                    call tlu_build_features(cfg % init_cash, cfg % init_cash, this % mean_cash, &
                                            0.0_rk, this % use_partner_wealth, this % use_memory, x_init)
                    p_init = tlu_action_probability(this % pop(i) % weights, x_init, &
                                                    this % policy_temperature)
                    u = this % policy_rng % unif_01()
                    a_init = tlu_sample_action(p_init, u)
                    this % pop(i) % last_action = a_init
                    this % pop(i) % last_policy_probability = p_init
                    this % pop(i) % saving_propensity = &
                        this % lambda_low + real(a_init, rk) * (this % lambda_high - this % lambda_low)
                end select
            end do

            allocate (this % names(9))
            this % names(1) = 'gini_coefficient'
            this % names(2) = 'top_10pct_wealth_share'
            this % names(3) = 'top_1pct_wealth_share'
            this % names(4) = 'mean_saving_propensity'
            this % names(5) = 'high_action_fraction'
            this % names(6) = 'mean_abs_reward'
            this % names(7) = 'rejected_trade_fraction'
            this % names(8) = 'total_wealth'
            this % names(9) = 'mean_weight_norm'

        class default
            error stop 'Unsupported config type in CCMExchangeTLU init'
        end select
    end subroutine init

    !------------------------------------------------------------
    ! One exchange step: pair, decide from pre-trade state, trade
    ! with the unchanged CCM 2x2 transition, then learn (tlu_learning).
    subroutine step(this)
        class(CCMExchangeTLU), intent(inout) :: this
        integer :: n, n_pairs, p, i, j, k
        integer, allocatable :: perm(:)
        real(rk), allocatable :: eps(:), policy_u(:)
        real(rk) :: x_i(TLU_NFEATURES), x_j(TLU_NFEATURES)
        real(rk) :: m_i, m_j, lambda_i, lambda_j, b_i, b_j
        real(rk) :: p_i, p_j, r_i, r_j, adv_i, adv_j
        real(rk) :: exchange_matrix(2, 2), exchange_result(2)
        integer :: a_i, a_j, n_rejected
        real(rk) :: sum_abs_reward
        logical :: accepted, need_policy_draws
        character(len=32) :: mode

        mode = this % policy_mode
        n = size(this % pop)
        n_pairs = n / 2

        ! 1. Pairing (same Fisher-Yates shuffle as CCMExchange)
        allocate (perm(n))
        perm = [(k, k=1, n)]
        call shuffle_int(perm)

        ! 2. Randomness up front: epsilons on the exchange stream and,
        ! only for stochastic policies, policy uniforms on the separate
        ! policy stream. No RNG calls inside the pair loop.
        allocate (eps(n_pairs))
        call parallel_random_number(n_pairs, eps, this % rng, this % prng)
        need_policy_draws = (trim(mode) == 'binary_random' .or. trim(mode) == 'tlu_learning')
        if (need_policy_draws) then
            allocate (policy_u(2 * n_pairs))
            do k = 1, 2 * n_pairs
                policy_u(k) = this % policy_rng % unif_01()
            end do
        end if

        n_rejected = 0
        sum_abs_reward = 0.0_rk

        ! 3-5. Observe/decide, exchange, reward/update per pair.
        ! Serial loop: deterministic and reproducible on CPU.
        do p = 1, n_pairs
            i = perm(2 * p - 1)
            j = perm(2 * p)

            m_i = this % pop(i) % cash
            m_j = this % pop(j) % cash
            b_i = this % pop(i) % reward_ema
            b_j = this % pop(j) % reward_ema

            ! 3. Observe (pre-trade only) and decide.
            call tlu_build_features(m_i, m_j, this % mean_cash, b_i, &
                                    this % use_partner_wealth, this % use_memory, x_i)
            call tlu_build_features(m_j, m_i, this % mean_cash, b_j, &
                                    this % use_partner_wealth, this % use_memory, x_j)
            select case (trim(mode))
            case ('ccm_fixed')
                lambda_i = this % pop(i) % saving_propensity
                lambda_j = this % pop(j) % saving_propensity
                a_i = this % pop(i) % last_action
                a_j = this % pop(j) % last_action
                p_i = this % pop(i) % last_policy_probability
                p_j = this % pop(j) % last_policy_probability
            case ('binary_fixed')
                lambda_i = this % pop(i) % saving_propensity
                lambda_j = this % pop(j) % saving_propensity
                a_i = this % pop(i) % fixed_action
                a_j = this % pop(j) % fixed_action
                p_i = real(a_i, rk)
                p_j = real(a_j, rk)
            case ('binary_random')
                a_i = tlu_sample_action(0.5_rk, policy_u(2 * p - 1))
                a_j = tlu_sample_action(0.5_rk, policy_u(2 * p))
                p_i = 0.5_rk
                p_j = 0.5_rk
                lambda_i = this % lambda_low + real(a_i, rk) * (this % lambda_high - this % lambda_low)
                lambda_j = this % lambda_low + real(a_j, rk) * (this % lambda_high - this % lambda_low)
            case ('heuristic')
                ! High saving when own pre-trade wealth is below the
                ! population initial mean; low saving otherwise.
                if (m_i < this % mean_cash) then
                    a_i = 1
                else
                    a_i = 0
                end if
                if (m_j < this % mean_cash) then
                    a_j = 1
                else
                    a_j = 0
                end if
                p_i = real(a_i, rk)
                p_j = real(a_j, rk)
                lambda_i = this % lambda_low + real(a_i, rk) * (this % lambda_high - this % lambda_low)
                lambda_j = this % lambda_low + real(a_j, rk) * (this % lambda_high - this % lambda_low)
            case ('tlu_fixed')
                a_i = tlu_hard_action(this % pop(i) % weights, x_i)
                a_j = tlu_hard_action(this % pop(j) % weights, x_j)
                p_i = real(a_i, rk)
                p_j = real(a_j, rk)
                lambda_i = this % lambda_low + real(a_i, rk) * (this % lambda_high - this % lambda_low)
                lambda_j = this % lambda_low + real(a_j, rk) * (this % lambda_high - this % lambda_low)
            case ('tlu_learning')
                p_i = tlu_action_probability(this % pop(i) % weights, x_i, this % policy_temperature)
                p_j = tlu_action_probability(this % pop(j) % weights, x_j, this % policy_temperature)
                a_i = tlu_sample_action(p_i, policy_u(2 * p - 1))
                a_j = tlu_sample_action(p_j, policy_u(2 * p))
                lambda_i = this % lambda_low + real(a_i, rk) * (this % lambda_high - this % lambda_low)
                lambda_j = this % lambda_low + real(a_j, rk) * (this % lambda_high - this % lambda_low)
            case default
                error stop 'CCMExchangeTLU: unknown policy_mode in step'
            end select

            ! 4. Exchange with the unchanged CCM 2x2 transition matrix
            ! (same arithmetic as CCMExchange for reference parity).
            exchange_matrix(1, 1) = lambda_i + eps(p) * (1.0_rk - lambda_i)
            exchange_matrix(1, 2) = eps(p) * (1.0_rk - lambda_j)
            exchange_matrix(2, 1) = (1.0_rk - eps(p)) * (1.0_rk - lambda_i)
            exchange_matrix(2, 2) = lambda_j + (1.0_rk - eps(p)) * (1.0_rk - lambda_j)
            exchange_result = apply_exchange_matrix(m_i, m_j, exchange_matrix)

            ! Retain the corrected model's debt-limit / rejected-exchange
            ! semantics.
            accepted = .not. ((exchange_result(1) < -this % debt_limit + this % exchange_delta) .or. &
                              (exchange_result(2) < -this % debt_limit + this % exchange_delta))

            if (accepted) then
                ! 5a. Reward and update for a valid exchange.
                this % pop(i) % cash = exchange_result(1)
                this % pop(j) % cash = exchange_result(2)
                r_i = tlu_clip((exchange_result(1) - m_i) / this % mean_cash, &
                               -this % reward_clip, this % reward_clip)
                r_j = tlu_clip((exchange_result(2) - m_j) / this % mean_cash, &
                               -this % reward_clip, this % reward_clip)
                if (trim(mode) == 'tlu_learning') then
                    adv_i = r_i - b_i
                    adv_j = r_j - b_j
                    call tlu_reinforce_update(this % pop(i) % weights, x_i, a_i, p_i, adv_i, &
                                              this % learning_rate, this % policy_temperature, &
                                              this % weight_clip)
                    call tlu_reinforce_update(this % pop(j) % weights, x_j, a_j, p_j, adv_j, &
                                              this % learning_rate, this % policy_temperature, &
                                              this % weight_clip)
                end if
                this % pop(i) % reward_ema = this % memory_decay * b_i + (1.0_rk - this % memory_decay) * r_i
                this % pop(j) % reward_ema = this % memory_decay * b_j + (1.0_rk - this % memory_decay) * r_j
                this % pop(i) % last_reward = r_i
                this % pop(j) % last_reward = r_j
                sum_abs_reward = sum_abs_reward + abs(r_i) + abs(r_j)
            else
                ! 5b. Rejected trade: retain pre-trade cash, skip learning,
                ! and decay the EMA toward zero (realized reward is zero).
                n_rejected = n_rejected + 1
                this % pop(i) % reward_ema = this % memory_decay * b_i
                this % pop(j) % reward_ema = this % memory_decay * b_j
                this % pop(i) % last_reward = 0.0_rk
                this % pop(j) % last_reward = 0.0_rk
            end if

            ! 6. Record the selected saving propensity / action diagnostics.
            if (trim(mode) /= 'ccm_fixed' .and. trim(mode) /= 'binary_fixed') then
                this % pop(i) % saving_propensity = lambda_i
                this % pop(j) % saving_propensity = lambda_j
                this % pop(i) % last_action = a_i
                this % pop(j) % last_action = a_j
                this % pop(i) % last_policy_probability = p_i
                this % pop(j) % last_policy_probability = p_j
            else if (trim(mode) == 'binary_fixed') then
                this % pop(i) % last_action = a_i
                this % pop(j) % last_action = a_j
                this % pop(i) % last_policy_probability = p_i
                this % pop(j) % last_policy_probability = p_j
            end if
        end do
        ! Agents left unpaired in an odd-sized population retain state.

        ! 6. Step-level diagnostics.
        this % last_rejected_fraction = real(n_rejected, rk) / real(max(n_pairs, 1), rk)
        this % last_mean_abs_reward = sum_abs_reward / real(max(2 * n_pairs, 1), rk)
    end subroutine step

    !------------------------------------------------------------
    subroutine write_population_csv(this, filename)
        class(CCMExchangeTLU), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, i, ios

        open (newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening file: ', filename
            return
        end if

        ! Single physical CSV header line
        write (unit, '(a)') 'agent_id,cash,saving_propensity,last_action,'// &
            'last_policy_probability,last_reward,reward_ema,w0,w1,w2,w3'

        ! Write agent data
        do i = 1, size(this % pop)
            write (unit, '(i0,",",es23.15,",",es23.15,",",i0,",",es23.15,",",es23.15,'// &
                '",",es23.15,",",es23.15,",",es23.15,",",es23.15,",",es23.15)') &
                i, this % pop(i) % cash, this % pop(i) % saving_propensity, &
                this % pop(i) % last_action, this % pop(i) % last_policy_probability, &
                this % pop(i) % last_reward, this % pop(i) % reward_ema, &
                this % pop(i) % weights(1), this % pop(i) % weights(2), &
                this % pop(i) % weights(3), this % pop(i) % weights(4)
        end do

        close (unit)
        write (*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(CCMExchangeTLU), intent(in) :: this
        real(rk), allocatable :: metrics(:)
        real(rk), allocatable :: cash_list(:)
        integer :: n, i, n_high
        real(rk) :: total_wealth, weight_norm_sum

        n = size(this % pop)
        allocate (cash_list(n))
        do i = 1, n
            cash_list(i) = this % pop(i) % cash
        end do

        n_high = 0
        do i = 1, n
            if (this % pop(i) % last_action == 1) n_high = n_high + 1
        end do

        total_wealth = sum(cash_list)
        weight_norm_sum = 0.0_rk
        do i = 1, n
            weight_norm_sum = weight_norm_sum + sqrt(dot_product(this % pop(i) % weights, &
                                                                 this % pop(i) % weights))
        end do

        allocate (metrics(9))
        metrics(1) = metric_gini_coefficient(cash_list)
        metrics(2) = top_wealth_share(cash_list, 0.10_rk)
        metrics(3) = top_wealth_share(cash_list, 0.01_rk)
        metrics(4) = sum(this % pop % saving_propensity) / real(n, rk)
        metrics(5) = real(n_high, rk) / real(n, rk)
        metrics(6) = this % last_mean_abs_reward
        metrics(7) = this % last_rejected_fraction
        metrics(8) = total_wealth
        metrics(9) = weight_norm_sum / real(n, rk)
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(CCMExchangeTLU), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i
        allocate (result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

end module model_ccm_exchange_tlu_m

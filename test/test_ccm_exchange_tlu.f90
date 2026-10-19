! Tests for CCMExchangeTLU (see CCMExchangeTLU_implementation_plan.md):
! factory/config selection, pure policy units, learning arithmetic,
! two-agent exchange, conservation/bounds, no look-ahead,
! reproducibility, CCM baseline parity, six-mode integration with
! metrics/CSV checks, and edge cases.
program test_ccm_exchange_tlu
    use kinds_m
    use config_m
    use sim_base_m
    use sim_factory_m
    use model_ccm_exchange_m
    use model_ccm_exchange_tlu_m
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

    integer :: n_fail = 0
    integer :: n_pass = 0

    call test_pure_policy()
    call test_learning_arithmetic()
    call test_two_agent_exchange()
    call test_config_validation()
    call test_factory_template_files()
    call test_all_modes_init()
    call test_conservation_and_bounds()
    call test_no_lookahead_and_learning_context()
    call test_reproducibility()
    call test_ccm_baseline_parity()
    call test_integration_metrics_csv()
    call test_edge_cases()

    print *, 'CCMExchangeTLU tests: ', n_pass, ' passed, ', n_fail, ' failed.'
    if (n_fail > 0) stop 1

contains

    subroutine check(cond, msg)
        logical, intent(in) :: cond
        character(*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            print *, 'FAIL: ', msg
        end if
    end subroutine check

    ! Bitwise real equality without tripping -Werror=compare-reals
    logical function req(a, b)
        real(rk), intent(in) :: a, b

        req = transfer(a, 0_int64) == transfer(b, 0_int64)
    end function req

    logical function reqv(a, b)
        real(rk), intent(in) :: a(:), b(:)
        integer :: k

        reqv = .false.
        if (size(a) /= size(b)) return
        do k = 1, size(a)
            if (.not. req(a(k), b(k))) return
        end do
        reqv = .true.
    end function reqv

    ! Build a small valid config for a given mode (no namelist files needed)
    subroutine make_cfg(cfg, mode, n_agents, seed)
        type(Config_CCMExchangeTLU), intent(out) :: cfg
        character(*), intent(in) :: mode
        integer(int64), intent(in) :: n_agents, seed

        cfg = Config_CCMExchangeTLU()
        cfg % n_agents = n_agents
        cfg % n_steps = 10_int64
        cfg % init_cash = 50.0_rk
        cfg % debt_limit = 0.0_rk
        cfg % exchange_delta = 1.0_rk
        cfg % min_saving_propensity = 0.1_rk
        cfg % max_saving_propensity = 0.9_rk
        cfg % seed = seed
        cfg % write_every = 1000_int64
        cfg % policy_mode = mode
        cfg % lambda_low = 0.2_rk
        cfg % lambda_high = 0.8_rk
        cfg % learning_rate = 0.01_rk
        cfg % policy_temperature = 1.0_rk
        cfg % weight_init_scale = 0.1_rk
        cfg % weight_clip = 5.0_rk
        cfg % reward_clip = 2.0_rk
        cfg % memory_decay = 0.9_rk
        cfg % use_partner_wealth = .true.
        cfg % use_memory = .true.
    end subroutine make_cfg

    !------------------------------------------------------------
    subroutine test_pure_policy()
        real(rk) :: w(4), x(4), p
        integer :: a

        ! Hard threshold: z = -0.1 + 1.0*0.2 = 0.1 >= 0 -> 1
        w = [-0.1_rk, 1.0_rk, 0.0_rk, 0.0_rk]
        x = [1.0_rk, 0.2_rk, 0.9_rk, -0.3_rk]
        a = tlu_hard_action(w, x)
        call check(a == 1, 'hard action fires at z >= 0')

        ! z = -0.5 + 0.2 = -0.3 -> 0
        w = [-0.5_rk, 1.0_rk, 0.0_rk, 0.0_rk]
        a = tlu_hard_action(w, x)
        call check(a == 0, 'hard action silent at z < 0')

        ! Exact zero boundary fires (z = 0)
        w = [0.0_rk, 0.0_rk, 0.0_rk, 0.0_rk]
        x = [1.0_rk, 0.0_rk, 0.0_rk, 0.0_rk]
        a = tlu_hard_action(w, x)
        call check(a == 1, 'hard action fires at z == 0')

        ! Stochastic probability: sigmoid(0.1) at T = 1
        w = [-0.1_rk, 1.0_rk, 0.0_rk, 0.0_rk]
        x = [1.0_rk, 0.2_rk, 0.0_rk, 0.0_rk]
        p = tlu_action_probability(w, x, 1.0_rk)
        call check(abs(p - 0.5249791874789399_rk) < 1.0e-12_rk, 'sigmoid probability value')

        ! Bernoulli sampling against fixed uniforms
        call check(tlu_sample_action(0.7_rk, 0.3_rk) == 1, 'bernoulli sample fires below p')
        call check(tlu_sample_action(0.7_rk, 0.8_rk) == 0, 'bernoulli sample silent above p')

        ! Feature vector: own +50%, partner -50% vs mean 50, ema 0.5
        call tlu_build_features(75.0_rk, 25.0_rk, 50.0_rk, 0.5_rk, .true., .true., x)
        call check(abs(x(1) - 1.0_rk) < 1.0e-15_rk, 'feature bias is 1')
        call check(abs(x(2) - 0.46211715726000974_rk) < 1.0e-12_rk, 'feature own wealth')
        call check(abs(x(3) + 0.46211715726000974_rk) < 1.0e-12_rk, 'feature partner wealth')
        call check(abs(x(4) - 0.46211715726000974_rk) < 1.0e-12_rk, 'feature memory')

        ! Ablations zero the corresponding inputs
        call tlu_build_features(75.0_rk, 25.0_rk, 50.0_rk, 0.5_rk, .false., .true., x)
        call check(req(x(3), 0.0_rk), 'disabled partner input is zero')
        call check(abs(x(2) - 0.46211715726000974_rk) < 1.0e-12_rk, 'own input kept under partner ablation')
        call tlu_build_features(75.0_rk, 25.0_rk, 50.0_rk, 0.5_rk, .true., .false., x)
        call check(req(x(4), 0.0_rk), 'disabled memory input is zero')

        ! Sigmoid numerical guards
        p = tlu_sigmoid(1000.0_rk)
        call check(ieee_is_finite(p) .and. p > 1.0_rk - 1.0e-12_rk, 'sigmoid saturates high, finite')
        p = tlu_sigmoid(-1000.0_rk)
        call check(ieee_is_finite(p) .and. p < 1.0e-12_rk, 'sigmoid saturates low, finite')

        ! Clip helper
        call check(req(tlu_clip(5.0_rk, -2.0_rk, 3.0_rk), 3.0_rk), 'clip high')
        call check(req(tlu_clip(-5.0_rk, -2.0_rk, 3.0_rk), -2.0_rk), 'clip low')
        call check(req(tlu_clip(1.0_rk, -2.0_rk, 3.0_rk), 1.0_rk), 'clip passthrough')
    end subroutine test_pure_policy

    !------------------------------------------------------------
    subroutine test_learning_arithmetic()
        real(rk) :: w(4), x(4)

        x = [1.0_rk, 0.5_rk, -0.25_rk, 0.1_rk]
        ! lr*adv*(a-p)/T = 0.1*0.5*0.4/2.0 = 0.01 per unit x
        w = [0.1_rk, -0.2_rk, 0.3_rk, -0.4_rk]
        call tlu_reinforce_update(w, x, 1, 0.6_rk, 0.5_rk, 0.1_rk, 2.0_rk, 5.0_rk)
        call check(abs(w(1) - 0.11_rk) < 1.0e-12_rk, 'reinforce w0')
        call check(abs(w(2) + 0.195_rk) < 1.0e-12_rk, 'reinforce w1')
        call check(abs(w(3) - 0.2975_rk) < 1.0e-12_rk, 'reinforce w2')
        call check(abs(w(4) + 0.399_rk) < 1.0e-12_rk, 'reinforce w3')

        ! Zero learning rate leaves weights unchanged
        w = [0.1_rk, -0.2_rk, 0.3_rk, -0.4_rk]
        call tlu_reinforce_update(w, x, 1, 0.6_rk, 0.5_rk, 0.0_rk, 2.0_rk, 5.0_rk)
        call check(reqv(w, [0.1_rk, -0.2_rk, 0.3_rk, -0.4_rk]), 'zero learning rate freezes weights')

        ! Per-weight clipping on both sides
        w = [4.9_rk, -4.9_rk, 0.0_rk, 0.0_rk]
        x = [1.0_rk, -1.0_rk, 0.0_rk, 0.0_rk]
        call tlu_reinforce_update(w, x, 1, 0.0_rk, 1.0_rk, 10.0_rk, 1.0_rk, 5.0_rk)
        call check(req(w(1), 5.0_rk), 'weight clip high')
        call check(req(w(2), -5.0_rk), 'weight clip low')
        call check(req(w(3), 0.0_rk), 'zero feature gives zero update')
    end subroutine test_learning_arithmetic

    !------------------------------------------------------------
    subroutine test_two_agent_exchange()
        real(rk) :: n1, n2, m(2, 2), res(2)

        ! Hand-computed: pool = 0.7*100 + 0.3*50 = 85;
        ! new = (30 + 0.4*85, 35 + 0.6*85) = (64, 86)
        call ccm_pair_update(100.0_rk, 50.0_rk, 0.3_rk, 0.7_rk, 0.4_rk, n1, n2)
        call check(abs(n1 - 64.0_rk) < 1.0e-9_rk, 'ccm pair update agent 1')
        call check(abs(n2 - 86.0_rk) < 1.0e-9_rk, 'ccm pair update agent 2')
        call check(abs((n1 + n2) - 150.0_rk) < 1.0e-12_rk, 'ccm pair conserves wealth')

        ! Pool form matches the 2x2 matrix form used in step()
        m(1, 1) = 0.3_rk + 0.4_rk * 0.7_rk
        m(1, 2) = 0.4_rk * 0.3_rk
        m(2, 1) = 0.6_rk * 0.7_rk
        m(2, 2) = 0.7_rk + 0.6_rk * 0.3_rk
        res = apply_exchange_matrix(100.0_rk, 50.0_rk, m)
        call check(abs(res(1) - n1) < 1.0e-9_rk, 'matrix form matches pool form (agent 1)')
        call check(abs(res(2) - n2) < 1.0e-9_rk, 'matrix form matches pool form (agent 2)')
    end subroutine test_two_agent_exchange

    !------------------------------------------------------------
    subroutine test_config_validation()
        type(Config_CCMExchangeTLU) :: cfg
        character(len=:), allocatable :: msg

        cfg = Config_CCMExchangeTLU()
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) == 0, 'default config is valid')

        cfg = Config_CCMExchangeTLU()
        cfg % policy_mode = 'not_a_mode'
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'unknown policy mode rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % policy_mode = 'tlu_learning'
        cfg % policy_temperature = 0.0_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'nonpositive temperature rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % learning_rate = -0.1_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'negative learning rate rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % memory_decay = 1.0_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'memory_decay = 1 rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % memory_decay = -0.1_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'negative memory_decay rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % lambda_low = 0.9_rk
        cfg % lambda_high = 0.1_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'lambda_low > lambda_high rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % lambda_high = 1.5_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'lambda_high > 1 rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % min_saving_propensity = 0.8_rk
        cfg % max_saving_propensity = 0.2_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'min > max saving rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % init_cash = 0.0_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'nonpositive init_cash rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % n_agents = 1_int64
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'single agent rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % weight_clip = 0.0_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'nonpositive weight_clip rejected')

        cfg = Config_CCMExchangeTLU()
        cfg % reward_clip = 0.0_rk
        msg = cfg % check_ccm_exchange_tlu()
        call check(len(msg) > 0, 'nonpositive reward_clip rejected')
    end subroutine test_config_validation

    !------------------------------------------------------------
    ! Select the model through the shipped template namelist files.
    ! Only runs when the test working directory is the package root;
    ! otherwise it reports a skip (direct-config tests still cover
    ! all six modes).
    subroutine test_factory_template_files()
        logical :: has_templates, has_sim_type, has_in
        integer :: es, cs
        class(AbstractConfig), allocatable :: gcfg
        class(AbstractSimulator), allocatable :: gsim
        real(rk), allocatable :: m(:)
        logical :: saw_tlu

        inquire (file='templates/CCMExchangeTLU/sim_type.nml', exist=has_templates)
        inquire (file='sim_type.nml', exist=has_sim_type)
        if (.not. (has_templates .and. has_sim_type)) then
            print *, 'SKIP: template-file factory test (not running from package root)'
            return
        end if
        inquire (file='in.nml', exist=has_in)

        call execute_command_line('cp sim_type.nml test_tlu_backup_sim_type.nml', &
                                  exitstat=es, cmdstat=cs)
        call check(es == 0, 'backup sim_type.nml')
        if (has_in) then
            call execute_command_line('cp in.nml test_tlu_backup_in.nml', exitstat=es, cmdstat=cs)
            call check(es == 0, 'backup in.nml')
        end if
        call execute_command_line('cp templates/CCMExchangeTLU/sim_type.nml sim_type.nml', &
                                  exitstat=es, cmdstat=cs)
        call check(es == 0, 'install template sim_type.nml')
        call execute_command_line('cp templates/CCMExchangeTLU/in.nml in.nml', &
                                  exitstat=es, cmdstat=cs)
        call check(es == 0, 'install template in.nml')

        saw_tlu = .false.
        gcfg = create_config()
        select type (gcfg)
        type is (Config_CCMExchangeTLU)
            saw_tlu = .true.
            call gcfg % read_nml()
            call check(trim(gcfg % policy_mode) == 'tlu_learning', 'template policy mode parses')
            call check(gcfg % n_agents == 10000_int64, 'template n_agents parses')
        class default
            saw_tlu = .false.
        end select
        call check(saw_tlu, 'template selects Config_CCMExchangeTLU')

        saw_tlu = .false.
        gsim = create_simulator()
        select type (gsim)
        type is (CCMExchangeTLU)
            saw_tlu = .true.
            call gsim % init(gcfg)
            call gsim % step()
            m = gsim % compute_metrics()
            call check(size(m) == 9, 'template-run metrics size')
            call check(all(ieee_is_finite(m)), 'template-run metrics finite')
        class default
            saw_tlu = .false.
        end select
        call check(saw_tlu, 'template selects CCMExchangeTLU simulator')

        ! Restore the original namelist files before any further checks.
        call execute_command_line('cp test_tlu_backup_sim_type.nml sim_type.nml', &
                                  exitstat=es, cmdstat=cs)
        call check(es == 0, 'restore sim_type.nml')
        if (has_in) then
            call execute_command_line('cp test_tlu_backup_in.nml in.nml', exitstat=es, cmdstat=cs)
            call check(es == 0, 'restore in.nml')
        else
            call execute_command_line('rm -f in.nml', exitstat=es, cmdstat=cs)
        end if
        call execute_command_line('rm -f test_tlu_backup_sim_type.nml test_tlu_backup_in.nml', &
                                  exitstat=es, cmdstat=cs)
    end subroutine test_factory_template_files

    !------------------------------------------------------------
    subroutine test_all_modes_init()
        character(len=32) :: modes(6)
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim, sim_learn
        integer :: im, i
        logical :: all_zero, cash_ok

        modes = ['ccm_fixed     ', 'binary_fixed  ', 'binary_random ', &
                 'heuristic     ', 'tlu_fixed     ', 'tlu_learning  ']

        do im = 1, 6
            call make_cfg(cfg, modes(im), 10_int64, 12345_int64)
            call sim % init(cfg)
            call check(size(sim % pop) == 10, trim(modes(im))//': population size')
            cash_ok = .true.
            do i = 1, 10
                if (.not. req(sim % pop(i) % cash, 50.0_rk)) cash_ok = .false.
            end do
            call check(cash_ok, trim(modes(im))//': initial cash')
            call check(all(sim % pop % saving_propensity >= 0.0_rk) .and. &
                      all(sim % pop % saving_propensity <= 1.0_rk), &
                      trim(modes(im))//': initial saving in [0,1]')
        end do

        ! ccm_fixed samples the continuous range; binary_fixed the two levels
        call make_cfg(cfg, 'ccm_fixed', 10_int64, 12345_int64)
        call sim % init(cfg)
        call check(all(sim % pop % saving_propensity >= 0.1_rk) .and. &
                  all(sim % pop % saving_propensity <= 0.9_rk), 'ccm_fixed continuous range')
        call check(all(sim % pop % last_action == -1), 'ccm_fixed inapplicable action sentinel')

        call make_cfg(cfg, 'binary_fixed', 10_int64, 12345_int64)
        call sim % init(cfg)
        cash_ok = .true.
        do i = 1, 10
            if (.not. (req(sim % pop(i) % saving_propensity, 0.2_rk) .or. &
                       req(sim % pop(i) % saving_propensity, 0.8_rk))) cash_ok = .false.
        end do
        call check(cash_ok, 'binary_fixed two levels')

        ! Equal initial wealth: heuristic chooses low saving everywhere
        call make_cfg(cfg, 'heuristic', 10_int64, 12345_int64)
        call sim % init(cfg)
        call check(all(sim % pop % last_action == 0), 'heuristic low saving at equal wealth')

        ! Non-TLU modes keep zero weights
        call make_cfg(cfg, 'binary_random', 10_int64, 12345_int64)
        call sim % init(cfg)
        all_zero = .true.
        do i = 1, 10
            if (.not. reqv(sim % pop(i) % weights, [0.0_rk, 0.0_rk, 0.0_rk, 0.0_rk])) all_zero = .false.
        end do
        call check(all_zero, 'non-TLU weights are zero')

        ! TLU weights are bounded by the init scale and identical across
        ! tlu_fixed / tlu_learning under the same seed and policy stream.
        call make_cfg(cfg, 'tlu_fixed', 10_int64, 12345_int64)
        call sim % init(cfg)
        call make_cfg(cfg, 'tlu_learning', 10_int64, 12345_int64)
        call sim_learn % init(cfg)
        all_zero = .true.
        do i = 1, 10
            if (any(abs(sim % pop(i) % weights) > 0.1_rk)) &
                call check(.false., 'tlu weight init bounded')
            if (.not. reqv(sim_learn % pop(i) % weights, sim % pop(i) % weights)) all_zero = .false.
        end do
        call check(all_zero, 'tlu_fixed/learning share initial weights')
    end subroutine test_all_modes_init

    !------------------------------------------------------------
    subroutine total_and_bounds(sim, n, total, ok_cash, ok_sav, ok_w, ok_prob)
        type(CCMExchangeTLU), intent(in) :: sim
        integer, intent(in) :: n
        real(rk), intent(out) :: total
        logical, intent(out) :: ok_cash, ok_sav, ok_w, ok_prob
        integer :: i

        total = 0.0_rk
        ok_cash = .true.
        ok_sav = .true.
        ok_w = .true.
        ok_prob = .true.
        do i = 1, n
            total = total + sim % pop(i) % cash
            if (.not. ieee_is_finite(sim % pop(i) % cash)) ok_cash = .false.
            if (sim % pop(i) % cash < -1.0e-9_rk) ok_cash = .false.
            if (sim % pop(i) % saving_propensity < 0.0_rk .or. &
                sim % pop(i) % saving_propensity > 1.0_rk) ok_sav = .false.
            if (any(abs(sim % pop(i) % weights) > 5.0_rk + 1.0e-12_rk)) ok_w = .false.
            if (.not. ieee_is_finite(sim % pop(i) % reward_ema)) ok_cash = .false.
            if (sim % policy_mode /= 'ccm_fixed') then
                if (sim % pop(i) % last_policy_probability < 0.0_rk .or. &
                    sim % pop(i) % last_policy_probability > 1.0_rk) ok_prob = .false.
            end if
        end do
    end subroutine total_and_bounds

    subroutine test_conservation_and_bounds()
        character(len=32) :: modes(6)
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim
        integer :: im, t, n
        real(rk) :: total, total0
        logical :: ok_cash, ok_sav, ok_w, ok_prob
        real(rk) :: w_before(4, 20)

        modes = ['ccm_fixed     ', 'binary_fixed  ', 'binary_random ', &
                 'heuristic     ', 'tlu_fixed     ', 'tlu_learning  ']
        n = 20
        do im = 1, 6
            call make_cfg(cfg, modes(im), int(n, int64), 999_int64)
            call sim % init(cfg)
            total0 = sum(sim % pop % cash)
            do t = 1, 10
                call sim % step()
            end do
            call total_and_bounds(sim, n, total, ok_cash, ok_sav, ok_w, ok_prob)
            call check(abs(total - total0) < 1.0e-9_rk * max(total0, 1.0_rk), &
                       trim(modes(im))//': wealth conserved')
            call check(ok_cash, trim(modes(im))//': cash finite and nonnegative')
            call check(ok_sav, trim(modes(im))//': saving in [0,1]')
            call check(ok_w, trim(modes(im))//': weights within clip')
            call check(ok_prob, trim(modes(im))//': policy probabilities in [0,1]')
        end do

        ! tlu_fixed never changes weights; learning with lr = 0 neither.
        call make_cfg(cfg, 'tlu_fixed', int(n, int64), 999_int64)
        call sim % init(cfg)
        do t = 1, n
            w_before(:, t) = sim % pop(t) % weights
        end do
        do t = 1, 5
            call sim % step()
        end do
        do t = 1, n
            call check(reqv(sim % pop(t) % weights, w_before(:, t)), 'tlu_fixed weights frozen')
        end do

        call make_cfg(cfg, 'tlu_learning', int(n, int64), 999_int64)
        cfg % learning_rate = 0.0_rk
        call sim % init(cfg)
        do t = 1, n
            w_before(:, t) = sim % pop(t) % weights
        end do
        do t = 1, 5
            call sim % step()
        end do
        do t = 1, n
            call check(reqv(sim % pop(t) % weights, w_before(:, t)), 'zero-lr weights frozen')
        end do
    end subroutine test_conservation_and_bounds

    !------------------------------------------------------------
    ! Decisions use pre-trade cash and the prior EMA only; the
    ! learning update uses the cached pre-trade context.
    subroutine test_no_lookahead_and_learning_context()
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim
        real(rk) :: x1(4), x2(4), m1, m2, b1, b2, p1, r1, adv1
        real(rk) :: w_expect(4)
        integer :: a1, a2, t

        ! Two agents form the only pair, so decisions are predictable.
        call make_cfg(cfg, 'tlu_fixed', 2_int64, 4242_int64)
        call sim % init(cfg)
        ! Inject a nonzero prior memory and asymmetric wealth.
        sim % pop(1) % cash = 60.0_rk
        sim % pop(2) % cash = 40.0_rk
        sim % pop(1) % reward_ema = 0.7_rk
        sim % pop(2) % reward_ema = -0.3_rk
        m1 = 60.0_rk
        m2 = 40.0_rk
        b1 = 0.7_rk
        b2 = -0.3_rk
        call tlu_build_features(m1, m2, sim % mean_cash, b1, .true., .true., x1)
        call tlu_build_features(m2, m1, sim % mean_cash, b2, .true., .true., x2)
        a1 = tlu_hard_action(sim % pop(1) % weights, x1)
        a2 = tlu_hard_action(sim % pop(2) % weights, x2)
        call sim % step()
        call check(sim % pop(1) % last_action == a1, 'no look-ahead: agent 1 action from pre-trade')
        call check(sim % pop(2) % last_action == a2, 'no look-ahead: agent 2 action from pre-trade')
        ! The EMA baseline used above was the prior one: after the step it
        ! must have moved toward the realized reward, not stayed put.
        call check(.not. req(sim % pop(1) % reward_ema, b1) .or. req(sim % pop(1) % last_reward, 0.0_rk), &
                   'ema responds to realized reward')

        ! Learning uses the cached pre-trade (x, p, action, baseline).
        call make_cfg(cfg, 'tlu_learning', 2_int64, 4242_int64)
        call sim % init(cfg)
        sim % pop(1) % cash = 60.0_rk
        sim % pop(2) % cash = 40.0_rk
        sim % pop(1) % reward_ema = 0.7_rk
        sim % pop(2) % reward_ema = -0.3_rk
        m1 = 60.0_rk
        b1 = 0.7_rk
        call tlu_build_features(m1, 40.0_rk, sim % mean_cash, b1, .true., .true., x1)
        p1 = tlu_action_probability(sim % pop(1) % weights, x1, sim % policy_temperature)
        w_expect = sim % pop(1) % weights
        call sim % step()
        ! Agent 1 may be perm(1) or perm(2); find it via last_action match is
        ! unnecessary: with two agents both traded, recompute from recording.
        a1 = sim % pop(1) % last_action
        r1 = sim % pop(1) % last_reward
        adv1 = r1 - b1
        call tlu_reinforce_update(w_expect, x1, a1, p1, adv1, sim % learning_rate, &
                                 sim % policy_temperature, sim % weight_clip)
        t = 0
        if (reqv(sim % pop(1) % weights, w_expect)) t = 1
        call check(t == 1, 'learning uses cached pre-trade context')
        ! EMA follows the documented recursion with the prior baseline.
        call check(abs(sim % pop(1) % reward_ema - &
                       (sim % memory_decay * b1 + (1.0_rk - sim % memory_decay) * r1)) < 1.0e-15_rk, &
                   'ema recursion uses prior baseline')
    end subroutine test_no_lookahead_and_learning_context

    !------------------------------------------------------------
    subroutine test_reproducibility()
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim_a, sim_b
        integer :: t, i
        logical :: same

        call make_cfg(cfg, 'tlu_learning', 10_int64, 777_int64)
        call sim_a % init(cfg)
        do t = 1, 5
            call sim_a % step()
        end do
        call make_cfg(cfg, 'tlu_learning', 10_int64, 777_int64)
        call sim_b % init(cfg)
        do t = 1, 5
            call sim_b % step()
        end do

        same = .true.
        do i = 1, 10
            if (.not. req(sim_a % pop(i) % cash, sim_b % pop(i) % cash)) same = .false.
            if (.not. reqv(sim_a % pop(i) % weights, sim_b % pop(i) % weights)) same = .false.
            if (.not. req(sim_a % pop(i) % saving_propensity, sim_b % pop(i) % saving_propensity)) same = .false.
            if (.not. req(sim_a % pop(i) % reward_ema, sim_b % pop(i) % reward_ema)) same = .false.
        end do
        call check(same, 'identical seeds reproduce trajectories')
        call check(req(sim_a % last_rejected_fraction, sim_b % last_rejected_fraction), &
                   'rejected fraction reproduces')
    end subroutine test_reproducibility

    !------------------------------------------------------------
    ! ccm_fixed must reproduce the corrected CCMExchange benchmark
    ! trajectory exactly (aligned RNG streams, same 2x2 arithmetic).
    subroutine test_ccm_baseline_parity()
        type(Config_CCMExchange) :: cfg_ref
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchange) :: ref
        type(CCMExchangeTLU) :: sim
        integer :: t, i
        logical :: same

        cfg_ref = Config_CCMExchange()
        cfg_ref % n_agents = 10_int64
        cfg_ref % n_steps = 5_int64
        cfg_ref % init_cash = 50.0_rk
        cfg_ref % debt_limit = 0.0_rk
        cfg_ref % exchange_delta = 1.0_rk
        cfg_ref % min_saving_propensity = 0.1_rk
        cfg_ref % max_saving_propensity = 0.9_rk
        cfg_ref % seed = 31337_int64
        cfg_ref % write_every = 1000_int64
        call ref % init(cfg_ref)
        do t = 1, 5
            call ref % step()
        end do

        call make_cfg(cfg, 'ccm_fixed', 10_int64, 31337_int64)
        cfg % min_saving_propensity = 0.1_rk
        cfg % max_saving_propensity = 0.9_rk
        call sim % init(cfg)
        do t = 1, 5
            call sim % step()
        end do

        same = .true.
        do i = 1, 10
            if (.not. req(sim % pop(i) % cash, ref % pop(i) % cash)) same = .false.
            if (.not. req(sim % pop(i) % saving_propensity, ref % pop(i) % saving_propensity)) same = .false.
        end do
        call check(same, 'ccm_fixed matches CCMExchange trajectories')
    end subroutine test_ccm_baseline_parity

    !------------------------------------------------------------
    subroutine check_csv_file(sim, n, tag)
        type(CCMExchangeTLU), intent(in) :: sim
        integer, intent(in) :: n
        character(*), intent(in) :: tag
        character(len=512) :: line
        character(len=*), parameter :: fname = 'test_tlu_tmp_pop.csv'
        integer :: uu, ios, nlines, rid, ract
        real(rk) :: rcash, rsav, rprob, rrew, rema, w0, w1, w2, w3

        call sim % write_population_csv(fname)
        open (newunit=uu, file=fname, status='old', action='read', iostat=ios)
        call check(ios == 0, tag//': csv opens')
        if (ios /= 0) return
        read (uu, '(a)', iostat=ios) line
        call check(ios == 0, tag//': csv header reads')
        call check(trim(line) == 'agent_id,cash,saving_propensity,last_action,'// &
                  'last_policy_probability,last_reward,reward_ema,w0,w1,w2,w3', tag//': csv header')
        nlines = 0
        do
            read (uu, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        call check(nlines == n, tag//': csv row count')
        rewind (uu)
        read (uu, '(a)') line
        read (uu, *, iostat=ios) rid, rcash, rsav, ract, rprob, rrew, rema, w0, w1, w2, w3
        call check(ios == 0, tag//': first csv row parses')
        call check(rid == 1, tag//': first csv row id')
        close (uu, status='delete')
    end subroutine check_csv_file

    subroutine test_integration_metrics_csv()
        character(len=32) :: modes(6)
        character(len=32) :: expected(9)
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim
        character(len=32), allocatable :: names(:)
        real(rk), allocatable :: m(:)
        integer :: im, t, i

        modes = ['ccm_fixed     ', 'binary_fixed  ', 'binary_random ', &
                 'heuristic     ', 'tlu_fixed     ', 'tlu_learning  ']
        expected(1) = 'gini_coefficient'
        expected(2) = 'top_10pct_wealth_share'
        expected(3) = 'top_1pct_wealth_share'
        expected(4) = 'mean_saving_propensity'
        expected(5) = 'high_action_fraction'
        expected(6) = 'mean_abs_reward'
        expected(7) = 'rejected_trade_fraction'
        expected(8) = 'total_wealth'
        expected(9) = 'mean_weight_norm'
        do im = 1, 6
            call make_cfg(cfg, modes(im), 30_int64, 20261015_int64)
            call sim % init(cfg)
            do t = 1, 3
                call sim % step()
            end do
            names = sim % get_metric_names()
            call check(size(names) == 9, trim(modes(im))//': nine metric names')
            do i = 1, 9
                call check(trim(names(i)) == trim(expected(i)), &
                           trim(modes(im))//': metric label '//trim(expected(i)))
            end do
            m = sim % compute_metrics()
            call check(size(m) == 9, trim(modes(im))//': nine metric values')
            call check(all(ieee_is_finite(m)), trim(modes(im))//': metrics finite')
            call check(m(1) >= 0.0_rk .and. m(1) <= 1.0_rk, trim(modes(im))//': gini in [0,1]')
            call check(m(2) >= m(3), trim(modes(im))//': top10 share >= top1 share')
            call check(m(8) > 0.0_rk, trim(modes(im))//': positive total wealth')
            call check(m(7) >= 0.0_rk .and. m(7) <= 1.0_rk, trim(modes(im))//': rejected fraction in [0,1]')
            call check_csv_file(sim, 30, trim(modes(im)))
        end do

        ! ccm_fixed binary-action diagnostics are inapplicable by construction
        call make_cfg(cfg, 'ccm_fixed', 30_int64, 20261015_int64)
        call sim % init(cfg)
        call sim % step()
        m = sim % compute_metrics()
        call check(req(m(5), 0.0_rk), 'ccm_fixed high_action_fraction is 0')
        call check(req(m(9), 0.0_rk), 'ccm_fixed mean_weight_norm is 0')
    end subroutine test_integration_metrics_csv

    !------------------------------------------------------------
    subroutine test_edge_cases()
        type(Config_CCMExchangeTLU) :: cfg
        type(CCMExchangeTLU) :: sim
        character(len=32) :: modes(6)
        integer :: im, t
        real(rk) :: total0, total1, ema0
        real(rk) :: w0(4)

        modes = ['ccm_fixed     ', 'binary_fixed  ', 'binary_random ', &
                 'heuristic     ', 'tlu_fixed     ', 'tlu_learning  ']

        ! Two agents: smallest valid population, every mode.
        do im = 1, 6
            call make_cfg(cfg, modes(im), 2_int64, 5_int64)
            call sim % init(cfg)
            total0 = sum(sim % pop % cash)
            do t = 1, 3
                call sim % step()
            end do
            total1 = sum(sim % pop % cash)
            call check(abs(total1 - total0) < 1.0e-12_rk, trim(modes(im))//': two-agent conservation')
            call check(all(ieee_is_finite(sim % compute_metrics())), &
                       trim(modes(im))//': two-agent metrics finite')
        end do

        ! Odd population: the unpaired agent retains its state.
        call make_cfg(cfg, 'tlu_learning', 5_int64, 5_int64)
        call sim % init(cfg)
        total0 = sum(sim % pop % cash)
        do t = 1, 3
            call sim % step()
        end do
        total1 = sum(sim % pop % cash)
        call check(abs(total1 - total0) < 1.0e-12_rk, 'odd population conserves wealth')

        ! Forced rejection: an enormous exchange_delta rejects every trade.
        call make_cfg(cfg, 'tlu_learning', 4_int64, 5_int64)
        call sim % init(cfg)
        call sim % step()
        ema0 = sim % pop(1) % reward_ema
        w0 = sim % pop(1) % weights
        sim % exchange_delta = 1.0e9_rk
        call sim % step()
        call check(req(sim % last_rejected_fraction, 1.0_rk), 'forced rejection fraction is 1')
        call check(req(sim % last_mean_abs_reward, 0.0_rk), 'rejected step has zero mean abs reward')
        call check(reqv(sim % pop(1) % weights, w0), 'rejected trade skips learning')
        call check(abs(sim % pop(1) % reward_ema - sim % memory_decay * ema0) < 1.0e-15_rk, &
                   'rejected trade decays ema toward zero')
        call check(req(sim % pop(1) % last_reward, 0.0_rk), 'rejected trade reward is zero')
    end subroutine test_edge_cases

end program test_ccm_exchange_tlu

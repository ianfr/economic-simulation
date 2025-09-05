!===============================================================
! model_stochastic_preferences.f90
! Stochastic Preferences Exchange Market implementation
! Based on Silver, Slud, and Takamoto (2002) - reference (5)
! See MODELS.md for more details
!===============================================================
module model_stochastic_preferences_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use sim_base_m
    use abm_metrics_m
    implicit none
    public 

    ! Define the agent privately
    private :: Agent
    type :: Agent
        real(rk) :: good_a = 0.0_rk           ! Holdings of good A
        real(rk) :: good_b = 0.0_rk           ! Holdings of good B
        real(rk) :: preference = 0.0_rk       ! Stochastic preference f_it ∈ [0,1]
        real(rk) :: wealth = 0.0_rk           ! Total wealth in monetary units
    end type Agent

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: StochasticPreferences
        type(Agent), allocatable :: pop(:)
        character(len=32), allocatable :: names(:)  ! Metric names

        integer(int64)  :: N_agents = 0         ! Number of agents
        real(rk)        :: alpha = 1            ! Conservation parameter for good A (αN total)
        real(rk)        :: beta = 1             ! Conservation parameter for good B (βN total)
        real(rk)        :: price = 1.0_rk       ! Current market price θ_t
    contains
        ! Override the abstract methods
        procedure :: init
        procedure :: step
        procedure :: write_population_csv
        procedure :: compute_metrics
        procedure :: get_metric_names

        ! Private helper methods
        procedure, private :: gini_coefficient_good_a
        procedure, private :: gini_coefficient_good_b
        procedure, private :: gini_coefficient_wealth
        procedure, private :: compute_market_price
        procedure, private :: update_agent_allocations
        procedure, private :: validate_market_clearing
    end type StochasticPreferences

contains
    subroutine init(this, cfg)
        class(StochasticPreferences), intent(out) :: this
        class(AbstractConfig), intent(in)  :: cfg
        integer(int64) :: i
        real(rk) :: u

        select type (cfg)
        type is (Config_StochasticPreferences)
            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % alpha = cfg % alpha
            this % beta = cfg % beta
            this % N_agents = cfg % n_agents

            ! Initialize random number generator
            call init_rng(cfg % seed)
            
            ! Allocate population
            allocate (this % pop(cfg % n_agents))
            
            ! Allocate metric names
            allocate (this % names(4))
            this % names(1) = 'gini_good_a'
            this % names(2) = 'gini_good_b'
            this % names(3) = 'gini_wealth'
            this % names(4) = 'price'
            
            ! Initialize agents with equal holdings and random preferences
            do i = 1, cfg % n_agents
                this % pop(i) % good_a = this % alpha
                this % pop(i) % good_b = this % beta

                ! Assign random preference f_it ∈ [0,1]
                call random_number(u)
                this % pop(i) % preference = u
                
                ! Initial wealth calculation (price starts at 1.0)
                this % pop(i) % wealth = this % pop(i) % good_a + this % price * this % pop(i) % good_b
            end do

            ! Validate initial market clearing constraints
            call this % validate_market_clearing(1.0e-6_rk)
            
            write (*, '(a,i0)') 'Initialized StochasticPreferences simulation with agents: ', cfg % n_agents
        class default
            error stop 'Unsupported config type in StochasticPreferences init'
        end select
    end subroutine init

    !------------------------------------------------------------
    ! Main simulation step following Silver et al. 2002 algorithm:
    ! 1. Update stochastic preferences for all agents
    ! 2. Compute new market-clearing price
    ! 3. Update agent allocations based on utility maximization
    subroutine step(this)
        class(StochasticPreferences), intent(inout) :: this
        integer(int64) :: i
        ! real(rk) :: u
        real, dimension(:), allocatable :: u_list

        ! Leverage doing RNG all at once
        allocate(u_list(size(this % pop)))
        call random_number(u_list)

        ! Step 1: Update stochastic preferences (new random values each period)
        do i = 1, size(this % pop)
! \            call random_number(this % pop(i) % preference)
            this % pop(i) % preference = u_list(i)
        end do

        ! Step 2: Compute market-clearing price
        call this % compute_market_price()

        ! Step 3: Update agent allocations and wealth
        call this % update_agent_allocations()

        ! Step 4: Validate market clearing constraints
        call this % validate_market_clearing(0.1_rk)
    end subroutine step

    !------------------------------------------------------------
    ! Validate that market clearing constraints are satisfied
    ! Sum of all good_a must equal alpha*N and sum of all good_b must equal beta*N
    subroutine validate_market_clearing(this, tolerance)
        class(StochasticPreferences), intent(in) :: this
        real(rk), intent(in) :: tolerance ! Relative error to allow for fluctuations
        real(rk) :: total_good_a, total_good_b, expected_total_a, expected_total_b
        real(rk) :: error_a, error_b
        integer(int64) :: i

        ! Calculate actual totals
        total_good_a = 0.0_rk
        total_good_b = 0.0_rk
        do i = 1, size(this % pop)
            total_good_a = total_good_a + this % pop(i) % good_a
            total_good_b = total_good_b + this % pop(i) % good_b
        end do

        ! Calculate expected totals
        expected_total_a = this % alpha * real(size(this % pop), rk)
        expected_total_b = this % beta * real(size(this % pop), rk)


        ! Calculate relative errors
        error_a = abs(total_good_a - expected_total_a) / max(expected_total_a, 1.0e-12_rk)
        error_b = abs(total_good_b - expected_total_b) / max(expected_total_b, 1.0e-12_rk)

        ! Check constraints and stop with error if violated
        if (error_a > tolerance) then
            write(*, '(a)') 'ERROR: Market clearing constraint violated for good A!'
            write(*, '(a,f0.10)') 'Expected total good A: ', expected_total_a
            write(*, '(a,f0.10)') 'Actual total good A:   ', total_good_a
            write(*, '(a,f0.10)') 'Relative error:        ', error_a
            write(*, '(a,f0.10)') 'Tolerance:             ', tolerance
            error stop 'Market clearing constraint violated for good A'
        end if

        if (error_b > tolerance) then
            write(*, '(a)') 'ERROR: Market clearing constraint violated for good B!'
            write(*, '(a,f0.10)') 'Expected total good B: ', expected_total_b
            write(*, '(a,f0.10)') 'Actual total good B:   ', total_good_b
            write(*, '(a,f0.10)') 'Relative error:        ', error_b
            write(*, '(a,f0.10)') 'Tolerance:             ', tolerance
            error stop 'Market clearing constraint violated for good B'
        end if
    end subroutine validate_market_clearing

    !------------------------------------------------------------
    ! Compute market-clearing price θ_t using equation (6) from Silver et al. 2002
    ! θ_t = Σ(1-f_it)a_(i,t-1) / Σf_it*b_(i,t-1)
    subroutine compute_market_price(this)
        class(StochasticPreferences), intent(inout) :: this
        real(rk) :: numerator, denominator
        integer(int64) :: i

        numerator = 0.0_rk
        denominator = 0.0_rk

        do i = 1, size(this % pop)
            numerator = numerator + (1.0_rk - this % pop(i) % preference) * this % pop(i) % good_a
            denominator = denominator + this % pop(i) % preference * this % pop(i) % good_b
        end do

        ! Avoid division by zero with a larger safety margin
        if (denominator > 1.0e-8_rk) then
            this % price = numerator / denominator
        end if
        !     this % price = 1.0_rk  ! Default fallback price
        ! end if
        this % price = numerator / denominator

        ! Ensure price stays within reasonable bounds
        if (this % price <= 0.0_rk .or. this % price > 1.0e6_rk) then
            ! this % price = 1.0_rk
            stop 'ERROR: Computed market price out of bounds'
        end if
    end subroutine compute_market_price

    !------------------------------------------------------------
    ! Update agent allocations
    subroutine update_agent_allocations(this)
        class(StochasticPreferences), intent(inout) :: this
        integer(int64) :: i
        real(rk) :: old_a, old_b, f_it, theta_t

        theta_t = this % price

        ! Update each agent's holdings
        ! do i = 1, size(this % pop)
        do concurrent(i=1:size(this % pop))
            old_a = this % pop(i) % good_a
            old_b = this % pop(i) % good_b
            f_it = this % pop(i) % preference

            ! Use equation (7), substituting in price appropriately
            this % pop(i) % good_a = (f_it * old_a) + (f_it * old_b * theta_t)
            this % pop(i) % good_b = (1.0 - f_it) * (old_b + old_a * (1.0 / theta_t))

            ! Update wealth using equation (1): w_it = a_it + θ_t * b_it
            this % pop(i) % wealth = this % pop(i) % good_a + this % price * this % pop(i) % good_b
        end do
    end subroutine update_agent_allocations

    !------------------------------------------------------------
    subroutine write_population_csv(this, filename)
        class(StochasticPreferences), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, i, ios

        open (newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*, '(a,a)') 'Error opening file: ', filename
            return
        end if

        ! Write CSV header
        write (unit, '(a)') 'agent_id,good_a,good_b,preference,wealth'

        ! Write agent data
        do i = 1, size(this % pop)
            write (unit, '(i0,",",f0.6,",",f0.6,",",f0.6,",",f0.6)') &
                i, this % pop(i) % good_a, this % pop(i) % good_b, &
                this % pop(i) % preference, this % pop(i) % wealth
        end do

        close (unit)
        write (*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(StochasticPreferences), intent(in) :: this
        real(rk), allocatable :: metrics(:)

        allocate(metrics(4))
        metrics(1) = this % gini_coefficient_good_a()
        metrics(2) = this % gini_coefficient_good_b()
        metrics(3) = this % gini_coefficient_wealth()
        metrics(4) = this % price
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(StochasticPreferences), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i

        allocate(result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

    !------------------------------------------------------------
    function gini_coefficient_good_a(this) result(gini)
        class(StochasticPreferences), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: good_a_list(:)
        integer :: i

        allocate(good_a_list(size(this % pop)))
        do i = 1, size(this % pop)
            good_a_list(i) = this % pop(i) % good_a
        end do

        gini = metric_gini_coefficient(good_a_list)
    end function gini_coefficient_good_a

    !------------------------------------------------------------
    function gini_coefficient_good_b(this) result(gini)
        class(StochasticPreferences), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: good_b_list(:)
        integer :: i

        allocate(good_b_list(size(this % pop)))
        do i = 1, size(this % pop)
            good_b_list(i) = this % pop(i) % good_b
        end do

        gini = metric_gini_coefficient(good_b_list)
    end function gini_coefficient_good_b

    !------------------------------------------------------------
    function gini_coefficient_wealth(this) result(gini)
        class(StochasticPreferences), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: wealth_list(:)
        integer :: i

        allocate(wealth_list(size(this % pop)))
        do i = 1, size(this % pop)
            wealth_list(i) = this % pop(i) % wealth
        end do

        gini = metric_gini_coefficient(wealth_list)
    end function gini_coefficient_wealth

end module model_stochastic_preferences_m

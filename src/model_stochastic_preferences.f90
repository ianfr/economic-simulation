!===============================================================
! model_stochastic_preferences.f90
! Stochastic Preferences Exchange Market implementation
! Based on Silver, Slud, and Takamoto (2002) - reference (5)
!===============================================================
module model_stochastic_preferences_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use sim_base_m
    use abm_metrics_m
    implicit none
    public ! public by default, Agent below is private

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
        real(rk)                 :: alpha = 0.5_rk            ! Conservation parameter for good A (αN total)
        real(rk)                 :: beta = 0.5_rk             ! Conservation parameter for good B (βN total)
        real(rk)                 :: init_good_a = 50.0_rk     ! Initial holdings of good A per agent
        real(rk)                 :: init_good_b = 50.0_rk     ! Initial holdings of good B per agent
        real(rk)                 :: price = 1.0_rk            ! Current market price θ_t
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
        procedure, private :: write_population_binary
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
            this % init_good_a = cfg % init_good_a
            this % init_good_b = cfg % init_good_b
            
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
                this % pop(i) % good_a = this % init_good_a
                this % pop(i) % good_b = this % init_good_b
                
                ! Assign random preference f_it ∈ [0,1]
                call random_number(u)
                this % pop(i) % preference = u
                
                ! Initial wealth calculation (price starts at 1.0)
                this % pop(i) % wealth = this % pop(i) % good_a + this % price * this % pop(i) % good_b
            end do
            
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
        real(rk) :: u

        ! Step 1: Update stochastic preferences (new random values each period)
        do i = 1, size(this % pop)
            call random_number(u)
            this % pop(i) % preference = u
        end do

        ! Step 2: Compute market-clearing price
        call this % compute_market_price()

        ! Step 3: Update agent allocations and wealth
        call this % update_agent_allocations()
    end subroutine step

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
        else
            this % price = 1.0_rk  ! Default fallback price
        end if

        ! Ensure price stays within reasonable bounds
        if (this % price <= 0.0_rk .or. this % price > 1.0e6_rk) then
            this % price = 1.0_rk
        end if
    end subroutine compute_market_price

    !------------------------------------------------------------
    ! Update agent allocations using equations (7) from Silver et al. 2002
    subroutine update_agent_allocations(this)
        class(StochasticPreferences), intent(inout) :: this
        integer(int64) :: i
        real(rk) :: sum_f_a, sum_f_b, sum_one_minus_f_a, sum_one_minus_f_b
        real(rk) :: old_a, old_b, new_a, new_b

        ! Calculate denominators for the allocation formulas
        sum_f_a = 0.0_rk
        sum_f_b = 0.0_rk
        sum_one_minus_f_a = 0.0_rk
        sum_one_minus_f_b = 0.0_rk

        do i = 1, size(this % pop)
            sum_f_a = sum_f_a + this % pop(i) % preference * this % pop(i) % good_a
            sum_f_b = sum_f_b + this % pop(i) % preference * this % pop(i) % good_b
            sum_one_minus_f_a = sum_one_minus_f_a + (1.0_rk - this % pop(i) % preference) * this % pop(i) % good_a
            sum_one_minus_f_b = sum_one_minus_f_b + (1.0_rk - this % pop(i) % preference) * this % pop(i) % good_b
        end do

        ! Update each agent's holdings using equation (7) with safety checks
        do i = 1, size(this % pop)
            old_a = this % pop(i) % good_a
            old_b = this % pop(i) % good_b

            ! Avoid division by zero with larger safety margins
            if (sum_f_b > 1.0e-8_rk .and. sum_one_minus_f_a > 1.0e-8_rk) then
                ! a_it = f_it * a_(i,t-1) + f_it * b_(i,t-1) * Σ(1-f_jt)*a_(j,t-1) / Σf_jt*b_(j,t-1)
                new_a = this % pop(i) % preference * old_a + &
                        this % pop(i) % preference * old_b * sum_one_minus_f_a / sum_f_b

                ! b_it = (1-f_it) * b_(i,t-1) + a_(i,t-1) * Σf_jt*b_(j,t-1) / Σ(1-f_jt)*a_(j,t-1)
                new_b = (1.0_rk - this % pop(i) % preference) * old_b + &
                        old_a * sum_f_b / sum_one_minus_f_a

                ! Safety checks for valid values
                if (new_a >= 0.0_rk .and. new_a < 1.0e6_rk .and. &
                    new_b >= 0.0_rk .and. new_b < 1.0e6_rk) then
                    this % pop(i) % good_a = new_a
                    this % pop(i) % good_b = new_b
                else
                    ! Keep old values if new ones are problematic
                    this % pop(i) % good_a = old_a
                    this % pop(i) % good_b = old_b
                end if
            end if

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
    ! Binary output for faster I/O and Python compatibility
    subroutine write_population_binary(this, folder_name)
        class(StochasticPreferences), intent(in) :: this
        character(*), intent(in) :: folder_name
        integer :: unit, ios, i
        real(rk), allocatable :: good_a_list(:), good_b_list(:), preference_list(:), wealth_list(:)

        ! Create output directory
        call system('mkdir -p ' // folder_name)

        ! Allocate arrays
        allocate(good_a_list(size(this % pop)))
        allocate(good_b_list(size(this % pop)))
        allocate(preference_list(size(this % pop)))
        allocate(wealth_list(size(this % pop)))

        ! Copy data to arrays
        do i = 1, size(this % pop)
            good_a_list(i) = this % pop(i) % good_a
            good_b_list(i) = this % pop(i) % good_b
            preference_list(i) = this % pop(i) % preference
            wealth_list(i) = this % pop(i) % wealth
        end do

        ! Write binary files
        open (newunit=unit, file=trim(folder_name)//'/good_a.bin', access='stream', form='unformatted', status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (unit) good_a_list
            close (unit)
        end if

        open (newunit=unit, file=trim(folder_name)//'/good_b.bin', access='stream', form='unformatted', status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (unit) good_b_list
            close (unit)
        end if

        open (newunit=unit, file=trim(folder_name)//'/preference.bin', access='stream', form='unformatted', status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (unit) preference_list
            close (unit)
        end if

        open (newunit=unit, file=trim(folder_name)//'/wealth.bin', access='stream', form='unformatted', status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (unit) wealth_list
            close (unit)
        end if

        write (*, '(a,a)') 'Population binary data written to folder: ', folder_name
    end subroutine write_population_binary

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

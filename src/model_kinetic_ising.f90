!===============================================================
! kinetic_ising.f90
! Hybrid kinetic-Ising fusion simulation implementation
! Agents trade money only when they form buy-sell pairs (opposite spins)
! Agents also have a small probability of randomly flipping their spin
!===============================================================
module model_kinetic_ising_m
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
        real(rk) :: cash = 0.0_rk
        logical  :: spin = .true.  ! .true. = buy (+1), .false. = sell (-1)
    end type Agent

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: KineticIsing
        type(Agent), allocatable :: pop(:)
        character(len=32), allocatable :: names(:)  ! Metric names
        integer(int64)           :: n_steps = 0
        integer(int64)           :: write_every = 1000
        real(rk)                 :: debt_limit = 0.0_rk  ! Maximum debt allowed
        real(rk)                 :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
        real(rk)                 :: flip_prob = 0.01_rk  ! Probability of random spin flip
    contains
        ! Override the abstract methods
        procedure :: init
        procedure :: run
        procedure :: write_population_csv
        procedure :: step
        procedure :: compute_metrics
        procedure :: get_metric_names

        ! Private helper methods
        procedure, private :: gini_coefficient
        procedure, private :: magnetization
        procedure, private :: spin_correlation
    end type KineticIsing

contains
    subroutine init(this, cfg)
        class(KineticIsing), intent(out) :: this
        class(AbstractConfig), intent(in)  :: cfg
        integer(int64) :: i
        real(rk) :: u

        select type (cfg)
        type is (Config_KineticIsing)
            call init_rng(cfg % seed)
            allocate (this % pop(cfg % n_agents))
            
            ! Initialize agents with random spins and initial cash
            do i = 1, cfg % n_agents
                this % pop(i) % cash = cfg % init_cash
                call random_number(u)
                this % pop(i) % spin = (u < 0.5_rk)  ! Random initial spin
            end do
            
            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % debt_limit = cfg % debt_limit
            this % exchange_delta = cfg % exchange_delta
            this % flip_prob = cfg % flip_prob
        
            allocate(this % names(3))
            this % names(1) = 'gini_coefficient'
            this % names(2) = 'magnetization'
            this % names(3) = 'spin_correlation'
        class default
            error stop 'Unsupported config type in KineticIsing init'
        end select
    end subroutine init

    !------------------------------------------------------------
    subroutine run(this)
        class(KineticIsing), intent(inout) :: this
        integer(int64) :: t, i
        character(len=20) :: filename
        character(len=30) :: metrics_filename
        
        ! Initialize metrics file
        metrics_filename = "out/simulation_metrics.csv"
        call this % write_metrics_header(metrics_filename)
        
        ! Write initial state
        call write_population_csv(this, "out/0.step.csv")
        call this % write_metrics_csv(metrics_filename, 0_int64)

        do t = 1, this % n_steps
            call this % step()
            
            ! Write CSV and metrics only at specified intervals
            if (mod(t, this % write_every) == 0) then
                write(filename, '("out/",i0,".step.csv")') t
                call write_population_csv(this, filename)
                call this % write_metrics_csv(metrics_filename, t)
            end if
        end do
        
        write (*, '(a,i0)') 'Finished simulation.  Steps = ', this % n_steps
        write (*, '("Average cash = ",f10.4)') sum([(this % pop(i) % cash, i=1, size(this % pop))]) &
            / real(size(this % pop), rk)
        write (*, '("Final magnetization = ",f10.4)') this % magnetization()
    end subroutine run

    !------------------------------------------------------------
    subroutine step(this)
        class(KineticIsing), intent(inout) :: this
        integer :: n_pairs, p, i, j, n_agents
        integer, allocatable :: perm(:)
        real(rk), allocatable :: flip_randoms(:)
        
        n_agents = size(this % pop)
        n_pairs = n_agents / 2
        allocate (perm(n_agents))
        allocate (flip_randoms(n_agents))
        
        perm = [(i, i=1, n_agents)]
        call shuffle_int(perm)
        
        ! Generate all random numbers for spin flips sequentially (random_number is not pure)
        do i = 1, n_agents
            call random_number(flip_randoms(i))
        end do
        
        ! Trading phase
        do concurrent (p = 1:n_pairs)
            i = perm(2 * p - 1); j = perm(2 * p)
            
            ! Check if agents have opposite spins and can afford the transaction
            if (this % pop(i) % spin .neqv. this % pop(j) % spin) then
                ! Determine who is buying (spin = .true.) and who is selling (spin = .false.)
                if (this % pop(i) % spin) then
                    ! Agent i is buying, agent j is selling
                    if (this % pop(i) % cash >= this % debt_limit + this % exchange_delta) then
                        this % pop(i) % cash = this % pop(i) % cash - this % exchange_delta
                        this % pop(j) % cash = this % pop(j) % cash + this % exchange_delta
                    end if
                else
                    ! Agent j is buying, agent i is selling
                    if (this % pop(j) % cash >= this % debt_limit + this % exchange_delta) then
                        this % pop(j) % cash = this % pop(j) % cash - this % exchange_delta
                        this % pop(i) % cash = this % pop(i) % cash + this % exchange_delta
                    end if
                end if
            end if
        end do
        
        ! Spin flip phase
        do i = 1, n_agents
            if (flip_randoms(i) < this % flip_prob) then
                this % pop(i) % spin = .not. this % pop(i) % spin
            end if
        end do
    end subroutine step

    !------------------------------------------------------------
    subroutine write_population_csv(this, filename)
        class(KineticIsing), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, i, ios
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(*,'(a,a)') 'Error opening file: ', filename
            return
        end if
        
        ! Write CSV header
        write(unit, '(a)') 'agent_id,cash,spin'
        
        ! Write agent data
        do i = 1, size(this % pop)
            write(unit, '(i0,",",f0.4,",",i0)') i, this % pop(i) % cash, merge(1, 0, this % pop(i) % spin)
        end do
        
        close(unit)
        write(*,'(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(KineticIsing), intent(in) :: this
        real(rk), allocatable :: metrics(:)
        
        allocate(metrics(3))
        metrics(1) = this % gini_coefficient()
        metrics(2) = this % magnetization()
        metrics(3) = this % spin_correlation()
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(KineticIsing), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i
        allocate(result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

    !------------------------------------------------------------
    function gini_coefficient(this) result(gini)
        class(KineticIsing), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: cash_list(:)
        integer :: n, i
        
        n = size(this % pop)
        allocate(cash_list(n))
        
        ! Extract cash values
        do i = 1, n
            cash_list(i) = this % pop(i) % cash
        end do

        gini = metric_gini_coefficient(cash_list)
    end function gini_coefficient

    !------------------------------------------------------------
    function magnetization(this) result(mag)
        class(KineticIsing), intent(in) :: this
        real(rk) :: mag
        integer :: i, n_up
        
        n_up = 0
        do i = 1, size(this % pop)
            if (this % pop(i) % spin) n_up = n_up + 1
        end do
        
        ! Magnetization: (N_up - N_down) / N_total = (2*N_up - N_total) / N_total
        mag = (2.0_rk * real(n_up, rk) - real(size(this % pop), rk)) / real(size(this % pop), rk)
    end function magnetization

    !------------------------------------------------------------
    function spin_correlation(this) result(corr)
        class(KineticIsing), intent(in) :: this
        real(rk) :: corr
        real(rk) :: sum_cash_up, sum_cash_down, mean_cash_up, mean_cash_down
        integer :: n_up, n_down, i
        
        sum_cash_up = 0.0_rk
        sum_cash_down = 0.0_rk
        n_up = 0
        n_down = 0
        
        do i = 1, size(this % pop)
            if (this % pop(i) % spin) then
                sum_cash_up = sum_cash_up + this % pop(i) % cash
                n_up = n_up + 1
            else
                sum_cash_down = sum_cash_down + this % pop(i) % cash
                n_down = n_down + 1
            end if
        end do
        
        if (n_up > 0 .and. n_down > 0) then
            mean_cash_up = sum_cash_up / real(n_up, rk)
            mean_cash_down = sum_cash_down / real(n_down, rk)
            corr = mean_cash_up - mean_cash_down
        else
            corr = 0.0_rk
        end if
    end function spin_correlation

end module model_kinetic_ising_m

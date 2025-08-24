!===============================================================
! conservative_exchange_market.f90
! Conservative Exchange Market implementation
!===============================================================

! References in README.md
! Ref (3) III.B
! Ref (4)

! -------------------------------------------------------------------------------------------------------------------
! Notes: 
!       The lattice here is static, and only used during initialization
!       The lattice is also not completely full (on purpose), and this degree of filling is an input
!       Periodic BCs
!       "Self-connections or double-connections between the same sites are forbidden" (4)
!       "Complete disconnection of one portion of the lattice is avoided" (4)
!       "apart from conservation, we do not impose any limit on the wealth evolution, so any negative value is in principle possible" (4)
!       "In the marketplace, all agents strive to improve their situation. In particular, the poorest agent is the one feeling the strongest pressure to move up the ladder. We
!           model this process via an extremal dynamics" (4)

! We are simulating "Case I" detailed below:
!     " In case I, the rewiring of the lattice is performed at the beginning
!       of the simulation (t = 0). This represents a limit situation in which the commercial
!       environment of each agent does not change with time and also where the commercial
!       links are symmetric." (4)
! -------------------------------------------------------------------------------------------------------------------


module model_conservative_exchange_market_m
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
        real(rk) :: wealth = 0.0_rk                       ! This will be on [0, 1] this time
        integer :: coord = 0                            ! Location on the 1D 'lattice'
        integer, dimension(:), allocatable :: neighbors ! List of nearest 2k neighbor indices on the 'lattice'
    end type Agent

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: ConservativeExchangeMarket
        type(Agent), allocatable :: pop(:)
        character(len=32), allocatable :: names(:)  ! Metric names
        integer :: k = 0                             ! Number of nearest neighbors on each side
        real(rk) :: rewiring_probability = 0.0_rk   ! Probability of rewiring connections
    contains
        ! Override the abstract methods
        procedure :: init
        procedure :: step
        procedure :: write_population_csv
        procedure :: compute_metrics
        procedure :: get_metric_names

        ! Private helper methods
        procedure, private :: gini_coefficient
    end type ConservativeExchangeMarket
contains
    subroutine init(this, cfg)
        class(ConservativeExchangeMarket), intent(out) :: this
        class(AbstractConfig), intent(in)  :: cfg
        integer(int64) :: i

        select type (cfg)
        type is (Config_ConservativeExchangeMarket)
            call init_rng(cfg % seed)
            allocate (this % pop(cfg % n_agents))
            do i = 1, cfg % n_agents
                ! Randomly initialize between 0 and 1
                call random_number(this % pop(i) % wealth)
            end do
            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % k = cfg % k
            this % rewiring_probability = cfg % rewiring_probability
            call init_lattice(this)

            allocate (this % names(1))
            this % names(1) = 'gini_coefficient'
        class default
            error stop 'Unsupported config type in ConservativeExchangeMarket init'
        end select
    end subroutine init

    ! Helper function that generates a random integer between 2 numbers (inclusive)
    function random_int(low, high) result(r)
        integer, intent(in) :: low, high
        integer :: r
        real(rk) :: rand_real
        call random_number(rand_real)
        r = floor(rand_real * (high - low + 1)) + low
    end function random_int

    ! "We assume
    ! that each agent interacts with its 2k nearest neighbors (k to each side) and that, with
    ! probability p, each of these 2k links is disconnected and reconnected to some other
    ! agent chosen at random from the rest of the lattice. The algorithm employed makes
    ! sure that self-connections or double-connections between the same sites are forbidden.
    ! Also, complete disconnection of one portion of the lattice is avoided." (4)
    subroutine init_lattice(this)
        class(ConservativeExchangeMarket), intent(inout) :: this
        integer :: i, j, tmp_neigh_idx
        real(rk) :: trial_wealth

        ! Allocate neighbor lists for each agent
        ! Ensure periodic boundary conditions
        ! Exclude self-connections
        do i = 1, size(this % pop)
            allocate (this % pop(i) % neighbors(2 * this % k))
            ! Assign initial neighbors (to be refined later)
            do j = 1, 2 * this % k
            if (j <= this % k) then
                ! Left neighbors
                this % pop(i) % neighbors(j) = mod(i - j - 1 + size(this % pop), size(this % pop)) + 1
            else
                ! Right neighbors
                this % pop(i) % neighbors(j) = mod(i + j - this % k - 1, size(this % pop)) + 1
            end if
            end do
        end do

        ! Randomly rewire connections with probability p
        ! Exclude self-connections
        do i = 1, size(this % pop)
            do j = 1, 2 * this % k
                call random_number(trial_wealth)
                if (trial_wealth < this % rewiring_probability) then
                    ! Find a new neighbor that is not self and not already a neighbor
                    do
                    tmp_neigh_idx = random_int(1, size(this % pop))
                    if (tmp_neigh_idx /= i .and. .not. any(this % pop(i) % neighbors == tmp_neigh_idx)) then
                        exit
                    end if
                    end do
                    this % pop(i) % neighbors(j) = tmp_neigh_idx
                end if
            end do
        end do

    end subroutine init_lattice

    !------------------------------------------------------------
    ! NOTE: No parallelism here, we only pick 1 agent at a time to match the description in (4)
    subroutine step(this)
        class(ConservativeExchangeMarket), intent(inout) :: this
        integer :: i, poorest_idx, neigh_idx
        real(rk) lowest_wealth, transfer_amount, amount_per_neighbor
        
        ! Find the poorest agent
        lowest_wealth = 1e20
        poorest_idx = 1
        do i = 1, size(this % pop)
            if (this % pop(i) % wealth < lowest_wealth) then
                lowest_wealth = this % pop(i) % wealth
                poorest_idx = i
            end if
        end do

        ! Randomly pick between 0 and 1 how much wealth to transfer to the poorest
        call random_number(transfer_amount)
        ! TODO: Check if this should really be between -1 and 1, i.e. magnitude 0 and 1 but with a +/- sign
        ! transfer_amount = (transfer_amount - 1.0) / 2.0 ! between -1 and 1
        amount_per_neighbor = transfer_amount / real(2 * this % k, rk)

        ! Perform the transfer
        this % pop(poorest_idx) % wealth = this % pop(poorest_idx) % wealth + transfer_amount
        do i = 1, 2 * this % k
            neigh_idx = this % pop(poorest_idx) % neighbors(i)
            this % pop(neigh_idx) % wealth = this % pop(neigh_idx) % wealth - amount_per_neighbor
        end do

    end subroutine step

    !------------------------------------------------------------
    subroutine write_population_csv(this, filename)
        class(ConservativeExchangeMarket), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, i, ios

        open (newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening file: ', filename
            return
        end if

        ! Write CSV header
        write (unit, '(a)') 'agent_id,wealth'

        ! Write agent data
        do i = 1, size(this % pop)
            write (unit, '(i0,",",f0.4)') i, this % pop(i) % wealth
        end do

        close (unit)
        write (*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(ConservativeExchangeMarket), intent(in) :: this
        real(rk), allocatable :: metrics(:)

        allocate (metrics(1))
        metrics(1) = this % gini_coefficient()
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(ConservativeExchangeMarket), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i
        allocate (result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

    !------------------------------------------------------------
    function gini_coefficient(this) result(gini)
        class(ConservativeExchangeMarket), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: wealth_list(:)
        integer :: n, i

        n = size(this % pop)
        allocate (wealth_list(n))

        ! Extract wealth values
        do i = 1, n
            wealth_list(i) = this % pop(i) % wealth
        end do

        gini = metric_gini_coefficient(wealth_list)

    end function gini_coefficient

end module model_conservative_exchange_market_m

!===============================================================
! model_ccm_exchange.f90
! Chakraborti and Chakrabarti 'saved wealth' model with random individual saving propensities
!===============================================================
module model_ccm_exchange_m
    use kinds_m
    use abstract_config_m
    use config_m
    use rng_m
    use sim_base_m
    use abm_metrics_m
    ! use h5fortran
    implicit none
    public ! public by default, Agent below is private

    ! Define the agent privately
    private :: Agent
    type :: Agent
        real(rk) :: cash = 0.0_rk
        real(rk) :: saving_propensity = 0.0_rk ! Individual saving propensity
    end type Agent

    ! The concrete simulation class
    type, extends(AbstractSimulator) :: CCMExchange
        type(Agent), allocatable :: pop(:)
        character(len=32), allocatable :: names(:)  ! Metric names
        integer(int64)           :: n_steps = 0
        integer(int64)           :: write_every = 1000
        real(rk)                 :: debt_limit = 0.0_rk  ! Maximum debt allowed
        real(rk)                 :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
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
        ! procedure, private :: write_population_hdf5
    end type CCMExchange
contains
    subroutine init(this, cfg)
        class(CCMExchange), intent(out) :: this
        class(AbstractConfig), intent(in)  :: cfg
        integer(int64) :: i
        real(rk) :: rnd_prop_uni

        select type (cfg)
        type is (Config_CCMExchange)
            call init_rng(cfg % seed)
            allocate (this % pop(cfg % n_agents))
            do i = 1, cfg % n_agents
                this % pop(i) % cash = cfg % init_cash
                call random_number(rnd_prop_uni)
                this % pop(i) % saving_propensity = (rnd_prop_uni * (cfg % max_saving_propensity - cfg % min_saving_propensity + 1)) + cfg % min_saving_propensity
                ! this % pop(i) % saving_propensity = random_real(cfg % min_saving_propensity, cfg % max_saving_propensity)
            end do
            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % debt_limit = cfg % debt_limit
            this % exchange_delta = cfg % exchange_delta

            allocate (this % names(1))
            this % names(1) = 'gini_coefficient'
        class default
            error stop 'Unsupported config type in SimpleExchange init'
        end select
    end subroutine init
    !------------------------------------------------------------
    subroutine run(this)
        class(CCMExchange), intent(inout) :: this
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
                call this % write_metrics_csv(metrics_filename, t)
                write (filename, '(a,i0,a)') 'out/', t, '.step.csv'
                call write_population_csv(this, filename)
            end if
        end do

        write (*, '(a,i0)') 'Finished simulation.  Steps =', this % n_steps
        write (*, '("Average cash = ",f10.4)') sum([(this % pop(i) % cash, i=1, size(this % pop))]) &
            / real(size(this % pop), rk)
    end subroutine run

    !------------------------------------------------------------
    ! Transition matrix:
    !       | lambda_i + eps*(1-lambda_i)      eps*(1-lambda_j)             |
    !       | (1-eps)*(1-lambda_i)          lambda_j + (1-eps)*(1-lambda_j) |
    !       where lambda is the saving propensity, and eps on [0,1] is randomly chose for every exchange
    subroutine step(this)
        class(CCMExchange), intent(inout) :: this
        integer :: n_pairs, p, i, j
        integer, allocatable :: perm(:)
        real(rk) :: lambda_i, lambda_j, cash_i, cash_j
        real(rk), allocatable :: eps(:)

        ! Generate the random set of pairs of agents
        n_pairs = size(this % pop) / 2
        allocate (perm(size(this % pop)))
        perm = [(i, i=1, size(this % pop))]
        call shuffle_int(perm)

        ! Generate the eps values for each pair randomly
        allocate (eps(n_pairs))
        call random_number(eps)

        ! NOT NEEDED WITH THE GFORTRAN 15.1 UPDATE
        ! For now, run serially with gfortran since it doesn't support `do concurrent` locality yet
        ! https://fortran-lang.discourse.group/t/do-concurrent-can-loop-variable-be-specified-in-local/8396/2
        ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101602
! #ifdef __GFORTRAN__
!         do p = 1, n_pairs
! #else
        do concurrent(p=1:n_pairs) local(lambda_i, lambda_j, cash_i, cash_j)
! #endif
            i = perm(2 * p - 1)
            j = perm(2 * p)

            lambda_i = this % pop(i) % saving_propensity
            lambda_j = this % pop(j) % saving_propensity

            cash_i = this % pop(i) % cash
            cash_j = this % pop(j) % cash

            ! Calculate the new cash values after exchange
            ! TODO: Double-check the formula correctness based on the matrix above
            this % pop(i) % cash = cash_i * (lambda_i + eps(p) * (1.0_rk - lambda_i)) + cash_j * (1.0_rk - eps(p)) * (1.0_rk - lambda_j)
            this % pop(j) % cash = cash_j * (lambda_j + (1.0_rk - eps(p)) * (1.0_rk - lambda_j)) + cash_i * (1.0_rk - eps(p)) * (1.0_rk - lambda_i)

            ! TODO: Do we want a debt limit check here?
            ! If the cash went below the limit, reset it using cash_i and cash_j

        end do
    end subroutine step

    !------------------------------------------------------------
    subroutine write_population_csv(this, filename)
        class(CCMExchange), intent(in) :: this
        character(*), intent(in) :: filename
        integer :: unit, i, ios

        open (newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening file: ', filename
            return
        end if

        ! Write CSV header
        write (unit, '(a)') 'agent_id,cash,saving_propensity'

        ! Write agent data
        do i = 1, size(this % pop)
            write (unit, '(i0,",",f0.4,",",f0.4)') i, this % pop(i) % cash, this % pop(i) % saving_propensity
        end do

        close (unit)
        write (*, '(a,a)') 'Population data written to: ', filename
    end subroutine write_population_csv

    !------------------------------------------------------------
    ! subroutine write_population_hdf5(this, filename)
    !     class(CCMExchange), intent(in) :: this
    !     character(*), intent(in) :: filename

    !     call h5write(filename, '/cash', this % pop % cash)
    !     call h5write(filename, '/saving_propensity', this % pop % saving_propensity)

    ! end subroutine write_population_hdf5

    !------------------------------------------------------------
    function compute_metrics(this) result(metrics)
        class(CCMExchange), intent(in) :: this
        real(rk), allocatable :: metrics(:)

        allocate (metrics(1))
        metrics(1) = this % gini_coefficient()
    end function compute_metrics

    !------------------------------------------------------------
    function get_metric_names(this) result(result_names)
        class(CCMExchange), intent(in) :: this
        character(len=32), allocatable :: result_names(:)
        integer :: i
        allocate (result_names(size(this % names)))
        do i = 1, size(this % names)
            result_names(i) = this % names(i)
        end do
    end function get_metric_names

    !------------------------------------------------------------
    function gini_coefficient(this) result(gini)
        class(CCMExchange), intent(in) :: this
        real(rk) :: gini
        real(rk), allocatable :: cash_list(:)
        integer :: n, i

        n = size(this % pop)
        allocate (cash_list(n))

        ! Extract cash values
        do i = 1, n
            cash_list(i) = this % pop(i) % cash
        end do

        gini = metric_gini_coefficient(cash_list)

    end function gini_coefficient

end module model_ccm_exchange_m

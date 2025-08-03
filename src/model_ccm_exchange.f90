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
        real(rk)                 :: debt_limit = 0.0_rk  ! Maximum debt allowed
        real(rk)                 :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
    contains
        ! Override the abstract methods
        procedure :: init
        procedure :: step
        procedure :: write_population_csv
        procedure :: compute_metrics
        procedure :: get_metric_names

        ! Private helper methods
        procedure, private :: gini_coefficient
        procedure, private :: write_population_binary
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
            end do
            this % n_steps = cfg % n_steps
            this % write_every = cfg % write_every
            this % debt_limit = cfg % debt_limit
            this % exchange_delta = cfg % exchange_delta

            allocate (this % names(1))
            this % names(1) = 'gini_coefficient'
        class default
            error stop 'Unsupported config type in CCMExchange init'
        end select
    end subroutine init

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
        real(rk) :: exchange_result(2)
        real(rk) :: exchange_matrix(2, 2)
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
        do concurrent(p=1:n_pairs) local(lambda_i, lambda_j, cash_i, cash_j, exchange_matrix, exchange_result)
! #endif
            i = perm(2 * p - 1)
            j = perm(2 * p)

            lambda_i = this % pop(i) % saving_propensity
            lambda_j = this % pop(j) % saving_propensity

            cash_i = this % pop(i) % cash
            cash_j = this % pop(j) % cash

            ! Calculate the new cash values after exchange
            exchange_matrix(1, 1) = lambda_i + eps(p) * (1.0_rk - lambda_i)
            exchange_matrix(1, 2) = eps(p) * (1.0_rk - lambda_j)
            exchange_matrix(2, 1) = (1.0_rk - eps(p)) * (1.0_rk - lambda_i)
            exchange_matrix(2, 2) = lambda_j + (1.0_rk - eps(p)) * (1.0_rk - lambda_j)
            exchange_result = apply_exchange_matrix(cash_i, cash_j, exchange_matrix)
            this % pop(i) % cash = exchange_result(1)
            this % pop(j) % cash = exchange_result(2)

            ! If the cash went below the limit, reset it using cash_i and cash_j
            if ((this % pop(i) % cash < -this % debt_limit + this % exchange_delta) .or. (this % pop(j) % cash < -this % debt_limit + this % exchange_delta)) then
                this % pop(i) % cash = cash_i
                this % pop(j) % cash = cash_j
            end if

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
    ! - Custom HDF5-like binary format instead of CSV for population data
    ! - Folder containing binary files
    ! - Each binary file is directly an unformatted array that was formerly a CSV column
    ! - Should be readable with numpy np.fromfile
    subroutine write_population_binary(this, folder_name)
        class(CCMExchange), intent(in) :: this
        character(*), intent(in) :: folder_name

        ! Write cash values to binary file
        call write_list_binary(folder_name, 'cash', this % pop % cash)

        ! Write saving propensity values to binary file
        call write_list_binary(folder_name, 'saving_propensity', this % pop % saving_propensity)

    end subroutine write_population_binary

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

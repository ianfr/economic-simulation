!===============================================================
! kinds_config.f90
! Kind parameters and configuration types
!===============================================================
module kinds_m
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    integer, parameter :: rk = real64
end module kinds_m
!---------------------------------------------------------------

module abstract_config_m
    use kinds_m
    implicit none

    ! Abstract configuration type for simulations
    type, abstract :: AbstractConfig
    contains
        procedure(read_nml_interface), deferred :: read_nml
    end type AbstractConfig

    ! Abstract interface for read_nml procedure
    abstract interface
        subroutine read_nml_interface(this)
            import :: AbstractConfig
            class(AbstractConfig), intent(inout) :: this
        end subroutine read_nml_interface
    end interface

end module abstract_config_m

module config_m
    use kinds_m
    use abstract_config_m
    implicit none

    ! Configuration type for simple exchange simulations
    type, extends(AbstractConfig) :: Config_SimpleExchange
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        real(rk)       :: init_cash = 100.0_rk
        real(rk)       :: debt_limit = 0.0_rk  ! Maximum debt allowed (0.0 = debt-free)
        real(rk)       :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
        integer(int64) :: seed = 20250715
        integer(int64) :: write_every = 1000
    contains
        procedure :: read_nml
    end type Config_SimpleExchange

    ! Configuration type for kinetic-Ising simulations
    type, extends(AbstractConfig) :: Config_KineticIsing
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        real(rk)       :: init_cash = 100.0_rk
        real(rk)       :: debt_limit = 0.0_rk  ! Maximum debt allowed (0.0 = debt-free)
        real(rk)       :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
        real(rk)       :: flip_prob = 0.01_rk  ! Probability of random spin flip
        integer(int64) :: seed = 20250715
        integer(int64) :: write_every = 1000
    contains
        procedure :: read_nml => read_nml_kinetic_ising
    end type Config_KineticIsing

    ! Configuration type for CCM (Chakraborti and Chakrabarti) simulations
    type, extends(AbstractConfig) :: Config_CCMExchange
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        real(rk)       :: init_cash = 100.0_rk
        real(rk)       :: debt_limit = 0.0_rk  ! Maximum debt allowed (0.0 = debt-free)
        real(rk)       :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
        real(rk)       :: min_saving_propensity = 0.1_rk  ! Minimum individual saving propensity
        real(rk)       :: max_saving_propensity = 0.9_rk  ! Maximum individual saving propensity
        integer(int64) :: seed = 20250715
        integer(int64) :: write_every = 1000
    contains
        procedure :: read_nml => read_nml_ccm_exchange
    end type Config_CCMExchange

    ! Configuration type for CEM (Conservative Exchange Market) simulations
    type, extends(AbstractConfig) :: Config_ConservativeExchangeMarket
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        integer(int64) :: seed = 20250715
        integer(int64) :: write_every = 1000
        integer :: k = 1 ! Number of nearest neighbors on each side
        real(rk) :: rewiring_probability = 0.1_rk ! Probability of rewiring connections
    contains
        procedure :: read_nml => read_nml_conservative_exchange_market
    end type Config_ConservativeExchangeMarket

    ! Configuration type for Stochastic Preferences simulations
    type, extends(AbstractConfig) :: Config_StochasticPreferences
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        real(rk)       :: alpha = 0.5_rk           ! Conservation parameter for good A (αN total)
        real(rk)       :: beta = 0.5_rk            ! Conservation parameter for good B (βN total)
        real(rk)       :: init_good_a = 50.0_rk    ! Initial holdings of good A per agent
        real(rk)       :: init_good_b = 50.0_rk    ! Initial holdings of good B per agent
        integer(int64) :: seed = 20250715
        integer(int64) :: write_every = 1000
    contains
        procedure :: read_nml => read_nml_stochastic_preferences
    end type Config_StochasticPreferences

contains

    ! Helper function to write an array to an unformatted binary file
    subroutine write_list_binary(folder_name, filename, data)
        character(*), intent(in) :: filename, folder_name
        real(rk), intent(in) :: data(:)
        integer :: unit, ios
        character(len=100) :: io_emsg
        logical :: folder_exists

        ! Make the output folder if it doesn't exist
        inquire (file=folder_name, exist=folder_exists)
        if (.not. folder_exists) then
            call system('mkdir -p '//trim(folder_name))
        end if

        open (newunit=unit, file=trim(folder_name)//'/'//filename//'.bin', access='stream', form='unformatted', status='replace', action='write', iostat=ios, iomsg=io_emsg)
        if (ios /= 0) then
            write (*, '(a,a)') 'Error opening binary file: ', trim(folder_name)//'/'//filename//'.bin'
            print *, 'Error code: ', ios
            print *, 'Error message: ', trim(io_emsg)
            stop 1
        end if

        write (unit) data
        close (unit)
    end subroutine write_list_binary

    ! Use the type in sim_type.nml to decide which configuration to read
    function create_config() result(cfg)
        class(AbstractConfig), allocatable :: cfg
        character(len=32) :: sim_type
        integer :: unit
        namelist /sim_config/ sim_type

        open (newunit=unit, file='sim_type.nml', status='old', action='read')
        read (unit, nml=sim_config)
        close (unit)

        select case (trim(sim_type))
        case ('SimpleExchange')
            allocate (cfg, source=Config_SimpleExchange())
        case ('KineticIsing')
            allocate (cfg, source=Config_KineticIsing())
        case ('CCMExchange')
            allocate (cfg, source=Config_CCMExchange())
        case ('ConservativeExchangeMarket')
            allocate (cfg, source=Config_ConservativeExchangeMarket())
        case ('StochasticPreferences')
            allocate (cfg, source=Config_StochasticPreferences())
        case default
            error stop 'Unknown simulation type: '//trim(sim_type)
        end select
    end function create_config

    ! Read the configuration from 'in.nml'
    ! Concrete Config_SimpleExchange implementation of AbstractConfig's read_nml
    subroutine read_nml(this)
        class(Config_SimpleExchange), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        real(rk) :: init_cash, debt_limit, exchange_delta

        namelist /run/ n_agents, n_steps, init_cash, debt_limit, exchange_delta, seed, write_every

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            init_cash = this % init_cash
            debt_limit = this % debt_limit
            exchange_delta = this % exchange_delta
            seed = this % seed
            write_every = this % write_every

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            ! Update object with read values
            this % n_agents = n_agents
            this % n_steps = n_steps
            this % init_cash = init_cash
            this % debt_limit = debt_limit
            this % exchange_delta = exchange_delta
            this % seed = seed
            this % write_every = write_every
        end if
    end subroutine read_nml

    ! Read the configuration from 'in.nml' for KineticIsing simulation
    subroutine read_nml_kinetic_ising(this)
        class(Config_KineticIsing), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        real(rk) :: init_cash, debt_limit, exchange_delta, flip_prob

        namelist /run/ n_agents, n_steps, init_cash, debt_limit, exchange_delta, flip_prob, seed, write_every

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            init_cash = this % init_cash
            debt_limit = this % debt_limit
            exchange_delta = this % exchange_delta
            flip_prob = this % flip_prob
            seed = this % seed
            write_every = this % write_every

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            ! Update object with read values
            this % n_agents = n_agents
            this % n_steps = n_steps
            this % init_cash = init_cash
            this % debt_limit = debt_limit
            this % exchange_delta = exchange_delta
            this % flip_prob = flip_prob
            this % seed = seed
            this % write_every = write_every
        end if
    end subroutine read_nml_kinetic_ising

    ! Read the configuration from 'in.nml' for CCMExchange simulation
    subroutine read_nml_ccm_exchange(this)
        class(Config_CCMExchange), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        real(rk) :: init_cash, debt_limit, exchange_delta

        namelist /run/ n_agents, n_steps, init_cash, debt_limit, exchange_delta, seed, write_every

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            init_cash = this % init_cash
            debt_limit = this % debt_limit
            exchange_delta = this % exchange_delta
            seed = this % seed
            write_every = this % write_every

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            ! Update object with read values
            this % n_agents = n_agents
            this % n_steps = n_steps
            this % init_cash = init_cash
            this % debt_limit = debt_limit
            this % exchange_delta = exchange_delta
            this % seed = seed
            this % write_every = write_every
        end if
    end subroutine read_nml_ccm_exchange

    ! Read the configuration from 'in.nml' for Conservative Exchange Market simulation
    subroutine read_nml_conservative_exchange_market(this)
        class(Config_ConservativeExchangeMarket), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        integer :: k
        real(rk) :: rewiring_probability

        namelist /run/ n_agents, n_steps, seed, write_every, k, rewiring_probability

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            seed = this % seed
            write_every = this % write_every
            k = this % k
            rewiring_probability = this % rewiring_probability

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            ! Update object with read values
            this % n_agents = n_agents
            this % n_steps = n_steps
            this % seed = seed
            this % write_every = write_every
            this % k = k
            this % rewiring_probability = rewiring_probability
        end if
    end subroutine read_nml_conservative_exchange_market

    ! Read the configuration from 'in.nml' for Stochastic Preferences simulation
    subroutine read_nml_stochastic_preferences(this)
        class(Config_StochasticPreferences), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        real(rk) :: alpha, beta, init_good_a, init_good_b

        namelist /run/ n_agents, n_steps, seed, write_every, alpha, beta, init_good_a, init_good_b

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            seed = this % seed
            write_every = this % write_every
            alpha = this % alpha
            beta = this % beta
            init_good_a = this % init_good_a
            init_good_b = this % init_good_b

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            ! Update object with read values
            this % n_agents = n_agents
            this % n_steps = n_steps
            this % seed = seed
            this % write_every = write_every
            this % alpha = alpha
            this % beta = beta
            this % init_good_a = init_good_a
            this % init_good_b = init_good_b
        end if
    end subroutine read_nml_stochastic_preferences

end module config_m

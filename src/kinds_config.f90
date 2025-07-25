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

    ! Configuration type for simple exchange simulations
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

contains

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

end module config_m

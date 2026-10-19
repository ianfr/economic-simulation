!  /$$$$$$$            /$$   /$$                                                                                  /$$
! | $$__  $$          | $$  | $$                                                                                 |__/
! | $$  \ $$  /$$$$$$ | $$ /$$$$$$  /$$$$$$$$ /$$$$$$/$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$   /$$$$$$  /$$$$$$/$$$$  /$$  /$$$$$$$  /$$$$$$$
! | $$$$$$$  /$$__  $$| $$|_  $$_/ |____ /$$/| $$_  $$_  $$ |____  $$| $$__  $$| $$__  $$ /$$__  $$| $$_  $$_  $$| $$ /$$_____/ /$$_____/
! | $$__  $$| $$  \ $$| $$  | $$      /$$$$/ | $$ \ $$ \ $$  /$$$$$$$| $$  \ $$| $$  \ $$| $$  \ $$| $$ \ $$ \ $$| $$| $$      |  $$$$$$
! | $$  \ $$| $$  | $$| $$  | $$ /$$ /$$__/  | $$ | $$ | $$ /$$__  $$| $$  | $$| $$  | $$| $$  | $$| $$ | $$ | $$| $$| $$       \____  $$
! | $$$$$$$/|  $$$$$$/| $$  |  $$$$//$$$$$$$$| $$ | $$ | $$|  $$$$$$$| $$  | $$| $$  | $$|  $$$$$$/| $$ | $$ | $$| $$|  $$$$$$$ /$$$$$$$/
! |_______/  \______/ |__/   \___/ |________/|__/ |__/ |__/ \_______/|__/  |__/|__/  |__/ \______/ |__/ |__/ |__/|__/ \_______/|_______/

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
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

    ! Configuration type for simple exchange simulations
    type, extends(AbstractConfig) :: Config_SimpleExchange
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        real(rk)       :: init_cash = 100.0_rk
        real(rk)       :: debt_limit = 0.0_rk  ! Maximum debt allowed (0.0 = debt-free)
        real(rk)       :: exchange_delta = 1.0_rk  ! Amount to exchange between agents
        integer(int64) :: seed = 0
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
        integer(int64) :: seed = 0
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
        ! No default: required namelist inputs. The sentinel fails validate() unless overwritten.
        real(rk)       :: min_saving_propensity = -1.0_rk  ! Minimum individual saving propensity on [0, 1]
        real(rk)       :: max_saving_propensity = -1.0_rk  ! Maximum individual saving propensity on [0, 1]
        integer(int64) :: seed = 0
        integer(int64) :: write_every = 1000
    contains
        procedure :: read_nml => read_nml_ccm_exchange
        procedure :: validate => validate_ccm_exchange
    end type Config_CCMExchange

    ! Configuration type for CCMExchangeTLU simulations (adaptive-threshold agents)
    type, extends(AbstractConfig) :: Config_CCMExchangeTLU
        integer(int64) :: n_agents = 10000
        integer(int64) :: n_steps = 20000
        real(rk)       :: init_cash = 50.0_rk
        real(rk)       :: debt_limit = 0.0_rk
        real(rk)       :: exchange_delta = 1.0_rk
        real(rk)       :: min_saving_propensity = 0.1_rk
        real(rk)       :: max_saving_propensity = 0.9_rk
        integer(int64) :: seed = 20261015
        integer(int64) :: write_every = 500
        character(len=32) :: policy_mode = 'tlu_learning'
        real(rk)       :: lambda_low = 0.2_rk
        real(rk)       :: lambda_high = 0.8_rk
        real(rk)       :: learning_rate = 0.01_rk
        real(rk)       :: policy_temperature = 1.0_rk
        real(rk)       :: weight_init_scale = 0.1_rk
        real(rk)       :: weight_clip = 5.0_rk
        real(rk)       :: reward_clip = 2.0_rk
        real(rk)       :: memory_decay = 0.9_rk
        logical :: use_partner_wealth = .true.
        logical :: use_memory = .true.
    contains
        procedure :: read_nml => read_nml_ccm_exchange_tlu
        procedure :: validate => validate_ccm_exchange_tlu
        procedure :: check_ccm_exchange_tlu
    end type Config_CCMExchangeTLU

    ! Configuration type for CEM (Conservative Exchange Market) simulations
    type, extends(AbstractConfig) :: Config_ConservativeExchangeMarket
        integer(int64) :: n_agents = 1000
        integer(int64) :: n_steps = 10000
        integer(int64) :: seed = 0
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
        real(rk)       :: alpha = 0.5_rk           ! Conservation parameter for good A (alpha*N total)
        real(rk)       :: beta = 0.5_rk            ! Conservation parameter for good B (beta*N total)
        integer(int64) :: seed = 0
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
        case ('CCMExchangeTLU')
            allocate (cfg, source=Config_CCMExchangeTLU())
        case ('ConservativeExchangeMarket')
            allocate (cfg, source=Config_ConservativeExchangeMarket())
        case ('StochasticPreferences')
            allocate (cfg, source=Config_StochasticPreferences())
        case ('CoSimpleExchange')
            allocate (cfg, source=Config_SimpleExchange())
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
        real(rk) :: min_saving_propensity, max_saving_propensity

        namelist /run/ n_agents, n_steps, init_cash, debt_limit, exchange_delta, &
            min_saving_propensity, max_saving_propensity, seed, write_every

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            init_cash = this % init_cash
            debt_limit = this % debt_limit
            exchange_delta = this % exchange_delta
            min_saving_propensity = this % min_saving_propensity
            max_saving_propensity = this % max_saving_propensity
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
            this % min_saving_propensity = min_saving_propensity
            this % max_saving_propensity = max_saving_propensity
            this % seed = seed
            this % write_every = write_every
        end if

        ! Saving propensities must be valid regardless of whether in.nml exists
        call this % validate()
    end subroutine read_nml_ccm_exchange

    ! Validate the CCM saving propensities, which are required namelist inputs:
    ! unset values keep the out-of-range sentinel above and fail here.
    subroutine validate_ccm_exchange(this)
        class(Config_CCMExchange), intent(in) :: this

        if (this % min_saving_propensity < 0.0_rk .or. this % min_saving_propensity > 1.0_rk) &
            error stop 'min_saving_propensity is required in in.nml and must lie on [0, 1]'
        if (this % max_saving_propensity < 0.0_rk .or. this % max_saving_propensity > 1.0_rk) &
            error stop 'max_saving_propensity is required in in.nml and must lie on [0, 1]'
        if (this % min_saving_propensity > this % max_saving_propensity) &
            error stop 'min_saving_propensity must be <= max_saving_propensity'
    end subroutine validate_ccm_exchange

    ! Read the configuration from 'in.nml' for a CCMExchangeTLU simulation
    subroutine read_nml_ccm_exchange_tlu(this)
        class(Config_CCMExchangeTLU), intent(inout) :: this

        logical :: nml_exists
        integer(int64) :: n_agents, n_steps, seed, write_every
        real(rk) :: init_cash, debt_limit, exchange_delta
        real(rk) :: min_saving_propensity, max_saving_propensity
        real(rk) :: lambda_low, lambda_high, learning_rate, policy_temperature
        real(rk) :: weight_init_scale, weight_clip, reward_clip, memory_decay
        character(len=32) :: policy_mode
        logical :: use_partner_wealth, use_memory

        namelist /run/ n_agents, n_steps, init_cash, debt_limit, exchange_delta, &
            min_saving_propensity, max_saving_propensity, seed, write_every, &
            policy_mode, lambda_low, lambda_high, learning_rate, policy_temperature, &
            weight_init_scale, weight_clip, reward_clip, memory_decay, &
            use_partner_wealth, use_memory

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            n_agents = this % n_agents
            n_steps = this % n_steps
            init_cash = this % init_cash
            debt_limit = this % debt_limit
            exchange_delta = this % exchange_delta
            min_saving_propensity = this % min_saving_propensity
            max_saving_propensity = this % max_saving_propensity
            seed = this % seed
            write_every = this % write_every
            policy_mode = this % policy_mode
            lambda_low = this % lambda_low
            lambda_high = this % lambda_high
            learning_rate = this % learning_rate
            policy_temperature = this % policy_temperature
            weight_init_scale = this % weight_init_scale
            weight_clip = this % weight_clip
            reward_clip = this % reward_clip
            memory_decay = this % memory_decay
            use_partner_wealth = this % use_partner_wealth
            use_memory = this % use_memory

            open (unit=10, file='in.nml', status='old', action='read')
            read (10, nml=run)
            close (10)

            this % n_agents = n_agents
            this % n_steps = n_steps
            this % init_cash = init_cash
            this % debt_limit = debt_limit
            this % exchange_delta = exchange_delta
            this % min_saving_propensity = min_saving_propensity
            this % max_saving_propensity = max_saving_propensity
            this % seed = seed
            this % write_every = write_every
            this % policy_mode = policy_mode
            this % lambda_low = lambda_low
            this % lambda_high = lambda_high
            this % learning_rate = learning_rate
            this % policy_temperature = policy_temperature
            this % weight_init_scale = weight_init_scale
            this % weight_clip = weight_clip
            this % reward_clip = reward_clip
            this % memory_decay = memory_decay
            this % use_partner_wealth = use_partner_wealth
            this % use_memory = use_memory
        end if

        call this % validate()
    end subroutine read_nml_ccm_exchange_tlu

    ! Non-stopping validation check for CCMExchangeTLU configs.
    ! Returns an empty string when valid, otherwise a human-readable reason.
    ! The stopping validate() below and the unit tests both use this function.
    function check_ccm_exchange_tlu(this) result(errmsg)
        class(Config_CCMExchangeTLU), intent(in) :: this
        character(len=:), allocatable :: errmsg

        errmsg = ''
        ! Check finiteness before comparisons (NaNs can otherwise pass range
        ! checks, or trap before a useful diagnostic is returned).
        if (.not. all(ieee_is_finite([this % init_cash, this % debt_limit, &
            this % exchange_delta, this % min_saving_propensity, this % max_saving_propensity, &
            this % lambda_low, this % lambda_high, this % learning_rate, &
            this % policy_temperature, this % weight_init_scale, this % weight_clip, &
            this % reward_clip, this % memory_decay]))) then
            errmsg = 'CCMExchangeTLU: all real-valued parameters must be finite'
            return
        end if
        if (this % n_agents < 2) then
            errmsg = 'CCMExchangeTLU: n_agents must be >= 2'
            return
        end if
        if (this % n_steps < 0) then
            errmsg = 'CCMExchangeTLU: n_steps must be >= 0'
            return
        end if
        if (this % write_every < 1) then
            errmsg = 'CCMExchangeTLU: write_every must be >= 1'
            return
        end if
        if (.not. ieee_is_finite(this % init_cash) .or. this % init_cash <= 0.0_rk) then
            errmsg = 'CCMExchangeTLU: init_cash must be positive and finite (mean_cash > 0 required)'
            return
        end if
        if (.not. ieee_is_finite(this % debt_limit) .or. this % debt_limit < 0.0_rk) then
            errmsg = 'CCMExchangeTLU: debt_limit must be >= 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % exchange_delta) .or. this % exchange_delta <= 0.0_rk) then
            errmsg = 'CCMExchangeTLU: exchange_delta must be > 0 and finite'
            return
        end if
        if (this % min_saving_propensity < 0.0_rk .or. this % min_saving_propensity > 1.0_rk) then
            errmsg = 'CCMExchangeTLU: min_saving_propensity must lie on [0, 1]'
            return
        end if
        if (this % max_saving_propensity < 0.0_rk .or. this % max_saving_propensity > 1.0_rk) then
            errmsg = 'CCMExchangeTLU: max_saving_propensity must lie on [0, 1]'
            return
        end if
        if (this % min_saving_propensity > this % max_saving_propensity) then
            errmsg = 'CCMExchangeTLU: min_saving_propensity must be <= max_saving_propensity'
            return
        end if
        select case (trim(this % policy_mode))
        case ('ccm_fixed', 'binary_fixed', 'binary_random', 'heuristic', 'tlu_fixed', 'tlu_learning')
        case default
            errmsg = 'CCMExchangeTLU: unknown policy_mode "'//trim(this % policy_mode)// &
                '"; expected one of ccm_fixed, binary_fixed, binary_random, heuristic, tlu_fixed, tlu_learning'
            return
        end select
        if (.not. ieee_is_finite(this % lambda_low) .or. this % lambda_low < 0.0_rk .or. &
            this % lambda_low > 1.0_rk) then
            errmsg = 'CCMExchangeTLU: lambda_low must lie on [0, 1]'
            return
        end if
        if (.not. ieee_is_finite(this % lambda_high) .or. this % lambda_high < 0.0_rk .or. &
            this % lambda_high > 1.0_rk) then
            errmsg = 'CCMExchangeTLU: lambda_high must lie on [0, 1]'
            return
        end if
        if (this % lambda_low > this % lambda_high) then
            errmsg = 'CCMExchangeTLU: lambda_low must be <= lambda_high'
            return
        end if
        if (.not. ieee_is_finite(this % learning_rate) .or. this % learning_rate < 0.0_rk) then
            errmsg = 'CCMExchangeTLU: learning_rate must be >= 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % policy_temperature) .or. this % policy_temperature <= 0.0_rk) then
            errmsg = 'CCMExchangeTLU: policy_temperature must be > 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % weight_init_scale) .or. this % weight_init_scale < 0.0_rk) then
            errmsg = 'CCMExchangeTLU: weight_init_scale must be >= 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % weight_clip) .or. this % weight_clip <= 0.0_rk) then
            errmsg = 'CCMExchangeTLU: weight_clip must be > 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % reward_clip) .or. this % reward_clip <= 0.0_rk) then
            errmsg = 'CCMExchangeTLU: reward_clip must be > 0 and finite'
            return
        end if
        if (.not. ieee_is_finite(this % memory_decay) .or. this % memory_decay < 0.0_rk .or. &
            this % memory_decay >= 1.0_rk) then
            errmsg = 'CCMExchangeTLU: memory_decay must lie on [0, 1)'
            return
        end if
    end function check_ccm_exchange_tlu

    ! Stopping validation: fail early with a useful message on bad inputs.
    subroutine validate_ccm_exchange_tlu(this)
        class(Config_CCMExchangeTLU), intent(in) :: this
        character(len=:), allocatable :: errmsg

        errmsg = this % check_ccm_exchange_tlu()
        if (len(errmsg) > 0) error stop errmsg
    end subroutine validate_ccm_exchange_tlu

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
        real(rk) :: alpha, beta

        namelist /run/ n_agents, n_steps, seed, write_every, alpha, beta

        inquire (file='in.nml', exist=nml_exists)

        if (nml_exists) then
            ! Initialize with current values
            n_agents = this % n_agents
            n_steps = this % n_steps
            seed = this % seed
            write_every = this % write_every
            alpha = this % alpha
            beta = this % beta

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
        end if
    end subroutine read_nml_stochastic_preferences

end module config_m

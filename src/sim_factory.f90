! This module provides a factory for creating simulator instances at runtime based on configuration files

module sim_factory_m
    use sim_base_m
    use model_simple_exchange_m
    use model_kinetic_ising_m
    implicit none

contains

    ! Create a new simulator instance based on the type specified in 'sim_type.nml' which 
    ! only contains a single entry for the type of simulation to run.
    function create_simulator() result(sim)
        character(len=32) :: sim_type
        integer :: unit
        class(AbstractSimulator), allocatable :: sim
        
        namelist /sim_config/ sim_type
        
        open(newunit=unit, file='sim_type.nml', status='old', action='read')
        read(unit, nml=sim_config)
        close(unit)

        ! https://www.ibm.com/docs/en/xffbg/121.141?topic=control-select-type-construct-fortran-2003
        select case(trim(sim_type))
            case('SimpleExchange')
                allocate(sim, source=SimpleExchange())
            case('KineticIsing')
                allocate(sim, source=KineticIsing())
            case default
                error stop 'Unknown simulation type: '//trim(sim_type)
        end select
    end function create_simulator

end module sim_factory_m
program main

    ! Imports
    use abstract_config_m
    use sim_base_m
    use sim_factory_m
    use config_m

    implicit none

    ! Data
    class(AbstractConfig), allocatable :: cfg
    class(AbstractSimulator), allocatable :: sim

    ! Simulation
    cfg = create_config()          ! Create configuration parser based on 'sim_type.nml'
    sim = create_simulator()       ! Reads 'sim_type.nml' to determine which simulator to create
    call cfg % read_nml()          ! Reads in simulation configuration from 'in.nml'
    call sim % init(cfg)           ! Initialize the simulator instance with the configuration
    call sim % run()               ! Run the simulation
end program main

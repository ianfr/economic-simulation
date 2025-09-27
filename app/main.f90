program main

    ! Imports
    use abstract_config_m
    use sim_base_m
    use sim_factory_m
    use config_m

#ifdef BOLTZ_USE_MPI
#include "../src/mpi.use"
#endif

    implicit none

    ! Data
    class(AbstractConfig), allocatable :: cfg
    class(AbstractSimulator), allocatable :: sim

    ! Initialize MPI (if using)
    ! Also wrap the main code in an if to ensure only rank 0 does output
#ifdef BOLTZ_USE_MPI
    integer :: ierr, rank, nprocs
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
    if (rank == 0) then
#endif

    ! Simulation
    cfg = create_config()          ! Create configuration parser based on 'sim_type.nml'
    sim = create_simulator()       ! Reads 'sim_type.nml' to determine which simulator to create
    call cfg % read_nml()          ! Reads in simulation configuration from 'in.nml'
    call sim % init(cfg)           ! Initialize the simulator instance with the configuration
    call sim % run()               ! Run the simulation

    ! Finalize MPI (if using)
#ifdef BOLTZ_USE_MPI
    end if
    call MPI_Finalize(ierr)
#endif
end program main

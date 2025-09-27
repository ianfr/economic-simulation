export FPM_FC=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/compilers/bin/nvfortran

# See https://forums.developer.nvidia.com/t/nvfortran-c-preprocessor-bug/292217
export FPM_FFLAGS="-O2 -Minline -Mpreprocess -Wall -Werror -Wextra -Minfo=all"

# MPI configuration
export OMPI_FC=$FPM_FC
export MPIFC=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/comm_libs/mpi/bin/mpifort
export MPIRUN=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/comm_libs/mpi/bin/mpirun

# Check if MPI argument is provided
if [ "$1" = "MPI" ]; then
    echo "Setting up MPI environment..."
    export FPM_FC=$MPIFC
    export FPM_FFLAGS="$FPM_FFLAGS -DBOLTZ_USE_MPI"
    # Use CPU multicore parallelism with MPI
    export FPM_FFLAGS="$FPM_FFLAGS -stdpar=multicore"
else
    # Choose GPU vs CPU std paralellism
    # Note: NVIDIA Fortran compiler with GPU acceleration (-stdpar=gpu) doesn't support type-bound procedures on the device (GPU) 
    export FPM_FFLAGS="$FPM_FFLAGS -stdpar=gpu" # GPU
    # export FPM_FFLAGS="$FPM_FFLAGS -stdpar=multicore" # CPU
fi
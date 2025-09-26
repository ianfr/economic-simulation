export FPM_FC=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/compilers/bin/nvfortran

# See https://forums.developer.nvidia.com/t/nvfortran-c-preprocessor-bug/292217
export FPM_FFLAGS="-O2 -Minline -Mpreprocess -Wall -Werror -Wextra -Minfo=all"

# Choose GPU vs CPU paralellism
# Note: NVIDIA Fortran compiler with GPU acceleration (-stdpar=gpu) doesn't support type-bound procedures on the device (GPU) 
export FPM_FFLAGS="$FPM_FFLAGS -stdpar=gpu" # GPU
# export FPM_FFLAGS="$FPM_FFLAGS -stdpar=multicore" # CPU

# For using the bundled openmpi from nvidia HPC SDK
# If MPI is needed, run `export FPM_FC=$MPIFC` before building with fpm
export MPIFC=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/comm_libs/mpi/bin/mpifort
export MPIRUN=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/comm_libs/mpi/bin/mpirun

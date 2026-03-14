# See https://gcc.gnu.org/onlinedocs/gcc-9.4.0/gfortran/Preprocessing-and-conditional-compilation.html

# Make sure that GFORT_CONDA_ENV is defined before running this script
# e.g. with export GFORT_CONDA_ENV=$HOME/miniconda3/envs/gfortran or GFORT_CONDA_ENV=$HOME/anaconda3/envs/gfortran

### Put the compiler path in FPM_FC
export FPM_FC=$GFORT_CONDA_ENV/bin/gfortran
export FPM_CC=$GFORT_CONDA_ENV/bin/gcc
export FPM_CXX=$GFORT_CONDA_ENV/bin/g++

# MPI configuration
export OMPI_FC=$FPM_FC # https://docs.open-mpi.org/en/v5.0.x/building-apps/customizing-wrappers.html
export MPIFC=$GFORT_CONDA_ENV/bin/mpifort
export MPIRUN=$GFORT_CONDA_ENV/bin/mpirun

### Set the Fortran compiler flags
export GFORT_N_CORES=20
export OMP_NUM_THREADS=$GFORT_N_CORES
export FPM_FFLAGS="-O2 -cpp -Wall -Werror -Wextra -ffpe-trap=invalid,zero,overflow,underflow --tree-parallelize-loops=$GFORT_N_CORES -fopt-info-loop -fopenmp -v -free -ffree-line-length-512"
# Use the conda environment's include and lib directories for HDF5 & dependencies
export FPM_FFLAGS="$FPM_FFLAGS -L$GFORT_CONDA_ENV/lib -I$GFORT_CONDA_ENV/include"

# Check if MPI argument is provided
if [ "$1" = "MPI" ]; then
    echo "Setting up MPI environment..."
    export FPM_FC=$MPIFC
    export FPM_FFLAGS="$FPM_FFLAGS -DBOLTZ_USE_MPI"
fi

# If on macOS, revert to actual OpenMP loops for now instead of relying on `do concurrent` magic
# Still not sure why this is a thing, but will figure it out eventually
# if [[ "$(uname)" == "Darwin" ]]; then
#     export FPM_FFLAGS="$FPM_FFLAGS -DBOLTZ_MACOS"
# fi

# For debugging, you might want to use:
# export FPM_FFLAGS="-O0 -g -cpp -Wall -Werror -Wextra -free"


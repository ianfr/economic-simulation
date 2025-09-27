# See https://gcc.gnu.org/onlinedocs/gcc-9.4.0/gfortran/Preprocessing-and-conditional-compilation.html

### Put the compiler path in FPM_FC
# export FPM_FC=gfortran
export FPM_FC=$HOME/miniconda3/envs/gfortran/bin/gfortran

# MPI configuration
export OMPI_FC=$FPM_FC # https://docs.open-mpi.org/en/v5.0.x/building-apps/customizing-wrappers.html
export MPIFC=$HOME/miniconda3/envs/gfortran/bin/mpifort
export MPIRUN=$HOME/miniconda3/envs/gfortran/bin/mpirun

### Set the Fortran compiler flags
export GFORT_N_CORES=8
export FPM_FFLAGS="-O2 -g -cpp -Wall -Werror -Wextra -ffpe-trap=invalid,zero,overflow,underflow --tree-parallelize-loops=$GFORT_N_CORES -free -ffree-line-length-512"
# Use the conda environment's include and lib directories for HDF5 & dependencies
export FPM_FFLAGS="$FPM_FFLAGS -L$HOME/miniconda3/envs/gfortran/lib -I$HOME/miniconda3/envs/gfortran/include"

# Check if MPI argument is provided
if [ "$1" = "MPI" ]; then
    echo "Setting up MPI environment..."
    export FPM_FC=$MPIFC
    export FPM_FFLAGS="$FPM_FFLAGS -DBOLTZ_USE_MPI"
fi

# For debugging, you might want to use:
# export FPM_FFLAGS="-O0 -g -cpp -Wall -Werror -Wextra -free"


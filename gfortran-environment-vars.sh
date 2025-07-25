# See https://gcc.gnu.org/onlinedocs/gcc-9.4.0/gfortran/Preprocessing-and-conditional-compilation.html

### Put the compiler path in FPM_FC
# export FPM_FC=gfortran
export FPM_FC=/home/linuxuser/miniconda3/envs/gfortran/bin/gfortran

### Set the Fortran compiler flags
export GFORT_N_CORES=4
export FPM_FFLAGS="-O2 -cpp -Wall -Werror -Wextra --tree-parallelize-loops=$GFORT_N_CORES -free -ffree-line-length-512"

# For debugging, you might want to use:
# export FPM_FFLAGS="-O0 -g -cpp -Wall -Werror -Wextra -free"
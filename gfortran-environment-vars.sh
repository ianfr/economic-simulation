export FPM_FC=gfortran
export GFORT_N_CORES=4
# See https://gcc.gnu.org/onlinedocs/gcc-9.4.0/gfortran/Preprocessing-and-conditional-compilation.html
export FPM_FFLAGS="-O2 -cpp -Wall -Werror -Wextra --tree-parallelize-loops=$GFORT_N_CORES -free"
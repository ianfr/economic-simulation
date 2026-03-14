# For Debian 13 arm64 with spack opencoarrays setup
# Assumes this has been run: spack load opencoarrays fpm

export FPM_FC=$(which caf) # Confirmed this wraps around spack mpif90 with `caf -s`
export FPM_CC=$(which mpicc) 
export FPM_CXX=$(which mpicxx)

# Note we relax a bit and don't use -Werror with CAF to make the whole project compile
export FPM_FFLAGS="-O2 -cpp -Wall -Wextra -ffpe-trap=invalid,zero,overflow,underflow -fopt-info-loop -v -free -ffree-line-length-512 -DBOLTZ_USE_COARRAY"


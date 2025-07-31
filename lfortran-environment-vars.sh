# NOTE: Unable to use lfortran because of "semantic error: Namelists not implemented yet"
#   https://github.com/lfortran/lfortran/issues/1999
#   Could still be useful if we ever separate out namelists from the rest of the library to use Boltzmannomics interactively with lfortran

# See https://docs.lfortran.org/en/usage/
# See https://lfortran.org/blog/2024/07/lfortran-supports-openmp-pragmas-and-do-concurrent/

### Put the compiler path in FPM_FC
export FPM_FC=/home/linuxuser/miniconda3/envs/lf/bin/lfortran

### Set the Fortran compiler flags
export FPM_FFLAGS="--std=lf --cpp --fast --openmp -v"

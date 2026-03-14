# For Debian 13 arm64 with spack opencoarrays setup
# Assumes this has been run: spack load opencoarrays fpm gcc@13.3.0 scalasca

# Note: It was necessary to install gcc-13-plugin-dev to build scalasca with spack

# Note: 
# ian@Debian13arm64:~/coding/economic-simulation$ cat ~/scorep-gfortran13
# #!/bin/bash
# exec scorep gfortran-13 "$@"

export FPM_FC="$HOME/scorep-gfortran13"
export FPM_CC=$(which mpicc) 
export FPM_CXX=$(which mpicxx)

# Note we relax a bit and don't use -Werror with CAF to make the whole project compile
export FPM_FFLAGS="-fcoarray=lib -lcaf_mpi -g -O2 -cpp -Wall -Wextra -v -free -ffree-line-length-512 -DBOLTZ_USE_COARRAY"

# Trying for scorep
# export CC="gcc-13" #no
# export SCOREP_CC="gcc-13" #no
# yes? below
# mkdir -p ~/bin
# ln -s $(which gcc-13) ~/bin/gcc
# export PATH="$HOME/bin:$PATH"

export CAF_LIB=$(spack location -i opencoarrays)/lib
export FPM_LDFLAGS="-L$CAF_LIB -lcaf_mpi -Wl,-rpath,$CAF_LIB"

export SCOREP_TOTAL_MEMORY=10G
export SCOREP_ENABLE_TRACING=true
export SCOREP_ENABLE_PROFILING=true


# History from (Mar 8 2026) session
# 158  sudo apt remove gcc
# 159  clear
# 160  fpm clean --all
# 161  fpm build
# 162  which gcc-13
# 163  clear
# 164  source scalasca-environment-vars.sh && fpm clean --all && fpm build
# 165  echo $CC
# 166  source scalasca-environment-vars.sh && fpm clean --all && fpm build
# 167  mkdir -p ~/bin
# 168  ln -s $(which gcc-13) ~/bin/gcc
# 169  export PATH="$HOME/bin:$PATH"
# 170  source scalasca-environment-vars.sh && fpm clean --all && fpm build
# 171  clear
# 172  source scalasca-environment-vars.sh && fpm clean --all && fpm build
# 173  cat ~/scorep-gfortran13
# 174  clear
# 175  scalasca -analyze mpirun -n 4 fpm run
# 176  find build -name "Boltzmannomics" -type f
# 177  scalasca -analyze mpirun -n 4 build/scorep-gfortran13_902D1D337519AFE7/app/Boltzmannomics
# 178  export SCOREP_TOTAL_MEMORY=10G
# 179  scalasca -analyze mpirun -n 4 build/scorep-gfortran13_902D1D337519AFE7/app/Boltzmannomics
# 180  rm ./scorep_Boltzmannomics_4_sum/
# 181  rm -rf ./scorep_Boltzmannomics_4_sum/
# 182  clear
# 183  scalasca -analyze mpirun -n 4 build/scorep-gfortran13_902D1D337519AFE7/app/Boltzmannomics
# 184  scalasca -examine -s scorep_Boltzmannomics_4_sum
# 185  scorep-score scorep_Boltzmannomics_4_sum/profile.cubex
# 186  scorep-score -r scorep_Boltzmannomics_4_sum/profile.cubex | grep MPI
# 187  history | tail -n 30
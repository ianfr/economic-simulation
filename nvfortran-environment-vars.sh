export FPM_FC=/opt/nvidia/hpc_sdk/Linux_x86_64/25.5/compilers/bin/nvfortran

# See https://forums.developer.nvidia.com/t/nvfortran-c-preprocessor-bug/292217
export FPM_FFLAGS="-O2 -Minfo=accel -Mpreprocess -Wall -Werror -Wextra"

# Choose GPU vs CPU paralellism
# Note: NVIDIA Fortran compiler with GPU acceleration (-stdpar=gpu) doesn't support type-bound procedures on the device (GPU) 
export FPM_FFLAGS="$FPM_FFLAGS -stdpar=gpu" # GPU
# export FPM_FFLAGS="$FPM_FFLAGS -stdpar=multicore" # CPU
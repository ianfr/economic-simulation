# BUILD: docker build -t ifriedri/economic-simulation-runtime:latest --progress=plain .
# RUN: docker run --rm -it -v ${PWD}/..:/src -w /src --gpus all ifriedri/economic-simulation-runtime:latest
# PUSH: docker push ifriedri/economic-simulation-runtime:latest

# This will make sure nvfortran is available
FROM nvcr.io/nvidia/nvhpc:25.5-devel-cuda12.9-ubuntu24.04
SHELL ["/bin/bash", "-c"]

# Install Python post-processing dependencies system-wide
RUN apt update
RUN apt install -y python3 python3-scipy python3-numpy python3-matplotlib python3-pandas python3-statsmodels libopenmpi-dev

# Install fpm to ~
RUN wget https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12 -O ~/fpm
RUN chmod +x ~/fpm
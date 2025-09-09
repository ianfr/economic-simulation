# BUILD: docker build -t ifriedri/economic-simulation-runtime:latest --progress=plain .
# RUN: docker run --rm -it -v ${PWD}/..:/src -w /src --gpus all ifriedri/economic-simulation-runtime:latest
# PUSH: docker push ifriedri/economic-simulation-runtime:latest

# This will make sure nvfortran is available
FROM nvcr.io/nvidia/nvhpc:25.5-devel-cuda12.9-ubuntu24.04
SHELL ["/bin/bash", "-c"]

# Install Python post-processing dependencies system-wide
RUN apt update
RUN apt install -y python3 python3-scipy python3-numpy python3-matplotlib python3-pandas python3-statsmodels

# Install fpm to ~
RUN wget https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12 -O ~/fpm
RUN chmod +x ~/fpm

### Install Miniconda & create conda env ###
# See https://www.anaconda.com/docs/getting-started/miniconda/install#linux-2

# RUN mkdir -p ~/miniconda3
# RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh
# RUN bash ~/miniconda3/miniconda.sh -b -u -p /opt/miniconda
# RUN rm ~/miniconda3/miniconda.sh

# ENV PATH="/opt/miniconda/bin:${PATH}"

# RUN conda init bash
# RUN conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main
# RUN conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r
# RUN source ~/.bashrc

# RUN cat ~/.bashrc 

# RUN source /opt/miniconda/etc/profile.d/conda.sh

# RUN apt update && apt install -y curl

# # Install our public GPG key to trusted store
# RUN curl https://repo.anaconda.com/pkgs/misc/gpgkeys/anaconda.asc | gpg --dearmor > conda.gpg
# RUN install -o root -g root -m 644 conda.gpg /usr/share/keyrings/conda-archive-keyring.gpg

# # Check whether fingerprint is correct (will output an error message otherwise)
# RUN gpg --keyring /usr/share/keyrings/conda-archive-keyring.gpg --no-default-keyring --fingerprint 34161F5BF5EB1D4BFBBB8F0A8AEB4F8B29D82806

# # Add our Debian repo
# RUN echo "deb [arch=amd64 signed-by=/usr/share/keyrings/conda-archive-keyring.gpg] https://repo.anaconda.com/pkgs/misc/debrepo/conda stable main" > /etc/apt/sources.list.d/conda.list

# RUN apt update
# RUN apt install conda -y

# RUN . /opt/conda/etc/profile.d/conda.sh
# # RUN ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d

# ENV PATH="/opt/conda/bin:${PATH}"

# RUN conda init bash
# RUN source /root/.bashrc

# COPY docker-create-conda-envs.sh /tmp/docker-create-conda-envs.sh
# RUN bash /tmp/docker-create-conda-envs.sh

# # Create the env containing the right version of gfortran
# RUN conda create -n gfortran
# RUN conda activate gfortran
# RUN conda install -y conda-forge::gfortran=15.1.0 conda-forge::fpm 
# RUN conda deactivate

# # Create the env for Python post-processing
# RUN conda create -n analysis python=3.12
# RUN conda activate analysis
# RUN conda install -c conda-forge pandas matplotlib scipy statsmodels numpy
# RUN conda deactivate

# RUN cat ~/.bashrc

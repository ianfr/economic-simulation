#!/bin/bash

# see fprettify -h for description of options

# Ensure the path to the conda environment with fortls and fprettify installed is set like:
# export CONDA_FORTLS_ENV=/home/linuxuser/miniconda3/envs/fortran-fortls or
# export CONDA_FORTLS_ENV=/Users/ian/anaconda3/envs/fortran-fortls

$CONDA_FORTLS_ENV/bin/fprettify \
    src/*.f90 app/*.f90 \
    --indent 4 \
    --line-length 300 \
    --whitespace 4

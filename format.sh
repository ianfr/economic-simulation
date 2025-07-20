#!/bin/bash

# see fprettify -h for description of options

/home/linuxuser/miniconda3/envs/fortran-fortls/bin/fprettify \
    src/*.f90 app/*.f90 \
    --indent 4 \
    --line-length 300 \
    --whitespace 4

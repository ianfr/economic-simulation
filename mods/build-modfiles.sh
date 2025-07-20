#!/bin/sh
# This script builds the Fortran source files into object files and modfiles for intellisense

rm *.o *.mod
gfortran -c ../src/kinds_config.f90
gfortran -c ../src/abm_metrics.f90
gfortran -c ../src/rng.f90
gfortran -c ../src/sim_base.f90
gfortran -c ../src/kinetic_ising.f90
gfortran -c ../src/simple_exchange.f90
gfortran -c ../src/sim_factory.f90
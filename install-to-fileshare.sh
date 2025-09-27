#!/bin/bash
# For usage with the future parallel 'study' runner tools on an SSH/MPI cluster
# Assumes the executable has already been built

# Path to the folder on the shared file system for all the nodes/workers
export FILESHARE_PATH=/home/linuxuser/fshare/sim_original

# Copy the executable to the shared file system
# The parallel runner will then create copies of this for each case, changing the .nml configs as needed for each case
mkdir -p $FILESHARE_PATH
rm -rf $FILESHARE_PATH/*
~/fpm install --prefix $FILESHARE_PATH
mkdir -p $FILESHARE_PATH/bin/out # '/bin/' is a quirk of fpm install
mkdir -p $FILESHARE_PATH/bin/postprocess
cp postprocess/analyze.py $FILESHARE_PATH/bin/postprocess
cp postprocess/analyze_config.json $FILESHARE_PATH/bin/postprocess
cp *.nml $FILESHARE_PATH/bin
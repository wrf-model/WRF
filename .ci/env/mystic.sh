#!/bin/sh

echo "Setting up mmm-mystic environment"

# use python virtaul env
. $HOME/venv/ncwrf/bin/activate

# Add specific libraries to our setup
export PATH=$HOME/dependencies/openmpi/bin:$PATH
export NETCDF=$HOME/dependencies/netcdf_for_wrf/
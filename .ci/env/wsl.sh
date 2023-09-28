#!/bin/sh

echo "Setting up WSL environment"

export NETCDF=/opt/ncar/netcdf
export MPICH=/opt/ncar/mpich
export PATH=$PATH:$NETCDF/bin

# use python virtaul env
. /opt/ncar/netcdf-python/bin/activate
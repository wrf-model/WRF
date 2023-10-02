#!/bin/sh

echo "Setting up WSL environment"

export NETCDF=/opt/ncar/netcdf
export MPI=/opt/ncar/openmpi-4.0.0
export PATH=$MPI/bin/:$NETCDF/bin:$PATH:

# use python virtaul env
. /opt/ncar/netcdf-python/bin/activate
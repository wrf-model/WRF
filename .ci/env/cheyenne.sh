#!/bin/sh

echo "Setting up cheyenne environment"
. /etc/profile
cmd="module purge"
echo $cmd && eval "${cmd}"
setup=$1

cmd="module load cmake/3.22.0"
echo $cmd && eval "${cmd}"

if [ $( contains $setup gnu ) -eq 0 ]; then
  cmd='module load gnu'
elif [ $( contains $setup intel ) -eq 0 ]; then
  cmd="module load intel"
elif [ $( contains $setup pgi ) -eq 0 ]; then
  cmd="module load pgi"
elif [ $( contains $setup nvhpc ) -eq 0 ]; then
  cmd="module load nvhpc"
fi

echo $cmd && eval "${cmd}"

if [ $( contains $setup mpi ) -eq 0 ]; then
  cmd="module load mpt"
  echo $cmd && eval "${cmd}"
fi

cmd="module load netcdf"
echo $cmd && eval "${cmd}"
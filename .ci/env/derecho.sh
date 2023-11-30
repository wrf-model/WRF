#!/bin/sh

echo "Setting up derecho environment"
. /etc/profile.d/z00_modules.sh
echo "Loading modules : $*"
cmd="module purge"
echo $cmd && eval "${cmd}"

# We should be handed in the modules to load
while [ $# -gt 0 ]; do 
  cmd="module load $1"
  echo $cmd && eval "${cmd}"
  shift
done

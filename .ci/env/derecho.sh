#!/bin/sh

echo "Setting up derecho environment"
workingDirectory=$PWD
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

#  Go back to working directory if for unknown reason HPC config changing your directory on you
if [ "$workingDirectory" != "$PWD" ]; then
  echo "derecho module loading changed working directory"
  echo "  Moving back to $workingDirectory"
  cd $workingDirectory
fi

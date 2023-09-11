#!/bin/sh
# https://stackoverflow.com/a/8811800
# contains(string, substring)
#
# Returns 0 if the specified string contains the specified substring,
# otherwise returns 1.
contains()
{
  string="$1"
  substring="$2"
  
  if [ "${string#*"$substring"}" != "$string" ]; then
    echo 0    # $substring is in $string
  else
    echo 1    # $substring is not in $string
  fi
}

# Better than uname and what we use in the HPC Workflows
hostname=$( python3 -c "import socket; print( socket.getfqdn() )" )

if [ "${hostname}" = "polaris" ]; then
  # WSL Agent
  . .ci/env/wsl.sh $*
elif [ $( contains ${hostname} cheyenne ) -eq 0 ]; then
  # Cheyenne HPC SuSE PBS
  . .ci/env/cheyenne.sh
else
  echo "No known environment for '${hostname}', using current"
fi
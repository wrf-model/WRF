#!/bin/sh

# Better than uname and what we use in the HPC Workflows
hostname=$( python3 -c "import socket; print( socket.getfqdn() )" )

if [ $( contains ${hostname} polaris ) -eq 0 ]; then
  # WSL Agent
  . .ci/env/wsl.sh $*
elif [ $( contains ${hostname} cheyenne ) -eq 0 ]; then
  # Cheyenne HPC SuSE PBS
  . .ci/env/cheyenne.sh
else
  echo "No known environment for '${hostname}', using current"
fi
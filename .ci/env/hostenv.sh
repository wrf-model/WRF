#!/bin/sh

hostname=$( uname -n )

if [ "${hostname}" == "polaris" ]; then
  # WSL Agent
  source ./wsl.sh
elif [ "${hostname}" == *"cheyenne"* ]; then
  # Cheyenne HPC SuSE PBS
  echo "not set yet"
fi
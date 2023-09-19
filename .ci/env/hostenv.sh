#!/bin/sh

# Useful string manipulation functions, leaving in for posterity
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

# Assumes contains() already returns true and substring exists
# modules will be listed as moduleA/version-moduleB/version-moduleC/version
specificModule()
{
  string="$1"
  substring="$2"
  # 
  spec=$( echo "${string}" | sed -e "s@.*\($substring/\?\([^-]\+\)\?\)\(-\|$\).*@\1@" )

  echo $spec
}

#
# Once a module is loaded, to avoid potential duplicates remove it from the string
filterModule()
{
  string="$1"
  substring="$2"

  filt=$( echo "${string}" | sed -e "s@$substring@@" )

  echo $filt
}

setenvStr()
{
  # Changing IFS produces the most consistent results
  tmpIFS=$IFS
  IFS=","
  string="$1"
  for s in $string; do
    echo "export $s"
    export "$s"
  done
  IFS=$tmpIFS
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
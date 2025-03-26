#!/bin/sh

# Allow selection of hostname, and if none is provided use the current machine
# While this may seem unintuitive at first, it provides the flexibility of using
# "named" configurations without being explicitly tied to fqdn
hostname=$AS_HOST
if [ -z "$hostname" ]; then
  hostname=$( python3 -c "import socket; print( socket.getfqdn() )" )
fi

if [ $( contains ${hostname} hsn.de.hpc ) -eq 0 ]; then
  # Derecho HPC SuSE PBS
  . .ci/env/derecho.sh
else
  echo "No known environment for '${hostname}', using current"
fi

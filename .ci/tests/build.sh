#!/bin/sh
workingDirectory=$1
shift
compileOption=$1
makejobs=$2

cd $workingDirectory

. .ci/env/hostenv.sh

./clean -a
echo "$compileOption" | ./configure

if [ ! -f configure.wrf ]; then
  echo  "Failed to configure"
  exit 1
fi

./compile em_real $makejobs

result=$?

if [ $result -eq 0 ]; then
  echo "TEST $(basename $0) PASS"
fi
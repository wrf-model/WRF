#!/bin/sh
workingDirectory=$1
shift
core=$1
compileOption=$2
makejobs=$3
cd $workingDirectory

# Everything else goes to our env setup
shift; shift; shift
. .ci/env/hostenv.sh $*

./clean -a
./configure << EOF
$compileOption
EOF

if [ ! -f configure.wrf ]; then
  echo  "Failed to configure"
  exit 1
fi

./compile $core $makejobs

result=$?

if [ $result -eq 0 ]; then
  echo "TEST $(basename $0) PASS"
fi
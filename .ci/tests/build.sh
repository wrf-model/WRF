#!/bin/sh
help()
{
  echo "./build.sh [workingdir] [options] [-- <hostenv.sh options>]"
  echo "  [workingdir]              First argument must be the working dir to immediate cd to"
  echo "  -c                        Configuration dochere string, piped directly into configure"
  echo "  -b                        Build command passed into compile"
  echo "  -e                        environment variables in comma-delimited list, e.g. var=1,foo,bar=0"
  echo "  -- <hostenv.sh options>   Directly pass options to hostenv.sh, equivalent to hostenv.sh <options>"
  echo "  -h                  Print this message"
}

workingDirectory=$1
shift
cd $workingDirectory

while getopts c:b:e:h opt; do
  case $opt in
    c)
      configCommand=$OPTARG
    ;;
    b)
      buildCommand=$OPTARG
    ;;
    e)
      envVars="$OPTARG"
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

shift "$((OPTIND - 1))"

# Everything else goes to our env setup
. .ci/env/hostenv.sh $*

# Set any environment variables
if [ ! -z $envVars ]; then
  setenvStr "$envVars"
fi

./clean -a
./configure << EOF
$configCommand
EOF

if [ ! -f configure.wrf ]; then
  echo  "Failed to configure"
  exit 1
fi

./compile $buildCommand

result=$?

if [ $result -eq 0 ]; then
  echo "TEST $(basename $0) PASS"
fi
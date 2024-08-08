#!/bin/sh
help()
{
  echo "./build.sh as_host workingdir [options] [-- <hostenv.sh options>]"
  echo "  as_host                   First argument must be the host configuration to use for environment loading"
  echo "  workingdir                First argument must be the working dir to immediate cd to"
  echo "  -c                        Configuration build type, piped directly into configure"
  echo "  -n                        Configuration nesting type, piped directly into configure"
  echo "  -o                        Configuration optstring passed into configure"
  echo "  -b                        Build command passed into compile"
  echo "  -e                        environment variables in comma-delimited list, e.g. var=1,foo,bar=0"
  echo "  -- <hostenv.sh options>   Directly pass options to hostenv.sh, equivalent to hostenv.sh <options>"
  echo "  -h                  Print this message"
  echo ""
  echo "If you wish to use an env var in your arg such as '-c \$SERIAL -e SERIAL=32', you must"
  echo "you will need to do '-c \\\$SERIAL -e SERIAL=32' to delay shell expansion"
}

echo "Input arguments:"
echo "$*"

AS_HOST=$1
shift
if [ $AS_HOST = "-h" ]; then
  help
  exit 0
fi

workingDirectory=$1
shift

cd $workingDirectory

# Get some helper functions
. .ci/env/helpers.sh

while getopts c:n:o:b:e:h opt; do
  case $opt in
    c)
      configuration="$OPTARG"
    ;;
    n)
      nesting="$OPTARG"
    ;;
    o)
      configOpt="$OPTARG"
    ;;
    b)
      buildCommand="$OPTARG"
    ;;
    e)
      envVars="$envVars,$OPTARG"
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

# Now evaluate env vars in case it pulls from hostenv.sh
if [ ! -z "$envVars" ]; then
  setenvStr "$envVars"
fi

# Re-evaluate input values for delayed expansion
eval "configuration=\"$configuration\""
eval "nesting=\"$nesting\""
eval "configOpt=\"$configOpt\""
eval "buildCommand=\"$buildCommand\""

./clean -a

echo "Compiling with option $configuration nesting=$nesting and additional flags '$configOpt'"
./configure $configOpt << EOF
$configuration
$nesting
EOF

if [ ! -f configure.wrf ]; then
  echo  "Failed to configure"
  exit 1
fi

echo "./compile $buildCommand"
./compile $buildCommand

result=$?

if [ $result -ne 0 ]; then
  echo "Failed to compile"
  exit 1
fi

# And a *very* special check because WRF compiles the WRF way and force-ignores all make errors
# putting the onus on US to check for things
if [ ! -x ./main/wrf.exe ]; then # There's a bunch of other execs but this is the most important and 
                                 # doing more checks to accomodate just reinforces this bad design
  echo "Failed to compile"
  exit 1
fi

echo "TEST $(basename $0) PASS"

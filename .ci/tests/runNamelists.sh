#!/bin/sh
help()
{
  echo "./runNamelists.sh [workingdir] [options] [-- <hostenv.sh options>]"
  echo "  [workingdir]              First argument must be the working dir to immediate cd to"
  echo "  -b                        Binary executable for WRF front-end (ideal/real/etc.)"
  echo "  -f                        Namelist folder to look for namelists under"
  echo "  -n                        Comma-delimited list of namelists to test, each of which will in serial be run by replacing namelist.input"
  echo "  -d                        Data directory to link into core directory"
  echo "  -p                        Parallel launch command (MPI)"
  echo "  -e                        environment variables in comma-delimited list, e.g. var=1,foo,bar=0"
  echo "  -- <hostenv.sh options>   Directly pass options to hostenv.sh, equivalent to hostenv.sh <options>"
  echo "  -h                  Print this message"
  echo ""
  echo "If you wish to use an env var in your arg such as '-c \$SERIAL -e SERIAL=32', you must"
  echo "you will need to do '-c \\\$SERIAL -e SERIAL=32' to delay shell expansion"
}

workingDirectory=$1
shift
if [ $workingDirectory == "-h" ]; then
  help
  exit 0
fi
cd $workingDirectory

# Get some helper functions
. .ci/env/helpers.sh

while getopts c:b:f:n:d:e:h opt; do
  case $opt in
    c)
      coreDir="$OPTARG"
    ;;
    b)
      binFirst="$OPTARG"
    ;;
    f)
      namelistFolder="$OPTARG"
    ;;
    n)
      namelists="$OPTARG"
    ;;
    d)
      data="$OPTARG"
    ;;
    p)
      parallelExec="$OPTARG"
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
if [ ! -z $envVars ]; then
  setenvStr "$envVars"
fi

# Re-evaluate input values for delayed expansion
eval "coreDir=\$( realpath \"$coreDir\" )"
eval "binFirst=\$( realpath \"$coreDir/$binFirst\" )"
eval "namelistFolder=\$( realpath \"$namelistFolder\" )"
eval "namelists=\"$namelists\""
eval "data=\$( realpath \"$data\" )"
eval "parallelExec=\"$parallelExec\""

wrf=$( realpath $coreDir/wrf.exe )

################################################################################
#
# Things done only once
# Go to core dir
cd $coreDir || exit $?

# Link in data in here
ln -sf $data/* .

#
################################################################################


################################################################################
#
# Loop testing namelists
tmpIFS=$IFS
IFS=","

for namelist in $namelists; do
  banner 42 "START $namelist"

  # Clean up run
  rm wrfinput_d* wrfbdy_d* wrfout_d* wrfchemi_d* wrf_chem_input_d* rsl* real.print.out* wrf.print.out* -rf

  # Copy namelist
  echo "Setting $namelistFolder/$namelist as namelist.input"
  cp $namelistFolder/$namelist namelist.input || exit $?

  # Run setup
  echo "Running $parallelExec $binFirst"
  $parallelExec $binFirst

  result=$?
  if [ $result -ne 0 ]; then
    echo "$parallelExec $binFirst failed"
    exit $result
  fi

  # run wrf
  echo "Running $parallelExec $wrf"
  $parallelExec $wrf
  result=$?
  if [ $result -ne 0 ]; then
    echo "$parallelExec $wrf failed"
    exit $result
  fi

  banner 42 "STOP $namelist"
done
IFS=$tmpIFS
# Unlink everything we linked in
ls $data/ | xargs -I{} rm {}

# Clean up once more since we passed
rm -rf wrfinput_d* wrfbdy_d* wrfout_d* wrfchemi_d* wrf_chem_input_d* rsl* real.print.out* wrf.print.out*

# We passed!
echo "TEST $(basename $0) PASS"
#!/bin/sh
help()
{
  echo "./runNamelists.sh [workingdir] [options] [-- <hostenv.sh options>]"
  echo "  [workingdir]              First argument must be the working dir to immediate cd to"
  echo "  -c <folder>               Directory for specific core built"
  echo "  -b <exec>                 Binary executable for WRF front-end (ideal/real/etc.)"
  echo "  -f <folder>               Namelist folder to look for namelists under"
  echo "  -n <fileA,fileB,...>      Comma-delimited list of namelists to test, each of which will in serial be run by replacing namelist.input"
  echo "  -d <folder>               Data directory to link into core directory"
  echo "  -p <mpirun cmd>           Parallel launch command (MPI), e.g. mpirun, mpiexec_mpt, mpiexec -np 8 --oversubscribe"
  echo "  -s <folder>               Save result data to prefix location, full path for run constructed as <work>/<thisfolder>/<namelist>/ "
  echo "  -i <folder>               Folder for bitwise-identical results, full path for run constructed as <work>/<thisfolder>/<namelist>/ "
  echo "  -e <varA=val,varB,...>    environment variables in comma-delimited list, e.g. var=1,foo,bar=0"
  echo "  -- <hostenv.sh options>   Directly pass options to hostenv.sh, equivalent to hostenv.sh <options>"
  echo "  -h                  Print this message"
  echo ""
  echo "If you wish to use an env var in your arg such as '-c \$SERIAL -e SERIAL=32', you must"
  echo "you will need to do '-c \\\$SERIAL -e SERIAL=32' to delay shell expansion when input from shell/CLI"
  echo ""
  echo "If -i <folder> is provided, bitwise-identical checks are performed as part of checks"
}

workingDirectory=$1
shift
if [ $workingDirectory = "-h" ]; then
  help
  exit 0
fi
cd $workingDirectory

# Get some helper functions
. .ci/env/helpers.sh

while getopts c:b:f:n:d:p:s:i:e:h opt; do
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
    s)
      moveFolder="$OPTARG"
    ;;
    i)
      identicalFolder="$OPTARG"
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
eval "moveFolder=\"$moveFolder\""
eval "identicalFolder=\"$identicalFolder\""

wrf=$( realpath $coreDir/wrf.exe )
rd_12_norm=$( realpath .ci/tests/SCRIPTS/rd_l2_norm.py )

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
  rm wrfinput_d* wrfbdy_d* wrfout_d* wrfchemi_d* wrf_chem_input_d* rsl* real.print.out* wrf.print.out* wrf_d0*_runstats.out -rf

  # Copy namelist
  echo "Setting $namelistFolder/$namelist as namelist.input"
  cp $namelistFolder/$namelist namelist.input || exit $?

  # Run setup
  echo "Running $parallelExec $binFirst"
  # Go through echo to effectively "split" on spaces
  eval "$parallelExec $binFirst"

  result=$?
  if [ $result -ne 0 ]; then
    echo "$parallelExec $binFirst failed"
    exit $result
  fi

  # run wrf
  echo "Running $parallelExec $wrf"
  # Go through echo to effectively "split" on spaces
  eval "$parallelExec $wrf"
  result=$?
  if [ $result -ne 0 ]; then
    echo "$parallelExec $wrf failed"
    exit $result
  fi

  # Check output per domain
  maxDom=$( grep max_dom namelist.input |  awk '{print $3}' | tr -d ',' )
  currentDom=0
  while [ $currentDom -lt $maxDom ]; do
    currentDom=$(( $currentDom + 1 ))

    domFiles=$( ls -1 | grep wrfout_d0${currentDom} | wc -l | awk '{print $1}' )

    if [ $domFiles -eq 0 ]; then
      echo "No output files generated for domain $currentDom"
      exit 123
    fi

    ncdump wrfout_d0${currentDom}_* | grep -i nan | grep -vi dominant
    okNan=$?
    timeSteps=$( ncdump -h wrfout_d0${currentDom}_* | grep "Time = UNLIMITED" | cut -d"(" -f2 | awk '{print $1}' )

    if [ $okNan -eq 1 ] && [ $timeSteps -eq 2 ]; then
      # Super ok, store output in file for comparison later
      python3 $rd_12_norm wrfout_d0${currentDom}_* > wrf_d0${currentDom}_runstats.out
    else
      # Super NOT ok
      echo "Checks on output failed. okNan=$okNan, timeSteps=$timeSteps when expected okNan=1 && timeSteps=2"
      exit 123
    fi
  done

  # If we have a move folder, do that
  if [ ! -z $moveFolder ]; then
    mkdir -p $workingDirectory/$moveFolder/$namelist/


    # we are in core dir, find our output
    find . -type f -name "wrfinput_d*"       \
                -o -name "wrfbdy_d*"         \
                -o -name "wrfout_d*"         \
                -o -name "wrfchemi_d*"       \
                -o -name "wrf_chem_input_d*" \
                -o -name "rsl*"              \
                -o -name "real.print.out*"   \
                -o -name "wrf.print.out*"    \
                -o -name "wrf_d0*_runstats.out" \
                -exec mv {} $workingDirectory/$moveFolder/$namelist/ \;

  # else try comp
  elif [ ! -z $identicalFolder ]; then
    if [ -d $workingDirectory/$identicalFolder/$namelist/ ]; then
      for dom in d01 d02 d03; do
        if [ -e $workingDirectory/$identicalFolder/$namelist/wrf_${dom}_runstats.out ]; then
          # We have a domain to check - should exist in both (we are treating the identical folder as truth)
          diff $workingDirectory/$identicalFolder/$namelist/wrf_${dom}_runstats.out wrf_${dom}_runstats.out > /dev/null
          result=$?
          if [ $result -ne 0 ]; then
            echo "$workingDirectory/$identicalFolder/$namelist/wrf_${dom}_runstats.out and wrf_${dom}_runstats.out differ"
            exit $result
          fi
        elif [ -e ./wrf_${dom}_runstats.out ]; then
          # Domain does not exist in our current run but in the provided folder, that should not happen -FAIL!
          echo "Domain $dom exists in $workingDirectory/$identicalFolder/$namelist but not in this run, cannot compare results"
          exit 123
        else 
          # neither has it, skip
          echo "Domain $dom not generated for namelist $namelist, skipping check"
        fi
      done
    else
      echo "No output to check in $workingDirectory/$identicalFolder for $namelist"
    fi
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
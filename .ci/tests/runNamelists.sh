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
  echo "  -k <diffwrf cmd>          diffwrf command binary path, if not set will use external/io_netcdf/diffwrf"
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

while getopts c:b:f:n:d:p:k:s:i:e:h opt; do
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
    k)
      diffExec="$OPTARG"
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

wrf=$( realpath $( find $coreDir -type f -name wrf -o -name wrf.exe | head -n 1 ) )
rd_12_norm=$( realpath .ci/tests/SCRIPTS/rd_l2_norm.py )

# Check our paths
if [ ! -x "${wrf}" ]; then
  echo "No wrf executable found"
  exit 1
fi

if [ ! -x "${binFirst}" ]; then
  echo "No domain preparation executable found"
  exit 1
fi

if [ ! -d "${data}" ]; then
  echo "No valid data path provided"
  exit 1
fi

if [ ! -d "${namelistFolder}" ]; then
  echo "No valid namelist folder provided"
  exit 1
fi

if [ -z "${diffExec}" ]; then
  diffExec=$( realpath $workingDirectory/external/io_netcdf/diffwrf )
else
  eval "diffExec=\$( realpath \"$diffExec\" )"
fi




################################################################################
#
# Things done only once
# Go to core dir
cd $coreDir || exit $?

# Clean up previous runs
rm wrfinput_d* wrfbdy_d* wrfout_d* wrfchemi_d* wrf_chem_input_d* rsl* real.print.out* wrf.print.out* wrf_d0*_runstats.out qr_acr_qg_V4.dat fort.98 fort.88 -rf

# Link in data in here
ln -sf $data/* .
#
################################################################################


################################################################################
#
# Loop testing namelists
tmpIFS=$IFS
IFS=","

# Since we might fail or succeed on certain namelists, make a running set of failures
errorMsg=""

for namelist in $namelists; do
  banner 42 "START $namelist"

  parallelExecToUse="$parallelExec"
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # SPECIFIC TO NESTED DOMAINS - WE MUST RUN AN ODD NUMBER OF MPI TASKS
  # THIS IS NOTED BY THE NAMELIST HAVING 'NE' OR 'VN' AT THE END
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if [ -n "$parallelExec" ] && { [ $( echo $namelist | grep -Ec "NE$" ) -eq 1 ] || [ $( echo $namelist | grep -Ec "VN$" ) -eq 1 ]; } then
    # Check if parallel exec np is even and if so, reduce by 1
    parallelExecSplit=$( echo "$parallelExec" | sed -e "s/\(.*\?-np[ ]*\)\([0-9]\+\)\(.*\?\)/\1;\2;\3/g" )
    partFront=$( echo "$parallelExecSplit" | awk -F';' '{print $1}' )
    partEnd=$( echo "$parallelExecSplit" | awk -F';' '{print $3}' )
    numProcs=$( echo "$parallelExecSplit" | awk -F';' '{print $2}' )
    if [ $(( $numProcs % 2 )) -eq 0 ]; then
      echo "MPI runs with nested namelist domains require odd-number tasks, reducing by one"
      numProcs=$(( $numProcs - 1 ))
      parallelExecToUse="$partFront$numProcs$partEnd"
      parallelExecToUseBinFirst="$partFront$numProcs$partEnd"
      echo "New command will be '$parallelExecToUse'"
    fi

    # Check if we are runnng ideal
    if [ $( contains $binFirst "ideal" ) -eq 0 ]; then
      echo "Ideal test case initial conditions must be generated with one MPI rank at most"
      parallelExecToUseBinFirst="$partFront"1"$partEnd"
    fi
  fi


  # Clean up output of any of these runs
  rm wrfinput_d* wrfbdy_d* wrfout_d* rsl* real.print.out* wrf.print.out* wrf_d0*_runstats.out qr_acr_qg_V4.dat fort.98 fort.88 -rf

  # Copy namelist
  echo "Setting $namelistFolder/$namelist as namelist.input"
  cp $namelistFolder/$namelist namelist.input || exit $?

  # Run setup
  echo "Running $parallelExecToUseBinFirst $binFirst"

  eval "$parallelExecToUseBinFirst $binFirst | tee setup.print.out"
  result=$?
  if [ -n "$parallelExecToUseBinFirst" ]; then
    # Output the rsl. output
    cat $( ls $coreDir/rsl.out.* | sort | head -n 1 )
  fi

  if [ $result -ne 0 ]; then
    currentErrorMsg="[$namelist] $parallelExecToUseBinFirst $binFirst failed"
    echo "$currentErrorMsg"
    errorMsg="$errorMsg\n$currentErrorMsg"
    continue
  fi

  # run wrf
  echo "Running $parallelExecToUse $wrf"

  eval "$parallelExecToUse $wrf | tee wrf.print.out"
  result=$?
  if [ -n "$parallelExecToUse" ]; then
    # Output the rsl. output
    cat $( ls $coreDir/rsl.out.* | sort | head -n 1 )
  fi

  if [ $result -ne 0 ]; then
    currentErrorMsg="[$namelist] $parallelExecToUse $wrf failed"
    echo "$currentErrorMsg"
    errorMsg="$errorMsg\n$currentErrorMsg"
    continue
  fi

  # Check output per domain
  maxDom=$( grep max_dom namelist.input |  awk '{print $3}' | tr -d ',' )
  currentDom=0
  while [ $currentDom -lt $maxDom ]; do
    currentDom=$(( $currentDom + 1 ))

    domFiles=$( ls -1 | grep wrfout_d0${currentDom} | wc -l | awk '{print $1}' )

    if [ $domFiles -eq 0 ]; then
      currentErrorMsg="[$namelist] No output files generated for domain $currentDom"
      echo "$currentErrorMsg"
      errorMsg="$errorMsg\n$currentErrorMsg"
      continue
    fi

    ncdump wrfout_d0${currentDom}_* | grep -i nan | grep -vi dominant
    okNan=$?
    timeSteps=$( ncdump -h wrfout_d0${currentDom}_* | grep "Time = UNLIMITED" | cut -d"(" -f2 | awk '{print $1}' )

    if [ $okNan -eq 1 ] && [ $timeSteps -eq 2 ]; then
      # Super ok, store output in file for comparison later
      python3 $rd_12_norm wrfout_d0${currentDom}_* > wrf_d0${currentDom}_runstats.out
    else
      # Super NOT ok
      currentErrorMsg="[$namelist] Checks on output failed. okNan=$okNan, timeSteps=$timeSteps when expected okNan=1 && timeSteps=2"
      echo "$currentErrorMsg"
      errorMsg="$errorMsg\n$currentErrorMsg"
      continue
    fi
  done

  # try comp
  if [ ! -z $identicalFolder ]; then
    if [ -d $workingDirectory/$identicalFolder/$namelist/ ]; then
      echo "Comparing current $namelist output to output stored in $workingDirectory/$identicalFolder/$namelist/"

      for io_type in input out; do
        for dom in d01 d02 d03; do
          if [ -e $workingDirectory/$identicalFolder/$namelist/wrf${io_type}_${dom}* ]; then
            if [ ! -e ./wrf${io_type}_${dom}* ]; then
              # Domain does not exist in our current run but in the provided folder, that should not happen -FAIL!
              currentErrorMsg="[$namelist] Domain $( ls wrf${io_type}_${dom}* ) file $dom exists in $workingDirectory/$identicalFolder/$namelist but not in this run, cannot compare results"
              echo "$currentErrorMsg"
              errorMsg="$errorMsg\n$currentErrorMsg"
              continue
            fi

            # We have a domain to check - should exist in both (we are treating the identical folder as truth)
            if [ "${io_type}" = "out" ]; then
              diff $workingDirectory/$identicalFolder/$namelist/wrf_${dom}_runstats.out $(pwd)/wrf_${dom}_runstats.out
              statDiff=$?
            fi

            $diffExec $workingDirectory/$identicalFolder/$namelist/wrf${io_type}_${dom}* wrf${io_type}_${dom}*
            result=$?

            if [ $result -ne 0 ]; then
              currentErrorMsg="[$namelist] $diffExec failed"
              echo "$currentErrorMsg"
              errorMsg="$errorMsg\n$currentErrorMsg"
              continue
            fi

            if [ -e fort.98 ] || [ -e fort.88 ]; then
              currentErrorMsg="[$namelist] $( ls $workingDirectory/$identicalFolder/$namelist/wrf${io_type}_${dom}* ) and current wrf${io_type}_${dom}* differ"
              if [ -n "$statDiff" ] && [ $statDiff -eq 0 ]; then
                currentErrorMsg="$currentErrorMsg - but are statistically equivalent based on checksum"
              fi
              echo "$currentErrorMsg"
              cat fort.98 fort.88 2>/dev/null
              errorMsg="$errorMsg\n$currentErrorMsg"
              continue
            fi
          elif [ -e ./wrf${io_type}_${dom}* ]; then
            echo "Domain file $( ls wrf${io_type}_${dom}* ) exists in $( pwd ) but not in $workingDirectory/$identicalFolder/$namelist/, is this an error?"
          else 
            # neither has it, skip
            echo "Domain file wrf${io_type}_${dom}* not generated for namelist $namelist, skipping check"
          fi
        done
      done
    else
      echo "No output to check in $workingDirectory/$identicalFolder for $namelist"
    fi
  fi

  # Now if we have a move folder, do that
  if [ ! -z $moveFolder ]; then
    mkdir -p $workingDirectory/$moveFolder/$namelist/

    # we are in core dir, find our output
    echo "Moving output files to $workingDirectory/$moveFolder/$namelist/"
    find . -type f -name "wrfinput_d*"       \
                -o -name "wrfbdy_d*"         \
                -o -name "wrfout_d*"         \
                -o -name "wrfchemi_d*"       \
                -o -name "wrf_chem_input_d*" \
                -o -name "rsl*"              \
                -o -name "setup.print.out*"  \
                -o -name "wrf.print.out*"    \
                -o -name "wrf_d0*_runstats.out"
    # Now move
    find . -type f \(  -name "wrfinput_d*"       \
                    -o -name "wrfbdy_d*"         \
                    -o -name "wrfout_d*"         \
                    -o -name "wrfchemi_d*"       \
                    -o -name "wrf_chem_input_d*" \
                    -o -name "rsl*"              \
                    -o -name "setup.print.out*"  \
                    -o -name "wrf.print.out*"    \
                    -o -name "wrf_d0*_runstats.out" \) \
                -exec mv {} $workingDirectory/$moveFolder/$namelist/ \;
  fi

  banner 42 "STOP $namelist"
done
IFS=$tmpIFS

# If we passed, clean up after ourselves
if [ -z "$errorMsg" ]; then
  # Unlink everything we linked in
  ls $data/ | xargs -I{} rm {}

  # Clean up once more since we passed
  rm -rf wrfinput_d* wrfbdy_d* wrfout_d* wrfchemi_d* wrf_chem_input_d* rsl* setup.print.out* wrf.print.out* qr_acr_qg_V4.dat fort.98 fort.88

  # We passed!
  echo "TEST $(basename $0) PASS"
else
  printf "%b" "$errorMsg"
  exit 1
fi

#!/usr/bin/sh
help()
{
  echo "./run_wrf_restart.sh [options]"
  echo "  -r <folder>               Folder to run wrf in, assuming everything is setup there already and WRF has already been run"
  echo "  -d <exec>                 Diff executable to use for comparing runs, default is ./external/io_netcdf/diffwrf"
  echo "  -w <exec>                 WRF executable for WRF, default is wrf.exe"
  echo "  -o <n>                    OMPTHREADS setting for sm/openmp usage"
  echo "  -p <mpirun cmd>           Parallel launch command (MPI) for wrf, e.g. mpirun, mpiexec_mpt, mpiexec -np 8 --oversubscribe"
  echo "  -t <num comparisons>      Number of history files to compare, starting from the end of the restart"
  echo "if provided, these override the namelist inside the run folder"
  echo "  -n <file>                 Namelist to run for wrf inside run folder, otherwise namelist.input"
  echo ""
  echo "  -h                        Print this message"
  echo ""
}

banner()
{
  lengthBanner=$1
  shift
  # https://www.shellscript.sh/examples/banner/
  printf "#%${lengthBanner}s#\n" | tr " " "="
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" "`date`"
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" " "
  printf "# %-$(( ${lengthBanner} - 2 ))s #\n" "$*"
  printf "#%${lengthBanner}s#\n" | tr " " "="
}


finish()
{
  banner 42 "STOP $namelist"

  # If somehow we got here, make sure we report errors
  if [ ! -z "$err" ]; then
    printf "%b" "$err"
    exit 1
  fi
}

echo "Input arguments:"
echo "$*"

wrf_nml="namelist.input"
wrf="wrf.exe"
diffwrf="./external/io_netcdf/diffwrf"
hist_comparisons=1
while getopts r:d:w:o:q:p:t:n:h opt; do
  case $opt in
    r)
      run_dir="$OPTARG"
    ;;
    d)
      diffwrf="$OPTARG"
    ;;
    w)
      wrf="$OPTARG"
    ;;
    o)
      ompthreads="$OPTARG"
    ;;
    p)
      mpi_wrf="$OPTARG"
    ;;
    t)
      hist_comparisons="$OPTARG"
    ;;
    n)
      wrf_nml="$OPTARG"
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

# May not be in the run dir so search first from working dir
diffwrf=$( realpath $diffwrf )

# Go to run location now - We only operate here from now on
cd $run_dir || exit $?

wrf=$( realpath $( find -L $run_dir -type f -name $wrf | head -n 1 ) )

# Check our paths
if [ ! -x "${wrf}" ]; then
  echo "No wrf executable found"
  exit 1
fi
if [ ! -x "${diffwrf}" ]; then
  echo "No diffwrf executable found"
  exit 1
fi
if [ ! -z "${ompthreads}" ]; then
  export OMPTHREADS=$ompthreads
fi

################################################################################

banner 42 "START $wrf_nml"
# Move previous output of previous run to safe spot
ls wrfout_d0* | xargs -i mv {} {}.orig
cp $( ls rsl.out.* | sort | head -n 1 ) rsl.out.orig

# Remove previous diffing
rm *.diff_log

# Copy namelist
if [ "$wrf_nml" != "namelist.input" ]; then
  echo "Setting $wrf_nml as namelist.input"
  # remove old namelist.input which may be a symlink in which case this would have failed
  rm namelist.input
  cp $wrf_nml namelist.input || exit $?
fi

# run wrf
echo "Running $mpi_wrf $wrf"

eval "$mpi_wrf $wrf" &
wrf_pid=$!
sleep 1

if [ -n "$mpi_wrf" ]; then
  if [ $( ls ./rsl.out.* 2>/dev/null | wc -l ) -ne 0 ]; then
    # Output the rsl. output
    tail -f $( ls ./rsl.out.* | sort | head -n 1 ) --pid $wrf_pid -n 9999
  fi
fi

wait $wrf_pid
result=$?
if [ $result -ne 0 ]; then
  err="[$wrf_nml] $mpi_wrf $wrf failed"
  finish
fi

# Check output per domain
maxDom=$( grep max_dom namelist.input |  awk '{print $3}' | tr -d ',' )
currentDom=0
while [ $currentDom -lt $maxDom ]; do
  currentDom=$(( $currentDom + 1 ))

  domFiles=$( ls -1 | grep wrfout_d0${currentDom} | wc -l | awk '{print $1}' )

  if [ $domFiles -eq 0 ]; then
    err="[$wrf_nml] No output files generated for domain $currentDom"
    finish
  fi
done

echo "Comparing output of restart to previous run"
currentDom=0
while [ $currentDom -lt $maxDom ]; do
  currentDom=$(( $currentDom + 1 ))
  # Skip first file as that doesn't match?? Gotta ask Kelly or Wei if that is expected
  dom_files=$( ls -1 wrfout_d0${currentDom}* | grep -vE ".orig$" | sort | tail -n $hist_comparisons )
  for wrfout in $dom_files; do
    $diffwrf $wrfout.orig $wrfout | tee $wrfout.diff_log
    result=$?
    if [ $result -ne 0 ]; then
      err="[$wrfout] $diffwrf failed"
      finish
    fi
    if [ -e fort.98 ] || [ -e fort.88 ]; then
      cat fort.98 fort.88 2>/dev/null
      err="[$wrfout] $diffwrf failed"
      finish
    fi
    # otherwise
    echo "[ok] $wrfout.orig and $wrfout match"
  done
done

finish

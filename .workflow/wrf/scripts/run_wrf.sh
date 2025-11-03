#!/usr/bin/sh
help()
{
  echo "./run_wrf.sh [options]"
  echo "  -r <folder>               Folder to run wrf in, assuming everything is setup there already"
  echo "  -i <exec>                 Init executable for WRF front-end (ideal/real/etc.)"
  echo "  -w <exec>                 WRF executable for WRF, default is wrf.exe"
  echo "  -o <n>                    OMPTHREADS setting for sm/openmp usage"
  echo "  -q <mpirun cmd>           Parallel launch command (MPI) for init executable, if not set equal to -p <cmd>"
  echo "  -p <mpirun cmd>           Parallel launch command (MPI) for wrf, e.g. mpirun, mpiexec_mpt, mpiexec -np 8 --oversubscribe"
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
while getopts r:i:w:o:q:p:n:h opt; do
  case $opt in
    r)
      run_dir="$OPTARG"
    ;;
    i)
      bin_first="$OPTARG"
    ;;
    w)
      wrf="$OPTARG"
    ;;
    o)
      ompthreads="$OPTARG"
    ;;
    q)
      mpi_bin_first="$OPTARG"
    ;;
    p)
      mpi_wrf="$OPTARG"
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


# Go to run location now - We only operate here from now on
cd $run_dir || exit $?

wrf=$( realpath $( find -L $run_dir -type f -name $wrf | head -n 1 ) )
bin_first=$( realpath $( find -L $run_dir -type f -name $bin_first | head -n 1 ) )

# Check our paths
if [ ! -x "${wrf}" ]; then
  echo "No wrf executable found"
  exit 1
fi

if [ ! -x "${bin_first}" ]; then
  echo "No domain preparation executable found"
  exit 1
fi

if [ -z "${mpi_bin_first}" ]; then
  mpi_bin_first="${mpi_wrf}"
fi

if [ ! -z "${ompthreads}" ]; then
  export OMPTHREADS=$ompthreads
fi

################################################################################

banner 42 "START $wrf_nml"
# Clean up output of any of these runs
rm wrfinput_d* wrfbdy_d* wrfout_d* rsl* real.print.out* wrf.print.out* wrf_d0*_runstats.out qr_acr_qg_V4.dat fort.98 fort.88 -rf

# Copy namelist
if [ "$wrf_nml" != "namelist.input" ]; then
  echo "Setting $wrf_nml as namelist.input"
  # remove old namelist.input which may be a symlink in which case this would have failed
  rm namelist.input
  cp $wrf_nml namelist.input || exit $?
fi

# Run setup
echo "Running $mpi_bin_first $bin_first"

eval "$mpi_bin_first $bin_first" &
init_pid=$!
sleep 1 # sleep to let the file be made, as -F is not a standard option on tail

if [ -n "$mpi_bin_first" ]; then
  if [ $( ls ./rsl.out.* 2>/dev/null | wc -l ) -ne 0 ]; then
    # Output the rsl. output
    tail -f $( ls ./rsl.out.* | sort | head -n 1 ) --pid $init_pid -n 9999
  fi
fi

# Get exit status
wait $init_pid
result=$?
if [ $result -ne 0 ]; then
  err="[$wrf_nml] $mpi_bin_first $bin_first failed"
  finish
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

  # ncdump wrfout_d0${currentDom}_* | grep -i nan | grep -vi dominant
  # okNan=$?
  # # timeSteps=$( ncdump -h wrfout_d0${currentDom}_* | grep "Time = UNLIMITED" | cut -d"(" -f2 | awk '{print $1}' )

  # if [ $okNan -ne 1 ]; then
  #   # Super NOT ok
  #   err="[$wrf_nml] Checks on output failed. okNan=$okNan when expected okNan=1"
  #   finish
  # fi
done

finish

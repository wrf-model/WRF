#!/usr/bin/sh
help()
{
  echo "./run_init.sh [options]"
  echo "  -f <folder>               Folder to run wrf in, assuming everything is setup there already"
  echo "  -r <exec>                 Init executable for WRF front-end (ideal/real/etc.)"
  echo "  -o <n>                    OMPTHREADS setting for sm/openmp usage"
  echo "  -p <mpirun cmd>           Parallel launch command (MPI) for executable, e.g. mpirun, mpiexec_mpt, mpiexec -np 8 --oversubscribe"
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
  banner 42 "STOP $cmd"

  # If somehow we got here, make sure we report errors
  if [ ! -z "$err" ]; then
    printf "%b" "$err"
    exit 1
  fi
}

echo "Input arguments:"
echo "$*"

wrf_nml="namelist.input"
wrf_init="real.exe"
while getopts f:r:i:w:o:q:p:n:h opt; do
  case $opt in
    f)
      run_folder="$OPTARG"
    ;;
    r)
      wrf_init="$OPTARG"
    ;;
    o)
      ompthreads="$OPTARG"
    ;;
    p)
      mpi_cmd="$OPTARG"
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
cd $run_folder || exit $?

wrf_init=$( realpath $( find -L $run_folder -type f -name $wrf_init | head -n 1 ) )

if [ ! -x "${wrf_init}" ]; then
  echo "No domain preparation executable found"
  exit 1
fi

if [ ! -z "${ompthreads}" ]; then
  export OMPTHREADS=$ompthreads
fi

################################################################################
cmd="$mpi_cmd $wrf_init $wrf_nml"
banner 42 "START $cmd"
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
echo "Running $mpi_cmd $wrf_init"

eval "$mpi_cmd $wrf_init" &
init_pid=$!
sleep 5 # sleep to let the file be made, as -F is not a standard option on tail

if [ -n "$mpi_cmd" ]; then
  if [ $( ls ./rsl.out.* 2>/dev/null | wc -l ) -ne 0 ]; then
    # Output the rsl. output
    tail -f $( ls ./rsl.out.* | sort | head -n 1 ) --pid $init_pid -n 9999
  fi
fi

# Get exit status
wait $init_pid
result=$?
if [ $result -ne 0 ]; then
  err="[$wrf_nml] $mpi_cmd $wrf_init failed"
  finish
fi

# Check output per domain
maxDom=$( grep max_dom namelist.input |  awk '{print $3}' | tr -d ',' )
currentDom=0
while [ $currentDom -lt $maxDom ]; do
  currentDom=$(( $currentDom + 1 ))

  domFiles=$( ls -1 | grep wrfinput_d0${currentDom} | wc -l | awk '{print $1}' )

  if [ $domFiles -eq 0 ]; then
    err="[$wrf_nml] No output files generated for domain $currentDom"
    finish
  fi
done

finish

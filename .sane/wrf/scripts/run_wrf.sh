#!/usr/bin/sh
help()
{
  echo "./run_wrf.sh [options]"
  echo "  -f <folder>               Folder to run wrf in, assuming everything is setup there already"
  echo "  -r <exec>                 WRF executable for WRF, default is wrf.exe"
  echo "  -o <n>                    OMP_NUM_THREADS setting for sm/openmp usage"
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
wrf_exec="wrf.exe"
while getopts f:r:i:w:o:q:p:n:h opt; do
  case $opt in
    f)
      run_folder="$OPTARG"
    ;;
    r)
      wrf_exec="$OPTARG"
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

wrf_exec=$( realpath $( find -L $run_folder -type f -name $wrf_exec | head -n 1 ) )

if [ ! -x "${wrf_exec}" ]; then
  echo "No wrf executable found"
  exit 1
fi

if [ ! -z "${ompthreads}" ]; then
  echo "Setting OMP_NUM_THREADS=$ompthreads"
  export OMP_NUM_THREADS=$ompthreads
fi

################################################################################
cmd="$mpi_cmd $wrf_exec $wrf_nml"
banner 42 "START $cmd"

# Copy namelist
if [ "$wrf_nml" != "namelist.input" ]; then
  echo "Setting $wrf_nml as namelist.input"
  # remove old namelist.input which may be a symlink in which case this would have failed
  rm namelist.input
  cp $wrf_nml namelist.input || exit $?
fi

# Run setup
echo "Running $mpi_cmd $wrf_exec"

eval "$mpi_cmd $wrf_exec" &
wrf_pid=$!

if [ -n "$mpi_cmd" ]; then
  until [ $( ls ./rsl.out.* 2>/dev/null | wc -l ) -ne 0 ]; do
    sleep 1
    # check if the process is done or failed
    if ! kill -0 $wrf_pid >/dev/null 2>&1; then
      break
    fi
  done
  # Output the rsl. output
  tail -f $( ls ./rsl.out.* | sort | head -n 1 ) --pid $wrf_pid -n 9999
fi

# Get exit status
wait $wrf_pid
result=$?
if [ $result -ne 0 ]; then
  err="[$wrf_nml] $mpi_cmd $wrf_exec failed"
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

finish

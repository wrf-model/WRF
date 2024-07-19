#!/bin/bash 
#SBATCH -N 1
#SBATCH -q debug
#SBATCH -t 00:30:00
#SBATCH -J elvis_test01
#SBATCH -A m4232
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=elvis@nersc.gov
#SBATCH -L scratch,cfs
#SBATCH -C cpu

ntile=1  #number of OpenMP threads per MPI task; also need to change WRF namelist variable "numtiles"
n=64 # number of MPI ranks
#n=4  # number of MPI ranks

 mod_perftools=""
#mod_perftools=perftools-lite       # Use perftools-lite
#mod_perftools=perftools-lite-gpu   # Use perftools-lite-gpu
#mod_perftools=perftools            # Use perftools

 use_gprof=0                        # Set to 1 to use gprof
 [[ $use_gprof -eq 1 ]] && mod_perftools=""

#peenv=gnu                          # Use PrgEnv-gnu
 peenv=nvidia                       # Use PrgEnv-nvidia

 use_gpu=0                          # Set to 1 to use GPU offload code

#Modules --------------------------------------------------------------------
if [[ $use_gpu -eq 1 ]]; then
  module load gpu
  if [[ $peenv == gnu ]]; then
    export GOMP_DEBUG=1
  elif [[ $peenv == nvidia ]]; then
    export NVCOMPILER_ACC_NOTIFY=3
  elif [[ $peenv == cray ]]; then
    export CRAY_ACC_DEBUG=3
  fi
else
  module load cpu
fi
module load PrgEnv-$peenv

#module for WRF file I/O
#order of loading matters!
module load cray-hdf5  #required to load netcdf library
module load cray-netcdf
module load cray-parallel-netcdf
[[ -n ${mod_perftools} ]] && module load ${mod_perftools}
ml -t

#if to run with a wrf executable from modified source codes:
#1. don't load the wrf module
#2. the modified executable (wrf.exe) has to be placed in the rundir 

#OpenMP settings:
export OMP_NUM_THREADS=$ntile
export OMP_PLACES=threads
export OMP_PROC_BIND=spread
[[ $OMP_NUM_THREADS -gt 1 ]] && export OMP_STACKSIZE=64MB  #increase memory segment to store local variables, needed by each thread

#run simulation
#c = number of cpus per task
if [[ $use_gpu -eq 0 ]]; then
  (( c = (128 / (n / SLURM_JOB_NUM_NODES)) * 2 ))
  srun -n $n -c $c --cpu-bind=cores /global/common/software/m4232/pm/v4.5.2/wrf.exe
else
  (( c = (64 / (n / SLURM_JOB_NUM_NODES)) * 2 ))
  srun -n $n -c $c --cpu-bind=cores --gpus-per-task=1 -gpu-bind=none /global/common/software/m4232/pm/v4.5.2/wrf.exe
fi

#capture error code
srunval=$?

#Get the total "elapsed seconds"
if [[ -f rsl.out.0000 ]]; then
  elapsed_seconds=$(awk '/^Timing for / {s+=$(NF-2)}; END {printf("%20.5f\n", s)}' rsl.out.0000)
  echo "Total elapsed seconds: $elapsed_seconds"
fi

#rename and save the process 0 out and err files
mv rsl.error.0000 rsl.error_0_$SLURM_JOB_ID
mv rsl.out.0000 rsl.out_0_$SLURM_JOB_ID

if [ $srunval -ne 0 ]; then
    echo "run failed"
    exit 10
fi

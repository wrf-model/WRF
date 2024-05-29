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

#Modules --------------------------------------------------------------------
module load cpu
module load PrgEnv-gnu

#module for WRF file I/O
#order of loading matters!
module load cray-hdf5  #required to load netcdf library
module load cray-netcdf
module load cray-parallel-netcdf

#if to run with a wrf executable from modified source codes:
#1. don't load the wrf module
#2. the modified executable (wrf.exe) has to be placed in the rundir 

#OpenMP settings:
export OMP_NUM_THREADS=$ntile
export OMP_PLACES=threads
export OMP_PROC_BIND=spread
export OMP_STACKSIZE=64MB  #increase memory segment to store local variables, needed by each thread

#run simulation
#c = number of cpus per task
 (( c = (128 / ($n / $SLURM_JOB_NUM_NODES)) * 2 ))
 
srun -n $n -c $c --cpu_bind=cores /global/common/software/m4232/pm/v4.5.2/wrf.exe

#capture error code
srunval=$?

#rename and save the process 0 out and err files
mv rsl.error.0000 rsl.error_0_$SLURM_JOB_ID
mv rsl.out.0000 rsl.out_0_$SLURM_JOB_ID

if [ $srunval -ne 0 ]; then
    echo "run failed"
    exit 10
fi


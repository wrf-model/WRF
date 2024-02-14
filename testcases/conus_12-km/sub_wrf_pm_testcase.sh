#!/bin/bash 
#SBATCH -N 3
#SBATCH -q debug
#SBATCH -t 00:30:00
#SBATCH -J elvis_test01
#SBATCH -A m4232
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=elvis@nersc.gov
#SBATCH -L scratch,cfs
#SBATCH -C cpu
#SBATCH --tasks-per-node=64
##SBATCH -S 4

ntile=4  #number of OpenMP threads per MPI task

#Modules --------------------------------------------------------------------
module load contrib
module load wrf/4.5.2

#if to run with a wrf executable from modified source codes:
#1. don't load the wrf module
#2. the modified executable (wrf.exe) has to be placed in the rundir 

#OpenMP settings:
export OMP_NUM_THREADS=$ntile
export OMP_PLACES=threads
export OMP_PROC_BIND=spread

#run simulation
srun -n 192 -c 4 --cpu_bind=cores wrf.exe  #3 nodes

#capture error code
srunval=$?

#rename and save the process 0 out and err files
cp rsl.error.0000 rsl.error_0_$SLURM_JOB_ID
cp rsl.out.0000 rsl.out_0_$SLURM_JOB_ID

if [ $srunval -ne 0 ]; then
    echo "run failed"
    exit 10
fi


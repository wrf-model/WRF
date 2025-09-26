#!/bin/csh
#
#BSUB -P 64000400           # account
#BSUB -n 4                  # number of tasks
#BSUB -J test4_0_ESMFSST            # job name
#BSUB -o test4_0_ESMFSST.%J.out     # stdout redirect
#BSUB -e test4_0_ESMFSST.%J.err     # stderr redirect
#BSUB -q debug              # queue
##BSUB -N                    # send email report
#BSUB -W 0:30               # wall-clock limit (HH:MM)
#

# This is a LSF script to run WRF for a simple MPI-only em_real case 
# on bluevista.  jan00 case modified to read SSTs via ESMF is used.  
# Only wrf.exe is run.  
# To use it, type "bsub < test4_0_ESMFSST.lsf.csh" from the test/em_esmf_exp 
# subdirectory.  

unalias cd cp rm ls pushd popd mv

setenv TARGET_CPU_LIST "-1"

# Run WRF+CPL+SST
mpirun.lsf /usr/local/bin/launch ../main/wrf_SST_ESMF.exe


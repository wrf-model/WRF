#
# LSF batch script for WRF-Channel Model    
#
##BSUB -P 86850103                       # Project NRCM      
#BSUB -P 66770001                       # Project NRCM    
#BSUB -N                                # send email report
#BSUB -u hender                         # email address
##BSUB -a poe                           # select poe - optional    
#BSUB -x                                # exclusive use of node (not_shared)
#BSUB -n 64                             # number of total (MPI) tasks
                                        # 32n=256p ; 16n=128p
#BSUB -R "span[ptile=8]"                # run a max of 8 tasks per node
#BSUB -J wrf_columbia                   # job name
#BSUB -o wrf.out                        # output filename
#BSUB -e wrf.err                        # error filename
#BSUB -W 2:00                           # wallclock time
#BSUB -q premium                        # queue

cd /ptmp/hender/NRCM/ColumbiaTestCase/TEST/WRFV2/test/em_real

mpirun.lsf ./wrf.exe


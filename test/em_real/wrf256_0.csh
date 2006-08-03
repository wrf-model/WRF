#!/bin/csh
#
# PBS script for shorter-than-two-day run.  
# No output quilt servers are used.
#
#PBS -S /bin/csh
#PBS -N wrf256_0
# #PBS -q twok
# #PBS -q twok@c19
# #PBS -q twok@columbia19
#PBS -l ncpus=256
#PBS -l mem=9996MB
#PBS -l walltime=1:00:00
#PBS -W group_list=g22104
#PBS -m ae
#PBS -V

# change working directory
cd $PBS_O_WORKDIR

# Use one group of zero output quilt servers...  
set pid = $$
mv namelist.input namelist.input.${pid}
sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 0/g' \
    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 1/g' \
        namelist.input.${pid} > namelist.input
echo "Built namelist.input with output quilt servers disabled."

mpirun -np 256 ./wrf.exe


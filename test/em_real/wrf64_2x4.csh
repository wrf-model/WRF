#!/bin/csh
#
# PBS script for two-day run.  
# Two groups of four output quilt servers are used.
#
#PBS -S /bin/csh
#PBS -N wrf64_2x4
# #PBS -q twok
# #PBS -q twok@c19
# #PBS -q twok@columbia19
#PBS -l ncpus=72
#PBS -l mem=9996MB
#PBS -l walltime=2:00:00
#PBS -W group_list=g22104
#PBS -m ae
#PBS -V

# change working directory
cd $PBS_O_WORKDIR

# Use two groups of four output quilt servers...  
set pid = $$
mv namelist.input namelist.input.${pid}
sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 4/g' \
    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 2/g' \
        namelist.input.${pid} >! namelist.input
echo "Built namelist.input with 2 groups of 4 output quilt servers enabled."

mpirun -np 72 ./wrf.exe


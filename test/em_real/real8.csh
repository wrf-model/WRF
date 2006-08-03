#!/bin/csh
#
# PBS script to run real.exe ...  
#
#PBS -S /bin/csh
#PBS -N real8
# #PBS -q twok
# #PBS -q twok@c19
# #PBS -q twok@columbia19
#PBS -l ncpus=8
#PBS -l mem=9996MB
#PBS -l walltime=0:10:00
#PBS -W group_list=g22104
#PBS -m ae
#PBS -V

# change working directory
cd $PBS_O_WORKDIR

# ASSUME ALL INPUT FILES REQUIRED BY real.exe ARE ALREADY VISIBLE FROM $PBS_O_WORKDIR

# Use no output quilt servers...  
set pid = $$
mv namelist.input namelist.input.${pid}
sed -e 's/ nio_tasks_per_group *= *[0-9][0-9]*/ nio_tasks_per_group = 0/g' \
    -e 's/ nio_groups *= *[0-9][0-9]*/ nio_groups = 1/g' \
        namelist.input.${pid} >! namelist.input
echo "Built namelist.input with no output quilt servers enabled."

mpirun -np 8 ./real.exe


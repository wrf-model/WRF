#!/bin/ksh
# da_compare_wrfvar.ksh
# Purpose: Compare files between wrfvar runs

if [[ $# != 2 ]]; then
   echo "Arguments dir1 dir2"
   exit 1
fi

DIR1=$1
DIR2=$2

if [[ ! -d $DIR1 ]]; then
   echo "Directory $DIR1 does not exist"
   exit 1
fi

if [[ ! -d $DIR2 ]]; then
   echo "Directory $DIR2 does not exist"
   exit 1
fi

# Text files

export TEXT_FILES="cost_fn grad_fn rsl/rsl.error.0000.html namelist.input"
#namelist.output
#statistics
#working/unpert_obs
#working/pert_obs
#working/check_max_iv
#working/gts_omb_oma
#working/filtered_obs
#working/rand_obs_error

export NETCDF_FILES=working/wrfvar_output

da_compare_files.ksh $DIR1 $DIR2
exit $?

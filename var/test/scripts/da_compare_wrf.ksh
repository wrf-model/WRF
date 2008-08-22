#!/bin/ksh
# da_compare_wrf.ksh
# Purpose: Compare files between wrf runs

# Text files
export TEXT_FILES="rsl/rsl.out.0000.html rsl/rsl.error.0000.html namelist.input namelist.output"

export NETCDF_FILES="working/wrfbdy_d01 working/wrfinput_d01"

da_compare_files.ksh $1 $2
exit $?

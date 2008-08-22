#!/bin/ksh
# da_compare_wps.ksh
# Purpose: Compare files between wps runs

# Text files
export TEXT_FILES="namelist.wps"

da_compare_files.ksh $1 $2
exit $?

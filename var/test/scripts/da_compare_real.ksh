#!/bin/ksh
# da_compare_real.ksh
# Purpose: Compare files between real runs

# Text files
export TEXT_FILES="namelist.input namelist.output"

da_compare_files.ksh $1 $2
exit $?

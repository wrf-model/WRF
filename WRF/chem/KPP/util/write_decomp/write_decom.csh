#!/bin/csh -f


if ( $#argv != 2 ) then
echo ERROR: USAGE: write_decom.csh  kpp_mechanism path_to_chem_directory
exit 
endif

set mech=$argv[1]
set cpath=$argv[2]

# case sensitive!
grep decomp  ${cpath}/module_kpp_${mech}_Integr.F | grep CALL > /dev/null


if ( $status != 0 ) then

make MECH=${mech} CPATH=${cpath}

endif


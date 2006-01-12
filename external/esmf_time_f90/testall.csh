#!/bin/csh
#
# Build and run Test1.exe and compare results with known-good output
#
if ( ! -f ../../configure.wrf ) then
  echo "ERROR:  must run ../../configure before building esmf_time_f90 unit tests"
  exit -1
endif
make superclean
cd ../.. ; make esmf_time_f90_only ; cd external/esmf_time_f90
./Test1.exe >&! Test1.out
which xxdiff >& /dev/null
set ok = $status
if ( $ok == 0 ) then
  xxdiff Test1.out.correct Test1.out
else
  diff Test1.out.correct Test1.out
endif


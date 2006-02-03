#!/bin/csh
#
# Build and run Test1.exe and compare results with known-good output
#
if ( ! -f ../../configure.wrf ) then
  echo "ERROR:  must run ../../configure before building esmf_time_f90 unit tests"
  exit -1
endif
# build
make superclean
cd ../.. ; make esmf_time_f90_only ; cd external/esmf_time_f90
# run
./Test1.exe >&! Test1.out || echo "ERROR failed to execute ./Test1.exe" && exit 1
# evaluate test results
diff Test1.out.correct Test1.out >& /dev/null
set ok = $status
if ( $ok == 0 ) then
  echo
  echo "PASS"
  echo
else
  echo
  echo "FAIL"
  echo
  which xxdiff >& /dev/null
  set ok = $status
  if ( $ok == 0 ) then
    xxdiff Test1.out.correct Test1.out
  else
    diff Test1.out.correct Test1.out
  endif
endif

#!/bin/csh
#
# Build and run Test1.exe and compare results with known-good output
#
set selflong = $0
set self = $selflong:t

if ( ! -f ../../configure.wrf ) then
  echo "ERROR:  must run ../../configure before building esmf_time_f90 unit tests"
  exit -1
endif

# build
set allpass = "true"
make superclean >& /dev/null
cd ../.. ; make esmf_time_f90_only >&! external/esmf_time_f90/make_tests.out ; cd external/esmf_time_f90
# run tests for both ESMF_ and WRFU_ interfaces...  
set testoutok = "Test1.out.correct"
foreach tst ( "ESMF" "WRFU" )
  set testname = "Test1_${tst}"
  ./${testname}.exe >&! ${testname}.out || echo "ERROR ${testname}:  failed to execute ./${testname}.exe, see make_tests.out" && exit 20
  # evaluate test results
  diff ${testoutok} ${testname}.out >& /dev/null
  set ok = $status
  if ( $ok == 0 ) then
    echo "PASS ${testname}"
  else
    set allpass = "false"
    echo
    echo "FAIL ${testname}"
    echo
    which xxdiff >& /dev/null
    set ok = $status
    if ( $ok == 0 ) then
      xxdiff ${testoutok} ${testname}.out
    else
      diff ${testoutok} ${testname}.out
    endif
  endif
end
# clean up if all tests passed
if ( $allpass == "true" ) then
#  make testclean >& /dev/null
  make superclean >& /dev/null
endif


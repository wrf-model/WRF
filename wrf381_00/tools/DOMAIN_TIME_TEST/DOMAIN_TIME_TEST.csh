#!/bin/csh
#
#  DOMAIN_TIME_TEST.csh wrf_ascii_output test_case
#
#  Extract domain self-test results from WRF ASCII output file 
#  "wrf_ascii_output" and compare with known-good results, echoing 
#  "PASS" iff results match.  Note that these tests are normally turned off 
#  but may be enabled by setting namelist variable self_test_domain to .true. 
#  in namelist /time_control/ .  This script looks in the directory in which 
#  it resides to locate files that contain known-good test output.  Second 
#  argument "test_case" is used to determine which known-good file to compare 
#  against via the naming convention:  
#    DOMAIN_TIME_TEST.${test_case}.correct
#
#  If the test passes, the following string is echoed by itself:  
#    "PASS   DOMAIN_TIME_TEST.csh $test_case"
#
#  If the test fails, the following string is echoed along with other 
#  diagnostics:  
#    "FAIL   DOMAIN_TIME_TEST.csh $test_case"
#
#  In the event of an error (file not found, etc.) the following string is 
#  echoed along with other diagnostics:
#    "ERROR  DOMAIN_TIME_TEST.csh $test_case"
#  Note that in this case, $test_case is left off if not specified as an 
#  argument.  
#
#  This script must be run using a fully-qualified path as it deduces the 
#  location of correct test results from this path.  
#
#  EXAMPLE (running from test/em_real):
#   >> ../../tools/DOMAIN_TIME_TEST/DOMAIN_TIME_TEST.csh rsl.out.0000 jan00_12hr
#   PASS   DOMAIN_TIME_TEST.csh jan00_12hr
#
#
#  AUTHOR:
#    Tom Henderson    NCAR/MMM
#
#  DATE:
#    20060316
#

set selflong = $0
set self = $selflong:t
set scriptdir = $selflong:h

if ( $#argv != 2 ) then
  echo "ERROR  ${self}"
  echo "ERROR:  Must specify a WRF ASCII output file as first argument and "
  echo "ERROR:  name of test case as second argument."
  echo "USAGE:  ${self} wrf_ascii_output test_case"
  echo "EXAMPLE:  ${selflong} rsl.out.0000 jan00_12hr"
  exit 10
endif

set ascii_file = $1
set test_case = $2
set pid = $$
set correct_file = "${scriptdir}/DOMAIN_TIME_TEST.${test_case}.correct"
set tmp_file = "DOMAIN_TIME_TEST.${test_case}.${pid}.tmp"

if ( ! -f $ascii_file ) then
  echo "ERROR  ${self} ${test_case}"
  echo "ERROR:  could not find WRF ASCII output file ${ascii_file}"
  exit 20
endif
if ( ! -f $correct_file ) then
  echo "ERROR  ${self} ${test_case}"
  echo "ERROR:  could not find correct test results file ${correct_file}"
  exit 30
endif

\rm -f ${tmp_file}
grep DOMAIN_TIME_TEST $ascii_file >! ${tmp_file} || echo "ERROR  ${self} ${test_case}:  could not grep file ${ascii_file}" && exit 40

if ( ! `diff ${correct_file} ${tmp_file} | wc -c` ) then
  echo "PASS   ${self} ${test_case}"
  \rm -f ${tmp_file}
else
  echo "FAIL   ${self} ${test_case}"
  echo "FAIL:  Differences follow:"
  echo "diff ${correct_file} ${tmp_file}"
  diff ${correct_file} ${tmp_file}
  \rm -f ${tmp_file}
  exit 50
endif

exit 0


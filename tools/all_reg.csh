#!/bin/csh

#	This script runs a large number of WRF regression tests.  On the
#	IBM machines (which at NCAR allow queueing), the jobs are sent to
#	the queue system via load leveler commands.  On all other machines,
#	the jobs are sent in sequentially as soon as the process returns 
#	from completing the previous task.

set OLD_TEXT = ( "NESTED = FALSE"		\
                 "NESTED = FALSE"		\
                 "REG_TYPE = BIT4BIT"		\
                 "CHEM = FALSE"			\
                 "RSL_LITE = FALSE"		\
                 "ESMF_LIB = FALSE"		\
                 "QUILT = FALSE"		\
                 "IO_FORM = 2"			\
               )

set NEW_TEXT = ( "NESTED = FALSE"		\
                 "NESTED = TRUE"		\
                 "REG_TYPE = OPTIMIZED"		\
                 "CHEM = TRUE"			\
                 "RSL_LITE = TRUE"		\
                 "ESMF_LIB = TRUE"		\
                 "QUILT = TRUE"			\
                 "IO_FORM = 5"			\
               )

set NAME     = ( "Standard"			\
                 "Moving_Nest"			\
                 "Full_Optimization"		\
                 "Chemistry"			\
                 "RSL_LITE"			\
                 "ESMF_Library"			\
                 "Quilting"			\
                 "GriB1_Output"			\
               )

set TOAST    = ( "NONE"				\
                 "AIX"				\
                 "NONE"				\
                 "NONE"				\
                 "NONE"				\
                 "ONLY_AIX"			\
                 "Linux"			\
                 "NONE"				\
               )

set num_tests = ${#OLD_TEXT}
set count = 1

while ( $count <= $num_tests )

	if      ( `uname` == $TOAST[$count] ) then
		echo skipping test $NAME[$count] for `uname` specifically
	else if ( ( `echo $TOAST[$count] | cut -c 1-5` == ONLY_ ) && ( ONLY_`uname` != $TOAST[$count] ) ) then
		echo skipping test $NAME[$count] for `uname`, works on $TOAST[$count]
	else if ( ( $TOAST[$count] == NONE ) || \
		  ( $TOAST[$count] != `uname` ) || \
		  ( ONLY_`uname` ==  $TOAST[$count] ) ) then
		echo doing test $NAME[$count] for `uname`
		if ( -e ed.in ) rm ed.in

		cat >! ed_in << EOF
			/$OLD_TEXT[$count]/s/$OLD_TEXT[$count]/$NEW_TEXT[$count]/
			w reg.foo.$count.$NAME[$count]
			q
EOF
		ed regtest.csh < ed_in >& /dev/null

		if ( `uname` == AIX ) then
			llsubmit reg.foo.$count.$NAME[$count] >&! llsub.out
			sleep 10 ; llq -u $USER >&! llq.report
			set ok = 0
			set in_already = 0
			while ( $ok == 0 )
				grep `cat llsub.out | grep '"bs1' | cut -d\" -f2` llq.report >& /dev/null
				set ok = $status
				if ( ( $ok == 0 ) && ( $in_already == 0 ) ) then
					set in_already = 1
					set joe_id = `cat llsub.out | grep '"bs1' | cut -d\" -f2 | cut -d. -f2`
				endif
			end
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.out wrftest.output.$count.$NAME[$count]
			rm llsub.out llq.report
		else
			chmod +x reg.foo.$count.$NAME[$count]
			reg.foo.$count.$NAME[$count] -f wrf.tar >&! output.$count.$NAME[$count]
			mv wrftest.output wrftest.output.$count.$NAME[$count]
			if ( -d regression_test ) rm -rf regression_test
		endif
	endif

	@ count ++
end

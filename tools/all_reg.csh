#!/bin/csh

#	This script runs a large number of WRF regression tests.  On the
#	IBM machines (which at NCAR allow queueing), the jobs are sent to
#	the queue system via load leveler commands.  On all other machines,
#	the jobs are sent in sequentially as soon as the process returns 
#	from completing the previous task.

#	Which test to run, either put "ALL" or the test index.

set WHICH_TEST_TO_RUN = 1
set WHICH_TEST_TO_RUN = ALL

#	You can also run this script to generate and compare the various
#	baseline tests.  

set BASELINE = GENERATE
set BASELINE = COMPARE
set BASELINE = NOPE

#	HOW TO RUN
#	----------

#	DEC
#	joshua1 or joshua3
#	mkdir /mmmtmp/$USER/`hostname`
#	put all_reg.csh, regtest.csh, and wrf.tar in dir
#	execute all_reg.csh
#	takes about 24-30 h, so expect troubles on runs
#	around 1 AM due to scrubbers

#	Linux
#	big (bay-mmm is too flaky with Flex lm pgi license)
#	cd /big6/gill/DO_NOT_REMOVE_DIR
#	put all_reg.csh, regtest.csh, and wrf.tar in dir
#	execute all_reg.csh
#	takes 8-10 h, stay away from Jim's real-time
#	runs from 05-08Z and 17-20Z (Boulder local is 10-1 winter, 
#	11-2 summer)

#	AIX
#	bluesky
#	put all_reg.csh, regtest.csh, and wrf.tar in ~
#	execute all_reg.csh
#	takes about 8-10 h

#	Unless you are editing the script, no changes are required below

#=======================================================================
#=======================================================================

#	The regtest.csh file is treated as a template.  The following
#	strings (first occurrence) is sought (OLD_TEXT) and replaced
#	with the modified string (NEW_TEXT).  This edited regression 
#	script is then processed.

set OLD_TEXT = ( "NESTED = FALSE"		\
                 "NESTED = FALSE"		\
                 "REG_TYPE = BIT4BIT"		\
                 "CHEM = FALSE"			\
                 "RSL_LITE = FALSE"		\
                 "ESMF_LIB = FALSE"		\
                 "QUILT = FALSE"		\
                 "IO_FORM = 2"			\
                 "IO_FORM = 2"			\
                 "REAL8 = FALSE"		\
               )

set NEW_TEXT = ( "NESTED = FALSE"		\
                 "NESTED = TRUE"		\
                 "REG_TYPE = OPTIMIZED"		\
                 "CHEM = TRUE"			\
                 "RSL_LITE = TRUE"		\
                 "ESMF_LIB = TRUE"		\
                 "QUILT = TRUE"			\
                 "IO_FORM = 1"			\
                 "IO_FORM = 5"			\
                 "REAL8 = TRUE"			\
               )

#	For clarity, what these tests do, must be a single string.

set NAME     = ( "Standard"			\
                 "Moving_Nest"			\
                 "Full_Optimization"		\
                 "Chemistry"			\
                 "RSL_LITE"			\
                 "ESMF_Library"			\
                 "Quilting"			\
                 "Binary_IO"			\
                 "GriB1_Output"			\
                 "REAL8_Floats"			\
               )

#	Any exceptions to where they can run?  NONE means no 
#	exceptions, should run on all machines.  AIX/Linux/OSF1
#	means it will NOT run on that single machine.  The 
#	option ONLY_AIX/ONLY_Linux/ONLY_OSF1 means that the option
#	ONLY works on that specific architecture.

set TOAST    = ( "NONE"				\
                 "NONE"				\
                 "NONE"				\
                 "NONE"				\
                 "NONE"				\
                 "ONLY_AIX"			\
                 "NONE"				\
                 "NONE"				\
                 "NONE"				\
                 "NONE"				\
               )

#	We need the regtest.csh file, badly.

if ( ! -e regtest.csh ) then
	echo we need regtest.csh in this dir
	echo "supply-ez vous, s'il vous plait"
	exit ( 1 )
endif

#	They said which test we are doing.  We turn that info into
#	some while loop constraints: how many in the list, and where 
#	in the list to start.  If these two values are the same, we
#	do a single test (the else branch).

if ( $WHICH_TEST_TO_RUN == ALL ) then
	set num_tests = ${#OLD_TEXT}
	set count = 1
else
	set num_tests = $WHICH_TEST_TO_RUN
	set count = $WHICH_TEST_TO_RUN
endif

#	Loop over all selected tests.

while ( $count <= $num_tests )

	#	Specifically skip this test on this architecture.

	if      ( `uname` == $TOAST[$count] ) then
		echo skipping test $NAME[$count] for `uname` specifically

	#	Skip this test on this architecture because it ONLY runs
	#	on a different architecture.

	else if ( ( `echo $TOAST[$count] | cut -c 1-5` == ONLY_ ) && ( ONLY_`uname` != $TOAST[$count] ) ) then
		echo skipping test $NAME[$count] for `uname`, works on $TOAST[$count]

	#	OK, we are allowed to run this test on this architecture.

	else if ( ( $TOAST[$count] == NONE ) || \
		  ( $TOAST[$count] != `uname` ) || \
		  ( ONLY_`uname` ==  $TOAST[$count] ) ) then
		echo doing test $NAME[$count] for `uname`

		#	If this is the generate or compare baseline test, where do we
		#	save the data to/read the data from.

		if ( ( $BASELINE == GENERATE ) || ( $BASELINE == COMPARE ) ) then
			if ( `uname` == AIX ) then
				set SAVE_DIR = /ptmp/gill/BASELINE/`uname`/$NAME[$count]
			else if ( ( `uname` == OSF1 ) && ( `hostname | cut -c 1-6` == joshua ) ) then
				set SAVE_DIR = /data3/mp/gill/BASELINE/`uname`/$NAME[$count]
			else if ( ( `uname` == Linux ) && ( `hostname` == master ) ) then
				set SAVE_DIR = /big6/gill/DO_NOT_REMOVE_DIR/BASELINE/`uname`/$NAME[$count]
			else
				echo No idea where to put the data, stopping
				exit ( 2 )
			endif

			#	Either zap existing stuff (GENERATE), or make sure it is there (COMPARE)

			if      ( $BASELINE == GENERATE ) then
				/bin/rm -rf $SAVE_DIR
			else if ( $BASELINE == COMPARE  ) then
				if ( ! -d $SAVE_DIR ) then
					echo $SAVE_DIR does not exist for BASELINE comparison, stopping
					exit ( 3 )
				endif
			endif
		endif

		#	Build the short edit input script for ed and edit the regtest.csh file.

		if      ( $BASELINE == NOPE     ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
				/$OLD_TEXT[$count]/s/$OLD_TEXT[$count]/$NEW_TEXT[$count]/
				w reg.foo.$count.$NAME[$count]
				q
EOF
		else if ( $BASELINE == GENERATE ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
				/$OLD_TEXT[$count]/s/$OLD_TEXT[$count]/$NEW_TEXT[$count]/
				/^set GENERATE_BASELINE = FALSE/s?GENERATE_BASELINE = FALSE?GENERATE_BASELINE = $SAVE_DIR?
				w reg.foo.$count.$NAME[$count]
				q
EOF
		else if ( $BASELINE == COMPARE  ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
				/$OLD_TEXT[$count]/s/$OLD_TEXT[$count]/$NEW_TEXT[$count]/
				/^set COMPARE_BASELINE = FALSE/s?COMPARE_BASELINE = FALSE?COMPARE_BASELINE = $SAVE_DIR?
				w reg.foo.$count.$NAME[$count]
				q
EOF
		endif
		ed regtest.csh < ed_in >& /dev/null
		chmod +x reg.foo.$count.$NAME[$count]

		#	On AIX, we submit jobs to the load leveler queue.  After submission,
		#	we wait around until it completes, and then we send in the next one.

		if ( `uname` == AIX ) then
			llsubmit reg.foo.$count.$NAME[$count] >&! llsub.out
			set ok = 0
			set in_already = 0
			while ( $ok == 0 )
				sleep 10 ; llq -u $USER >&! llq.report
				grep `cat llsub.out | grep '"bs1' | cut -d\" -f2` llq.report >& /dev/null
				set ok = $status
				if ( ( $ok == 0 ) && ( $in_already == 0 ) ) then
					set in_already = 1
					set joe_id = `cat llsub.out | grep '"bs1' | cut -d\" -f2 | cut -d. -f2`
				endif
			end
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.output wrftest.output.$count.$NAME[$count]
			rm llsub.out llq.report

		#	On the "other" non-queued machines, we just execute the script and wait until
		#	we get the process returning control, then we move on.

		else
			reg.foo.$count.$NAME[$count] -f wrf.tar >&! output.$count.$NAME[$count]
			mv wrftest.output wrftest.output.$count.$NAME[$count]
			if ( $WHICH_TEST_TO_RUN == ALL ) then
				if ( -d regression_test ) rm -rf regression_test
			else
				if ( -d regression_test ) then
					mv regression_test regression_test.$count.$NAME[$count]
				endif
			endif
		endif
	endif

	@ count ++
end

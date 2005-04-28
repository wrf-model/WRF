#!/bin/csh

#	This script runs a large number of WRF regression tests.  On the
#	IBM machines (which at NCAR allow queueing), the jobs are sent to
#	the queue system via load leveler commands.  On all other machines,
#	the jobs are sent in sequentially as soon as the process returns 
#	from completing the previous task.

#	HOW TO RUN
#	----------

#	DEC
#	joshua1 or joshua3
#	mkdir /data3/mp/$USER/`hostname`
#	put all_reg.csh, regtest.csh, and wrf.tar in dir
#	execute all_reg.csh
#	takes about 24-30 h

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

#	What these tests do, must be a single string.

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

#	Allocate space for strings to be added.  Must have at least as many
#	values as there are tests in $NAME.

set tests	= ( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 )

#	If there are any command line args, they are processed, else
#	we run all of the regression tests without a generate or
#	compare flag being set.

if ( $#argv == 0 ) then

	set BASELINE = RUN_ONLY

	set count_test = 0
	foreach n ( $NAME )
		@ count_test ++
		set tests[$count_test] = $n
	end
	set NUM_TESTS = $#NAME

#	We have some command line args.  They are either a request to run 
#	the test with a generate/compare flag, or a list of tests to perform.

else

	set INIT_OPTS = ( $* )

	#	First, find the baseline type.  This is going to be one of three
	#	possibilities: GENERATE, COMPARE, or a RUN_ONLY option.  The
	#	default is RUN_ONLY.  Only the first baseline option found is
	#	used.

	set count = 0 
	while ( $count < $#INIT_OPTS )
		@ count ++
		set arg = $INIT_OPTS[$count]
		if ( ( $arg == GENERATE ) || \
		     ( $arg == COMPARE  ) || \
		     ( $arg == RUN_ONLY ) ) then
			set BASELINE = $arg
			goto FINISHED_BASELINE_TYPE
		endif
	end
	set BASELINE = RUN_ONLY
FINISHED_BASELINE_TYPE:

	#	Find which tests are to be conducted.  Loop over all of the
	#	input, and compare each of the input fields with the list
	#	of available test names.  When a match occurs, increment the
	#	test found counter, and save the test name.

	set count = 0 
	set count_test = 0
	while ( $count < $#INIT_OPTS ) 
		@ count ++
		foreach n ( $NAME )
			if ( $INIT_OPTS[$count] == $n ) then
				@ count_test ++
				set tests[$count_test] = $n
			endif
		end
	end
	
	#	If there a no tests requested, then they (by default) get them all

	if ( $count_test == 0 ) then
		foreach n ( $NAME )
			@ count_test ++
			set tests[$count_test] = $n
		end
	endif
	set NUM_TESTS = $count_test
endif

#	A friendly check for the baseline directory existence

if ( ( $BASELINE == GENERATE ) || ( $BASELINE == COMPARE ) ) then
	if ( `uname` == AIX ) then
		set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`
	else if ( ( `uname` == OSF1 ) && ( `hostname | cut -c 1-6` == joshua ) ) then
		set SAVE_DIR = /data3/mp/${USER}/BASELINE/`uname`
	else if ( ( `uname` == Linux ) && ( `hostname` == master ) ) then
		set SAVE_DIR = /big6/gill/DO_NOT_REMOVE_DIR/BASELINE/`uname`
	else
		echo Hmm, no idea where to put/get this baseline data, stopping
		exit ( 10 )
	endif

	if ( ( (   -d $SAVE_DIR ) && ( $BASELINE == GENERATE ) ) || \
	     ( ( ! -d $SAVE_DIR ) && ( $BASELINE == COMPARE  ) ) ) then
		echo WARNING Might be troubles with $SAVE_DIR logic
	endif
endif

#	We need the regtest.csh file, badly.

if ( ! -e regtest.csh ) then
	echo we need regtest.csh in this dir
	echo "supply-ez vous, s'il vous plait"
	exit ( 1 )
endif

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

#	Loop over all selected tests.

set count_test = 0
while ( $count_test < $NUM_TESTS )

	@ count_test ++

	set count = 0 
	while ( $count < $#NAME )
		@ count ++
		if ( $tests[$count_test] == $NAME[$count] ) then
			goto FOUND_SELECTED_TEST
		endif
	end
	echo "Hmmm, no valid test found"
	exit ( 11 )

FOUND_SELECTED_TEST:

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
				set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`/$NAME[$count]
			else if ( ( `uname` == OSF1 ) && ( `hostname | cut -c 1-6` == joshua ) ) then
				set SAVE_DIR = /data3/mp/${USER}/BASELINE/`uname`/$NAME[$count]
			else if ( ( `uname` == Linux ) && ( `hostname` == master ) ) then
				set SAVE_DIR = /big6/gill/DO_NOT_REMOVE_DIR/BASELINE/`uname`/$NAME[$count]
			else
				echo No idea where to put the data, stopping
				exit ( 2 )
			endif

			#	Either zap existing stuff (GENERATE), or make sure it is there (COMPARE)

			if        ( $BASELINE == GENERATE ) then
				/bin/rm -rf $SAVE_DIR
			else if ( ( $BASELINE == COMPARE  )  && ( $NAME[$count] != Full_Optimization ) ) then
				if ( ! -d $SAVE_DIR ) then
					echo $SAVE_DIR does not exist for BASELINE comparison, stopping
#				exit ( 3 )
				endif
			else if ( ( $BASELINE == COMPARE  )  && ( $NAME[$count] == Full_Optimization ) ) then
				echo No comparison done with baseline since this is an optimized run
			endif
		endif

		#	Build the short edit input script for ed and edit the regtest.csh file.

		if      ( $BASELINE == RUN_ONLY ) then
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
cat >! foo1 << EOF
			reg.foo.$count.$NAME[$count] -f wrf.tar >&! output.$count.$NAME[$count]
			mv wrftest.output wrftest.output.$count.$NAME[$count]
			if ( $NUM_TESTS != $#NAME ) then
				if ( -d regression_test ) rm -rf regression_test
			else
				if ( -d regression_test ) then
					mv regression_test regression_test.$count.$NAME[$count]
				endif
			endif
EOF
		endif
	endif
end

rm ed_in >& /dev/null
rm io_format >& /dev/null

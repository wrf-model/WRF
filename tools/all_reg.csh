#!/bin/csh
unalias rm
unalias cp
unalias mv

#       This script runs a large number of WRF regression tests.  On the
#       IBM machines (which at NCAR allow queueing), the jobs are sent to
#       the queue system via load leveler commands.  On all other machines,
#       the jobs are sent in sequentially as soon as the process returns 
#       from completing the previous task.

#       HOW TO RUN
#       ----------

#       DEC
#       joshua1 or joshua3
#       mkdir /data3/mp/$USER/`hostname`
#       put all_reg.csh, regtest.csh, and wrf.tar in dir
#       execute all_reg.csh
#       takes about 24-30 h

#       Linux
#       big (bay-mmm is too flaky with Flex lm pgi license)
#       cd /big6/gill/DO_NOT_REMOVE_DIR
#       put all_reg.csh, regtest.csh, and wrf.tar in dir
#       execute all_reg.csh
#       takes 8-10 h, stay away from Jim's real-time
#       runs from 05-08Z and 17-20Z (Boulder local is 10-1 winter, 
#       11-2 summer)

#       AIX
#       bluesky
#       put all_reg.csh, regtest.csh, and wrf.tar in ~
#       execute all_reg.csh
#       takes about 8-10 h

#       Unless you are editing the script, no changes are required below


#=======================================================================
#=======================================================================

#       What these tests do, must be a single string.

set NAME     = ( "Standard"             "NESTED=FALSE"        "NESTED=FALSE"        "NONE"  \
                 "Moving_Nest"          "NESTED=FALSE"        "NESTED=TRUE"         "NONE"  \
                 "Full_Optimization"    "REG_TYPE=BIT4BIT"    "REG_TYPE=OPTIMIZED"  "NONE"  \
                 "Chemistry"            "CHEM=FALSE"          "CHEM=TRUE"           "NONE"  \
                 "RSL_LITE"             "RSL_LITE=FALSE"      "RSL_LITE=TRUE"       "NONE"  \
                 "ESMF_Library"         "ESMF_LIB=FALSE"      "ESMF_LIB=TRUE"       "ONLY_AIX"  \
                 "Quilting"             "QUILT=FALSE"         "QUILT=TRUE"          "NONE"  \
                 "Binary_IO"            "IO_FORM=2"           "IO_FORM=1"           "NONE"  \
                 "GriB1_Output"         "IO_FORM=2"           "IO_FORM=5"           "NONE"  \
                 "REAL8_Floats"         "REAL8=FALSE"         "REAL8=TRUE"          "NONE"  \
               )

#	Allocate space for strings to be added.  Must have at least as many
#	values as there are tests in $NAME.

set tests	= ( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 )

#	If there are any command line args, they are processed, else
#	we run all of the regression tests without a generate or
#	compare flag being set.

if ( $#argv == 0 ) then

	set BASELINE = RUN_ONLY

        set OLD_TEXT
        set NEW_TEXT
        set TOAST
        set tests
	set count_test = 1
        while ( $count_test < $#NAME )
		set tests =  ( $tests "$NAME[$count_test]" )
		@ count_test ++
                set OLD_TEXT = ( $OLD_TEXT "$NAME[$count_test]" ) 
		@ count_test ++
                set NEW_TEXT = ( $NEW_TEXT "$NAME[$count_test]" ) 
		@ count_test ++
                set TOAST    = ( $TOAST "$NAME[$count_test]" ) 
		@ count_test ++
	end

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
                set OLD_TEXT
                set NEW_TEXT
                set TOAST
                set tests
	        set count_test = 1
                while ( $count_test < $#NAME )
			if ( $INIT_OPTS[$count] == $NAME[$count_test] ) then
                             set tests =  ( $tests "$NAME[$count_test]" )
                             @ count_test ++
                             set OLD_TEXT = ( $OLD_TEXT "$NAME[$count_test]" )
                             @ count_test ++
                             set NEW_TEXT = ( $NEW_TEXT "$NAME[$count_test]" )
                             @ count_test ++
                             set TOAST    = ( $TOAST "$NAME[$count_test]" )
                             @ count_test ++
                        else
                             @ count_test += 4
			endif
		end
	end
	
	#	If there a no tests requested, then they (by default) get them all

	if ( $count_test == 1 ) then
                echo no valid test requested in argument list
                exit
	endif
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

#	Any exceptions to where they can run?  NONE means no 
#	exceptions, should run on all machines.  AIX/Linux/OSF1
#	means it will NOT run on that single machine.  The 
#	option ONLY_AIX/ONLY_Linux/ONLY_OSF1 means that the option
#	ONLY works on that specific architecture.


#	Loop over all selected tests.

set count_test = 0
while ( $count_test < $#tests )

	@ count_test ++

	set count = 1 
	while ( $count < $#NAME )

	    if ( "$tests[$count_test]" == "$NAME[$count]" ) then
		    goto FOUND_SELECTED_TEST
	    endif
	    @ count += 4
	end
	echo "Hmmm, no valid test found"
	exit ( 11 )

FOUND_SELECTED_TEST:

	#	Specifically skip this test on this architecture.

	if      ( `uname` == $TOAST[$count_test] ) then
		echo skipping test $tests[$count_test] for `uname` specifically

	#	Skip this test on this architecture because it ONLY runs
	#	on a different architecture.

	else if ( ( `echo $TOAST[$count_test] | cut -c 1-5` == ONLY_ ) && ( ONLY_`uname` != $TOAST[$count_test] ) ) then
		echo skipping test $tests[$count_test] for `uname`, works on $TOAST[$count_test]

	#	OK, we are allowed to run this test on this architecture.

	else if ( ( $TOAST[$count_test] == NONE ) || \
		  ( $TOAST[$count_test] != `uname` ) || \
		  ( ONLY_`uname` ==  $TOAST[$count_test] ) ) then
		echo doing test $tests[$count_test] for `uname`

		#	If this is the generate or compare baseline test, where do we
		#	save the data to/read the data from.

		if ( ( $BASELINE == GENERATE ) || ( $BASELINE == COMPARE ) ) then
			if ( `uname` == AIX ) then
				set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`/$tests[$count_test]
			else if ( ( `uname` == OSF1 ) && ( `hostname | cut -c 1-6` == joshua ) ) then
				set SAVE_DIR = /data3/mp/${USER}/BASELINE/`uname`/$tests[$count_test]
			else if ( ( `uname` == Linux ) && ( `hostname` == master ) ) then
				set SAVE_DIR = /big6/gill/DO_NOT_REMOVE_DIR/BASELINE/`uname`/$tests[$count_test]
			else
				echo No idea where to put the data, stopping
				exit ( 2 )
			endif

			#	Either zap existing stuff (GENERATE), or make sure it is there (COMPARE)

			if        ( $BASELINE == GENERATE ) then
				/bin/rm -rf $SAVE_DIR
			else if ( ( $BASELINE == COMPARE  )  && ( $tests[$count_test] != Full_Optimization ) ) then
				if ( ! -d $SAVE_DIR ) then
					echo $SAVE_DIR does not exist for BASELINE comparison, stopping
					exit ( 3 )
				endif
			else if ( ( $BASELINE == COMPARE  )  && ( $tests[$count_test] == Full_Optimization ) ) then
				echo No comparison done with baseline since this is an optimized run
			endif
		endif

		#	Build the short edit input script for ed and edit the regtest.csh file.

                set OLDT = `echo $OLD_TEXT[$count_test] | sed 's/=/ = /'`
                set NEWT = `echo $NEW_TEXT[$count_test] | sed 's/=/ = /'`

		if      ( ( $BASELINE == RUN_ONLY ) || ( $tests[$count_test] == Full_Optimization ) ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
,s/$OLDT/$NEWT/
w reg.foo.$count_test.$tests[$count_test]
q
EOF
		else if ( $BASELINE == GENERATE ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
,s/$OLDT/$NEWT/
,s?GENERATE_BASELINE = FALSE?GENERATE_BASELINE = $SAVE_DIR?
w reg.foo.$count_test.$tests[$count_test]
q
EOF
		else if ( $BASELINE == COMPARE  ) then
			if ( -e ed.in ) rm ed.in
			cat >! ed_in << EOF
,s/$OLDT/$NEWT/
,s?COMPARE_BASELINE = FALSE?COMPARE_BASELINE = $SAVE_DIR?
w reg.foo.$count_test.$tests[$count_test]
q
EOF
		endif
		ed regtest.csh < ed_in >& /dev/null
		chmod +x reg.foo.$count_test.$tests[$count_test]

		#	On AIX, we submit jobs to the load leveler queue.  After submission,
		#	we wait around until it completes, and then we send in the next one.

		if ( `uname` == AIX ) then
			llsubmit reg.foo.$count_test.$tests[$count_test] >&! llsub.out
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
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.output wrftest.output.$count_test.$tests[$count_test]
			rm llsub.out llq.report

		#	On the "other" non-queued machines, we just execute the script and wait until
		#	we get the process returning control, then we move on.

		else
			reg.foo.$count_test.$tests[$count_test] -f wrf.tar # >&! output.$count_test.$tests[$count_test]
			mv wrftest.output wrftest.output.$count_test.$tests[$count_test]
#			if ( $NUM_TESTS != $#NAME ) then
#				if ( -d regression_test ) rm -rf regression_test
#			else
				if ( -d regression_test ) then
					mv regression_test regression_test.$count_test.$tests[$count_test]
				endif
#			endif
		endif
	endif
end

rm ed_in >& /dev/null
rm io_format >& /dev/null

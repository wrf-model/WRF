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

#       Linux
#       joshua1 or joshua3
#       mkdir /data3/mp/$USER/`hostname`
#       put all_reg.csh, regtest.csh, and wrf.tar in dir
#       execute all_reg.csh
#       takes about 36-48 h
#	flex lm errors show up as fails to compile

#       AIX
#       bluevista or blueice
#       put all_reg.csh, regtest.csh, and wrf.tar in ~
#       execute all_reg.csh
#       takes about 8-10 h

#       Unless you are editing the script, no changes are required below


#=======================================================================
#=======================================================================

#	The only really important thing.

if ( ! -e wrf.tar ) then
	echo " "
	echo "Whoa there pardner, where is that wrf.tar file"
	echo " "
	exit ( 1 )
endif

#       What these tests do, must be a single string.

set NAME     = ( "Standard"             "NESTED=FALSE"        "NESTED=FALSE"        "NONE"  	1	\
                 "Moving_Nest1"         "NESTED1=FALSE"       "NESTED1=TRUE"        "NONE"  	2	\
                 "Moving_Nest2"         "NESTED2=FALSE"       "NESTED2=TRUE"        "NONE"  	3	\
                 "NMM_Nest"             "NESTED2=FALSE"       "NESTED2=FALSE"       "NONE"  	4	\
                 "Full_Optimization"    "REG_TYPE=BIT4BIT"    "REG_TYPE=OPTIMIZED"  "NONE"  	5	\
                 "Chemistry"            "CHEM=FALSE"          "CHEM=TRUE"           "NONE"  	6	\
                 "Chemistry2"           "KPP=FALSE"           "KPP=TRUE"            "NONE"  	7	\
                 "Quilting"             "QUILT=FALSE"         "QUILT=TRUE"          "NONE"  	8	\
                 "Binary_IO"            "IO_FORM=2"           "IO_FORM=1"           "NONE"  	9	\
                 "GriB1_Output"         "IO_FORM=2"           "IO_FORM=5"           "NONE"  	10	\
                 "REAL8_Floats"         "REAL8=FALSE"         "REAL8=TRUE"          "NONE"	11 	\
                 "FDDA"                 "FDDA=FALSE"          "FDDA=TRUE"           "NONE"  	12	\
                 "FDDA2"                "FDDA2=FALSE"         "FDDA2=TRUE"          "NONE"  	13	\
                 "ESMF_Library"         "ESMF_LIB=FALSE"      "ESMF_LIB=TRUE"       "ONLY_AIX"  14    	\
                 "Global"               "GLOBAL=FALSE"        "GLOBAL=TRUE"         "NONE"      15      \
                 "Adaptive"             "ADAPTIVE=FALSE"      "ADAPTIVE=TRUE"       "NONE"      16      \
               )

#	Where are we located.

set starting_dir = `pwd`

#	Get the tag manually.  This is for the auto-report that gets
#	sent to the WRF web page on the history of the regression tests.

set current_year4  = `date -u +"%Y"`
set current_month  = `date -u +"%m"`
set current_day    = `date -u +"%d"`
set current_hour   = `date -u +"%H"`
set current_minute = `date -u +"%M"`
set current_second = `date -u +"%S"`
set datehms        = ${current_year4}-${current_month}-${current_day}_${current_hour}:${current_minute}:${current_second}_UTC
set dateh          = ${current_year4}${current_month}${current_day}${current_hour}
set date           = ${current_year4}${current_month}${current_day}
if ( ! $?TAG ) then
	if      ( $user == michalak ) then
		set initials = jm
	else if ( $user == hender   ) then
		set initials = th
	else if ( $user == gill     ) then
		set initials = dg
	else if ( $user == dudhia   ) then
		set initials = jd
	else if ( $user == weiwang  ) then
		set initials = ww
	else
		set initials = XX
	endif
	echo the TAG is NOT defined
	echo Please define an environment variable that is the WRFV3 tag
	echo Something such as: setenv TAG trunk_${date}_${initials}
	echo " " 
	exit ( 1 )
endif

#	Are we only interested in the PASS/FAIL report?

if ( $#argv == 1 ) then
	if ( $argv[1] == PASSFAIL ) then
		goto PASSFAIL
	endif
endif

#	If there are any command line args, they are processed, else
#	we run all of the regression tests without a generate or
#	compare flag being set.

if ( $#argv == 0 ) then

	set BASELINE = RUN_ONLY

        set OLD_TEXT
        set NEW_TEXT
        set TOAST
        set tests
	set TEST_NUM
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
                set TEST_NUM = ( $TEST_NUM "$NAME[$count_test]" ) 
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

	#	If there was only one input, and it was telling us to do which type of baseline
	#	option (GENERATE, COMPARE, RUN_ONLY), we assume they want all of the tests
	#	conducted.

	if ( $#INIT_OPTS == 1 ) then
		if ( ( $INIT_OPTS == GENERATE ) || ( $INIT_OPTS == COMPARE  ) || ( $INIT_OPTS == RUN_ONLY ) ) then
		        set OLD_TEXT
		        set NEW_TEXT
		        set TOAST
		        set tests
			set TEST_NUM
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
				set TEST_NUM = ( $TEST_NUM "$NAME[$count_test]" ) 
				@ count_test ++
			end
			goto FINISHED_TEST_LIST
		endif
	endif

	#	Find which tests are to be conducted.  Loop over all of the
	#	input, and compare each of the input fields with the list
	#	of available test names.  When a match occurs, increment the
	#	test found counter, and save the test name.

	set count = 0 
        set OLD_TEXT
        set NEW_TEXT
        set TOAST
        set tests
	set TEST_NUM
	while ( $count < $#INIT_OPTS ) 
		@ count ++
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
				set TEST_NUM = ( $TEST_NUM "$NAME[$count_test]" ) 
				@ count_test ++
                        else
				@ count_test += 5
			endif
		end
	end

	#	If there are no recognizable tests requested, let them know our concern.
	
	if ( $#tests == 0 ) then
                echo No valid test requested in argument list
                exit ( 4 ) 
	endif
endif
FINISHED_TEST_LIST:

#	A friendly check for the baseline directory existence, and locations for known
#	NCAR machines.

if ( ( $BASELINE == GENERATE ) || ( $BASELINE == COMPARE ) ) then
	if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
	                             ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
		set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`
	else if   ( `uname` == AIX ) then
		set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`
	else if ( ( `uname` == Linux  ) && ( `hostname` == basswood ) ) then
		set SAVE_DIR = /basswood/${user}/Regression_Tests/BASELINE/`uname`
	else if ( ( `uname` == Darwin  ) && ( `hostname` == stink ) ) then
		set SAVE_DIR = /stink/${user}/Regression_Tests/BASELINE/`uname`
	else if ( ( `uname` == Linux ) && ( `hostname` == bay-mmm ) ) then
		set SAVE_DIR = /data3/mp/${USER}/BASELINE/`uname`
	else
		echo Hmm, no idea where to put/get this baseline data, stopping
		exit ( 10 )
	endif

	if      ( (   -d $SAVE_DIR ) && ( $BASELINE == GENERATE ) ) then
		echo "Troubles with SAVE_DIR logic."
		echo "$SAVE_DIR should not exist for a $BASELINE run."
#	exit ( 2 )
	else if ( ( ! -d $SAVE_DIR ) && ( $BASELINE == COMPARE  ) ) then
		echo "Troubles with SAVE_DIR logic."
		echo "$SAVE_DIR should exist for a $BASELINE run."
		exit ( 3 )
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
#	exceptions, should run on all machines.  AIX/Linux
#	means it will NOT run on that single machine.  The 
#	option ONLY_AIX/ONLY_Linux means that the option
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
	    @ count += 5
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
			if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
			                             ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
				set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`/$tests[$count_test]
			else if   ( `uname` == AIX ) then
				set SAVE_DIR = /ptmp/${USER}/BASELINE/`uname`/$tests[$count_test]
			else if ( ( `uname` == Linux ) && ( `hostname` == basswood ) ) then
				set SAVE_DIR = /basswood/${USER}/Regression_Tests/BASELINE/`uname`/$tests[$count_test]
			else if ( ( `uname` == Darwin ) && ( `hostname` == stink ) ) then
				set SAVE_DIR = /stink/${USER}/Regression_Tests/BASELINE/`uname`/$tests[$count_test]
			else if ( ( `uname` == Linux ) && ( `hostname` == bay-mmm ) ) then
				set SAVE_DIR = /data3/mp/${USER}/BASELINE/`uname`/$tests[$count_test]
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

                if ( $tests[$count_test] == "NMM_Nest" ) then
			cp regtest_nmmnest.csh reg.foo.$TEST_NUM[$count_test].$tests[$count_test]
		else
			if      ( ( $BASELINE == RUN_ONLY ) || ( $tests[$count_test] == Full_Optimization ) ) then
				if ( -e ed.in ) rm ed.in
				cat >! ed_in << EOF
,s/$OLDT/$NEWT/
,s/1 2 3 4 5 6 7 8 9 10 11 12 13 14/1 2 3 4 5 6 7 8 9 10 11 12/
w reg.foo.$TEST_NUM[$count_test].$tests[$count_test]
q
EOF
			else if ( $BASELINE == GENERATE ) then
				if ( -e ed.in ) rm ed.in
				cat >! ed_in << EOF
,s/$OLDT/$NEWT/
,s/1 2 3 4 5 6 7 8 9 10 11 12 13 14/1 2 3 4 5 6 7 8 9 10 11 12/
,s?GENERATE_BASELINE = FALSE?GENERATE_BASELINE = $SAVE_DIR?
w reg.foo.$TEST_NUM[$count_test].$tests[$count_test]
q
EOF
			else if ( $BASELINE == COMPARE  ) then
				if ( -e ed.in ) rm ed.in
				cat >! ed_in << EOF
,s/$OLDT/$NEWT/
,s/1 2 3 4 5 6 7 8 9 10 11 12 13 14/1 2 3 4 5 6 7 8 9 10 11 12/
,s?COMPARE_BASELINE = FALSE?COMPARE_BASELINE = $SAVE_DIR?
w reg.foo.$TEST_NUM[$count_test].$tests[$count_test]
q
EOF
			endif
			ed regtest.csh < ed_in >& /dev/null
		endif
		chmod +x reg.foo.$TEST_NUM[$count_test].$tests[$count_test]

		#	On AIX, we submit jobs to the load leveler queue for bluesky and to the LSF queue for 
		#	bluevista.  After submission, we wait around until it completes, and then we send in 
		#	the next one.

		if      ( ( `uname` == AIX ) && ( `hostname | cut -c 1-2` == bs   ) ) then
			llsubmit reg.foo.$TEST_NUM[$count_test].$tests[$count_test] >&! llsub.out
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
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.output wrftest.output.$TEST_NUM[$count_test].$tests[$count_test]
			rm llsub.out llq.report
		else if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` == bv ) || ( `hostname | cut -c 1-2` == be ) ) ) then
			bsub < reg.foo.$TEST_NUM[$count_test].$tests[$count_test] >&! bsub.out
			set ok = 0
			set in_already = 0
			while ( $ok == 0 )
				sleep 10 ; bjobs >&! bjobs.report
				grep `cat bsub.out | grep Job | cut -d"<" -f2 | cut -d">" -f1` bjobs.report >& /dev/null
				set ok = $status
				if ( ( $ok == 0 ) && ( $in_already == 0 ) ) then
					set in_already = 1
					set joe_id = `cat bsub.out | grep Job | cut -d"<" -f2 | cut -d">" -f1`
				endif
			end
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.output wrftest.output.$TEST_NUM[$count_test].$tests[$count_test]
			rm bsub.out bjobs.report
		else if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
		                                  ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
			llsubmit reg.foo.$TEST_NUM[$count_test].$tests[$count_test] >&! llsub.out
			set ok = 0
			set in_already = 0
			while ( $ok == 0 )
				sleep 10 ; llq -u $USER >&! llq.report
				set llsubmit_name_all   = `cat llsub.out  | grep '"b'  | cut -d\" -f2`
				set llsubmit_name_front = `echo $llsubmit_name_all | cut -d\. -f1`
				set llsubmit_name_end   = `echo $llsubmit_name_all | cut -d\. -f5`
				grep $llsubmit_name_front llq.report | grep $llsubmit_name_end >& /dev/null
				set ok = $status
				if ( ( $ok == 0 ) && ( $in_already == 0 ) ) then
					set in_already = 1
					set joe_id = `cat llsub.out | grep '"b'  | cut -d\" -f2 | cut -d. -f5`
				endif
			end
			cp /ptmp/$USER/wrf_regression.$joe_id/wrftest.output wrftest.output.$TEST_NUM[$count_test].$tests[$count_test]
			rm llsub.out llq.report

		#	On the "other" non-queued machines, we just execute the script and wait until
		#	we get the process returning control, then we move on.

		else
			reg.foo.$TEST_NUM[$count_test].$tests[$count_test] -f wrf.tar >&! output.$TEST_NUM[$count_test].$tests[$count_test]
			mv wrf_regression/wrftest.output wrftest.output.$TEST_NUM[$count_test].$tests[$count_test]
			if ( -d wrf_regression ) then
				mv wrf_regression wrf_regression.$TEST_NUM[$count_test].$tests[$count_test]
			endif
		endif
	endif
end

if ( ( `uname` == AIX ) && ( ( `hostname | cut -c 1-2` != bs ) && \
                             ( `hostname | cut -c 1-2` != bv ) && ( `hostname | cut -c 1-2` != be ) ) ) then
	echo no web page building, stopping
	exit
endif

PASSFAIL:

#	Build the html page.  We only need the middle portion.  It's
#	a table with 5 columns: Date of test, WRFV3 tag, Developer
#	who conducted the test, machine the test was run on, and the
#	pass/fail status of the all_reg.csh script when compared
#	to the benchmark results (usually a released code).

cat >! history_middle_OK.html << EOF
<tr>
<td> XDATEX </td>
<td> XTAGX </td>
<td> XTESTERX </td>
<td> XARCHITECTUREX </td>
<td BGCOLOR="#00FF00"><a href="ARCHITECTURE/wrftest.all_${datehms}">PASS</a>
                      <a href="ARCHITECTURE/message_${datehms}">DIFFS</a></td>
<td>
<!--
<a href="other_docs/${date}_notes">Descriptions</a>
<a href="other_docs/${date}_minutes">Minutes</a>
-->
</td>
</tr>
EOF

cat >! history_middle_OOPS.html << EOF
<tr>
<td> XDATEX </td>
<td> XTAGX </td>
<td> XTESTERX </td>
<td> XARCHITECTUREX </td>
<td BGCOLOR="#FF0000"><a href="ARCHITECTURE/wrftest.all_${datehms}">FAIL</a>
                      <a href="ARCHITECTURE/message_${datehms}">DIFFS</a></td>
<td>
<!--
<a href="other_docs/${date}_notes">Descriptions</a>
<a href="other_docs/${date}_minutes">Minutes</a>
-->
</td>
</tr>
EOF

set name = `uname`
cat >! ed2.in << EOF
,s/ARCHITECTURE/${name}/g
w history_middle.html
q
EOF

#	Get all of the wrftest.output.* files in one BIG file.

if ( -e wrftest.all_$datehms ) rm wrftest.all_$datehms
cat wrftest.output.?.* wrftest.output.??.* >>! wrftest.all_$datehms
if ( -d `uname` ) rm -rf `uname`
mkdir `uname`
mv wrftest.all_$datehms `uname`

#	Compare regression PASS/FAILs with previous runs.

pushd ~gill/RESULTS/`uname`
grep FAIL wrftest.output.* >! ${starting_dir}/PREV.FAILS
popd
grep FAIL wrftest.output.* >! CURR.FAILS
echo Comparison of regression results on `hostname` for `date` >! message
echo "==================================================================" >> message
echo "     " >> message
echo "Previous FAILs" >> message
echo "==================" >> message
echo "     " >> message
cat PREV.FAILS >> message
echo "     " >> message
echo "Current FAILs" >> message
echo "==================" >> message
echo "     " >> message
cat CURR.FAILS >> message
echo "     " >> message
echo "Difference of FAILs" >> message
echo "==================" >> message
echo "     " >> message
cat CURR.FAILS | grep -vi baseline >! CURR2.FAILS
diff PREV.FAILS CURR2.FAILS >! diffs
set ok = $status
cat diffs >> message
echo "     " >> message
cp message message_$datehms
mv message_$datehms `uname`

#	Send out status info on the regression test.

set OS = `uname`
set NUMARGS = $#argv
if ( $#argv != 0 ) then
	set ARG1 = $argv[1]
else
	set ARG1 = BLANK
endif

if ( ( $NUMARGS == 0 ) || ( $ARG1 == PASSFAIL ) || \
     ( ( $ARG1 == COMPARE  ) && ( $NUMARGS == 1 ) ) || \
     ( ( $ARG1 == GENERATE ) && ( $NUMARGS == 1 ) ) ) then
	if ( $ok != 0 ) then
		Mail -s "REG DIFFS $OS" ${user}@ucar.edu < message
		echo "   "
		echo "Different FAILS from before for $OS - repository may have been broken"
		echo "   "
		m4 -DXDATEX=${dateh} -DXTAGX=${TAG} -DXTESTERX=${user} -DXARCHITECTUREX=`uname` \
			history_middle_OOPS.html >! history_middle_1.html
	else if ( $ok == 0 ) then
		Mail -s "REG OK $OS" ${user}@ucar.edu < message
		echo "   "
		echo "Same FAILS as before for $OS - repository OK"
		echo "   "
		m4 -DXDATEX=${dateh} -DXTAGX=${TAG} -DXTESTERX=${user} -DXARCHITECTUREX=`uname` \
			history_middle_OK.html >! history_middle_1.html
	endif
	
	#	Store on the NCAR MSS system to circumvent security.
	
	ed history_middle_1.html < ed2.in >& /dev/null
	tar -cf `uname`_hist.tar history_middle.html `uname`
	echo Storing info on NCAR MSS
	msrcp `uname`_hist.tar mss:/GILL/ALLREG/`uname`_hist.tar
	if ( -e `uname`_NEW ) rm `uname`_NEW
	echo "`uname` `date`" >! `uname`_NEW
	msrcp `uname`_NEW mss:/GILL/ALLREG/`uname`_NEW
endif

rm ed_in >& /dev/null
rm io_format >& /dev/null
rm PREV.FAILS CURR.FAILS CURR2.FAILS message diffs >& /dev/null
rm history_middle_OOPS.html history_middle_OK.html history_middle_1.html history_middle.html >& /dev/null
rm `uname`_hist.tar `uname`_NEW ed2.in >& /dev/null
rm -rf `uname` >& /dev/null

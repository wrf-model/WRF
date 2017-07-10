#!/bin/csh

set TEST = TEST_1_100m_EXP
set TEST = TEST_2_700m_EXP
set TEST = TEST_3_2000m_SCHAR

if      ( $TEST == TEST_1_100m_EXP ) then

	if ( -d $TEST ) then
		rm -rf $TEST
	endif
	mkdir $TEST

	cp input_sounding-U=10,N=0.01 input_sounding

        foreach opt ( TF HYBRID )
		echo start $TEST $opt `date`
		mkdir $TEST/$opt

		if      ( $opt == TF     ) then
			m4 -DETAC=0.0 -DHYBRID_OPT=0 namelist.input-HILL > namelist.input
		else if ( $opt == HYBRID ) then
			m4 -DETAC=0.2 -DHYBRID_OPT=2 namelist.input-HILL > namelist.input
		endif
		ideal.exe >& print.ideal.${TEST}.$opt
		wrf.exe >&  print.wrf.${TEST}.$opt
		mv print.ideal.${TEST}.$opt print.wrf.${TEST}.$opt wrfi* wrfo* ${TEST}/$opt
		cp namelist.input input_sounding ${TEST}/$opt
        end 

else if ( $TEST == TEST_2_700m_EXP ) then

	if ( -d $TEST ) then
		rm -rf $TEST
	endif
	mkdir $TEST

	cp input_sounding-U=15,N=0.01 input_sounding

        foreach opt ( TF HYBRID )
		echo start $TEST $opt `date`
		mkdir $TEST/$opt

		if      ( $opt == TF     ) then
			m4 -DETAC=0.0 -DHYBRID_OPT=0 namelist.input-HILL-51 > namelist.input
		else if ( $opt == HYBRID ) then
			m4 -DETAC=0.1 -DHYBRID_OPT=2 namelist.input-HILL-51 > namelist.input
		endif
		ideal.exe >& print.ideal.${TEST}.$opt
		wrf.exe >&  print.wrf.${TEST}.$opt
		mv print.ideal.${TEST}.$opt print.wrf.${TEST}.$opt wrfi* wrfo* ${TEST}/$opt
		cp namelist.input input_sounding ${TEST}/$opt
        end 

else if ( $TEST == TEST_3_2000m_SCHAR ) then

	if ( -d $TEST ) then
		rm -rf $TEST
	endif
	mkdir $TEST

	cp input_sounding-layers-20mps input_sounding

        foreach opt ( TF HYBRID )
		echo start $TEST $opt `date`
		mkdir $TEST/$opt

		if      ( $opt == TF     ) then
			m4 -DETAC=0.0 -DHYBRID_OPT=0 namelist.input-HILL-schar > namelist.input
		else if ( $opt == HYBRID ) then
			m4 -DETAC=0.4 -DHYBRID_OPT=2 namelist.input-HILL-schar > namelist.input
		endif
		ideal.exe >& print.ideal.${TEST}.$opt
		wrf.exe >&  print.wrf.${TEST}.$opt
		mv print.ideal.${TEST}.$opt print.wrf.${TEST}.$opt wrfi* wrfo* ${TEST}/$opt
		cp namelist.input input_sounding ${TEST}/$opt
        end 

endif

echo COMPLETE $TEST `date`

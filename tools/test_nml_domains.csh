#!/bin/csh

unalias cp rm 

#	This script needs know where the Registry directory is
#	located and also namelist.input file to review.

#	No arguments, maybe this is a simple default location.
#	Make a few educated guesses

set OK = 1
if (${#argv} == 0 ) then

	#	Is this the WRFV3/run directory
	
	if      ( ( -d ../Registry ) && ( -f namelist.input ) ) then
		set Reg_Dir = ../Registry
		set NML_File = namelist.input 
		set OK = 0
	
	#	Is this the WRFV3/test/em_* directory
	
	else if ( ( -d ../../Registry ) && ( -f namelist.input ) ) then
		set Reg_Dir = ../../Registry
		set NML_File = namelist.input 
		set OK = 0

	endif
endif

if ( $OK != 0 ) then
	if (${#argv} != 2) then
		echo "usage: $0 Full_path/Registry Full_path/namelist.input"
		exit ( 1 )
	else
		set Reg_Dir = $argv[1]
		set NML_File = $argv[2]
	endif
endif

#	Check that the input arguments are OK: Registry

if ( -d $Reg_Dir ) then
	if ( -e ${Reg_Dir}/Registry.EM_COMMON ) then
		#	noop
	else
		echo Cannot find the expected Registry files in the $Reg_Dir directory
		exit ( 3 )
	endif
else
	echo $Reg_Dir is not a valid directory
	exit ( 2 )
endif

#	Check that the input arguments are OK: namelist.input

if ( -e $NML_File ) then
	grep -iq time_control $NML_File
	set OK_time_control = $status
	grep -iq domains      $NML_File
	set OK_domains      = $status
	grep -iq physics      $NML_File
	set OK_physics      = $status
	grep -iq dynamics     $NML_File
	set OK_dynamics     = $status
	if ( ( $OK_time_control == 0 ) && \
	     ( $OK_domains      == 0 ) && \
	     ( $OK_physics      == 0 ) && \
	     ( $OK_dynamics     == 0 ) )then
	else
		echo "The supplied namelist.input file does not seem to have the necessary NML records"
		exit ( 5 )
	endif
else
	echo "Cannot find the namelist.input file specified: $NML_File"
	exit ( 4 )
endif

#	Get a list of all possible variables in the Registry directory
#	that have max_domains, and all variables that have only a single
#	domain of info

if ( -e list_of_all_max_dom_vars ) then
	rm -rf list_of_all_max_dom_vars
endif
touch list_of_all_max_dom_vars

if ( -e list_of_all_one_dom_vars ) then
	rm -rf list_of_all_one_dom_vars
endif
touch list_of_all_one_dom_vars

foreach f ( $Reg_Dir/Registry.* $Reg_Dir/registry.* )
	grep -i ^rconfig $f | grep -i  max_domains | awk '{print $3}' >> list_of_all_max_dom_vars
	grep -i ^rconfig $f | grep -vi max_        | awk '{print $3}' >> list_of_all_one_dom_vars
end

#	Pick up the KNOWN namelist variable max_dom

foreach f ( $Reg_Dir/Registry.* $Reg_Dir/registry.* )
	grep -i ^rconfig $f | grep -iw max_dom     | awk '{print $3}' >> list_of_all_one_dom_vars
end

sort -u list_of_all_max_dom_vars > list_of_all_max_dom_vars_sorted
sort -u list_of_all_one_dom_vars > list_of_all_one_dom_vars_sorted

#	Check the namelist, record by record. Ignore commented out portions,
#	and ignore parts outside of the first "/" namelist record closing character.

sed -e 's/\!.*//' $NML_File > .nml_no_comments
awk '/&/,/\//' .nml_no_comments > .nml_no_comments-within_record_marks
grep "=" .nml_no_comments-within_record_marks > .nml_no_comments-within_record_marks-has_equals
sed -e 's/=/ /' .nml_no_comments-within_record_marks-has_equals > .nml_no_comments-within_record_marks-has_equals-no_equals
sed -e 's/,/ /g' .nml_no_comments-within_record_marks-has_equals-no_equals > .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas
grep -v '\&' .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas | grep -v '\/' > .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks
awk '{print $1     }' .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks > .var_list_only
awk '{print $1 , NF}' .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks > .var_list_num_fields

#	How many domains are we trying to use

grep -iwq max_dom .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks
set OK = $status
if ( $OK != 0 ) then
	echo "The number of domains needs to be specified in the namelist.input file: max_dom"
	exit ( 6 )
else
	set max_dom = `grep -iw max_dom .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks | awk '{print $2}'`
endif

foreach v ( `cat .var_list_only` )
	set num_fields = `grep -iw $v .var_list_num_fields | awk '{print $2}'`

	#	Is this a single variable, or does the var have an entry for each domain

	grep -iwq $v list_of_all_one_dom_vars_sorted
	set OK1 = $status
	grep -iwq $v list_of_all_max_dom_vars_sorted
	set OK2 = $status

	if ( $OK1 == 0 ) then
		if ( $num_fields > 2 ) then
			echo "The $v variable should have only one entry: FATAL"
		endif
	else if ( $OK2 == 0 ) then
                if ( `expr $num_fields - 1` < $max_dom ) then
                        echo "The $v variable should have entries for each domain: BE CAREFUL"
                endif 
	else if ( ( $OK1 != 0 ) && ( $OK2 != 0 ) ) then
		echo "The $v variable is not in any Registry: Problem if you just modified $v in the namelist.input file"
	endif
end

#	Whack the temporary files.

set dummy_files = ( .nml_no_comments .nml_no_comments-within_record_marks .nml_no_comments-within_record_marks-has_equals .nml_no_comments-within_record_marks-has_equals-no_equals .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas .nml_no_comments-within_record_marks-has_equals-no_equals-no_commas-no_record_marks .var_list_num_fields .var_list_only .var_list list_of_all_max_dom_vars list_of_all_max_dom_vars_sorted list_of_all_one_dom_vars list_of_all_one_dom_vars_sorted )

foreach f ( $dummy_files ) 
	rm -rf $f
end

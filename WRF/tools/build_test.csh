#!/bin/csh

set root = data1
set root = mmmtmp

echo starting
if ( ! -d /$root/${user} ) then
	echo give me a break
	exit ( 1 )
endif
cd /$root/${user}

if ( ! -d /$root/${user}/`hostname` ) then
	mkdir /$root/${user}/`hostname`
endif
cd /$root/${user}/`hostname`

if ( ! -e /$root/${user}/wrfv2.tar ) then
	echo need the wrf tar file
	exit ( 2 )
endif

echo untar wrfv2
tar -xf ../wrfv2.tar

if      ( `uname` == SunOS ) then
	set OPTS = ( 1   3   )
else if ( ( `uname` == Linux ) && ( `hostname` == jacaranda ) ) then
	set OPTS = ( 2       )
else if ( `uname` == Linux ) then
	set OPTS = ( 1 2 3   )
else if ( `uname` == OSF1  ) then
	set OPTS = ( 1 3 6   )
endif

foreach opt ( $OPTS )
	cd /$root/${user}/`hostname`/WRFV2

	foreach test ( em_real em_b_wave em_quarter_ss )
		echo building $test for compiler option $opt `date`

		clean -a >& /dev/null

		echo "$opt" | ./configure >&! make.out.${test}.opt=${opt} 

		compile $test >>& make.out.${test}.opt=${opt}

		if ( $test == em_real ) then
			if ( ( ! -e main/real.exe ) || ( ! -e main/wrf.exe ) ) then
				echo missing $test executables
				exit ( 3 )
			endif
		else
			if ( ( ! -e main/ideal.exe ) || ( ! -e main/wrf.exe ) ) then
				echo missing $test executables
				exit ( 4 )
			endif
		endif
		echo $test exec build OK `date`
		echo " "

	end
end

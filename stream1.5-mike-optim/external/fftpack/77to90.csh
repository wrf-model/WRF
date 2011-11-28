#!/bin/csh

#	The NCAR CISL fftpack (version 5)
#	convert files to f90


cd fftpack5

foreach f ( *.f )

	set root = $f:r
	
	#	Get rid of those pesky tab characters

	expand $f >! ${root}.F

	#	The f77 to f90 converter wants files to
	#	end with the .f extension.  

	mv ${root}.F ${root}.f

	echo "${root} /" >! input
	../a.out < input

	#	The converter makes a .f90 file, WRF expects
	#	things to actually be .F extensioned.

	mv ${root}.f90 ${root}.F

	#	Zap the original f77 file.

	rm ${root}.f
	
end

rm input

cd ..

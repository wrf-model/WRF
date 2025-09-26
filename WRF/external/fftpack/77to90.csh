#!/bin/csh

#	The NCAR CISL fftpack (version 5) rename *.f90 to *.F


cd temp

foreach f ( *.f90 )

	set root = $f:r
	
	#	Get rid of those pesky tab characters

	expand $f >! ../${root}.F

	rm ${root}.f90
	
end

cd ..

rm -rf temp

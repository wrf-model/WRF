#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for IBM AIX xlf2003 compiler
#-------------------------------------------------------------------------------#

setenv FC "xlf2003"
setenv FCFLAGS "-qcheck -qdbg -qextchk -qfloat=nomaf:rndsngl -qflttrap=ov:zero:en -qinitauto -qhalt=W -qlanglvl=2003pure -qmaxmem=-1 -qsuffix=f=f90:cpp=fpp:cpp=F90"
setenv LDFLAGS ""
setenv LIBS ""

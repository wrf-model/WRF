#!/bin/csh
#-------------------------------------------------------------------------------#
# PRODUCTION build settings for ftn compiler on EMC Linux Cray machine
#-------------------------------------------------------------------------------#

setenv FC "ftn"
setenv FCFLAGS "-O3 -axCore-AVX2 -fp-model source -convert big_endian -free -assume byterecl"
setenv LDFLAGS ""
setenv LIBS ""

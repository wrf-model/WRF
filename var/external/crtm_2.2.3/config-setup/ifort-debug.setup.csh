#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for Linux ifort compiler
#-------------------------------------------------------------------------------#

setenv FC "ifort"
setenv FCFLAGS "-g -check bounds -e03 -traceback -free -assume byterecl -fp-stack-check -mieee-fp"
setenv LDFLAGS ""
setenv LIBS ""

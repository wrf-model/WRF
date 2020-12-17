#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for Linux ifort compiler
#-------------------------------------------------------------------------------#

setenv FC "ifort"
setenv FCFLAGS "-g -check bounds -e08 -traceback -free -assume byterecl,realloc_lhs -fp-stack-check -mieee-fp"
setenv LDFLAGS ""
setenv LIBS ""

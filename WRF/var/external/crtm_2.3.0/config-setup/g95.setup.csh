#!/bin/csh
#-------------------------------------------------------------------------------#
# PRODUCTION build settings for Linux g95 compiler
#-------------------------------------------------------------------------------#

setenv FC "g95"
setenv FCFLAGS "-O2 -ffast-math -ffree-form -fno-second-underscore -funroll-loops -malign-double"
setenv LDFLAGS ""
setenv LIBS ""

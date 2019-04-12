#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for Linux g95 compiler
#-------------------------------------------------------------------------------#

setenv FC "g95"
setenv FCFLAGS "-fbounds-check -ffree-form -fno-second-underscore -ftrace=frame -malign-double -Wall"
setenv LDFLAGS ""
setenv LIBS ""


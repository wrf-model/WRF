#!/bin/csh
#-------------------------------------------------------------------------------#
# PRODUCTION build settings for Linux gfortran compiler
#-------------------------------------------------------------------------------#

setenv FC "gfortran"
setenv FCFLAGS "-O3 -fimplicit-none -ffree-form -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -Wall -Wconversion -std=f2003"
setenv LDFLAGS ""
setenv LIBS ""

#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for Linux gfortran compiler
#-------------------------------------------------------------------------------#

setenv FC "gfortran"
setenv FCFLAGS "-fbounds-check -fimplicit-none -ffpe-trap=overflow,zero,invalid -ffree-form -fno-second-underscore -frecord-marker=4 -ggdb -Wall -Wconversion -std=f2008"
setenv LDFLAGS ""
setenv LIBS ""

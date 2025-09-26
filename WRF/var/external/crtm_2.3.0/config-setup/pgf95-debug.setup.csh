#!/bin/csh
#-------------------------------------------------------------------------------#
# DEBUG build settings for Linux pgf95 compiler
#-------------------------------------------------------------------------------#

setenv FC "pgf95"
setenv FCFLAGS "-g -Ktrap=ovf,divz -Mdaz -Mbounds -Mchkstk -Mdclchk -Minform,inform -Mnosave -Mref_externals -Kieee"
setenv LDFLAGS "-Kieee"
setenv LIBS ""

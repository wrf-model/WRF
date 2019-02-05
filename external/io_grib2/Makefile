#------------------------------------------------------------------------------
#  Makefile for producing libdbclient.so 
#
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#

.SUFFIXES:      .c .o

#
# Specity location for Makefiles that are included.
#
INCLUDEDIRS = -I.
BUILD_DIR = $(IO_GRIB_SHARE_DIR)../io_grib_share/build
#
# Specify directory that output library is to be put in.
#
LIB_DEST = .
#
#  SPECIFY local include directories used during compilation of source code. 
#
#    CXX_INCLUDES is for C++ files
#    C_INCLUDES is for C files
#
C_INCLUDES   = -I.
CXX_INCLUDES = -I.
F_INCLUDES   = -I. -Ig2lib -Ibacio-1.3 -I../io_grib_share
ARFLAGS      = cruv

FORMAT = $(FREE)

#
#  List of subdirectories to which to pass make commands.
#
LIB_DIRS = \
	bacio-1.3 \
	g2lib
EXE_DIRS =
SUB_DIRS = $(LIB_DIRS) $(EXE_DIRS)

#
#  SPECIFY information for building a library:
#
#    LIB_NAME - Fragment of name of the library to build
#               e.g. if library file name is libfoo.so, set LIB_NAME = foo 
#    DEP_LIBS - The tokens required to link a shared library against other
#               shared libraries upon which it depends.  DEP_LIBS should
#               contain -L<dir> tokens to specify where the dependent 
#               libraries are, and -l<lib> tokens to specify libraries to link.
#    OBJS     - List of object files that go into the library.
#
#  NOTES:
#  1. Be careful about whitespace after the last character in the LIB_NAME.
#     These spaces will generate an error when the library is made.
#
LIB_NAME = io_grib2
SYS_DEFINES =
DEP_LIBS = 
OBJS =  \
	grib2tbls_types.o \
	io_grib2.o \
	read_grib2map.o

#
#  Clean up old build files
#
superclean:
	@/bin/rm -f *.o > /dev/null 2>&1
	@/bin/rm -f *.f90 > /dev/null 2>&1
	@/bin/rm -f *.mod > /dev/null 2>&1
	@/bin/rm -f *.a > /dev/null 2>&1
	@( cd bacio-1.3 ; make clean )
	@( cd g2lib ; make clean )


#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
read_grib2map.o: grib2tbls_types.o

io_grib2.o: grib2tbls_types.o

# DO NOT DELETE THIS LINE -- make depend depends on it.

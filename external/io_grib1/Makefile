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
INCLUDEDIRS = -I. -I./MEL_grib1 -Igrib1_util -I../io_grib_share -I../
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
C_INCLUDES   = $(INCLUDEDIRS)
CXX_INCLUDES = $(INCLUDEDIRS)
F_INCLUDES   = $(INCLUDEDIRS)
ARFLAGS      = cruv

FORMAT = $(FREE)

#
#  SPECIFY any subdirectories containing libraries that may be dynamically
#          linked by this library.
#
SUB_DIRS =

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
LIB_NAME = io_grib1
SYS_DEFINES =
DEP_LIBS = 
OBJS =  \
	grib1_routines.o \
	gribmap.o \
	io_grib1.o \
	trim.o

#
#  List of subdirectories to which to pass make commands.
#
LIB_DIRS = \
	MEL_grib1 \
	grib1_util \
	WGRIB
EXE_DIRS =
SUB_DIRS = $(LIB_DIRS) $(EXE_DIRS)

#
#  Clean up old build files
#
superclean:
	@/bin/rm -f *.o *.obj > /dev/null 2>&1
	@/bin/rm -f *.f90 > /dev/null 2>&1
	@/bin/rm -f *.mod > /dev/null 2>&1
	@/bin/rm -f *.a > /dev/null 2>&1
	@/bin/rm -f wgrid.exe > /dev/null 2>&1
	@( cd grib1_util ; make clean )
	@( cd MEL_grib1 ; make clean )
	@( cd WGRIB ; make clean )

#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
# DO NOT DELETE THIS LINE -- make depend depends on it.

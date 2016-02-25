#------------------------------------------------------------------------------
#  Makefile for producing libdbclient.so 
#
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#

.SUFFIXES:      .c .o

# Specity location for Makefiles that are included.
#
BUILD_DIR = ../../io_grib_share/build
INCLUDEDIRS = -I. -I.. -I ../MEL_grib1
#
# Specify directory that output library is to be put in.
#
LIB_DEST = ..
#
#  SPECIFY local include directories used during compilation of source code. 
#
#    CXX_INCLUDES is for C++ files
#    C_INCLUDES is for C files
#
C_INCLUDES   = -I. -I../MEL_grib1
CXX_INCLUDES = -I. -I../MEL_grib1
ARFLAGS      = ruv
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
UNIT_TEST = test_rg
LIB_NAME = io_grib1
DEP_LIBS =  -L$(LIB_DEST) -lMEL_grib1
OBJS =  alloc_2d.o \
	read_grib.o \
	write_grib.o

#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
# DO NOT DELETE THIS LINE -- make depend depends on it.

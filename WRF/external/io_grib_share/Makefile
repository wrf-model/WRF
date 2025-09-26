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
BUILD_DIR = $(IO_GRIB_SHARE_DIR)./build
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
F_INCLUDES   = -I.
AR           = ar
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
LIB_NAME = io_grib_share
SYS_DEFINES =
DEP_LIBS = 
OBJS =  \
	io_grib_share.o \
	get_region_center.o \
	gridnav.o \
	open_file.o

#
#  List of subdirectories to which to pass make commands.
#
LIB_DIRS = 
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


#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
# DO NOT DELETE THIS LINE -- make depend depends on it.

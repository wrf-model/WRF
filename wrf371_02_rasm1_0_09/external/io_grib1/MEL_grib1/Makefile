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
INCLUDEDIRS = -I. -I.. -I../grib1_util
BUILD_DIR = ../../io_grib_share/build
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
C_INCLUDES   = -I.
CXX_INCLUDES = -I.
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
LIB_NAME = io_grib1
DEP_LIBS = 
OBJS =  FTP_getfile.o \
	apply_bitmap.o \
	display_gribhdr.o \
	gbyte.o \
	grib_dec.o \
	grib_enc.o \
	grib_seek.o \
	gribgetbds.o \
	gribgetbms.o \
	gribgetgds.o \
	gribgetpds.o \
	gribhdr2file.o \
	gribputbds.o \
	gribputgds.o \
	gribputpds.o \
	hdr_print.o \
	init_dec_struct.o \
	init_enc_struct.o \
	init_gribhdr.o \
	init_struct.o \
	ld_dec_lookup.o \
	ld_enc_input.o \
	ld_enc_lookup.o \
	ld_grib_origctrs.o \
	make_default_grbfn.o \
	make_grib_log.o \
	map_lvl.o \
	map_parm.o \
	pack_spatial.o \
	prt_inp_struct.o \
	upd_child_errmsg.o \
	prt_badmsg.o \
	swap.o\
	grib_uthin.o\
	set_bytes.o

# 
# List the header files that should be installed.
#
HDRS =	gribfuncs.h \
	grib.h \
	input.h

#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
# DO NOT DELETE THIS LINE -- make depend depends on it.

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
F_INCLUDES   = -I. -Ig2lib
FORMAT       = $(FIXED)
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
LIB_NAME = io_grib2
CPPFLAGS = -DUSE_JPEG2000
# 
# To have the option of using the PNG compression scheme in the grib2 output,
#   uncomment the following line.  In addition, you will need to assure
#   that you have libpng and libz installed on your system.  If the libraries
#   are not in standard system locations, you will need to modify your 
#   configure.wrf file in order to specify the locations of the libraries.
#
#CPPFLAGS = -DUSE_PNG -DUSE_JPEG2000
# 
# In addition, you will need to add the following objects to the object list
#   below.
#        pngpack.o \
#        pngunpack.o \
#        enc_png.o \
#        dec_png.o \
#
#  Further, you will need to add "-lpng" to your link line in configure.wrf
#
# Note: PNG functionality has only been tested on Linux.
#

DEP_LIBS = 
OBJS =  \
	gridtemplates.o \
        pdstemplates.o \
        drstemplates.o \
        gribmod.o \
        realloc.o \
        addfield.o \
        addgrid.o \
        addlocal.o \
        getfield.o \
        gb_info.o \
        gf_getfld.o \
        gf_free.o \
        gf_unpack1.o \
        gf_unpack2.o \
        gf_unpack3.o \
        gf_unpack4.o \
        gf_unpack5.o \
        gf_unpack6.o \
        gf_unpack7.o \
        gettemplates.o \
        getlocal.o \
        getdim.o \
        getpoly.o \
        gribcreate.o \
        gribend.o \
        gribinfo.o \
        mkieee.o \
        rdieee.o \
        simpack.o \
        simunpack.o \
        cmplxpack.o \
        compack.o \
        misspack.o \
        pack_gp.o \
        reduce.o \
        comunpack.o \
        specpack.o \
        specunpack.o \
        jpcpack.o \
        jpcunpack.o \
        enc_jpeg2000.o \
        dec_jpeg2000.o \
        gbytesc.o \
        skgb.o \
        ixgb2.o \
        getidx.o \
        getg2i.o \
        getg2ir.o \
        getgb2s.o \
        getgb2r.o \
        getgb2l.o \
        getgb2.o \
        getgb2p.o \
        getgb2rp.o \
        putgb2.o \
        g2grids.o \
        gdt2gds.o \
        mova2i.o \
        params.o

#
#  Include the boilerplate rules for building library modules.
#
include $(BUILD_DIR)/library_rules.mk

#
#  Compile dependencies.  These are appended to this file by make depend. 
#
addfield.o: pdstemplates.o drstemplates.o

addgrid.o: gridtemplates.o

getfield.o: gridtemplates.o pdstemplates.o drstemplates.o

getg2ir.o: realloc.o

getgb2.o: gribmod.o

getgb2l.o: gribmod.o

getgb2p.o: gribmod.o

getgb2r.o: gribmod.o

getgb2s.o: gribmod.o

gf_free.o: gribmod.o

gf_getfld.o: gribmod.o

gf_unpack3.o: gridtemplates.o realloc.o

gf_unpack4.o: pdstemplates.o realloc.o

gf_unpack5.o: drstemplates.o realloc.o

ixgb2.o: realloc.o

putgb2.o: gribmod.o

# DO NOT DELETE THIS LINE -- make depend depends on it.

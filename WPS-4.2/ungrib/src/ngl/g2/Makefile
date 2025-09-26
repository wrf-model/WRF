include $(DEV_TOP)/configure.wps

#  Make sure one of the following options appears in your CFLAGS
#  variable to indicate which system you are on.  Used to call
#  "C" routines from Fortran.
#  -DLINUX, -DLINUXG95, -DSGI, -DHP, -DCRAY90, -DAIX, -DLINUXF90, -DVPP5000

#  If you want to enable support for PNG or JPEG2000 encoding/decoding,
#  you must specify -DUSE_PNG and/or -DUSE_JPEG2000 in the FDEFS variable
#  for the Fortran pre-processor
#  -DUSE_PNG requires libpng.a and libz.a
#                     ( and png.h pngconf.h zconf.h zlib.h include files).
#  -DUSE_JPEG2000 requires libjasper.a
#                     ( and all the jasper/*.h include files).
#

CFLAGS2	=	$(CPPFLAGS) $(COMPRESSION_INC) $(FDEFS) -D__64BIT__
LIB	=	libg2_4.a

#--------------------------------------
# The following was used for XLF on AIX
#DEFS=-DAIX -DHAVE_SYS_TYPES_H=1
#FC=ncepxlf
#CC=ncepxlc
#CPP=/usr/ccs/lib/cpp -P
#FFLAGS=-O3 -g -qnosave -qarch=auto
#CFLAGS=-O3 -q64 -g -qcpluscmt -qarch=auto $(DEFS) $(INCDIR)
#ARFLAGS=-X64
#--------------------------------------
# The following was used for G95 on LINUX
#
#DEFS=-DLINUXG95
#FC=g95
#CC=cc
#CPP=cpp -P -C
#MODDIR=.
#FFLAGS=-O3 -g -I $(MODDIR)
#CFLAGS=-O3 $(DEFS) $(INCDIR)
#CFLAGS=-O3 $(DEFS) $(INCDIR) -D__64BIT__
#ARFLAGS=
#--------------------------------------

OBJS	=	gridtemplates.o pdstemplates.o drstemplates.o gribmod.o realloc.o intmath.o addfield.o  \
		addgrid.o addlocal.o getfield.o gb_info.o gf_getfld.o gf_free.o gf_unpack1.o  \
		gf_unpack2.o gf_unpack3.o gf_unpack4.o gf_unpack5.o gf_unpack6.o gf_unpack7.o \
		gettemplates.o getlocal.o getdim.o getpoly.o gribcreate.o gribend.o gribinfo.o \
		mkieee.o rdieee.o simunpack.o \
		reduce.o comunpack.o specunpack.o jpcunpack.o \
		dec_jpeg2000.o pngunpack.o dec_png.o gbytesc.o skgb.o ixgb2.o \
		getidx.o getg2i.o getg2ir.o getgb2s.o getgb2r.o getgb2l.o getgb2.o getgb2p.o getgb2rp.o \
		putgb2.o g2grids.o gdt2gds.o params.o params_ecmwf.o mova2i.o

all:	$(LIB)

$(LIB):	$(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

clean:
	$(RM) $(OBJS)
	$(RM) *.a *.M *.mod
	
superclean:	clean

.F.o:
	$(CPP) $(FDEFS) $*.F > $*.f90
	$(FC) -c $(F77FLAGS) $*.f90
	/bin/rm -f $*.f90

.f.o:
	$(FC) -c $(F77FLAGS) $*.f

.c.o:
	$(CC) -c $(CFLAGS) $(CFLAGS2) $<

#makefile to build io_int that does binary i/o

OBJSL   = io_grib1.o pdstool.o std_ncep_gds.o wrt_grib_rec.o wrt_grib_msg.o \
  append_24section.o cpGRIBsec.o mk_bds_bms.o \
  gribw_aux.o gribw_private.o intpower.o ibm2flt.o \
  flt2ibm.o flist2bit.o gdstool.o rd_grib_msg.o rd_grib_rec.o \
  seekgrib.o readgrib.o get_nxny.o BDSunpk.o  unpk_bds.o gribw_time.o \
  inv_scan.o open_file.o write_grib.o gridnav.o gribmap.o
OBJS    = $(OBJSL)
LIBS    = 
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
CPPFLAGS = -C -P

.SUFFIXES:      .F90 .F .f .c .o

all : libio_grib1.a wgrib_exe

wgrib_exe :
	( cd WGRIB ; make "CC=$(CC)" )
	ln -s WGRIB/wgrib .

libio_grib1.a:		$(OBJS)
			/bin/rm -f libio_grib1.a
			$(AR) cr libio_grib1.a $(OBJSL)
			$(RANLIB) libio_grib1.a

io_grib1.o:     io_grib1.F 
	mv $*.F $*.F90
	$(CPP) $(CPPFLAGS) -I../../main -I. io_grib1.F90 | $(M4) - > io_grib1.f
	$(FC) $(FCFLAGS) -I../../main -I../../inc -c io_grib1.f
	mv $*.F90 $*.F

pdstool.o: pdstool.c pdstool.h

std_ncep_gds.o: std_ncep_gds.c gribw.h

wrt_grib_rec.o: wrt_grib_rec.c gribw.h

wrt_grib_msg.o: wrt_grib_msg.c gribw.h

append_24section.o: append_24section.c gribw.h

cpGRIBsec.o: cpGRIBsec.c gribw.h

mk_bds_bms.o: mk_bds_bms.c gribw.h

gribw_aux.o: gribw_aux.c gribw.h

gribw_private.o: gribw_private.c gribw.h

intpower.o: intpower.c 

ibm2flt.o: ibm2flt.c 

flt2ibm.o: flt2ibm.c

flist2bit.o: flist2bit.c 

gdstool.o: gdstool.c gdstool.h

rd_grib_msg.o: rd_grib_msg.c pds4.h gribw.h

rd_grib_rec.o: rd_grib_rec.c gribwlib.h

seekgrib.o: seekgrib.c grib.h

readgrib.o: readgrib.c grib.h

get_nxny.o: get_nxny.c gds.h bms.h bds.h gribwlib.h

BDSunpk.o: BDSunpk.c grib.h pds4.h bms.h bds.h

unpk_bds.o: unpk_bds.c gribw.h pds4.h bms.h gds.h gribwlib.h

gribw_time.o: gribw_time.c gribw_time.h

inv_scan.o: inv_scan.c 

open_file.o: open_file.c 

write_grib.o: write_grib.c 

gribmap.o: gribmap.c

gridnav.o: gridnav.c gridnav.h

gribw.h: gribw_macros.h gribw_time.h

gribwlib.h: pdstool.h gdstool.h gribw_time.h

superclean: clean
			/bin/rm *.a

clean:  
			/bin/rm -f  *.f *.o *.mod
			( cd WGRIB ; make clean )
			/bin/rm wgrib

.c.o:
	$(CC) -c $(CFLAGS) $*.c


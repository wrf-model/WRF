#makefile to build io_int that does binary i/o

OBJSL   = io_grib1.o gribmap.o gridnav.o open_file.o grib1_routines.o trim.o
OBJS    = $(OBJSL)
LIBS    = -lm
LOCAL_LIBS = MEL_grib1 grib1_util
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
#RANLIB	= ranlib
RANLIB  = echo
CPPFLAGS = -C -P
INCLUDEDIRS  = -I. -Igrib1_util -IMEL_grib1
FC	= ifort

.SUFFIXES:      .F90 .F .f .c .o

all : clean_a $(LOCAL_LIBS) libio_grib1.a wgrib

wgrib :
	( cd WGRIB ; make "CC=$(CC)" )
	ln -sf WGRIB/wgrib .

clean_a :
	/bin/rm -f libio_grib1.a

libio_grib1.a:	$(OBJS) makelib

makelib: .FORCE
	$(AR) cruv libio_grib1.a $(OBJS)

io_grib1.o:     io_grib1.F 
	$(CPP) $(CPPFLAGS) -I../../main io_grib1.F -I. | $(M4) - > io_grib1.f
	$(FC) $(FCFLAGS) -I../../main -I../../inc -I. -c io_grib1.f

test_write_grib: test_write_grib.c grib1_routines.c gridnav.c gribmap.c open_file.c trim.c
	$(CC) $(CFLAGS) -g test_write_grib.c grib1_routines.c gridnav.c gribmap.c open_file.c trim.c \
		libgrib1_util.a libMEL_grib1.a -lm

test_grib1_routines: test_grib1_routines.F90 gridnav.c gribmap.c open_file.c trim.c
	$(FC) -c -g test_grib1_routines.F90
	$(CC) $(CFLAGS) -c -g grib1_routines.c gridnav.c gribmap.c open_file.c trim.c
	$(FC) -g -o test_grib1_routines test_grib1_routines.o grib1_routines.o gridnav.o gribmap.o open_file.o trim.o \
		libgrib1_util.a libMEL_grib1.a -lm

MEL_grib1: .FORCE
	cd MEL_grib1; \
	make CC=$(CC) archive; \
	cd ..

grib1_util: .FORCE
	cd grib1_util; \
	make CC=$(CC) archive; \
	cd ..

.FORCE:

superclean: clean
	/bin/rm *.a; \
	cd MEL_grib1; rm *.a; make clean; cd ..;\
	cd grib1_util; rm *.a; make clean; cd ..

clean:  
	/bin/rm -f  *.f *.o *.mod; \
	cd MEL_grib1; make clean; cd ..; \
	cd grib1_util; make clean; cd ..; \
	cd WGRIB; make clean; cd ..

.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDEDIRS) $(DEBUG) $*.c

# DO NOT DELETE THIS LINE -- make depend depends on it.

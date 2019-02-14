#makefile to build a wrf-phdf5 with netCDF

OBJSL   = wrf-phdf5.o wrf-phdf5attr.o wrf-phdf5support.o
OBJS    = $(OBJSL)
OPTS    = -bmaxdata:0x80000000
FFLAGS  =  $(OPTS) -I$(PHDF5PATH)/lib -I../ioapi_share
FORTRANLIB=-I$(PHDF5PATH)/lib $(PHDF5PATH)/lib/libhdf5_fortran.a
LIBSHDF = $(FORTRANLIB) $(PHDF5PATH)/lib/libhdf5.a
LIB     =-lm -lz
CPP     = /lib/cpp -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f90 .o .code

all : libwrfio_phdf5.a 

libwrfio_phdf5.a:		$(OBJS)
			/bin/rm -f libwrfio_phdf5.a
			$(AR) cr libwrfio_phdf5.a $(OBJSL)
			$(RANLIB) libwrfio_phdf5.a

wrf-phdf5support.o:      wrf-phdf5support.F90 
			$(CPP) -I../ioapi_share wrf-phdf5support.F90 | $(M4) - > wrf-phdf5support.f90
			$(FC) $(FFLAGS) -c wrf-phdf5support.f90
wrf-phdf5attr.o:         wrf-phdf5attr.F90 wrf-phdf5support.o
			$(CPP) wrf-phdf5attr.F90 | $(M4) - > wrf-phdf5attr.f90
			$(FC) $(FFLAGS) -c wrf-phdf5attr.f90

wrf-phdf5.o:             wrf-phdf5.F90 wrf-phdf5attr.o wrf-phdf5support.o
			$(CPP) wrf-phdf5.F90 | $(M4) - > wrf-phdf5.f90
			$(FC) $(FFLAGS) -c wrf-phdf5.f90

clean:
	@/bin/rm -f *.f90 *.o *.mod

superclean: clean
	@/bin/rm -f libwrfio_phdf5.a


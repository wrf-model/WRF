#makefile to build a wrf_io with PIO
#$Id$

FCOPTIM         = -O0 -g
FCNOOPT         = -O0 -fno-inline -fno-ip -g
FCDEBUG         =  -g $(FCNOOPT) -traceback # -fpe0 -check all -ftrapuv -unroll0 -u
FORMAT_FIXED    = -FI
FORMAT_FREE     = -FR
FCSUFFIX        =
BYTESWAPIO      = -convert big_endian
FCBASEOPTS_NO_G = -ip -fp-model precise -w -ftz -align all -fno-alias $(FORMAT_FREE) $(BYTESWAPIO)
FCBASEOPTS      = $(FCBASEOPTS_NO_G) $(FCDEBUG)
FCFLAGS         = $(FCOPTIM) $(FCBASEOPTS)

TRADFLAG        = -traditional
CPP             = /lib/cpp -P
AR              = ar
ARFLAGS         = ru
M4              = m4
RANLIB          = ranlib
DM_FC           = mpiifort
FC              = $(DM_FC)

OBJS    = wrf_data_pio.o pio_routines.o wrf_io.o field_routines.o read_bdy_routines.o module_wrfsi_static.o

PIOPATH = /glade/p/work/huangwei/lib/intel
FFLAGS  = $(FCFLAGS) -I. -I$(PIOPATH)/include -I../ioapi_share -I../../frame -I../esmf_time_f90
LIBS    = -L$(PIOPATH)/lib -lpio
CPP1    = $(CPP) -P $(TRADFLAG) -DINTSPECIAL
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o

all : libwrfio_pio.a 

libwrfio_pio.a:		$(OBJS) $(CODE)
			/bin/rm -f libwrfio_pio.a
			$(AR) cr libwrfio_pio.a $(OBJS)
			$(RANLIB) libwrfio_pio.a

wrf_data_pio.o:         wrf_data_pio.F90 $(CODE)
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share wrf_data_pio.F90 > wrf_data_pio.f
			$(FC) $(FFLAGS) -c wrf_data_pio.f

pio_routines.o:		pio_routines.F90 $(CODE) wrf_data_pio.o
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share pio_routines.F90 > pio_routines.f
			$(FC) $(FFLAGS) -c pio_routines.f

wrf_io.o:               wrf_io.F90 $(CODE) wrf_data_pio.o read_bdy_routines.o field_routines.o
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share wrf_io.F90 > wrf_io.f
			$(FC) $(FFLAGS) -c wrf_io.f

module_wrfsi_static.o:  module_wrfsi_static.F90
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share module_wrfsi_static.F90 > module_wrfsi_static.f
			$(FC) $(FFLAGS) -c module_wrfsi_static.f

field_routines.o:	field_routines.F90 wrf_data_pio.o pio_routines.o
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

read_bdy_routines.o:	read_bdy_routines.F90 wrf_data_pio.o pio_routines.o
			$(CPP1) -I. -I$(PIOPATH)/include -I../ioapi_share read_bdy_routines.F90 > read_bdy_routines.f
			$(FC) $(FFLAGS) -c read_bdy_routines.f

clean:	superclean

superclean:
			@/bin/rm -f *.f *.o \
			*.mod libwrfio_pio.a

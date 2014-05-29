#makefile to build a wrf_io with PIO

FCOPTIM         = -O3
FCNOOPT         = -O0 -fno-inline -fno-ip
FCDEBUG         = # -g $(FCNOOPT) -traceback # -fpe0 -check all -ftrapuv -unroll0 -u
FORMAT_FIXED    = -FI
FORMAT_FREE     = -FR
FCSUFFIX        =
BYTESWAPIO      = -convert big_endian
FCBASEOPTS_NO_G = -ip -fp-model precise -w -ftz -align all -fno-alias $(FORMAT_FREE) $(BYTESWAPIO)
FCBASEOPTS      = $(FCBASEOPTS_NO_G) $(FCDEBUG)
FCFLAGS         = $(FCOPTIM) $(FCBASEOPTS)

#PNETCDFPATH     = /glade/p/work/huangwei/lib/pnetcdf/1.3.1
PIO_PATH        = /glade/p/work/huangwei/lib/pio/1.8.8
TRADFLAG        = -traditional
CPP             = /lib/cpp -P
AR              = ar
ARFLAGS         = ru
M4              = m4
RANLIB          = ranlib
DM_FC           = mpfort
FC              = $(DM_FC)

OBJS    = wrf_data_pio.o wrf_io.o field_routines.o module_wrfsi_static.o
CODE    = ext_pio_get_dom_ti.code \
	  ext_pio_get_var_td.code \
	  ext_pio_get_var_ti.code \
	  ext_pio_put_dom_ti.code \
	  ext_pio_put_var_td.code \
	  ext_pio_put_var_ti.code \
	  transpose.code 

FFLAGS  = $(FCFLAGS) -I. -I$(PIO_PATH)/include -I../ioapi_share
LIBS    = -L$(PIO_PATH)/lib -lpio
CPP1    = $(CPP) -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o .code

all : libwrfio_pio.a 

libwrfio_pio.a:		$(OBJS) $(CODE)
			/bin/rm -f libwrfio_pio.a
			$(AR) cr libwrfio_pio.a $(OBJS)
			$(RANLIB) libwrfio_pio.a

wrf_data_pio.o:         wrf_data_pio.F90 $(CODE)
			$(CPP1) -I. -I$(PIO_PATH)/include -I../ioapi_share wrf_data_pio.F90 | $(M4) - > wrf_data_pio.f
			$(FC) $(FFLAGS) -c wrf_data_pio.f

wrf_io.o:               wrf_io.F90 $(CODE) wrf_data_pio.o
			$(CPP1) -I. -I$(PIO_PATH)/include -I../ioapi_share wrf_io.F90 | $(M4) - > wrf_io.f
			$(FC) $(FFLAGS) -c wrf_io.f

module_wrfsi_static.o:  module_wrfsi_static.F90
			$(CPP1) -I. -I$(PIO_PATH)/include -I../ioapi_share module_wrfsi_static.F90 > module_wrfsi_static.f
			$(FC) $(FFLAGS) -c module_wrfsi_static.f

field_routines.o:	field_routines.F90 wrf_io.o
			$(CPP1) -I. -I$(PIO_PATH)/include -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

superclean:
			/bin/rm -f *.f *.o \
			*.mod libwrfio_pio.a

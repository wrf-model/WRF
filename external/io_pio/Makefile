#makefile to build a wrf_io with PIO
#$Id$

OBJS    = wrf_data_pio.o pio_routines.o wrf_io.o field_routines.o read_bdy_routines.o module_wrfsi_static.o
FFLAGS  = $(FCFLAGS) -I. -I$(NETCDFPATH)/include -I../ioapi_share -I../../frame -I../esmf_time_f90 -I../../main
LIBS    = -L$(NETCDFPATH)/lib -lpio
CPP1    = $(CPP) -P $(TRADFLAG) #-DINTSPECIAL

.SUFFIXES:      .F90 .f .o

all : libwrfio_pio.a

libwrfio_pio.a:		$(OBJS)
			/bin/rm -f libwrfio_pio.a
			$(AR) cr libwrfio_pio.a $(OBJS)
			$(RANLIB) libwrfio_pio.a

wrf_data_pio.o:         wrf_data_pio.F90
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share wrf_data_pio.F90 > wrf_data_pio.f
			$(FC) $(FFLAGS) -c wrf_data_pio.f

pio_routines.o:		pio_routines.F90 wrf_data_pio.o ../../frame/module_domain.o ../esmf_time_f90/module_utility.o
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share pio_routines.F90 > pio_routines.f
			$(FC) $(FFLAGS) -c pio_routines.f

wrf_io.o:               wrf_io.F90 wrf_data_pio.o read_bdy_routines.o field_routines.o pio_routines.o ../../frame/module_domain.o
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share wrf_io.F90 > wrf_io.f
			$(FC) $(FFLAGS) -c wrf_io.f

module_wrfsi_static.o:  module_wrfsi_static.F90
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share module_wrfsi_static.F90 > module_wrfsi_static.f
			$(FC) $(FFLAGS) -c module_wrfsi_static.f

field_routines.o:	field_routines.F90 wrf_data_pio.o pio_routines.o
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

read_bdy_routines.o:	read_bdy_routines.F90 wrf_data_pio.o pio_routines.o
			$(CPP1) -I. -I$(NETCDFPATH)/include -I../ioapi_share read_bdy_routines.F90 > read_bdy_routines.f
			$(FC) $(FFLAGS) -c read_bdy_routines.f

clean:	superclean

superclean:
			/bin/rm -f *.f *.o \
			*.mod libwrfio_pio.a
# DEPENDENCIES :

#pio_routines.o: ../../frame/module_domain.o ../esmf_time_f90/module_utility.o
#wrf_io.o: ../../frame/module_domain.o

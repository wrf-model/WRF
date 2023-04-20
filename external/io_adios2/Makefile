#makefile to build a wrf_io with ADIOS2

OBJSL   = wrf_io.o field_routines.o
OBJS    = $(OBJSL)
CODE    = ext_adios2_get_dom_ti.code \
	  ext_adios2_get_var_td.code \
	  ext_adios2_get_var_ti.code \
	  ext_adios2_put_dom_ti.code \
	  ext_adios2_put_var_td.code \
	  ext_adios2_put_var_ti.code \
	  transpose.code 

FFLAGS  = $(FCFLAGS) -I$(ADIOS2)/include/adios2/fortran -I../ioapi_share
CPP1    = $(CPP) -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o .code

all : libwrfio_adios2.a 

libwrfio_adios2.a:		$(OBJS) $(CODE)
			/bin/rm -f libwrfio_adios2.a
			$(AR) cr libwrfio_adios2.a $(OBJSL)
			$(RANLIB) libwrfio_adios2.a

wrf_io.o:               wrf_io.F90 $(CODE)
			$(CPP1) -I$(ADIOS2)/include/adios2/fortran  -I../ioapi_share wrf_io.F90 | $(M4) - > wrf_io.f
			$(FC) $(FFLAGS)  -c wrf_io.f

field_routines.o:	field_routines.F90 wrf_io.o
			$(CPP1)  -I$(ADIOS2)/include/adios2/fortran -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

superclean:
			@/bin/rm -f *.f *.o *.mod *.smod libwrfio_adios2.a

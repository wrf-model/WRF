#makefile to build a wrf_io with netCDF

OBJSL   = wrf_io.o field_routines.o
OBJS    = $(OBJSL)
OPTS    = 
CODE    = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code 
FFLAGS  =  $(OPTS) -I$(NETCDFPATH)/include
LIBS    = -L$(NETCDFPATH)/lib -lnetcdf
CPP1    = $(CPP) -C -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o .code

all : libwrfio_nf.a 

libwrfio_nf.a:		$(OBJS) $(CODE)
			/bin/rm -f libwrfio_nf.a
			$(AR) cr libwrfio_nf.a $(OBJSL)

wrf_io.o:               wrf_io.F90 $(CODE)
			sed -e '1,/BEGINIOFLAGS/d' -e '/ENDIOFLAGS/,$$d' wrf_io.F90 > wrf_io_flags.h
			$(CPP1) wrf_io.F90 | $(M4) - > wrf_io.f
			$(FC) $(FFLAGS) -c wrf_io.f

diffwrf:                diffwrf.F90
			$(CPP1) diffwrf.F90 > diffwrf.f
			$(FC) -c $(FFLAGS) diffwrf.f
			@if [ -f ../../frame/module_wrf_error.o ] ; then \
			echo "diffwrf is being built now. "; \
			  $(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) $(LIBS) ../../frame/module_wrf_error.o ;\
                        else \
			  echo "***************************************************************************** " ; \
			  echo "*** Rerun compile to make diffwrf in external/io_netcdf directory         *** " ; \
			  echo "***************************************************************************** " ; \
                        fi

field_routines.o:	field_routines.F90
			$(CPP1) field_routines.F90 > field_routines.f
			$(FC) $(FFLAGS) -c field_routines.f

superclean:
			/bin/rm -f *.f *.o testWRFWrite testWRFRead \
			*.mod libwrfio_nf.a diffwrf wrf_io_flags.h

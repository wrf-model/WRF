#makefile to build a wrf_io with netCDF

OBJSL   = wrf_io.o field_routines.o module_wrfsi_static.o
OBJS    = $(OBJSL)
CODE    = ext_ncdpar_get_dom_ti.code ext_ncdpar_get_var_td.code ext_ncdpar_get_var_ti.code ext_ncdpar_put_dom_ti.code ext_ncdpar_put_var_td.code ext_ncdpar_put_var_ti.code transpose.code 
FFLAGS  =  $(FCFLAGS) -I$(NETCDFPARPATH)/include -I../ioapi_share
LIBS    = $(LIB_LOCAL) -L$(NETCDFPARPATH)/lib -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lm -lz -lcurl
LIBFFS  = $(LIB_LOCAL) -L$(NETCDFPARPATH)/lib -lnetcdff -lnetcdf $(NETCDF4_DEP_LIB)
CPP1    = $(CPP) -P $(TRADFLAG)  -D_NETCDFPAR -D_NETCDF4
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .f .o .code

all : libwrfio_nfpar.a 

libwrfio_nfpar.a:         $(OBJS) $(CODE)
			/bin/rm -f $@
			if [ "$(AR)" != "lib.exe" ] ; then \
                          $(AR) cr libwrfio_nfpar.a $(OBJSL) ; \
                        else \
                          $(AR) /out:libwrfio_nfpar.a $(OBJSL) ; \
                        fi
			$(RANLIB) $@

wrf_io.o:               wrf_io.F90 $(CODE)
			grep nf_format_64bit $(NETCDFPARPATH)/include/netcdf.inc ;\
                        a=$$? ; export a ; \
			if [ $$a -a "$$WRFIO_NCD_LARGE_FILE_SUPPORT" = "1" ] ; then \
                          $(CPP1) -DWRFIO_NCD_LARGE_FILE_SUPPORT -I../ioapi_share wrf_io.F90 | $(M4) - > wrf_io.f ; \
                        else \
                          $(CPP1) -I../ioapi_share wrf_io.F90 | $(M4) - > wrf_io.f ; \
                        fi
			$(FC) -o $@ $(FFLAGS) -c wrf_io.f


module_wrfsi_static.o:  module_wrfsi_static.F90
			$(CPP1) -I../ioapi_share module_wrfsi_static.F90 > module_wrfsi_static.f
			$(FC) -o $@ $(FFLAGS) -c module_wrfsi_static.f

# gfortran treats iargc as intrinsic, so get rid of external declaration in that case
diffwrf:                diffwrf.F90
	x=`echo "$(FC)" | awk '{print $$1}'` ; export x ; \
	if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
	   $(CPP1) -I$(NETCDFPARPATH)/include -I../ioapi_share diffwrf.F90 | sed '/integer *, *external.*iargc/d' > diffwrf.f ;\
        else \
	   $(CPP1) -I$(NETCDFPARPATH)/include -I../ioapi_share diffwrf.F90 > diffwrf.f ; \
        fi
	$(FC) -c $(FFLAGS) diffwrf.f
	@if [ \( -f ../../frame/wrf_debug.o \) -a \( -f ../../frame/module_wrf_error.o \) -a \( -f $(ESMF_MOD_DEPENDENCE) \) -a \( -f ../../frame/clog.o \) ] ; then \
	  echo "diffwrf io_netcdfpar is being built now. " ; \
          if [ \( -f $(NETCDFPARPATH)/lib/libnetcdff.a -o -f $(NETCDFPARPATH)/lib/libnetcdff.so \) ] ; then \
            $(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) ../../frame/wrf_debug.o ../../frame/module_wrf_error.o ../../frame/clog.o $(ESMF_IO_LIB_EXT) $(LIBFFS) ;\
          else \
            $(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) ../../frame/wrf_debug.o ../../frame/module_wrf_error.o ../../frame/clog.o $(ESMF_IO_LIB_EXT) $(LIBS) ;\
          fi ; \
        else \
	   echo "***************************************************************************** " ; \
	   echo "*** Rerun compile to make diffwrf in external/io_netcdf directory         *** " ; \
	   echo "***************************************************************************** " ; \
        fi

field_routines.o:	field_routines.F90 wrf_io.o
			$(CPP1) -I../ioapi_share field_routines.F90 > field_routines.f
			$(FC) -o $@ $(FFLAGS) -c field_routines.f

superclean:
			/bin/rm -f *.f *.o *.obj *.i testWRFWrite testWRFRead \
			*.mod libwrfio_nfpar.a diffwrf

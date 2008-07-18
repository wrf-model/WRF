#makefile to build io_int that does binary i/o

OBJSL   = io_int.o 
OBJS    = $(OBJSL)
FFLAGS  = $(FCFLAGS)
LIBS    = 
CPP1    = $(CPP) $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar

.SUFFIXES:      .F90 .F .f .o .code

all : libwrfio_int.a diffwrf

libwrfio_int.a:		$(OBJS)
			/bin/rm -f libwrfio_int.a
			$(AR) cr libwrfio_int.a $(OBJSL)
			$(RANLIB) libwrfio_int.a

io_int.o:               io_int.F90 module_internal_header_util.o
			$(CPP1)  -I../ioapi_share io_int.F90 | $(M4) - > io_int.f
			$(FC) $(FFLAGS) -I. -I../ioapi_share -o $@ -c io_int.f
			/bin/rm -f intio_tags.h

# this is just so we can get the .mod file, it will be recompiled in frame again as part of WRF frmwk
module_internal_header_util.o :
			cp ../../frame/module_internal_header_util.F module_internal_header_util.b
			cp ../../inc/intio_tags.h intio_tags.h
			/bin/rm -f module_internal_header_util.f
			$(CPP1)  -I../ioapi_share module_internal_header_util.b > module_internal_header_util.f
			$(FC) $(FFLAGS) -I. -o $@ -c module_internal_header_util.f
			/bin/rm -f module_internal_header_util.b

diffwrf:                diffwrf.F ../../frame/pack_utils.o ../../frame/module_machine.o ../../frame/module_wrf_error.o \
				../../frame/wrf_debug.o libwrfio_int.a $(ESMF_MOD_DEPENDENCE)
			if [ -f ../../frame/pack_utils.o ] ; then \
			mv diffwrf.F diffwrf.F90    ; \
                        x=`echo "$(FC)" | awk '{print $$1}'` ; export x ; \
                        if [ $$x = "gfortran" ] ; then \
                          echo removing external declaration of iargc for gfortran ; \
			  $(CPP1)  -I../ioapi_share diffwrf.F90 | sed '/integer *, *external.*iargc/d' > diffwrf.f  ; \
                        else \
			  $(CPP1)  -I../ioapi_share diffwrf.F90 > diffwrf.f  ; \
                        fi ; \
			$(FC) -c $(FFLAGS) -I../ioapi_share diffwrf.f    ; \
			mv diffwrf.F90 diffwrf.F ; \
			$(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) \
                        ../../frame/pack_utils.o ../../frame/module_internal_header_util.o \
                        ../../frame/module_driver_constants.o \
			../../frame/module_machine.o ../../frame/wrf_debug.o ../../frame/module_wrf_error.o \
			$(ESMF_IO_LIB_EXT) ; fi

../../frame/pack_utils.o ../../frame/internal_header_util.o ../../frame/module_machine.o ../../frame/module_wrf_error.o \
../../frame/wrf_debug.o $(ESMF_MOD_DEPENDENCE) :
			@echo "Diffwrf io_int will be built later on in this compile. No need to rerun compile. "

superclean:
			/bin/rm -f *.f *.o *.obj *.i *.mod libwrfio_int.a diffwrf 

#makefile to build io_int that does binary i/o

OBJSL   = io_int.o 
OBJS    = $(OBJSL)
OPTS    =
FFLAGS  =  $(OPTS)
LIBS    = 
CPP1    = $(CPP) $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
RANLIB  = echo

.SUFFIXES:      .F90 .F .f .o .code

all : libwrfio_int.a diffwrf

libwrfio_int.a:		$(OBJS)
			/bin/rm -f libwrfio_int.a
			$(AR) cr libwrfio_int.a $(OBJSL)
			$(RANLIB) libwrfio_int.a

io_int.o:               io_int.F90
			$(CPP1) -I.. io_int.F90 | $(M4) - > io_int.f
			$(FC) $(FFLAGS) -I.. -I../../inc -c io_int.f

diffwrf:                diffwrf.F ../../frame/pack_utils.o ../../frame/internal_header_util.o ../../frame/module_machine.o libwrfio_int.a
			if [ -f ../../frame/pack_utils.o ] ; then \
			$(CPP1) diffwrf.F > diffwrf.f     ; \
			$(FC) -c $(FFLAGS) diffwrf.f     ; \
			$(FC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) \
                              ../../frame/pack_utils.o ../../frame/internal_header_util.o ../../frame/module_machine.o ; fi

../../frame/pack_utils.o ../../frame/internal_header_util.o ../../frame/module_machine.o :
			@echo "Diffwrf will be built later on in this compile. No need to rerun compile. "

superclean:
			/bin/rm -f *.f *.o *.mod libwrfio_int.a diffwrf 

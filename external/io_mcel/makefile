#makefile to build io_mcel that does binary i/o

OBJSL   = io_mcel.o 
OBJS    = $(OBJSL)
OPTS    =
FFLAGS  =  $(OPTS) -w -g
LIBS    = 
CPP     = /lib/cpp -C -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
RANLIB  = echo

.SUFFIXES:      .F90 .F .f .o .code

all : libwrfio_mcel.a

libwrfio_mcel.a:		$(OBJS)
			/bin/rm -f libwrfio_mcel.a
			$(AR) cr libwrfio_mcel.a $(OBJSL)
			$(RANLIB) libwrfio_mcel.a

io_mcel.o:               io_mcel.F90
			$(CPP) io_mcel.F90 | $(M4) - > io_mcel.f
			$(FC) $(FFLAGS) -I../../inc -c io_mcel.f

superclean:
			/bin/rm -f *.f *.o libwrfio_mcel.a


#makefile to build io_mcel that does binary i/o

OBJSL   = io_mcel.o ext_mcel_open_for_read.o ext_mcel_open_for_write.o ext_mcel_read_field.o ext_mcel_write_field.o timeconvert.o
OBJS    = $(OBJSL)
OPTS    =
FFLAGS  =  $(OPTS) -w -g
LIBS    = 
CPP     = /lib/cpp -P $(TRADFLAG)
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
RANLIB  = echo

.SUFFIXES:      .F90 .F .f .o .code

all : libwrfio_mcel.a

libwrfio_mcel.a:		$(OBJS)
			/bin/rm -f libwrfio_mcel.a
			$(AR) cr libwrfio_mcel.a $(OBJSL)
			$(RANLIB) libwrfio_mcel.a

timeconvert.o : timeconvert.c
			$(CC) -c timeconvert.c

.F90.o:
			$(CPP) $(TRADFLAG) $(INCL) $*.F90 | $(M4) - > $*.f
			$(FC) $(FFLAGS) $(INCL) -I../ioapi_share -c $*.f

superclean:
			/bin/rm -f *.f *.o libwrfio_mcel.a

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

ext_mcel_open_for_read.o : io_mcel.o
ext_mcel_open_for_write.o : io_mcel.o
ext_mcel_read_field.o : io_mcel.o
ext_mcel_write_field.o : io_mcel.o


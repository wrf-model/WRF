OBJSL   = c_code.o buf_for_proc.o rsl_malloc.o rsl_bcast.o task_for_point.o period.o swap.o cycle.o f_pack.o f_xpose.o
OBJS    = $(OBJSL)
OPTS    = $(PLUSFLAG)
FFLAGS  =  $(OPTS)
LIBS    = 
M4      = m4 -Uinclude -Uindex -Ulen
AR      = ar
CFLAGS = $(PLUSFLAG)
CPPFLAGS =
FFLAGS =


.SUFFIXES:      .F90 .F .f .o .code

all : librsl_lite.a

librsl_lite.a:          $(OBJS)
			/bin/rm -f librsl_lite.a
			$(AR) cr librsl_lite.a $(OBJSL)

c_code.o:	        c_code.c
			$(CC) $(CFLAGS) -c c_code.c

period.o:	        period.c
			$(CC) $(CFLAGS) -c period.c

swap.o:	                swap.c
			$(CC) $(CFLAGS) -c swap.c

cycle.o:	        cycle.c
			$(CC) $(CFLAGS) -c cycle.c

rsl_bcast.o:	        rsl_bcast.c
			$(CC) $(CFLAGS) -c rsl_bcast.c

rsl_malloc.o:	        rsl_malloc.c
			$(CC) $(CFLAGS) -c rsl_malloc.c

task_for_point.o:	task_for_point.c
			$(CC) $(CFLAGS) -c task_for_point.c

buf_for_proc.o:	        buf_for_proc.c
			$(CC) $(CFLAGS) -c buf_for_proc.c

f_pack.o:	        f_pack.F90
			$(CPP) $(CPPFLAGS) f_pack.F90 > f_pack.f
			$(FC) -o $@ $(FFLAGS) -c f_pack.f

f_xpose.o:	        f_xpose.F90 f_pack.o
			$(CPP) f_xpose.F90 > f_xpose.f
			$(FC) -o $@ $(FFLAGS) -c f_xpose.f

clean : 
			/bin/rm -f *.f *.o *.mod *.obj *.i

superclean : clean
			/bin/rm -f *.a

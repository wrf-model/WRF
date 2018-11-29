SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.c .o .F
OBJS=	rdmet.o \
	rdsom_mem.o \
	rdbz.o \
	kwm_date_utilities.o \
	kwm_plot_utilities.o \
	kwm_string_utilities.o \
	module_hd.o \
	lccone.o llxy_generic.o get_unused_unit.o
#
#
BZDIR=	/home/kmanning/bzip2-1.0.2
CC=cc

# FC=ifort
# FFLAGS  = -free -convert big_endian -g -check bounds

FC=pgf90
FFLAGS  = -Mfree -byteswapio

CCFLAGS = -I. -I$(BZDIR)
LDFLAGS=
FINCLUDES=	-I. -I$(NETCDF)/include
LIBS=	-L$(NETCDF)/lib -lnetcdf -L$(BZDIR) -lbz2 \
	-L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c \
	-L/usr/X11R6/lib -lX11 \
	-L/usr/lib/gcc/i386-redhat-linux/3.4.6 -lg2c 

RM = 	rm -f
CMD=	okmeso_statistics.exe

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build $(CMD).
#
all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) -o $(CMD) $(OBJS) $(LIBS)

.F.o:
	$(FC) -c $(FINCLUDES) $(FFLAGS) $(<)

.c.o:
	$(CC) -c $(CCFLAGS) $(<)

clean:
	$(RM) $(CMD) *.o *~ *.mod
#
kwm_plot_utilities.o:	kwm_string_utilities.o
rdsom_mem.o:		kwm_date_utilities.o
rdsom_mem.o:		kwm_plot_utilities.o
rdsom_mem.o:		module_hd.o

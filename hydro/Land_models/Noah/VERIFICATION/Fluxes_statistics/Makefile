.SUFFIXES:	.F .o
#
OBJS=	module_hd.o \
	fluxes_statistics.o \
	kwm_date_utilities.o \
	kwm_plot_utilities.o \
	kwm_grid_utilities.o \
	kwm_string_utilities.o \
	llxy_generic.o lccone.o get_unused_unit.o

CMD=	fluxes_statistics.exe


F90=	pgf90
FFLAGS=	-Mfree -byteswapio -C -g
NETCDF=/scholar/kmanning/netcdf-3.6.3-pgf90

# F90=	ifort
# FFLAGS=	-free -convert big_endian -check bounds -g
# NETCDF=/home/kmanning/netcdf-3.6.0-p1-ifort

INCLUDES= -I. -I$(NETCDF)/include
LIBS=	-L$(NETCDF)/lib -lnetcdf \
	-L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c \
	-L/usr/X11R6/lib -lX11 \
	-L/usr/lib/gcc/i386-redhat-linux/3.4.6 -lg2c 

all: $(CMD)

$(CMD): $(OBJS)
	$(F90) $(FFLAGS) -o $(CMD) $(INCLUDES) $(OBJS) $(LIBS)

.F.o:
	$(F90) $(FFLAGS) $(INCLUDES) -c $(<)

clean:
	rm -f $(OBJS) $(CMD) *~ *.mod

fluxes_statistics.o:	kwm_plot_utilities.o
fluxes_statistics.o:	kwm_date_utilities.o
fluxes_statistics.o:	kwm_grid_utilities.o
kwm_plot_utilities.o:	kwm_string_utilities.o

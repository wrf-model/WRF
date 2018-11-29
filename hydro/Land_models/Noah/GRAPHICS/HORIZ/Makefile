SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .o .exe
OBJS=	module_plot2d_graphics.o \
	module_2ddata.o \
	kwm_date_utilities.o \
	kwm_string_utilities.o \
	llxy_generic.o \
	lccone.o \
	plt2d.o


#F90=	ifort
#FFLAGS=-free

F90=	pgf90
FFLAGS=-Mfree 
LIBS2 = -L${PGI}/linux86/5.2/lib -lpgftnrtl -lpgc 

RM = 	rm -f
NCARGLIBS=	-L/usr/local/ncarg/lib -L/usr/X11R6/lib \
		-lncarg -lncarg_gks -lncarg_c -lX11 -lXext \
		-L/usr/lib/gcc/i386-redhat-linux/3.4.6 -lg2c
#	        -L/usr/lib/gcc-lib/i386-linux/3.0.4 -lg2c

CMD=	plt2d.exe


# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build $(CMD).
#

all:	$(CMD)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
	$(F90) -c -I$(NETCDF)/include $(FFLAGS) $(MODDIR) $(*).f 
	$(RM) $(*).f

$(CMD):	$(OBJS)
	$(F90) -o $(@) -I$(NETCDF)/include $(FFLAGS) $(OBJS) \
	$(LIBS2) -L$(NETCDF)/lib -lnetcdf $(NCARGLIBS) -L/scholar/kmanning/TESTN4/HDF5/lib -lhdf5_hl -lhdf5 -lz

clean:
	$(RM) *.o *~ *.exe *.mod
#
module_2ddata.o:	kwm_string_utilities.o
module_plot2d_graphics.o:	module_2ddata.o

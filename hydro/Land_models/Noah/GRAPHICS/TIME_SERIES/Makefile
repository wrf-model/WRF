SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .o .exe
OBJS=	module_plotts_graphics.o \
	module_ptdata.o \
	ldasout_timeseries.o \
	kwm_date_utilities.o \
	kwm_string_utilities.o \
	llxy_generic.o \
	lccone.o


F90=	ifort -warn unused
FFLAGS=-free

# F90=	pgf90
# FFLAGS=-Mfree

RM = 	rm -f
NCARGLIBS=	-L/usr/local/ncarg/lib -L/usr/X11R6/lib \
		-lncarg -lncarg_gks -lncarg_c -lX11 -lXext \
	        -L/usr/lib/gcc/i386-redhat-linux/3.4.6 -lg2c

CMD=	pltts.exe


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
	-L$(NETCDF)/lib -lnetcdf $(NCARGLIBS) 

clean:
	$(RM) *.o *~ *.exe *.mod
#
module_ptdata.o:	kwm_string_utilities.o
ldasout_timeseries.o:	kwm_date_utilities.o
module_plotts_graphics.o:	kwm_string_utilities.o

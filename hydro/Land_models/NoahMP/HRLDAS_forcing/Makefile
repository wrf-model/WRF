 SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .c .o .exe

include ../user_build_options

OBJS=	create_forcing.o

CMD=	create_forcing.exe

default:
	(cd lib; make)
	(make -f Makefile all)

all:	lib/libsmda.a $(CMD)
	(cd lib; make)

lib/libsmda.a:
	(cd lib; make)

.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPP_NETCDF4_COMPRESS) $(*).F > $(*).f90
	$(COMPILERF90) $(FREESOURCE) $(F90FLAGS) $(LDFLAGS) -c $(NETCDFMOD) -I./lib -I./lib $(*).f90

$(CMD):	lib/libsmda.a $(OBJS)
	(cd lib; make)
	$(COMPILERF90) -o $(@) -I./lib $(F90FLAGS) $(LDFLAGS) -I./lib $(OBJS) \
		-L./lib -lsmda $(NETCDFLIB) $(BZIP2_LIB) $(LIBJASPER)

clean:
	$(RM) *.o *~ *.exe *.mod *.f90
	(cd lib; make clean)
#






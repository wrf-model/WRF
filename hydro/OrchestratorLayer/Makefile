# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../macros

OBJS = \
	orchestrator.o \
	config.o \
	io_manager.o

all:	$(OBJS)

#module_RT.o: module_RT.F
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f
#	$(RMD) $(*).f
#	@echo ""
#	cp *.mod ../mod

.f90.o:
	@echo "Orchestrator Makefile:"
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(COMPILER90) $(CPPINVOKE) -o $(@) $(CPPFLAGS) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $(*).f90
#	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#
io_manager.o: ../IO/netcdf_layer.o

orchestrator.o: io_manager.o config.o

clean:	
	rm -f *.o *.mod *.stb *~ *.f

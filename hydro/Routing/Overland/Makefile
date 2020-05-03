#simple compilation test for modules in overland routing

include ../../macros

# Settings for testing with ifort
FC=ifort
FFLAGS=-c -free -O3

# Setting for testing with gfortran
#FC=gfortran
#FFLAGS=-c --free-form -std=f2003 -O3

FLFLAGS=

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules	
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAGS) -I$(NETCDFINC) module_overland_control.F
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAGS) -I$(NETCDFINC) module_overland_streams_and_lakes.F
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAGS) -I$(NETCDFINC) module_overland_routing_properties.F
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAGS) -I$(NETCDFINC) module_overland_mass_balance.F
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAGS) -I$(NETCDFINC) module_overland.F
	ar -r ../../lib/libHYDRO.a module_overland_control.o
	ar -r ../../lib/libHYDRO.a module_overland_streams_and_lakes.o
	ar -r ../../lib/libHYDRO.a module_overland_routing_properties.o
	ar -r ../../lib/libHYDRO.a module_overland_mass_balance.o
	ar -r ../../lib/libHYDRO.a module_overland.o

	cp *.mod ../../mod
test:
	$(COMPILER90) $(FFLAGS) overland_tests.F
	$(COMPILER90) -o overland_tests \
		module_overland_control.o \
		module_overland_streams_and_lakes.o \
		module_overland_routing_properties.o \
		module_overland_mass_balance.o \
		module_overland.o \
		overland_tests.o
clean:
	rm -f *.o 
	rm -f *.mod
	rm -f overland_tests

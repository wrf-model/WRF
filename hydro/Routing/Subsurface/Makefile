#simple compilation test for modules in overland routing

include ../../macros

# Settings for testing with ifort
FC=ifort
FFLAGS=-c -free -O3

# Setting for testing with gfortran
#FC=gfortran
#FFLAGS=-c --free-form -std=f2003 -O3

FLFLAGS=

MODFLAG := $(MODFLAG) -I ../../mod

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_grid_transform.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_properties.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_state.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_static_data.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_output.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_subsurface_input.F90
	ar -r ../../lib/libHYDRO.a module_subsurface_grid_transform.o
	ar -r ../../lib/libHYDRO.a module_subsurface_properties.o
	ar -r ../../lib/libHYDRO.a module_subsurface_state.o
	ar -r ../../lib/libHYDRO.a module_subsurface.o
	ar -r ../../lib/libHYDRO.a module_subsurface_static_data.o
	ar -r ../../lib/libHYDRO.a module_subsurface_output.o
	ar -r ../../lib/libHYDRO.a module_subsurface_input.o

	cp *.mod ../../mod
test:
	$(COMPILER90) $(F90FLAGS) $(MODFLAG) subsurface_tests.F90
	$(COMPILER90) -o subsurface_tests \
		module_subsurface_grid_transform.o \
		module_subsurface_properties.o \
		module_subsurface_state.o \
		module_subsurface.o \
		../Overland/module_overland.o \
		../Overland/module_overland_control.o \
		../Overland/module_overland_routing_properties.o \
		../Overland/module_overland_mass_balance.o \
		../Overland/module_overland_streams_and_lakes.o \
		../Overland/module_subsurface_static_data.o \
		../Overland/module_subsurface_output.o \
		../Overland/module_subsurface_input.o \
		subsurface_tests.o
clean:
	rm -f *.o
	rm -f *.mod
	rm -f subsurface_tests

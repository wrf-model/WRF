
include ../../../macros

MODFLAG := -I ../../../MPP -I ../../../mod

%.o : %.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_properties.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_state.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_tests.F90
	ar -r ../../../lib/libHYDRO.a module_levelpool_properties.o
	ar -r ../../../lib/libHYDRO.a module_levelpool_state.o
	ar -r ../../../lib/libHYDRO.a module_levelpool.o
	ar -r ../../../lib/libHYDRO.a module_levelpool_tests.o

	cp *.mod ../../../mod

clean:
	rm -f *.o
	rm -f *.mod

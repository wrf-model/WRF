
include ../../../macros

MODFLAG := -I ../../../MPP -I ../../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_persistence_levelpool_hybrid_properties.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_persistence_levelpool_hybrid_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_persistence_levelpool_hybrid.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_persistence_levelpool_hybrid_tests.F
	ar -r ../../../lib/libHYDRO.a module_persistence_levelpool_hybrid_properties.o
	ar -r ../../../lib/libHYDRO.a module_persistence_levelpool_hybrid_state.o
	ar -r ../../../lib/libHYDRO.a module_persistence_levelpool_hybrid.o
	ar -r ../../../lib/libHYDRO.a module_persistence_levelpool_hybrid_tests.o

	cp *.mod ../../../mod

clean:
	rm -f *.o
	rm -f *.mod

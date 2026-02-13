# Makefile
#
include ../../macros

OBJS = \
	module_diversions_timeslice.o \
	module_diversions.o

all:	$(OBJS)

%.o: %.F90
	@echo "Routing Diversions Makefile:"
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(LDFLAGS) -I"../../mod" -I$(NETCDFINC) $(*).F90  
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)
	cp *.mod ../../mod


fortglob_dir:
	make -C ../../utils/fortglob

module_diversions_timeslice.o: fortglob_dir

module_diversions.o: module_diversions_timeslice.o

clean:
	rm -f *.o *.mod *.stb *~

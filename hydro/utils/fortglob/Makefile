# Makefile

.PHONY: cp *.F90 *.c

include ../../macros

OBJS = \
	fortglob.o \
	libfortglob.o

all:	$(OBJS) cp

cp:
	cp *.mod ../../mod

%.o: %.F90
	@echo "Utils fortglob Makefile: %.F90"
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(LDFLAGS) -I../../ -I$(NETCDFINC) $(*).F90
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)
%.o: %.c
	@echo "Utils fortglob Makefile: %.c"
	$(COMPILERCC) -o $(@) -c $(*).c
	ar -r ../../lib/libHYDRO.a $(@)
	@echo ""
#
# Dependencies:
#
fortglob.o: libfortglob.o


clean:
	rm -f *.o *.mod *.stb *~

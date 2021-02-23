# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	debug_dump_variable.o 

all:	$(OBJS) 

.F.o:
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I../mod $(*).F
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~

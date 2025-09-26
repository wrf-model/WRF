# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_namelist.o \
	module_RT_data.o \
	module_gw_gw2d_data.o

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

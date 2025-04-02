# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../macros

OBJS = \
	module_namelist_inc.o \
	module_namelist.o \
	module_rt_inc.o	\
	module_RT_data.o \
	module_gw_gw2d_data.o

all:	$(OBJS)

.F90.o:
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I../mod $(*).F90
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

# Dependencies:
#
module_namelist.o: module_namelist_inc.o
module_RT_data.o: module_rt_inc.o

clean:
	rm -f *.o *.mod *.stb *~

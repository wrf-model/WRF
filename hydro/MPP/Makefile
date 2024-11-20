# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../macros

OBJS =  hashtable.o CPL_WRF.o mpp_land.o module_mpp_ReachLS.o module_mpp_GWBUCKET.o

all:	$(OBJS)

hashtable.o: hashtable.F90
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F90
	cp hashtable.mod ../mod
	ar -r ../lib/libHYDRO.a $(@)

mpp_land.o: mpp_land.F90
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F90
	ar -r ../lib/libHYDRO.a $(@)

CPL_WRF.o: CPL_WRF.F90
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) $(*).F90

	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F90
	ar -r ../lib/libHYDRO.a $(@)

module_mpp_ReachLS.o: module_mpp_ReachLS.F90
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F90
	ar -r ../lib/libHYDRO.a $(@)

module_mpp_GWBUCKET.o: module_mpp_GWBUCKET.F90
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F90
	ar -r ../lib/libHYDRO.a $(@)

clean:
	$(RMD) *.o *.mod *.stb *~

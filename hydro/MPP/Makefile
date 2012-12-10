# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS =  CPL_WRF.o mpp_land.o 

all:	$(OBJS)
mpp_land.o: mpp_land.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)

CPL_WRF.o: CPL_WRF.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~ *.f
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
        
	$(COMPILER90) $(F90FLAGS)  -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)
clean:
	$(RMD) *.o *.mod *.stb *~

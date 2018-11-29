# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
	module_sf_noahmp_groundwater.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

.F.o:
	@echo ""
	$(RM) $(*).f90
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f90
	$(COMPILERF90) -o $(@) -c -I../Utility_routines $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $(*).f90
	@echo ""

#
# Dependencies:
#
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~ *.f90


# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_sf_noahlsm.o \
	module_sf_urban.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f
	$(COMPILERF90) -o $(@) -c $(MODFLAG). $(MODFLAG)../Utility_routines $(F90FLAGS) $(FREESOURCE) $(*).f
	$(RM) $(*).f
	@echo ""

#
# Dependencies:
#
module_sf_noahlsm.o:	module_sf_urban.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~


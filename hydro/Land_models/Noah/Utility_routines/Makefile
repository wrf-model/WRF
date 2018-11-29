# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_date_utilities.o \
	module_model_constants.o \
	module_Noahlsm_utility.o \
	module_sfcdif_wrf.o \
	kwm_string_utilities.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_


all:	$(OBJS)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(*).f
	$(RM) $(*).f
	@echo ""

#
# Dependencies:
#

module_Noahlsm.o:	module_Noahlsm_param_init.o

#
# This command cleans up object (etc.) files:
#

clean:
	$(RM) *.o *.mod *.stb *~


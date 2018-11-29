# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	Noah_hrldas_driver.o \
	module_hrldas_netcdf_io.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

Noah_hrldas_driver.o: Noah_hrldas_driver.F
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(CPPHRLDAS) $(*).F > $(*).f
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG). \
	$(MODFLAG)../Noah $(MODFLAG)../Utility_routines $(NETCDFMOD) $(*).f
	$(RM) $(*).f
	@echo ""

module_hrldas_netcdf_io.o: module_hrldas_netcdf_io.F
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(NETCDFMOD) $(CPPHRLDAS) $(*).F > $(*).f
	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG)../Utility_routines $(NETCDFMOD)  $(*).f
	$(RM) $(*).f
	@echo ""

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

Noah_hrldas_driver.o:	module_hrldas_netcdf_io.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.stb *~

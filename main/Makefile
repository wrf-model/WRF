#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES = 

OBJS    =

LIBPATHS = 

include ../configure.wrf

$(SOLVER)_wrf : wrf.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o libwrflib.a $(LIB)

$(SOLVER)_ideal : module_initialize ideal.o
	$(RANLIB) libwrflib.a
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

$(SOLVER)_real : module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(RANLIB) libwrflib.a
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

convert_nmm : convert_nmm.o
	$(RANLIB) libwrflib.a
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o libwrflib.a $(LIB)

module_initialize :
	( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

## prevent real being compiled for OMP -- only for regtesting
#$(SOLVER)_real : module_initialize real_$(SOLVER).o
#	$(RANLIB) libwrflib.a
#	if [ -z "$(OMP)" ] ; then $(FC) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB) ; fi
#
## prevent module_initialize being compiled for OMP --remove after IBM debugging
#module_initialize :
#	if [ -z "$(OMP)" ] ; then ( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o ) ; fi
# end of regtest changes

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../external/esmf_time_f90/ESMF_Mod.o 

ideal.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_io_domain.o \
	../dyn_$(SOLVER)/$(CASE_MODULE)

ndown_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_real.o \
	../dyn_em/module_big_step_utilities_em.o \
	../external/esmf_time_f90/ESMF_Mod.o 

# this already built above :../dyn_em/module_initialize.real.o \
real_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../dyn_em/module_si_io_em.o \
	../dyn_em/module_big_step_utilities_em.o \
	../share/module_io_domain.o \
	../share/module_date_time.o \
	../share/module_optional_si_input.o \
	../share/module_bc_time_utilities.o \
	../external/esmf_time_f90/ESMF_Mod.o 
wrf.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_integrate.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_wrf_error.o \
	../frame/module_dm.o \
	../external/esmf_time_f90/ESMF_Mod.o 

# DO NOT DELETE

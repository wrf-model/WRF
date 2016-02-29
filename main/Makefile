#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =  module_wrf_top.F

OBJS    =

LIBPATHS = 

include ../configure.wrf

$(SOLVER)_wrf : wrf.o ../main/module_wrf_top.o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o ../main/module_wrf_top.o $(LIBWRFLIB) $(LIB)

$(SOLVER)_wrf_SST_ESMF : wrf_ESMFMod.o wrf_SST_ESMF.o ../main/module_wrf_top.o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o ../main/module_wrf_top.o $(LIBWRFLIB) $(LIB)

$(SOLVER)_ideal : module_initialize ideal_$(SOLVER).o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(LD) -o ideal.exe $(LDFLAGS) ideal_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)

$(SOLVER)_real : module_initialize ndown_$(SOLVER).o nup_$(SOLVER).o tc_$(SOLVER).o real_$(SOLVER).o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	#TEMPORARILY REMOVED $(LD) -o nup.exe $(LDFLAGS) nup_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	$(LD) -o tc.exe $(LDFLAGS) tc_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)

convert_em : convert_em.o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(LD) -o convert_em.exe $(LDFLAGS) convert_em.o $(LIBWRFLIB) $(LIB)

convert_nmm : convert_nmm.o
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(LIBWRFLIB) $(LIB)

real_nmm : real_nmm.o
	( cd ../dyn_nmm ;  $(MAKE) module_initialize_real.o )
	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o $(LIBWRFLIB) $(LIB)

module_initialize : ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o
#	( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

## prevent real being compiled for OMP -- only for regtesting
#$(SOLVER)_real : module_initialize real_$(SOLVER).o
#	$(RANLIB) $(RLFLAGS) $(LIBWRFLIB)
#	if [ -z "$(OMP)" ] ; then $(FC) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB) ; fi
#
## prevent module_initialize being compiled for OMP --remove after IBM debugging
#module_initialize :
#	if [ -z "$(OMP)" ] ; then ( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o ) ; fi
# end of regtest changes

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include depend.common

# DO NOT DELETE

# main

all_wrf : all_$(SOLVER) diffwrf

chem : convert_bioemiss convert_emiss

all_em : wrf em_real em_quarter_ss em_squall2d_x em_squall2d_y em_b_wave em_hill2d_x \
   em_grav2d_x diffwrf

all_nmm : wrf nmm_real

wrf : setup $(WRF_LIBS) libwrf.a wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)
	(cd ../main; $(LN) ../build/wrf.exe .)

$(SOLVER)_wrf_ESMFApp : setup $(WRF_LIBS) wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o $(WRF_LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o $(WRF_LIB)

$(SOLVER)_real : setup $(WRF_LIBS) module_initialize_real.o real_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o module_initialize_real.o $(WRF_LIB)
	(cd ../main; $(LN) ../build/real.exe .)

em_quarter_ss : setup $(WRF_LIBS) module_initialize_quarter_ss.o ideal.o
	$(LD) -o ideal_em_quarter_ss.exe $(LDFLAGS) ideal.o module_initialize_quarter_ss.o $(WRF_LIB)
	cp ideal_em_quarter_ss.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_quarter_ss.exe .)

em_squall2d_x : setup $(WRF_LIBS) module_initialize_squall2d_x.o ideal.o
	$(LD) -o ideal_em_squall2d_x.exe $(LDFLAGS) ideal.o module_initialize_squall2d_x.o $(WRF_LIB)
	cp ideal_em_squall2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_squall2d_x.exe .)

em_squall2d_y : setup $(WRF_LIBS) module_initialize_squall2d_y.o ideal.o
	$(LD) -o ideal_em_squall2d_y.exe $(LDFLAGS) ideal.o module_initialize_squall2d_y.o $(WRF_LIB)
	cp ideal_em_squall2d_y.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_squall2d_y.exe .)

em_b_wave : setup $(WRF_LIBS) module_initialize_b_wave.o ideal.o
	$(LD) -o ideal_em_b_wave.exe $(LDFLAGS) ideal.o module_initialize_b_wave.o $(WRF_LIB)
	cp ideal_em_b_wave.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_b_wave.exe .)

em_hill2d_x : setup $(WRF_LIBS) module_initialize_hill2d_x.o ideal.o
	$(LD) -o ideal_em_hill2d_x.exe $(LDFLAGS) ideal.o module_initialize_hill2d_x.o $(WRF_LIB)
	cp ideal_em_hill2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_hill2d_x.exe .)

em_grav2d_x : setup $(WRF_LIBS) module_initialize_grav2d_x.o ideal.o
	$(LD) -o ideal_em_grav2d_x.exe $(LDFLAGS) ideal.o module_initialize_grav2d_x.o $(WRF_LIB)
	cp ideal_em_grav2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/ideal_em_grav2d_x.exe .)

convert_bioemiss : setup $(WRF_LIBS) convert_bioemiss.o module_input_chem_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o \
        module_input_chem_bioemiss.o libwrf.a $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_bioemiss.exe .)

convert_emiss : setup $(WRF_LIBS) convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrf.a $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_emiss.exe .)

convert_nmm : $(WRF_LIBS) convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_nmm.exe .)

diffwrf : diffwrf_netcdf.exe diffwrf_int.exe

convert_nmm.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_comm_dm.o \
	module_bc.o \
	module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ideal.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_comm_dm.o \
	module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ndown_em.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_comm_dm.o \
	module_wrf_error.o \
	wrf_debug.o \
	module_integrate.o \
	module_bc.o \
	module_io_domain.o \
	module_get_file_names.o \
	module_soil_pre.o \
	module_initialize_real.o \
	module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

wrf.o: \
	module_machine.o \
	module_domain.o \
	module_integrate.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_wrf_error.o \
	wrf_debug.o \
	module_dm.o \
	module_comm_dm.o \
        module_wrf_top.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

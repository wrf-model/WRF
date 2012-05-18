#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =                       \
	module_bc.o	        \
	module_bc_time_utilities.o	\
	module_io_wrf.o	        \
	module_date_time.o	\
	module_get_file_names.o	\
	module_io_domain.o	\
	module_model_constants.o \
	module_MPP.o \
	module_optional_input.o	\
	module_compute_geop.o           \
	module_soil_pre.o		\
	module_check_a_mundo.o		\
        module_random.o \
	module_llxy.o

OBJS    = 		\
	dfi.o				\
	mediation_integrate.o		\
	mediation_interp_domain.o	\
	mediation_force_domain.o	\
	mediation_feedback_domain.o	\
        mediation_nest_move.o           \
	mediation_wrfmain.o		\
	solve_interface.o               \
        start_domain.o                  \
        init_modules.o                  \
        set_timekeeping.o               \
        interp_fcn.o sint.o             \
        input_wrf.o                     \
        output_wrf.o                    \
        wrf_timeseries.o                \
        wrf_ext_write_field.o           \
        wrf_ext_read_field.o            \
        wrf_bdyout.o			\
	wrf_fddaobs_in.o		\
        wrf_bdyin.o             	\
        wrf_tsin.o                      \
        landread.o			\
        track_driver.o                  \
        track_input.o                   \
        bobrand.o                       \
	setfeenv.o


NMM_MODULES = 


LIBTARGET    =  shared
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		if [ $(WRF_NMM_CORE) -eq 1 ] ; then \
                  $(MAKE) nmm_contrib                                      ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) $(NMM_OBJS) $(NMM_MODULES) ; \
                  else                                                         \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)             ; \
                fi

include ../configure.wrf

nmm_contrib : $(NMM_OBJS) $(NMM_MODULES)

#set_timekeeping_defs.inc : set_timekeeping_defs.m4
#	$(M4) set_timekeeping_defs.m4 > set_timekeeping_defs.inc

#set_timekeeping_alarms.inc : set_timekeeping_alarms.m4
#	$(M4) set_timekeeping_alarms.m4 > set_timekeeping_alarms.inc

#module_io_domain_defs.inc : module_io_domain_defs.m4
#	$(M4) module_io_domain_defs.m4 > module_io_domain_defs.inc

clean:
	@ echo 'use the clean script'


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_interface.o: solve_em.int ../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_timing.o ../frame/module_driver_constants.o \
		../frame/module_wrf_error.o

start_domain.o: start_domain_em.int wrf_timeseries.o track_driver.o  ../frame/module_domain.o ../frame/module_configure.o

module_bc.o: ../frame/module_configure.o ../frame/module_state_description.o \
		../frame/module_wrf_error.o module_model_constants.o

module_bc_time_utilities.o: $(ESMF_MOD_DEPENDENCE)

module_get_file_names.o: ../frame/module_dm.o

module_io_domain.o: module_io_wrf.o module_date_time.o ../frame/module_io.o  \
		../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_state_description.o

module_io_wrf.o: module_date_time.o \
                ../frame/module_wrf_error.o ../frame/module_streams.o \
		$(ESMF_MOD_DEPENDENCE)

output_wrf.o:   ../frame/module_io.o ../frame/module_wrf_error.o \
                ../frame/module_domain.o ../frame/module_state_description.o \
                ../frame/module_configure.o module_io_wrf.o  \
		$(ESMF_MOD_DEPENDENCE)

wrf_fddaobs_in.o: \
		module_date_time.o \
		module_llxy.o

wrf_timeseries.o: wrf_tsin.o \
		module_model_constants.o \
		module_llxy.o \
		../frame/module_domain.o \
		../frame/module_configure.o \
		../frame/module_dm.o

track_driver.o: track_input.o \
                module_model_constants.o \
                module_llxy.o \
                module_date_time.o \
                ../frame/module_domain.o \
                ../frame/module_configure.o \
                ../frame/module_state_description.o \
                ../frame/module_dm.o

input_wrf.o:    ../frame/module_io.o ../frame/module_wrf_error.o \
                ../frame/module_domain.o ../frame/module_state_description.o \
                ../frame/module_configure.o module_io_wrf.o  \
		$(ESMF_MOD_DEPENDENCE)

wrf_ext_write_field.o : ../frame/module_io.o ../frame/module_wrf_error.o \
                ../frame/module_domain.o ../frame/module_timing.o

wrf_ext_read_field.o : ../frame/module_io.o ../frame/module_wrf_error.o \
                ../frame/module_domain.o ../frame/module_timing.o

module_date_time.o: ../frame/module_wrf_error.o ../frame/module_configure.o \
		module_model_constants.o 

module_soil_pre.o: module_date_time.o ../frame/module_state_description.o

module_check_a_mundo.o: ../frame/module_configure.o ../frame/module_wrf_error.o \
		../frame/module_state_description.o

dfi.o : 	../frame/module_wrf_error.o ../frame/module_configure.o \
		../frame/module_state_description.o \
		../frame/module_domain.o ../frame/module_timing.o \
		module_model_constants.o module_date_time.o module_io_domain.o \
		$(ESMF_MOD_DEPENDENCE)

module_optional_input.o: module_io_wrf.o module_io_domain.o \
		../frame/module_domain.o ../frame/module_configure.o

mediation_wrfmain.o: ../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_timing.o $(ESMF_MOD_DEPENDENCE) \
		module_bc_time_utilities.o module_io_domain.o

init_modules.o: ../frame/module_configure.o ../frame/module_driver_constants.o \
		../frame/module_domain.o ../frame/module_machine.o \
		../frame/module_nesting.o ../frame/module_timing.o \
		../frame/module_tiles.o ../frame/module_io.o \
		../frame/module_io_quilt.o ../frame/module_dm.o \
		../external/io_int/io_int.o \
		 module_io_wrf.o module_bc.o module_model_constants.o  \
		../frame/module_wrf_error.o

interp_fcn.o: ../frame/module_timing.o ../frame/module_state_description.o ../frame/module_configure.o \
		../frame/module_wrf_error.o

mediation_feedback_domain.o: ../frame/module_domain.o ../frame/module_configure.o

mediation_force_domain.o: ../frame/module_domain.o ../frame/module_configure.o

mediation_integrate.o: ../frame/module_domain.o ../frame/module_configure.o \
			../frame/module_timing.o \
			$(ESMF_MOD_DEPENDENCE) \
			module_date_time.o module_bc_time_utilities.o \
			module_compute_geop.o                         \
			module_io_domain.o


mediation_interp_domain.o: ../frame/module_domain.o ../frame/module_configure.o \
			../frame/module_timing.o

mediation_nest_move.o: \
		../frame/module_domain.o \
		../frame/module_configure.o \
		../frame/module_state_description.o \
		../frame/module_driver_constants.o \
		module_io_domain.o

#mediation_conv_emissions.o: ../frame/module_domain.o ../frame/module_configure.o \
#			../external/esmf_time_f90/ESMF_Mod.o \
#			module_date_time.o module_bc_time_utilities.o \
#			module_io_domain.o

set_timekeeping.o: ../frame/module_domain.o ../frame/module_configure.o \
                   $(ESMF_MOD_DEPENDENCE)

wrf_inputout.o                  : ../frame/module_domain.o \
				../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o 
wrf_auxinput1out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput2out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput3out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput4out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput5out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput6out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput7out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput8out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput9out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput10out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput11out.o              : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_histout.o                   : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist1out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist2out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist3out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist4out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist5out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist6out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist7out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist8out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist9out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist10out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist11out.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_restartout.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_bdyout.o                    : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_inputin.o                   : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist1in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist2in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist3in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist4in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist5in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist6in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist7in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist8in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist9in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist10in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist11in.o                : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput1in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput2in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput3in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput4in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput5in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput6in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput7in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput8in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput9in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput10in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput11in.o               : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_bdyin.o                     : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_histin.o                    : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_restartin.o                 : ../frame/module_domain.o \
                                ../frame/module_configure.o ../frame/module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_tsin.o	                : ../frame/module_domain.o

track_input.o                   : ../frame/module_domain.o

module_random.o: bobrand.o

# DO NOT DELETE

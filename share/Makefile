#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES1=                               \
        module_model_constants.o        \
        module_bc_time_utilities.o      \
        module_get_file_names.o         \
        module_compute_geop.o           \
        module_check_a_mundo.o          \
        module_random.o                 \
        module_llxy.o                   \
        module_interp_nmm.o             \
        module_interp_store.o           \
        module_string_tools.o           \
        module_MPP.o 

MODULES2=                               \
        module_date_time.o              \
        module_bc.o                     \
        module_io_wrf.o                 \
        module_soil_pre.o 

MODULES3=                               \
        module_io_domain.o              

MODULES4=                               \
        module_optional_input.o         

MODULES = $(MODULES1) $(MODULES2) $(MODULES3) $(MODULES4)

OBJS4   = 		                \
        input_wrf.o                     \
        output_wrf.o                    \
        wrf_bdyout.o                    \
        wrf_bdyin.o                     \
	dfi.o				\
	$(PERTMOD) \
	mediation_integrate.o		\
        mediation_wrfmain.o 

OBJS1   =                               \
        solve_interface.o               \
	mediation_interp_domain.o	\
	mediation_force_domain.o	\
	mediation_feedback_domain.o

OBJS2  	=                               \
        start_domain.o                  \
        init_modules.o                  \
        set_timekeeping.o               \
        interp_fcn.o                    \
        sint.o                          \
        wrf_ext_write_field.o           \
        wrf_ext_read_field.o 

OBJS3   =                               \
        wrf_tsin.o                      \
        landread.o			\
        track_driver.o                  \
        track_input.o                   \
        module_trajectory.o             \
        bobrand.o                       \
        wrf_timeseries.o                \
        track_driver.o                  \
        wrf_fddaobs_in.o                \
        mediation_nest_move.o           \
	setfeenv.o

OBJS    = $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4)

NMM_MODULES = 


LIBTARGET    =  shared
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES1) $(OBJS1) $(MODULES2) $(OBJS2) $(MODULES3) $(OBJS3) $(MODULES4) $(OBJS4)
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

module_bc_em.o: ../share/module_bc.o \
                ../share/module_model_constants.o \
                ../frame/module_configure.o \
                ../frame/module_wrf_error.o

include ../main/depend.common

# DO NOT DELETE

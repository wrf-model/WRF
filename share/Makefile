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
	module_optional_si_input.o	\
	module_soil_pre.o

OBJS    = 		\
	mediation_integrate.o		\
	mediation_interp_domain.o	\
	mediation_force_domain.o	\
	mediation_feedback_domain.o	\
	mediation_wrfmain.o		\
	solve_interface.o               \
        start_domain.o                  \
        init_modules.o                  \
        set_timekeeping.o               \
        interp_fcn.o sint.o             \
        wrf_inputout.o        		\
        wrf_auxinput1out.o		\
        wrf_auxinput2out.o		\
        wrf_auxinput3out.o		\
        wrf_auxinput4out.o		\
        wrf_auxinput5out.o		\
        wrf_histout.o			\
        wrf_auxhist1out.o		\
        wrf_auxhist2out.o		\
        wrf_auxhist3out.o		\
        wrf_auxhist4out.o		\
        wrf_auxhist5out.o		\
        wrf_restartout.o		\
        wrf_bdyout.o			\
        wrf_inputin.o         	  	\
        wrf_auxhist1in.o                \
        wrf_auxhist2in.o                \
        wrf_auxhist3in.o                \
        wrf_auxhist4in.o                \
        wrf_auxhist5in.o                \
        wrf_auxinput1in.o               \
        wrf_auxinput2in.o               \
        wrf_auxinput3in.o               \
        wrf_auxinput4in.o               \
        wrf_auxinput5in.o               \
        wrf_bdyin.o             	\
        wrf_histin.o            	\
        wrf_restartin.o



NMM_MODULES =                   \
	module_MPP.o


LIBTARGET    =  shared
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		if [ $(WRF_NMM_CORE) -eq 1 ] ; then \
                  $(MAKE) nmm_contrib                                      ; \
                  $(AR) ../main/libwrflib.a $(MODULES) $(OBJS) $(NMM_OBJS) $(NMM_MODULES) ; \
                  else                                                         \
                  $(AR) ../main/libwrflib.a $(MODULES) $(OBJS)             ; \
                fi

include ../configure.wrf

nmm_contrib : $(NMM_OBJS) $(NMM_MODULES)

clean:
	@ echo 'use the clean script'


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_interface.o: solve_em.int ../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_timing.o ../frame/module_driver_constants.o \
		../frame/module_wrf_error.o

start_domain: start_domain_em.int ../frame/module_domain.o

module_bc.o: ../frame/module_configure.o ../frame/module_state_description.o \
		../frame/module_wrf_error.o

module_bc_time_utilities.o: ../external/esmf_time_f90/ESMF_Mod.o

module_get_file_names.o: 

module_io_domain.o: module_io_wrf.o module_date_time.o ../frame/module_io.o  \
		../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_state_description.o

module_io_wrf.o: module_date_time.o module_bc_time_utilities.o \
		../frame/module_wrf_error.o ../frame/module_domain.o \
		../frame/module_state_description.o ../frame/module_configure.o \
		../frame/module_io.o ../frame/module_timing.o \
		../external/esmf_time_f90/ESMF_Mod.o

module_date_time.o: ../frame/module_wrf_error.o

module_soil_pre.o: module_date_time.o

module_optional_si_input.o: module_io_wrf.o module_io_domain.o \
		../frame/module_domain.o ../frame/module_configure.o

mediation_wrfmain.o: ../frame/module_domain.o ../frame/module_configure.o \
		../frame/module_timing.o ../external/esmf_time_f90/ESMF_Mod.o \
		module_bc_time_utilities.o module_io_domain.o

init_modules.o: ../frame/module_configure.o ../frame/module_driver_constants.o \
		../frame/module_domain.o ../frame/module_machine.o \
		../frame/module_nesting.o ../frame/module_timing.o \
		../frame/module_tiles.o ../frame/module_io.o \
		../frame/module_io_quilt.o ../frame/module_dm.o \
		../external/io_int/io_int.o \
		 module_io_wrf.o module_bc.o module_model_constants.o 

interp_fcn.o: ../frame/module_timing.o ../frame/module_state_description.o \
		../frame/module_wrf_error.o

mediation_feedback_domain.o: ../frame/module_domain.o ../frame/module_configure.o

mediation_force_domain.o: ../frame/module_domain.o ../frame/module_configure.o

mediation_integrate.o: ../frame/module_domain.o ../frame/module_configure.o \
			../frame/module_timing.o \
			../external/esmf_time_f90/ESMF_Mod.o \
			module_date_time.o module_bc_time_utilities.o \
			module_io_domain.o

mediation_interp_domain.o: ../frame/module_domain.o ../frame/module_configure.o \
			../frame/module_timing.o

set_timekeeping.o: ../frame/module_domain.o ../frame/module_configure.o \
			../external/esmf_time_f90/ESMF_Mod.o \

# DO NOT DELETE

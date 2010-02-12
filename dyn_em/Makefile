#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_advect_em.o   		\
	module_diffusion_em.o  		\
	module_small_step_em.o 		\
        module_big_step_utilities_em.o  \
        module_em.o         		\
        module_solvedebug_em.o    	\
        module_bc_em.o                  \
        module_init_utilities.o         \
	module_damping_em.o		\
	module_polarfft.o		\
        module_force_scm.o              \
        module_first_rk_step_part1.o    \
        module_first_rk_step_part2.o    \
        module_avgflx_em.o              \
	module_sfs_nba.o		\
	module_sfs_driver.o		\
	$(CASE_MODULE)

# possible CASE_MODULE settings
#	module_initialize_b_wave.o      \
#	module_initialize_grav2d_x.o    \
#	module_initialize_heldsuarez.o  \
#	module_initialize_hill2d_x.o    \
#	module_initialize_quarter_ss.o  \
#	module_initialize_real.o        \
#	module_initialize_lsm_x.o       \
#	module_initialize_les.o         \
#	module_initialize_squall2d_x.o  \
#	module_initialize_squall2d_y.o 

OBJS    = 		        \
	init_modules_em.o       \
	solve_em.o              \
        start_em.o              \
        shift_domain_em.o       \
        couple_or_uncouple_em.o \
        nest_init_utils.o	\
        adapt_timestep_em.o     \
        interp_domain_em.o
 

LIBTARGET    =  dyn_em
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

include ../configure.wrf

cio.o :
	$(CC) -c $(CFLAGS) cio.c

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

couple_or_uncouple_em.o: ../frame/module_domain.o \
		../frame/module_configure.o \
		../frame/module_driver_constants.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_dm.o \
		../frame/module_state_description.o

shift_domain_em.o : \
		../frame/loop_based_x_shift_code.h \
		../frame/loop_based_y_shift_code.h

init_modules_em.o: module_big_step_utilities_em.o

interp_domain_em.o: ../frame/module_domain.o \
		../frame/module_configure.o 

module_advect_em.o: ../share/module_bc.o \
		../share/module_model_constants.o \
		../frame/module_wrf_error.o

module_bc_em.o: ../share/module_bc.o ../frame/module_configure.o \
		../frame/module_wrf_error.o

module_big_step_utilities_em.o: \
		../share/module_llxy.o	\
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o 

module_damping_em.o: ../frame/module_wrf_error.o

module_diffusion_em.o:  module_big_step_utilities_em.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../frame/module_wrf_error.o

module_em.o:    module_big_step_utilities_em.o module_advect_em.o \
		module_damping_em.o \
		../frame/module_state_description.o \
		../share/module_model_constants.o 

module_polarfft.o: ../share/module_model_constants.o \
		../frame/module_wrf_error.o

module_sfs_driver.o:  \
		module_sfs_nba.o \
		../frame/module_state_description.o \
		../frame/module_domain.o \
		../frame/module_configure.o \
		../frame/module_tiles.o \
		../frame/module_dm.o \
		../frame/module_machine.o \
		../share/module_bc.o

module_small_step_em.o: \
		../frame/module_configure.o  \
		../share/module_model_constants.o 

module_initialize_b_wave.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_force_scm.o: module_init_utilities.o

module_initialize_grav2d_x.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_heldsuarez.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_hill2d_x.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_fire.o : \
                ../frame/module_domain.o \
                ../frame/module_configure.o  \
                ../frame/module_state_description.o \
                ../frame/module_timing.o \
                ../frame/module_dm.o \
                ../share/module_io_domain.o \
                ../share/module_model_constants.o \
                ../share/module_bc.o \
                module_init_utilities.o

module_initialize_quarter_ss.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_les.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_seabreeze2d_x.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_scm_xy.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_real.o :  \
		nest_init_utils.o	\
		../share/module_llxy.o	\
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../share/module_soil_pre.o \
		../share/module_optional_input.o

module_initialize_squall2d_x.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_squall2d_y.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

nest_init_utils.o: \
		../frame/module_domain.o \
		../frame/module_configure.o

start_em.o:     module_bc_em.o \
                module_avgflx_em.o              \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../frame/module_comm_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../share/module_date_time.o \
		../phys/module_physics_init.o 

solve_em.o:     module_small_step_em.o \
		module_em.o            \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
                module_first_rk_step_part1.o \
                module_first_rk_step_part2.o \
                module_avgflx_em.o              \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_dm.o \
		../frame/module_comm_dm.o \
		../share/module_llxy.o	\
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_microphysics_driver.o \
		../phys/module_microphysics_zero_out.o \
		../phys/module_physics_addtendc.o

module_first_rk_step_part1.o : \
                module_force_scm.o \
		module_big_step_utilities_em.o \
                module_em.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_pbl_driver.o \
		../frame/module_comm_dm.o \
		../phys/module_fddagd_driver.o

module_first_rk_step_part2.o : \
                module_em.o \
		module_diffusion_em.o  \
                module_bc_em.o         \
		module_sfs_driver.o \
		../frame/module_domain.o \
		../frame/module_state_description.o \
		../frame/module_driver_constants.o \
		../frame/module_configure.o \
		../frame/module_dm.o \
		../share/module_bc.o \
		../frame/module_comm_dm.o \
		../phys/module_fddaobs_driver.o \
		../phys/module_fddaobs_driver.o

adapt_timestep_em.o: \
                module_bc_em.o         \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_dm.o

#		../chem/module_chem_utilities.o \
# 		../chem/module_input_chem_data.o

# DO NOT DELETE

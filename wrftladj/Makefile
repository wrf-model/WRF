

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

# 12 dyn_em pairs

DYNEM_MODS =                          \
        module_advect_em_tl.o           \
        module_advect_em_ad.o           \
        module_diffusion_em_tl.o        \
        module_diffusion_em_ad.o        \
        module_small_step_em_tl.o       \
        module_small_step_em_ad.o       \
        module_big_step_utilities_em_tl.o  \
        module_big_step_utilities_em_ad.o  \
        module_em_tl.o                  \
        module_em_ad.o                  \
        module_bc_em_tl.o               \
        module_bc_em_ad.o               \
        module_first_rk_step_part1_tl.o    \
        module_first_rk_step_part1_ad.o    \
        module_first_rk_step_part2_tl.o    \
        module_first_rk_step_part2_ad.o    \
        module_sfs_nba_tl.o             \
        module_sfs_nba_ad.o             \
        module_sfs_driver_tl.o          \
        module_sfs_driver_ad.o

DYNEM_OBJS =       \
        solve_em_tl.o           \
        solve_em_ad.o           \
        start_em_tl.o           \
        start_em_ad.o

# 11 phys pairs

PHYS_MODS = \
	module_bl_gwdo_tl.o \
	module_bl_gwdo_ad.o \
	module_bl_surface_drag_tl.o \
	module_bl_surface_drag_ad.o \
	module_cu_du_tl.o \
	module_cu_du_ad.o \
	module_mp_mkessler_tl.o \
	module_mp_mkessler_ad.o \
	module_mp_nconvp_tl.o \
	module_mp_nconvp_ad.o \
	module_physics_addtendc_tl.o \
	module_physics_addtendc_ad.o \
	module_physics_init_tl.o \
	module_physics_init_ad.o \
	module_pbl_driver_tl.o \
	module_pbl_driver_ad.o \
	module_cumulus_driver_tl.o \
	module_cumulus_driver_ad.o \
	module_microphysics_driver_tl.o \
	module_microphysics_driver_ad.o \
	module_microphysics_zero_out_tl.o \
	module_microphysics_zero_out_ad.o

SHARE_MODS = \
 	module_adtl_grid_utilities.o	\
 	module_bc_tl.o	        \
 	module_bc_ad.o

SHARE_OBJS = jcdfi.o
  
FRAME_OBJS = adStack.o adBuffer.o

TLMMODULES = $(FRAME_OBJS) $(SHARE_MODS) $(SHARE_OBJS) $(PHYS_MODS) $(DYNEM_OBJS) $(DYNEM_MODS)

LIBTARGET    =  wrftladj
TARGETDIR    =  ./
$(LIBTARGET) : $(TLMMODULES)
	$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(TLMMODULES)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
# DO NOT DELETE

include depend.wrftladj

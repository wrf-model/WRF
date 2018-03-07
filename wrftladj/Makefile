

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

# 12 dyn_em pairs

DYN_EM_ADTL =                          \
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
        module_sfs_driver_ad.o          \
        solve_em_tl.o           \
        solve_em_ad.o           \
        start_em_tl.o           \
        start_em_ad.o           \
        $(CASE_MODULE)

# 11 phys pairs

PHYS_ADTL = \
	module_bl_gwdo_tl.o \
	module_bl_gwdo_ad.o \
	module_bl_surface_drag.o \
	module_bl_surface_drag_tl.o \
	module_bl_surface_drag_ad.o \
	module_cu_du.o \
	module_cu_du_tl.o \
	module_cu_du_ad.o \
	module_mp_mkessler.o \
	module_mp_mkessler_tl.o \
	module_mp_mkessler_ad.o \
	module_mp_nconvp.o \
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

SHARE_ADTL = \
 	module_adtl_grid_utilities.o	\
 	mediation_pertmod_io.o	\
 	module_linked_lisk2.o	\
 	module_bc_tl.o	        \
 	module_bc_ad.o	        \
 	jcdfi.o
  
FRAME_ADTL = \
 	adStack.o     \
	adBuffer.o

adStack.o : adStack.c
	$(CC) -c $(CFLAGS) adStack.c

adBuffer.o : adBuffer.F
	$(RM) $@
	$(SED_FTN) $*.F > $*.b
	$(CPP) $(CPPFLAGS) -I../inc $*.b  > $*.for
	$(RM) $*.b
	$(RM) $@
	$(SFC) -c $(FORMAT_FIXED) $(PROMOTION) $(FCNOOPT) $*.for
	$(RM) adBuffer.for

OBJS    =

LIBTARGET    =  wrftladj
TARGETDIR    =  ./

#$(LIBTARGET) :  $(DYN_EM_ADTL) $(PHYS_ADTL) $(FRAME_ADTL) $(SHARE_ADTL)
#                  $(MAKE) $(J) non_nmm      ; \
#                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) \
#                                   $(FIRE_MODULES) \
#                                   $(DIAGNOSTIC_MODULES_EM)  ; \

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
# DO NOT DELETE

include depend.wrftladj

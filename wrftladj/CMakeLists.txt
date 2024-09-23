# WRF CMake Build


target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  # Mods MP
                  module_mp_mkessler.F
                  module_mp_nconvp.F
                  # Mods BL
                  module_bl_surface_drag.F
                  # Mods CU
                  module_cu_du.F
                  # Mods LL
                  module_linked_list2.F
                  # Mods PT
                  mediation_pertmod_io.F
                  # Dynem mods
                  module_advect_em_tl.F
                  module_advect_em_ad.F
                  module_diffusion_em_tl.F
                  module_diffusion_em_ad.F
                  module_small_step_em_tl.F
                  module_small_step_em_ad.F
                  module_big_step_utilities_em_tl.F
                  module_big_step_utilities_em_ad.F
                  module_em_tl.F
                  module_em_ad.F
                  module_bc_em_tl.F
                  module_bc_em_ad.F
                  module_first_rk_step_part1_tl.F
                  module_first_rk_step_part1_ad.F
                  module_first_rk_step_part2_tl.F
                  module_first_rk_step_part2_ad.F
                  module_sfs_nba_tl.F
                  module_sfs_nba_ad.F
                  module_sfs_driver_tl.F
                  module_sfs_driver_ad.F
                  # Dynem objs
                  solve_em_tl.F
                  solve_em_ad.F
                  start_em_tl.F
                  start_em_ad.F
                  # Phys mods
                  module_bl_gwdo_tl.F
                  module_bl_gwdo_ad.F
                  module_bl_surface_drag_tl.F
                  module_bl_surface_drag_ad.F
                  module_cu_du_tl.F
                  module_cu_du_ad.F
                  module_mp_mkessler_tl.F
                  module_mp_mkessler_ad.F
                  module_mp_wsm6r_tl.F
                  module_mp_wsm6r_ad.F
                  module_mp_nconvp_tl.F
                  module_mp_nconvp_ad.F
                  module_physics_addtendc_tl.F
                  module_physics_addtendc_ad.F
                  module_physics_init_tl.F
                  module_physics_init_ad.F
                  module_pbl_driver_tl.F
                  module_pbl_driver_ad.F
                  module_cumulus_driver_tl.F
                  module_cumulus_driver_ad.F
                  module_microphysics_driver_tl.F
                  module_microphysics_driver_ad.F
                  module_microphysics_zero_out_tl.F
                  module_microphysics_zero_out_ad.F
                  # Share mods
                  module_adtl_grid_utilities.F
                  module_bc_tl.F
                  module_bc_ad.F
                  # Share objs
                  jcdfi.F
                  # Frame objs
                  adStack.c
                  adBuffer.F
                )

set_source_files_properties(
                            adBuffer.F
                            TARGET_DIRECTORY ${PROJECT_NAME}_Core
                            PROPERTIES
                              Fortran_FORMAT FIXED
                            )


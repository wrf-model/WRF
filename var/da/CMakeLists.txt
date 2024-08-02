
# To somewhat mirror the DA build (and for "neatness") I won't put cmake files
# in each source folder and instead build at this level

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  da_4dvar/da_4dvar.f90
                  da_airep/da_airep.f90
                  da_airsr/da_airsr.f90
                  da_bogus/da_bogus.f90
                  da_buoy/da_buoy.f90
                  da_chem_sfc/da_chem_sfc.f90
                  # This is not in the original da lib, but I don't see how that
                  # isn't correct as many modules in this require it
                  da_control/da_control.f90


                  da_define_structures/da_define_structures.f90
                  da_dynamics/da_dynamics.f90
                  da_ffts/da_ffts.f90
                  da_ffts/module_ffts.f90
                  da_geoamv/da_geoamv.f90
                  da_gpseph/da_gpseph.f90
                  da_gpspw/da_gpspw.f90
                  da_gpsref/da_gpsref.f90
                  da_grid_definitions/da_grid_definitions.f90
                  da_interpolation/da_interpolation.f90
                  da_lightning/da_lightning.f90
                  da_main/copyfile.c
                  da_main/da_wrfvar_io.f90
                  da_main/da_wrfvar_top.f90
                  da_metar/da_metar.f90
                  da_minimisation/da_minimisation.f90
                  da_mtgirs/da_mtgirs.f90
                  da_obs_io/da_join_iv_for_multi_inc.c
                  da_obs_io/da_obs_io.f90
                  da_obs/da_obs.f90
                  da_par_util/da_par_util.f90
                  da_par_util/da_par_util1.f90
                  da_physics/da_physics.f90
                  da_physics/f_qv_from_rh.f90
                  da_pilot/da_pilot.f90
                  da_polaramv/da_polaramv.f90
                  da_profiler/da_profiler.f90
                  da_pseudo/da_pseudo.f90
                  da_qscat/da_qscat.f90
                  da_radar/da_radar.f90
                  da_radiance/amsr2time_.c
                  da_radiance/da_crtm.f90
                  da_radiance/da_radiance.f90
                  da_radiance/da_radiance1.f90
                  da_radiance/da_rttov.f90
                  da_radiance/gsi_constants.f90
                  da_radiance/gsi_kinds.f90
                  da_radiance/gsi_thinning.f90
                  da_radiance/mod_clddet_geoir.f90
                  da_radiance/module_radiance.f90
                  da_rain/da_rain.f90
                  da_recursive_filter/da_mat_cv3.f90
                  da_recursive_filter/da_recursive_filter.f90
                  da_recursive_filter/da_rf_cv3.f90
                  da_recursive_filter/da_rfz_cv3.f90
                  da_reporting/da_reporting.f90
                  da_satem/da_satem.f90
                  da_setup_structures/da_setup_structures.f90
                  da_ships/da_ships.f90
                  da_sound/da_sound.f90
                  da_spectral/da_spectral.f90
                  da_ssmi/da_ssmi.f90
                  da_ssmi/module_ssmi.f90
                  da_statistics/da_statistics.f90
                  da_synop/da_synop.f90
                  da_tamdar/da_tamdar.f90
                  da_test/da_test.f90
                  da_tools/da_rsl_interfaces.f90
                  da_tools/da_tools_serial.f90
                  da_tools/da_tools.f90
                  da_tools/da_wrf_interfaces.f90
                  da_tools/gamma1.f90
                  da_tracing/da_memory.c
                  da_tracing/da_tracing.f90
                  da_transfer_model/da_transfer_model.f90
                  da_update_bc/da_netcdf_interface.f90
                  da_varbc_tamdar/da_varbc_tamdar.f90
                  da_varbc/da_varbc.f90
                  da_vtox_transforms/da_vtox_transforms.f90
                  da_wavelet/da_wavelet.f90
                  # For da_bias_*
                  da_biascorr_airmass/rad_bias.f90

                )


target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              da_4dvar
                              da_airep
                              da_airsr
                              da_bogus
                              da_buoy
                              da_chem_sfc
                              da_define_structures
                              da_dynamics
                              da_etkf
                              da_ffts
                              da_gen_be
                              da_geoamv
                              da_gpseph
                              da_gpspw
                              da_gpsref
                              da_grid_definitions
                              da_interpolation
                              da_lightning
                              da_main
                              da_metar
                              da_minimisation
                              da_mtgirs
                              da_obs
                              da_obs_io
                              da_par_util
                              da_physics
                              da_pilot
                              da_polaramv
                              da_profiler
                              da_pseudo
                              da_qscat
                              da_radar
                              da_radiance
                              da_rain
                              da_recursive_filter
                              da_reporting
                              da_satem
                              da_setup_structures
                              da_ships
                              da_sound
                              da_spectral
                              da_ssmi
                              da_statistics
                              da_synop
                              da_tamdar
                              da_test
                              da_tools
                              da_tracing
                              da_transfer_model
                              da_update_bc
                              da_varbc
                              da_varbc_tamdar
                              da_vtox_transforms
                              da_wavelet
                            )

# DA util executables
set( DA_EXECUTABLES )
add_executable( da_tune_obs_hollingsworth1  da_util/da_tune_obs_hollingsworth1.f90  )
list( APPEND DA_EXECUTABLES da_tune_obs_hollingsworth1 )


add_executable( da_tune_obs_hollingsworth2  da_util/da_tune_obs_hollingsworth2.f90  )
list( APPEND DA_EXECUTABLES da_tune_obs_hollingsworth2 )


add_executable( da_tune_obs_desroziers      da_util/da_tune_obs_desroziers.f90      )
list( APPEND DA_EXECUTABLES da_tune_obs_desroziers )


add_executable( da_update_bc
                da_update_bc/da_update_bc.f90
                da_update_bc/da_module_couple_uv.f90
                )
target_include_directories( da_update_bc PRIVATE da_update_bc )
list( APPEND DA_EXECUTABLES da_update_bc )


add_executable( da_update_bc_ad
                da_update_bc/da_update_bc_ad.f90
                da_update_bc/da_module_couple_uv.f90
                da_update_bc/da_module_couple_uv_ad.f90
                )
target_include_directories( da_update_bc_ad PRIVATE da_update_bc )
list( APPEND DA_EXECUTABLES da_update_bc_ad )


add_executable( da_advance_time             da_util/da_advance_time.f90             )
list( APPEND DA_EXECUTABLES da_advance_time )


add_executable( da_verif_obs
                da_verif_obs/da_verif_obs.f90
                da_verif_obs/da_verif_obs_control.f90
                da_verif_obs/da_verif_obs_init.f90
                da_verif_obs/da_verif_tools.f90
                )
list( APPEND DA_EXECUTABLES da_verif_obs  )



add_executable( da_verif_grid
                da_verif_grid/da_verif_grid.f90
                da_verif_grid/da_verif_grid_control.f90
                )
list( APPEND DA_EXECUTABLES da_verif_grid )



add_executable( da_bias_airmass
                da_biascorr_airmass/da_bias_airmass.f90
                da_biascorr_airmass/pythag.f90
                da_biascorr_airmass/tqli.f90
                da_biascorr_airmass/tred2.f90
                da_biascorr_airmass/regress_one.f90
                )
list( APPEND DA_EXECUTABLES da_bias_airmass )



add_executable( da_bias_sele                da_biascorr_airmass/da_bias_sele.f90    )
list( APPEND DA_EXECUTABLES da_bias_sele )


add_executable( da_bias_scan                da_biascorr_airmass/da_bias_scan.f90    )
list( APPEND DA_EXECUTABLES da_bias_scan )


add_executable( da_bias_verif               da_biascorr_airmass/da_bias_verif.f90   )
list( APPEND DA_EXECUTABLES da_bias_verif )


add_executable( da_rad_diags                da_monitor/da_rad_diags.f90             )
list( APPEND DA_EXECUTABLES da_rad_diags )

# Now finally da_wrfvar
add_executable( da_wrfvar                   da_main/da_wrfvar_main.f90              )
list( APPEND DA_EXECUTABLES da_wrfvar )

foreach( DA_EXEC ${DA_EXECUTABLES} )
  target_link_libraries( 
                        ${DA_EXEC}
                        PRIVATE
                          ${PROJECT_NAME}_Core
                          $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                          $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        )
  target_link_options       ( ${DA_EXEC} PRIVATE ${WRF_LINK_FLAGS} )
  target_compile_definitions( ${DA_EXEC} PRIVATE ${PROJECT_COMPILE_DEFINITIONS} )
  target_compile_options    ( ${DA_EXEC} PRIVATE ${PROJECT_COMPILE_OPTIONS} )

endforeach()

install(
        TARGETS ${DA_EXECUTABLES}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

wrf_setup_targets(
                  TARGETS         ${DA_EXECUTABLES}
                  DEST_PATH       ${CMAKE_INSTALL_PREFIX}/run
                  USE_SYMLINKS
                  )

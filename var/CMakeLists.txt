
if ( ${ENABLE_CRTM} )
  add_subdirectory( external/crtm_2.3.0 )
endif()

if ( ${ENABLE_BUFR} )
  add_subdirectory( external/bufr )
endif()

if ( ${ENABLE_WAVELET} )
  add_subdirectory( external/wavelet )
endif()

wrf_m4_preproc_fortran( 
                        TARGET_NAME     da_generic_boilerplate
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".inc"
                        SOURCES         da/da_par_util/da_generic_boilerplate.m4
                        TARGET_SCOPE    ${PROJECT_NAME}_Core
                        FLAGS           ${M4_FLAGS}
                        )


add_dependencies( ${PROJECT_NAME}_Core da_generic_boilerplate )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                            )

# To somewhat mirror the DA build (and for "neatness") I won't put cmake files
# in each source folder and instead build at this level

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  da/da_4dvar/da_4dvar.f90
                  da/da_airep/da_airep.f90
                  da/da_airsr/da_airsr.f90
                  da/da_bogus/da_bogus.f90
                  da/da_buoy/da_buoy.f90
                  da/da_chem_sfc/da_chem_sfc.f90
                  # This is not in the original da lib, but I don't see how that
                  # isn't correct as many modules in this require it
                  da/da_control/da_control.f90


                  da/da_define_structures/da_define_structures.f90
                  da/da_dynamics/da_dynamics.f90
                  da/da_ffts/da_ffts.f90
                  da/da_ffts/module_ffts.f90
                  da/da_geoamv/da_geoamv.f90
                  da/da_gpseph/da_gpseph.f90
                  da/da_gpspw/da_gpspw.f90
                  da/da_gpsref/da_gpsref.f90
                  da/da_grid_definitions/da_grid_definitions.f90
                  da/da_interpolation/da_interpolation.f90
                  da/da_lightning/da_lightning.f90
                  da/da_main/copyfile.c
                  da/da_main/da_wrfvar_io.f90
                  da/da_main/da_wrfvar_top.f90
                  da/da_metar/da_metar.f90
                  da/da_minimisation/da_minimisation.f90
                  da/da_mtgirs/da_mtgirs.f90
                  da/da_obs_io/da_join_iv_for_multi_inc.c
                  da/da_obs_io/da_obs_io.f90
                  da/da_obs/da_obs.f90
                  da/da_par_util/da_par_util.f90
                  da/da_par_util/da_par_util1.f90
                  da/da_physics/da_physics.f90
                  da/da_physics/f_qv_from_rh.f90
                  da/da_pilot/da_pilot.f90
                  da/da_polaramv/da_polaramv.f90
                  da/da_profiler/da_profiler.f90
                  da/da_pseudo/da_pseudo.f90
                  da/da_qscat/da_qscat.f90
                  da/da_radar/da_radar.f90
                  da/da_radiance/amsr2time_.c
                  da/da_radiance/da_crtm.f90
                  da/da_radiance/da_radiance.f90
                  da/da_radiance/da_radiance1.f90
                  da/da_radiance/da_rttov.f90
                  da/da_radiance/gsi_constants.f90
                  da/da_radiance/gsi_kinds.f90
                  da/da_radiance/gsi_thinning.f90
                  da/da_radiance/mod_clddet_geoir.f90
                  da/da_radiance/module_radiance.f90
                  da/da_rain/da_rain.f90
                  da/da_recursive_filter/da_mat_cv3.f90
                  da/da_recursive_filter/da_recursive_filter.f90
                  da/da_recursive_filter/da_rf_cv3.f90
                  da/da_recursive_filter/da_rfz_cv3.f90
                  da/da_reporting/da_reporting.f90
                  da/da_satem/da_satem.f90
                  da/da_setup_structures/da_setup_structures.f90
                  da/da_ships/da_ships.f90
                  da/da_sound/da_sound.f90
                  da/da_spectral/da_spectral.f90
                  da/da_ssmi/da_ssmi.f90
                  da/da_ssmi/module_ssmi.f90
                  da/da_statistics/da_statistics.f90
                  da/da_synop/da_synop.f90
                  da/da_tamdar/da_tamdar.f90
                  da/da_test/da_test.f90
                  da/da_tools/da_rsl_interfaces.f90
                  da/da_tools/da_tools_serial.f90
                  da/da_tools/da_tools.f90
                  da/da_tools/da_wrf_interfaces.f90
                  da/da_tools/gamma1.f90
                  da/da_tracing/da_memory.c
                  da/da_tracing/da_tracing.f90
                  da/da_transfer_model/da_transfer_model.f90
                  da/da_update_bc/da_netcdf_interface.f90
                  da/da_varbc_tamdar/da_varbc_tamdar.f90
                  da/da_varbc/da_varbc.f90
                  da/da_vtox_transforms/da_vtox_transforms.f90
                  da/da_wavelet/da_wavelet.f90

                  # blas and lapack
                  external/blas/da_blas.f90
                  external/lapack/da_lapack.f90
                )


target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              da/da_4dvar
                              da/da_airep
                              da/da_airsr
                              da/da_bogus
                              da/da_buoy
                              da/da_chem_sfc
                              da/da_define_structures
                              da/da_dynamics
                              da/da_etkf
                              da/da_ffts
                              da/da_gen_be
                              da/da_geoamv
                              da/da_gpseph
                              da/da_gpspw
                              da/da_gpsref
                              da/da_grid_definitions
                              da/da_interpolation
                              da/da_lightning
                              da/da_main
                              da/da_metar
                              da/da_minimisation
                              da/da_mtgirs
                              da/da_obs
                              da/da_obs_io
                              da/da_par_util
                              da/da_physics
                              da/da_pilot
                              da/da_polaramv
                              da/da_profiler
                              da/da_pseudo
                              da/da_qscat
                              da/da_radar
                              da/da_radiance
                              da/da_rain
                              da/da_recursive_filter
                              da/da_reporting
                              da/da_satem
                              da/da_setup_structures
                              da/da_ships
                              da/da_sound
                              da/da_spectral
                              da/da_ssmi
                              da/da_statistics
                              da/da_synop
                              da/da_tamdar
                              da/da_test
                              da/da_tools
                              da/da_tracing
                              da/da_transfer_model
                              da/da_update_bc
                              da/da_varbc
                              da/da_varbc_tamdar
                              da/da_vtox_transforms
                              da/da_wavelet

                              # blas and lapack
                              external/blas
                              external/lapack
                              )

# WRF CMake Build

target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                            )

########################################################################################################################
#
# Now define base share
#
########################################################################################################################
set( 
    NON_DA_SOURCES
    module_llxy.F
    module_soil_pre.F
    module_compute_geop.F
    module_chem_share.F
    module_check_a_mundo.F
    module_HLaw.F
    module_ctrans_aqchem.F
    module_random.F
    module_interp_nmm.F
    module_interp_store.F
    module_string_tools.F
    module_MPP.F
    module_optional_input.F
    dfi.F
    solve_interface.F
    mediation_interp_domain.F
    mediation_force_domain.F
    mediation_feedback_domain.F
    wrf_tsin.F
    track_driver.F
    track_input.F
    module_trajectory.F
    bobrand.c
    wrf_timeseries.F
    track_driver.F
    wrf_fddaobs_in.F
    mediation_nest_move.F
    setfeenv.c
    )

if ( ${WRF_CORE} STREQUAL "DA" OR ${WRF_CORE} STREQUAL "DA_4D_VAR" )
  set( NON_DA_SOURCES "" )
endif()

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  # Common sources used in all cores                  
                  module_model_constants.F
                  
                  module_date_time.F
                  module_bc.F

                  module_bc_time_utilities.F
                  module_get_file_names.F

                  module_io_wrf.F

                  module_io_domain.F

                  input_wrf.F
                  output_wrf.F
                  wrf_bdyout.F
                  wrf_bdyin.F
                  
                  mediation_integrate.F
                  mediation_wrfmain.F

                  start_domain.F
                  init_modules.F
                  set_timekeeping.F
                  interp_fcn.F
                  sint.F
                  wrf_ext_write_field.F
                  wrf_ext_read_field.F
                  
                  landread.c

                  ${NON_DA_SOURCES}
                  )

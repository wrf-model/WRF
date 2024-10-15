# WRF CMake Build

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)
set( 
    WRF_INCLUDE_FILES
    module_internal_header_util.F
    ${CMAKE_BINARY_DIR}/frame/module_state_description.F
    )


########################################################################################################################
# Extra stuff for things that depend on registry code
# https://stackoverflow.com/a/50640971
# Generate all the combinations dynamically, not a fan of this file breakdown
########################################################################################################################
set( nl_dyn_source )
foreach( n RANGE 0 7 )
  
  wrf_c_preproc_fortran( 
                        TARGET_NAME     nl_set_${n}
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".f90"
                        PREFIX          "nl_set_${n}_"
                        INCLUDES        ${CMAKE_CURRENT_SOURCE_DIR}
                                        ${CMAKE_BINARY_DIR}/inc
                        DEPENDENCIES    registry_code
                        DEFINITIONS     ${PROJECT_COMPILE_DEFINITIONS} NNN=${n} NL_set_ROUTINES
                        SOURCES         nl_access_routines.F
                        )
  wrf_c_preproc_fortran( 
                      TARGET_NAME     nl_get_${n}
                      OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                      EXTENSION       ".f90"
                      PREFIX          "nl_get_${n}_"
                      INCLUDES        ${CMAKE_CURRENT_SOURCE_DIR}
                                      ${CMAKE_BINARY_DIR}/inc
                      DEPENDENCIES    registry_code
                      DEFINITIONS     ${PROJECT_COMPILE_DEFINITIONS} NNN=${n} NL_get_ROUTINES
                      SOURCES         nl_access_routines.F
                      )

  add_dependencies( ${PROJECT_NAME}_Core nl_get_${n} nl_set_${n} )

  list(
        APPEND
        nl_dyn_source
        ${CMAKE_CURRENT_BINARY_DIR}/preproc/nl_set_${n}_nl_access_routines.f90
        ${CMAKE_CURRENT_BINARY_DIR}/preproc/nl_get_${n}_nl_access_routines.f90
        )

endforeach()

if ( ${USE_M4} )
  wrf_m4_preproc_fortran( 
                          TARGET_NAME     md_calls
                          OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                          EXTENSION       ".inc"
                          SOURCES         md_calls.m4
                          TARGET_SCOPE    ${PROJECT_NAME}_Core
                          FLAGS           ${M4_FLAGS}
                          )
  
else()
  # Copy from arch quickly
  # Normally I would say we just add it as source but it is an include file and I don't want to potentially 
  # pollute the include chain by adding in arch/ *and* I want to maintain the order of operations
  # for target dependencies
  wrf_copy_source_files(
                        TARGET_NAME     md_calls
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        SOURCES         ${PROJECT_SOURCE_DIR}/arch/md_calls.inc
                        )
endif()

add_dependencies( ${PROJECT_NAME}_Core md_calls )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                            )
########################################################################################################################
#
# Now define base framework
#
########################################################################################################################
set( MODULE_DM module_dm_stubs.F )
if ( ${USE_RSL_LITE} )
  message( STATUS "Setting module_dm to RSL_LITE" )
  set(
      MODULE_DM
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/module_dm.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/feedback_domain_em_part1.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/feedback_domain_em_part2.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/force_domain_em_part2.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/interp_domain_em_part1.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/interp_domain_em_part2.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/interp_domain_em_part3.F
      ${PROJECT_SOURCE_DIR}/external/RSL_LITE/interp_domain_em_small.F
      )
endif()

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  ${WRF_INCLUDE_FILES}

                  module_configure.F
                  module_driver_constants.F
                  module_domain_type.F
                  module_domain.F
                  module_streams.F
                  module_wrf_error.F
                  module_machine.F
                  module_timing.F
                  # module_dm.F
                  ${MODULE_DM}
                  module_cpl.F
                  module_cpl_oasis3.F


                  ${nl_dyn_source}                                     # GENERATED

                  clog.c
                  collect_on_comm.c
                  hires_timer.c
                  libmassv.F
                  
                  module_clear_halos.F
                  module_comm_dm.F
                  module_comm_dm_0.F
                  module_comm_dm_1.F
                  module_comm_dm_2.F
                  module_comm_dm_3.F
                  module_comm_dm_4.F
                  module_comm_nesting_dm.F
                  
                  
                                    
                  module_integrate.F
                  
                  module_io.F
                  module_io_quilt.F
                  
                  module_nesting.F
                  module_quilt_outbuf_ops.F
                  module_sm.F
                  module_tiles.F
                  
                  pack_utils.c
                  wrf_debug.F
                  wrf_num_bytes_between.c
                  wrf_shutdown.F
                  )

# Disable optimizations on these files always
set_source_files_properties(
                            ${nl_dyn_source}
                            PROPERTIES
                            COMPILE_OPTIONS_OPTIMIZATION
                              $<$<COMPILE_LANGUAGE:Fortran>:${WRF_FCNOOPT}>
                            )

install( 
        FILES       ${WRF_INCLUDE_FILES}
        DESTINATION include/${FOLDER_COMPILE_TARGET}
        )

# WRF CMake Build

# get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)
# add_library(
#             ${FOLDER_COMPILE_TARGET}
#               STATIC
#               )

# ########################################################################################################################
# #
# # Extra object archive to set up dependencies
# #
# ########################################################################################################################
# add_library(
#             ${FOLDER_COMPILE_TARGET}_deps
#             OBJECT
#               module_configure.F
#               module_driver_constants.F
#               module_domain_type.F
#               module_domain.F
#               module_streams.F
#               module_wrf_error.F
#               module_machine.F
#               module_timing.F
#               module_dm.F
#               module_cpl.F
#               module_cpl_oasis3.F


#               module_alloc_space_0.F
#               module_alloc_space_1.F
#               module_alloc_space_2.F
#               module_alloc_space_3.F
#               module_alloc_space_4.F
#               module_alloc_space_5.F
#               module_alloc_space_6.F
#               module_alloc_space_7.F
#               module_alloc_space_8.F
#               module_alloc_space_9.F
              
#               ${CMAKE_BINARY_DIR}/frame/module_state_description.F # GENERATED
#               )

# add_dependencies( ${FOLDER_COMPILE_TARGET}_deps share_model_constants registry_code )


# # Tell CMake to chill, we'll generate those files shortly
# # Thanks to https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#how-can-i-add-a-dependency-to-a-source-file-which-is-generated-in-a-subdirectory
# # and https://samthursfield.wordpress.com/2015/11/21/cmake-dependencies-between-targets-and-files-and-custom-commands/
# set_source_files_properties(
#                             ${CMAKE_BINARY_DIR}/frame/module_state_description.F
#                             PROPERTIES
#                               GENERATED TRUE
#                             )

# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET}_deps
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE
#                       )

# target_include_directories( ${FOLDER_COMPILE_TARGET}_deps
#                             PRIVATE
#                               ${CMAKE_CURRENT_SOURCE_DIR}
#                               ${PROJECT_SOURCE_DIR}/inc
#                               # Compiled sources
#                               $<TARGET_PROPERTY:share_model_constants,Fortran_MODULE_DIRECTORY>
#                               $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
#                               # Generated sources
#                               ${CMAKE_BINARY_DIR}/frame
#                               ${CMAKE_BINARY_DIR}/inc
#                             )
# target_link_libraries(
#                       ${FOLDER_COMPILE_TARGET}_deps
#                       PRIVATE
#                         esmf_time_f90
#                       )



# ########################################################################################################################
# # Extra stuff for things that depend on registry code
# # https://stackoverflow.com/a/50640971
# # Generate all the combinations dynamically, not a fan of this file breakdown
# ########################################################################################################################
# foreach( n RANGE 0 7 )

#   add_library( nl_set_${n} OBJECT nl_access_routines.F )
#   add_library( nl_get_${n} OBJECT nl_access_routines.F )

  
#   # Wait for registry code
#   add_dependencies( nl_set_${n} registry_code ${FOLDER_COMPILE_TARGET}_deps )
#   add_dependencies( nl_get_${n} registry_code ${FOLDER_COMPILE_TARGET}_deps )

#   target_compile_definitions( nl_set_${n} PRIVATE -DNNN=${n} -DNL_set_ROUTINES )
#   target_compile_definitions( nl_get_${n} PRIVATE -DNNN=${n} -DNL_get_ROUTINES )

#   set_target_properties( 
#                         nl_set_${n}
#                         nl_get_${n}
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE
#                       )

#   target_include_directories( nl_set_${n}
#                               PRIVATE
#                                 ${CMAKE_CURRENT_SOURCE_DIR}
#                                 # Generated sources
#                                 ${CMAKE_BINARY_DIR}/inc
#                               )
#   target_include_directories( nl_get_${n}
#                               PRIVATE
#                                 ${CMAKE_CURRENT_SOURCE_DIR}
#                                 # Generated sources
#                                 ${CMAKE_BINARY_DIR}/inc
#                               )

#   target_link_libraries(
#                         ${FOLDER_COMPILE_TARGET}
#                         PRIVATE
#                           nl_set_${n}
#                           nl_get_${n}
#                         )

# endforeach()

########################################################################################################################
# Extra stuff for things that depend on registry code
# https://stackoverflow.com/a/50640971
# Generate all the combinations dynamically, not a fan of this file breakdown
########################################################################################################################
set( nl_dyn_source )
get_directory_property( DIR_DEFS DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
foreach( n RANGE 0 7 )
  
  wrf_c_preproc_fortran( 
                        TARGET_NAME     nl_set_${n}
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".f90"
                        PREFIX          "nl_set_${n}_"
                        INCLUDES        ${CMAKE_CURRENT_SOURCE_DIR}
                                        ${CMAKE_BINARY_DIR}/inc
                        DEPENDENCIES    registry_code
                        DEFINITIONS     ${DIR_DEFS} NNN=${n} NL_set_ROUTINES
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
                        DEFINITIONS     ${DIR_DEFS} NNN=${n} NL_get_ROUTINES
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
target_sources(
                # ${FOLDER_COMPILE_TARGET}
                ${PROJECT_NAME}_Core
                PRIVATE
                  module_configure.F
                  module_driver_constants.F
                  module_domain_type.F
                  module_domain.F
                  module_streams.F
                  module_wrf_error.F
                  module_machine.F
                  module_timing.F
                  module_dm.F
                  module_cpl.F
                  module_cpl_oasis3.F


                  module_alloc_space_0.F
                  module_alloc_space_1.F
                  module_alloc_space_2.F
                  module_alloc_space_3.F
                  module_alloc_space_4.F
                  module_alloc_space_5.F
                  module_alloc_space_6.F
                  module_alloc_space_7.F
                  module_alloc_space_8.F
                  module_alloc_space_9.F
                  
                  ${CMAKE_BINARY_DIR}/frame/module_state_description.F # GENERATED
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
                  module_internal_header_util.F
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

# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET} 
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE
#                       )


# target_link_libraries(  ${FOLDER_COMPILE_TARGET}
#                           PRIVATE
#                             ${FOLDER_COMPILE_TARGET}_deps
#                             ${netCDF_LIBRARIES}
#                             $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
#                             $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
#                         )

target_include_directories( #${FOLDER_COMPILE_TARGET}
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              # ${netCDF_INCLUDE_DIRS}
                              ${CMAKE_CURRENT_SOURCE_DIR}
                              # $<TARGET_PROPERTY:${FOLDER_COMPILE_TARGET}_deps,Fortran_MODULE_DIRECTORY>
                              # $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
                              # # Generated sources
                              # ${CMAKE_BINARY_DIR}/frame
                              # ${CMAKE_BINARY_DIR}/inc
                            )

# Depend on other generated files and libs, this should be one of the last things made apparently
# add_dependencies( ${FOLDER_COMPILE_TARGET} ${FOLDER_COMPILE_TARGET}_deps registry_code share )

# install(
#         TARGETS ${FOLDER_COMPILE_TARGET}
#         RUNTIME DESTINATION bin/
#         ARCHIVE DESTINATION lib/
#         LIBRARY DESTINATION lib/
#         )

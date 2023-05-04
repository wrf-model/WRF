# WRF CMake Build

# # get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

# # add_library(
# #             ${FOLDER_COMPILE_TARGET}
# #               STATIC
# #               )
# ########################################################################################################################
# #
# # Consider only making one library rather than forcing all these target dependencies - it would simplify a ton
# # but for now trying to recreate WRF building as a one-to-one replica
# #
# ########################################################################################################################
# add_library(
#             ${FOLDER_COMPILE_TARGET}_model_constants
#             OBJECT
#               module_model_constants.F
#               # module_llxy.F
#               # module_soil_pre.F
#               # module_date_time.F
#               # module_bc.F

#               )

# add_dependencies( ${FOLDER_COMPILE_TARGET}_model_constants frame_deps )


# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET}_model_constants
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE
#                       )

# target_include_directories( ${FOLDER_COMPILE_TARGET}_model_constants
#                             PRIVATE
#                               ${CMAKE_CURRENT_SOURCE_DIR}
#                               # Compiled sources
#                               $<TARGET_PROPERTY:frame_deps,Fortran_MODULE_DIRECTORY>
#                               $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
#                             )

# ########################################################################################################################
# #
# # Extra object archive to set up dependencies
# #
# ########################################################################################################################
# add_library(
#             ${FOLDER_COMPILE_TARGET}_deps
#             OBJECT
#               module_model_constants.F
#               module_llxy.F
#               module_soil_pre.F
#               module_date_time.F
#               module_bc.F

#               )

# add_dependencies( ${FOLDER_COMPILE_TARGET}_deps frame_deps )


# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET}_deps
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE
#                       )

target_include_directories( #${FOLDER_COMPILE_TARGET}_deps
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                              # ${PROJECT_SOURCE_DIR}/inc
                              # Compiled sources
                              # $<TARGET_PROPERTY:frame_deps,Fortran_MODULE_DIRECTORY>
                              # $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
                              # # Generated sources
                              # ${CMAKE_BINARY_DIR}/frame
                              # ${CMAKE_BINARY_DIR}/inc
                            )
# target_link_libraries(
#                       ${FOLDER_COMPILE_TARGET}_deps
#                       PRIVATE
#                         esmf_time_f90
#                       )



########################################################################################################################
#
# Now define base share
#
########################################################################################################################
target_sources(
                # ${FOLDER_COMPILE_TARGET}
                ${PROJECT_NAME}_Core
                PRIVATE
                  
                  module_model_constants.F
                  module_llxy.F
                  module_soil_pre.F
                  module_date_time.F
                  module_bc.F

                  module_bc_time_utilities.F
                  module_get_file_names.F
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

                  module_io_wrf.F
                  

                  module_io_domain.F

                  module_optional_input.F

                  input_wrf.F
                  output_wrf.F
                  wrf_bdyout.F
                  wrf_bdyin.F
                  dfi.F
                  mediation_integrate.F
                  mediation_wrfmain.F

                  solve_interface.F
                  mediation_interp_domain.F
                  mediation_force_domain.F
                  mediation_feedback_domain.F

                  start_domain.F
                  init_modules.F
                  set_timekeeping.F
                  interp_fcn.F
                  sint.F
                  wrf_ext_write_field.F
                  wrf_ext_read_field.F

                  
                  wrf_tsin.F
                  landread.c
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

# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET} 
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE                          
#                       )


# target_link_libraries(  ${FOLDER_COMPILE_TARGET}
#                           PRIVATE
#                             ${FOLDER_COMPILE_TARGET}_deps
#                             ${FOLDER_COMPILE_TARGET}_model_constants
#                             ${netCDF_LIBRARIES}
#                             $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
#                             $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
#                         )

# target_include_directories( ${FOLDER_COMPILE_TARGET}
#                             PRIVATE
#                               # ${netCDF_INCLUDE_DIRS}
#                               ${CMAKE_CURRENT_SOURCE_DIR}
#                               $<TARGET_PROPERTY:frame_deps,Fortran_MODULE_DIRECTORY>
#                               $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>

#                               #!TODO Fix duplicates of wrf_[io|status]_flags.h
#                               # ${CMAKE_CURRENT_SOURCE_DIR}/../ioapi_share 
#                               # ${CMAKE_CURRENT_SOURCE_DIR}/../io_grib_share
#                               # ${CMAKE_CURRENT_SOURCE_DIR}/grib1_util
#                               # ${CMAKE_CURRENT_SOURCE_DIR}/MEL_grib1
#                             )

# # Depend on framework, why? I don't know, this is a mess
# add_dependencies( 
#                   ${FOLDER_COMPILE_TARGET}
#                   ${FOLDER_COMPILE_TARGET}_deps
#                   ${FOLDER_COMPILE_TARGET}_model_constants
#                   frame_deps
#                   phys 
#                   )

# install(
#         TARGETS ${FOLDER_COMPILE_TARGET}
#         RUNTIME DESTINATION bin/
#         ARCHIVE DESTINATION lib/
#         LIBRARY DESTINATION lib/
#         )

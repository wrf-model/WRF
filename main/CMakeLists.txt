# WRF CMake Build
set( FOLDER_COMPILE_TARGETS )

# First make true executables
if ( ${WRF_CORE} STREQUAL "ARW" )
  add_executable( 
                  wrf
                  wrf.F
                  module_wrf_top.F
                  )
  list( APPEND FOLDER_COMPILE_TARGETS wrf )

elseif ( ${WRF_CORE} STREQUAL "WRFPLUS" )
  add_executable(
                  wrfplus
                  wrf.F
                  module_wrf_top.F
                  )
  list( APPEND FOLDER_COMPILE_TARGETS wrfplus )

# #!TODO When does this get activated?
# elseif()
#   add_executable(
#                   wrf_SST_ESMF
#                   wrf_ESMFMod.F
#                   wrf_SST_ESMF.F
#                   module_wrf_top.F
#                   )
#   list( APPEND FOLDER_COMPILE_TARGETS em_wrf_SST_ESMF )
endif()

# Use case info from higher CMakeLists.txt
set( MODULE_FILE ${PROJECT_SOURCE_DIR}/dyn_em/module_initialize_${WRF_CASE_MODULE}.F )

if ( ${WRF_CASE} STREQUAL "EM_REAL" )
  add_executable(
                  ndown
                  ndown_em.F
                  ${MODULE_FILE}
                  )
  add_executable(
                  tc
                  tc_em.F
                  ${MODULE_FILE}
                  )
  add_executable( 
                  real
                  real_em.F
                  ${MODULE_FILE}
                )
  list( APPEND FOLDER_COMPILE_TARGETS ndown tc real )

elseif( NOT ${WRF_GENERAL_IDEAL_CASE} ) # Not general ideal and not real
  # All others are variants of ideal
  add_executable( 
                  ideal
                  ideal_em.F
                  ${MODULE_FILE}
                )
  list( APPEND FOLDER_COMPILE_TARGETS ideal )
else()
  # greater than or equal to general ideal case
  add_executable( 
                  ideal
                  ideal_em.F
                  ${PROJECT_SOURCE_DIR}/dyn_em/module_initialize_ideal.F
                )
  list( APPEND FOLDER_COMPILE_TARGETS ideal )
endif()


foreach ( TARGET ${FOLDER_COMPILE_TARGETS} )
  set_target_properties( 
                        ${TARGET}
                          PROPERTIES
                            # Just dump everything in here
                            Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/${TARGET}/
                            Fortran_FORMAT           FREE                          
                        )
  target_link_libraries( 
                        ${TARGET}
                        PRIVATE
                          ${PROJECT_NAME}_Core
                        )
  target_include_directories(
                              ${TARGET}
                              PRIVATE
                                ${PROJECT_SOURCE_DIR}/inc
                                ${PROJECT_BINARY_DIR}/inc
                                $<TARGET_PROPERTY:${PROJECT_NAME}_Core,Fortran_MODULE_DIRECTORY>
                            )
endforeach()


install(
        TARGETS ${FOLDER_COMPILE_TARGETS}
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

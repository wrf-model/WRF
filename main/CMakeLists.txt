# WRF CMake Build
set( FOLDER_COMPILE_TARGETS )

# First make true executables
if ( ${WRF_CORE} STREQUAL "PLUS" )
  add_executable(
                  wrfplus
                  wrf.F
                  module_wrf_top.F
                  )
  list( APPEND FOLDER_COMPILE_TARGETS wrfplus )
else()
  # I believe this is always made if not WRF PLUS or ESMF
  add_executable( 
                  wrf
                  wrf.F
                  module_wrf_top.F
                  )
  list( APPEND FOLDER_COMPILE_TARGETS wrf )
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

  
  if ( ${USE_IPO} )
    set_target_properties(
                        ${TARGET}
                        PROPERTIES
                          INTERPROCEDURAL_OPTIMIZATION TRUE
                        )

    if ( ${CMAKE_VERSION} VERSION_LESS 3.24 )
      target_link_libraries( 
                        ${TARGET}
                        PRIVATE
                          ${PROJECT_NAME}_Core
                        )

      # Static libraries with LTO/IPO sometimes don't pull all the correct symbols
      set( LINKER_OPTION ${CMAKE_Fortran_LINKER_WRAPPER_FLAG} )
      target_link_options(
                          ${TARGET}
                          PRIVATE
                            ${LINKER_OPTION}--whole-archive $<TARGET_FILE:${PROJECT_NAME}_Core> ${LINKER_OPTION}--no-whole-archive
                          )
    else()
      target_link_libraries( 
                        ${TARGET}
                        PRIVATE
                          $<LINK_LIBRARY:WHOLE_ARCHIVE,${PROJECT_NAME}_Core>
                        )
    endif()
  else()
    target_link_libraries( 
                        ${TARGET}
                        PRIVATE
                          ${PROJECT_NAME}_Core
                        )
  endif()

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

# Install the "run" directory
install(
        DIRECTORY   ${PROJECT_SOURCE_DIR}/run/
        DESTINATION ${CMAKE_INSTALL_PREFIX}/run
        PATTERN CMakeLists.txt EXCLUDE
        PATTERN .gitignore     EXCLUDE
        )
wrf_setup_targets(
                  TARGETS         ${FOLDER_COMPILE_TARGETS}
                  DEST_PATH       ${CMAKE_INSTALL_PREFIX}/run
                  USE_SYMLINKS
                  )

# Re-setup this particular file
wrf_setup_files(
                FILES
                  ${PROJECT_SOURCE_DIR}/phys/noahmp/parameters/MPTABLE.TBL
                DEST_PATH
                  ${CMAKE_INSTALL_PREFIX}/run/
                )

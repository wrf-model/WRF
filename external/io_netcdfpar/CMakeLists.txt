# WRF CMake Build

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

# Do this first to simplify steps later
set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                      )


target_link_libraries(
                      ${FOLDER_COMPILE_TARGET}
                        PUBLIC
                          $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                          $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                          netCDF::netcdff
                      )


target_include_directories(
                            ${FOLDER_COMPILE_TARGET}
                            PUBLIC
                              $<TARGET_PROPERTY:${FOLDER_COMPILE_TARGET},Fortran_MODULE_DIRECTORY>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/external/ioapi_share>
                              $<INSTALL_INTERFACE:include/external/ioapi_share>
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                            )


# First preprocess
get_target_property   ( FOLDER_COMPILE_TARGET_INCLUDES ${FOLDER_COMPILE_TARGET} INCLUDE_DIRECTORIES )
wrf_c_preproc_fortran( 
                        TARGET_NAME     ${FOLDER_COMPILE_TARGET}_c_preproc_wrf_io
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".f90"
                        INCLUDES        ${FOLDER_COMPILE_TARGET_INCLUDES}
                        DEFINITIONS     ${PROJECT_COMPILE_DEFINITIONS}
                        SOURCES         wrf_io.F90
                        )

# Now M4 preprocess
wrf_m4_preproc_fortran( 
                        TARGET_NAME     ${FOLDER_COMPILE_TARGET}_m4_preproc_wrf_io
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        PREFIX          "m4_preproc_"
                        EXTENSION       ".f90"
                        SOURCES         ${CMAKE_CURRENT_BINARY_DIR}/preproc/wrf_io.f90
                        DEPENDENCIES    ${FOLDER_COMPILE_TARGET}_c_preproc_wrf_io
                        TARGET_SCOPE    ${FOLDER_COMPILE_TARGET}
                        FLAGS           ${M4_FLAGS}
                        )

add_dependencies( ${FOLDER_COMPILE_TARGET} ${FOLDER_COMPILE_TARGET}_m4_preproc_wrf_io )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  ${CMAKE_CURRENT_BINARY_DIR}/preproc/m4_preproc_wrf_io.f90
                  module_wrfsi_static.F90
                  field_routines.F90
                  )

# Now build diffwrf
set( DIFFWRF_TARGET diffwrf_ncpar )
add_executable(
                ${DIFFWRF_TARGET}
                diffwrf.F90
                ${PROJECT_SOURCE_DIR}/frame/clog.c
                ${PROJECT_SOURCE_DIR}/frame/module_wrf_error.F
                ${PROJECT_SOURCE_DIR}/frame/wrf_debug.F
                )

target_link_libraries(
                      ${DIFFWRF_TARGET}
                        PRIVATE
                          ${FOLDER_COMPILE_TARGET}
                      )
set_target_properties( 
                      ${DIFFWRF_TARGET}
                        PROPERTIES
                          # Just dump everything in here
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/${DIFFWRF_TARGET}
                          Fortran_FORMAT           FREE
                      )


install(
        TARGETS ${FOLDER_COMPILE_TARGET} ${DIFFWRF_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

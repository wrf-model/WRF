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


target_link_libraries(  ${FOLDER_COMPILE_TARGET}
                          PRIVATE
                            ${netCDF_LIBRARIES}
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                            $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        )

target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${netCDF_INCLUDE_DIRS}
                              ${CMAKE_CURRENT_SOURCE_DIR}
                              ${CMAKE_CURRENT_SOURCE_DIR}/../ioapi_share
                              ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                            )


# First preprocess
get_directory_property( DIR_DEFS DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
get_target_property   ( FOLDER_COMPILE_TARGET_INCLUDES ${FOLDER_COMPILE_TARGET} INCLUDE_DIRECTORIES )
wrf_c_preproc_fortran( 
                        TARGET_NAME     ${FOLDER_COMPILE_TARGET}_c_preproc_wrf_io
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".f90"
                        INCLUDES        ${FOLDER_COMPILE_TARGET_INCLUDES}
                        DEFINITIONS     ${DIR_DEFS}
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
                  field_routines.F90
                  )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

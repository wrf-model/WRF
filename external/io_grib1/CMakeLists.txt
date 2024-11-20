# WRF CMake Build

add_subdirectory( MEL_grib1  )
add_subdirectory( grib1_util )
add_subdirectory( WGRIB      )

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME )

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  grib1_routines.c
                  gribmap.c
                  io_grib1.F
                  trim.c
                  )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          Fortran_FORMAT           FREE
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
                              ${CMAKE_CURRENT_SOURCE_DIR}/../io_grib_share
                              ${CMAKE_CURRENT_SOURCE_DIR}/grib1_util
                              ${CMAKE_CURRENT_SOURCE_DIR}/MEL_grib1
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

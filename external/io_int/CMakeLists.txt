# WRF CMake Build
get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  io_int.F90
                  io_int_idx.c
                  module_io_int_idx.F90
                  module_io_int_read.F90
                  ${PROJECT_SOURCE_DIR}/frame/module_internal_header_util.F
                  )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                          Fortran_FORMAT           FREE                          
                      )

target_link_libraries(  ${FOLDER_COMPILE_TARGET}
                          PUBLIC
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                        )

target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PUBLIC
                              $<TARGET_PROPERTY:${FOLDER_COMPILE_TARGET},Fortran_MODULE_DIRECTORY>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/external/ioapi_share>
                              $<INSTALL_INTERFACE:include/external/ioapi_share>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/inc>
                              $<INSTALL_INTERFACE:include/inc>
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                            )

# Now build diffwrf
set( DIFFWRF_TARGET diffwrf_int )
add_executable(
                ${DIFFWRF_TARGET}
                diffwrf.F90
                ${PROJECT_SOURCE_DIR}/frame/module_machine.F
                ${PROJECT_SOURCE_DIR}/frame/module_driver_constants.F
                ${PROJECT_SOURCE_DIR}/frame/pack_utils.c
                ${PROJECT_SOURCE_DIR}/frame/module_wrf_error.F
                ${PROJECT_SOURCE_DIR}/frame/wrf_debug.F
                )

target_link_libraries(
                      ${DIFFWRF_TARGET}
                        PRIVATE
                          ${FOLDER_COMPILE_TARGET}
                      )

target_include_directories(
                            ${DIFFWRF_TARGET}
                            PRIVATE
                              ${CMAKE_BINARY_DIR}/inc
                            )

set_target_properties( 
                        ${DIFFWRF_TARGET}
                          PROPERTIES
                            # Just dump everything in here
                            Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/${DIFFWRF_TARGET}
                            Fortran_FORMAT           FREE
                        )

add_dependencies( ${DIFFWRF_TARGET} registry_code )

install(
        TARGETS ${FOLDER_COMPILE_TARGET} ${DIFFWRF_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

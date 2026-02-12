# WRF CMake Build
# get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)
set( FOLDER_COMPILE_TARGET CRTM )

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                          Fortran_FORMAT           FREE                          
                      )

target_compile_options(
                        ${FOLDER_COMPILE_TARGET}
                        PRIVATE
                          ${WRF_FCOPTIM} # No other source langauges so this is fine
                      )

add_subdirectory( libsrc )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

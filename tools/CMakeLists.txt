# WRF CMake Build
add_compile_options    ( "${PROJECT_COMPILE_OPTIONS}"     )
add_compile_definitions( "${PROJECT_COMPILE_DEFINITIONS}" )

#!TODO ORGANIZE THIS FOLDER
set( FOLDER_COMPILE_TARGET registry )

add_executable(
               ${FOLDER_COMPILE_TARGET}
              )

set( GEN_COMMS gen_comms.stub )
if ( ${USE_RSL_LITE} )
  message( STATUS "Setting gen_comms to RSL_LITE" )
  set( GEN_COMMS ${PROJECT_SOURCE_DIR}/external/RSL_LITE/gen_comms.c )
else()
  # Account for the weird makefile nonsense of copying things around
  set_source_files_properties( 
                              gen_comms.stub
                              TARGET_DIRECTORY ${FOLDER_COMPILE_TARGET}
                              PROPERTIES
                                LANGUAGE C
                              )
endif()

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  registry.c
                  my_strtok.c
                  reg_parse.c
                  data.c
                  type.c
                  misc.c
                  gen_defs.c
                  gen_allocs.c
                  gen_mod_state_descr.c
                  gen_scalar_indices.c
                  gen_args.c
                  gen_config.c
                  sym.c
                  symtab_gen.c
                  gen_irr_diag.c
                  gen_model_data_ord.c
                  gen_interp.c
                  # gen_comms.c
                  ${GEN_COMMS}
                  gen_scalar_derefs.c
                  set_dim_strs.c
                  gen_wrf_io.c
                  gen_streams.c
                  )

# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET} 
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE                          
#                       )

target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

# Extra stuff for weird registry stuff
set( REGISTRY_FILE "NO_REGISTRY_FILE_SET" )
if ( ${WRF_CORE} STREQUAL "ARW" )
  
  if ( ${ENABLE_CHEM} )
    set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.EM_CHEM )

  # This check does nothing
  # elseif ( ${WRF_DFI_RADAR} )
  #   set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.EM )

  else()
    set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.EM )

  endif()

elseif ( ${WRF_CORE} STREQUAL "PLUS" )
    set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.tladj )

elseif ( ${WRF_CORE} STREQUAL "CONVERT" )
  set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.CONVERT )

elseif ( ${WRF_CORE} STREQUAL "DA" OR ${WRF_CORE} STREQUAL "DA_4D_VAR" )
  if ( ${WRF_CHEM} )
    set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.wrfchemvar )
  else()
    set( REGISTRY_FILE ${PROJECT_SOURCE_DIR}/Registry/Registry.wrfvar )
  endif()

endif()

set( dealloc_source )
foreach( n RANGE 0 11 )
  list(
        APPEND
        dealloc_source
        ${CMAKE_BINARY_DIR}/inc/deallocs_${n}.F
        )
endforeach()

set( allocs_source )
foreach( n RANGE 0 31 )
  list(
        APPEND
        allocs_source
        ${CMAKE_BINARY_DIR}/inc/allocs_${n}.F
        )
endforeach()

wrf_expand_definitions( 
                        RESULT_VAR  REGISTRY_DEFS
                        DEFINITIONS ${PROJECT_COMPILE_DEFINITIONS}
                        )

# How this is not a bigger thing or not resolved is beyond me
# https://gitlab.kitware.com/cmake/cmake/-/issues/18005
# Also the suggestion does not work
add_custom_command(
                    OUTPUT
                      ${CMAKE_BINARY_DIR}/inc/nl_config.inc
                      ${CMAKE_BINARY_DIR}/frame/module_state_description.F
                      ${dealloc_source}
                      ${allocs_source}
                    COMMENT
                      "Generating registry code..."
                    WORKING_DIRECTORY
                      ${CMAKE_BINARY_DIR}
                    # Replicate what exists in project directory for registry
                    COMMAND
                      ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Registry
                    COMMAND
                      ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/inc
                    COMMAND
                      ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/frame
                    COMMAND
                      ${CMAKE_BINARY_DIR}/tools/registry ${REGISTRY_DEFS} -DNEW_BDYS ${REGISTRY_FILE} > ${CMAKE_BINARY_DIR}/registry.log 2>&1
                    #!TODO Just have the registry code actually check for failure or better yet rewrite the 
                    # registry code to not be so obfuscating
                    COMMAND 
                      ${CMAKE_COMMAND} -E compare_files ${CMAKE_BINARY_DIR}/inc/nl_config.inc ${CMAKE_BINARY_DIR}/inc/nl_config.inc
                    DEPENDS
                      ${FOLDER_COMPILE_TARGET}
                    )



add_custom_target( 
                  registry_code
                  DEPENDS
                    ${CMAKE_BINARY_DIR}/inc/nl_config.inc
                    ${CMAKE_BINARY_DIR}/frame/module_state_description.F
                    ${dealloc_source}
                    ${allocs_source}
                  )

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  ${CMAKE_BINARY_DIR}/frame/module_state_description.F
                  ${dealloc_source}
                  ${allocs_source}
                )


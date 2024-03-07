

set( FOLDER_COMPILE_TARGET registry_kpp )

add_executable(
               ${FOLDER_COMPILE_TARGET}
              )

set( GEN_COMMS ${PROJECT_SOURCE_DIR}/tools/gen_comms.stub )
# if ( ${USE_RSL_LITE} )
#   message( STATUS "Setting gen_comms to RSL_LITE" )
#   set( GEN_COMMS ${PROJECT_SOURCE_DIR}/external/RSL_LITE/gen_comms.c )
# else()
#   # Account for the weird makefile nonsense of copying things around
# Apparently chem-kpp does not use the rsl_lite comms, but I'm leaving the logic
# here just in case it needs to change
set_source_files_properties( 
                            ${GEN_COMMS}
                            TARGET_DIRECTORY ${FOLDER_COMPILE_TARGET}
                            PROPERTIES
                              LANGUAGE C
                            )
# endif()

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  registry_kpp.c
                  ${PROJECT_SOURCE_DIR}/tools/my_strtok.c
                  ${PROJECT_SOURCE_DIR}/tools/reg_parse.c
                  ${PROJECT_SOURCE_DIR}/tools/data.c
                  ${PROJECT_SOURCE_DIR}/tools/type.c
                  ${PROJECT_SOURCE_DIR}/tools/misc.c
                  ${PROJECT_SOURCE_DIR}/tools/sym.c
                  ${PROJECT_SOURCE_DIR}/tools/symtab_gen.c

                  gen_kpp.c
                  get_wrf_chem_specs.c
                  gen_kpp_mech_dr.c
                  gen_kpp_interface.c
                  get_kpp_chem_specs.c
                  compare_kpp_to_species.c
                  get_wrf_radicals.c
                  get_wrf_jvals.c
                  gen_kpp_utils.c
                  gen_kpp_interf_utils.c
                  gen_kpp_args_to_Update_Rconst.c
                  kpp_data.c

                  ${GEN_COMMS}
                  )


target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                              ${PROJECT_SOURCE_DIR}/tools
                            )

# Make sure we don't do any weird in-place modification of files
target_compile_definitions( ${FOLDER_COMPILE_TARGET} PRIVATE NO_MODIFY_MAKEFILE )

# Should this be installed? Consider also removing regular registry from install...
# install(
#         TARGETS ${FOLDER_COMPILE_TARGET}
#         RUNTIME DESTINATION bin/
#         ARCHIVE DESTINATION lib/
#         LIBRARY DESTINATION lib/
#         )


# Quickly add tuv_kpp exec as well
add_executable( tuv_kpp tuv_kpp.c )


# Do registry_kpp generation of sources and include files
set( KPP_INTERFACE_SOURCES )
set( KPP_INTERFACE_SOURCES_CMD_CHECK )
foreach( MECH ${MECHANISMS} )
  set( KPP_INTERFACE_SOURCE ${CMAKE_BINARY_DIR}/chem/module_kpp_${MECH}_interface.F )
  list( APPEND KPP_INTERFACE_SOURCES ${KPP_INTERFACE_SOURCE} )
  list( 
        APPEND KPP_INTERFACE_SOURCES_CMD_CHECK
        COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_INTERFACE_SOURCE} ${KPP_INTERFACE_SOURCE}
        )

endforeach()


get_directory_property( DIR_DEFS DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
wrf_expand_definitions( 
                        RESULT_VAR  REGISTRY_DEFS
                        DEFINITIONS ${DIR_DEFS}
                        )

add_custom_command(
                    OUTPUT
                      gen_${FOLDER_COMPILE_TARGET}
                      ${CMAKE_BINARY_DIR}/chem/kpp_mechanism_driver.F
                      ${KPP_INTERFACE_SOURCES}
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
                      $<TARGET_FILE:${FOLDER_COMPILE_TARGET}> ${REGISTRY_DEFS} ${REGISTRY_FILE} > ${CMAKE_BINARY_DIR}/${FOLDER_COMPILE_TARGET}.log 2>&1
                    ${KPP_INTERFACE_SOURCES_CMD_CHECK}
                    COMMAND
                      ${CMAKE_COMMAND} -E compare_files ${CMAKE_BINARY_DIR}/chem/kpp_mechanism_driver.F ${CMAKE_BINARY_DIR}/chem/kpp_mechanism_driver.F
                    COMMAND 
                      ${CMAKE_COMMAND} -E touch gen_${FOLDER_COMPILE_TARGET}
                    DEPENDS
                      ${FOLDER_COMPILE_TARGET}
                    )



add_custom_target( 
                  registry_kpp_code
                  DEPENDS
                    ${KPP_INTERFACE_SOURCES}
                    ${CMAKE_BINARY_DIR}/chem/kpp_mechanism_driver.F
                    gen_${FOLDER_COMPILE_TARGET}
                  )


add_dependencies( ${PROJECT_NAME}_Core registry_kpp_code )
target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  ${KPP_INTERFACE_SOURCES}
                  ${CMAKE_BINARY_DIR}/chem/kpp_mechanism_driver.F
                )


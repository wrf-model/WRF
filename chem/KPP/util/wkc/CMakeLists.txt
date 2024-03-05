

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
                  list_kpp_generated.c

                  ${GEN_COMMS}
                  
                  # gen_defs.c
                  # gen_allocs.c
                  # gen_mod_state_descr.c
                  # gen_scalar_indices.c
                  # gen_args.c
                  # gen_config.c
                  # gen_irr_diag.c
                  # gen_model_data_ord.c
                  # gen_interp.c
                  # gen_scalar_derefs.c
                  # set_dim_strs.c
                  # gen_wrf_io.c
                  # gen_streams.c
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


# Quickly add tuv_kpp exec as well and generate tuv photolysis inc file
add_executable( tuv_kpp tuv_kpp.c )


for all kpp_mech dirs

  # util/create_inc_files model
  for u l b a ibu ib ia e as thing
    if kpp_mechd_${thing}_${model}.inc not exist
      create
  if extra_args_to_update_rconst_${model}.inc not exist
    create
  if extra_args_update_rconst_${model}.inc not exist
    create
  if extra_decls_update_rconst_${model}.inc not exist
    create

  # run kpp model, generates .F files - KPP_HOME must be set by this point
  # often set to chem/KPP/kpp/kpp-2.1 in case you didn't know it was KPP
  kpp ${model}.kpp
  ${model}_Integrator.f90
  ${model}_Parameters.f90
  ${model}_Precision.f90
  ${model}_JacobianSP.f90
  ${model}_Jacobian.f90
  ${model}_Update_Rconst.f90


  if tuv.jmap 
    gen first once
    gen ${model}
  
  # write_decom ${model} chem_dir
  find in ${model}_Integrator.f90 if decomp && call
    make 


if tuv.jmap used
  gen last once

"gather" KPP/inc/*.inc files (known)


# Now do coupler




add_custom_command(
                    OUTPUT
                      ${CMAKE_BINARY_DIR}/inc/tuv2wrf_jvals.inc
                      ${CMAKE_BINARY_DIR}/inc/tuvdef_jvals.inc
                    WORKING_DIRECTORY
                      ${CMAKE_BINARY_DIR}
                    # Replicate what exists in project directory for registry
                    COMMAND
                      ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/inc
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
                  )









get_directory_property( DIR_DEFS DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
wrf_expand_definitions( 
                        RESULT_VAR  REGISTRY_DEFS
                        DEFINITIONS ${DIR_DEFS}
                        )

# How this is not a bigger thing or not resolved is beyond me
# https://gitlab.kitware.com/cmake/cmake/-/issues/18005
# Also the suggestion does not work
add_custom_command(
                    OUTPUT
                      ${CMAKE_BINARY_DIR}/inc/nl_config.inc
                      ${CMAKE_BINARY_DIR}/frame/module_state_description.F
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
                  )





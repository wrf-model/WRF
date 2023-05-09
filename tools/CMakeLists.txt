# WRF CMake Build

#!TODO ORGANIZE THIS FOLDER
set( FOLDER_COMPILE_TARGET registry )
# get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_executable(
               ${FOLDER_COMPILE_TARGET}
              )

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
                  gen_comms.c
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
                      ${CMAKE_BINARY_DIR}/tools/registry 
                      -DEM_CORE=1 -DNMM_CORE=0 -DNMM_MAX_DIM=2600 -DDA_CORE=0 -DWRFPLUS=0 -DIWORDSIZE=4 -DDWORDSIZE=8 -DRWORDSIZE=4 -DLWORDSIZE=4 -DNONSTANDARD_SYSTEM_SUBR  -DWRF_USE_CLM -DUSE_NETCDF4_FEATURES -DWRFIO_NCD_LARGE_FILE_SUPPORT  -DDM_PARALLEL -DNETCDF -DLANDREAD_STUB=1 -DUSE_ALLOCATABLES -Dwrfmodel -DGRIB1 -DINTIO -DKEEP_INT_AROUND -DLIMIT_ARGS -DBUILD_RRTMG_FAST=0 -DBUILD_RRTMK=0 -DBUILD_SBM_FAST=1 -DSHOW_ALL_VARS_USED=0 -DCONFIG_BUF_LEN=65536 -DMAX_DOMAINS_F=21 -DMAX_HISTORY=25 -DNMM_NEST=0  -DNEW_BDYS ${REGISTRY_FILE} &> ${CMAKE_BINARY_DIR}/registry.log
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


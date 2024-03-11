
message( STATUS "[KPP] Starting preprocessing preparation..." )
list(APPEND CMAKE_MESSAGE_INDENT "  ")

# These names will also be used by the registry_kpp
set( 
    MECHANISMS
    cb05_sorg_aq
    cb05_sorg_vbs_aq
    cbm4
    cbmz_bb
    cbmz_mosaic
    crimech
    cri_mosaic_4bin_aq
    cri_mosaic_8bin_aq
    gocartracm
    mozart
    mozart_mosaic_4bin
    mozart_mosaic_4bin_aq
    mozcart
    nmhc9
    racm
    racm_esrlsorg
    racm_esrlsorg_aqchem
    racm_mim
    racmpm
    racm_soa_vbs
    racm_soa_vbs_aqchem
    racm_soa_vbs_het
    racmsorg
    racmsorg_aqchem
    radm2
    radm2sorg
    saprc99
    saprc99_mosaic_4bin_vbs2
    saprc99_mosaic_8bin_vbs2_aq
    t1_mozcart
    )

add_subdirectory( kpp/kpp-2.1 )
add_subdirectory( util/wkc )
add_subdirectory( util/write_decomp )


target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  module_wkppc_constants.F
              )

target_include_directories( 
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}/inc
                          )

set( GEN_MECHD_INC  u l b a ibu ib ia e )
set(
    GEN_RCONST_INC
    extra_args_to_update_rconst
    extra_args_update_rconst
    extra_decls_update_rconst
    )

message( STATUS "[KPP] Adding generation for mechanisms" )

# Some helper vars for grouping the kpp gen and tuv inc
set( KPP_GEN_GROUPS )
set( KPP_CURRENT_GEN_GROUP 0 )

set( TUV_PHOTOLYSIS_GEN FALSE )
set( TUV_PHOTOLYSIS_CMD )
set( TUV_PHOTOLYSIS_OUT )

foreach ( MECH ${MECHANISMS} )

  ######################################################################################################################
  ##
  ## Generate any "missing" includes that are just empty
  ##
  if ( EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/inc/${MECH} )
    file( COPY ${CMAKE_CURRENT_SOURCE_DIR}/inc/${MECH}/ DESTINATION ${CMAKE_BINARY_DIR}/inc )
  endif()

  foreach ( MECHD_INAME ${GEN_MECHD_INC} )
    set( GEN_MECHD_INC_FILENAME kpp_mechd_${MECHD_INAME}_${MECH}.inc )
    if ( NOT EXISTS ${CMAKE_BINARY_DIR}/inc/${GEN_MECHD_INC_FILENAME} )
      message( STATUS "[KPP] Touching empty include ${GEN_MECHD_INC_FILENAME}" )
      file( TOUCH ${CMAKE_BINARY_DIR}/inc/${GEN_MECHD_INC_FILENAME} )
    endif()
  endforeach()

  foreach ( RCONST_INAME ${GEN_RCONST_INC} )
    set( GEN_RCONST_INC_FILENAME ${RCONST_INAME}_${MECH}.inc )
    if ( NOT EXISTS ${CMAKE_BINARY_DIR}/inc/${GEN_RCONST_INC_FILENAME} )
      message( STATUS "[KPP] Touching empty include ${GEN_RCONST_INC_FILENAME}" )
      file( TOUCH ${CMAKE_BINARY_DIR}/inc/${GEN_RCONST_INC_FILENAME} )
    endif()
  endforeach()
  ##
  ##
  ##
  ######################################################################################################################

  ######################################################################################################################
  ##
  ## run kpp model, generates .F files
  ##
  #  KPP_HOME must be set by this point
  # often set to chem/KPP/kpp/kpp-2.1 in case you didn't know it was KPP
  set( KPP_HOME ${CMAKE_CURRENT_SOURCE_DIR}/kpp/kpp-2.1 )

  # I'm using the configure-time dir creation to simplify custom target/command usage
  set( KPP_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR}/mechanisms/${MECH} )
  file( COPY ${CMAKE_CURRENT_SOURCE_DIR}/mechanisms/${MECH}/ DESTINATION ${KPP_WORKING_DIR} )
  set(
      ${MECH}_KPP_SOURCES
      ${KPP_WORKING_DIR}/${MECH}_Parameters.f90
      ${KPP_WORKING_DIR}/${MECH}_Precision.f90
      ${KPP_WORKING_DIR}/${MECH}_JacobianSP.f90
      ${KPP_WORKING_DIR}/${MECH}_Jacobian.f90
      ${KPP_WORKING_DIR}/${MECH}_Update_Rconst.f90
      )
  set(
      ${MECH}_KPP_SOURCES_INTEGRATOR
      ${KPP_WORKING_DIR}/${MECH}_Integrator.f90
      )

  add_custom_command(
                      OUTPUT
                        ${${MECH}_KPP_SOURCES}
                        ${${MECH}_KPP_SOURCES_INTEGRATOR}
                      COMMAND ${CMAKE_COMMAND} -E env KPP_HOME=${KPP_HOME} $<TARGET_FILE:kpp> ${MECH}.kpp > ${CMAKE_BINARY_DIR}/kpp_${MECH}.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Integrator.f90     ${KPP_WORKING_DIR}/${MECH}_Integrator.f90
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Parameters.f90     ${KPP_WORKING_DIR}/${MECH}_Parameters.f90
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Precision.f90      ${KPP_WORKING_DIR}/${MECH}_Precision.f90
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_JacobianSP.f90     ${KPP_WORKING_DIR}/${MECH}_JacobianSP.f90
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Jacobian.f90       ${KPP_WORKING_DIR}/${MECH}_Jacobian.f90
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Update_Rconst.f90  ${KPP_WORKING_DIR}/${MECH}_Update_Rconst.f90
                      WORKING_DIRECTORY ${KPP_WORKING_DIR}
                      DEPENDS kpp
                      )
  add_custom_target(
                    ${MECH}_kpp
                    COMMENT "[KPP] Generating kpp files for ${MECH}"
                    DEPENDS
                      ${${MECH}_KPP_SOURCES}
                      ${${MECH}_KPP_SOURCES_INTEGRATOR}
                    )

  add_dependencies( ${PROJECT_NAME}_Core ${MECH}_kpp )
  target_sources( ${PROJECT_NAME}_Core PRIVATE ${${MECH}_KPP_SOURCES} )
  ##
  ##
  ##
  ######################################################################################################################

  ######################################################################################################################
  ##
  ## Special KPP generation logic to streamline parallel building without crashing
  ##
  # Assign target to a group to serialize based on maximum threads allowed as
  # this consumes a ton of RAM
  list( LENGTH KPP_GEN_GROUPS KPP_GEN_GROUPS_LEN )
  if ( ${KPP_GEN_GROUPS_LEN} GREATER ${KPP_CURRENT_GEN_GROUP} )
    list( GET KPP_GEN_GROUPS ${KPP_CURRENT_GEN_GROUP} KPP_PREVIOUS_GROUP_TARGET )
  endif()

  # This is the start point, add it into the list
  list( INSERT KPP_GEN_GROUPS ${KPP_CURRENT_GEN_GROUP} ${MECH}_kpp )
  MATH( EXPR KPP_NEXT_IDX "${KPP_CURRENT_GEN_GROUP} + 1" )

  if ( DEFINED KPP_PREVIOUS_GROUP_TARGET )
    # Add as a dependency
    add_dependencies( ${KPP_PREVIOUS_GROUP_TARGET} ${MECH}_kpp )
    # Replace current listing - currently next index is actually pointing to old
    list( REMOVE_AT KPP_GEN_GROUPS ${KPP_NEXT_IDX} )
  endif()

  # Now we have min( MAX_KPP_GEN_THREADS, KPP_CURRENT_GEN_GROUP ) in list, loop modulus to go back to zero
  MATH( EXPR KPP_CURRENT_GEN_GROUP "${KPP_NEXT_IDX} % ${MAX_KPP_GEN_THREADS}" )
  ##
  ## end grouping logic
  ##
  ######################################################################################################################


  # generate tuv photolysis inc file
  if ( EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/mechanisms/${MECH}/${MECH}.tuv.jmap )
    if ( NOT ${TUV_PHOTOLYSIS_GEN} )
      set( TUV_PHOTOLYSIS_GEN TRUE )
    endif()
    
    list(
          APPEND TUV_PHOTOLYSIS_CMD
          COMMAND ${CMAKE_COMMAND} -E chdir ${KPP_WORKING_DIR} $<TARGET_FILE:tuv_kpp> ${MECH} ${CMAKE_BINARY_DIR}/inc/ >> ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
          COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_${MECH}
          )
    list(
          APPEND TUV_PHOTOLYSIS_OUT
          tuv_photolysis_${MECH}
          )
  endif()

  ######################################################################################################################
  ##
  ## Integrator decomp rewrite
  ##
  # Do decomp for all - easier to manually specify rather than greps
  file( 
        WRITE ${KPP_WORKING_DIR}/decomp_uses.inc
        "USE ${MECH}_Parameters\nUSE ${MECH}_JacobianSP\n"
      )
  file( 
        WRITE ${KPP_WORKING_DIR}/mech.tmp
        "${MECH}\n"
      )
  
  add_executable(
                  write_decomp_${MECH}
                  util/write_decomp/write_decomp.F
                  ${KPP_WORKING_DIR}/${MECH}_Parameters.f90
                  ${KPP_WORKING_DIR}/${MECH}_Precision.f90
                  ${KPP_WORKING_DIR}/${MECH}_JacobianSP.f90
                  )
  target_include_directories( write_decomp_${MECH} PRIVATE ${KPP_WORKING_DIR} )
  set_target_properties( 
                        write_decomp_${MECH}
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/decomp/${MECH}/
                          Fortran_FORMAT           FREE
                      )
  add_dependencies( write_decomp_${MECH} ${MECH}_kpp )


  # Now that write_decomp is generated for this kpp mechanism, process it
  add_custom_command(
                      OUTPUT
                        ${KPP_WORKING_DIR}/${MECH}_Integrator_decomp.f90
                        ${KPP_WORKING_DIR}/decomp_${MECH}.inc
                        # ${CMAKE_COMMAND} -E chdir ${KPP_WORKING_DIR}
                      COMMAND $<TARGET_FILE:write_decomp_${MECH}> > ${CMAKE_BINARY_DIR}/kpp_write_decomp_${MECH}.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/decomp_${MECH}.inc                 ${KPP_WORKING_DIR}/decomp_${MECH}.inc

                      COMMAND $<TARGET_FILE:integration_edit>
                                ${MECH} ${MECH}_Integrator.f90
                                decomp_${MECH}.inc
                                ${MECH}_Integrator_decomp.f90 > ${CMAKE_BINARY_DIR}/kpp_integrator_edit_${MECH}.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E compare_files ${KPP_WORKING_DIR}/${MECH}_Integrator_decomp.f90      ${KPP_WORKING_DIR}/${MECH}_Integrator_decomp.f90
                      
                      WORKING_DIRECTORY ${KPP_WORKING_DIR}
                      DEPENDS kpp
                      )
  add_custom_target(
                    ${MECH}_integrator_decomp
                    COMMENT "[KPP] Post-processing integrator decomposition for ${MECH}_Integrator.f90"
                    DEPENDS
                      ${KPP_WORKING_DIR}/${MECH}_Integrator_decomp.f90
                      ${KPP_WORKING_DIR}/decomp_${MECH}.inc
                    )

  add_dependencies( ${PROJECT_NAME}_Core ${MECH}_integrator_decomp )
  target_sources( ${PROJECT_NAME}_Core PRIVATE ${KPP_WORKING_DIR}/${MECH}_Integrator_decomp.f90 )
  ##
  ## 
  ##
  ######################################################################################################################

endforeach()

# Group our aggregated tuv commands if it was specified
if ( ${TUV_PHOTOLYSIS_GEN} )
  add_custom_command(
                      OUTPUT
                        # Note: I am not setting output as 
                        # inc/tuv2wrf_jvals.inc
                        # inc/tuvdef_jvals.inc
                        # Because I want each step to succeed and not just the start or stop
                        tuv_photolysis_start
                        ${TUV_PHOTOLYSIS_OUT}
                        tuv_photolysis_stop
                      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/inc/
                      COMMAND $<TARGET_FILE:tuv_kpp> FIRST ${CMAKE_BINARY_DIR}/inc/  > ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_start
                      ${TUV_PHOTOLYSIS_CMD}
                      COMMAND $<TARGET_FILE:tuv_kpp> LAST ${CMAKE_BINARY_DIR}/inc/  >> ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_stop
                      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
                      DEPENDS tuv_kpp
                      )
  add_custom_target(
                      tuv_photolysis_inc # Naming this something different just to identify this is a target
                      COMMENT "[KPP] Generating tuv_photolysis include file"
                      DEPENDS
                        tuv_photolysis_start
                        ${TUV_PHOTOLYSIS_OUT}
                        tuv_photolysis_stop
                      )
  add_dependencies( ${PROJECT_NAME}_Core tuv_photolysis_inc )
endif()

list(POP_BACK CMAKE_MESSAGE_INDENT)


add_subdirectory( kpp/kpp-2.1 )
add_subdirectory( util/wkc )
add_subdirectory( util/write_decomp )
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

message( STATUS "[KPP] Adding generation for mechanisms" )

# Some helper vars for grouping the kpp gen and tuv inc
set( KPP_GEN_GROUPS )
set( KPP_CURRENT_GEN_GROUP 0 )
set( KPP_SOURCES )

set( TUV_PHOTOLYSIS_GEN FALSE )
set( TUV_PHOTOLYSIS_CMD )
set( TUV_PHOTOLYSIS_OUT )

foreach ( MECH ${MECHANISMS} )
  # Is this even necessary?
  # # util/create_inc_files model
  # for u l b a ibu ib ia e as thing
  #   if kpp_mechd_${thing}_${model}.inc not exist
  #     create
  # if extra_args_to_update_rconst_${model}.inc not exist
  #   create
  # if extra_args_update_rconst_${model}.inc not exist
  #   create
  # if extra_decls_update_rconst_${model}.inc not exist
  #   create

  # run kpp model, generates .F files - KPP_HOME must be set by this point
  # often set to chem/KPP/kpp/kpp-2.1 in case you didn't know it was KPP
  set( KPP_HOME ${CMAKE_CURRENT_SOURCE_DIR}/kpp/kpp-2.1 )

  # I'm using the configure-time dir creation to simplify custom target/command usage
  set( KPP_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR}/${MECH}/kpp/)
  file( COPY ${CMAKE_CURRENT_SOURCE_DIR}/mechanisms/${MECH}/ DESTINATION ${KPP_WORKING_DIR} )
  set(
      ${MECH}_KPP_SOURCES
      ${MECH}_Integrator.f90
      ${MECH}_Parameters.f90
      ${MECH}_Precision.f90
      ${MECH}_JacobianSP.f90
      ${MECH}_Jacobian.f90
      ${MECH}_Update_Rconst.f90
      )

  add_custom_command(
                      OUTPUT
                        ${${MECH}_KPP_SOURCES}
                      COMMAND ${CMAKE_COMMAND} -E env KPP_HOME=${KPP_HOME} $<TARGET_FILE:kpp> ${MECH}.kpp > ${CMAKE_BINARY_DIR}/kpp_${MECH}.log 2>&1
                      WORKING_DIRECTORY ${KPP_WORKING_DIR}
                      DEPENDS kpp
                      )
  add_custom_target(
                    ${MECH}_kpp
                    COMMENT "[KPP] Generating kpp files for ${MECH}"
                    DEPENDS
                      ${${MECH}_KPP_SOURCES}
                    )
  set_source_files_properties(
                              ${${MECH}_KPP_SOURCES}
                              DIRECTORY ${PROJECT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}
                              TARGET_DIRECTORY ${PROJECT_NAME}_Core
                              PROPERTIES
                                GENERATED ON
                              )
  add_dependencies( ${PROJECT_NAME}_Core ${MECH}_kpp )
  target_sources( ${PROJECT_NAME}_Core PRIVATE ${${MECH}_KPP_SOURCES} )

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
          COMMAND ${CMAKE_COMMAND} -E chdir ${KPP_WORKING_DIR} $<TARGET_FILE:tuv_kpp> ${MECH} ${CMAKE_CURRENT_BINARY_DIR}/inc/ >> ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
          COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_${MECH}
          )
    list(
          APPEND TUV_PHOTOLYSIS_OUT
          tuv_photolysis_${MECH}
          )
  endif()

  # # write_decom ${model} chem_dir
  # add_custom_command(
  #                     OUTPUT
  #                       ${KPP_WORKING_DIR}/${MECH}_decomp_check
  #                     COMMAND ${CMAKE_COMMAND} -E 
  #                     COMMAND ${CMAKE_COMMAND} -E ${KPP_WORKING_DIR}/${MECH}_decomp_check
  #                     WORKING_DIRECTORY ${KPP_WORKING_DIR}
  #                     DEPENDS ${MECH}_kpp
  #                     )
  # add_custom_target(
  #                   ${MECH}_kpp
  #                   COMMENT "[KPP] Checking for decomposition rewrite in ${MECH}_Integrator.f90"
  #                   DEPENDS
  #                     ${KPP_WORKING_DIR}/${MECH}_decomp_check
  #                   )
  # set_source_files_properties(
  #                             ${KPP_WORKING_DIR}/${MECH}_Integrator-decomp.f90
  #                             DIRECTORY ${PROJECT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}
  #                             TARGET_DIRECTORY ${PROJECT_NAME}_Core
  #                             PROPERTIES
  #                               GENERATED ON
  #                             )

  # As far as I can tell the decomp is not used
  # find in ${model}_Integrator.f90 if decomp && call
    
  #   gen use inc statements to decomp_uses.inc:
  #     ${model}_Parameters
  #     ${model}_JacobianSP
  #   compile write_decomp :
  #     write_decomp.F
  #     ${model}_Parameters.f90
  #     ${model}_Precision.f90
  #     ${model}_JacobianSP.f90
  #   gen mech.tmp in same directory run with ${model} as sole string
  #   run write_decomp outputs decomp_${model}.inc

  #   # compile integr_edit.exe (already done above)
  #   run integr_edit ${model} ${model}_Integrator.f90 decomp_${model}.inc new_output
  #   take new output as ${model}_Integrator.f90 stand-in now
  
  

endforeach()

if ( ${TUV_PHOTOLYSIS_GEN} )
  add_custom_command(
                      OUTPUT
                        tuv_photolysis_start
                        ${TUV_PHOTOLYSIS_OUT}
                        tuv_photolysis_stop
                      COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/inc/
                      COMMAND $<TARGET_FILE:tuv_kpp> FIRST ${CMAKE_CURRENT_BINARY_DIR}/inc/  > ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_start
                      ${TUV_PHOTOLYSIS_CMD}
                      COMMAND $<TARGET_FILE:tuv_kpp> LAST ${CMAKE_CURRENT_BINARY_DIR}/inc/  >> ${CMAKE_BINARY_DIR}/tuv_photolysis_inc.log 2>&1
                      COMMAND ${CMAKE_COMMAND} -E touch tuv_photolysis_stop
                      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
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

# "gather" KPP/inc/*.inc files (known)


# # Now do coupler
# registry stuff



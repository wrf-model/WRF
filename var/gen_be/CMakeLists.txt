set(
    GEN_BE_EXECUTABLES
    gen_be_stage0_wrf
    gen_be_stage0_gsi
    gen_be_ep1
    gen_be_ep2
    gen_be_stage1
    gen_be_vertloc
    gen_be_addmean
    gen_be_stage1_gsi
    gen_be_stage1_1dvar
    gen_be_stage2
    gen_be_stage2_gsi
    gen_mbe_stage2
    gen_be_stage2_1dvar
    gen_be_stage2a
    gen_be_stage3
    gen_be_stage4_global
    gen_be_stage4_regional
    gen_be_cov2d
    gen_be_cov3d
    gen_be_cov3d3d_bin3d_contrib
    gen_be_cov3d3d_contrib
    gen_be_cov2d3d_contrib
    gen_be_cov3d2d_contrib
    gen_be_diags
    gen_be_diags_read
    gen_be_hist
    gen_be_ensrf
    gen_be_etkf
    gen_be_ensmean
    )

foreach( GEN_BE ${GEN_BE_EXECUTABLES} )
  add_executable( ${GEN_BE} ${GEN_BE}.f90 )
  target_link_libraries( 
                        ${GEN_BE}
                        PRIVATE
                          ${PROJECT_NAME}_Core
                          $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                          $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        )
  target_link_options       ( ${GEN_BE} PRIVATE ${WRF_LINK_FLAGS} )
  target_compile_definitions( ${GEN_BE} PRIVATE ${PROJECT_COMPILE_DEFINITIONS} )
  target_compile_options    ( ${GEN_BE} PRIVATE ${PROJECT_COMPILE_OPTIONS} )

endforeach()


install(
        TARGETS ${GEN_BE_EXECUTABLES}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

wrf_setup_targets(
                  TARGETS         ${GEN_BE_EXECUTABLES}
                  DEST_PATH       ${CMAKE_INSTALL_PREFIX}/run
                  USE_SYMLINKS
                  )


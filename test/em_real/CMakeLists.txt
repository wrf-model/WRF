# These are just rules for this case, could be named CMakeLists.txt or something 
# like install_rules.cmake, whatever you want really
get_filename_component( FOLDER_DEST ${CMAKE_CURRENT_SOURCE_DIR} NAME )

install(
        DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/test/
        PATTERN CMakeLists.txt EXCLUDE
        PATTERN .gitignore     EXCLUDE
        )
wrf_setup_targets(
                  TARGETS         real tc ndown wrf
                  DEST_PATH       ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}
                  USE_SYMLINKS
                  )

wrf_setup_files( 
                FILES
                  ${PROJECT_SOURCE_DIR}/run/README.namelist
                  ${PROJECT_SOURCE_DIR}/run/README.physics_files
                  ${PROJECT_SOURCE_DIR}/run/README.physics_files

                  ${PROJECT_SOURCE_DIR}/run/ETAMPNEW_DATA
                  ${PROJECT_SOURCE_DIR}/run/ETAMPNEW_DATA.expanded_rain
                  ${PROJECT_SOURCE_DIR}/run/RRTM_DATA
                  ${PROJECT_SOURCE_DIR}/run/RRTMG_LW_DATA
                  ${PROJECT_SOURCE_DIR}/run/RRTMG_SW_DATA
                  ${PROJECT_SOURCE_DIR}/run/CAM_ABS_DATA
                  ${PROJECT_SOURCE_DIR}/run/CAM_AEROPT_DATA
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.RCP4.5
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.RCP6
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.RCP8.5
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.A1B
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.A2
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP119
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP126
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP245
                  #!TODO Why does this have an alt name?
                  # ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP245 # Has alt name, why?
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP370
                  ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP585
                  ${PROJECT_SOURCE_DIR}/run/CLM_ALB_ICE_DFS_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_ALB_ICE_DRC_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_ASM_ICE_DFS_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_ASM_ICE_DRC_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_DRDSDT0_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_EXT_ICE_DFS_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_EXT_ICE_DRC_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_KAPPA_DATA
                  ${PROJECT_SOURCE_DIR}/run/CLM_TAU_DATA
                  ${PROJECT_SOURCE_DIR}/run/ozone.formatted
                  ${PROJECT_SOURCE_DIR}/run/ozone_lat.formatted
                  ${PROJECT_SOURCE_DIR}/run/ozone_plev.formatted
                  ${PROJECT_SOURCE_DIR}/run/aerosol.formatted
                  ${PROJECT_SOURCE_DIR}/run/aerosol_lat.formatted
                  ${PROJECT_SOURCE_DIR}/run/aerosol_lon.formatted
                  ${PROJECT_SOURCE_DIR}/run/aerosol_plev.formatted
                  ${PROJECT_SOURCE_DIR}/run/eclipse_besselian_elements.dat
                  ${PROJECT_SOURCE_DIR}/run/capacity.asc
                  ${PROJECT_SOURCE_DIR}/run/coeff_p.asc
                  ${PROJECT_SOURCE_DIR}/run/coeff_q.asc
                  ${PROJECT_SOURCE_DIR}/run/constants.asc
                  ${PROJECT_SOURCE_DIR}/run/masses.asc
                  ${PROJECT_SOURCE_DIR}/run/termvels.asc
                  ${PROJECT_SOURCE_DIR}/run/kernels.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/kernels_z.asc
                  ${PROJECT_SOURCE_DIR}/run/bulkdens.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/bulkradii.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/CCN_ACTIVATE.BIN
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_1.dat-v5.4_2momI
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_1.dat-v5.4_3momI
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_2.dat-v5.3
                  ${PROJECT_SOURCE_DIR}/run/HLC.TBL
                  ${PROJECT_SOURCE_DIR}/run/wind-turbine-1.tbl
                  ${PROJECT_SOURCE_DIR}/run/ishmael-gamma-tab.bin
                  ${PROJECT_SOURCE_DIR}/run/ishmael-qi-qc.bin
                  ${PROJECT_SOURCE_DIR}/run/ishmael-qi-qr.bin
                  ${PROJECT_SOURCE_DIR}/run/BROADBAND_CLOUD_GODDARD.bin
                  ${PROJECT_SOURCE_DIR}/run/STOCHPERT.TBL
                  
                  ${PROJECT_SOURCE_DIR}/run/GENPARM.TBL
                  ${PROJECT_SOURCE_DIR}/run/LANDUSE.TBL
                  ${PROJECT_SOURCE_DIR}/run/SOILPARM.TBL
                  ${PROJECT_SOURCE_DIR}/run/URBPARM.TBL
                  ${PROJECT_SOURCE_DIR}/run/URBPARM_LCZ.TBL
                  ${PROJECT_SOURCE_DIR}/run/VEGPARM.TBL
                  ${PROJECT_SOURCE_DIR}/phys/noahmp/parameters/MPTABLE.TBL
                  ${PROJECT_SOURCE_DIR}/run/tr49t67
                  ${PROJECT_SOURCE_DIR}/run/tr49t85
                  ${PROJECT_SOURCE_DIR}/run/tr67t85
                  ${PROJECT_SOURCE_DIR}/run/gribmap.txt
                  ${PROJECT_SOURCE_DIR}/run/grib2map.tbl
                DEST_PATH
                  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}
                )

wrf_setup_file_new_name( 
                        FILE      ${PROJECT_SOURCE_DIR}/run/CAMtr_volume_mixing_ratio.SSP245
                        NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/CAMtr_volume_mixing_ratio
                        USE_SYMLINKS
                        )


if ( ${USE_DOUBLE} )
  
  wrf_setup_file_new_name(
                          FILE      ${PROJECT_SOURCE_DIR}/run/ETAMPNEW_DATA_DBL
                          NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/ETAMPNEW_DATA
                          )
  wrf_setup_file_new_name(
                          FILE      ${PROJECT_SOURCE_DIR}/run/ETAMPNEW_DATA.expanded_rain_DBL
                          NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/ETAMPNEW_DATA.expanded_rain
                          )
  wrf_setup_file_new_name(
                          FILE      ${PROJECT_SOURCE_DIR}/run/RRTM_DATA_DBL
                          NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/RRTM_DATA
                          )
  wrf_setup_file_new_name(
                          FILE      ${PROJECT_SOURCE_DIR}/run/RRTMG_LW_DATA_DBL
                          NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/RRTMG_LW_DATA
                          )
  wrf_setup_file_new_name(
                          FILE      ${PROJECT_SOURCE_DIR}/run/RRTMG_SW_DATA_DBL
                          NEW_NAME  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}/RRTMG_SW_DATA
                          )
endif()

# These are just rules for this case, could be named CMakeLists.txt or something 
# like install_rules.cmake, whatever you want really
wrf_link_targets(
                  TARGETS         real tc ndown wrf
                  SYM_PATH        ${CMAKE_CURRENT_SOURCE_DIR}
                  OLD_BASE_PATH   ${CMAKE_INSTALL_PREFIX}/bin
                  )

wrf_link_files( 
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
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_1.dat-5.3-2momI
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_1.dat-3momI_v5.1.7
                  ${PROJECT_SOURCE_DIR}/run/p3_lookupTable_2.dat-2momI_v5.2.3
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
                  ${PROJECT_SOURCE_DIR}/run/MPTABLE.TBL # Missing?
                  ${PROJECT_SOURCE_DIR}/run/tr49t67
                  ${PROJECT_SOURCE_DIR}/run/tr49t85
                  ${PROJECT_SOURCE_DIR}/run/tr67t85
                  ${PROJECT_SOURCE_DIR}/run/gribmap.txt
                  ${PROJECT_SOURCE_DIR}/run/grib2map.tbl
                SYM_PATH  ${CMAKE_CURRENT_SOURCE_DIR}
                )

#!TODO Add alternate double stuff
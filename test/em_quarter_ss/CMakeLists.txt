# These are just rules for this case, could be named CMakeLists.txt or something 
# like install_rules.cmake, whatever you want really
wrf_link_targets(
                  TARGETS   ideal wrf
                  SYM_PATH  ${CMAKE_CURRENT_SOURCE_DIR}
                  )

wrf_link_files( 
                FILES
                  ${PROJECT_SOURCE_DIR}/run/README.namelist
                  ${PROJECT_SOURCE_DIR}/run/bulkdens.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/bulkradii.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/capacity.asc
                  ${PROJECT_SOURCE_DIR}/run/coeff_p.asc
                  ${PROJECT_SOURCE_DIR}/run/coeff_q.asc
                  ${PROJECT_SOURCE_DIR}/run/constants.asc
                  ${PROJECT_SOURCE_DIR}/run/kernels.asc_s_0_03_0_9
                  ${PROJECT_SOURCE_DIR}/run/kernels_z.asc
                  ${PROJECT_SOURCE_DIR}/run/masses.asc
                  ${PROJECT_SOURCE_DIR}/run/termvels.asc
                SYM_PATH  ${CMAKE_CURRENT_SOURCE_DIR}
                )
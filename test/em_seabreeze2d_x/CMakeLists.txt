# These are just rules for this case, could be named CMakeLists.txt or something 
# like install_rules.cmake, whatever you want really
wrf_link_targets(
                  TARGETS   ideal wrf
                  SYM_PATH  ${CMAKE_CURRENT_SOURCE_DIR}
                  )

wrf_link_files( 
                FILES
                  ${PROJECT_SOURCE_DIR}/run/README.namelist
                  ${PROJECT_SOURCE_DIR}/run/LANDUSE.TBL
                  ${PROJECT_SOURCE_DIR}/run/RRTM_DATA
                SYM_PATH  ${CMAKE_CURRENT_SOURCE_DIR}
                )
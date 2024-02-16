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
                  TARGETS   ideal wrf
                  DEST_PATH  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}
                  USE_SYMLINKS
                  )

wrf_setup_files( 
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
                DEST_PATH  ${CMAKE_INSTALL_PREFIX}/test/${FOLDER_DEST}
                )
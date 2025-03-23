get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)
set( 
    WRF_INCLUDE_FILES
    wrf_io_flags.h
    wrf_status_codes.h
    )

install( 
        FILES       ${WRF_INCLUDE_FILES}
        DESTINATION include/external/${FOLDER_COMPILE_TARGET}
        )
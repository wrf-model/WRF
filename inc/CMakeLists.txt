get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)
set( 
    WRF_INCLUDE_FILES
    intio_tags.h
    streams.h
    version_decl
    ${PROJECT_BINARY_DIR}/inc/commit_decl
    )

install( 
        FILES       ${WRF_INCLUDE_FILES}
        DESTINATION include/inc/${FOLDER_COMPILE_TARGET}
        )
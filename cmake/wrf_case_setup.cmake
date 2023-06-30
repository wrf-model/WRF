# WRF Macro for adding target symlinks to be run after internal install() code
macro( wrf_link_targets )

  set( options        )
  set( oneValueArgs   SYM_PATH OLD_BASE_PATH )
  set( multiValueArgs TARGETS  )

  cmake_parse_arguments(
                        WRF_LINK
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )


  foreach ( WRF_LINK_TARGET ${WRF_LINK_TARGETS} )

    # Generate install code for each target
    # https://stackoverflow.com/a/56528615
    #!TODO Do we *need* the rm for symlinks beforehand?
    # get_filename_component( WRF_LINK_FILE_ONLY $<TARGET_FILE:${WRF_LINK_TARGET}> NAME

    install( 
            CODE "
                  message( STATUS \"Creating symlinks for $<TARGET_FILE_NAME:${WRF_LINK_TARGET}>\" )
                  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${WRF_LINK_OLD_BASE_PATH}/$<TARGET_FILE_NAME:${WRF_LINK_TARGET}> ${WRF_LINK_SYM_PATH}/$<TARGET_FILE_NAME:${WRF_LINK_TARGET}> )
                  "
            COMPONENT links
            )

  endforeach()

endmacro()

# WRF Macro for adding file symlinks to be run after internal install() code
macro( wrf_link_files )

  set( options        )
  set( oneValueArgs   SYM_PATH )
  set( multiValueArgs FILES  )

  cmake_parse_arguments(
                        WRF_LINK
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  
  foreach ( WRF_LINK_FILE ${WRF_LINK_FILES} )
    
    # Generate install code for each file, this could be done in a simpler manner
    # with regular commands but to preserve order of operations it will be done via install( CODE ... )
    # https://stackoverflow.com/a/56528615
    get_filename_component( WRF_LINK_FULL_FILE ${WRF_LINK_FILE} ABSOLUTE )
    get_filename_component( WRF_LINK_FILE_ONLY ${WRF_LINK_FILE} NAME     )
    # Left here for debug purposes, may want to turn this into a trace-level debug msg
    # message( "Generating install commands for ${WRF_LINK_FILE_ONLY} into ${WRF_LINK_SYM_PATH}" )
    install( 
            CODE "
                  message( STATUS \"Creating symlinks for ${WRF_LINK_FILE_ONLY}\" )
                  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${WRF_LINK_FULL_FILE} ${WRF_LINK_SYM_PATH}/${WRF_LINK_FILE_ONLY} )
                  "
            COMPONENT links            
            )

  endforeach()

endmacro()

# WRF Macro for adding file symlink to be run after internal install() code
macro( wrf_link_file_new_name )

  set( options        )
  set( oneValueArgs   SYM_PATH FILE NEW_NAME )
  set( multiValueArgs )

  cmake_parse_arguments(
                        WRF_LINK
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  
  
  # Generate install code for each file, this could be done in a simpler manner
  # with regular commands but to preserve order of operations it will be done via install( CODE ... )
  # https://stackoverflow.com/a/56528615
  get_filename_component( WRF_LINK_FULL_FILE ${WRF_LINK_FILE} ABSOLUTE )
  get_filename_component( WRF_LINK_FILE_ONLY ${WRF_LINK_FILE} NAME     )
  get_filename_component( WRF_LINK_NEW_NAME_FULL_FILE ${WRF_LINK_NEW_NAME} ABSOLUTE )
  get_filename_component( WRF_LINK_NEW_NAME_FILE_ONLY ${WRF_LINK_NEW_NAME} NAME     )
  # Left here for debug purposes, may want to turn this into a trace-level debug msg
  # message( "Generating install commands for ${WRF_LINK_FILE_ONLY} to ${WRF_LINK_NEW_NAME_FILE_ONLY}" )
  install( 
          CODE "
                message( STATUS \"Creating symlinks for ${WRF_LINK_FILE_ONLY} (rename ${WRF_LINK_NEW_NAME_FILE_ONLY})\" )
                execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${WRF_LINK_FULL_FILE} ${WRF_LINK_NEW_NAME_FULL_FILE} )
                "
          COMPONENT links
          )

endmacro()


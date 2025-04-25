# WRF Macro for adding target symlinks/copies to be run after internal install() code
macro( wrf_setup_targets )

  set( options        USE_SYMLINKS )
  set( oneValueArgs   DEST_PATH )
  set( multiValueArgs TARGETS  )

  cmake_parse_arguments(
                        WRF_SETUP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  set( WRF_SETUP_CMD copy_if_different )
  if ( ${WRF_SETUP_USE_SYMLINKS} )
    set( WRF_SETUP_CMD create_symlink )
  endif()


  foreach ( WRF_SETUP_TARGET ${WRF_SETUP_TARGETS} )

    # Generate install code for each target
    # https://stackoverflow.com/a/56528615
    #!TODO Do we *need* the rm for symlinks beforehand?
    # get_filename_component( WRF_SETUP_FILE_ONLY $<TARGET_FILE:${WRF_SETUP_TARGET}> NAME

    # If we ever wanted to link or copy things other than binaries we could change this
    set( WRF_SETUP_INSTALL_LOCATION ${CMAKE_INSTALL_PREFIX}/bin )

    install( 
            CODE "
                  message( STATUS \"Setting up $<TARGET_FILE_NAME:${WRF_SETUP_TARGET}> via ${WRF_SETUP_CMD}\" )
                  execute_process( COMMAND ${CMAKE_COMMAND} -E ${WRF_SETUP_CMD} ${WRF_SETUP_INSTALL_LOCATION}/$<TARGET_FILE_NAME:${WRF_SETUP_TARGET}> ${WRF_SETUP_DEST_PATH}/$<TARGET_FILE_NAME:${WRF_SETUP_TARGET}> )
                  "
            COMPONENT setup
            )
    
    # Add .exe link as well
    install( 
            CODE "
                  message( STATUS \"Creating symlink for $<TARGET_FILE_NAME:${WRF_SETUP_TARGET}>.exe\" )
                  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${WRF_SETUP_DEST_PATH}/$<TARGET_FILE_NAME:${WRF_SETUP_TARGET}> ${WRF_SETUP_DEST_PATH}/$<TARGET_FILE_NAME:${WRF_SETUP_TARGET}>.exe )
                  "
            COMPONENT setup
            )

  endforeach()

endmacro()

# WRF Macro for adding file symlinks/copies to be run after internal install() code
macro( wrf_setup_files )

  set( options        USE_SYMLINKS )
  set( oneValueArgs   DEST_PATH )
  set( multiValueArgs FILES  )

  cmake_parse_arguments(
                        WRF_SETUP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  set( WRF_SETUP_CMD copy_if_different )
  if ( ${WRF_SETUP_USE_SYMLINKS} )
    set( WRF_SETUP_CMD create_symlink )
  endif()
  
  foreach ( WRF_SETUP_FILE ${WRF_SETUP_FILES} )
    
    # Generate install code for each file, this could be done in a simpler manner
    # with regular commands but to preserve order of operations it will be done via install( CODE ... )
    # https://stackoverflow.com/a/56528615
    get_filename_component( WRF_SETUP_FULL_FILE ${WRF_SETUP_FILE} ABSOLUTE )
    get_filename_component( WRF_SETUP_FILE_ONLY ${WRF_SETUP_FILE} NAME     )
    # Left here for debug purposes, may want to turn this into a trace-level debug msg
    # message( "Generating install commands for ${WRF_SETUP_FILE_ONLY} into ${WRF_SETUP_DEST_PATH}" )
    install( 
            CODE "
                  message( STATUS \"Setting up ${WRF_SETUP_FILE_ONLY} via ${WRF_SETUP_CMD}\" )
                  execute_process( COMMAND ${CMAKE_COMMAND} -E ${WRF_SETUP_CMD} ${WRF_SETUP_FULL_FILE} ${WRF_SETUP_DEST_PATH}/${WRF_SETUP_FILE_ONLY} )
                  "
            COMPONENT setup
            )

  endforeach()

endmacro()

# WRF Macro for adding file symlink to be run after internal install() code
macro( wrf_setup_file_new_name )

  set( options        USE_SYMLINKS )
  set( oneValueArgs   FILE NEW_NAME )
  set( multiValueArgs )

  cmake_parse_arguments(
                        WRF_SETUP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  
  set( WRF_SETUP_CMD copy_if_different )
  if ( ${WRF_SETUP_USE_SYMLINKS} )
    set( WRF_SETUP_CMD create_symlink )
  endif()
  
  # Generate install code for each file, this could be done in a simpler manner
  # with regular commands but to preserve order of operations it will be done via install( CODE ... )
  # https://stackoverflow.com/a/56528615
  get_filename_component( WRF_SETUP_FULL_FILE ${WRF_SETUP_FILE} ABSOLUTE )
  get_filename_component( WRF_SETUP_FILE_ONLY ${WRF_SETUP_FILE} NAME     )
  get_filename_component( WRF_SETUP_NEW_NAME_FULL_FILE ${WRF_SETUP_NEW_NAME} ABSOLUTE )
  get_filename_component( WRF_SETUP_NEW_NAME_FILE_ONLY ${WRF_SETUP_NEW_NAME} NAME     )
  # Left here for debug purposes, may want to turn this into a trace-level debug msg
  # message( "Generating install commands for ${WRF_SETUP_FILE_ONLY} to ${WRF_SETUP_NEW_NAME_FILE_ONLY}" )
  install( 
          CODE "
                message( STATUS \"Setting up ${WRF_SETUP_FILE_ONLY} (rename ${WRF_SETUP_NEW_NAME_FILE_ONLY}) via ${WRF_SETUP_CMD}\" )
                execute_process( COMMAND ${CMAKE_COMMAND} -E ${WRF_SETUP_CMD} ${WRF_SETUP_FULL_FILE} ${WRF_SETUP_NEW_NAME_FULL_FILE} )
                "
          COMPONENT setup
          )

endmacro()


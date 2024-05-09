# WRF Macro for copying files with generated dependency
# https://stackoverflow.com/a/34800230
macro( wrf_copy_source_files )

  set( options        )
  set( oneValueArgs   TARGET_NAME SUFFIX PREFIX EXTENSION OUTPUT_DIR )
  set( multiValueArgs DEPENDENCIES SOURCES )

  cmake_parse_arguments(
                        WRF_COPY
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  
  # Generate compile command and file outputs
  set( WRF_COPY_OUTPUT   )
  set( WRF_COPY_COMMANDS )
  foreach( WRF_COPY_SOURCE_FILE  ${WRF_COPY_SOURCES} )
    get_filename_component( WRF_COPY_INPUT_SOURCE           ${WRF_COPY_SOURCE_FILE} REALPATH )
    get_filename_component( WRF_COPY_INPUT_SOURCE_FILE_ONLY ${WRF_COPY_SOURCE_FILE} NAME     )
    
    if ( ${WRF_COPY_EXTENSION} MATCHES "^[.][a-z0-9]+$" )
      string( REGEX REPLACE "[.].*$" "${WRF_COPY_EXTENSION}" WRF_COPY_OUTPUT_FILE ${WRF_COPY_INPUT_SOURCE_FILE_ONLY} )
    else()
      # Default to original filename
      set( WRF_COPY_OUTPUT_FILE ${WRF_COPY_INPUT_SOURCE_FILE_ONLY} )
    endif()

    set( WRF_COPY_OUTPUT_FILE ${WRF_COPY_OUTPUT_DIR}/${WRF_COPY_PREFIX}${WRF_COPY_OUTPUT_FILE}${WRF_COPY_SUFFIX} )
    

    list( 
          APPEND WRF_COPY_COMMANDS 
          COMMAND ${CMAKE_COMMAND} -E copy ${WRF_COPY_INPUT_SOURCE} ${WRF_COPY_OUTPUT_FILE}
          # Force check that they were made
          COMMAND ${CMAKE_COMMAND} -E compare_files ${WRF_COPY_OUTPUT_FILE} ${WRF_COPY_OUTPUT_FILE}
          )
    list( 
          APPEND WRF_COPY_OUTPUT
          ${WRF_COPY_OUTPUT_FILE}
          )
    
    # # Tell all targets that eventually use this file that it is generated - this is useful if this macro is used in a
    # # different directory than where the target dependency is set
    # # Thanks to https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#how-can-i-add-a-dependency-to-a-source-file-which-is-generated-in-a-subdirectory
    # # and https://samthursfield.wordpress.com/2015/11/21/cmake-dependencies-between-targets-and-files-and-custom-commands/
    # # It keeps getting better lol
    # # https://gitlab.kitware.com/cmake/cmake/-/issues/18399
    # # We could use cmake 3.20+ and CMP0118, but this allows usage from 3.18.6+
    # TL;DR - This doesn't work despite all documentation stating otherwise, need to use CMP0118
    # set_source_files_properties(
    #                             ${WRF_COPY_OUTPUT_FILE}
    #                             ${WRF_COPY_TARGET_DIRECTORY}
    #                             PROPERTIES
    #                               GENERATED TRUE
    #                             )

    message( STATUS "File ${WRF_COPY_SOURCE_FILE} will be copied to ${WRF_COPY_OUTPUT_FILE}" )

  endforeach()

  # Preprocess sources into a custom target
  add_custom_command(
                      OUTPUT ${WRF_COPY_OUTPUT}
                      COMMAND ${CMAKE_COMMAND} -E  make_directory ${WRF_COPY_OUTPUT_DIR}
                      ${WRF_COPY_COMMANDS}
                      COMMENT "Preprocessing ${WRF_COPY_TARGET_NAME}"
                      DEPENDS ${WRF_COPY_DEPENDENCIES}
                      )

  add_custom_target(
                    ${WRF_COPY_TARGET_NAME}
                    DEPENDS ${WRF_COPY_OUTPUT}
                    )
endmacro()

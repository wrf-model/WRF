# WRF Macro for m4 preprocessing F files
macro( wrf_m4_preproc_fortran )

  set( options        )
  set( oneValueArgs   TARGET_NAME SUFFIX PREFIX EXTENSION OUTPUT_DIR M4_PROGRAM )
  set( multiValueArgs DEPENDENCIES SOURCES M4_FLAGS )

  cmake_parse_arguments(
                        WRF_PP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  set( WRF_PP_M4_PROGRAM_TO_USE m4 )
  if ( DEFINED WRF_PP_M4_PROGRAM )
    set( WRF_PP_M4_PROGRAM_TO_USE ${WRF_PP_M4_PROGRAM} )
  endif()

  # Generate compile command and file outputs
  set( WRF_PP_OUTPUT   )
  set( WRF_PP_COMMANDS )
  foreach( WRF_PP_SOURCE_FILE  ${WRF_PP_SOURCES} )
    get_filename_component( WRF_PP_INPUT_SOURCE           ${WRF_PP_SOURCE_FILE} REALPATH )
    get_filename_component( WRF_PP_INPUT_SOURCE_FILE_ONLY ${WRF_PP_SOURCE_FILE} NAME     )

    if ( ${WRF_PP_EXTENSION} MATCHES "^[.][a-z0-9]+$" )
      string( REGEX REPLACE "[.].*$" "${WRF_PP_EXTENSION}" WRF_PP_OUTPUT_FILE ${WRF_PP_INPUT_SOURCE_FILE_ONLY} )
    else()
      # Default extension
      string( REGEX REPLACE "[.].*$" ".i" WRF_PP_OUTPUT_FILE ${WRF_PP_INPUT_SOURCE_FILE_ONLY} )
    endif()

    set( WRF_PP_OUTPUT_FILE ${WRF_PP_OUTPUT_DIR}/${WRF_PP_PREFIX}${WRF_PP_OUTPUT_FILE}${WRF_PP_SUFFIX} )
    

    list( 
          APPEND WRF_PP_COMMANDS 
          COMMAND ${WRF_PP_M4_PROGRAM_TO_USE} ${WRF_PP_M4_FLAGS} ${WRF_PP_INPUT_SOURCE} > ${WRF_PP_OUTPUT_FILE}
          # Force check that they were made
          COMMAND ${CMAKE_COMMAND} -E compare_files ${WRF_PP_OUTPUT_FILE} ${WRF_PP_OUTPUT_FILE}
          )
    list( 
          APPEND WRF_PP_OUTPUT
          ${WRF_PP_OUTPUT_FILE}
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
    #                             ${WRF_PP_OUTPUT_FILE}
    #                             ${WRF_PP_TARGET_DIRECTORY}
    #                             PROPERTIES
    #                               GENERATED TRUE
    #                             )

    # message( STATUS "File ${WRF_PP_SOURCE_FILE} will be preprocessed into ${WRF_PP_OUTPUT_FILE}" )

  endforeach()

  # Preprocess sources into a custom target
  add_custom_command(
                      OUTPUT ${WRF_PP_OUTPUT}
                      COMMAND ${CMAKE_COMMAND} -E  make_directory ${WRF_PP_OUTPUT_DIR}
                      ${WRF_PP_COMMANDS}
                      COMMENT "Preprocessing ${WRF_PP_TARGET_NAME}"
                      DEPENDS ${WRF_PP_DEPENDENCIES}
                      )

  add_custom_target(
                    ${WRF_PP_TARGET_NAME}
                    DEPENDS ${WRF_PP_OUTPUT}
                    )
endmacro()

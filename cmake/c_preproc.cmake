# WRF Macro for C preprocessing F files that are just... bad ifdef usage to say the least
macro( wrf_c_preproc_fortran )

  set( options        )
  set( oneValueArgs   TARGET_NAME SUFFIX PREFIX EXTENSION OUTPUT_DIR )
  set( multiValueArgs DEPENDENCIES INCLUDES SOURCES DEFINITIONS TARGET_SCOPE )

  cmake_parse_arguments(
                        WRF_PP_F
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  # Santitize input
  if ( DEFINED WRF_PP_F_TARGET_SCOPE )
    set( WRF_PP_F_TARGET_DIRECTORY TARGET_DIRECTORY ${WRF_PP_F_TARGET_SCOPE} )
  endif()

  set( WRF_PP_F_INCLUDES_FLAGS )
  foreach( WRF_PP_F_INC  ${WRF_PP_F_INCLUDES} )
    list( APPEND WRF_PP_F_INCLUDES_FLAGS -I${WRF_PP_F_INC} )
  endforeach()

  wrf_expand_definitions( 
                          RESULT_VAR   WRF_PP_F_DEFS
                          DEFINITIONS  ${WRF_PP_F_DEFINITIONS}
                          )

  # Generate compile command and file outputs
  set( WRF_PP_F_OUTPUT   )
  set( WRF_PP_F_COMMANDS )
  foreach( WRF_PP_F_SOURCE_FILE  ${WRF_PP_F_SOURCES} )
    get_filename_component( WRF_PP_F_INPUT_SOURCE           ${WRF_PP_F_SOURCE_FILE} REALPATH )
    get_filename_component( WRF_PP_F_INPUT_SOURCE_FILE_ONLY ${WRF_PP_F_SOURCE_FILE} NAME     )

    if ( ${WRF_PP_F_EXTENSION} MATCHES "^[.][a-z0-9]+$" )
      string( REGEX REPLACE "[.].*$" "${WRF_PP_F_EXTENSION}" WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_INPUT_SOURCE_FILE_ONLY} )
    else()
      # Default extension
      string( REGEX REPLACE "[.].*$" ".i" WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_INPUT_SOURCE_FILE_ONLY} )
    endif()

    set( WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_OUTPUT_DIR}/${WRF_PP_F_PREFIX}${WRF_PP_F_OUTPUT_FILE}${WRF_PP_F_SUFFIX} )

    list( 
          APPEND WRF_PP_F_COMMANDS 
          COMMAND ${CMAKE_C_PREPROCESSOR} ${CMAKE_C_PREPROCESSOR_FLAGS} ${WRF_PP_F_INPUT_SOURCE} ${WRF_PP_F_DEFS} ${WRF_PP_F_INCLUDES_FLAGS} > ${WRF_PP_F_OUTPUT_FILE}
          # Force check that they were made
          COMMAND ${CMAKE_COMMAND} -E compare_files ${WRF_PP_F_OUTPUT_FILE} ${WRF_PP_F_OUTPUT_FILE}
          )
    list( 
          APPEND WRF_PP_F_OUTPUT
          ${WRF_PP_F_OUTPUT_FILE}
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
    #                             ${WRF_PP_F_OUTPUT_FILE}
    #                             ${WRF_PP_F_TARGET_DIRECTORY}
    #                             PROPERTIES
    #                               GENERATED TRUE
    #                             )
    set_source_files_properties(
                                ${WRF_PP_F_OUTPUT_FILE}
                                DIRECTORY ${PROJECT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}
                                ${WRF_PP_F_TARGET_DIRECTORY}
                                PROPERTIES
                                  Fortran_PREPROCESS OFF
                                )
    # message( STATUS "File ${WRF_PP_F_SOURCE_FILE} will be preprocessed into ${WRF_PP_F_OUTPUT_FILE}" )

  endforeach()

  # Preprocess sources into a custom target
  add_custom_command(
                      OUTPUT ${WRF_PP_F_OUTPUT}
                      COMMAND ${CMAKE_COMMAND} -E  make_directory ${WRF_PP_F_OUTPUT_DIR}
                      ${WRF_PP_F_COMMANDS}
                      COMMENT "Preprocessing ${WRF_PP_F_TARGET_NAME}"
                      DEPENDS ${WRF_PP_F_DEPENDENCIES}
                      )

  add_custom_target(
                    ${WRF_PP_F_TARGET_NAME}
                    COMMENT "Building ${WRF_PP_F_TARGET_NAME}"
                    DEPENDS ${WRF_PP_F_OUTPUT}
                    )

endmacro()

# Helper macro to take current defintions and santize them with -D, compatible with generator expressions
# for use when definitions are needed at generation time for custom commands
macro( wrf_expand_definitions )
  set( options        )
  set( oneValueArgs   RESULT_VAR )
  set( multiValueArgs DEFINITIONS )

  cmake_parse_arguments(
                        WRF_EXP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  set( WRF_EXP_DEFS )
  foreach( WRF_EXP_DEF  ${WRF_EXP_DEFINITIONS} )
    if ( NOT ${WRF_EXP_DEF} MATCHES ".*-D.*" )
      # We have a generator expression, inject the -D correctly
      # THIS SHOULD ONLY BE USED FOR CONDITIONALLY APPLIED DEFINITIONS
      if ( ${WRF_EXP_DEF} MATCHES "^[$]<" )
        # Take advantage of the fact that a define is most likely not an expanded variable (i.e. starts with a-zA-Z, adjust if not)
        # preceeded by the defining generator expression syntax $<<condition>>:var or <condition>,var
        # Yes this is fragile but is probably more robust than the current code if you're relying on this macro :D
        string( REGEX REPLACE "(>:|,)([a-zA-Z])" "\\1-D\\2" WRF_EXP_DEF_SANITIZED ${WRF_EXP_DEF} )
        list( APPEND WRF_EXP_DEFS ${WRF_EXP_DEF_SANITIZED} )
      else()
        list( APPEND WRF_EXP_DEFS -D${WRF_EXP_DEF} )
      endif()
    endif()
    
  endforeach()

  set( ${WRF_EXP_RESULT_VAR} ${WRF_EXP_DEFS} )
endmacro()
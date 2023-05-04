# WRF Macro for preprocessing F files that are just... bad ifdef usage to say the least
macro( wrf_preproc_fortran )

  set( options        )
  set( oneValueArgs   TARGET_NAME SUFFIX PREFIX EXTENSION OUTPUT_DIR )
  set( multiValueArgs DEPENDENCIES INCLUDE_DIRECTORIES SOURCES DEFINITIONS GENERATED_SCOPE )

  cmake_parse_arguments(
                        WRF_PP_F
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  #!TODO Verify -o/-I/-E/-D are all compiler independent flags
  
  # Santitize input
  if ( DEFINED WRF_PP_F_GENERATED_SCOPE )
    set( WRF_PP_F_TARGET_DIRECTORY TARGET_DIRECTORY ${WRF_PP_F_GENERATED_SCOPE} )
  endif()

  set( WRF_PP_F_INCLUDES )
  foreach( WRF_PP_F_INC  ${WRF_PP_F_INCLUDE_DIRECTORIES} )
    list( APPEND WRF_PP_F_INCLUDES -I${WRF_PP_F_INC} )
  endforeach()

  set( WRF_PP_F_DEFS )
  foreach( WRF_PP_F_DEF  ${WRF_PP_F_DEFINITIONS} )
    if ( NOT ${WRF_PP_F_DEF} MATCHES ".*-D.*" )
      # We have a generator expression, inject the -D correctly
      if ( ${WRF_PP_F_DEF} MATCHES "^[$]<" )
        # Take advantage of the fact that the first time a generator expression ends to finally 
        # name the define it has the first ">:" then unexpanded characters (hopefully)
        # Yes this is fragile but is probably more robust than the current code if you're relying on this macro :D
        string( REGEX REPLACE "^(.*>:)([a-zA-Z])" "\\1-D\\2" WRF_PP_F_DEF_SANITIZED ${WRF_PP_F_DEF} )
        list( APPEND WRF_PP_F_DEFS ${WRF_PP_F_DEF_SANITIZED} )
      else()
        list( APPEND WRF_PP_F_DEFS -D${WRF_PP_F_DEF} )
      endif()
    endif()
    
  endforeach()


  # Generate compile command and file outputs
  set( WRF_PP_F_OUTPUT   )
  set( WRF_PP_F_COMMANDS )
  foreach( WRF_PP_F_SOURCE_FILE  ${WRF_PP_F_SOURCES} )
    if ( ${WRF_PP_F_EXTENSION} MATCHES "^[.][a-z0-9]+$" )
      string( REGEX REPLACE "[.].*$" "${WRF_PP_F_EXTENSION}" WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_SOURCE_FILE} )
    else()
      # Default extension
      string( REGEX REPLACE "[.].*$" ".i" WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_SOURCE_FILE} )
    endif()

    set( WRF_PP_F_OUTPUT_FILE ${WRF_PP_F_OUTPUT_DIR}/${WRF_PP_F_PREFIX}${WRF_PP_F_OUTPUT_FILE}${WRF_PP_F_SUFFIX} )
    get_filename_component( WRF_PP_F_INPUT_SOURCE ${WRF_PP_F_SOURCE_FILE} REALPATH )

    list( 
          APPEND WRF_PP_F_COMMANDS 
          COMMAND ${CMAKE_Fortran_COMPILER} -E ${WRF_PP_F_INPUT_SOURCE} ${WRF_PP_F_DEFS} ${WRF_PP_F_INCLUDES} > ${WRF_PP_F_OUTPUT_FILE}
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

    message( STATUS "File ${WRF_PP_F_SOURCE_FILE} will be preprocessed into ${WRF_PP_F_OUTPUT_FILE}" )

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
                    DEPENDS ${WRF_PP_F_OUTPUT}
                    )

endmacro()
# WRF Macro for adding configuration checks from source file, default is fortran
# https://cmake.org/cmake/help/latest/module/CheckFortranSourceCompiles.html
# https://github.com/ufs-community/ufs-weather-model/issues/132
include( CheckFortranSourceRuns )
include( CheckFortranSourceCompiles )
include( CheckCSourceRuns )
include( CheckCSourceCompiles )
include( CheckCXXSourceRuns )
include( CheckCXXSourceCompiles )

macro( wrf_conf_check )

  set( options        QUIET RUN REQUIRED )
  set( oneValueArgs   RESULT_VAR EXTENSION FAIL_REGEX SOURCE MESSAGE SOURCE_TYPE )
  set( multiValueArgs ADDITIONAL_FLAGS ADDITIONAL_DEFINITIONS ADDITIONAL_INCLUDES ADDITIONAL_LINK_OPTIONS ADDITIONAL_LIBRARIES )

  cmake_parse_arguments(
                        WRF_CFG
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  get_filename_component( WRF_CFG_SOURCE_FILE ${WRF_CFG_SOURCE} REALPATH )  
  file( READ ${WRF_CFG_SOURCE_FILE} WRF_CFG_CODE )

  # Santize for newlines
  string( REPLACE "\\n" "\\\\n" WRF_CFG_CODE "${WRF_CFG_CODE}" )

  if ( NOT DEFINED WRF_CFG_SOURCE_TYPE )
    set( WRF_CFG_SOURCE_TYPE fortran )
  endif()

  if ( DEFINED WRF_CFG_FAIL_REGEX )
    if ( DEFINED WRF_CFG_RUN )
      message( WARNING "wrf_conf_check: FAIL_REGEX ignored when running check" )
    else()
      set( WRF_CFG_FAIL_REGEX FAIL_REGEX ${WRF_CFG_FAIL_REGEX} )
    endif()
  endif()

  if ( DEFINED WRF_CFG_EXTENSION )
    set( WRF_CFG_EXTENSION SRC_EXT ${WRF_CFG_EXTENSION} )
  endif()

  # Additional options
  if ( DEFINED WRF_CFG_QUIET AND ${WRF_CFG_QUIET} )
    set( CMAKE_REQUIRED_QUIET ${WRF_CFG_QUIET} )
  endif()

  if ( DEFINED WRF_CFG_ADDITIONAL_FLAGS )
    set( CMAKE_REQUIRED_FLAGS ${WRF_CFG_ADDITIONAL_FLAGS} )
  endif()

  if ( DEFINED WRF_CFG_ADDITIONAL_DEFINITIONS )
    set( CMAKE_REQUIRED_DEFINITIONS ${WRF_CFG_ADDITIONAL_DEFINITIONS} )
  endif()

  if ( DEFINED WRF_CFG_ADDITIONAL_INCLUDES )
    set( CMAKE_REQUIRED_INCLUDES ${WRF_CFG_ADDITIONAL_INCLUDES} )
  endif()

  if ( DEFINED WRF_CFG_ADDITIONAL_LINK_OPTIONS )
    set( CMAKE_REQUIRED_LINK_OPTIONS ${WRF_CFG_ADDITIONAL_LINK_OPTIONS} )
  endif()

  if ( DEFINED WRF_CFG_ADDITIONAL_LIBRARIES )
    set( CMAKE_REQUIRED_LIBRARIES ${WRF_CFG_ADDITIONAL_LIBRARIES} )
  endif()

  string( TOLOWER "${WRF_CFG_SOURCE_TYPE}" WRF_CFG_SOURCE_TYPE )
  if ( DEFINED WRF_CFG_RUN )
    if ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "fortran" )
      check_fortran_source_runs(
                                "${WRF_CFG_CODE}"
                                ${WRF_CFG_RESULT_VAR}
                                ${WRF_CFG_FAIL_REGEX}
                                ${WRF_CFG_EXTENSION}
                                )
    elseif ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "c" )
      check_c_source_runs(
                          "${WRF_CFG_CODE}"
                          ${WRF_CFG_RESULT_VAR}
                          ${WRF_CFG_FAIL_REGEX}
                          ${WRF_CFG_EXTENSION}
                          )
    elseif ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "cpp" )
      check_cpp_source_runs(
                            "${WRF_CFG_CODE}"
                            ${WRF_CFG_RESULT_VAR}
                            ${WRF_CFG_FAIL_REGEX}
                            ${WRF_CFG_EXTENSION}
                            )
    endif()
  else()
    if ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "fortran" )
      check_fortran_source_compiles(
                                    "${WRF_CFG_CODE}"
                                    ${WRF_CFG_RESULT_VAR}
                                    ${WRF_CFG_EXTENSION}
                                    )
    elseif ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "c" )
      check_c_source_compiles(
                              "${WRF_CFG_CODE}"
                              ${WRF_CFG_RESULT_VAR}
                              ${WRF_CFG_EXTENSION}
                              )
    elseif ( ${WRF_CFG_SOURCE_TYPE} STREQUAL "cpp" )
      check_cpp_source_compiles(
                                "${WRF_CFG_CODE}"
                                ${WRF_CFG_RESULT_VAR}
                                ${WRF_CFG_EXTENSION}
                                )
    endif()
  endif()

  # If it failed - note that since this is a run/compile test we expect pass/true 
  # to just proceed as normal, but if failure we should do something about it
  if ( NOT ( DEFINED ${WRF_CFG_RESULT_VAR} AND "${${WRF_CFG_RESULT_VAR}}" ) )
    set( WRF_CFG_MSG_TYPE STATUS )
    if ( DEFINED WRF_CFG_REQUIRED AND ${WRF_CFG_REQUIRED} )
      set( WRF_CFG_MSG_TYPE FATAL_ERROR )
    endif()
    
    if ( DEFINED WRF_CFG_MESSAGE )
      message( ${WRF_CFG_MSG_TYPE} "${WRF_CFG_MESSAGE}" )
    else()
      message( ${WRF_CFG_MSG_TYPE} "${WRF_CFG_RESULT_VAR} marked as required, check failed" )
    endif()
  endif()

endmacro()



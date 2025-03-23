# WRF Macro for adding configuration checks from source file, default is fortran
# https://cmake.org/cmake/help/latest/module/CheckFortranSourceCompiles.html
# https://github.com/ufs-community/ufs-weather-model/issues/132
# include( CheckFortranSourceRuns )
# include( CheckFortranSourceCompiles )
# include( CheckCSourceRuns )
# include( CheckCSourceCompiles )
# include( CheckCXXSourceRuns )
# include( CheckCXXSourceCompiles )

function( wrf_conf_check )

  set( options        QUIET RUN REQUIRED )
  set( oneValueArgs   RESULT_VAR MESSAGE )
  set( multiValueArgs SOURCES OPTIONS )

  cmake_parse_arguments(
                        WRF_CFG
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  if ( NOT DEFINED WRF_CFG_BINDIR )
    set( WRF_CFG_BINDIR ${CMAKE_CURRENT_BINARY_DIR}/confcheck/${WRF_CFG_RESULT_VAR}/ )
  endif()

  message( STATUS "Performing Check ${WRF_CFG_RESULT_VAR}" )

  if ( DEFINED WRF_CFG_RUN )
    try_run( 
            ${WRF_CFG_RESULT_VAR}
            WRF_CFG_COMPILE_RESULT_VAR
            ${WRF_CFG_BINDIR}
            ${WRF_CFG_SOURCES}
            ${WRF_CFG_OPTIONS}
            )
    if ( ${WRF_CFG_COMPILE_RESULT_VAR} )
      # Did it run successfully
      if ( ${${WRF_CFG_RESULT_VAR}} EQUAL 0 )
        set( ${WRF_CFG_RESULT_VAR} TRUE )
      endif()
    else()
      set( ${WRF_CFG_RESULT_VAR} FALSE )
    endif()
  else()
      try_compile(
                  ${WRF_CFG_RESULT_VAR}
                  ${WRF_CFG_BINDIR}
                  SOURCES ${WRF_CFG_SOURCES}
                  ${WRF_CFG_OPTIONS}
                  )
  endif()

  # If it failed - note that since this is a run/compile test we expect pass/true 
  # to just proceed as normal, but if failure we should do something about it
  if ( NOT ( DEFINED ${WRF_CFG_RESULT_VAR} AND "${${WRF_CFG_RESULT_VAR}}" ) )
    message( STATUS "Performing Check ${WRF_CFG_RESULT_VAR} - Failure" )
    set( WRF_CFG_MSG_TYPE STATUS )
    if ( DEFINED WRF_CFG_REQUIRED AND ${WRF_CFG_REQUIRED} )
      set( WRF_CFG_MSG_TYPE FATAL_ERROR )
    endif()
    
    if ( DEFINED WRF_CFG_MESSAGE )
      message( ${WRF_CFG_MSG_TYPE} "${WRF_CFG_MESSAGE}" )
    else()
      message( ${WRF_CFG_MSG_TYPE} "${WRF_CFG_RESULT_VAR} marked as required, check failed" )
    endif()
  else()
    message( STATUS "Performing Check ${WRF_CFG_RESULT_VAR} - Success" )
  endif()

  set( ${WRF_CFG_RESULT_VAR} ${${WRF_CFG_RESULT_VAR}} PARENT_SCOPE )

endfunction()



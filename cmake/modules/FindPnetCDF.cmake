# Find pnetcdf
# Eventually replace with PnetCDF's actual config if using that
# Once found this file will define:
#  PnetCDF_FOUND - System has pnetcdf
#  PnetCDF_INCLUDE_DIRS - The pnetcdf include directories
#  PnetCDF_LIBRARIES - The libraries needed to use pnetcdf
#  PnetCDF_DEFINITIONS - Compiler switches required for using pnetcdf

# list( REMOVE_ITEM CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )
# find_package( PnetCDF )
# list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )


# exit early if we don't even need to be here
if ( PnetCDF_FOUND )
  return()
endif()

# Use pnetcdf-config
find_program( 
                PNETCDF_PROGRAM
                pnetcdf-config
                QUIET
                )

if ( ${PNETCDF_PROGRAM} MATCHES "-NOTFOUND$" )
  message( STATUS "No pnetcdf-config found" )
else()
  message( STATUS "Found PNETCDF_PROGRAM : ${PNETCDF_PROGRAM}" )

  execute_process( COMMAND ${PNETCDF_PROGRAM} --includedir   OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_INCLUDE_DIR )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --prefix       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_PREFIX )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --libdir       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_LIBDIR   )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --version      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_VERSION_RAW )

  # These are what PnetCDF was built with
  execute_process( COMMAND ${PNETCDF_PROGRAM} --ldflags      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_LDFLAGS )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --libs         OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_BUILD_LIBS )

  # Sanitize version
  string( REPLACE " " ";" PnetCDF_VERSION_LIST ${PnetCDF_VERSION_RAW} )
  list( GET PnetCDF_VERSION_LIST -1 PnetCDF_VERSION )

  # These do not pull all options available from nc-config out, but rather mirrors what is available from PnetCDFConfig.cmake.in
  set(
      PnetCDF_QUERY_YES_OPTIONS
      has-c++
      has-fortran
      netcdf4
      adios
      )

  foreach( PN_QUERY ${PnetCDF_QUERY_YES_OPTIONS} )
    string( REPLACE "has-" "" PN_QUERY_FILTERED ${PN_QUERY} )
    execute_process( COMMAND ${PNETCDF_PROGRAM} --${PN_QUERY} OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PnetCDF_${PN_QUERY_FILTERED}_LOWERCASE )
    if ( "${PnetCDF_${PN_QUERY_FILTERED}_LOWERCASE}" STREQUAL "" )
      # might be empty
      set( PnetCDF_${PN_QUERY_FILTERED}_LOWERCASE no )
    endif()

    string( TOUPPER ${PN_QUERY_FILTERED}                      PN_QUERY_UPPERCASE )
    string( TOUPPER ${PnetCDF_${PN_QUERY_FILTERED}_LOWERCASE} PN_ANSWER_UPPERCASE )

    set( PN_QUERY_YES_RESPONSE YES ENABLED )
    list( FIND PN_QUERY_YES_RESPONSE ${PN_ANSWER_UPPERCASE} PN_FIND_ANSWER )
    if ( ${PN_FIND_ANSWER} EQUAL -1 )
      set( PN_ANSWER_UPPERCASE NO )
    else()
      set( PN_ANSWER_UPPERCASE YES )
    endif()

    # Convert to PnetCDF_HAS_* = YES/NO - Note this cannot be generator expression if you want to use it during configuration time
    set( PnetCDF_HAS_${PN_QUERY_UPPERCASE} ${PN_ANSWER_UPPERCASE} )
  endforeach()

  # Because we may need this for in-situ manual preprocessing do not use genex
  set( PnetCDF_INCLUDE_DIRS ${PnetCDF_INCLUDE_DIR} )

  # Find the actual name of the library
  find_library(
                PnetCDF_LIBRARY
                pnetcdf
                PATHS ${PnetCDF_LIBDIR}
                NO_DEFAULT_PATH
                )

  set( PnetCDF_DEFINITIONS  )
  set( PnetCDF_LIBRARIES ${PnetCDF_LIBRARY} )
endif()

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set PnetCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(
                                  PnetCDF
                                  FOUND_VAR PnetCDF_FOUND
                                  REQUIRED_VARS
                                    PnetCDF_INCLUDE_DIRS
                                    PnetCDF_LIBRARIES
                                    PnetCDF_VERSION
                                  VERSION_VAR PnetCDF_VERSION
                                  HANDLE_VERSION_RANGE
                                )

if ( PnetCDF_FOUND AND NOT TARGET PnetCDF::pnetcdf )
  if ( ${PnetCDF_HAS_NETCDF4} )
    message( STATUS "Will try to find netCDF from PnetCDF ldflags..." )
    list( APPEND CMAKE_MESSAGE_INDENT "  " )
    if ( NOT "${PnetCDF_LDFLAGS}" STREQUAL "" )
      # Find prefix
      message( STATUS "Looking for netCDF location provided by ${PNETCDF_PROGRAM}")
      foreach( PNETCDF_BUILD_LINK_DIR ${PnetCDF_LDFLAGS} )
        string( REGEX MATCH "^-L([^ ]*)" PN_LIBRARY_LINK_LOCATION_TMP ${PNETCDF_BUILD_LINK_DIR} )
        if ( NOT "${PN_LIBRARY_LINK_LOCATION_TMP}" STREQUAL "" )
          message( STATUS "Searching ${PN_LIBRARY_LINK_LOCATION_TMP}..." )
          set( PN_LIBRARY_LINK_DIR ${CMAKE_MATCH_1} )
          find_library( 
                        netCDF_LIBRARY_FOR_PNETCDF
                        netcdf
                        PATHS ${PN_LIBRARY_LINK_DIR}
                        NO_DEFAULT_PATH
                        )
          if ( NOT "${netCDF_LIBRARY_FOR_PNETCDF}" MATCHES "-NOTFOUND$" )
            string( REGEX REPLACE "/(lib|lib64)$" "" netCDF_ROOT ${PN_LIBRARY_LINK_DIR} )
            message( STATUS "Using ${netCDF_ROOT} as prefix for netCDF requirements of PnetCDF" )
            break()
          endif() # found netcdf location
        endif() # found valid link library location
      endforeach() # looped over ldflags built with
    endif() # see if we should use the built-with ldflags
    list( POP_BACK CMAKE_MESSAGE_INDENT )
    find_package( netCDF REQUIRED )
  endif()

  set( PNETCDF_LINK_INTERFACE_LANGUAGES )
  list( APPEND PNETCDF_LINK_INTERFACE_LANGUAGES C )
  if ( ${PnetCDF_HAS_CXX} )
    list( APPEND PNETCDF_LINK_INTERFACE_LANGUAGES CXX )
  endif()
  if( ${PnetCDF_HAS_FORTRAN} )
    list( APPEND PNETCDF_LINK_INTERFACE_LANGUAGES Fortran )
  endif()
  add_library( PnetCDF::pnetcdf UNKNOWN IMPORTED )
  set_target_properties(
                        PnetCDF::pnetcdf
                        PROPERTIES
                          IMPORTED_LOCATION                   "${PnetCDF_LIBRARY}"
                          IMPORTED_LINK_INTERFACE_LANGUAGES   "${PNETCDF_LINK_INTERFACE_LANGUAGES}"
                          INTERFACE_INCLUDE_DIRECTORIES       "${PnetCDF_INCLUDE_DIRS}"
                        )
  if ( ${PnetCDF_HAS_NETCDF4} )
    target_link_libraries( PnetCDF::pnetcdf INTERFACE netCDF::netcdf )
  endif()
endif()

mark_as_advanced( PnetCDF_FLIBS PnetCDF_PREFIX PnetCDF_LIBRARY_DIR )

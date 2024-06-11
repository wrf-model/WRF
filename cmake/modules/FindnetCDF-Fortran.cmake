# Find netcdf
# Eventually replace with netCDF-Fortran's actual config if using that
# Once found this file will define:
#  netCDF-Fortran_FOUND - System has netcdf
#  netCDF-Fortran_INCLUDE_DIRS - The netcdf include directories
#  netCDF-Fortran_LIBRARIES - The libraries needed to use netcdf
#  netCDF-Fortran_DEFINITIONS - Compiler switches required for using netcdf

# list( REMOVE_ITEM CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )
# find_package( netCDF-Fortran )
# list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )


# exit early if we don't even need to be here
if ( netCDF-Fortran_FOUND )
  return()
endif()

###############################################################################
# First try to find using netCDF-Fortran native cmake build
# TODO : Enable this when netCDF-Fortran native cmake build works well as an imported package
# find_package( netCDF-Fortran CONFIG )
# if ( netCDF-Fortran_FOUND )
#   message( STATUS "Found netCDF-Fortran through native cmake build" )
#   return()
# endif()
###############################################################################

# else
# Use nf-config
find_program( 
                NETCDF-FORTRAN_PROGRAM
                nf-config
                QUIET
                )

if ( ${NETCDF-FORTRAN_PROGRAM} MATCHES "-NOTFOUND$" )
  message( STATUS "No nf-config found" )
else()
  message( STATUS "Found NETCDF-FORTRAN_PROGRAM : ${NETCDF-FORTRAN_PROGRAM}" )

  execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --includedir   OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_INCLUDE_DIR )
  execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --prefix       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_PREFIX )
  execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --flibs        OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_FLIBS   )
  execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --version      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_VERSION_RAW )

  # check for large file support
  find_file( netCDF-Fortran_INCLUDE_FILE netcdf.inc ${netCDF-Fortran_INCLUDE_DIR} )
  file( READ ${netCDF-Fortran_INCLUDE_FILE} netCDF-Fortran_INCLUDE_FILE_STR )
  string( FIND "${netCDF-Fortran_INCLUDE_FILE_STR}" "nf_format_64bit_data" netCDF-Fortran_LARGE_FILE_SUPPORT_FOUND )
  if ( ${netCDF-Fortran_LARGE_FILE_SUPPORT_FOUND} EQUAL -1 )
    set( netCDF-Fortran_LARGE_FILE_SUPPORT "NO" )
  else()
    set( netCDF-Fortran_LARGE_FILE_SUPPORT "YES" )
  endif()

  # Sanitize version
  string( REPLACE " " ";" netCDF-Fortran_VERSION_LIST ${netCDF-Fortran_VERSION_RAW} )
  list( GET netCDF-Fortran_VERSION_LIST -1 netCDF-Fortran_VERSION )

  # These do not pull all options available from nc-config out, but rather mirrors what is available from netCDFConfig.cmake.in
  set(
      netCDF-Fortran_QUERY_YES_OPTIONS
      dap
      nc2
      nc4
      f90
      f03
      )

  foreach( NF_QUERY ${netCDF-Fortran_QUERY_YES_OPTIONS} )
    execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --has-${NF_QUERY} OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_${NF_QUERY}_LOWERCASE )
    if ( NOT "${netCDF-Fortran_${NF_QUERY}_LOWERCASE}" )
      # might be empty
      set( netCDF-Fortran_${NF_QUERY}_LOWERCASE no )
    endif()
    string( TOUPPER ${NF_QUERY}                     NF_QUERY_UPPERCASE )
    string( TOUPPER ${netCDF-Fortran_${NF_QUERY}_LOWERCASE} NF_ANSWER_UPPERCASE )
    # Convert to netCDF-Fortran_HAS_* = YES/NO - Note this cannot be generator expression if you want to use it during configuration time
    set( netCDF-Fortran_HAS_${NF_QUERY_UPPERCASE} ${NF_ANSWER_UPPERCASE} )
  endforeach()


  # A bug in previous netcdf-fortran cmake builds, extract from flibs
  string( REGEX MATCH "^-L([^ ]*)" netCDF-Fortran_LIBRARY_LINK_LOCATION ${netCDF-Fortran_FLIBS} )
  set( netCDF-Fortran_LIBRARY_DIR ${CMAKE_MATCH_1} )

  set( netCDF-Fortran_DEFINITIONS  )
  set( netCDF-Fortran_LIBRARIES
      $<$<LINK_LANGUAGE:Fortran>:${netCDF-Fortran_FLIBS}>
      )

  # Because we may need this for in-situ manual preprocessing do not use genex
  set( netCDF-Fortran_INCLUDE_DIRS ${netCDF-Fortran_INCLUDE_DIR} )

  # Find the actual name of the library
  find_library(
                netCDF-Fortran_LIBRARY
                netcdff
                PATHS ${netCDF-Fortran_LIBRARY_DIR}
                NO_DEFAULT_PATH
                )
endif()

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set netCDF-Fortran_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(
                                  netCDF-Fortran
                                  FOUND_VAR netCDF-Fortran_FOUND
                                  REQUIRED_VARS
                                    netCDF-Fortran_INCLUDE_DIRS
                                    netCDF-Fortran_LIBRARIES
                                    netCDF-Fortran_VERSION
                                  VERSION_VAR netCDF-Fortran_VERSION
                                  HANDLE_VERSION_RANGE
                                )

# Note that the name of the target is the target library name as specified by the netCDF cmake build,
# NOT the netCDF repository name, I've kept this consistent to the provided netCDF builds rather
# than the convention of *_<LANG> to specify multiple components. This also helps account for the
# fact that the netCDF langauge-specific projects are separate projects
if ( netCDF-Fortran_FOUND AND NOT TARGET netCDF::netcdff )
  find_package( netCDF REQUIRED )

  add_library( netCDF::netcdff UNKNOWN IMPORTED )
  set_target_properties(
                        netCDF::netcdff
                        PROPERTIES
                          IMPORTED_LOCATION                   "${netCDF-Fortran_LIBRARY}"
                          IMPORTED_LINK_INTERFACE_LANGUAGES   Fortran
                          INTERFACE_INCLUDE_DIRECTORIES      "${netCDF-Fortran_INCLUDE_DIRS}"
                        )
  target_link_libraries( netCDF::netcdff INTERFACE netCDF::netcdf )
endif()

mark_as_advanced( netCDF-Fortran_FLIBS netCDF-Fortran_PREFIX netCDF-Fortran_LIBRARY_DIR )

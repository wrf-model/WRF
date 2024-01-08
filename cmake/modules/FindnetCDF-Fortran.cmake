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
  execute_process( COMMAND ${NETCDF-FORTRAN_PROGRAM} --has-nc4      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF-Fortran_NC4_YES )

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

  # Convert to YES/NO - Note cannot be generator expression if you want to use it during configuration time
  string( TOUPPER ${netCDF-Fortran_NC4_YES}      netCDF-Fortran_NC4      )

  set( netCDF-Fortran_DEFINITIONS  )
  set( netCDF-Fortran_LIBRARY_DIR  ${netCDF-Fortran_PREFIX}/lib )

  set( netCDF-Fortran_LIBRARIES
      $<$<LINK_LANGUAGE:Fortran>:${netCDF-Fortran_FLIBS}>
      )

  # Because we may need this for in-situ manual preprocessing do not use genex
  set( netCDF-Fortran_INCLUDE_DIRS ${netCDF-Fortran_INCLUDE_DIR} )
endif()

find_package( PkgConfig )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set netCDF-Fortran_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(
                                  netCDF-Fortran  DEFAULT_MSG
                                  netCDF-Fortran_INCLUDE_DIRS
                                  netCDF-Fortran_FLIBS
                                  netCDF-Fortran_VERSION
                                  )

mark_as_advanced( netCDF-Fortran_FLIBS netCDF-Fortran_PREFIX netCDF-Fortran_LIBRARY_DIR )
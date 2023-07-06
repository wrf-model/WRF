# Find netcdf
# Eventually replace with netCDF's actual config if using that
# Once found this file will define:
#  netCDF_FOUND - System has netcdf
#  netCDF_INCLUDE_DIRS - The netcdf include directories
#  netCDF_LIBRARIES - The libraries needed to use netcdf
#  netCDF_DEFINITIONS - Compiler switches required for using netcdf

# list( REMOVE_ITEM CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )
# find_package( netCDF )
# list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )

# Use nc-config
find_program( 
                NETCDF_PROGRAM
                nc-config
                QUIET
                )

if ( ${NETCDF_PROGRAM} MATCHES "-NOTFOUND$" )
  message( STATUS "No nc-config found" )
else()
  message( STATUS "Found NETCDF_PROGRAM : ${NETCDF_PROGRAM}" )

  execute_process( COMMAND ${NETCDF_PROGRAM} --includedir   OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_INCLUDE_DIR )
  execute_process( COMMAND ${NETCDF_PROGRAM} --libdir       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_LIBRARY_DIR )
  execute_process( COMMAND ${NETCDF_PROGRAM} --prefix       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_PREFIX )
  execute_process( COMMAND ${NETCDF_PROGRAM} --libs         OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_CLIBS   )
  execute_process( COMMAND ${NETCDF_PROGRAM} --cxx4libs     OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_CXXLIBS )
  execute_process( COMMAND ${NETCDF_PROGRAM} --flibs        OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_FLIBS   )
  execute_process( COMMAND ${NETCDF_PROGRAM} --version      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_VERSION_RAW )
  execute_process( COMMAND ${NETCDF_PROGRAM} --has-nc4      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_NC4_YES )
  execute_process( COMMAND ${NETCDF_PROGRAM} --has-pnetcdf  OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_PNETCDF_YES )
  execute_process( COMMAND ${NETCDF_PROGRAM} --has-parallel OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_PARALLEL_YES )

  # check for large file support
  find_file( netCDF_INCLUDE_FILE netcdf.inc ${netCDF_INCLUDE_DIR} )
  file( READ ${netCDF_INCLUDE_FILE} netCDF_INCLUDE_FILE_STR )
  string( FIND "${netCDF_INCLUDE_FILE_STR}" "nf_format_64bit" netCDF_LARGE_FILE_SUPPORT_FOUND )
  if ( ${netCDF_LARGE_FILE_SUPPORT_FOUND} EQUAL -1 )
    set( netCDF_LARGE_FILE_SUPPORT "NO" )
  else()
    set( netCDF_LARGE_FILE_SUPPORT "YES" )
  endif()

  # Sanitize version
  string( REPLACE " " ";" netCDF_VERSION_LIST ${netCDF_VERSION_RAW} )
  list( GET netCDF_VERSION_LIST -1 netCDF_VERSION )

  # Convert to YES/NO - Note cannot be generator expression if you want to use it during configuration time
  string( TOUPPER ${netCDF_NC4_YES}      netCDF_NC4      )
  string( TOUPPER ${netCDF_PNETCDF_YES}  netCDF_PNETCDF  )
  string( TOUPPER ${netCDF_PARALLEL_YES} netCDF_PARALLEL )

  set( netCDF_DEFINITIONS  )

  set( netCDF_LIBRARIES
      $<$<LINK_LANGUAGE:C>:${netCDF_CLIBS}>
      $<$<LINK_LANGUAGE:CXX>:${netCDF_CXXLIBS}> 
      $<$<LINK_LANGUAGE:Fortran>:${netCDF_FLIBS}>
      )
  set( netCDF_INCLUDE_DIRS ${netCDF_INCLUDE_DIR} )
endif()

find_package( PkgConfig )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set netCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args( netCDF  DEFAULT_MSG
                                  netCDF_INCLUDE_DIRS
                                  netCDF_LIBRARY_DIR
                                  netCDF_CLIBS
                                  netCDF_CXXLIBS
                                  netCDF_FLIBS
                                  netCDF_VERSION
                                  )

mark_as_advanced( netCDF_CLIBS netCDF_CXXLIBS netCDF_FLIBS netCDF_PREFIX netCDF_LIBRARY_DIR )
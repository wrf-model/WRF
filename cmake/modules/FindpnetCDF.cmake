# Find pnetcdf
# Eventually replace with pnetCDF's actual config if using that
# Once found this file will define:
#  pnetCDF_FOUND - System has pnetcdf
#  pnetCDF_INCLUDE_DIRS - The pnetcdf include directories
#  pnetCDF_LIBRARIES - The libraries needed to use pnetcdf
#  pnetCDF_DEFINITIONS - Compiler switches required for using pnetcdf

# list( REMOVE_ITEM CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )
# find_package( pnetCDF )
# list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR} )

# Use pnetcdf-config
find_program( 
                PNETCDF_PROGRAM
                pnetcdf-config
                QUIET
                )

if ( ${PNETCDF_PROGRAM} MATCHES "-NOTFOUND$" )
  message( STATUS "No pnetcdf-config found : ${PNETCDF_PROGRAM}" )
else()

  execute_process( COMMAND ${PNETCDF_PROGRAM} --includedir   OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_INCLUDE_DIR  )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --libdir       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_LIBRARY_DIR )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --prefix       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_PREFIX )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --version      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_VERSION_RAW  )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --netcdf4      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_NC4_ENABLED  )

  execute_process( COMMAND ${PNETCDF_PROGRAM} --has-c++      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_CXX_YES      )
  execute_process( COMMAND ${PNETCDF_PROGRAM} --has-fortran  OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pnetCDF_FORTRAN_YES  )



  # Sanitize version
  string( REPLACE " " ";" pnetCDF_VERSION_LIST ${pnetCDF_VERSION_RAW} )
  list( GET pnetCDF_VERSION_LIST -1 pnetCDF_VERSION )

  # Note that pnetCDF has decided to change things up and use "disabled" instead of "yes/no"
  string( TOLOWER ${pnetCDF_NC4_ENABLED} pnetCDF_NC4_ENABLED )
  if ( ${pnetCDF_NC4_ENABLED} STREQUAL "enabled" )
    set( pnetCDF_NC4 "YES" )
  else()
    set( pnetCDF_NC4 "NO" )
  endif()

  string( TOUPPER ${pnetCDF_CXX_YES}      pnetCDF_CXX      )
  string( TOUPPER ${pnetCDF_FORTRAN_YES}  pnetCDF_FORTRAN  )


  set( pnetCDF_DEFINITIONS  )

  # Find libraries
  find_library(
                pnetCDF_LIBRARY
                NAMES pnetcdf
                # Hints before PATHS
                HINTS ${pnetCDF_LIBRARY_DIR}
                NO_DEFAULT_PATH
              )


  set( pnetCDF_LIBRARIES
      $<$<LINK_LANGUAGE:C>:${pnetCDF_LIBRARY}>
      $<$<BOOL:${pnetCDF_CXX}>:$<LINK_LANGUAGE:CXX>:${pnetCDF_LIBRARY}>
      $<$<BOOL:${pnetCDF_FORTRAN}>:$<LINK_LANGUAGE:Fortran>:${pnetCDF_LIBRARY}>
      )
  set( pnetCDF_INCLUDE_DIRS ${pnetCDF_INCLUDE_DIR} )
endif()
find_package( PkgConfig )
include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set pnetCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args( pnetCDF  DEFAULT_MSG
                                  pnetCDF_INCLUDE_DIRS
                                  pnetCDF_LIBRARIES
                                  pnetCDF_VERSION
                                  )

# mark_as_advanced( pnetCDF_CLIBS pnetCDF_CXXLIBS pnetCDF_FLIBS )
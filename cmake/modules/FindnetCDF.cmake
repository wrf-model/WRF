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
execute_process( COMMAND nc-config --includedir  OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_INCLUDE_DIR )
execute_process( COMMAND nc-config --libs        OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_CLIBS   )
execute_process( COMMAND nc-config --cxx4libs    OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_CXXLIBS )
execute_process( COMMAND nc-config --flibs       OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_FLIBS   )
execute_process( COMMAND nc-config --version     OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_VERSION_RAW )
execute_process( COMMAND nc-config --has-nc4     OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_NC4_YES )
execute_process( COMMAND nc-config --has-pnetcdf OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_PNETCDF_YES )

# Sanitize version
string( REPLACE " " ";" netCDF_VERSION_LIST ${netCDF_VERSION_RAW} )
list( GET netCDF_VERSION_LIST -1 netCDF_VERSION )

# Convert yes/no to 0/1 bool
set( netCDF_NC4 $<BOOL:${netCDF_NC4_YES}> )
set( netCDF_PNETCDF $<BOOL:${netCDF_PNETCDF_YES}> )


find_package( PkgConfig )
set( netCDF_DEFINITIONS  )



include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set netCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args( netCDF  DEFAULT_MSG
                                  netCDF_INCLUDE_DIR
                                  netCDF_CLIBS
                                  netCDF_CXXLIBS
                                  netCDF_FLIBS
                                  netCDF_VERSION
                                  )

mark_as_advanced( netCDF_INCLUDE_DIR netCDF_CLIBS netCDF_CXXLIBS netCDF_FLIBS )

set( netCDF_LIBRARIES
    $<$<LINK_LANGUAGE:C>:${netCDF_CLIBS}>
    $<$<LINK_LANGUAGE:CXX>:${netCDF_CXXLIBS}> 
    $<$<LINK_LANGUAGE:Fortran>:${netCDF_FLIBS}>
    )
set( netCDF_INCLUDE_DIRS ${netCDF_INCLUDE_DIR} )

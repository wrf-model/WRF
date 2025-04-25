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

# exit early if we don't even need to be here
if ( netCDF_FOUND )
  return()
endif()


###############################################################################
# First try to find using netCDF native cmake build
# TODO : Enable this when netCDF native cmake build works well as an imported package
# find_package( netCDF CONFIG )
# if ( netCDF_FOUND )
#   message( STATUS "Found netCDF through native cmake build" )
#   return()
# endif()
###############################################################################


# else
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
  execute_process( COMMAND ${NETCDF_PROGRAM} --version      OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_VERSION_RAW )

  # These do not pull all options available from nc-config out, but rather mirrors what is available from netCDFConfig.cmake.in
  set(
      netCDF_QUERY_YES_OPTIONS
      dap
      dap2
      dap4
      nc2
      nc4
      hdf5
      hdf4
      pnetcdf
      parallel

      # These are not part of the config but used in this to provide the properly linking
      szlib
      zstd
      )

  foreach( NC_QUERY ${netCDF_QUERY_YES_OPTIONS} )
    execute_process( COMMAND ${NETCDF_PROGRAM} --has-${NC_QUERY} OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE netCDF_${NC_QUERY}_LOWERCASE )
    if ( NOT "${netCDF-Fortran_${NF_QUERY}_LOWERCASE}" )
      # might be empty
      set( netCDF-Fortran_${NF_QUERY}_LOWERCASE no )
    endif()
    string( TOUPPER ${NC_QUERY}                     NC_QUERY_UPPERCASE )
    string( TOUPPER ${netCDF_${NC_QUERY}_LOWERCASE} NC_ANSWER_UPPERCASE )
    # Convert to netCDF_HAS_* = YES/NO - Note this cannot be generator expression if you want to use it during configuration time
    set( netCDF_HAS_${NC_QUERY_UPPERCASE} ${NC_ANSWER_UPPERCASE} )
  endforeach()

  # check for large file support
  find_file( netCDF_INCLUDE_FILE netcdf.h ${netCDF_INCLUDE_DIR} )
  file( READ ${netCDF_INCLUDE_FILE} netCDF_INCLUDE_FILE_STR )
  string( FIND "${netCDF_INCLUDE_FILE_STR}" "NC_FORMAT_64BIT_DATA" netCDF_LARGE_FILE_SUPPORT_FOUND )
  if ( ${netCDF_LARGE_FILE_SUPPORT_FOUND} EQUAL -1 )
    set( netCDF_LARGE_FILE_SUPPORT "NO" )
  else()
    set( netCDF_LARGE_FILE_SUPPORT "YES" )
  endif()

  # Sanitize version
  string( REPLACE " " ";" netCDF_VERSION_LIST ${netCDF_VERSION_RAW} )
  list( GET netCDF_VERSION_LIST -1 netCDF_VERSION )

  set( netCDF_DEFINITIONS  )
  set( netCDF_LIBRARIES
      # All supported language variants will need this regardless - this may conflict with the RPATH in any
      # supplemental packages so be careful to use compatible langauge versions of netCDF
      $<$<OR:$<LINK_LANGUAGE:C>,$<LINK_LANGUAGE:Fortran>>:${netCDF_CLIBS}>
      )
  # Because we may need this for in-situ manual preprocessing do not use genex
  set( netCDF_INCLUDE_DIRS ${netCDF_INCLUDE_DIR} )

  # Find the actual name of the library
  find_library(
                netCDF_LIBRARY
                netcdf
                PATHS ${netCDF_LIBRARY_DIR}
                NO_DEFAULT_PATH
                )
  if( ${netCDF_LIBRARY} MATCHES ".a$" )
    set( netCDF_STATIC TRUE  )
    set( netCDF_SHARED FALSE )
  else()
    set( netCDF_STATIC FALSE  )
    set( netCDF_SHARED TRUE )
  endif()
endif()

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set netCDF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(
                                  netCDF
                                  FOUND_VAR netCDF_FOUND
                                  REQUIRED_VARS
                                    netCDF_INCLUDE_DIRS
                                    netCDF_LIBRARIES
                                    netCDF_VERSION
                                  VERSION_VAR netCDF_VERSION
                                  HANDLE_VERSION_RANGE
                                )

# Note that the name of the target is the target library name as specified by the netCDF cmake build,
# NOT the netCDF repository name, I've kept this consistent to the provided netCDF builds rather
# than the convention of *_<LANG> to specify multiple components. This also helps account for the
# fact that the netCDF langauge-specific projects are separate projects
if ( netCDF_FOUND AND NOT TARGET netCDF::netcdf )
  add_library( netCDF::netcdf UNKNOWN IMPORTED )
  set_target_properties(
                        netCDF::netcdf
                        PROPERTIES
                          IMPORTED_LOCATION                   "${netCDF_LIBRARY}"
                          IMPORTED_LINK_INTERFACE_LANGUAGES   C
                          INTERFACE_INCLUDE_DIRECTORIES      "${netCDF_INCLUDE_DIRS}"
                        )

  # I believe this is not required as the API will wrap usage of these
  # libnetcdf should provide the rpath dependency linking if not static
  # This is untested! Probably missing a lot of stuff
  if ( ${netCDF_STATIC} )
    if ( ${netCDF_HAS_HDF4} OR ${netCDF_HAS_HDF5} )
      if ( ${netCDF_HAS_PARALLEL} )
        set( HDF5_PREFER_PARALLEL TRUE )
      endif()

      find_package( HDF5 COMPONENTS C HL REQUIRED )
      target_link_libraries( netCDF::netcdf INTERFACE HDF5::HDF5 )
      
      find_package( ZLIB REQUIRED )
      target_link_libraries( netCDF::netcdf INTERFACE ZLIB::ZLIB )
    endif()

    # If available do find it and link in
    find_package( ZLIB QUIET )
    if ( ${ZLIB_FOUND} )
      target_link_libraries( netCDF::netcdf INTERFACE ZLIB::ZLIB )
    endif()

    if ( ${netCDF_HAS_SZLIB} )
      # Currently no standard way to find szlib, and netCDF does not export its module out soooo
      message( WARNING "No standard way to locate szlib, will attempt to link with -lsz" )
      target_link_libraries( netCDF::netcdf INTERFACE sz )
    endif()

    if ( ${netCDF_HAS_ZSTD} )
      find_package( PkgConfig )
      pkg_check_modules( PC_ZSTD QUIET zstd )
      target_link_libraries( netCDF::netcdf INTERFACE ${PC_ZSTD_LINK_LIBRARIES} )
    endif()

    # HAS_BZ2 is not defined right now
    # if ( ${netCDF_HAS_BZ2} )
    find_package( BZip2 QUIET )
    if ( ${BZIP2_FOUND} )
      target_link_libraries( netCDF::netcdf INTERFACE BZip2::BZip2 )
    endif()
    # endif()

    # HAS_LIBXML2 is not defined right now
    find_package( LibXml2 QUIET )
    if( LibXml2_FOUND )
      target_link_libraries( netCDF::netcdf INTERFACE LibXml2::LibXml2 )
    endif()  

    if ( ${netCDF_HAS_PARALLEL} )
      find_package( MPI COMPONENTS C REQUIRED )
      target_link_libraries( netCDF::netcdf INTERFACE MPI::MPI_C )
    endif()

    # Always linked
    find_package( CURL REQUIRED )
    find_library( MATH_LIB NAMES m )
    target_link_libraries(
                          netCDF::netcdf
                          INTERFACE
                            ${CMAKE_DL_LIBS}
                            CURL::libcurl
                            ${MATH_LIB}
                          )
  endif()

endif()


mark_as_advanced( netCDF_CLIBS netCDF_PREFIX netCDF_LIBRARY_DIR )

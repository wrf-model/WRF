# WRF CMake Build

# The way ncep has written these makes this difficult if not impossible to do...
# # External projects, run them inline but make an alias to their target as if 
# # we "built" them ourselves - useful to avoid ExternalProject_Add() + find_package() weirdness
# # Newer versions we might need to do that since g2 relies on bacio with find_package()
# add_subdirectory( bacio )
# add_subdirectory( g2    )

# # bacio v2.6.0
# add_library( bacio::bacio ALIAS bacio )

# # g2 v3.1.2
# if ( ${USE_DOUBLE} )
#   add_library( g2::g2 ALIAS g2_d )
# else()
#   add_library( g2::g2 ALIAS g2_4 )
# endif()


# Always build

add_subdirectory( io_int           )
add_subdirectory( io_grib1         )
add_subdirectory( io_grib_share    )
add_subdirectory( ioapi_share      )
add_subdirectory( fftpack/fftpack5 )

if ( AMT_OCN )
  # I have no clue how this gets used
  message( STATUS "Adding [atm_ocn] to configuration" )
  add_subdirectory( atm_ocn        )
endif()

if ( ADIOS2 )
  message( STATUS "Adding [io_adios2] to configuration" )
  add_subdirectory( io_adios2      )
endif()

if ( ESMF )
  message( STATUS "Adding [io_esmf] to configuration" )
  add_subdirectory( io_esmf        )
endif()

#!TODO Is this always needed
add_subdirectory( esmf_time_f90    )

# netCDF
#!TODO I believe this is always required from configure:651
add_subdirectory( io_netcdf        )
#!TODO We should collapse all these files into #ifdefs even if they are compiled
#      multiple times with different defs for the same configuration
if ( ${netCDF_PARALLEL} AND ${USE_MPI} )
  message( STATUS "Adding [io_netcdfpar] to configuration" )
  add_subdirectory( io_netcdfpar     )
endif()

if ( ${pnetCDF_FOUND} )
  message( STATUS "Adding [io_pnetcdf] to configuration" )
  add_subdirectory( io_pnetcdf       )
endif()

if ( ${PIO_FOUND} )
  message( STATUS "Adding [io_pio] to configuration" )
  add_subdirectory( io_pio           )
endif()

# https://cmake.org/cmake/help/latest/module/FindHDF5.html
# I don't think this is the correct variable to control this IO capability...
if ( ${HDF5_IS_PARALLEL} )
  message( STATUS "Adding [io_phdf5] to configuration" )
  add_subdirectory( io_phdf5         )
endif()


if ( ${Jasper_FOUND} )
  message( STATUS "Adding [io_grib2] to configuration" )
  add_subdirectory( io_grib2         )
endif()

if ( ${USE_RSL_LITE} )
  add_subdirectory( RSL_LITE         )
endif()



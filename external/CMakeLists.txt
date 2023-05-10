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


if ( AMT_OCN )
  # I have no clue how this gets used
  add_subdirectory( atm_ocn        )
endif()

if ( ADIOS2 )
  add_subdirectory( io_adios2      )
endif()

if ( ESMF )
  add_subdirectory( io_esmf        )
endif()

add_subdirectory( esmf_time_f90    )
add_subdirectory( io_pnetcdf       )
add_subdirectory( io_pio           )
add_subdirectory( io_int           )
add_subdirectory( io_netcdf        )
add_subdirectory( io_netcdfpar     )
add_subdirectory( io_phdf5         )
add_subdirectory( io_grib1         )
add_subdirectory( io_grib_share    )
add_subdirectory( io_grib2         )

add_subdirectory( ioapi_share      )
add_subdirectory( RSL_LITE         )
add_subdirectory( fftpack/fftpack5 )


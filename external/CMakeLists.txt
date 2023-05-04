# WRF CMake Build
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


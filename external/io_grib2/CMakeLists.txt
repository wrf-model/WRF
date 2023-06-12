# WRF CMake Build

# Eventually switch to ncep tag on github but for now make this 1-to-1 with make-style WRF
add_subdirectory( g2lib     )
add_subdirectory( bacio-1.3 )

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  grib2tbls_types.F
                  io_grib2.F
                  read_grib2map.F
                  )

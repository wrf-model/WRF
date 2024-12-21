# build the version static library
add_library(hydro_driver STATIC
        module_HYDRO_drv.F90
)

target_link_libraries(hydro_driver PUBLIC
        hydro_mpp
        hydro_data_rec
        hydro_routing
        hydro_debug_utils
        PRIVATE
          netCDF::netcdff
)

if(WRF_HYDRO_NUDGING STREQUAL "1")
        target_link_libraries(hydro_driver PUBLIC hydro_nudging)
endif()


# build the version static library
add_library(hydro_driver STATIC
        module_HYDRO_drv.F90
)

target_link_libraries(hydro_driver PUBLIC
        hydro_mpp
        hydro_data_rec
        hydro_routing
        hydro_debug_utils
)

if(WRF_HYDRO_NUDGING STREQUAL "1")
        target_link_libraries(hydro_driver PUBLIC hydro_nudging)
endif()

target_include_directories(hydro_driver
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

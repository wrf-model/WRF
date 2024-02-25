# build the version static library
add_library(hydro_nudging STATIC
        module_date_utils_nudging.F90
        module_nudging_io.F90
        module_nudging_utils.F90
        module_stream_nudging.F90
)

target_link_libraries(hydro_nudging PRIVATE
        hydro_mpp
        hydro_data_rec
        hydro_orchestrator
)

target_include_directories(hydro_nudging
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

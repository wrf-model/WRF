add_library(hydro_routing_reservoirs STATIC
        module_reservoir.F90
        module_reservoir_read_timeslice_data.F90
        module_reservoir_read_rfc_time_series_data.F90
        module_reservoir_utilities.F90
)

target_include_directories(hydro_routing_reservoirs
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

add_subdirectory("Level_Pool")
add_subdirectory("Persistence_Level_Pool_Hybrid")
add_subdirectory("RFC_Forecasts")

add_dependencies(hydro_routing_reservoirs hydro_utils)

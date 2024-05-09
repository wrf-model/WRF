add_library(hydro_routing_reservoirs_rfc STATIC
        module_rfc_forecasts.F90
        module_rfc_forecasts_state.F90
        module_rfc_forecasts_properties.F90
)

add_dependencies(hydro_routing_reservoirs_rfc
        hydro_routing_reservoirs
        hydro_routing_reservoirs_levelpool
)

target_include_directories(hydro_routing_reservoirs_rfc
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

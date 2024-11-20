add_library(hydro_routing_reservoirs_levelpool STATIC
        module_levelpool.F90
        module_levelpool_state.F90
        module_levelpool_properties.F90
)

add_dependencies(hydro_routing_reservoirs_levelpool hydro_routing_reservoirs)

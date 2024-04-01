add_library(hydro_routing_reservoirs_hybrid STATIC
        module_persistence_levelpool_hybrid.F90
        module_persistence_levelpool_hybrid_state.F90
        module_persistence_levelpool_hybrid_properties.F90
)

add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs)

target_include_directories(hydro_routing_reservoirs_hybrid
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

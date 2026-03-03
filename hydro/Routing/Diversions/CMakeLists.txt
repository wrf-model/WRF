add_library(hydro_routing_diversions STATIC
    module_diversions.F90
    module_diversions_timeslice.F90
)

add_dependencies(hydro_routing_diversions hydro_orchestrator)
add_dependencies(hydro_routing_diversions fortglob)

target_link_libraries(hydro_routing_diversions PUBLIC hydro_orchestrator)
target_link_libraries(hydro_routing_diversions PUBLIC fortglob)

add_library(hydro_routing_subsurface STATIC
        module_subsurface_input.F90
        module_subsurface_output.F90
        module_subsurface_static_data.F90
        module_subsurface_grid_transform.F90
        module_subsurface_properties.F90
        module_subsurface_state.F90
        module_subsurface.F90
)

target_link_libraries(hydro_routing_subsurface PRIVATE hydro_routing_overland)

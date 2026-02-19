# build the version static library
add_library(hydro_nudging_io STATIC
        module_nudging_io.F90
)

target_link_libraries(hydro_nudging_io PRIVATE
        hydro_orchestrator
        hydro_data_rec
        hydro_mpp
)

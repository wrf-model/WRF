# build the version static library
add_library(hydro_debug_utils STATIC
        debug_dump_variable.F90
)
target_link_libraries( hydro_debug_utils PRIVATE hydro_mpp )

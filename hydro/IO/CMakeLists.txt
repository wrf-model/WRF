# build the orchestrator static library
add_library(hydro_netcdf_layer STATIC
        netcdf_layer.F90
)

target_link_libraries(hydro_netcdf_layer
        MPI::MPI_Fortran
        netCDF::netcdff
)

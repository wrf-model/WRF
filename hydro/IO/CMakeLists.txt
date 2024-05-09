# build the orchestrator static library
add_library(hydro_netcdf_layer STATIC
        netcdf_layer.F90
)

target_link_libraries(hydro_netcdf_layer
        MPI::MPI_Fortran
)

target_include_directories(hydro_netcdf_layer
        PRIVATE
        ${netCDF_INCLUDE_DIRS}
        ${netCDF-Fortran_INCLUDE_DIRS}
)

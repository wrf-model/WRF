add_library(hydro_mpp STATIC
        CPL_WRF.F90
        module_mpp_GWBUCKET.F90
        module_mpp_ReachLS.F90
        mpp_land.F90
        hashtable.F90
)

target_link_libraries(hydro_mpp MPI::MPI_Fortran)
target_include_directories(hydro_mpp PUBLIC
        ${MPI_Fortran_MODULE_DIR}
)

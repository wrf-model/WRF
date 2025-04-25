add_library(hydro_wrf_cpl STATIC
        wrf_drv_HYDRO.F90
        module_wrf_HYDRO.F90
)

add_dependencies(hydro_wrf_cpl
        hydro_mpp
        hydro_utils
        hydro_debug_utils
        hydro_data_rec
        hydro_driver
        hydro_orchestrator
        ${PROJECT_NAME}_Core
        MPI::MPI_Fortran
)

target_include_directories(hydro_wrf_cpl
        PRIVATE
        $<TARGET_PROPERTY:${PROJECT_NAME}_Core,Fortran_MODULE_DIRECTORY>
        $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
)

target_include_directories(hydro_wrf_cpl PUBLIC
        ${MPI_Fortran_MODULE_DIR}
)

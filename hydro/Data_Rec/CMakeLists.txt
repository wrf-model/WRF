add_library(hydro_data_rec STATIC
        module_gw_gw2d_data.F90
        module_rt_inc.F90
        module_namelist_inc.F90
        module_RT_data.F90
        module_namelist.F90
)
target_link_libraries( hydro_data_rec PRIVATE hydro_mpp )

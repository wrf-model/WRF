# build the version static library
add_library(hydro_nudging STATIC
	module_date_utils_nudging.F90
	module_nudging_io.F90
	module_nudging_utils.F90
	module_stream_nudging.F90
)

target_link_libraries(hydro_nudging PUBLIC hydro_mpp)
target_link_libraries(hydro_nudging PUBLIC hydro_data_rec)
target_link_libraries(hydro_nudging PUBLIC hydro_orchestrator)

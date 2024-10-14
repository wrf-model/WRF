# additions that WRF-Hydro's top CMakeLists.txt handles
add_compile_options( "${PROJECT_COMPILE_OPTIONS}" )
add_compile_definitions( "${PROJECT_COMPILE_DEFINITIONS}" )
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/hydro/mods)
add_definitions(-DMPP_LAND)
if (WRF_HYDRO_NUDGING STREQUAL "1")
        add_definitions(-DWRF_HYDRO_NUDGING=1)
endif()

# build the various sup-projects
add_subdirectory("MPP")
add_subdirectory("utils")
add_subdirectory("IO")
add_subdirectory("OrchestratorLayer")
add_subdirectory("Debug_Utilities")
add_subdirectory("Routing/Overland")
add_subdirectory("Routing/Subsurface")
add_subdirectory("Routing/Reservoirs")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")
add_subdirectory("CPL/WRF_cpl")

if (WRF_HYDRO_NUDGING STREQUAL "1")
        add_subdirectory("nudging")
        add_dependencies(hydro_routing hydro_nudging)
        add_dependencies(hydro_driver hydro_nudging)
endif()

# add module dependencies
add_dependencies(hydro_debug_utils hydro_mpp)
add_dependencies(hydro_utils hydro_mpp)
add_dependencies(hydro_orchestrator hydro_netcdf_layer)

add_dependencies(hydro_routing
        hydro_mpp
        hydro_routing_overland
        hydro_routing_subsurface
        hydro_routing_reservoirs
        hydro_routing_reservoirs_levelpool
        hydro_routing_reservoirs_hybrid
        hydro_utils
)

add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_levelpool)
add_dependencies(hydro_routing_overland hydro_mpp)

# currently unused Routing/Groundwater directory
# add_subdirectory("Routing/Groundwater")
# add_dependencies(hydro_routing
#         hydro_routing_groundwater
#         hydro_routing_groundwater_bucket
#         hydro_routing_groundwater_nhd
#         hydro_routing_groundwater_simple
# )
# add_dependencies(hydro_routing_groundwater hydro_mpp)
# add_dependencies(hydro_routing_groundwater_bucket hydro_routing_groundwater)
# add_dependencies(hydro_routing_groundwater_simple
#         hydro_routing_groundwater
#         hydro_routing_groundwater_bucket
# )
# add_dependencies(hydro_routing_groundwater_nhd
#         hydro_routing_groundwater
#         hydro_routing_groundwater_bucket
# )

add_dependencies(hydro_driver
        hydro_routing
        hydro_debug_utils
)

add_dependencies(hydro_data_rec
        hydro_routing_overland
        hydro_routing_subsurface
        hydro_routing_reservoirs
)

add_library(wrfhydro INTERFACE)
target_link_libraries(wrfhydro INTERFACE
        hydro_utils
        hydro_mpp
        hydro_debug_utils
        hydro_routing_overland
        hydro_routing_subsurface
        hydro_data_rec
        hydro_routing
        hydro_routing_reservoirs_levelpool
        hydro_routing_reservoirs_hybrid
        hydro_routing_reservoirs_rfc
        hydro_routing_reservoirs
        hydro_wrf_cpl
        hydro_orchestrator
        hydro_netcdf_layer
        hydro_driver
)

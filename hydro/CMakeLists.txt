# additions that WRF-Hydro's top CMakeLists.txt handles
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/hydro/mods)
add_definitions(-DMPP_LAND)


# build the various sup-projects
add_subdirectory("MPP")
add_subdirectory("utils")
add_subdirectory("IO")
add_subdirectory("OrchestratorLayer")
add_subdirectory("Debug_Utilities")
add_subdirectory("Routing/Overland")
add_subdirectory("Routing/Subsurface")
#add_subdirectory("Routing/Groundwater")
add_subdirectory("Routing/Reservoirs")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")
add_subdirectory("CPL/WRF_cpl")


if (WRF_HYDRO_NUDGING STREQUAL "1")
    add_subdirectory("nudging")
    add_dependencies(hydro_driver hydro_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")

# add module dependencies
add_dependencies(hydro_debug_utils hydro_mpp)
add_dependencies(hydro_utils hydro_mpp)

add_dependencies(hydro_orchestrator hydro_netcdf_layer)

add_dependencies(hydro_routing hydro_mpp)
add_dependencies(hydro_routing hydro_routing_overland)
add_dependencies(hydro_routing hydro_routing_subsurface)
add_dependencies(hydro_routing hydro_routing_reservoirs)
add_dependencies(hydro_routing hydro_routing_reservoirs_levelpool)
add_dependencies(hydro_routing hydro_routing_reservoirs_hybrid)
add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_levelpool)
#add_dependencies(hydro_routing hydro_routing_groundwater)
#add_dependencies(hydro_routing hydro_routing_groundwater_bucket)
#add_dependencies(hydro_routing hydro_routing_groundwater_nhd)
#add_dependencies(hydro_routing hydro_routing_groundwater_simple)
add_dependencies(hydro_routing hydro_utils)

add_dependencies(hydro_routing_overland hydro_mpp)

#add_dependencies(hydro_routing_groundwater hydro_mpp)
#add_dependencies(hydro_routing_groundwater_bucket hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater_bucket)
#add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater)
#add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater_bucket)

add_dependencies(hydro_driver hydro_routing)
add_dependencies(hydro_driver hydro_debug_utils)

add_dependencies(hydro_data_rec hydro_routing_overland)
add_dependencies(hydro_data_rec hydro_routing_subsurface)
#add_dependencies(hydro_data_rec hydro_routing_groundwater)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_bucket)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_simple)
#add_dependencies(hydro_data_rec hydro_routing_groundwater_nhd)
add_dependencies(hydro_data_rec hydro_routing_reservoirs)


add_library(wrfhydro STATIC
  $<TARGET_OBJECTS:hydro_utils>
  $<TARGET_OBJECTS:hydro_mpp>
  $<TARGET_OBJECTS:hydro_debug_utils>
  $<TARGET_OBJECTS:hydro_routing_overland>
  $<TARGET_OBJECTS:hydro_routing_subsurface>
  $<TARGET_OBJECTS:hydro_data_rec>
  $<TARGET_OBJECTS:hydro_routing>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_levelpool>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_hybrid>
  $<TARGET_OBJECTS:hydro_routing_reservoirs_rfc>
  $<TARGET_OBJECTS:hydro_routing_reservoirs>
  $<TARGET_OBJECTS:hydro_wrf_cpl>
  $<TARGET_OBJECTS:hydro_orchestrator>
  $<TARGET_OBJECTS:hydro_netcdf_layer>
  $<TARGET_OBJECTS:hydro_driver>
  )

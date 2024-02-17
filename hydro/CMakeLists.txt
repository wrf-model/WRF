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


# if (HYDRO_LSM MATCHES "NoahMP")
#     message ("-- Building NoahMP LSM")
#     add_subdirectory("Land_models/NoahMP")

#     add_subdirectory("CPL/NoahMP_cpl")
#     add_dependencies(hydro_noahmp_cpl hydro_routing)
#     add_dependencies(hydro_noahmp_cpl hydro_mpp )
#     add_dependencies(hydro_noahmp_cpl hydro_driver )


#     add_executable(wrfhydro.exe
#         Land_models/NoahMP/IO_code/main_hrldas_driver.F
#         Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.F
#         Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F
#     )

    # target_include_directories(wrfhydro.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

    # target_link_libraries(wrfhydro.exe hydro_utils)
    # target_link_libraries(wrfhydro.exe hydro_mpp)
    # target_link_libraries(wrfhydro.exe hydro_debug_utils)
    # target_link_libraries(wrfhydro.exe hydro_routing_overland)
    # target_link_libraries(wrfhydro.exe hydro_routing_subsurface)
    # #target_link_libraries(wrfhydro.exe hydro_routing_groundwater)
    # #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_bucket)
    # #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_nhd)
    # #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_simple)
    # target_link_libraries(wrfhydro.exe hydro_data_rec)
    # target_link_libraries(wrfhydro.exe hydro_routing)
    # target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_levelpool)
    # target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_hybrid)
    # target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_rfc)
    # target_link_libraries(wrfhydro.exe hydro_routing_reservoirs)
    # target_link_libraries(wrfhydro.exe hydro_driver)
    # target_link_libraries(wrfhydro.exe noahmp_util)
    # target_link_libraries(wrfhydro.exe noahmp_phys)
    # target_link_libraries(wrfhydro.exe noahmp_data)
    # target_link_libraries(wrfhydro.exe hydro_noahmp_cpl)
    # target_link_libraries(wrfhydro.exe ${NETCDF_LIBRARIES})

    # if (WRF_HYDRO_NUDGING STREQUAL "1")
    #     target_link_libraries(wrfhydro.exe hydro_nudging)
    #     add_dependencies(wrfhydro.exe hydro_nudging)
    # endif (WRF_HYDRO_NUDGING STREQUAL "1")

    # # bash commands to copy namelists to the Run directory
    # set(BASH_CP_HRLDAS_NML "if [[ ! -f ${CMAKE_BINARY_DIR}/Run/namelist.hrldas ]]\; then cp ${PROJECT_SOURCE_DIR}/src/template/NoahMP/namelist.hrldas ${CMAKE_BINARY_DIR}/Run \; fi\;")
    # set(BASH_CP_HYDRO_NML "if [[ ! -f ${CMAKE_BINARY_DIR}/Run/hydro.namelist ]]\; then cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run \; fi\;")

    # add_custom_command(TARGET wrfhydro.exe POST_BUILD
    #     COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
    #     COMMAND cp ${PROJECT_SOURCE_DIR}/tests/ctests/run_dir_makefile.mk ${CMAKE_BINARY_DIR}/Run/Makefile
    #     # copy tables
    #     COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
    #     COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
    #     COMMAND cp ${PROJECT_SOURCE_DIR}/src/Land_models/NoahMP/run/*.TBL ${CMAKE_BINARY_DIR}/Run
    #     # copy namelists
    #     COMMAND bash -c "${BASH_CP_HRLDAS_NML}"
    #     COMMAND bash -c "${BASH_CP_HYDRO_NML}"
    #     # copy and setup executables
    #     COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
    #     COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP.exe
    #     COMMAND cp ${PROJECT_BINARY_DIR}/src/wrfhydro.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP.exe
    #     COMMAND ln -s wrf_hydro_NoahMP.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
    #     COMMAND rm ${PROJECT_BINARY_DIR}/src/wrfhydro.exe
    # )


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
  $<TARGET_OBJECTS:hydro_driver>
  # $<TARGET_OBJECTS:noahmp_util>
  # $<TARGET_OBJECTS:noahmp_phys>
  # $<TARGET_OBJECTS:noahmp_data>
  # $<TARGET_OBJECTS:hydro_noahmp_cpl>
  )

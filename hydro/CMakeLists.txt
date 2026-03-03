if(${PROJECT_NAME} STREQUAL "WRF")
        # additions that WRF-Hydro's top CMakeLists.txt handles
        add_compile_options( "${PROJECT_COMPILE_OPTIONS}" )
        add_compile_definitions( "${PROJECT_COMPILE_DEFINITIONS}" )
        set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/hydro/mods)
        add_definitions(-DMPP_LAND)
        if (WRF_HYDRO_NUDGING STREQUAL "1")
                add_definitions(-DWRF_HYDRO_NUDGING=1)
        endif()
        if (OUTPUT_CHAN_CONN STREQUAL "1" )
                add_definitions(-DOUTPUT_CHAN_CONN)
                # requires nudging io module
                set(WRF_HYDRO_NUDGING_IO "1")
        endif()
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
add_subdirectory("Routing/Diversions")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")
if(${PROJECT_NAME} STREQUAL "WRF")
        add_subdirectory("CPL/WRF_cpl")
endif()

if (WRF_HYDRO_NUDGING_IO STREQUAL "1" OR
    WRF_HYDRO_NUDGING STREQUAL "1")
        add_subdirectory("nudging/io")
        add_dependencies(hydro_routing hydro_nudging_io)
endif()

if (WRF_HYDRO_NUDGING STREQUAL "1")
        add_subdirectory("nudging")
        add_dependencies(hydro_routing hydro_nudging)
        add_dependencies(hydro_driver hydro_nudging)
endif()

if (WRF_HYDRO_NUOPC STREQUAL "1")
        add_subdirectory("CPL/NUOPC_cpl")
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

if (HYDRO_LSM MATCHES "NoahMP")
        message("-- Building NoahMP LSM")
        add_subdirectory("Land_models/NoahMP")

        add_subdirectory("CPL/NoahMP_cpl")
        add_dependencies(hydro_noahmp_cpl hydro_routing)
        add_dependencies(hydro_noahmp_cpl hydro_mpp )
        add_dependencies(hydro_noahmp_cpl hydro_driver )

        add_executable(wrfhydro
                Land_models/NoahMP/IO_code/main_hrldas_driver.F
                Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.F
                Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F
        )

        target_include_directories(wrfhydro BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

        target_link_libraries(wrfhydro
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
                hydro_driver
                noahmp_util
                noahmp_phys
                noahmp_data
                hydro_noahmp_cpl
                ${NETCDF_LIBRARIES}
                # hydro_routing_groundwater
                # hydro_routing_groundwater_bucket
                # hydro_routing_groundwater_nhd
                # hydro_routing_groundwater_simple
        )

        if (WRF_HYDRO_NUDGING_IO STREQUAL "1")
          target_link_libraries(wrfhydro hydro_nudging_io)
          add_dependencies(wrfhydro hydro_nudging_io)
        endif()

        if (WRF_HYDRO_NUDGING STREQUAL "1")
                target_link_libraries(wrfhydro hydro_nudging)
                target_link_libraries(wrfhydro hydro_routing_diversions)
                add_dependencies(wrfhydro hydro_nudging)
                add_dependencies(wrfhydro hydro_routing_diversions)
        endif()

        # bash commands to copy namelists to the Run directory
        set(BASH_CP_HRLDAS_NML "if [[ ! -f ${CMAKE_BINARY_DIR}/Run/namelist.hrldas ]]\; then cp ${PROJECT_SOURCE_DIR}/src/template/NoahMP/namelist.hrldas ${CMAKE_BINARY_DIR}/Run \; fi\;")
        set(BASH_CP_HYDRO_NML "if [[ ! -f ${CMAKE_BINARY_DIR}/Run/hydro.namelist ]]\; then cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run \; fi\;")

        add_custom_command(TARGET wrfhydro POST_BUILD
                COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/tests/ctests/run_dir_makefile.mk ${CMAKE_BINARY_DIR}/Run/Makefile
                # copy tables
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/Land_models/NoahMP/run/*.TBL ${CMAKE_BINARY_DIR}/Run
                # copy namelists
                COMMAND bash -c "${BASH_CP_HRLDAS_NML}"
                COMMAND bash -c "${BASH_CP_HYDRO_NML}"
                # copy and setup executables
                COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/wrf_hydro
                COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP
                COMMAND cp ${PROJECT_BINARY_DIR}/src/wrfhydro ${CMAKE_BINARY_DIR}/Run/wrf_hydro
                COMMAND ln -sf ${CMAKE_BINARY_DIR}/Run/wrf_hydro ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP
                COMMAND rm ${PROJECT_BINARY_DIR}/src/wrfhydro
        )
        if(WRF_HYDRO_CREATE_EXE_SYMLINK)
                add_custom_command(TARGET wrfhydro POST_BUILD
                        COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_BINARY_DIR}/Run/wrf_hydro ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
                )
        endif()

elseif (HYDRO_LSM MATCHES "Noah")
        message("-- Building Noah LSM")
        add_subdirectory("Land_models/Noah")
        add_subdirectory("CPL/Noah_cpl")

        add_dependencies(hydro_noah_cpl hydro_routing)
        add_dependencies(hydro_noah_cpl hydro_mpp )
        add_dependencies(hydro_noah_cpl hydro_driver )

        add_executable(wrfhydro
                Land_models/Noah/IO_code/module_hrldas_netcdf_io.F
                Land_models/Noah/IO_code/Noah_hrldas_driver.F
        )

        target_include_directories(wrfhydro BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

        target_link_libraries(wrfhydro
                hydro_utils
                hydro_mpp
                hydro_debug_utils
                hydro_routing_overland
                hydro_routing_subsurface
                hydro_data_rec
                hydro_routing
                hydro_driver
                hydro_routing_reservoirs_levelpool
                hydro_routing_reservoirs_hybrid
                hydro_routing_reservoirs_rfc
                hydro_routing_reservoirs
                noah_util
                noah
                hydro_noah_cpl
                ${NETCDF_LIBRARIES}
                ${MPI_Fortran_LIBRARIES}
                # hydro_routing_groundwater
                # hydro_routing_groundwater_bucket
                # hydro_routing_groundwater_nhd
                # hydro_routing_groundwater_simple
        )

        if (WRF_HYDRO_NUDGING STREQUAL "1")
                target_link_libraries(wrfhydro hydro_nudging)
                add_dependencies(wrfhydro hydro_nudging)
        endif()

        add_custom_command(TARGET wrfhydro POST_BUILD
                COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
                COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*
                COMMAND cp ${PROJECT_BINARY_DIR}/src/wrfhydro ${CMAKE_BINARY_DIR}/Run/wrf_hydro_Noah
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/Noah/* ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
                COMMAND cp ${PROJECT_SOURCE_DIR}/src/Land_models/Noah/Run/*.TBL ${CMAKE_BINARY_DIR}/Run
                COMMAND ln -sf ${CMAKE_BINARY_DIR}/Run/wrf_hydro_Noah ${CMAKE_BINARY_DIR}/Run/wrf_hydro
                COMMAND ln -sf ${CMAKE_BINARY_DIR}/Run/wrf_hydro_Noah ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
                COMMAND rm ${PROJECT_BINARY_DIR}/src/wrfhydro
        )

elseif(${PROJECT_NAME} STREQUAL "WRF")
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
else()
        message("Unknown land surface model:" ${HYDRO_LSM} )
endif()

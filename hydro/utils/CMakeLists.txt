# read version numbers for wrf_hydro_version and nwm_version from
# ../.version and ../.nwm_version files

file(STRINGS "../.version" WRF_HYDRO_VERSION)
if(NWM_META AND EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/../.nwm_version)
        file (STRINGS "../.nwm_version"  NWM_VERSION)
else(NWM_META AND EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/../.nwm_version)
        set(NWM_VERSION "undefined")
endif()

# add the preprocessor definitions for NWM_VERSION and WRF_HYDRO_VERSION
# needed to compile module_version.F90
add_definitions(-DNWM_VERSION="${NWM_VERSION}")
add_definitions(-DWRF_HYDRO_VERSION="${WRF_HYDRO_VERSION}")

# build the version static library
add_library(hydro_utils STATIC
        module_version.F90
        module_hydro_stop.F90
)

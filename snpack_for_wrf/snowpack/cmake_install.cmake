# Install script for directory: /home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/varun/WRF/install_folder/code/snpack_for_wrf/snow_libs")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/snowpack" TYPE FILE FILES
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Constants.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/DataClasses.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Hazard.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Laws_sn.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/MainPage.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Meteo.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Saltation.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/SnowDrift.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/SnowpackConfig.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Stability.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/StabilityAlgorithms.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/TechnicalSnow.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/Utils.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/libsnowpack.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/vanGenuchten.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/snowpack/plugins" TYPE FILE FILES
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/AsciiIO.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/CaaMLIO.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/ImisDBIO.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/SmetIO.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/SnowpackIO.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/plugins/SnowpackIOInterface.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/snowpack/snowpackCore" TYPE FILE FILES
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/Aggregate.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/Canopy.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/Metamorphism.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/PhaseChange.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/ReSolver1d.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/SalinityTransport.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/SeaIce.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/Snowpack.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/Solver.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/VapourTransport.h"
    "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/snowpackCore/WaterTransport.h"
    )
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/snowpack/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/varun/WRF/install_folder/code/snpack_for_wrf/snowpack/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")

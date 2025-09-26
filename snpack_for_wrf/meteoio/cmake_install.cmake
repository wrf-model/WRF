# Install script for directory: /home/varun/WRF_Jul_6/snpack_for_wrf/meteoio

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/varun/WRF_Jul_6/snpack_for_wrf/snow_libs")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "release")
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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/Config.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/DataCreator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/DataGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/FileUtils.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/Graphics.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/GridsManager.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/IOExceptions.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/IOHandler.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/IOInterface.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/IOManager.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/IOUtils.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/MainPage.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/MathOptim.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/MessageBoxX11.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/Meteo1DInterpolator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/Meteo2DInterpolator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/MeteoIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/MeteoProcessor.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/TimeSeriesManager.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/Timer.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/dataClasses" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Array1D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Array2D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Array3D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Array4D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Buffer.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Coords.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/CoordsAlgorithms.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/DEMAlgorithms.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/DEMObject.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Date.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Grid2DObject.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Grid3DObject.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/Matrix.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/MeteoData.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/StationData.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/dataGenerators" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/AllSkyLWGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/AllSkySWGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/ClearSkyLWGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/ClearSkySWGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/ConstGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/ESOLIPGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/GeneratorAlgorithms.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/HumidityGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/IswrAlbedoGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/PrecSplitting.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/RadiationComponents.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/SinGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/StdPressGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/TauCLDGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/TsGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/WindComponents.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/template.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/meteoResampling" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/Accumulate.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/DailyAverageResampling.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/DailySolarResampling.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/LinearResampling.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/NearestNeighbour.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/NoResampling.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/ResamplingAlgorithms.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/SolarResampling.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/template.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/spatialInterpolations" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ALSScaleAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/AvgAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/AvgLapseAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ConstAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/IDWAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/IDWLapseAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/IDWLapseLocalAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/IDWSlopesAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ILWREpsAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/InterpolationAlgorithms.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ListonWindAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/NearestNeighbourAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/NoneAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ODKrigAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/ODKrigLapseAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/PPhaseAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/RHListonAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/RyanWindAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/SnowPsumAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/StdPressAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/SwRadAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/UserAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/WinstralAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/WinstralListonAlgorithm.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/template.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/plugins" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/A3DIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/ALPUG.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/ARCIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/ARPSIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/Argos.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/CosmoXMLIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/CsvIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/DBO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/GRIBIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/GeotopIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/Goes.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/GrassIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/ImisIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/NetCDFIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/OshdIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/PGMIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/PNGIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/PSQLIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/PmodIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/SASEIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/SMETIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/SNIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/ZRXPIO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/exports.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/libMatioWrapper.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/libncpp.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/libsmet.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/template.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/meteoLaws" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoLaws/Atmosphere.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoLaws/Meteoconst.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoLaws/Sun.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoLaws/Suntrajectory.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/meteoFilters" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterDeGrass.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterDespikingPS.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterKalman.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMAD.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMaths.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMax.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMin.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMinMax.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterMinMaxConditional.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterNoChange.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterParticle.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterPotentialSW.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterRate.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterStdDev.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterSuppr.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterTimeconsistency.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterTukey.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/FilterUnheatedPSUM.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcAdd.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcAggregate.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcDeAccumulate.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcExpSmoothing.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcIIR.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcMult.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcPSUMDistribute.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcQuantileMapping.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcRHWaterToIce.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcShade.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcUndercatch_Forland.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcUndercatch_Hamon.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcUndercatch_WMO.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcUnventilatedT.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcWMASmoothing.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcessingBlock.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/ProcessingStack.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/TimeFilters.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/WindowedFilter.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/template.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xheadersx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/meteoio/meteoStats" TYPE FILE FILES
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/RandomNumberGenerator.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/libfit1D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/libfit1DCore.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/libinterpol1D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/libinterpol2D.h"
    "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoStats/libresampling2D.h"
    )
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataClasses/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/dataGenerators/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/plugins/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoLaws/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoFilters/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/meteoResampling/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/spatialInterpolations/cmake_install.cmake")
  include("/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/meteoio/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/varun/WRF_Jul_6/snpack_for_wrf/meteoio/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")

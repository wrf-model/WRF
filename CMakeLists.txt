cmake_minimum_required( VERSION 3.20 )
cmake_policy( SET CMP0118 NEW )

enable_language( C )
enable_language( CXX )
enable_language( Fortran )

project( WRF )

# list( APPEND CMAKE_MODULE_PATH         )
list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/ ${PROJECT_SOURCE_DIR}/cmake/modules )

# TODO rename to something better
include( preproc )


################################################################################
##
## Options that can be user configured
##
################################################################################
set( CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/runTemp )

set( OPTIMIZATION_LEVEL "" )
set( WRF_OS             "" )
set( WRF_MACH           "" )

# Mode configuration
# DO NOT USE OPTION - IT DOES NOT WORK AS ANTICIPATED EVEN WHEN CLEARING CACHE - YOU HAVE BEEN WARNED
set( ENABLE_CHEM       OFF CACHE BOOL "ENABLE_CHEM"      )
set( ENABLE_CMAQ       OFF CACHE BOOL "ENABLE_CMAQ"      )
set( ENABLE_KPP        OFF CACHE BOOL "ENABLE_KPP"       )
set( ENABLE_DFI_RADAR  OFF CACHE BOOL "ENABLE_DFI_RADAR" )
set( ENABLE_TITAN      OFF CACHE BOOL "ENABLE_TITAN"     )
set( ENABLE_MARS       OFF CACHE BOOL "ENABLE_MARS"      )
set( ENABLE_VENUS      OFF CACHE BOOL "ENABLE_VENUS"     )
set( ENABLE_VENUS      OFF CACHE BOOL "ENABLE_VENUS"     )
set( ENABLE_TERRAIN    OFF CACHE BOOL "ENABLE_TERRAIN"   )

# What do these defines even do if they are always on????
set( USE_ALLOCATABLES     ON CACHE BOOL "USE_ALLOCATABLES"    )
set( wrfmodel             ON CACHE BOOL "wrfmodel"            )
set( GRIB1                ON CACHE BOOL "GRIB1"               )
set( INTIO                ON CACHE BOOL "INTIO"               )
set( KEEP_INT_AROUND      ON CACHE BOOL "KEEP_INT_AROUND"     )
set( LIMIT_ARGS           ON CACHE BOOL "LIMIT_ARGS"          )

set( BUILD_RRTMG_FAST    OFF CACHE BOOL "BUILD_RRTMG_FAST"    )
set( BUILD_RRTMK         OFF CACHE BOOL "BUILD_RRTMK"         )
set( BUILD_SBM_FAST       ON CACHE BOOL "BUILD_SBM_FAST"      )
set( SHOW_ALL_VARS_USED  OFF CACHE BOOL "SHOW_ALL_VARS_USED"  )
# Not cached, cannot be changed, do not touch
set( NMM_CORE            OFF )

# WRF Core selection
set( WRF_CORE_OPTIONS 
      # Options listed here
      ARW
      COAMPS  # Not sure how this is used
      CONVERT # This exists in compile but not configure
      DA
      DA_4D_VAR
      PLUS
      )

# Set default WRF_CORE
set( WRF_CORE "" CACHE STRING "WRF_CORE" )
list( GET WRF_CORE_OPTIONS 0 WRF_CORE )



option( USE_DOUBLE  OFF ) # I believe this is the r8/real8 promotion
option( USE_MPI     OFF )
option( USE_OPENMP  OFF )

# From arch/preamble
#### Single location for defining total number of domains.  You need
#### at least 1 + 2*(number of total nests).  For example, 1 coarse
#### grid + three fine grids = 1 + 2(3) = 7, so MAX_DOMAINS=7.
set( MAX_DOMAINS_F 21 )


#### DM buffer length for the configuration flags.

set( CONFIG_BUF_LEN 65536 )

#### Size of bitmasks (in 4byte integers) of stream masks for WRF I/O

set( MAX_HISTORY    25    )

set( IWORDSIZE      4     )
set( DWORDSIZE      8     )
set( LWORDSIZE      4     )
if ( USE_DOUBLE )
  set( RWORDSIZE    8     )
else()
  set( RWORDSIZE    4     )
endif()

########################

################################################################################
##
## Load alternate OS/architecture to be compiled for if cross-compiling
##
################################################################################
if ( ${WRF_OS} )
  # Need a good way of condensing this down
  set( WRF_ARCH_FILE ${WRF_OS} )

  set( CMAKE_TOOLCHAIN_FILE "" )
endif()


# Check WRF options
if ( NOT ${WRF_CORE} IN_LIST WRF_CORE_OPTIONS )
  message( FATAL_ERROR "WRF Core option not recognized : ${WRF_CORE}" )
endif()

set( EM_CORE 1 )
# Far easier to write this one as normal logic rather than generator expression
if( ${WRF_CORE} STREQUAL "CONVERT" OR ${WRF_CORE} STREQUAL "COAMPS" )
  set( EM_CORE 0 )
endif()

################################################################################
##
## Now find packages that cross-compilation is potentially handled
##
################################################################################
# Provided by cmake natively
find_package( ZLIB   REQUIRED )
find_package( CURL   REQUIRED )

# HDF5 has some funky weirdness between versions where the casing has changed
find_package( HDF5   REQUIRED )

# Will need our own finder
# find_package( GPFS   REQUIRED )

# Included is a lightweight finder, but we really should switch to using UniData's netCDF cmake config
find_package( netCDF REQUIRED )

# Make use of version checking here and not in find_package for previous versions that did not use cmake
if ( NOT netCDF_VERSION GREATER_EQUAL "4.1.3" )
  message( FATAL "Please make sure NETCDF version is 4.1.3 or later. " )
  
endif()

if ( USE_MPI )
  find_package( MPI REQUIRED COMPONENTS Fortran )
  add_compile_definitions( USE_MPI=1 DM_PARALLEL=1 )
endif()

if ( USE_OPENMP )
  find_package( OpenMP REQUIRED COMPONENTS Fortran )
endif()

################################################################################
##
## Adjust flags based on compiler and linker used
##
################################################################################
# https://stackoverflow.com/a/53155812
set( Fortran_COMPILER_ID ${CMAKE_Fortran_COMPILER_ID} )
message( STATUS "Set Fortran_COMPILER_ID to : ${Fortran_COMPILER_ID}" )


# Whole project flags
add_compile_options( 
                    $<$<COMPILE_LANGUAGE:Fortran>:-cpp>
                    # Use "" and ; specifically to evaluate correctly
                    "$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-diag-disable;6843>"
                    
                    # $<AND:$<COMPILE_LANGUAGE:Fortran>,$<STREQUAL:${Fortran_COMPILER_ID},Intel>:-diag-disable;6843>
                    )

add_compile_definitions(
                        MAX_DOMAINS_F=${MAX_DOMAINS_F}
                        CONFIG_BUF_LEN=${CONFIG_BUF_LEN}
                        MAX_HISTORY=${MAX_HISTORY}
                        IWORDSIZE=${IWORDSIZE}
                        DWORDSIZE=${DWORDSIZE}
                        LWORDSIZE=${LWORDSIZE}
                        RWORDSIZE=${RWORDSIZE}
                        # Only define if set, this is to use #ifdef/#ifndef preprocessors
                        # in code since cmake cannot handle basically any others :(
                        # https://gitlab.kitware.com/cmake/cmake/-/issues/17398
                        $<$<BOOL:${ENABLE_CHEM}>:WRF_CHEM=$<BOOL:${ENABLE_CHEM}>>
                        $<$<BOOL:${ENABLE_CMAQ}>:WRF_CMAQ=$<BOOL:${ENABLE_CMAQ}>>
                        $<$<BOOL:${ENABLE_KPP}>:WRF_KPP=$<BOOL:${ENABLE_KPP}>>
                        $<$<BOOL:${ENABLE_DFI_RADAR}>:WRF_DFI_RADAR=$<BOOL:${ENABLE_DFI_RADAR}>>
                        $<$<BOOL:${ENABLE_TITAN}>:WRF_TITAN=$<BOOL:${ENABLE_TITAN}>>
                        $<$<BOOL:${ENABLE_MARS}>:WRF_MARS=$<BOOL:${ENABLE_MARS}>>
                        $<$<BOOL:${ENABLE_VENUS}>:WRF_VENUS=$<BOOL:${ENABLE_VENUS}>>

                        # Does this actually need to check for EM_CORE (Config.pl:443)
                        $<$<NOT:$<BOOL:${ENABLE_TERRAIN}>>:LANDREAD_STUB=$<NOT:$<BOOL:${ENABLE_TERRAIN}>>>
                        

                        $<$<BOOL:${USE_ALLOCATABLES}>:USE_ALLOCATABLES>
                        $<$<BOOL:${wrfmodel}>:wrfmodel>
                        $<$<BOOL:${GRIB1}>:GRIB1>
                        $<$<BOOL:${INTIO}>:INTIO>
                        $<$<BOOL:${KEEP_INT_AROUND}>:KEEP_INT_AROUND>
                        $<$<BOOL:${LIMIT_ARGS}>:LIMIT_ARGS>
                        #!TODO Always defined - fix the ambiguous english in these BUILD_*_FAST defines
                        BUILD_RRTMG_FAST=$<BOOL:${BUILD_RRTMG_FAST}>
                        BUILD_RRTMK=$<BOOL:${BUILD_RRTMK}>
                        BUILD_SBM_FAST=$<BOOL:${BUILD_SBM_FAST}>
                        SHOW_ALL_VARS_USED=$<BOOL:${SHOW_ALL_VARS_USED}>
                        # Alwasys set
                        NMM_CORE=$<BOOL:${NMM_CORE}>
                        EM_CORE=${EM_CORE}
                        WRFPLUS=$<BOOL:$<STREQUAL:${WRF_CORE},"PLUS">>
                        DA_CORE=$<BOOL:$<OR:$<STREQUAL:${WRF_CORE},"DA_CORE">,$<STREQUAL:${WRF_CORE},"DA_4D_VAR">>>
                        # DFI_RADAR=$<BOOL:${NMM_CORE}>
                        
                        )


# add_custom_command( OUTPUT NONE_ENABLE_CHEM       COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_CHEM      => $<$<BOOL:${ENABLE_CHEM}>:WRF_CHEM=$<BOOL:${ENABLE_CHEM}>> "                )
# add_custom_command( OUTPUT NONE_ENABLE_CMAQ       COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_CMAQ      => $<$<BOOL:${ENABLE_CMAQ}>:WRF_CMAQ=$<BOOL:${ENABLE_CMAQ}>> "                )
# add_custom_command( OUTPUT NONE_ENABLE_KPP        COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_KPP       => $<$<BOOL:${ENABLE_KPP}>:WRF_KPP=$<BOOL:${ENABLE_KPP}>> "                   )
# add_custom_command( OUTPUT NONE_ENABLE_DFI_RADAR  COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_DFI_RADAR => $<$<BOOL:${ENABLE_DFI_RADAR}>:WRF_DFI_RADAR=$<BOOL:${ENABLE_DFI_RADAR}>> " )
# add_custom_command( OUTPUT NONE_ENABLE_TITAN      COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_TITAN     => $<$<BOOL:${ENABLE_TITAN}>:WRF_TITAN=$<BOOL:${ENABLE_TITAN}>> "             )
# add_custom_command( OUTPUT NONE_ENABLE_MARS       COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_MARS      => $<$<BOOL:${ENABLE_MARS}>:WRF_MARS=$<BOOL:${ENABLE_MARS}>> "                )
# add_custom_command( OUTPUT NONE_ENABLE_VENUS      COMMAND ${CMAKE_COMMAND} -E echo  "ENABLE_VENUS     => $<$<BOOL:${ENABLE_VENUS}>:WRF_VENUS=$<BOOL:${ENABLE_VENUS}>> "             )

#!TODO Investigate if this is better than setting -cpp
set( CMAKE_Fortran_PREPROCESS ON )


# Make core target
add_library(
            ${PROJECT_NAME}_Core
              STATIC
              )

# Supplemental to core
add_subdirectory( external )
add_subdirectory( tools    )


add_dependencies(
                  ${PROJECT_NAME}_Core
                  # So many things depend on this that I'm adding a dep here
                  registry_code
                  )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${PROJECT_SOURCE_DIR}
                              ${PROJECT_SOURCE_DIR}/inc
                              ${PROJECT_SOURCE_DIR}/dyn_em
                              ${PROJECT_SOURCE_DIR}/frame

                              # externals
                              ${PROJECT_SOURCE_DIR}/external/io_grib_share
                              ${PROJECT_SOURCE_DIR}/external/io_netcdf
                              ${PROJECT_SOURCE_DIR}/external/io_int
                              ${PROJECT_SOURCE_DIR}/external/ioapi_share
                              $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_grib_share,Fortran_MODULE_DIRECTORY>


                              ${CMAKE_BINARY_DIR}/inc
                              ${CMAKE_BINARY_DIR}/frame
                            )

# Add directly to core
add_subdirectory( phys     )
add_subdirectory( share    )
add_subdirectory( frame    )

# Configure core
set_target_properties( 
                      ${PROJECT_NAME}_Core
                        PROPERTIES
                          # Just dump everything in here
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/
                          Fortran_FORMAT           FREE                          
                      )

target_link_libraries(  ${PROJECT_NAME}_Core
                          PRIVATE
                            ${netCDF_LIBRARIES}
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                            $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                            esmf_time_f90
                        )

# target_include_directories( 
#                             ${PROJECT_NAME}_Core
#                             PRIVATE
                              
#                               )

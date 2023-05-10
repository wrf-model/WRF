cmake_minimum_required( VERSION 3.20 )
cmake_policy( SET CMP0118 NEW )

enable_language( C )
enable_language( CXX )
enable_language( Fortran )

project( WRF )

# list( APPEND CMAKE_MODULE_PATH         )
list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/ ${PROJECT_SOURCE_DIR}/cmake/modules )

include( c_preproc   )
include( m4_preproc  )
include( target_copy )
include( confcheck   )
include( printOption )


################################################################################
##
## Options that can be user configured
##
################################################################################
set( CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/runTemp )

set( OPTIMIZATION_LEVEL "" )
set( WRF_OS             "" )
set( WRF_MACH           "" )

# DO NOT USE OPTION - IT DOES NOT WORK AS ANTICIPATED EVEN WHEN CLEARING CACHE - YOU HAVE BEEN WARNED
# Mode configuration
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

# WRF Nesting selection
set( WRF_NESTING_OPTIONS 
      # Options listed here
      NONE
      BASIC #TODO Figure out what exactly this does aside from potentially stub things Config.pl:890
      MOVES
      VORTEX
      )

# Set default WRF_NESTING
set( WRF_NESTING "" CACHE STRING "WRF_NESTING" )
list( GET WRF_NESTING_OPTIONS 0 WRF_NESTING )


set( USE_DOUBLE  OFF CACHE BOOL "USE_DOUBLE"  ) # I believe this is the r8/real8 promotion
set( USE_MPI     OFF CACHE BOOL "USE_MPI"     )
set( USE_OPENMP  OFF CACHE BOOL "USE_OPENMP"  )


set( ENABLE_CHEM       OFF CACHE BOOL "ENABLE_CHEM"      )
set( ENABLE_CMAQ       OFF CACHE BOOL "ENABLE_CMAQ"      )
set( ENABLE_KPP        OFF CACHE BOOL "ENABLE_KPP"       )
set( ENABLE_DFI_RADAR  OFF CACHE BOOL "ENABLE_DFI_RADAR" )
set( ENABLE_TITAN      OFF CACHE BOOL "ENABLE_TITAN"     )
set( ENABLE_MARS       OFF CACHE BOOL "ENABLE_MARS"      )
set( ENABLE_VENUS      OFF CACHE BOOL "ENABLE_VENUS"     )
set( ENABLE_VENUS      OFF CACHE BOOL "ENABLE_VENUS"     )
set( ENABLE_TERRAIN    OFF CACHE BOOL "ENABLE_TERRAIN"   )
set( ENABLE_CLM        OFF CACHE BOOL "ENABLE_CLM"       )

# What do these defines even do if they are always on????
set( USE_ALLOCATABLES     ON CACHE BOOL "USE_ALLOCATABLES"    )
set( wrfmodel             ON CACHE BOOL "wrfmodel"            )
set( GRIB1                ON CACHE BOOL "GRIB1"               )
set( INTIO                ON CACHE BOOL "INTIO"               )
set( KEEP_INT_AROUND      ON CACHE BOOL "KEEP_INT_AROUND"     )
set( LIMIT_ARGS           ON CACHE BOOL "LIMIT_ARGS"          )
set( WRFIO_NCD_NO_LARGE_FILE_SUPPORT  OFF CACHE BOOL "WRFIO_NCD_NO_LARGE_FILE_SUPPORT" )

set( BUILD_RRTMG_FAST    OFF CACHE BOOL "BUILD_RRTMG_FAST"    )
set( BUILD_RRTMK         OFF CACHE BOOL "BUILD_RRTMK"         )
set( BUILD_SBM_FAST       ON CACHE BOOL "BUILD_SBM_FAST"      )
set( SHOW_ALL_VARS_USED  OFF CACHE BOOL "SHOW_ALL_VARS_USED"  )



# Not cached, cannot be changed, do not touch
set( USE_M4               ON )
# Same thing -what do these defines even do if they are always on????
set( NMM_CORE            OFF )
set( NETCDF               ON )


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

set( MOVE_NESTS 0 )
# Far easier to write this one as normal logic rather than generator expression
if( ${WRF_NESTING} STREQUAL "MOVES" OR ${WRF_NESTING} STREQUAL "VORTEX" )
  set( MOVE_NESTS 1 )
endif()

if ( ${ENABLE_KPP} AND NOT ${ENABLE_CHEM} )
  message( WARNING "ENABLE_KPP requires ENABLE_CHEM but is not set, ignoring" )
endif()



print_option( WRF_CORE         20 ${BOLD_CYAN} )
print_option( WRF_NESTING      20 ${BOLD_CYAN} )

print_option( USE_DOUBLE       20 )
print_option( USE_MPI          20 )
print_option( USE_OPENMP       20 )


print_option( ENABLE_CHEM      20 )
print_option( ENABLE_CMAQ      20 )
print_option( ENABLE_KPP       20 )
print_option( ENABLE_DFI_RADAR 20 )
print_option( ENABLE_TITAN     20 )
print_option( ENABLE_MARS      20 )
print_option( ENABLE_VENUS     20 )
print_option( ENABLE_VENUS     20 )
print_option( ENABLE_TERRAIN   20 )
print_option( ENABLE_CLM       20 )

print_option( USE_ALLOCATABLES 20 )
print_option( wrfmodel         20 )
print_option( GRIB1            20 )
print_option( INTIO            20 )
print_option( KEEP_INT_AROUND  20 )
print_option( LIMIT_ARGS       20 )

print_option( BUILD_RRTMG_FAST   20 )
print_option( BUILD_RRTMK        20 )
print_option( BUILD_SBM_FAST     20 )
print_option( SHOW_ALL_VARS_USED 20 )

print_option( WRFIO_NCD_NO_LARGE_FILE_SUPPORT 36 )

################################################################################
##
## Configuration checks for features & intrinsices
##
################################################################################
add_subdirectory( confcheck )

################################################################################
##
## Now find packages that cross-compilation is potentially handled
##
################################################################################
if ( ${USE_M4} )
  find_program( 
                M4_PROGRAM
                m4
                REQUIRED
                )
endif()

# Provided by cmake natively
find_package( ZLIB   REQUIRED )
find_package( CURL   REQUIRED )

# HDF5 has some funky weirdness between versions where the casing has changed
find_package( HDF5   REQUIRED )

find_package( Jasper REQUIRED )

if ( ${ENABLE_TERRAIN} )
  find_package( RPC )
endif()

if ( ${ENABLE_CLM} )
  # Will need our own finder
  # find_package( CLM REQUIRED )
endif()

# Will need our own finder
# find_package( GPFS   REQUIRED )

# Included is a lightweight finder, but we really should switch to using UniData's netCDF cmake config
find_package( netCDF REQUIRED )

# Make use of version checking here and not in find_package for previous versions that did not use cmake
if ( NOT netCDF_VERSION GREATER_EQUAL "4.1.3" )
  message( FATAL "Please make sure NETCDF version is 4.1.3 or later. " )
  
endif()

if ( USE_MPI )
  find_package( MPI REQUIRED COMPONENTS Fortran C )
  add_compile_definitions( USE_MPI=1 DM_PARALLEL=1 )
endif()

if ( USE_OPENMP )
  find_package( OpenMP REQUIRED COMPONENTS Fortran C )
  add_compile_definitions( USE_OPENMP=1 SM_PARALLEL=1 )
endif()

################################################################################
##
## Adjust flags based on compiler and linker used
##
################################################################################
# https://stackoverflow.com/a/53155812
set( Fortran_COMPILER_ID ${CMAKE_Fortran_COMPILER_ID} )
# message( STATUS "Set Fortran_COMPILER_ID to : ${Fortran_COMPILER_ID}" )


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
                        $<$<BOOL:${ENABLE_CHEM}>:BUILD_CHEM=$<BOOL:${ENABLE_CHEM}>>
                        $<$<BOOL:${ENABLE_CMAQ}>:WRF_CMAQ=$<BOOL:${ENABLE_CMAQ}>>
                        $<$<AND:$<BOOL:${ENABLE_CHEM}>,$<BOOL:${ENABLE_KPP}>>:WRF_KPP=$<BOOL:${ENABLE_KPP}>>
                        $<$<BOOL:${ENABLE_DFI_RADAR}>:WRF_DFI_RADAR=$<BOOL:${ENABLE_DFI_RADAR}>>
                        $<$<BOOL:${ENABLE_TITAN}>:WRF_TITAN=$<BOOL:${ENABLE_TITAN}>>
                        $<$<BOOL:${ENABLE_MARS}>:WRF_MARS=$<BOOL:${ENABLE_MARS}>>
                        $<$<BOOL:${ENABLE_VENUS}>:WRF_VENUS=$<BOOL:${ENABLE_VENUS}>>
                        $<$<BOOL:${ENABLE_CLM}>:WRF_USE_CLM=$<BOOL:${ENABLE_CLM}>>

                        $<$<BOOL:${netCDF_NC4}>:NETCDF_classic=$<BOOL:${netCDF_NC4}>>
                        $<$<BOOL:${WRFIO_NCD_NO_LARGE_FILE_SUPPORT}>:WRFIO_NCD_NO_LARGE_FILE_SUPPORT=$<BOOL:${WRFIO_NCD_NO_LARGE_FILE_SUPPORT}>>
                        # TODO Figure out how these should be set
                        # DUSE_NETCDF4_FEATURES -DWRFIO_NCD_LARGE_FILE_SUPPORT

                        $<$<BOOL:${MPI2_SUPPORT}>:MPI2_SUPPORT=$<BOOL:${MPI2_SUPPORT}>>
                        $<$<BOOL:${MPI2_THREAD_SUPPORT}>:MPI2_THREAD_SUPPORT=$<BOOL:${MPI2_THREAD_SUPPORT}>>

                        # Could simplify logic to just check if RPC is available but to be explicit
                        # Does this actually need to check for EM_CORE (Config.pl:443)
                        # not enable terran or not rpc_found do 
                        # not ( enable terrain and rpc_found )
                        $<$<NOT:$<AND:$<BOOL:${ENABLE_TERRAIN}>,$<BOOL:${RPC_FOUND}>>>:LANDREAD_STUB>
                        $<$<BOOL:${ENABLE_TERRAIN}>:TERRAIN_AND_LANDUSE>
                        

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
                        NMM_MAX_DIM=2600
                        NETCDF
                        
                        #!TODO Change this to a confcheck
                        NONSTANDARD_SYSTEM_SUBR
                        
                        EM_CORE=${EM_CORE}
                        WRFPLUS=$<BOOL:$<STREQUAL:${WRF_CORE},"PLUS">>
                        DA_CORE=$<BOOL:$<OR:$<STREQUAL:${WRF_CORE},"DA_CORE">,$<STREQUAL:${WRF_CORE},"DA_4D_VAR">>>
                        # DFI_RADAR=$<BOOL:${NMM_CORE}>

                        # Nesting options
                        $<$<BOOL${MOVE_NESTS}>:MOVE_NESTS>
                        $<$<BOOL:$<STREQUAL:${WRF_NESTING},"VORTEX">>:VORTEX_CENTER>

                        # Configuration checks
                        $<$<NOT:$<BOOL:${Fortran_2003_IEEE}>>:NO_IEEE_MODULE>
                        $<$<NOT:$<BOOL:${Fortran_2003_ISO_C}>>:NO_ISO_C_SUPPORT>
                        # If flush fails, check if we can fall back to fflush, and if not no support
                        $<$<NOT:$<BOOL:${Fortran_2003_FLUSH}>>:$<IF:$<BOOL:${Fortran_2003_FFLUSH}>,USE_FFLUSH,NO_FLUSH_SUPPORT>>
                        $<$<NOT:$<BOOL:${Fortran_2003_GAMMA}>>:NO_GAMMA_SUPPORT>

                        #!TODO Leaving as is in WRF for now but investigate why we don't do this
                        # https://stackoverflow.com/a/1035713
                        # If fseeko64 succeeds, use that, else check if we can fall back to fseeko, and if not just use fseek
                        $<IF:$<BOOL:${FSEEKO64}>,FSEEKO64_OK,$<IF:$<BOOL:${FSEEKO}>,FSEEKO_OK,FSEEK_OK>>
                        
                        
                        MPI2_SUPPORT
                        MPI2_THREAD_SUPPORT
                        
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

# Supplemental to core, or rather should be, some stuff in external is legitimately part of WRF and others
# are source code from truly external repositories - albeit old versions
add_subdirectory( external )
add_subdirectory( tools    )


add_dependencies(
                  ${PROJECT_NAME}_Core
                  # So many things depend on this that I'm adding a dep here
                  registry_code
                  # "External" io libs
                  esmf_time_f90
                  
                  # io_pio
                  
                  # io_netcdf
                  # io_netcdfpar
                  # io_pnetcdf
                  # io_phdf5
                  io_grib1
                  io_grib_share
                  g2lib
                  bacio-1.3
                  
                  RSL_LITE
                  fftpack5
                  # add_dependencies() does not support generator expressions
                  # $<TARGET_NAME_IF_EXISTS:atm_ocn>
                  # $<TARGET_NAME_IF_EXISTS:io_adios2>
                  # $<TARGET_NAME_IF_EXISTS:io_esmf>
                  )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${PROJECT_SOURCE_DIR}
                              ${PROJECT_SOURCE_DIR}/inc
                              ${PROJECT_SOURCE_DIR}/dyn_em
                              # ${PROJECT_SOURCE_DIR}/frame

                              # externals
                              ${PROJECT_SOURCE_DIR}/external/io_grib_share
                              ${PROJECT_SOURCE_DIR}/external/io_netcdf
                              ${PROJECT_SOURCE_DIR}/external/io_int
                              ${PROJECT_SOURCE_DIR}/external/grib2
                              ${PROJECT_SOURCE_DIR}/external/ioapi_share

                              $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_grib_share,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:g2lib,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:bacio-1.3,Fortran_MODULE_DIRECTORY>


                              ${CMAKE_BINARY_DIR}/inc
                              ${CMAKE_BINARY_DIR}/frame
                            )

# Add directly to core
add_subdirectory( phys     )
add_subdirectory( share    )
add_subdirectory( frame    )

if ( ${WRF_CHEM} )
  add_subdirectory( chem     )
endif()

add_subdirectory( dyn_em   )

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
                            g2lib
                            bacio-1.3

                            # This will add in target dependencies if they exist
                            $<TARGET_NAME_IF_EXISTS:atm_ocn>
                            $<TARGET_NAME_IF_EXISTS:io_adios2>
                            $<TARGET_NAME_IF_EXISTS:io_esmf>
                            $<TARGET_NAME_IF_EXISTS:Jasper::Jasper>
                      )

# target_include_directories( 
#                             ${PROJECT_NAME}_Core
#                             PRIVATE
                              
#                               )

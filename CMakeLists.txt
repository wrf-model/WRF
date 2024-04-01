cmake_minimum_required( VERSION 3.20 )
cmake_policy( SET CMP0118 NEW )

enable_language( C )
enable_language( CXX )
enable_language( Fortran )

project( WRF )
set( EXPORT_NAME ${PROJECT_NAME} )

if ( DEFINED CMAKE_TOOLCHAIN_FILE )
  set( WRF_CONFIG ${CMAKE_TOOLCHAIN_FILE} )
  # message( STATUS "Loading configuration file... : ${WRF_CONFIG}" )
  # include( ${WRF_CONFIG} )
endif()

# list( APPEND CMAKE_MODULE_PATH         )
list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/ ${PROJECT_SOURCE_DIR}/cmake/modules )

# Use link paths as rpaths
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )
set( CMAKE_Fortran_PREPROCESS          ON )

# This is always set
list( APPEND CMAKE_C_PREPROCESSOR_FLAGS -P -nostdinc -traditional )

include( CMakePackageConfigHelpers )
include( CheckIPOSupported )
include( c_preproc   )
include( m4_preproc  )
include( target_copy )
include( confcheck   )
include( gitinfo     )
include( printOption )
include( wrf_case_setup )
include( wrf_get_version )

check_ipo_supported( RESULT IPO_SUPPORT )

# First grab git info
wrf_git_commit(
                RESULT_VAR        GIT_VERSION
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                )

# Configure file for usage
configure_file(
                ${PROJECT_SOURCE_DIR}/cmake/template/commit_decl.cmake
                ${PROJECT_BINARY_DIR}/inc/commit_decl
                @ONLY
                )

# Grab version info
wrf_get_version( ${PROJECT_SOURCE_DIR}/README )

################################################################################
##
## Options that can be user configured
##
################################################################################
# Mode configuration

set( OPTIMIZATION_LEVEL "" )
set( WRF_OS             "" )
set( WRF_MACH           "" )

if ( "${CMAKE_BUILD_TYPE}" STREQUAL "" )
  set( CMAKE_BUILD_TYPE Release )
  message( STATUS "Set default build type to ${CMAKE_BUILD_TYPE}" )
endif()
################################################################################
## WRF Core selection
################################################################################
set( WRF_CORE_OPTIONS
      # Options listed here
      ARW
      CONVERT # This exists in compile but not configure
      DA      # Var directory
      DA_4D_VAR
      PLUS
      )

set( WRF_CORE "" CACHE STRING "WRF_CORE" )
if ( "${WRF_CORE}" STREQUAL "" )
  # Set default WRF_CORE
  list( GET WRF_CORE_OPTIONS 0 WRF_CORE )
endif()

################################################################################
## WRF Nesting selection
################################################################################
set( WRF_NESTING_OPTIONS
      # Options listed here
      NONE
      BASIC
      MOVES
      VORTEX
      )

set( WRF_NESTING "" CACHE STRING "WRF_NESTING" )
if ( "${WRF_NESTING}" STREQUAL "" )
  # Set default WRF_NESTING
  list( GET WRF_NESTING_OPTIONS 0 WRF_NESTING )
endif()

################################################################################
## WRF Case selection
##!TODO Maybe one day make it so this doesn't need to be a selection and all are
##      always built?
################################################################################
set( WRF_CASE_OPTIONS
      # Options listed here
      EM_REAL # make this the default
      # EM_IDEAL # Technically doable but does anyone build this? It is not a target option in make
      EM_FIRE
      EM_SCM_XY
      EM_TROPICAL_CYCLONE
      EM_HELDSUAREZ

      # These are sub-categories of ideal - Keep these lower in the list
      EM_B_WAVE # Keep this one here as it is used to check index
      EM_GRAV2D_X
      EM_HILL2D_X
      EM_LES
      EM_QUARTER_SS
      EM_SEABREEZE2D_X
      EM_CONVRAD
      EM_SQUALL2D_X
      EM_SQUALL2D_Y
      )

set( WRF_CASE "" CACHE STRING "WRF_CASE" )
if ( "${WRF_CASE}" STREQUAL "" )
  # Set default WRF_CASE
  list( GET WRF_CASE_OPTIONS 0 WRF_CASE )
endif()

# DO NOT USE OPTION - IT DOES NOT WORK AS ANTICIPATED EVEN WHEN CLEARING CACHE - YOU HAVE BEEN WARNED
# If you add anything here, the description should be the name itself - this helps the configuration script
set( USE_DOUBLE  OFF CACHE BOOL "USE_DOUBLE"  )
set( USE_MPI     OFF CACHE BOOL "USE_MPI"     )
set( USE_OPENMP  OFF CACHE BOOL "USE_OPENMP"  )
set( USE_HDF5    OFF CACHE BOOL "USE_HDF5"    )
set( USE_JASPER  OFF CACHE BOOL "USE_JASPER"  )
set( USE_PIO     OFF CACHE BOOL "USE_PIO"     )
set( USE_IPO     OFF CACHE BOOL "USE_IPO"     )


set( ENABLE_CHEM       OFF CACHE BOOL "ENABLE_CHEM"      )
set( ENABLE_CMAQ       OFF CACHE BOOL "ENABLE_CMAQ"      )
set( ENABLE_CTSM       OFF CACHE BOOL "ENABLE_CTSM"      )
set( ENABLE_DFI_RADAR  OFF CACHE BOOL "ENABLE_DFI_RADAR" )
set( ENABLE_HYDRO      OFF CACHE BOOL "ENABLE_HYDRO"     )
set( ENABLE_KPP        OFF CACHE BOOL "ENABLE_KPP"       )
set( ENABLE_MARS       OFF CACHE BOOL "ENABLE_MARS"      )
set( ENABLE_TERRAIN    OFF CACHE BOOL "ENABLE_TERRAIN"   )
set( ENABLE_TITAN      OFF CACHE BOOL "ENABLE_TITAN"     )
set( ENABLE_VENUS      OFF CACHE BOOL "ENABLE_VENUS"     )

# What do these defines even do if they are always on????
set( USE_ALLOCATABLES     ON CACHE BOOL "USE_ALLOCATABLES"    )
set( wrfmodel             ON CACHE BOOL "wrfmodel"            )
set( GRIB1                ON CACHE BOOL "GRIB1"               )
set( INTIO                ON CACHE BOOL "INTIO"               )
set( KEEP_INT_AROUND      ON CACHE BOOL "KEEP_INT_AROUND"     )
set( LIMIT_ARGS           ON CACHE BOOL "LIMIT_ARGS"          )

# Toggles, how do we want to best address these options? Environment vars are not
# the best
set( WRFIO_NCD_NO_LARGE_FILE_SUPPORT  OFF CACHE BOOL "WRFIO_NCD_NO_LARGE_FILE_SUPPORT" )
set( FORCE_NETCDF_CLASSIC OFF CACHE BOOL "FORCE_NETCDF_CLASSIC" )
set( BUILD_RRTMG_FAST     OFF CACHE BOOL "BUILD_RRTMG_FAST"     )
set( BUILD_RRTMK          OFF CACHE BOOL "BUILD_RRTMK"          )
set( BUILD_SBM_FAST        ON CACHE BOOL "BUILD_SBM_FAST"       )
set( SHOW_ALL_VARS_USED   OFF CACHE BOOL "SHOW_ALL_VARS_USED"   )


# TODO investigate if this needs set
# Not cached, cannot be changed, do not touch
set( USE_M4               ON )
# Same thing -what do these defines even do if they are always on????
set( NMM_CORE            OFF )
set( NETCDF               ON )


# Special internal flag for profiling compilation
set( PROFILE_COMPILATION  OFF CACHE BOOL "PROFILE_COMPILATION" )


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


########################

################################################################################
##
## Load options selected and any ancillary logic
##
################################################################################

# Check WRF options
if ( NOT ${WRF_CORE} IN_LIST WRF_CORE_OPTIONS )
  message( FATAL_ERROR "WRF Core option not recognized : ${WRF_CORE}" )
endif()

if ( NOT ${WRF_NESTING} IN_LIST WRF_NESTING_OPTIONS )
  message( FATAL_ERROR "WRF Nesting option not recognized : ${WRF_NESTING}" )
endif()

if ( NOT ${WRF_CASE} IN_LIST WRF_CASE_OPTIONS )
  message( FATAL_ERROR "WRF Case option not recognized : ${WRF_CASE}" )
endif()

# Handle selection
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


# Additional information on the type of case we are compiling
string( TOLOWER ${WRF_CASE} WRF_CASE_FOLDER )
string( REPLACE "em_" "" WRF_CASE_MODULE ${WRF_CASE_FOLDER} )

# Find if it is a specialized ideal case or general
list( FIND WRF_CASE_OPTIONS EM_B_WAVE   START_GENERAL_IDEAL_CASE_IDX )
list( FIND WRF_CASE_OPTIONS ${WRF_CASE} CURRENT_CASE_IDX )
set( WRF_GENERAL_IDEAL_CASE TRUE )
if ( ${CURRENT_CASE_IDX} LESS ${START_GENERAL_IDEAL_CASE_IDX} )
  set( WRF_GENERAL_IDEAL_CASE FALSE )
endif()

if ( NOT ${WRFIO_NCD_NO_LARGE_FILE_SUPPORT} )
  message( STATUS "netCDF large file support not suppressed, if available it will be used" )
endif()

# Hydro option requires MPI
if ( ${ENABLE_HYDRO} AND NOT ${USE_MPI} )
  message( STATUS "WRF Hydro requires MPI usage" )
  set( USE_MPI ON CACHE BOOL "Required by configuration" FORCE )
endif()

# Handle double promotion - doing this here instead of from config.cmake toolchain
# file since the double promotion logic is a nightmare
list( FIND WRF_CORE_OPTIONS ${WRF_CORE} CURRENT_WRF_CORE_IDX )
list( FIND WRF_CORE_OPTIONS "DA"        START_DA_IDX )
# DA + WRF PLUS cores require double precision
if ( ${CURRENT_WRF_CORE_IDX} GREATER_EQUAL ${START_DA_IDX} AND NOT ${USE_DOUBLE} )
  # if ( # Apparently set regardless of compiler
  #     ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR
  #     ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Fujitsu" )
  message( STATUS "DA and PLUS Core builds require double precision" )
  set( USE_DOUBLE ON CACHE BOOL "Required by configuration" FORCE )
  # endif()
endif()

if ( ${USE_DOUBLE} )
  set( RWORDSIZE    8     )
  if ( ${BUILD_SBM_FAST} )
    set( BUILD_SBM_FAST OFF CACHE BOOL "Required by configuration" FORCE )
    message( STATUS "BUILD_SBM_FAST does not support double, turning off" )
  endif()
else()
  set( RWORDSIZE    4     )
endif()

math( EXPR RWORDSIZE_B "8 * ${RWORDSIZE}" )


# Check if IPO usage
if ( ${USE_IPO} )
  if ( NOT ${IPO_SUPPORT} )
    message( STATUS "IPO/LTO not supported, request ignored" )
    set( USE_IPO OFF CACHE BOOL "Required by configuration" FORCE )
  endif()
endif()

################################################################################
##
## Now find packages that cross-compilation is potentially handled
##
################################################################################
# If nesting is enabled, DM_PARALLEL must be set, but if this is a non-MPI compilation
# we must stub its usage
list( FIND WRF_NESTING_OPTIONS ${WRF_NESTING} CURRENT_NESTING_IDX )

# If MPI or nesting
set( USE_RSL_LITE OFF )
if ( ${USE_MPI} )
  # Through ***MUCH*** debugging, if utilizing MPI_<LANG>_COMPILER
  # https://cmake.org/cmake/help/latest/module/FindMPI.html#variables-for-locating-mpi
  # the find logic makes a mess of things by utilizing <mpi> -show[me]
  # Which may or may not get polluted by the environment
  # It still technically finds MPI but the output is nonintuitive
  # saying things like hdf5 or pthread
  find_package( MPI REQUIRED COMPONENTS Fortran C )
  add_compile_definitions(
                          USE_MPI=1
                          DM_PARALLEL
                          )

  if ( DEFINED WRF_MPI_Fortran_FLAGS AND NOT "${WRF_MPI_Fortran_FLAGS}" STREQUAL "" )
    add_compile_options(
                        $<$<COMPILE_LANGUAGE:Fortran>:${WRF_MPI_Fortran_FLAGS}>
                        )
  endif()

  if ( DEFINED WRF_MPI_C_FLAGS AND NOT "${WRF_MPI_C_FLAGS}" STREQUAL "" )
    add_compile_options(
                        $<$<COMPILE_LANGUAGE:C>:${WRF_MPI_C_FLAGS}>
                        )
  endif()

  # Check if MPI in all its glory has forced IPO down our throats due to hard-coding the wrapper flags
  # https://www.open-mpi.org/faq/?category=mpi-apps#why-no-rpath LOL!
  # Quote "The default installation of Open MPI tries very hard to not include any non-essential flags in the wrapper compilers"
  # Okay, yea sure. Maybe it's the distro's lib config that does add all the bloatware flags
  if ( NOT ${USE_IPO} )
    # get compile info
    message( STATUS "Checking if MPI requires IPO" )
    foreach( IPO_FLAG IN LISTS CMAKE_Fortran_COMPILE_OPTIONS_IPO )
      string( FIND "${MPI_Fortran_COMPILE_OPTIONS}" ${IPO_FLAG} MPI_FORCE_IPO )
      # Note we are not using IN_LIST since certain IPO settings might not exactly match (e.g.  -flto vs -flto=auto)
      if ( NOT ${MPI_FORCE_IPO} EQUAL -1 )
        # An IPO flag was found
        if ( ${IPO_SUPPORT} )
          message( STATUS "NOTE: ${MPI_Fortran_COMPILER} requires IPO flags be enabled, forcing USE_IPO=ON" )
          set( USE_IPO ON CACHE BOOL "Required by MPI" FORCE )
          break()
        else()
          message( FATAL_ERROR "${MPI_Fortran_COMPILER} requires IPO support but selected compiler does not support it, would fail to link" )
        endif()
      endif()
    endforeach()
  endif()

  set( USE_RSL_LITE ON )
# We know NONE is the zero index so compare against that
elseif( ${CURRENT_NESTING_IDX} GREATER 0 )
  add_compile_definitions(
                          DM_PARALLEL
                          STUBMPI
                          )
  set( USE_RSL_LITE ON )
endif()

if ( ${USE_OPENMP} )
  find_package( OpenMP REQUIRED COMPONENTS Fortran C )
  add_compile_definitions( USE_OPENMP=1 SM_PARALLEL )
endif()

if ( ${USE_M4} )
  find_program(
                M4_PROGRAM
                m4
                REQUIRED
                )
  set( M4_FLAGS ${WRF_M4_FLAGS} -Uinclude -Uindex -Ulen )
endif()



# HDF5 has some funky weirdness between versions where the casing has changed
# Optional
if ( ${USE_HDF5} )
  find_package( HDF5 )
endif()

# Optional for grib2
if ( ${USE_JASPER} )
  find_package( Jasper 1.900.1...<1.900.24 )
endif()

# Optional
if ( ${USE_PIO} )
  find_package( PIO QUIET )
endif()

if ( ${ENABLE_TERRAIN} )
  find_package( RPC )
endif()

if ( ${ENABLE_CTSM} )
  # Will need our own finder
  # find_package( CTSM REQUIRED )
endif()

# Will need our own finder
# find_package( GPFS   REQUIRED )

# Included is a lightweight finder, but we really should switch to using UniData's netCDF cmake config
# The reason these are two separate and not COMPONENTS of one larger package is because that is the way UniData
# has laid out the cmake configs for each respective package
find_package( netCDF         REQUIRED )
find_package( netCDF-Fortran REQUIRED )

# Make use of version checking here and not in find_package for previous versions that did not use cmake
if ( ( NOT netCDF_VERSION GREATER_EQUAL "4.1.3" ) OR ( NOT netCDF-Fortran_VERSION GREATER_EQUAL "4.1.3" ) )
  message( FATAL "Please make sure NETCDF versions are 4.1.3 or later. " )
endif()

find_package( pnetCDF QUIET )

# Attempt to find zlib packaged with netcdf first
set( ZLIB_ROOT ${netCDF_PREFIX} )
find_package( ZLIB   REQUIRED )
find_package( CURL   REQUIRED )

################################################################################
##
## Print out final set of options to be used
## DO NOT MODIFY OPTIONS BEYOND THIS POINT
##
################################################################################

if ( DEFINED CMAKE_TOOLCHAIN_FILE )
  print_option( WRF_CONFIG         20 ${BOLD_CYAN} )
endif()

print_option( CMAKE_BUILD_TYPE 20 ${BOLD_CYAN} )


print_option( WRF_CORE         20 ${BOLD_CYAN} )
print_option( WRF_NESTING      20 ${BOLD_CYAN} )
print_option( WRF_CASE         20 ${BOLD_CYAN} )


print_option( USE_DOUBLE       20 )
print_option( USE_MPI          20 )
print_option( USE_OPENMP       20 )
print_option( USE_IPO          20 )

print_option( ENABLE_CHEM      20 )
print_option( ENABLE_CLM       20 )
print_option( ENABLE_CMAQ      20 )
print_option( ENABLE_DFI_RADAR 20 )
print_option( ENABLE_HYDRO     20 )
print_option( ENABLE_KPP       20 )
print_option( ENABLE_MARS      20 )
print_option( ENABLE_TERRAIN   20 )
print_option( ENABLE_TITAN     20 )
print_option( ENABLE_VENUS     20 )

print_option( USE_ALLOCATABLES 20 )
print_option( wrfmodel         20 )
print_option( GRIB1            20 )
print_option( INTIO            20 )
print_option( KEEP_INT_AROUND  20 )
print_option( LIMIT_ARGS       20 )

print_option( FORCE_NETCDF_CLASSIC 20 )
print_option( BUILD_RRTMG_FAST     20 )
print_option( BUILD_RRTMK          20 )
print_option( BUILD_SBM_FAST       20 )
print_option( SHOW_ALL_VARS_USED   20 )

print_option( WRFIO_NCD_NO_LARGE_FILE_SUPPORT 36 )

################################################################################
##
## Set any global cmake options decided by particular configuration
##
################################################################################
set( CMAKE_INTERPROCEDURAL_OPTIMIZATION ${USE_IPO} )

################################################################################
##
## Configuration checks for features & intrinsices
##
################################################################################
add_subdirectory( confcheck )

################################################################################
##
## Adjust flags based on compiler and linker used
##
################################################################################

# https://stackoverflow.com/a/47927921
# Define compile options to be inherited for directories
define_property(
                SOURCE
                PROPERTY   COMPILE_FLAGS
                INHERITED
                BRIEF_DOCS "brief-doc"
                FULL_DOCS  "full-doc"
                )

define_property(
                DIRECTORY
                PROPERTY   COMPILE_FLAGS
                INHERITED
                BRIEF_DOCS "brief-doc"
                FULL_DOCS  "full-doc"
                )

# Get current build type flags and put them in there
if ( "${CMAKE_BUILD_TYPE}" STREQUAL "Release" )
  set_directory_properties(
                            PROPERTIES
                              COMPILE_FLAGS
                              $<$<COMPILE_LANGUAGE:Fortran>:${WRF_FCOPTIM}>
                            )
# else()
#   # Assume no optimization
#   set_directory_properties(
#                             PROPERTIES
#                               COMPILE_FLAGS
#                               $<$<COMPILE_LANGUAGE:Fortran>:${WRF_FCNOOPT}>
#                             )
endif()


# This is really ugly but such is the cost of supporting many ways to say the same thing
# https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_ID.html
add_compile_options(
                    # Use "" and ; specifically to evaluate correctly
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,Absoft>:>"                                 #@ Absoft Fortran
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,ADSP>:>"                                   #@ Analog VisualDSP++
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,AppleClang>:>"                             #@ Apple Clang
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,ARMCC>:>"                                  #@ ARM Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,ARMClang>:>"                               #@ ARM Compiler based on Clang
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,Bruce>:>"                                  #@ Bruce C Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,CCur>:>"                                   #@ Concurrent Fortran
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,Clang>:>"                                  #@ LLVM Clang
                    "$<$<COMPILE_LANG_AND_ID:Fortran,Cray>:-s;integer32;-s;real${RWORDSIZE_B}>" #@ Cray Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,Embarcadero>:>"                            #@ Embarcadero
                    "$<$<AND:$<BOOL:${USE_DOUBLE}>,$<COMPILE_LANG_AND_ID:Fortran,Flang>>:-fdefault-real-${RWORDSIZE}>"                  #@ Classic Flang Fortran Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,LLVMFlang>:>"                              #@ LLVM Flang Fortran Compiler
                    "$<$<COMPILE_LANG_AND_ID:Fortran,Fujitsu>:-CcdRR${RWORDSIZE}>"              #@ Fujitsu HPC compiler (Trad mode)
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,FujitsuClang>:>"                           #@ Fujitsu HPC compiler (Clang mode)
                    "$<$<COMPILE_LANG_AND_ID:Fortran,G95>:-r${RWORDSIZE};-i4>"                                    #@ G95 Fortran
                    "$<$<AND:$<BOOL:${USE_DOUBLE}>,$<COMPILE_LANG_AND_ID:Fortran,GNU>>:-fdefault-real-${RWORDSIZE}>"         #@ GNU Compiler Collection
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,GHS>:>"                                    #@ Green Hills Software
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,HP>:>"                                     #@ Hewlett-Packard Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,IAR>:>"                                    #@ IAR Systems
                    "$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-real-size;${RWORDSIZE_B};-i4>"     #@ Intel Classic Compiler
                    "$<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-real-size;${RWORDSIZE_B};-i4>" #@ Intel LLVM-Based Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,LCC>:>"                                    #@ MCST Elbrus C/C++/Fortran Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,MSVC>:>"                                   #@ Microsoft Visual Studio
                    "$<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-r${RWORDSIZE};-i4>"                #@ NVIDIA HPC Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,NVIDIA>:>"                                 #@ NVIDIA CUDA Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,OpenWatcom>:>"                             #@ Open Watcom
                    "$<$<COMPILE_LANG_AND_ID:Fortran,PGI>:-r${RWORDSIZE};-i4>"                  #@ The Portland Group
                    "$<$<COMPILE_LANG_AND_ID:Fortran,PathScale>:-r${RWORDSIZE};-i4>"            #@ PathScale
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,SDCC>:>"                                   #@ Small Device C Compiler
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,SunPro>:>"                                 #@ Oracle Solaris Studio
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,Tasking>:>"                                #@ Tasking Compiler Toolsets
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,TI>:>"                                     #@ Texas Instruments
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,TinyCC>:>"                                 #@ Tiny C Compiler
                    "$<$<COMPILE_LANG_AND_ID:Fortran,XL>:-qrealsize=${RWORDSIZE};-qintsize=4>"  #@ IBM XL
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,XLClang>:>"                                #@ IBM Clang-based XL
                    # "$<$<COMPILE_LANG_AND_ID:Fortran,IBMClang>:>"                               #@ IBM LLVM-based Compiler
                    # Todo find how to handle default selection or add new compiler IDs
                    # unknown how to add support for sxf90

                    # line lengths
                    "$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-ffree-line-length-none>"                #@ GNU Compiler Collection
                    )


# https://stackoverflow.com/a/53155812
# set( Fortran_COMPILER_ID ${CMAKE_Fortran_COMPILER_ID} )
# message( STATUS "Set Fortran_COMPILER_ID to : ${Fortran_COMPILER_ID}" )


# Whole project flags
add_compile_options(
                    # $<$<COMPILE_LANGUAGE:Fortran>:-cpp>
                    # Use "" and ; specifically to evaluate correctly
                    "$<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-diag-disable;6843>"
                    $<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<VERSION_GREATER_EQUAL:${CMAKE_Fortran_COMPILER_VERSION},10>>:-fallow-argument-mismatch>
                    $<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<VERSION_GREATER_EQUAL:${CMAKE_Fortran_COMPILER_VERSION},10>>:-fallow-invalid-boz>
                    $<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,$<VERSION_GREATER_EQUAL:${CMAKE_Fortran_COMPILER_VERSION},10>>:-ffree-line-length-none>

                    # $<AND:$<COMPILE_LANGUAGE:Fortran>,$<STREQUAL:${Fortran_COMPILER_ID},Intel>:-diag-disable;6843>
                    )

if ( ${PROFILE_COMPILATION} )
  message( STATUS "Attemping to add compilation profiling..." )
  add_compile_options(
                      $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-ftime-report>
                      )
endif()

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
                        $<$<BOOL:${ENABLE_HYDRO}>:WRF_HYDRO=$<BOOL:${ENABLE_HYDRO}>>

                        # Because once again we need two defines to control one thing
                        $<$<BOOL:${ENABLE_CTSM}>:WRF_USE_CTSM=$<BOOL:${ENABLE_CTSM}>>
                        $<$<NOT:$<BOOL:${ENABLE_CTSM}>>:WRF_USE_CLM>

                        # If force classic or no nc-4 support enable classic
                        $<$<OR:$<BOOL:${FORCE_NETCDF_CLASSIC}>,$<NOT:$<BOOL:${netCDF_NC4}>>>:NETCDF_classic=1>
                        $<$<OR:$<BOOL:${WRFIO_NCD_NO_LARGE_FILE_SUPPORT}>,$<NOT:$<BOOL:${netCDF_LARGE_FILE_SUPPORT}>>>:WRFIO_NCD_NO_LARGE_FILE_SUPPORT=1>
                        # May need a check for WRFIO_ncdpar_LARGE_FILE_SUPPORT

                        # Now set the opposite in different defines, because why not :)
                        $<$<AND:$<NOT:$<BOOL:${FORCE_NETCDF_CLASSIC}>>,$<BOOL:${netCDF_NC4}>>:USE_NETCDF4_FEATURES=1>
                        $<$<AND:$<NOT:$<BOOL:${WRFIO_NCD_NO_LARGE_FILE_SUPPORT}>>,$<BOOL:${netCDF_LARGE_FILE_SUPPORT}>>:WRFIO_NCD_LARGE_FILE_SUPPORT=1>

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
                        $<$<BOOL:${MOVE_NESTS}>:MOVE_NESTS>
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

                        # I don't believe these are used anymore...
                        # $<$<BOOL:${MPI2_SUPPORT}>:MPI2_SUPPORT=$<BOOL:${MPI2_SUPPORT}>>
                        # $<$<BOOL:${MPI2_THREAD_SUPPORT}>:MPI2_THREAD_SUPPORT=$<BOOL:${MPI2_THREAD_SUPPORT}>>

                        )


# Make core target
add_library(
            ${PROJECT_NAME}_Core
              STATIC
              )

# Supplemental to core, or rather should be, some stuff in external is legitimately part of WRF and others
# are source code from truly external repositories - albeit old versions
add_subdirectory( external )
add_subdirectory( tools    )

# add_dependencies() does not support generator expressions so whatever we can defer to linking please do so
add_dependencies(
                  ${PROJECT_NAME}_Core
                  # So many things depend on this that I'm adding a dep here
                  registry_code
                  )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PUBLIC
                              # List module directories first so the compiler does not get confused
                              # about things "not being compiled yet" - yes, yes it is compiled
                              # These are already set up to be @ install location
                              $<TARGET_PROPERTY:esmf_time_f90,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_grib_share,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_grib1,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:fftpack5,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_netcdf,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:io_int,Fortran_MODULE_DIRECTORY>

                              $<TARGET_PROPERTY:${PROJECT_NAME}_Core,Fortran_MODULE_DIRECTORY>


                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/inc>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/frame>
                              $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/external/ioapi_share>

                              $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/inc>
                              $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/frame>

                              # For install interface includes, i.e. when used by external tools
                              # such as WPS
                              # $<INSTALL_INTERFACE::${PROJECT_SOURCE_DIR}>
                              $<INSTALL_INTERFACE:include/inc>
                              $<INSTALL_INTERFACE:include/frame>
                              $<INSTALL_INTERFACE:include/external/ioapi_share>

                              # May or may not exist
                              $<$<TARGET_EXISTS:atm_ocn>:$<TARGET_PROPERTY:atm_ocn,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_adios2>:$<TARGET_PROPERTY:io_adios2,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_esmf>:$<TARGET_PROPERTY:io_esmf,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_pio>:$<TARGET_PROPERTY:io_pio,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_netcdfpar>:$<TARGET_PROPERTY:io_netcdfpar,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_pnetcdf>:$<TARGET_PROPERTY:io_pnetcdf,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:io_phdf5>:$<TARGET_PROPERTY:io_phdf5,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:g2lib>:$<TARGET_PROPERTY:g2lib,Fortran_MODULE_DIRECTORY>>
                              $<$<TARGET_EXISTS:bacio-1.3>:$<TARGET_PROPERTY:bacio-1.3,Fortran_MODULE_DIRECTORY>>

                            PRIVATE
                              
                              ${PROJECT_SOURCE_DIR}/dyn_em

                              # externals
                              ${PROJECT_SOURCE_DIR}/external/esmf_time_f90
                              ${PROJECT_SOURCE_DIR}/external/io_grib_share
                              ${PROJECT_SOURCE_DIR}/external/io_netcdf
                              ${PROJECT_SOURCE_DIR}/external/io_int

                              # Found Packages not handled through :: imported target
                              ${netCDF_INCLUDE_DIRS}
                              ${netCDF-Fortran_INCLUDE_DIRS}
                              ${pnetCDF_INCLUDE_DIRS}
                            )

# Add directly to core
add_subdirectory( phys     )
add_subdirectory( share    )
add_subdirectory( frame    )
add_subdirectory( inc      )

if ( ${WRF_CHEM} )
  add_subdirectory( chem     )
endif()

if ( ${ENABLE_HYDRO} )
  add_subdirectory( hydro  )
endif()

add_subdirectory( dyn_em   )


add_subdirectory( main     )

################################################################################
# Add subdirectory with case info
################################################################################
if ( ${CURRENT_WRF_CORE_IDX} GREATER_EQUAL ${START_DA_IDX} )
  message( STATUS "DA or PLUS build, WRF_CASE selection ignored" )
else()
  add_subdirectory( test/${WRF_CASE_FOLDER} )
endif()

# Configure core
set_target_properties(
                      ${PROJECT_NAME}_Core
                        PROPERTIES
                          # Just dump everything in here
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/
                          Fortran_FORMAT           FREE
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                      )

# Because of the way netCDF provides its info and the way cmake auto-gens RPATH, we need to help it along
target_link_directories(
                        ${PROJECT_NAME}_Core
                        PUBLIC
                          ${netCDF_LIBRARY_DIR}
                          ${netCDF-Fortran_LIBRARY_DIR}
                        )


target_link_libraries(  ${PROJECT_NAME}_Core
                          PUBLIC
                            ${netCDF_LIBRARIES}
                            ${netCDF-Fortran_LIBRARIES}
                            ${pnetCDF_LIBRARIES}
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                            $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                            # This will add in target dependencies if they exist
                            $<TARGET_NAME_IF_EXISTS:Jasper::Jasper>
                            $<TARGET_NAME_IF_EXISTS:HDF5::HDF5>
                            $<TARGET_NAME_IF_EXISTS:ZLIB::ZLIB>
                            $<TARGET_NAME_IF_EXISTS:CURL::libcurl>
                            $<TARGET_NAME_IF_EXISTS:RPC::RPC>
                          PRIVATE


                            # "External" io libs
                            esmf_time_f90

                            io_grib1
                            grib1_util
                            MEL_grib1
                            WGRIB

                            io_grib_share
                            fftpack5

                            $<TARGET_NAME_IF_EXISTS:wrfhydro>
                            $<TARGET_NAME_IF_EXISTS:atm_ocn>
                            $<TARGET_NAME_IF_EXISTS:io_adios2>
                            $<TARGET_NAME_IF_EXISTS:io_esmf>
                            $<TARGET_NAME_IF_EXISTS:io_pio>
                            io_int
                            io_netcdf
                            $<TARGET_NAME_IF_EXISTS:io_netcdfpar>
                            $<TARGET_NAME_IF_EXISTS:io_pnetcdf>
                            $<TARGET_NAME_IF_EXISTS:io_phdf5>

                            $<TARGET_NAME_IF_EXISTS:g2lib>
                            $<TARGET_NAME_IF_EXISTS:bacio-1.3>

                            $<TARGET_NAME_IF_EXISTS:RSL_LITE>
                      )

################################################################################
##
## Install and export
##
################################################################################
set( CONFIG_INSTALL_DIRECTORY lib/cmake/${PROJECT_NAME} )
install(
        TARGETS ${PROJECT_NAME}_Core
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

# Install to namespace
install(
        EXPORT      ${EXPORT_NAME}Targets
        DESTINATION ${CONFIG_INSTALL_DIRECTORY}
        FILE        ${EXPORT_NAME}Targets.cmake
        NAMESPACE   ${EXPORT_NAME}::
        )

configure_package_config_file(
                              ${PROJECT_SOURCE_DIR}/cmake/template/${EXPORT_NAME}Config.cmake.in
                              ${CMAKE_BINARY_DIR}/${EXPORT_NAME}Config.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DIRECTORY}
                              )

write_basic_package_version_file(
                                  ${CMAKE_BINARY_DIR}/${EXPORT_NAME}ConfigVersion.cmake
                                  VERSION ${PROJECT_VERSION}
                                  #!TODO Check if this is the type of versioning support we want to use
                                  COMPATIBILITY SameMinorVersion
                                  )

install(
        FILES
          ${CMAKE_BINARY_DIR}/${EXPORT_NAME}Config.cmake
          ${CMAKE_BINARY_DIR}/${EXPORT_NAME}ConfigVersion.cmake
        DESTINATION ${CONFIG_INSTALL_DIRECTORY}
        )

# Install some helper files for anyone using this build as part of their code
install(
        DIRECTORY
          # Trailing / is important
          ${PROJECT_SOURCE_DIR}/cmake/modules/
        COMPONENT       helpers
        DESTINATION     share
        FILES_MATCHING
          PATTERN       "*.cmake"
        )
install(
        FILES
          ${PROJECT_SOURCE_DIR}/cmake/confcheck.cmake
          ${PROJECT_SOURCE_DIR}/cmake/gitinfo.cmake
          ${PROJECT_SOURCE_DIR}/cmake/printOption.cmake
          ${PROJECT_SOURCE_DIR}/cmake/wrf_get_version.cmake
        COMPONENT     helpers
        DESTINATION   share
        )

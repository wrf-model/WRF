# Find Jasper
# Eventually replace with Jasper's actual config if using that
# Once found this file will define:
#  Jasper_FOUND - System has Jasper
#  Jasper_INCLUDE_DIRS - The Jasper include directories
#  Jasper_LIBRARIES - The libraries needed to use Jasper

find_package( PkgConfig )
pkg_check_modules( PC_Jasper QUIET Jasper )
# set(CMAKE_FIND_DEBUG_MODE TRUE)
find_path(
          Jasper_INCLUDE_DIR
          NAMES jasper/jasper.h # Make it so we go up one dir
          # Hints before PATHS
          HINTS ${Jasper_ROOT} ${JASPERINC} ${JASPER_PATH} ENV Jasper_ROOT ENV JASPERINC ENV JASPER_PATH
          PATHS ${PC_Jasper_INCLUDE_DIRS}
          PATH_SUFFIXES Jasper jasper include #include/jasper
        )
find_library(
              Jasper_LIBRARY
              NAMES jasper
              # Hints before PATHS
              HINTS ${Jasper_ROOT} ${JASPERLIB} ${JASPER_PATH} ENV Jasper_ROOT ENV JASPERLIB ENV JASPER_PATH
              PATHS ${PC_Jasper_LIBRARY_DIRS}
              PATH_SUFFIXES lib
            )

# Ripped from https://github.com/Kitware/CMake/blob/master/Modules/FindJasper.cmake
if( Jasper_INCLUDE_DIR AND EXISTS "${Jasper_INCLUDE_DIR}/jasper/jas_config.h")
  file(STRINGS "${Jasper_INCLUDE_DIR}/jasper/jas_config.h" jasper_version_str REGEX "^#define[\t ]+JAS_VERSION[\t ]+\".*\".*")
  string(REGEX REPLACE "^#define[\t ]+JAS_VERSION[\t ]+\"([^\"]+)\".*" "\\1" Jasper_VERSION_STRING "${jasper_version_str}")
endif()
# set(CMAKE_FIND_DEBUG_MODE FALSE)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
                                  Jasper
                                  FOUND_VAR Jasper_FOUND
                                  REQUIRED_VARS
                                    Jasper_LIBRARY
                                    Jasper_INCLUDE_DIR
                                  VERSION_VAR Jasper_VERSION_STRING
                                  HANDLE_VERSION_RANGE
                                )

if ( Jasper_FOUND AND NOT TARGET Jasper::Jasper )
  add_library( Jasper::Jasper UNKNOWN IMPORTED )
  set_target_properties(
                        Jasper::Jasper
                        PROPERTIES
                          IMPORTED_LOCATION             "${Jasper_LIBRARY}"
                          INTERFACE_COMPILE_OPTIONS     "${PC_Jasper_CFLAGS_OTHER}"
                          INTERFACE_INCLUDE_DIRECTORIES "${Jasper_INCLUDE_DIR}"
                        )

  # Allow traditional/legacy style usage
  set( Jasper_LIBRARIES    ${Jasper_LIBRARY}         )
  set( Jasper_INCLUDE_DIRS ${Jasper_INCLUDE_DIR}     )
  set( Jasper_DEFINITIONS  ${PC_Jasper_CFLAGS_OTHER} )

  mark_as_advanced(
                    Jasper_INCLUDE_DIR
                    Jasper_LIBRARY
                  )
endif()
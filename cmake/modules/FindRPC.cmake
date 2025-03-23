# Find RPC
# Eventually replace with RPC's actual config if using that
# Once found this file will define:
#  RPC_FOUND - System has RPC
#  RPC_INCLUDE_DIRS - The RPC include directories
#  RPC_LIBRARIES - The libraries needed to use RPC

find_package( PkgConfig )
pkg_check_modules( PC_RPC QUIET RPC )
# set(CMAKE_FIND_DEBUG_MODE TRUE)
find_path(
          RPC_INCLUDE_DIR
          NAMES rpc/types.h # Make it so we go up one dir
          # Hints before PATHS
          HINTS ENV RPC_ROOT ENV RPCINC ENV RPC_PATH ${RPC_ROOT} ${RPCINC} ${RPC_PATH}
          PATHS ${PC_RPC_INCLUDE_DIRS}
          PATH_SUFFIXES tirpc
        )
find_library(
              RPC_LIBRARY
              NAMES rpc rpcsvc
              # Hints before PATHS
              HINTS ENV RPC_ROOT ENV RPCLIB ENV RPC_PATH ${RPC_ROOT} ${RPCLIB} ${RPC_PATH}
              PATHS ${PC_RPC_LIBRARY_DIRS}
              PATH_SUFFIXES lib
            )

# set(CMAKE_FIND_DEBUG_MODE FALSE)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
                                  RPC
                                  FOUND_VAR RPC_FOUND
                                  REQUIRED_VARS
                                    RPC_LIBRARY
                                    RPC_INCLUDE_DIR
                                  # VERSION_VAR RPC_VERSION
                                )

if ( RPC_FOUND AND NOT TARGET RPC::RPC )
  add_library( RPC::RPC UNKNOWN IMPORTED )
  set_target_properties(
                        RPC::RPC
                        PROPERTIES
                          IMPORTED_LOCATION             "${RPC_LIBRARY}"
                          INTERFACE_COMPILE_OPTIONS     "${PC_RPC_CFLAGS_OTHER}"
                          INTERFACE_INCLUDE_DIRECTORIES "${RPC_INCLUDE_DIR}"
                        )

  # Allow traditional/legacy style usage
  set( RPC_LIBRARIES    ${RPC_LIBRARY}         )
  set( RPC_INCLUDE_DIRS ${RPC_INCLUDE_DIR}     )
  set( RPC_DEFINITIONS  ${PC_RPC_CFLAGS_OTHER} )

  mark_as_advanced(
                    RPC_INCLUDE_DIR
                    RPC_LIBRARY
                  )
endif()
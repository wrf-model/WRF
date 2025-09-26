# - Try to find Glibmm-2.4
# Once done, this will define
#
#  Glibmm_FOUND - system has Glibmm
#  Glibmm_INCLUDE_DIRS - the Glibmm include directories
#  Glibmm_LIBRARIES - link these to use Glibmm

include(LibFindMacros)

# Dependencies
libfind_package(Glibmm Glib)
libfind_package(Glibmm SigC++)

# Use pkg-config to get hints about paths
libfind_pkg_check_modules(Glibmm_PKGCONF glibmm-2.4)

# Main include dir
find_path(Glibmm_INCLUDE_DIR
  NAMES glibmm/main.h
  PATHS ${Glibmm_PKGCONF_INCLUDE_DIRS}
  PATH_SUFFIXES glibmm-2.4
)

# Glib-related libraries also use a separate config header, which is in lib dir
find_path(GlibmmConfig_INCLUDE_DIR
  NAMES glibmmconfig.h
  PATHS ${Glibmm_PKGCONF_INCLUDE_DIRS} /usr
  PATH_SUFFIXES lib/glibmm-2.4/include
)

libfind_library(Glibmm glibmm 2.4)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
set(Glibmm_PROCESS_INCLUDES Glibmm_INCLUDE_DIR GlibmmConfig_INCLUDE_DIR Glib_INCLUDE_DIRS SigC++_INCLUDE_DIRS)
set(Glibmm_PROCESS_LIBS Glibmm_LIBRARY Glib_LIBRARIES SigC++_LIBRARIES)
libfind_process(Glibmm)


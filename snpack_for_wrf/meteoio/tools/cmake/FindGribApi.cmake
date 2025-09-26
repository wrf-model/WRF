INCLUDE(LibFindMacros)

# Finally the library itself
FIND_LIBRARY(GribApi_LIBRARY
NAMES grib_api
PATHS
	ENV LD_LIBRARY_PATH
	"~/usr/lib"
	"/usr/local/lib"
	"/usr/lib"
	"/opt/lib"
DOC "Location of the grib-api library, like /usr/lib/libgrib_api.so"
)

#build GRIBAPI_ROOT so we can provide a hint for searching for the header file
GET_FILENAME_COMPONENT(grib_api_libs_root ${GribApi_LIBRARY} PATH)
SET(GRIBAPI_ROOT "${meteoio_libs_root}/../")
STRING(REPLACE  " " "\\ " GRIBAPI_ROOT ${GRIBAPI_ROOT})

# locate main header file
FIND_PATH(GribApi_INCLUDE_DIR
  NAMES grib_api.h
  #HINTS ${GRIBAPI_ROOT}/include
  PATHS
	"${GRIBAPI_ROOT}/include"
	"~/usr/include"
	"/usr/local/include"
	"/usr/include"
	"/opt/include"
  DOC "Location of the grib-api headers, like /usr/include"
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
SET(GribApi_PROCESS_INCLUDES GribApi_INCLUDE_DIR)
SET(GribApi_PROCESS_LIBS GribApi_LIBRARY)
libfind_process(GribApi)

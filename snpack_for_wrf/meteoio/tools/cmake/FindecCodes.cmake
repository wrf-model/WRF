INCLUDE(LibFindMacros)

# Finally the library itself
FIND_LIBRARY(ecCodes_LIBRARY
NAMES eccodes
PATHS
	ENV LD_LIBRARY_PATH
	"~/usr/lib"
	"/usr/local/lib"
	"/usr/lib"
	"/opt/lib"
DOC "Location of the ecCodes library, like /usr/lib/eccodes.so"
)

#build ECCODES_ROOT so we can provide a hint for searching for the header file
GET_FILENAME_COMPONENT(eccodes_libs_root ${ecCodes_LIBRARY} PATH)
SET(ECCODES_ROOT "${meteoio_libs_root}/../")
STRING(REPLACE  " " "\\ " ECCODES_ROOT ${ECCODES_ROOT})

# locate main header file
FIND_PATH(ecCodes_INCLUDE_DIR
  NAMES eccodes.h
  #HINTS ${ECCODES_ROOT}/include
  PATHS
	"${ECCODES_ROOT}/include"
	"~/usr/include"
	"/usr/local/include"
	"/usr/include"
	"/opt/include"
  DOC "Location of the ecCodes headers, like /usr/include"
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
SET(ecCodes_PROCESS_INCLUDES ecCodes_INCLUDE_DIR)
SET(ecCodes_PROCESS_LIBS ecCodes_LIBRARY)
libfind_process(ecCodes)

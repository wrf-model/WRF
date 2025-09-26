INCLUDE(LibFindMacros)

FIND_LIBRARY(Matio_LIBRARY
NAMES libmatio.so
PATHS
	ENV LD_LIBRARY_PATH
	"~/usr/lib"
	"/usr/local/lib"
	"/usr/lib"
	"/opt/lib"
DOC "Location of the libmatio, like /usr/lib/libmatio.so"
)

#build POPC_ROOT so we can provide a hint for searching for the header file
IF("${Matio_LIBRARY}" MATCHES "^(.+)lib[\\/]libmatio\\.(.+)$")
   SET(MATIO_ROOT "${CMAKE_MATCH_1}")
ENDIF("${Matio_LIBRARY}" MATCHES "^(.+)lib[\\/]libmatio\\.(.+)$")

# locate main header file
FIND_PATH(Matio_INCLUDE_DIR
  NAMES matio.h
  PATHS
	"${MATIO_ROOT}/include"
	"~/usr/include"
	"/usr/local/include"
	"/usr/include"
	"/opt/include"
  DOC "Location of the matio headers, like /usr/include"
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
SET(Matio_PROCESS_INCLUDES Matio_INCLUDE_DIR)
SET(Matio_PROCESS_LIBS Matio_LIBRARY)
libfind_process(Matio)

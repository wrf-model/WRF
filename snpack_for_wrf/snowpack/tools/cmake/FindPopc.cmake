INCLUDE(LibFindMacros)

FIND_PROGRAM(POPCC_EXECUTABLE popcc)
MARK_AS_ADVANCED(FORCE POPCC_EXECUTABLE)
#build POPC_ROOT so we can provide a hint for searching for the header file
GET_FILENAME_COMPONENT(popc_exe_root ${POPCC_EXECUTABLE} PATH)
SET(POPC_ROOT "${popc_exe_root}/../")

#get POPC_LOCATION env. variable to provide as a hint
SET(POPC_LOCATION $ENV{POPC_LOCATION})

# Finally the library itself
FIND_LIBRARY(Popc_LIBRARY
NAMES libparoc_common.a
PATHS
	"${POPC_LOCATION}/lib"
	"${POPC_ROOT}/lib"
	"${POPC_ROOT}/popc/lib"
	ENV LD_LIBRARY_PATH
	"~/usr/lib"
	"/usr/local/lib"
	"/usr/lib"
	"/opt/lib"
DOC "Location of the libparoc_common, like /usr/lib/libparoc_common.a"
)

#build POPC_ROOT so we can provide a hint for searching for the header file
IF("${Popc_LIBRARY}" MATCHES "^(.+)lib[\\/]libparoc_common\\.(.+)$")
   SET(POPC_ROOT "${CMAKE_MATCH_1}")
ENDIF("${Popc_LIBRARY}" MATCHES "^(.+)lib[\\/]libparoc_common\\.(.+)$")

# locate main header file
FIND_PATH(Popc_INCLUDE_DIR
  NAMES paroc_base.h
  #HINTS ${POPC_ROOT}/include
  PATHS
	"${POPC_LOCATION}/lib"
	"${POPC_ROOT}/include"
	"${POPC_ROOT}/popc/include"
	"~/usr/include"
	"/usr/local/include"
	"/usr/include"
	"/opt/include"
  DOC "Location of the popc headers, like /usr/include"
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
SET(Popc_PROCESS_INCLUDES Popc_INCLUDE_DIR)
SET(Popc_PROCESS_LIBS Popc_LIBRARY)
libfind_process(Popc)

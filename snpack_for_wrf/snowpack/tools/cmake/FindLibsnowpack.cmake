INCLUDE(LibFindMacros)

# Where can we find something that looks like a Snowpack source tree?
FILE(GLOB sn_local_src LIST_DIRECTORIES TRUE  ../../../[sS]nowpack ../[sS]nowpack ../../../[sS]nowpack-[0-9]* ../[sS]nowpack-[0-9]*)
LIST(LENGTH sn_local_src n)
IF("${n}" EQUAL "0")
	SET(SRC_DIR ".")
ELSE("${n}" EQUAL "0")
	LIST(GET sn_local_src 0 SRC_DIR) #only keep the first match
ENDIF("${n}" EQUAL "0")

IF(WIN32)
	GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT1 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Snowpack;UninstallString]" PATH CACHE INTERNAL)
	GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT2 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Snowpack;UninstallString]" PATH CACHE INTERNAL)
	GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT3 "[HKEY_LOCAL_MACHINE\\SOFTWARE\\WSL Institute for Snow and Avalanche Research\\Snowpack]" ABSOLUTE CACHE INTERNAL)
	GET_FILENAME_COMPONENT(METEOIO_ROOT4 "C:/Progra~1/Snowpack*" ABSOLUTE CACHE INTERNAL)
	SET(SEARCH_PATH
		ENV LIB
		./lib ./bin
		./lib/Debug ./bin/Debug
		./lib/Release ./bin/Release
		../../lib ../../bin
		../../lib/Debug ../../bin/Debug
		../../lib/Release ../../bin/Release
		${SRC_DIR}/lib ${SRC_DIR}/bin
		${SRC_DIR}/lib/Debug ${SRC_DIR}/bin/Debug
		${SRC_DIR}/lib/Release ${SRC_DIR}/bin/Release
		${SRC_DIR}/../../lib ${SRC_DIR}/../../bin
		${SRC_DIR}/../../lib/Debug ${SRC_DIR}/../../bin/Debug
		${SRC_DIR}/../../lib/Release ${SRC_DIR}/../../bin/Release
		${SRC_DIR}/bin ${SRC_DIR}/lib
		${LIBSNOWPACK_ROOT1}/bin ${LIBSNOWPACK_ROOT1}/lib
		${LIBSNOWPACK_ROOT2}/bin ${LIBSNOWPACK_ROOT2}/lib
		${LIBSNOWPACK_ROOT3}/bin ${LIBSNOWPACK_ROOT3}/lib
		${LIBSNOWPACK_ROOT4}/bin ${LIBSNOWPACK_ROOT4}/lib )

	IF(MSVC)
		FIND_LIBRARY(LIBSNOWPACK_LIBRARY
			NAMES libsnowpack.lib
			HINTS ${SEARCH_PATH}
			DOC "Location of the libsnowpack, like c:/Program Files/Snowpack-2.0.0/lib/libsnowpack.lib"
			)
	ELSE(MSVC)
		FIND_LIBRARY(LIBSNOWPACK_LIBRARY
			NAMES libsnowpack.dll.a libsnowpack.a
			HINTS ${SEARCH_PATH}
			DOC "Location of the libsnowpack, like c:/Program Files/Snowpack-2.0.0/lib/libsnowpack.dll.a"
			)
	ENDIF(MSVC)
ELSE(WIN32)
	IF(APPLE)
		FIND_LIBRARY(LIBSNOWPACK_LIBRARY
		NAMES snowpack
		HINTS
			ENV LD_LIBRARY_PATH
			ENV DYLD_FALLBACK_LIBRARY_PATH
			./lib
			../../lib
			../../../lib
			${SRC_DIR}/lib
			"~/usr/lib"
			"/Applications/Snowpack/lib"
			"/usr/local/lib"
			"/usr/lib"
			"/opt/lib"
		DOC "Location of the libsnowpack, like /usr/lib/libsnowpack.dylib"
		)
	ELSE(APPLE)
		FIND_LIBRARY(LIBSNOWPACK_LIBRARY
		NAMES snowpack
		HINTS
			ENV LD_LIBRARY_PATH
			./lib
			../../lib
			../../../lib
			${SRC_DIR}/lib
			"~/usr/lib"
			"/usr/local/lib"
			"/usr/lib"
			"/opt/lib"
		DOC "Location of the libsnowpack, like /usr/lib/libsnowpack.so"
		)
	ENDIF(APPLE)
ENDIF(WIN32)

#build LIBSNOWPACK_ROOT so we can provide a hint for searching for the header file
IF(${CMAKE_VERSION} VERSION_GREATER "2.8.11")
	GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT ${LIBSNOWPACK_LIBRARY} DIRECTORY) #get PATH
	GET_FILENAME_COMPONENT(MSVC_TARGET ${LIBSNOWPACK_ROOT} NAME) #special directory name for some MSVC
	IF(("${MSVC_TARGET}" STREQUAL "Debug") OR ("${MSVC_TARGET}" STREQUAL "Release"))
		GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT ${LIBSNOWPACK_ROOT} DIRECTORY) #go up one level
	ENDIF(("${MSVC_TARGET}" STREQUAL "Debug") OR ("${MSVC_TARGET}" STREQUAL "Release"))
	GET_FILENAME_COMPONENT(LIBSNOWPACK_ROOT ${LIBSNOWPACK_ROOT} DIRECTORY) #go up one level
ELSE(${CMAKE_VERSION} VERSION_GREATER "2.8.11")
    GET_FILENAME_COMPONENT(snowpack_libs_root ${LIBSNOWPACK_LIBRARY} PATH)
	SET(LIBSNOWPACK_ROOT "${snowpack_libs_root}/../")
	STRING(REPLACE  " " "\\ " LIBSNOWPACK_ROOT ${LIBSNOWPACK_ROOT})
ENDIF(${CMAKE_VERSION} VERSION_GREATER "2.8.11")


# locate main header file
FIND_PATH(LIBSNOWPACK_INCLUDE_DIR
  NAMES snowpack/libsnowpack.h
  HINTS
	"${LIBSNOWPACK_ROOT}/include"
	"${LIBSNOWPACK_ROOT}"
	"~/usr/include"
	"/usr/local/include"
	"/usr/include"
	"/opt/include"
  DOC "Location of the libsnowpack headers, like /usr/include"
)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
SET(LIBSNOWPACK_PROCESS_INCLUDES LIBSNOWPACK_INCLUDE_DIR)
SET(LIBSNOWPACK_PROCESS_LIBS LIBSNOWPACK_LIBRARY)
libfind_process(LIBSNOWPACK)

INCLUDE(LibFindMacros)

# Finally the library itself
GET_FILENAME_COMPONENT(SRC_DIR ${CMAKE_SOURCE_DIR} PATH) #ie goes up one level
STRING(REPLACE " " "\\ " SRC_DIR ${SRC_DIR})

IF(WIN32)
	SET(SEARCH_PATH
		ENV LIB
		${SRC_DIR}/lib)
	IF(MSVC)
		FIND_LIBRARY(Lapack_LIBRARY
			NAMES liblapack.lib
			PATHS ${SEARCH_PATH}
			DOC "Location of the liblapack, like c:/Program Files/LAPACK-3.0.0/lib/liblapack.lib"
			)
		FIND_LIBRARY(Blas_LIBRARY
			NAMES libblas.lib
			PATHS ${SEARCH_PATH}
			DOC "Location of the liblapack, like c:/Program Files/LAPACK-3.0.0/lib/libblas.lib"
			)
	ELSE(MSVC)
		FIND_LIBRARY(Lapack_LIBRARY
			NAMES liblapack.dll
			PATHS ${SEARCH_PATH}
			DOC "Location of the libblas, like c:/Program Files/LAPACK-3.0.0/lib/libblas.dll"
			)
		FIND_LIBRARY(Blas_LIBRARY
			NAMES libblas.dll
			PATHS ${SEARCH_PATH}
			DOC "Location of the libblas, like c:/Program Files/LAPACK-3.0.0/lib/libblas.dll"
			)
	ENDIF(MSVC)
ELSE(WIN32)
	IF(APPLE)
		FIND_LIBRARY(Lapack_LIBRARY
		NAMES lapack
		PATHS
			"/Applications/LAPACK/lib"
			ENV LD_LIBRARY_PATH
			ENV DYLD_FALLBACK_LIBRARY_PATH
			"~/usr/lib"
			"/usr/local/lib"
			"/usr/lib"
			"/opt/lib"
			${SRC_DIR}/lib
		DOC "Location of the liblapack, like /usr/lib/liblapack.dylib"
		)
		FIND_LIBRARY(Blas_LIBRARY
		NAMES blas
		PATHS
			"/Applications/BLAS/lib"
			ENV LD_LIBRARY_PATH
			ENV DYLD_FALLBACK_LIBRARY_PATH
			"~/usr/lib"
			"/usr/local/lib"
			"/usr/lib"
			"/opt/lib"
			${SRC_DIR}/lib
		DOC "Location of the libblas, like /usr/lib/libblas.dylib"
		)
	ELSE(APPLE)
		FIND_LIBRARY(Lapack_LIBRARY
		NAMES lapack
		PATHS
			ENV LD_LIBRARY_PATH
			"~/usr/lib"
			"/usr/local/lib"
			"/usr/lib64"
			"/usr/lib64/atlas"
			"/usr/lib"
			"/usr/lib/atlas"
			"/opt/lib"
			${SRC_DIR}/lib
		DOC "Location of the liblapack, like /usr/lib/liblapack.so"
		)
		FIND_LIBRARY(Blas_LIBRARY
		NAMES blas
		PATHS
			ENV LD_LIBRARY_PATH
			"~/usr/lib"
			"/usr/local/lib"
			"/usr/lib64"
			"/usr/lib64/atlas"
			"/usr/lib"
			"/usr/lib/atlas"
			"/opt/lib"
			${SRC_DIR}/lib
		DOC "Location of the libblas, like /usr/lib/libblas.so"
		)
	ENDIF(APPLE)
ENDIF(WIN32)

SET(Lapack_all "${Lapack_LIBRARY};${Blas_LIBRARY}")

SET(Lapack_PROCESS_LIBS Lapack_all)
libfind_process(Lapack)

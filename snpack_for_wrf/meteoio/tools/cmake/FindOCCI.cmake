# - Find Oracle OCCI library
# Find the Oracle OCCI library and includes
#
# It defines the following variables:
# OCCI_LIBRARY - the library
# OCCI_INCLUDE_DIR - the header
# OCCI_FOUND - Set to false, or undefined, if we haven't found

include(LibFindMacros)

IF(WIN32)
	STRING(REGEX MATCHALL "([^;]+;|[^;]+$)" ENV_PATH "$ENV{PATH}") 
	FOREACH(dir ${ENV_PATH})
		IF (("${dir}" MATCHES "(O|o)racle") AND (NOT "${dir}" MATCHES "(J|j)ava"))
			FILE(TO_CMAKE_PATH ${dir} TMP)
			LIST(APPEND OCCI_HINT ${TMP})
		ENDIF()
	ENDFOREACH()
	
	FIND_LIBRARY(OCCI_LIBRARY
	  NAMES oraocci10.lib oraocci11.lib oraocci12.lib
	  PATHS
		"./"
		"lib"
	  PATH_SUFFIXES
		"${OCCI_HINT}"
	)
	
	#build ORACLE_ROOT so we can provide a hint for searching for the header file
	IF ("${OCCI_LIBRARY}" MATCHES "^(.+)oraocci[0-9]+\\.dll$")
	   SET(ORACLE_ROOT "${CMAKE_MATCH_1}")
	ENDIF ("${OCCI_LIBRARY}" MATCHES "^(.+)oraocci[0-9]+\\.dll$")
	
	SET(CLNTSH_LIBRARY ${OCCI_LIBRARY})
	
	# locate header files
	FIND_PATH(OCCI_INCLUDE_DIR
	  NAMES occi.h
	  PATHS
		"${ORACLE_ROOT}/rdbms/public"
	)
	
ELSE(WIN32)
	find_library(OCCI_LIBRARY
	  NAMES occi
	  #HINTS $ENV{ORACLE_HOME}/client_1/lib $ENV{ORACLE_BASE}/client_1/lib
	  PATHS
		"$ENV{ORACLE_HOME}/client_1/lib"
		"$ENV{ORACLE_BASE}/client_1/lib"
		ENV LD_LIBRARY_PATH
		"~/usr/lib"
		"/usr/local/lib"
		"/usr/lib"
		"/opt/lib"
	  PATH_SUFFIXES "oracle/*/client_1/lib"
	)

	#build ORACLE_ROOT so we can provide a hint for searching for the header file
	if ("${OCCI_LIBRARY}" MATCHES "^(.+)lib[\\/]libocci\\.(.+)$")
	   set(ORACLE_ROOT "${CMAKE_MATCH_1}")
	endif ("${OCCI_LIBRARY}" MATCHES "^(.+)lib[\\/]libocci\\.(.+)$")

	#libclntsh comes with libocci, in the same directory, but we need to explicitly reference it
	find_library(CLNTSH_LIBRARY  NAMES clntsh PATHS "${ORACLE_ROOT}/lib")

	# locate header files
	find_path(OCCI_INCLUDE_DIR
	  NAMES occi.h
	  #HINTS ${ORACLE_ROOT}/include
	  PATHS
		"${ORACLE_ROOT}/include"
		"~/usr/include"
		"/usr/local/include"
		"/usr/local"
		"/usr/include"
		"/opt/include"
	  PATH_SUFFIXES "oracle/*/client_1/include"
	)
ENDIF(WIN32)

# Set the include dir variables and the libraries and let libfind_process do the rest.
# NOTE: Singular variables for this library, plural for libraries this this lib depends on.
set(OCCI_PROCESS_INCLUDES OCCI_INCLUDE_DIR)
set(OCCI_PROCESS_LIBS OCCI_LIBRARY CLNTSH_LIBRARY)
libfind_process(OCCI)
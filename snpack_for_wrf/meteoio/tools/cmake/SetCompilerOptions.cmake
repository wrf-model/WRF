#Set different variables according to the detected compiler and processor
#based on $CMAKE_CXX_COMPILER_ID it sets the following variables:
# WARNINGS, EXTRA_WARNINGS, EXTRA, OPTIM, ARCH, DEBUG, _VERSION, PROFILING
# It can also edit CMAKE_SHARED_LINKER_FLAGS and CMAKE_EXE_LINKER_FLAGS

INCLUDE("${CMAKE_SOURCE_DIR}/tools/cmake/BuildVersion.cmake")
BuildVersion()

MACRO (SET_COMPILER_OPTIONS)
	SET(USER_COMPILER_OPTIONS "" CACHE STRING "Provide some extra compiler options")
	MARK_AS_ADVANCED(FORCE USER_COMPILER_OPTIONS)
	SET(EXTRA "${EXTRA} ${USER_COMPILER_OPTIONS}")

	###########################################################
	IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
		SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)	#this is required for building libraries
		SET(EXTRA "${EXTRA} /D_USE_MATH_DEFINES")   #USE_MATH_DEFINES needed for VC++
		IF(DEBUG_ARITHM)
			SET(EXTRA "${EXTRA} /EHa")
		ENDIF(DEBUG_ARITHM)
		
		#SET(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "limited configs"  FORCE)
		SET(WARNINGS "/W4 /D_CRT_SECURE_NO_WARNINGS /EHsc") #Za: strict ansi EHsc: handle c++ exceptions /w35045: inform about Spectre mitigation
		#SET(EXTRA_WARNINGS "/Wp64") #/Wall
		SET(OPTIM "/O2 /DNDEBUG /DEBUG:FASTLINK /MD /DNOSAFECHECKS")
		SET(ARCH_OPTIM "/arch:AVX2")
		SET(ARCH_SAFE "")
		IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64")
			SET(ARCH_SAFE  "/arch:SSE2")
		ENDIF()
		SET(DEBUG "/Z7 /Od /D__DEBUG /MDd")
		SET(_VERSION "/D_VERSION=${_versionString}")
		
	###########################################################
	ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL Intel)
		IF(ENABLE_LAPACK)
			SET(EXTRA "${EXTRA} -DCLAPACK")
		ENDIF(ENABLE_LAPACK)
		IF(DEBUG_ARITHM)
			SET(EXTRA "${EXTRA} -DDEBUG_ARITHM")
		ENDIF(DEBUG_ARITHM)
		
		SET(WARNINGS_OFF "-Wno-unknown-pragmas")
		SET(WARNINGS "-Wall -Wno-long-long  -Wswitch ${WARNINGS_OFF} -wd2015,11071")
		SET(DEEP_WARNINGS "-Wshadow -Wpointer-arith -Wconversion -Winline -Wdisabled-optimization") #-Wfloat-equal -Wpadded
		SET(EXTRA_WARNINGS "-Wextra -pedantic ${DEEP_WARNINGS}")
		SET(OPTIM "-g -O3 -DNDEBUG -DNOSAFECHECKS")
		IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64")
			SET(ARCH_SAFE  "-march=nocona -mtune=nocona")
			SET(ARCH_OPTIM  "-march=native -mtune=native")
		ENDIF()
		SET(DEBUG "-g3 -O0 -D__DEBUG")
		SET(_VERSION "-D_VERSION=${_versionString}")
		
	###########################################################
	ELSEIF(CMAKE_CXX_COMPILER_ID STREQUAL Cray)
		IF(ENABLE_LAPACK)
			SET(EXTRA "${EXTRA} -DCLAPACK")
		ENDIF(ENABLE_LAPACK)
		IF(DEBUG_ARITHM)
			SET(EXTRA "${EXTRA} -DDEBUG_ARITHM")
		ENDIF(DEBUG_ARITHM)
		
		SET(WARNINGS "-hlist=m -h negmsgs -h msglevel_3 -h nomessage=870") #870: accept multibyte chars
		#SET(EXTRA_WARNINGS "-h msglevel_2")
		SET(OPTIM "-O3 -hfp3 -h msglevel_4 -DNDEBUG -DNOSAFECHECKS")
		IF($ENV{CRAY_CPU_TARGET} MATCHES "^$")
			IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64")
				SET(ARCH_SAFE  "-h cpu=x86-64")
				MESSAGE("No CRAY_CPU_TARGET set, setting it to x86-64; please consider loading the proper target module.")
			ELSE()
				MESSAGE("No CRAY_CPU_TARGET set; please consider loading the proper target module.")
			ENDIF()
		ENDIF()
		SET(DEBUG "-g -D__DEBUG")
		SET(_VERSION "-D_VERSION=${_versionString}")
	
	###########################################################
	ELSEIF(CMAKE_CXX_COMPILER_ID MATCHES "^GNU$")
		#we consider that all other compilers support "-" options and silently ignore what they don't know
		IF(ENABLE_LAPACK)
			SET(EXTRA "${EXTRA} -DCLAPACK")
		ENDIF(ENABLE_LAPACK)
		IF(WIN32)
			LIST(APPEND CFLAGS " -D_USE_MATH_DEFINES") #USE_MATH_DEFINES needed for Win32
		ENDIF(WIN32)
		IF(DEBUG_ARITHM)
			SET(EXTRA "${EXTRA} -DDEBUG_ARITHM")
		ENDIF(DEBUG_ARITHM)
		
		SET(WARNINGS "-Wall -Wno-long-long  -Wswitch")
		SET(DEEP_WARNINGS "-Wunused-value -Wshadow -Wpointer-arith -Wconversion -Winline -Wdisabled-optimization -Wctor-dtor-privacy") #-Wfloat-equal -Wpadded
		SET(EXTRA_WARNINGS "-Wextra -pedantic -Weffc++ ${DEEP_WARNINGS}")
		SET(OPTIM "-g -O3 -DNDEBUG -DNOSAFECHECKS")
		IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64")
			SET(ARCH_SAFE  "-march=nocona -mtune=nocona")
		ENDIF()
		SET(DEBUG "-g3 -O0 -D__DEBUG")
		SET(_VERSION "-D_VERSION=${_versionString}")
		
		SET(PROFILING "-pg -fprofile-arcs") #add ${PROFILING} to the CFLAGS when necessary
		SET(EXTRA_WARNINGS "${EXTRA_WARNINGS} -Wunsafe-loop-optimizations -Wwrite-strings")
		IF(NOT ANDROID)
			SET(EXTRA_WARNINGS "${EXTRA_WARNINGS} -ansi")
			IF(WIN32) #for gcc on windows
				SET(CMAKE_SHARED_LINKER_FLAGS "--enable-auto-import")
				SET(CMAKE_EXE_LINKER_FLAGS "--enable-auto-import")
			ENDIF(WIN32)
		ENDIF(NOT ANDROID)
		EXECUTE_PROCESS(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
		IF(GCC_VERSION VERSION_GREATER 4.2 OR GCC_VERSION VERSION_EQUAL 4.2)
			SET(ARCH_OPTIM  "-march=native -mtune=native")
		ENDIF()
		IF(GCC_VERSION VERSION_GREATER 4.8 OR GCC_VERSION VERSION_EQUAL 4.8)
			SET(EXTRA_WARNINGS "${EXTRA_WARNINGS} -Wvector-operation-performance") #for gcc>=4.7.0
			IF(NOT WIN32)
				#for gcc>4.5, but first implementations were slow, so it is safe to enforce 4.8
				FIND_PROGRAM(CMAKE_GCC_AR NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-ar${_CMAKE_TOOLCHAIN_SUFFIX} HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
				FIND_PROGRAM(CMAKE_GCC_NM NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-nm HINTS ${_CMAKE_TOOLCHAIN_LOCATION})
				FIND_PROGRAM(CMAKE_GCC_RANLIB NAMES ${_CMAKE_TOOLCHAIN_PREFIX}gcc-ranlib HINTS ${_CMAKE_TOOLCHAIN_LOCATION})

				IF(CMAKE_GCC_AR AND CMAKE_GCC_NM AND CMAKE_GCC_RANLIB)
					SET(USE_LTO_OPTIMIZATIONS ON CACHE BOOL "Use Link Time Optmizations when compiling (memory heavy while compiling)")
					MARK_AS_ADVANCED(FORCE USE_LTO_OPTIMIZATIONS)
					IF(USE_LTO_OPTIMIZATIONS)
						SET(OPTIM "${OPTIM} -flto") 
					ENDIF()
					SET( CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -flto -fno-fat-lto-objects" )
					SET( CMAKE_AR "${CMAKE_GCC_AR}" )
					SET( CMAKE_NM "${CMAKE_GCC_NM}" )
					SET( CMAKE_RANLIB "${CMAKE_GCC_RANLIB}" )
				ELSE()
					MESSAGE( WARNING "GCC indicates LTO support, but binutils wrappers could not be found. Disabling LTO." )
				ENDIF()
			ENDIF(NOT WIN32)
			#if set to ON, all binaries depending on the library have to be compiled the same way.
			#Then, do an "export ASAN_SYMBOLIZER_PATH=/usr/bin/llvm-symbolizer-3.4" and run with "ASAN_OPTIONS=symbolize=1"
			SET(LEAKS_CHECK OFF CACHE BOOL "Set to ON to dynamically check for memory corruption (and do the same for applications linked with MeteoIO)")
			IF (LEAKS_CHECK)
				SET(EXTRA "${EXTRA} -fsanitize=address -fno-omit-frame-pointer")
			ENDIF(LEAKS_CHECK)
		ENDIF()

		IF(GCC_VERSION VERSION_GREATER 5.0 OR GCC_VERSION VERSION_EQUAL 5.0)
			IF(PLUGIN_IMISIO) #HACK: current OCCI does not support the short strings optimizations of gcc>=5
				SET(EXTRA "-D_GLIBCXX_USE_CXX11_ABI=0 ${EXTRA}")
			ENDIF(PLUGIN_IMISIO)
		ENDIF()

	###########################################################
	ELSEIF(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
		IF(ENABLE_LAPACK)
			SET(EXTRA "${EXTRA} -DCLAPACK")
		ENDIF(ENABLE_LAPACK)
		IF(WIN32)
			LIST(APPEND CFLAGS " -D_USE_MATH_DEFINES") #USE_MATH_DEFINES needed for Win32
		ENDIF(WIN32)
		IF(DEBUG_ARITHM)
			SET(EXTRA "${EXTRA} -DDEBUG_ARITHM")
		ENDIF(DEBUG_ARITHM)
		
		SET(WARNINGS_OFF "-Wno-long-long -Wno-float-equal -Wno-documentation -Wno-documentation-unknown-command -Wno-old-style-cast -Wno-padded -Wno-missing-noreturn -Wno-weak-vtables -Wno-switch-enum -Wno-covered-switch-default -Wno-global-constructors -Wno-exit-time-destructors -Wno-unknown-pragmas -Wno-format-nonliteral -Wno-date-time -Wno-unused-template -Wno-disabled-macro-expansion")
		SET(WARNINGS "-Wall -Wswitch -Weverything ${WARNINGS_OFF}") #obviously, we should try to fix the warnings! Keeping in mind that some of these W are half buggy...
		SET(DEEP_WARNINGS "-Wunused-value -Wshadow -Wpointer-arith -Wconversion -Winline -Wdisabled-optimization -Wctor-dtor-privacy") #-Rpass=.* for static analysis
		SET(EXTRA_WARNINGS "-Wextra -pedantic -Weffc++ ${DEEP_WARNINGS}")
		SET(OPTIM "-g -O3 -DNDEBUG -DNOSAFECHECKS -flto")
		IF(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" OR CMAKE_SYSTEM_PROCESSOR MATCHES "AMD64")
			SET(ARCH_SAFE  "-march=nocona -mtune=nocona")
		ENDIF()
		SET(DEBUG "-g3 -O0 -D__DEBUG")
		SET(_VERSION "-D_VERSION=${_versionString}")
		
		SET(PROFILING "-pg") #add ${PROFILING} to the CFLAGS when necessary
		SET(EXTRA "${EXTRA} -fcolor-diagnostics") #-fapple-pragma-pack does not seems necessary; -ftrapv should be replaced by sanitize=integer
		SET(LEAKS_CHECK OFF CACHE BOOL "Set to ON to dynamically check for memory corruption (and do the same for applications linked with MeteoIO)")
			IF (LEAKS_CHECK)
				SET(EXTRA "${EXTRA} -ftrapv -fno-omit-frame-pointer") #-fsanitize=address,undefined,integer,undefined-trap but this is currently not supported by Apple
			ENDIF(LEAKS_CHECK)
		SET(ARCH_OPTIM  "-march=native")
	ENDIF()
	
	###########################################################
	#targets providing SETs of compiler options
	IF(NOT DEST)
		SET(DEST "safe" CACHE STRING "Choose safe or optimized" FORCE)
	ENDIF(NOT DEST)

	IF (DEST STREQUAL "safe")
		SET(ARCH  "${ARCH_SAFE}")
	ENDIF(DEST STREQUAL "safe")

	IF(DEST STREQUAL "optimized")
		SET(ARCH  "${ARCH_OPTIM}")
	ENDIF(DEST STREQUAL "optimized")

	#show exception messages in a graphical message box
	SET(GUI_EXCEPTIONS OFF CACHE BOOL "Show a message box with exceptions texts ON or OFF")
	
ENDMACRO (SET_COMPILER_OPTIONS)

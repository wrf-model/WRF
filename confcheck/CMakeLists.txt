# WRF configuration checks
wrf_conf_check(
                RUN
                RESULT_VAR Fortran_2003_IEEE
                SOURCES    ${PROJECT_SOURCE_DIR}/tools/fortran_2003_ieee_test.F
                MESSAGE    "Some IEEE Fortran 2003 features missing, removing usage of these features"
                )

wrf_conf_check(
                RUN
                RESULT_VAR Fortran_2003_ISO_C
                SOURCES    ${PROJECT_SOURCE_DIR}/tools/fortran_2003_iso_c_test.F
                MESSAGE    "Some ISO_C Fortran 2003 features missing, removing usage ISO_C and stubbing code dependent on it"
                )

wrf_conf_check(
                RUN
                RESULT_VAR Fortran_2003_FLUSH
                SOURCES    ${PROJECT_SOURCE_DIR}/tools/fortran_2003_flush_test.F
                MESSAGE    "Standard FLUSH routine Fortran 2003 features missing, checking for alternate Fortran_2003_FFLUSH"
                )

if ( NOT ${Fortran_2003_FLUSH} )
  wrf_conf_check(
                  RUN
                  RESULT_VAR Fortran_2003_FFLUSH
                  SOURCES    ${PROJECT_SOURCE_DIR}/tools/fortran_2003_fflush_test.F
                  MESSAGE    "Standard FFLUSH routine Fortran 2003 features missing, no alternate to FLUSH found, feature stubbed out"
                  )
endif()

wrf_conf_check(
                RUN
                RESULT_VAR Fortran_2003_GAMMA
                SOURCES    ${PROJECT_SOURCE_DIR}/tools/fortran_2008_gamma_test.F
                MESSAGE    "Some Fortran 2003 features missing, removing usage gamma function intrinsic and stubbing code dependent on it"
                )



wrf_conf_check(
                RUN
                RESULT_VAR             FSEEKO64
                SOURCES                ${PROJECT_SOURCE_DIR}/tools/fseek_test.c
                OPTIONS
                  COMPILE_DEFINITIONS -DTEST_FSEEKO64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE=1 -DFILE_TO_TEST="${PROJECT_SOURCE_DIR}/CMakeLists.txt"
                MESSAGE                "fseeko64 not supported, checking alternate fseeko"
                )

if ( NOT "${FSEEKO64}" )
  wrf_conf_check(
                  RUN
                  RESULT_VAR             FSEEKO
                  SOURCES                ${PROJECT_SOURCE_DIR}/tools/fseek_test.c
                  OPTIONS
                    COMPILE_DEFINITIONS -DTEST_FSEEKO -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE=1 -DFILE_TO_TEST="${PROJECT_SOURCE_DIR}/CMakeLists.txt"
                  MESSAGE                "fseeko not supported, compiling with fseek (caution with large files)"
                  )
endif()

# Unsure if this is even necessary. Defines littered throughout configure.defaults
# if ( ${USE_MPI} )
#   wrf_conf_check(
#                   RUN
#                   SOURCE_TYPE           C
#                   RESULT_VAR            MPI2_SUPPORT
#                   SOURCE                ${PROJECT_SOURCE_DIR}/tools/mpi2_test.c
#                   EXTENSION             .c
#                   MESSAGE               "MPI_Comm_f2c() and MPI_Comm_c2f() not supported"
#                   )

#   wrf_conf_check(
#                   RUN
#                   SOURCE_TYPE           C
#                   RESULT_VAR            MPI2_THREAD_SUPPORT
#                   SOURCE                ${PROJECT_SOURCE_DIR}/tools/mpi2_thread_test.c
#                   EXTENSION             .c
#                   MESSAGE               "MPI_Init_thread() not supported"
#                   )
# endif()
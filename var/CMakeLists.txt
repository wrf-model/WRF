
if ( ${ENABLE_CRTM} )
  add_subdirectory( external/crtm_2.3.0 )
endif()

if ( ${ENABLE_BUFR} )
  add_subdirectory( external/bufr )
endif()

if ( ${ENABLE_WAVELET} )
  add_subdirectory( external/wavelet )
endif()

wrf_m4_preproc_fortran( 
                        TARGET_NAME     da_generic_boilerplate
                        OUTPUT_DIR      ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                        EXTENSION       ".inc"
                        SOURCES         da/da_par_util/da_generic_boilerplate.m4
                        TARGET_SCOPE    ${PROJECT_NAME}_Core
                        FLAGS           ${M4_FLAGS}
                        )


add_dependencies( ${PROJECT_NAME}_Core da_generic_boilerplate )
target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              ${CMAKE_CURRENT_BINARY_DIR}/preproc/
                            )

# To somewhat mirror the DA build (and for "neatness") I won't put cmake files
# in each source folder and instead build at this level

target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  # blas and lapack
                  external/blas/da_blas.f90
                  external/lapack/da_lapack.f90

                  # da gen_be aero mod
                  gen_be/aero_mod.f90
                )


target_include_directories(
                            ${PROJECT_NAME}_Core
                            PRIVATE
                              # blas and lapack
                              external/blas
                              external/lapack
                              )

# Disable float to double precision on these files always
set_source_files_properties(
                            # blas and lapack
                            external/blas/da_blas.f90
                            external/lapack/da_lapack.f90
                            TARGET_DIRECTORY ${PROJECT_NAME}_Core
                            PROPERTIES
                              COMPILE_OPTIONS_Fortran_PRECISION "" # set to nothing to void
                            )

# Add da/da* and gen_be executables dir
add_subdirectory( da )
add_subdirectory( gen_be )


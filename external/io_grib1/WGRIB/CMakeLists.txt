# WRF CMake Build

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  # wgrib_main.c # Driver
                  seekgrib.c
                  ibm2flt.c
                  readgrib.c
                  intpower.c
                  cnames.c
                  BDSunpk.c
                  flt2ieee.c
                  wrtieee.c
                  levels.c
                  PDStimes.c
                  missing.c
                  nceptable_reanal.c
                  nceptable_opn.c
                  ensemble.c
                  ombtable.c
                  ec_ext.c
                  gribtable.c
                  gds_grid.c
                  PDS_date.c
                  ectable_128.c
                  ectable_129.c
                  ectable_130.c
                  ectable_131.c
                  ectable_140.c
                  ectable_150.c
                  ectable_151.c
                  ectable_160.c
                  ectable_170.c
                  ectable_180.c
                  nceptab_129.c
                  dwdtable_002.c
                  dwdtable_201.c
                  dwdtable_202.c
                  dwdtable_203.c
                  cptectable_254.c
                  nceptab_130.c
                  nceptab_131.c
                  )


target_link_libraries(  ${FOLDER_COMPILE_TARGET}
                          PRIVATE
                            ${netCDF_LIBRARIES}
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                            $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        )

target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${netCDF_INCLUDE_DIRS}
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

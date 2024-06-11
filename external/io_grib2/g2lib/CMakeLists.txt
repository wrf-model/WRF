# WRF CMake Build
get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  addfield.F
                  addgrid.F
                  addlocal.F
                  cmplxpack.F
                  compack.F
                  comunpack.F
                  dec_jpeg2000.c
                  dec_png.c
                  drstemplates.F
                  enc_jpeg2000.c
                  enc_png.c
                  g2grids.F
                  gb_info.F
                  gbytesc.F
                  gdt2gds.F
                  getdim.F
                  getfield.F
                  getg2i.F
                  getg2ir.F
                  getgb2.F
                  getgb2l.F
                  getgb2p.F
                  getgb2r.F
                  getgb2rp.F
                  getgb2s.F
                  getidx.F
                  getlocal.F
                  getpoly.F
                  gettemplates.F
                  gf_free.F
                  gf_getfld.F
                  gf_unpack1.F
                  gf_unpack2.F
                  gf_unpack3.F
                  gf_unpack4.F
                  gf_unpack5.F
                  gf_unpack6.F
                  gf_unpack7.F
                  gribcreate.F
                  gribend.F
                  gribinfo.F
                  gribmod.F
                  gridtemplates.F
                  ixgb2.F
                  jpcpack.F
                  jpcunpack.F
                  misspack.F
                  mkieee.F
                  mova2i.c
                  pack_gp.F
                  params.F
                  pdstemplates.F
                  pngpack.F
                  pngunpack.F
                  putgb2.F
                  rdieee.F
                  realloc.F
                  reduce.F
                  simpack.F
                  simunpack.F
                  skgb.F
                  specpack.F
                  specunpack.F
                  )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                          # Fortran_FORMAT           FREE                          
                      )


target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}/../io_grib_share
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

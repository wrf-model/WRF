# WRF CMake Build

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  FTP_getfile.c
                  apply_bitmap.c
                  display_gribhdr.c
                  gbyte.c
                  grib_dec.c
                  grib_enc.c
                  grib_seek.c
                  gribgetbds.c
                  gribgetbms.c
                  gribgetgds.c
                  gribgetpds.c
                  gribhdr2file.c
                  gribputbds.c
                  gribputgds.c
                  gribputpds.c
                  hdr_print.c
                  init_dec_struct.c
                  init_enc_struct.c
                  init_gribhdr.c
                  init_struct.c
                  ld_dec_lookup.c
                  ld_enc_input.c
                  ld_enc_lookup.c
                  ld_grib_origctrs.c
                  make_default_grbfn.c
                  make_grib_log.c
                  map_lvl.c
                  map_parm.c
                  pack_spatial.c
                  prt_inp_struct.c
                  upd_child_errmsg.c
                  prt_badmsg.c
                  swap.c
                  grib_uthin.c
                  set_bytes.c
                  )


target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              # ${CMAKE_CURRENT_SOURCE_DIR}
                              ${CMAKE_CURRENT_SOURCE_DIR}/../../ioapi_share
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )

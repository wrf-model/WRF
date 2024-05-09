# WRF CMake Build

get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )

target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  c1f2kb.F  cfft1b.F  cmf3kf.F  cosqb1.F  costmi.F   dcosq1f.F  dfftb1.F   mradb2.F  mrfti1.F  r1fgkf.F     rfft2i.F  sinqmi.F  z1f2kf.F  zfft1f.F  zmf4kb.F
                  c1f2kf.F  cfft1f.F  cmf4kb.F  cosqf1.F  d1f2kb.F   dcosq1i.F  dfftf1.F   mradb3.F  msntb1.F  r4_factor.F  rfftb1.F  sint1b.F  z1f3kb.F  zfft1i.F  zmf4kf.F
                  c1f3kb.F  cfft1i.F  cmf4kf.F  cosqmb.F  d1f2kf.F   dcosqb1.F  dffti1.F   mradb4.F  msntf1.F  r4_mcfti1.F  rfftf1.F  sint1f.F  z1f3kf.F  zfft2b.F  zmf5kb.F
                  c1f3kf.F  cfft2b.F  cmf5kb.F  cosqmf.F  d1f3kb.F   dcosqf1.F  dsint1b.F  mradb5.F  r1f2kb.F  r4_tables.F  rffti1.F  sint1i.F  z1f4kb.F  zfft2f.F  zmf5kf.F
                  c1f4kb.F  cfft2f.F  cmf5kf.F  cosqmi.F  d1f3kf.F   dcost1b.F  dsint1f.F  mradbg.F  r1f2kf.F  r8_factor.F  rfftmb.F  sintb1.F  z1f4kf.F  zfft2i.F  zmfgkb.F
                  c1f4kf.F  cfft2i.F  cmfgkb.F  cost1b.F  d1f4kb.F   dcost1f.F  dsint1i.F  mradf2.F  r1f3kb.F  r8_mcfti1.F  rfftmf.F  sintf1.F  z1f5kb.F  zfftmb.F  zmfgkf.F
                  c1f5kb.F  cfftmb.F  cmfgkf.F  cost1f.F  d1f4kf.F   dcost1i.F  dsintb1.F  mradf3.F  r1f3kf.F  r8_tables.F  rfftmi.F  sintmb.F  z1f5kf.F  zfftmf.F  zmfm1b.F
                  c1f5kf.F  cfftmf.F  cmfm1b.F  cost1i.F  d1f5kb.F   dcostb1.F  dsintf1.F  mradf4.F  r1f4kb.F  rfft1b.F     sinq1b.F  sintmf.F  z1fgkb.F  zfftmi.F  zmfm1f.F
                  c1fgkb.F  cfftmi.F  cmfm1f.F  costb1.F  d1f5kf.F   dcostf1.F  mcsqb1.F   mradf5.F  r1f4kf.F  rfft1f.F     sinq1f.F  sintmi.F  z1fgkf.F  zmf2kb.F
                  c1fgkf.F  cmf2kb.F  cosq1b.F  costf1.F  d1fgkb.F   dfft1b.F   mcsqf1.F   mradfg.F  r1f5kb.F  rfft1i.F     sinq1i.F  xercon.F  z1fm1b.F  zmf2kf.F
                  c1fm1b.F  cmf2kf.F  cosq1f.F  costmb.F  d1fgkf.F   dfft1f.F   mcstb1.F   mrftb1.F  r1f5kf.F  rfft2b.F     sinqmb.F  xerfft.F  z1fm1f.F  zmf3kb.F
                  c1fm1f.F  cmf3kb.F  cosq1i.F  costmf.F  dcosq1b.F  dfft1i.F   mcstf1.F   mrftf1.F  r1fgkb.F  rfft2f.F     sinqmf.F  z1f2kb.F  zfft1b.F  zmf3kf.F
                  )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          Fortran_FORMAT           FREE
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
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


if ( ${ENABLE_CRTM} )
  add_subdirectory( external/crtm_2.3.0 )
endif()

if ( ${ENABLE_BUFR} )
  add_subdirectory( external/bufr )
endif()

if ( ${ENABLE_WAVELET} )
  add_subdirectory( external/wavelet )
endif()

# WRF Macro for getting version, this *should* be replaced with a better versioning scheme 
macro( wrf_get_version WRF_VERSION_FILE )
  file( STRINGS ${WRF_VERSION_FILE} WRF_VERSION_FILE_OUTPUT )

  list( POP_FRONT WRF_VERSION_FILE_OUTPUT FIRST_LINE )
  string( REPLACE " " ";" FIRST_LINE_LIST ${FIRST_LINE} )
  list( GET FIRST_LINE_LIST -1 WRF_VERSION )

  set( PROJECT_VERSION ${WRF_VERSION} )
  message( STATUS "Setting project version to ${PROJECT_VERSION}" )
endmacro()

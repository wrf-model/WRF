PROGRAM test
  IMPLICIT NONE

  CHARACTER, DIMENSION(:), POINTER :: grib_table_info
  INTEGER :: ret
  INTEGER :: size
  INTEGER :: index
  INTEGER :: istat

  CALL GET_GRIB1_TABLE_INFO_SIZE(size)
  ALLOCATE(grib_table_info(1:size), STAT=istat)
  CALL LOAD_GRIB1_TABLE_INFO("gribmap.txt",grib_table_info,ret)
  print *,'ret: ',ret
  print *,'again'
  CALL GET_GRIB_PARAM (grib_table_info, "TSK", index);
  print *,'got index: ',index


print *,'here1'

END PROGRAM

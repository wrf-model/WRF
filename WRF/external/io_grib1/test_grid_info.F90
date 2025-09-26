

PROGRAM test
  IMPLICIT NONE

  CHARACTER, DIMENSION(:), POINTER :: grib_table_info
  CHARACTER, DIMENSION(:), POINTER :: grid_info
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

  CALL GET_GRID_INFO_SIZE(size)
  ALLOCATE(grid_info(1:size), STAT=istat)
  CALL LOAD_GRID_INFO("test", "200509081200", 1, &
       1.0, 1.0, 1440.0, &
       0, 240, 1, &
       450, 250, 45.0, &
       -100.0, 12.0, 12.0, -101.0, &
       1, 30.0, &
       60.0, grib_table_info, &
       grid_info)

  print *,'here!'
  CALL PRINT_GRID_INFO(grid_info)
  CALL FREE_GRID_INFO(grid_info)

print *,'here1'

END PROGRAM


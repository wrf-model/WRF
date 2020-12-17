
PROGRAM test_grib1_routines
  IMPLICIT NONE

  CHARACTER, DIMENSION(:), POINTER :: fileindex
  REAL     , DIMENSION(:), POINTER :: data
  INTEGER :: fid
  INTEGER :: err
  INTEGER :: ret
  INTEGER :: size
  INTEGER :: index
  INTEGER :: istat

  INTEGER :: parmid
  CHARACTER(40) :: datestr
  INTEGER :: leveltype 
  INTEGER :: level1
  INTEGER :: level2
  INTEGER :: fcsttime1
  INTEGER :: fcsttime2
  character(200) :: value
  character(20) :: strval
  integer :: test
  character(50) :: form
  integer :: NumberTimes
  character (19) :: Time
  character (19), pointer  :: Times(:)
  integer :: ierr
  integer :: numcols, numrows
  integer :: center, subcenter, parmtbl
  

  parmid=33
  datestr = '2005041412'
  leveltype = 119
  level1 = 9965
  level2 = -HUGE(1)
  fcsttime1 = 0
  fcsttime2 = -HUGE(1)
  center = 250
  subcenter = 2
  parmtbl = 200

  print *,'about to call GET_FILEINDEX_SIZE'
  CALL GET_FILEINDEX_SIZE(size)
  ALLOCATE(fileindex(1:size), STAT=istat)

  print *,'about to call ALLOC_INDEX_FILE'
  CALL ALLOC_INDEX_FILE(fileindex)
  print *,'about to call OPEN_FILE'
  CALL OPEN_FILE('test.grb','r',fid,err)
  print *,'about to call INDEX_FILE'
  CALL INDEX_FILE(fid,fileindex)

  print *,'about to call GET_GRIB_INDEX'
  CALL GET_GRIB_INDEX(fileindex, center, subcenter, parmtbl, &
       parmid,trim(datestr),leveltype,level1,level2, &
       fcsttime1,fcsttime2,index)
  print *,'got grib index: ',index

  print *,'about to call GET_METADATA_VALUE'
  CALL GET_METADATA_VALUE(fileindex, 'GRIB_GRID_ID', "none", "none", &
       Value, istat)

  print *,'about to call GET_NUM_TIMES'
  CALL GET_NUM_TIMES(fileindex, NumberTimes)
  print *,'found ',NumberTimes,' times'

  ALLOCATE(Times(1:NumberTimes), STAT=ierr)
  print *,'about to call GET_TIME'
  CALL GET_TIME(fileindex,1,Time)
  print *,'Time: ',Time

  print *,'about to call GET_SIZEOF_GRID'
  CALL GET_SIZEOF_GRID(fileindex,index,numcols,numrows)
  allocate(data(1:numcols*numrows))

  print *,'about to call READ_GRIB'
  CALL READ_GRIB(fileindex,fid,index,data)
  print *,'data(20): ',data(20)

  deallocate(data)

  print *,'about to call FREE_INDEX_FILE'
  CALL FREE_INDEX_FILE(fileindex)

  print *,'program completed'

END PROGRAM

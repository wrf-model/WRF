
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
  

  parmid=34
  datestr = '2005041412'
  leveltype = 119
  level1 = 9965
  level2 = -HUGE(1)
  fcsttime1 = 0
  fcsttime2 = -HUGE(1)

  CALL GET_FILEINDEX_SIZE(size)
  ALLOCATE(fileindex(1:size), STAT=istat)

  CALL ALLOC_INDEX_FILE(fileindex)
!  CALL OPEN_FILE('200504141200_boundary_domain1.grb','r',fid,err)
  CALL OPEN_FILE('test.grb','r',fid,err)
  CALL INDEX_FILE(fid,fileindex)

  CALL GET_GRIB_INDEX(fileindex,parmid,datestr,leveltype,level1,level2, &
       fcsttime1,fcsttime2,index)
  print *,'got grib index: ',index

  CALL GET_METADATA_VALUE(fileindex, 'GRIB_GRID_ID', "none", "none", &
       Value, istat)
  print *,'got metadata value: ',Value
  print *,'istat: ',istat

  CALL GET_NUM_TIMES(fileindex, NumberTimes)
  print *,'found ',NumberTimes,' times'

  ALLOCATE(Times(1:NumberTimes), STAT=ierr)
  CALL GET_TIME(fileindex,1,Time)
  print *,'Time: ',Time

  CALL GET_SIZEOF_GRID(fileindex,index,numcols,numrows)
  allocate(data(1:numcols*numrows))

  CALL READ_GRIB(fileindex,fid,index,data)
  print *,'data(20): ',data(20)

  deallocate(data)

  strval = Value(1:20)//'   '
!  strval='24                                                                                      '
  print *,'strval: ',strval,'end'
!  read(strval,*)test
  print *,'test: ',test

  CALL FREE_INDEX_FILE(fileindex)


END PROGRAM

!WRF:MEDIATION_LAYER:IO
!

MODULE module_io_domain
USE module_io
USE module_io_wrf
USE module_configure, ONLY : grid_config_rec_type
USE module_domain, ONLY : domain

CONTAINS

  SUBROUTINE open_r_dataset ( id , fname , grid , config_flags , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*128             :: DataSet, tmp
   LOGICAL                   :: anyway
   CALL wrf_open_for_read ( fname ,                     &
                            grid ,                      &
                            sysdepinfo ,                &
                            id ,                        &
                            ierr )
   RETURN
  END SUBROUTINE open_r_dataset

  SUBROUTINE open_w_dataset ( id , fname , grid , config_flags , outsub , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   EXTERNAL outsub
   CHARACTER*128             :: DataSet, sysdepinfo_tmp
   LOGICAL                   :: anyway
   CALL wrf_debug ( 100 , 'calling wrf_open_for_write_begin in open_w_dataset' )
   sysdepinfo_tmp = ' '
   IF ( grid%id < 10 ) THEN
     write(sysdepinfo_tmp,'(a,i1)')TRIM(sysdepinfo)//',GRIDID=',grid%id
   ELSE
     write(sysdepinfo_tmp,'(a,i2)')TRIM(sysdepinfo)//',GRIDID=',grid%id
   ENDIF
   CALL wrf_open_for_write_begin ( fname ,     &
                                   grid ,      &
                                   sysdepinfo_tmp ,            &
                                   id ,                        &
                                   ierr )
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling outsub in open_w_dataset' )
     CALL outsub( id , grid , config_flags , ierr )
     CALL wrf_debug ( 100 , 'back from outsub in open_w_dataset' )
   ENDIF
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling wrf_open_for_write_commit in open_w_dataset' )
     CALL wrf_open_for_write_commit ( id , ierr )
     CALL wrf_debug ( 100 , 'back from wrf_open_for_write_commit in open_w_dataset' )
   ENDIF
  END SUBROUTINE open_w_dataset

  SUBROUTINE open_u_dataset ( id , fname , grid , config_flags , insub , sysdepinfo, ierr )
   TYPE (domain)             :: grid
   CHARACTER*(*) :: fname
   CHARACTER*(*) :: sysdepinfo
   INTEGER      , INTENT(INOUT) :: id , ierr
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   EXTERNAL insub
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_debug ( 100 , 'calling wrf_open_for_read_begin in open_u_dataset' )
   CALL wrf_open_for_read_begin ( fname ,     &
                                   grid ,     &
                                   sysdepinfo ,                &
                                   id ,                        &
                                   ierr )
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling insub in open_u_dataset' )
     CALL insub( id , grid , config_flags , ierr )
   ENDIF
   IF ( ierr .LE. 0 ) THEN
     CALL wrf_debug ( 100 , 'calling wrf_open_for_read_commit in open_u_dataset' )
     CALL wrf_open_for_read_commit ( id , ierr )
     CALL wrf_debug ( 100 , 'back from wrf_open_for_read_commit in open_u_dataset' )
   ENDIF
  END SUBROUTINE open_u_dataset

  SUBROUTINE close_dataset( id , config_flags, sysdepinfo ) 
   IMPLICIT NONE
   INTEGER id , ierr
   LOGICAL , EXTERNAL :: wrf_dm_on_monitor
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   CHARACTER*(*) :: sysdepinfo
   CHARACTER*128             :: DataSet
   LOGICAL                   :: anyway
   CALL wrf_ioclose( id , ierr )
  END SUBROUTINE close_dataset


! ------------  Output model input data sets

#include "module_io_domain_defs.inc"

!  ------------ Input model restart data sets

  SUBROUTINE input_restart ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_restart .GT. 0 ) THEN
      CALL input_wrf ( fid , grid , config_flags , restart_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE input_restart

!  ------------ Input model boundary data sets

  SUBROUTINE input_boundary ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_boundary .GT. 0 ) THEN
      CALL input_wrf ( fid , grid , config_flags , boundary_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE input_boundary

!  ------------ Output model restart data sets

  SUBROUTINE output_restart ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr 
    IF ( config_flags%io_form_restart .GT. 0 ) THEN
      CALL output_wrf ( fid , grid , config_flags , restart_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE output_restart

!  ------------ Output model boundary data sets

  SUBROUTINE output_boundary ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid 
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_boundary .GT. 0 ) THEN
      CALL output_wrf ( fid , grid , config_flags , boundary_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE output_boundary

END MODULE module_io_domain

! move outside module so callable without USE of module
SUBROUTINE construct_filename1( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_d" // TRIM(t1)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename1

SUBROUTINE construct_filename2( result , basename , fld1 , len1 , date_char )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // ".d" // TRIM(t1) // "." // TRIM(date_char)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename2

! this version looks for <date> and <domain> in the basename and replaces with the arguments

SUBROUTINE construct_filename2a( result , basename , fld1 , len1 , date_char )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  INTEGER   i, j, l

  result=basename
  CALL zero_pad ( t1 , fld1 , len1 )


! The string name length 12345678  including < > ----|
!                        ||||||||                    |
  i = index( result   , '<domain>' )       ! is this |
  DO WHILE ( i .GT. 0 )                    ! value   |
    l = len(trim(result))                  !        \/
    result = result  (1:i-1) // TRIM(t1) // result(i+8:l)
    i = index( result   , '<domain>' ) 
  END DO

  i = index( result , '<date>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char) // result(i+6:l)
    i = index( result , '<date>' )
  END DO

  i = index( result , '<year>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 1: 4)) // result(i+6:l)
    i = index( result , '<year>' )
  END DO

  i = index( result , '<month>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 6: 7)) // result(i+7:l)
    i = index( result , '<month>' )
  END DO

  i = index( result , '<day>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 9:10)) // result(i+5:l)
    i = index( result , '<day>' )
  END DO

  i = index( result , '<hour>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char(12:13)) // result(i+6:l)
    i = index( result , '<hour>' )
  END DO

  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename2a

SUBROUTINE construct_filename ( result , basename , fld1 , len1 , fld2 , len2 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2
  CHARACTER*64         :: t1, t2, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename

SUBROUTINE construct_filename3 ( result , basename , fld1 , len1 , fld2 , len2, fld3, len3 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1 , fld2 , len2, fld3, len3
  CHARACTER*64         :: t1, t2, t3, zeros

  CALL zero_pad ( t1 , fld1 , len1 )
  CALL zero_pad ( t2 , fld2 , len2 )
  CALL zero_pad ( t3 , fld3 , len3 )
  result = TRIM(basename) // "_d" // TRIM(t1) // "_" // TRIM(t2) // "_" // TRIM(t3)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename3

SUBROUTINE construct_filename4( result , basename , fld1 , len1 , date_char , io_form )
  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER, EXTERNAL :: use_package
  INTEGER , INTENT(IN) :: fld1 , len1 , io_form
  CHARACTER*64         :: t1, zeros
  CHARACTER*4          :: ext
  CALL zero_pad ( t1 , fld1 , len1 )
  IF      ( use_package(io_form) .EQ. IO_INTIO ) THEN
     ext = '.int'
  ELSE IF ( use_package(io_form) .EQ. IO_NETCDF ) THEN
     ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PNETCDF ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PIO ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_GRIB1 ) THEN
     ext = '.gb '
  ELSE
     CALL wrf_error_fatal ('improper io_form')
  END IF
  result = TRIM(basename) // ".d" // TRIM(t1) // "." // TRIM(date_char) // TRIM(ext)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE construct_filename4

! this version looks for <date> and <domain> in the basename and replaces with the arguments

SUBROUTINE construct_filename4a( result , basename , fld1 , len1 , date_char , io_form )
  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  CHARACTER*(*) :: date_char

  INTEGER, EXTERNAL :: use_package
  INTEGER , INTENT(IN) :: fld1 , len1 , io_form
  CHARACTER*64         :: t1, zeros
  CHARACTER*4          :: ext
  INTEGER   i, j, l
  result=basename
  CALL zero_pad ( t1 , fld1 , len1 )
  IF      ( use_package(io_form) .EQ. IO_INTIO ) THEN
     ext = '.int'
  ELSE IF ( use_package(io_form) .EQ. IO_NETCDF ) THEN
     ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PNETCDF ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_PIO ) THEN
    ext = '.nc '
  ELSE IF ( use_package(io_form) .EQ. IO_GRIB1 ) THEN
     ext = '.gb '
  ELSE
     CALL wrf_error_fatal ('improper io_form')
  END IF
  l = len(trim(basename))
  result = basename(1:l) // TRIM(ext)

! The string name length 12345678  including < >   ----|
!                        ||||||||                      |
  i = index( result   , '<domain>' )       !   is this |
  DO WHILE ( i .GT. 0 )                    !   value   |
    l = len(trim(result  ))                !          \/
    result = result  (1:i-1) // TRIM(t1) // result  (i+8:l)
    i = index( result   , '<domain>' )
  END DO

  i = index( result , '<date>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char) // result(i+6:l)
    i = index( result , '<date>' )
  END DO

  i = index( result , '<year>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 1: 4)) // result(i+6:l)
    i = index( result , '<year>' )
  END DO

  i = index( result , '<month>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 6: 7)) // result(i+7:l)
    i = index( result , '<month>' )
  END DO

  i = index( result , '<day>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char( 9:10)) // result(i+5:l)
    i = index( result , '<day>' )
  END DO

  i = index( result , '<hour>' )
  DO WHILE ( i .GT. 0 )
    l = len(trim(result))
    result = result(1:i-1) // TRIM(date_char(12:13)) // result(i+6:l)
    i = index( result , '<hour>' )
  END DO
  CALL maybe_remove_colons(result)

  RETURN
END SUBROUTINE construct_filename4a

SUBROUTINE append_to_filename ( result , basename , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  CHARACTER*(*) :: basename
  INTEGER , INTENT(IN) :: fld1 , len1
  CHARACTER*64         :: t1, zeros
  
  CALL zero_pad ( t1 , fld1 , len1 )
  result = TRIM(basename) // "_" // TRIM(t1)
  CALL maybe_remove_colons(result)
  RETURN
END SUBROUTINE append_to_filename

SUBROUTINE zero_pad ( result , fld1 , len1 )
  IMPLICIT NONE
  CHARACTER*(*) :: result
  INTEGER , INTENT (IN)      :: fld1 , len1
  INTEGER                    :: d , x
  CHARACTER*64         :: t2, zeros
  x = fld1 ; d = 0
  DO WHILE ( x > 0 )
    x = x / 10
    d = d + 1
  END DO
  write(t2,'(I9)')fld1
  zeros = '0000000000000000000000000000000'
  result = zeros(1:len1-d) // t2(9-d+1:9)
  RETURN
END SUBROUTINE zero_pad

SUBROUTINE init_wrfio
   USE module_io, ONLY : wrf_ioinit
   IMPLICIT NONE
   INTEGER ierr
   CALL wrf_ioinit(ierr)
END SUBROUTINE init_wrfio

!<DESCRIPTION>
! This routine figures out the nearest previous time instant 
! that corresponds to a multiple of the input time interval.
! Example use is to give the time instant that corresponds to 
! an I/O interval, even when the current time is a little bit
! past that time when, for example, the number of model time
! steps does not evenly divide the I/O interval. JM 20051013
!</DESCRIPTION>
! 
SUBROUTINE adjust_io_timestr ( TI, CT, ST, timestr )
   USE module_utility
   IMPLICIT NONE
! Args
   TYPE(WRFU_Time), INTENT(IN)            :: ST,CT    ! domain start and current time
   TYPE(WRFU_TimeInterval), INTENT(IN)    :: TI       ! interval
   CHARACTER*(*), INTENT(INOUT)           :: timestr  ! returned string
! Local
   TYPE(WRFU_Time)                        :: OT
   TYPE(WRFU_TimeInterval)                :: IOI
   INTEGER                                :: n

   IOI = CT-ST                               ! length of time since starting
   n = WRFU_TimeIntervalDIVQuot( IOI , TI )  ! number of whole time intervals
   IOI = TI * n                              ! amount of time since starting in whole time intervals
   OT = ST + IOI                             ! previous nearest time instant
   CALL wrf_timetoa( OT, timestr )           ! generate string
   RETURN
END SUBROUTINE adjust_io_timestr

! Modify the filename to remove things like ':' from the file name
! unless it is a drive number. Convert to '_' instead.

SUBROUTINE maybe_remove_colons( FileName )
  CHARACTER*(*) FileName
  CHARACTER c, d
  INTEGER i, l
  LOGICAL nocolons
  l = LEN(TRIM(FileName))
! do not change first two characters (naive way of dealing with
! possiblity of drive name in a microsoft path
  CALL nl_get_nocolons(1,nocolons)
  IF ( nocolons ) THEN
    DO i = 3, l
      IF ( FileName(i:i) .EQ. ':' ) THEN
        FileName(i:i) = '_'
! Remove this modification to filename - dashes are OK
!      ELSE IF ( FileName(i:i) .EQ. '-' ) THEN
!        FileName(i:i) = '_'
      ENDIF
    ENDDO
  ENDIF
  RETURN
END




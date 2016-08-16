!WRF:DRIVER_LAYER:IO
!
#define DEBUG_LVL 500

MODULE module_io
!<DESCRIPTION>
!<PRE>
! WRF-specific package-independent interface to package-dependent WRF-specific
! I/O packages.
!
! These routines have the same names as those specified in the WRF I/O API 
! except that:
! - Routines defined in this file and called by users of this module have 
!   the "wrf_" prefix.  
! - Routines defined in the I/O packages and called from routines in this 
!   file have the "ext_" prefix.  
! - Routines called from routines in this file to initiate communication 
!   with I/O quilt servers have the "wrf_quilt_" prefix.  
!
! See http://www.mmm.ucar.edu/wrf/WG2/software_2.0/IOAPI.doc for the latest 
! version of the WRF I/O API.  This document includes detailed descriptions 
! of subroutines and their arguments that are not duplicated in this file.  
!
! We wish to be able to link to different packages depending on whether
! the I/O is restart, initial, history, or boundary.  
!</PRE>
!</DESCRIPTION>

  USE module_configure

  LOGICAL :: is_inited = .FALSE.
  INTEGER, PARAMETER, PRIVATE :: MAX_WRF_IO_HANDLE = 1000
  INTEGER :: wrf_io_handles(MAX_WRF_IO_HANDLE)
  INTEGER :: how_opened(MAX_WRF_IO_HANDLE) 
  LOGICAL :: for_output(MAX_WRF_IO_HANDLE), first_operation(MAX_WRF_IO_HANDLE)
  INTEGER :: filtno = 0
  LOGICAL, PRIVATE :: bdy_dist_flag = .TRUE.   ! false is old style undecomposed boundary data structs,
                                                ! true is new style decomposed boundary data structs
                                                ! are_bdys_distributed, bdys_are_distributed and
                                                ! bdys_not_distributed routines access this flag
  CHARACTER*256 extradims

!<DESCRIPTION>
!<PRE>
!
! include the file generated from md_calls.m4 using the m4 preprocessor
! note that this file also includes the CONTAINS declaration for the module
!
!</PRE>
!</DESCRIPTION>
#include "md_calls.inc"

!--- registry-generated routine that gets the io format being used for a dataset

  INTEGER FUNCTION io_form_for_dataset ( DataSet )
    IMPLICIT NONE
    CHARACTER*(*), INTENT(IN)  :: DataSet
    INTEGER                    :: io_form 
#include "io_form_for_dataset.inc"
    io_form_for_dataset = io_form
    RETURN
  END FUNCTION io_form_for_dataset

  INTEGER FUNCTION io_form_for_stream ( stream )
    USE module_streams
    IMPLICIT NONE
    INTEGER,       INTENT(IN)  :: stream
    INTEGER                    :: io_form 
#include "io_form_for_stream.inc"
    io_form_for_stream = io_form
    RETURN
  END FUNCTION io_form_for_stream

!--- ioinit

SUBROUTINE wrf_ioinit( Status )
!<DESCRIPTION>
!<PRE>
! Initialize the WRF I/O system.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status
!Local
  CHARACTER(len=80) :: SysDepInfo
  INTEGER :: ierr(100), minerr, maxerr
!
  Status = 0
  ierr = 0
  SysDepInfo = " "
  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioinit' )
  CALL init_io_handles    ! defined below
#ifdef NETCDF
  if ( model_config_rec%use_netcdf_classic ) SysDepInfo="use_netcdf_classic"
  CALL ext_ncd_ioinit  ( SysDepInfo, ierr( 1) )
  SysDepInfo = " "
#endif
#ifdef INTIO
  CALL ext_int_ioinit  ( SysDepInfo, ierr( 2) )
#endif
#ifdef PHDF5
  CALL ext_phdf5_ioinit( SysDepInfo, ierr( 3) )
#endif
#ifdef MCELIO
  CALL ext_mcel_ioinit ( SysDepInfo, ierr( 4) )
#endif
#ifdef XXX
  CALL ext_xxx_ioinit  ( SysDepInfo, ierr( 5) )
#endif
#ifdef YYY
  CALL ext_yyy_ioinit  ( SysDepInfo, ierr( 6) )
#endif
#ifdef ZZZ
  CALL ext_zzz_ioinit  ( SysDepInfo, ierr( 7) )
#endif
#ifdef ESMFIO
  CALL ext_esmf_ioinit ( SysDepInfo, ierr( 8) )
#endif
#ifdef GRIB1
  CALL ext_gr1_ioinit  ( SysDepInfo, ierr( 9) )
#endif
#ifdef GRIB2
  CALL ext_gr2_ioinit  ( SysDepInfo, ierr(10) )
#endif
#ifdef PNETCDF
  CALL ext_pnc_ioinit  ( SysDepInfo, ierr(11) )
#endif
#ifdef PIO
  CALL ext_pio_ioinit  ( SysDepInfo, ierr(12) )
#endif
  minerr = MINVAL(ierr)
  maxerr = MAXVAL(ierr)
  IF ( minerr < 0 ) THEN
    Status = minerr
  ELSE IF ( maxerr > 0 ) THEN
    Status = maxerr
  ELSE
    Status = 0
  ENDIF
END SUBROUTINE wrf_ioinit

!--- ioexit

SUBROUTINE wrf_ioexit( Status )
!<DESCRIPTION>
!<PRE>
! Shut down the WRF I/O system.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status
!Local
  LOGICAL, EXTERNAL :: use_output_servers
  INTEGER :: ierr(100), minerr, maxerr
!
  Status = 0
  ierr = 0
  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioexit' )
#ifdef NETCDF
  CALL ext_ncd_ioexit  ( ierr( 1) )
#endif
#ifdef INTIO
  CALL ext_int_ioexit  ( ierr( 2) )
#endif
#ifdef PHDF5
  CALL ext_phdf5_ioexit( ierr( 3) )
#endif
#ifdef MCELIO
  CALL ext_mcel_ioexit ( ierr( 4) )
#endif
#ifdef XXX
  CALL ext_xxx_ioexit  ( ierr( 5) )
#endif
#ifdef YYY
  CALL ext_yyy_ioexit  ( ierr( 6) )
#endif
#ifdef ZZZ
  CALL ext_zzz_ioexit  ( ierr( 7) )
#endif
#ifdef ESMFIO
  CALL ext_esmf_ioexit ( ierr( 8) )
#endif
#ifdef GRIB1
  CALL ext_gr1_ioexit  ( ierr( 9) )
#endif
#ifdef GRIB2
  CALL ext_gr2_ioexit  ( ierr(10) )
#endif
#ifdef PNETCDF
  CALL ext_pnc_ioexit  ( ierr(11) )
#endif
#ifdef PIO
  CALL ext_pio_ioexit  ( ierr(12) )
#endif
 
  IF ( use_output_servers() ) THEN
      CALL wrf_quilt_ioexit( ierr(11) )
  ENDIF
  minerr = MINVAL(ierr)
  maxerr = MAXVAL(ierr)
  IF ( minerr < 0 ) THEN
    Status = minerr
  ELSE IF ( maxerr > 0 ) THEN
    Status = maxerr
  ELSE
    Status = 0
  ENDIF
END SUBROUTINE wrf_ioexit

!--- open_for_write_begin

SUBROUTINE wrf_open_for_write_begin( FileName , grid, SysDepInfo, &
                                     DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Begin data definition ("training") phase for writing to WRF dataset 
! FileName.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_domain
#ifdef DM_PARALLEL
  USE module_dm, ONLY :  ntasks_x, mytask_x, local_communicator_x
#endif
  IMPLICIT NONE
#include "wrf_io_flags.h"
  CHARACTER*(*) :: FileName
  type(domain)                :: grid
  CHARACTER*(*), INTENT(INOUT):: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 !Local 
  CHARACTER*128               :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files
  LOGICAL, EXTERNAL :: use_output_servers_for
  CHARACTER*512     :: LocFilename   ! for appending the process ID if necessary
  INTEGER           :: myproc
  CHARACTER*512     :: mess
  CHARACTER*1028    :: tstr, t1
  INTEGER           :: i,j
  INTEGER           :: Comm_compute , Comm_io
  LOGICAL ncd_nofill

  WRITE(mess,*) 'module_io.F: in wrf_open_for_write_begin, FileName = ',TRIM(FileName)
  CALL wrf_debug( 100, mess )

  Comm_compute = grid%communicator
  Comm_io = grid%iocommunicator

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  CALL nl_get_ncd_nofill( 1 , ncd_nofill )

  io_form = io_form_for_dataset( DataSet )

  Status = 0
  Hndl = -1
  IF ( .not. use_output_servers_for(io_form) ) THEN
    SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          IF ( ncd_nofill ) THEN
            CALL ext_ncd_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo // ",NOFILL=.TRUE.", &
                                                Hndl , Status )
          ELSE
            CALL ext_ncd_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                                Hndl , Status )
          ENDIF
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
#ifdef PHDF5
      CASE (IO_PHDF5  )
        CALL ext_phdf5_open_for_write_begin( FileName, Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl, Status)
#endif
#ifdef PNETCDF
      CASE (IO_PNETCDF  )
        WRITE(tstr,"(A,',NTASKS_X=',i10,',MYTASK_X=',i10,',LOCAL_COMMUNICATOR_X=',i10)") &
              TRIM(SysDepInfo),ntasks_x,mytask_x,local_communicator_x
        j=1
        t1 = " "
        DO i=1,len(TRIM(tstr))
          IF ( tstr(i:i) .NE. ' ' ) THEN
            t1(j:j) = tstr(i:i)
            j = j + 1
          ENDIF
        ENDDO
        tstr = t1
        CALL ext_pnc_open_for_write_begin( FileName, Comm_compute, Comm_io, tstr, &
                                            Hndl, Status)
#endif
#ifdef PIO
      CASE ( IO_PIO )
        WRITE(tstr,"(A,',NTASKS_X=',i10,',MYTASK_X=',i10,',LOCAL_COMMUNICATOR_X=',i10)") &
              TRIM(SysDepInfo),ntasks_x,mytask_x,local_communicator_x
        j=1
        t1 = " "
        DO i=1,len(TRIM(tstr))
          IF ( tstr(i:i) .NE. ' ' ) THEN
            t1(j:j) = tstr(i:i)
            j = j + 1
          ENDIF
        ENDDO
        tstr = t1
        CALL ext_pio_open_for_write_begin( FileName, grid, tstr, Hndl, Status)
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_yyy_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_gr1_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_gr2_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
#ifdef MCELIO
      CASE ( IO_MCEL )
        IF ( wrf_dm_on_monitor() ) THEN
          tstr = TRIM(SysDepInfo) // ',' // 'LAT_R=XLAT,LON_R=XLONG,LANDMASK_I=LU_MASK'
          CALL ext_mcel_open_for_write_begin ( FileName , Comm_compute, Comm_io, tstr, &
                                               Hndl , Status )
        ENDIF
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ESMFIO
      CASE ( IO_ESMF )
        CALL ext_esmf_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                             Hndl , Status )
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
        IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
            LocFilename = FileName
          ENDIF
          CALL ext_int_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
      CASE DEFAULT
        IF ( io_form .NE. 0 ) THEN
          WRITE(mess,*)'Tried to open ',FileName,' writing: no valid io_form (',io_form,')'
          CALL wrf_debug(1, mess)
          Status = WRF_FILE_NOT_OPENED
        ENDIF
    END SELECT
  ELSE ! use_output_servers_for(io_form)
    IF ( io_form .GT. 0 ) THEN
      IF ( ncd_nofill ) THEN
        CALL wrf_quilt_open_for_write_begin ( FileName , grid%id, Comm_compute, Comm_io, TRIM(SysDepInfo) // ",NOFILL=.TRUE.", &
                                              Hndl , io_form, Status )
      ELSE
        CALL wrf_quilt_open_for_write_begin ( FileName , grid%id, Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , io_form, Status )
      ENDIF
    ENDIF
  ENDIF
  CALL add_new_handle( Hndl, io_form, .TRUE., DataHandle )
END SUBROUTINE wrf_open_for_write_begin

!--- open_for_write_commit

SUBROUTINE wrf_open_for_write_commit( DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! This routine switches an internal flag to enable output for the data set 
! referenced by DataHandle. The call to wrf_open_for_write_commit() must be 
! paired with a call to wrf_open_for_write_begin().
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER (128)             :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
#include "wrf_io_flags.h"

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_write_commit' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL set_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_ncd_open_for_write_commit ( Hndl , Status )
          ENDIF
          IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          IF ( wrf_dm_on_monitor() ) THEN
            CALL ext_mcel_open_for_write_commit ( Hndl , Status )
          ENDIF
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ESMFIO
        CASE ( IO_ESMF )
          CALL ext_esmf_open_for_write_commit ( Hndl , Status )
#endif
#ifdef PHDF5
      CASE ( IO_PHDF5  )
        CALL ext_phdf5_open_for_write_commit ( Hndl , Status )
#endif
#ifdef PNETCDF
      CASE ( IO_PNETCDF  )
        CALL ext_pnc_open_for_write_commit ( Hndl , Status )
#endif
#ifdef PIO
      CASE ( IO_PIO )
        CALL ext_pio_open_for_write_commit ( Hndl , Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_write_commit ( Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
         IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_yyy_open_for_write_commit ( Hndl , Status )
         ENDIF
         IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_write_commit ( Hndl , Status )
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )
         IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_gr1_open_for_write_commit ( Hndl , Status )
         ENDIF
         IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )
         IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_gr2_open_for_write_commit ( Hndl , Status )
         ENDIF
         IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
        CALL ext_int_open_for_write_commit ( Hndl , Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_open_for_write_commit ( Hndl , Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_open_for_write_commit

!--- open_for_read_begin

SUBROUTINE wrf_open_for_read_begin( FileName , grid, SysDepInfo, &
                                     DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Begin data definition ("training") phase for reading from WRF dataset 
! FileName.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_domain
  IMPLICIT NONE
#include "wrf_io_flags.h"
  CHARACTER*(*) :: FileName
  TYPE (domain) :: grid
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  
  CHARACTER*128               :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: also_for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for

  CHARACTER*128     :: LocFilename   ! for appending the process ID if necessary
  INTEGER     myproc
  CHARACTER*128     :: mess, fhand
  CHARACTER*1028    :: tstr
  INTEGER           :: Comm_compute , Comm_io

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_read_begin' )

  Comm_compute = grid%communicator
  Comm_io = grid%iocommunicator

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  io_form = io_form_for_dataset( DataSet )

  Status = 0
  Hndl = -1
  also_for_out = .FALSE.
!  IF ( .NOT. use_output_servers_for(io_form) ) THEN
    SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
      CASE ( IO_NETCDF   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_ncd_open_for_read_begin ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                       Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        ENDIF
#endif
#ifdef PNETCDF
      CASE ( IO_PNETCDF   )
        CALL ext_pnc_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef PIO
      CASE ( IO_PIO )
        CALL ext_pio_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
        CALL ext_yyy_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef MCELIO
      CASE ( IO_MCEL )
        also_for_out = .TRUE.
        IF ( wrf_dm_on_monitor() ) THEN
          
        WRITE(fhand,'(a,i0)')"filter_",filtno
        filtno = filtno + 1
tstr = TRIM(SysDepInfo) // ',' // 'READ_MODE=UPDATE,LAT_R=XLAT,LON_R=XLONG,LANDMASK_I=LU_MASK,FILTER_HANDLE=' // TRIM(fhand)
          CALL ext_mcel_open_for_read_begin ( FileName , Comm_compute, Comm_io, tstr, &
                                               Hndl , Status )
        ENDIF
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ESMFIO
      CASE ( IO_ESMF )
        also_for_out = .TRUE.
        CALL ext_esmf_open_for_read_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_gr1_open_for_read_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
               Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        ENDIF
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )
        IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
          IF ( multi_files(io_form) ) THEN
              CALL wrf_get_myproc ( myproc )
              CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
          ELSE
              LocFilename = FileName
          ENDIF
          CALL ext_gr2_open_for_read_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
               Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        ENDIF
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
#endif
      CASE DEFAULT
        IF ( io_form .NE. 0 ) THEN
          WRITE(mess,*)'Tried to open ',FileName,' reading: no valid io_form (',io_form,')'
          CALL wrf_message(mess)
        ENDIF
        Status = WRF_FILE_NOT_OPENED
    END SELECT
!  ELSE
!    Status = 0
!  ENDIF
  CALL add_new_handle( Hndl, io_form, also_for_out, DataHandle )
END SUBROUTINE wrf_open_for_read_begin

!--- open_for_read_commit

SUBROUTINE wrf_open_for_read_commit( DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! End "training" phase for WRF dataset FileName.  The call to 
! wrf_open_for_read_commit() must be paired with a call to 
! wrf_open_for_read_begin().
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER (128)             :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
#include "wrf_io_flags.h"

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_read_commit' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL set_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) THEN
            CALL ext_ncd_open_for_read_commit ( Hndl , Status )
          ENDIF
          IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          IF ( wrf_dm_on_monitor() ) THEN
            CALL ext_mcel_open_for_read_commit ( Hndl , Status )
          ENDIF
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ESMFIO
        CASE ( IO_ESMF )
          CALL ext_esmf_open_for_read_commit ( Hndl , Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF )
          CALL ext_pnc_open_for_read_commit ( Hndl , Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_open_for_read_commit ( Hndl , Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_read_commit ( Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
        CALL ext_yyy_open_for_read_commit ( Hndl , Status )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_read_commit ( Hndl , Status )
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )
        CALL ext_gr1_open_for_read_commit ( Hndl , Status )
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )
        CALL ext_gr2_open_for_read_commit ( Hndl , Status )
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_FILE_NOT_OPENED
  ENDIF
  RETURN
END SUBROUTINE wrf_open_for_read_commit

!--- open_for_read 

SUBROUTINE wrf_open_for_read ( FileName , grid, SysDepInfo, &
                               DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Opens a WRF dataset for reading.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_domain
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  TYPE (domain) :: grid
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CHARACTER (128)             :: DataSet, LocFileName
  INTEGER                     :: io_form, myproc
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: Comm_compute, Comm_io

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_read' )

  Comm_compute = grid%communicator
  Comm_io = grid%iocommunicator

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )

  io_form = io_form_for_dataset( DataSet )

  Hndl = -1
  Status = 0
  SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
    CASE ( IO_NETCDF   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_ncd_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
      ENDIF
#endif
#ifdef PNETCDF
    CASE ( IO_PNETCDF  )
      CALL ext_pnc_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef PIO
    CASE ( IO_PIO )
      CALL ext_pio_open_for_read ( FileName , grid, SysDepInfo, Hndl , Status )
#endif
#ifdef PHDF5
    CASE ( IO_PHDF5  )
      CALL ext_phdf5_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef XXX
    CASE ( IO_XXX   )
      CALL ext_xxx_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef YYY
    CASE ( IO_YYY   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_yyy_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
      ENDIF
#endif
#ifdef ZZZ
    CASE ( IO_ZZZ   )
      CALL ext_zzz_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef GRIB1
    CASE ( IO_GRIB1   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_gr1_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
      ENDIF
#endif
#ifdef GRIB2
    CASE ( IO_GRIB2   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF

        CALL ext_gr2_open_for_read ( LocFilename , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
      ENDIF
#endif
#ifdef INTIO
    CASE ( IO_INTIO   )
      IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
        IF ( multi_files(io_form) ) THEN
            CALL wrf_get_myproc ( myproc )
            CALL append_to_filename ( LocFilename , FileName , myproc, 4 )
        ELSE
            LocFilename = FileName
        ENDIF
        CALL ext_int_open_for_read ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                     Hndl , Status )
      ENDIF
      IF ( .NOT. multi_files(io_form) ) THEN
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
      ENDIF
#endif
    CASE DEFAULT
        Status = 0
  END SELECT
  CALL add_new_handle( Hndl, io_form, .FALSE., DataHandle )
  RETURN  
END SUBROUTINE wrf_open_for_read

!--- inquire_opened

SUBROUTINE wrf_inquire_opened ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Inquire if the dataset referenced by DataHandle is open.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"

  INTEGER io_form
  INTEGER            :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_inquire_opened' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF (wrf_dm_on_monitor()) CALL ext_ncd_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef PHDF5
      CASE ( IO_PHDF5   )
          CALL ext_phdf5_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef PNETCDF
      CASE ( IO_PNETCDF   )
          CALL ext_pnc_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef PIO
      CASE ( IO_PIO )
          CALL ext_pio_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
          CALL ext_xxx_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
          IF (wrf_dm_on_monitor()) CALL ext_yyy_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
          CALL ext_zzz_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )
          IF (wrf_dm_on_monitor()) CALL ext_gr1_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )
          IF (wrf_dm_on_monitor()) CALL ext_gr2_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
          IF (wrf_dm_on_monitor()) CALL ext_int_inquire_opened ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
        CASE DEFAULT
          FileStatus = WRF_FILE_NOT_OPENED
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_inquire_opened ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileStatus = WRF_FILE_NOT_OPENED
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_opened

!--- inquire_filename


SUBROUTINE wrf_inquire_filename ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Returns the Filename and FileStatus associated with DataHandle.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  LOGICAL                     :: for_out

  INTEGER                     :: io_form
  INTEGER                     :: str_length , str_count
  INTEGER                     :: Hndl

  CALL wrf_debug( 300, 'module_io.F: in wrf_inquire_filename' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )

  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          str_length = LEN ( FileName )
          DO str_count = 1 , str_length
            FileName(str_count:str_count) = ' '
          END DO
          IF (wrf_dm_on_monitor()) CALL ext_ncd_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF   )
          CALL ext_pnc_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF (wrf_dm_on_monitor()) CALL ext_yyy_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
            CALL ext_zzz_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF (wrf_dm_on_monitor()) CALL ext_gr1_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF (wrf_dm_on_monitor()) CALL ext_gr2_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF (wrf_dm_on_monitor()) CALL ext_int_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_inquire_filename ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileName = ""
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_filename

!--- sync

SUBROUTINE wrf_iosync ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Synchronize the disk copy of a dataset with memory buffers.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  LOGICAL                     :: for_out

  INTEGER                     :: io_form
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_iosync' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_ncd_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_iosync( Hndl, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_yyy_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_gr1_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_gr2_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_iosync( Hndl, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_int_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_iosync( Hndl, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_iosync

!--- close

SUBROUTINE wrf_ioclose ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Close the dataset referenced by DataHandle.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: io_form
  LOGICAL                     :: for_out
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioclose' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL free_handle( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5  )
          CALL ext_phdf5_ioclose( Hndl, Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF  )
          CALL ext_pnc_ioclose( Hndl, Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_ioclose( Hndl, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_ioclose( Hndl, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_yyy_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_ioclose( Hndl, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          CALL ext_mcel_ioclose( Hndl, Status )
#endif
#ifdef ESMFIO
        CASE ( IO_ESMF )
          CALL ext_esmf_ioclose( Hndl, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_ioclose( Hndl, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_ioclose

!--- get_next_time (not defined for IntIO )

SUBROUTINE wrf_get_next_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Returns the next time stamp.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: io_form, len_of_str
  LOGICAL                     :: for_out
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_next_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF   )
          IF ( multi_files(io_form) ) THEN
            CALL ext_pnc_get_next_time( Hndl, DateStr, Status )
          ELSE
            IF ( wrf_dm_on_monitor() ) THEN
              CALL ext_pnc_get_next_time( Hndl, DateStr, Status )
            ENDIF

            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PIO
        CASE ( IO_PIO )
          IF ( multi_files(io_form) ) THEN
            CALL ext_pio_get_next_time( Hndl, DateStr, Status )
          ELSE
            IF ( wrf_dm_on_monitor() ) THEN
              CALL ext_pio_get_next_time( Hndl, DateStr, Status )
            ENDIF

            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_yyy_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_get_next_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_next_time

!--- get_previous_time (not defined for IntIO )

SUBROUTINE wrf_get_previous_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Returns the previous time stamp.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: io_form, len_of_str
  LOGICAL                     :: for_out
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_previous_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF   )
          CALL ext_pnc_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_yyy_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
         ENDIF
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
         ENDIF
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
         ENDIF
#endif
#ifdef INTIO
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_get_previous_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_previous_time

!--- set_time

SUBROUTINE wrf_set_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Sets the time stamp.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: io_form
  LOGICAL                     :: for_out
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_set_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5  )
          CALL ext_phdf5_set_time( Hndl, DateStr, Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF  )
          CALL ext_pnc_set_time( Hndl, DateStr, Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_set_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_set_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_yyy_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_set_time( Hndl, DateStr, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_set_time( Hndl, DateStr, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_set_time

!--- get_next_var  (not defined for IntIO)

SUBROUTINE wrf_get_next_var ( DataHandle, VarName, Status )
!<DESCRIPTION>
!<PRE>
! On reading, this routine returns the name of the next variable in the 
! current time frame.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status
#include "wrf_status_codes.h"

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: io_form
  LOGICAL                     :: for_out
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_next_var' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_next_var( Hndl, VarName, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_yyy_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_next_var( Hndl, VarName, Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr1_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_gr2_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_get_next_var( Hndl, VarName, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_next_var


! wrf_get_var_info  (not implemented for IntIO)

SUBROUTINE wrf_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
!<DESCRIPTION>
!<PRE>
! This routine applies only to a dataset that is open for read.  It returns 
! information about a variable.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  IMPLICIT NONE
  INTEGER               ,INTENT(IN)     :: DataHandle
  CHARACTER*(*)         ,INTENT(IN)     :: VarName
  INTEGER               ,INTENT(OUT)    :: NDim
  CHARACTER*(*)         ,INTENT(OUT)    :: MemoryOrder
  CHARACTER*(*)         ,INTENT(OUT)    :: Stagger
  INTEGER ,dimension(*) ,INTENT(OUT)    :: DomainStart, DomainEnd
  INTEGER               ,INTENT(OUT)    :: Status
#include "wrf_status_codes.h"
  INTEGER                     :: io_form
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers_for
  INTEGER                     :: Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_var_info' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF (( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) .AND. .NOT. (for_out .AND. use_output_servers_for(io_form)) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          CALL ext_ncd_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5)
          CALL ext_phdf5_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF)
          CALL ext_pnc_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL ext_pio_get_var_info ( Hndl , VarName , NDim ,                  &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef XXX
        CASE ( IO_XXX )
          CALL ext_xxx_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef YYY
        CASE ( IO_YYY )
          CALL ext_yyy_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1 )
          CALL ext_gr1_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2 )
          CALL ext_gr2_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers_for(io_form) ) THEN
      CALL wrf_quilt_get_var_info ( Hndl , VarName , NDim ,            &
                                    MemoryOrder , Stagger ,                  &
                                    DomainStart , DomainEnd ,                &
                                    Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN

END SUBROUTINE wrf_get_var_info



!---------------------------------------------------------------------------------


SUBROUTINE init_io_handles()
!<DESCRIPTION>
!<PRE>
! Initialize all I/O handles.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    DO i = 1, MAX_WRF_IO_HANDLE
      wrf_io_handles(i) = -999319
    ENDDO
    is_inited = .TRUE.
  ENDIF
  RETURN
END SUBROUTINE init_io_handles

SUBROUTINE add_new_handle( Hndl, Hopened, for_out, DataHandle )
!<DESCRIPTION>
!<PRE>
! Stash the package-specific I/O handle (Hndl) and return a WRF I/O handle 
! (DataHandle).  
! File format ID is passed in via Hopened.  
! for_out will be .TRUE. if this routine was called from an 
! open-for-read/write-begin operation and .FALSE. otherwise.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN)     :: Hndl
  INTEGER, INTENT(IN)     :: Hopened
  LOGICAL, INTENT(IN)     :: for_out
  INTEGER, INTENT(OUT)    :: DataHandle
  INTEGER i
  INTEGER, EXTERNAL       :: use_package
  LOGICAL, EXTERNAL       :: multi_files
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'add_new_handle: not initialized' )
  ENDIF
  IF ( multi_files( Hopened ) ) THEN
    SELECT CASE ( use_package( Hopened ) )
#ifdef PHDF5
      CASE ( IO_PHDF5  )
        CALL wrf_error_fatal( 'add_new_handle:  multiple output files not supported for PHDF5' )
#endif
#ifdef PNETCDF
      CASE ( IO_PNETCDF  )
        CALL wrf_error_fatal( 'add_new_handle:  multiple output files not supported for PNETCDF' )
#endif
#ifdef PIO
      CASE ( IO_PIO )
        CALL wrf_error_fatal( 'add_new_handle:  multiple output files not supported for PIO' )
#endif
#ifdef MCELIO
      CASE ( IO_MCEL   )
        CALL wrf_error_fatal( 'add_new_handle:  multiple output files not supported for MCEL' )
#endif
#ifdef ESMFIO
      CASE ( IO_ESMF )
        CALL wrf_error_fatal( 'add_new_handle:  multiple output files not supported for ESMF' )
#endif
    END SELECT
  ENDIF
  DataHandle = -1
  DO i = 1, MAX_WRF_IO_HANDLE
    IF ( wrf_io_handles(i) .EQ. -999319 ) THEN
      DataHandle = i 
      wrf_io_handles(i) = Hndl
      how_opened(i)     = Hopened
      for_output(DataHandle) = for_out
      first_operation(DataHandle) = .TRUE.
      EXIT
    ENDIF
  ENDDO
  IF ( DataHandle .EQ. -1 ) THEN
    CALL wrf_error_fatal( 'add_new_handle: no handles left' )
  ENDIF
  RETURN
END SUBROUTINE add_new_handle

SUBROUTINE get_handle ( Hndl, Hopened, for_out, DataHandle )
!<DESCRIPTION>
!<PRE>
! Return the package-specific handle (Hndl) from a WRF handle 
! (DataHandle).  
! Return file format ID via Hopened.  
! Also, for_out will be set to .TRUE. if the file was opened 
! with an open-for-read/write-begin operation and .FALSE. 
! otherwise.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(OUT)     :: Hndl
  INTEGER, INTENT(OUT)     :: Hopened
  LOGICAL, INTENT(OUT)     :: for_out
  INTEGER, INTENT(IN)    :: DataHandle
  CHARACTER*128 mess
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    Hndl = wrf_io_handles(DataHandle)
    Hopened = how_opened(DataHandle)
    for_out = for_output(DataHandle)
  ELSE
    Hndl = -1
  ENDIF
  RETURN
END SUBROUTINE get_handle

SUBROUTINE set_first_operation( DataHandle )
!<DESCRIPTION>
!<PRE>
! Sets internal flag to indicate that the first read or write has not yet 
! happened for the dataset referenced by DataHandle.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    first_operation(DataHandle) = .TRUE.
  ENDIF
  RETURN
END SUBROUTINE set_first_operation

SUBROUTINE reset_first_operation( DataHandle )
!<DESCRIPTION>
!<PRE>
! Resets internal flag to indicate that the first read or write has already 
! happened for the dataset referenced by DataHandle.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    first_operation(DataHandle) = .FALSE.
  ENDIF
  RETURN
END SUBROUTINE reset_first_operation

LOGICAL FUNCTION is_first_operation( DataHandle )
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. the first read or write has not yet happened for the dataset 
! referenced by DataHandle.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'module_io.F: get_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    is_first_operation = first_operation(DataHandle)
  ENDIF
  RETURN
END FUNCTION is_first_operation

SUBROUTINE free_handle ( DataHandle )
!<DESCRIPTION>
!<PRE>
! Trash a handle and return to "unused" pool.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'free_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. MAX_WRF_IO_HANDLE ) THEN
    wrf_io_handles(DataHandle) = -999319
  ENDIF
  RETURN
END SUBROUTINE free_handle

!--------------------------------------------------------------

SUBROUTINE init_module_io
!<DESCRIPTION>
!<PRE>
! Initialize this module.  Must be called before any other operations are 
! attempted.  
!</PRE>
!</DESCRIPTION>
  CALL init_io_handles
END SUBROUTINE init_module_io

SUBROUTINE are_bdys_distributed( res )
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: res
  res = bdy_dist_flag
END SUBROUTINE are_bdys_distributed

SUBROUTINE bdys_not_distributed
  IMPLICIT NONE
  bdy_dist_flag = .FALSE.
END SUBROUTINE bdys_not_distributed

SUBROUTINE bdys_are_distributed
  IMPLICIT NONE
  bdy_dist_flag = .TRUE.
END SUBROUTINE bdys_are_distributed

LOGICAL FUNCTION on_stream ( mask , switch )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: mask(*), switch
  INTEGER             :: result
! get_mask is a C routine defined in frame/pack_utils.c
! switch is decremented from its fortran value so it is zero based
  CALL get_mask( mask, switch-1, result )
  on_stream = ( result .NE. 0 )
END FUNCTION on_stream

END MODULE module_io


!<DESCRIPTION>
!<PRE>
! Remaining routines in this file are defined outside of the module to 
! defeat arg/param type checking.  
!</PRE>
!</DESCRIPTION>
SUBROUTINE wrf_read_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            grid ,                                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
!<DESCRIPTION>
!<PRE>
! Read the variable named VarName from the dataset pointed to by DataHandle.
! This routine is a wrapper that ensures uniform treatment of logicals across 
! platforms by reading as integer and then converting to logical.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_configure
  USE module_domain
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL ,       INTENT(INOUT) :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  TYPE(domain)                                 :: grid
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER, ALLOCATABLE        :: ICAST(:)
  LOGICAL perturb_input
  IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    ALLOCATE(ICAST((MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)))

    CALL wrf_read_field1 ( DataHandle , DateStr , VarName , ICAST , WRF_INTEGER ,       &
                           grid ,                                                       &
                           DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                           DomainStart , DomainEnd ,                                    &
                           MemoryStart , MemoryEnd ,                                    &
                           PatchStart , PatchEnd ,                                      &
                           Status )
    Field(1:(MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)) = ICAST == 1
    DEALLOCATE(ICAST)
  ELSE
    CALL wrf_read_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                           grid ,                                                       &
                           DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                           DomainStart , DomainEnd ,                                    &
                           MemoryStart , MemoryEnd ,                                    &
                           PatchStart , PatchEnd ,                                      &
                           Status )
    CALL nl_get_perturb_input( 1, perturb_input )
    IF ( perturb_input .AND. FieldType .EQ. WRF_FLOAT .AND. TRIM(MemoryOrder) .EQ. 'XZY' ) THEN
       CALL perturb_real ( Field, DomainStart, DomainEnd,        &
                                  MemoryStart, MemoryEnd,        &
                                  PatchStart, PatchEnd )
    ENDIF
  ENDIF
END SUBROUTINE wrf_read_field

SUBROUTINE wrf_read_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,        &
                            grid ,                                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
!<DESCRIPTION>
!<PRE>
! Read the variable named VarName from the dataset pointed to by DataHandle.
! Calls ext_pkg_read_field() via call_pkg_and_dist().  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_configure
  USE module_io
  USE module_domain
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  TYPE(domain)                                 :: grid
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
#include "wrf_status_codes.h"
  INTEGER                     :: io_form
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers, use_input_servers, use_output_servers_for
#ifdef NETCDF
  EXTERNAL     ext_ncd_read_field
#endif
#ifdef PNETCDF
  EXTERNAL     ext_pnc_read_field
#endif
#ifdef PIO
  EXTERNAL     ext_pio_read_field
#endif
#ifdef MCELIO
  EXTERNAL     ext_mcel_read_field
#endif
#ifdef ESMFIO
  EXTERNAL     ext_esmf_read_field
#endif
#ifdef INTIO
  EXTERNAL     ext_int_read_field
#endif
#ifdef XXX
  EXTERNAL ext_xxx_read_field
#endif
#ifdef YYY
  EXTERNAL ext_yyy_read_field
#endif
#ifdef GRIB1
  EXTERNAL ext_gr1_read_field
#endif
#ifdef GRIB2
  EXTERNAL ext_gr2_read_field
#endif

  INTEGER                     :: Hndl
  INTEGER                     :: Comm 
  INTEGER                     :: IOComm 

  Comm = grid%communicator
  IOComm = grid%iocommunicator

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_read_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL reset_first_operation( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( .NOT. io_form .GT. 0 ) THEN
      Status = 0 
    ELSE IF ( .NOT. use_input_servers() ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )

          CALL call_pkg_and_dist   ( ext_ncd_read_field, multi_files(io_form), .false. ,        &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )

#endif
#ifdef PHDF5
        CASE ( IO_PHDF5)
          CALL ext_phdf5_read_field   (                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF)
          CALL ext_pnc_read_field   (                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef PIO
        CASE ( IO_PIO)
          CALL ext_pio_read_field(Hndl , DateStr , VarName , Field , FieldType , grid , &
                                  DomainDesc , MemoryOrder , Stagger , DimNames ,       &
                                  DomainStart , DomainEnd ,                             &
                                  MemoryStart , MemoryEnd ,                             &
                                  PatchStart , PatchEnd ,                               &
                                  Status)
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          CALL call_pkg_and_dist   ( ext_mcel_read_field, multi_files(io_form), .true. ,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef ESMFIO
        CASE ( IO_ESMF )
          CALL ext_esmf_read_field( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                    DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                    DomainStart , DomainEnd ,                                    &
                                    MemoryStart , MemoryEnd ,                                    &
                                    PatchStart , PatchEnd ,                                      &
                                    Status )
#endif
#ifdef XXX
        CASE ( IO_XXX )
          CALL call_pkg_and_dist   ( ext_xxx_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef YYY
        CASE ( IO_YYY )
          CALL call_pkg_and_dist   ( ext_yyy_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO )
          CALL call_pkg_and_dist   ( ext_int_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1 )
          CALL call_pkg_and_dist   ( ext_gr1_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2 )
          CALL call_pkg_and_dist   ( ext_gr2_read_field, multi_files(io_form), .false.,         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE
      CALL wrf_error_fatal('module_io.F: wrf_read_field: input_servers not implemented yet')
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_read_field1

SUBROUTINE wrf_write_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                             grid ,                                                       &
                             DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
!<DESCRIPTION>
!<PRE>
! Write the variable named VarName to the dataset pointed to by DataHandle.
! This routine is a wrapper that ensures uniform treatment of logicals across 
! platforms by converting to integer before writing.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_configure
  USE module_domain
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  TYPE(domain)                                 :: grid
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)         ,INTENT(IN)    :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER, ALLOCATABLE        :: ICAST(:)
  IF ( FieldType .EQ. WRF_LOGICAL ) THEN
      ALLOCATE(ICAST((MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)))
      ICAST = 0
      WHERE ( Field(1:(MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)) )
        ICAST = 1
      END WHERE
    CALL wrf_write_field1 ( DataHandle , DateStr , VarName , ICAST , WRF_INTEGER ,         &
                            grid ,                                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
      DEALLOCATE(ICAST)
  ELSE
    CALL wrf_write_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            grid ,                                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  ENDIF
END SUBROUTINE wrf_write_field

SUBROUTINE wrf_write_field1 ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                             grid ,                                                        &
                             DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
!<DESCRIPTION>
!<PRE>
! Write the variable named VarName to the dataset pointed to by DataHandle.
! Calls ext_pkg_write_field() via collect_fld_and_call_pkg().  
!</PRE>
!</DESCRIPTION>

  USE module_state_description
  USE module_configure
  USE module_domain
  USE module_io
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  TYPE(domain)                                 :: grid
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)         ,INTENT(IN)    :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
#include "wrf_status_codes.h"
  INTEGER, DIMENSION(3) :: starts, ends
  INTEGER               :: io_form
  CHARACTER*3 MemOrd
  LOGICAL                     :: for_out, okay_to_call
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers, use_output_servers_for
#ifdef NETCDF
  EXTERNAL     ext_ncd_write_field
#endif
#ifdef PNETCDF
  EXTERNAL     ext_pnc_write_field
#endif
#ifdef PIO
  EXTERNAL     ext_pio_write_field
#endif
#ifdef MCELIO
  EXTERNAL     ext_mcel_write_field
#endif
#ifdef ESMFIO
  EXTERNAL     ext_esmf_write_field
#endif
#ifdef INTIO
  EXTERNAL     ext_int_write_field
#endif
#ifdef XXX
  EXTERNAL ext_xxx_write_field
#endif
#ifdef YYY
  EXTERNAL ext_yyy_write_field
#endif
#ifdef GRIB1
  EXTERNAL ext_gr1_write_field
#endif
#ifdef GRIB2
  EXTERNAL ext_gr2_write_field
#endif

  INTEGER                     :: Hndl
  INTEGER                     :: Comm 
  INTEGER                     :: IOComm 

  Comm = grid%communicator
  IOComm = grid%iocommunicator

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_write_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  CALL reset_first_operation ( DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. use_output_servers_for(io_form) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          CALL collect_fld_and_call_pkg ( ext_ncd_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          CALL collect_fld_and_call_pkg ( ext_mcel_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef ESMFIO
        CASE ( IO_ESMF )
          CALL ext_esmf_write_field( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5 )
          CALL ext_phdf5_write_field(                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef PNETCDF
        CASE ( IO_PNETCDF )
          CALL lower_case( MemoryOrder, MemOrd )
          okay_to_call = .TRUE.
          IF ((TRIM(MemOrd).EQ.'xsz' .OR. TRIM(MemOrd).EQ.'xs').AND. .NOT. bdy_mask(P_XSB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'xez' .OR. TRIM(MemOrd).EQ.'xe').AND. .NOT. bdy_mask(P_XEB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'ysz' .OR. TRIM(MemOrd).EQ.'ys').AND. .NOT. bdy_mask(P_YSB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'yez' .OR. TRIM(MemOrd).EQ.'ye').AND. .NOT. bdy_mask(P_YEB)) okay_to_call = .FALSE.
          IF ( okay_to_call ) THEN
             starts(1:3) = PatchStart(1:3) ; ends(1:3) = PatchEnd(1:3)
          ELSE
             starts(1:3) = PatchStart(1:3) ; ends(1:3) = PatchStart(1:3)-1
          ENDIF

               CALL ext_pnc_write_field(                  &
                                       Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                       DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                       DomainStart , DomainEnd ,                                    &
                                       MemoryStart , MemoryEnd ,                                    &
                                       starts , ends ,                                      &
                                       Status )
#endif
#ifdef PIO
        CASE ( IO_PIO )
          CALL lower_case( MemoryOrder, MemOrd )
          okay_to_call = .TRUE.
          IF ((TRIM(MemOrd).EQ.'xsz' .OR. TRIM(MemOrd).EQ.'xs').AND. .NOT. bdy_mask(P_XSB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'xez' .OR. TRIM(MemOrd).EQ.'xe').AND. .NOT. bdy_mask(P_XEB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'ysz' .OR. TRIM(MemOrd).EQ.'ys').AND. .NOT. bdy_mask(P_YSB)) okay_to_call = .FALSE.
          IF ((TRIM(MemOrd).EQ.'yez' .OR. TRIM(MemOrd).EQ.'ye').AND. .NOT. bdy_mask(P_YEB)) okay_to_call = .FALSE.
          IF ( okay_to_call ) THEN
             starts(1:3) = PatchStart(1:3) ; ends(1:3) = PatchEnd(1:3)
          ELSE
             starts(1:3) = PatchStart(1:3) ; ends(1:3) = PatchStart(1:3)-1
          ENDIF

          CALL ext_pio_write_field( Hndl , DateStr , VarName , Field , FieldType , grid , &
                                    DomainDesc , MemoryOrder , Stagger , DimNames ,       &
                                    DomainStart , DomainEnd ,                             &
                                    MemoryStart , MemoryEnd ,                             &
                                    starts , ends , Status )
#endif
#ifdef XXX
        CASE ( IO_XXX )
          CALL collect_fld_and_call_pkg ( ext_xxx_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef YYY
        CASE ( IO_YYY )
          CALL collect_fld_and_call_pkg ( ext_yyy_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef GRIB1
        CASE ( IO_GRIB1 )
          CALL collect_fld_and_call_pkg ( ext_gr1_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef GRIB2
        CASE ( IO_GRIB2 )
          CALL collect_fld_and_call_pkg ( ext_gr2_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO )
          CALL collect_fld_and_call_pkg ( ext_int_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( use_output_servers_for(io_form) ) THEN
      IF ( io_form .GT. 0 ) THEN
      CALL wrf_quilt_write_field ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                   DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                   DomainStart , DomainEnd ,                                    &
                                   MemoryStart , MemoryEnd ,                                    &
                                   PatchStart , PatchEnd ,                                      &
                                   Status )
      ENDIF
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_write_field1

SUBROUTINE get_value_from_pairs ( varname , str , retval )
!<DESCRIPTION>
!<PRE>
! parse comma separated list of VARIABLE=VALUE strings and return the
! value for the matching variable if such exists, otherwise return
! the empty string
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  CHARACTER*(*) ::    varname
  CHARACTER*(*) ::    str
  CHARACTER*(*) ::    retval

  CHARACTER (256) varstr, tstr
  INTEGER i,j,n,varstrn
  LOGICAL nobreak, nobreakouter

  varstr = TRIM(varname)//"="
  varstrn = len(TRIM(varstr))
  n = len(str)
  retval = ""
  i = 1
  nobreakouter = .TRUE.
  DO WHILE ( nobreakouter )
    j = 1
    nobreak = .TRUE.
    tstr = ""
! Potential for out of bounds array ref on str(i:i) for i > n; reported by jedwards
!    DO WHILE ( nobreak )
!      IF ( str(i:i) .NE. ',' .AND. i .LE. n ) THEN
!        tstr(j:j) = str(i:i)
!      ELSE
!        nobreak = .FALSE.
!      ENDIF
!      j = j + 1
!      i = i + 1
!    ENDDO
! fix 20021112, JM
    DO WHILE ( nobreak )
      nobreak = .FALSE.
      IF ( i .LE. n ) THEN
        IF (str(i:i) .NE. ',' ) THEN
           tstr(j:j) = str(i:i)
           nobreak = .TRUE.
        ENDIF
      ENDIF
      j = j + 1
      i = i + 1
    ENDDO
    IF ( i .GT. n ) nobreakouter = .FALSE.
    IF ( varstr(1:varstrn) .EQ. tstr(1:varstrn) ) THEN
      retval(1:) = TRIM(tstr(varstrn+1:))
      nobreakouter = .FALSE.
    ENDIF
  ENDDO
  RETURN
END SUBROUTINE get_value_from_pairs

LOGICAL FUNCTION multi_files ( io_form )
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. iff io_form is a multi-file format.  A multi-file format 
! results in one file for each compute process and can be used with any 
! I/O package.  A multi-file dataset can only be read by the same number 
! of tasks that were used to write it.  This feature can be useful for 
! speeding up restarts on machines that support efficient parallel I/O.  
! Multi-file formats cannot be used with I/O quilt servers.  
!
! Note: multi-file is selected by adding 100 to the standard WRF I/O
! form.  If you add 200 instead, then single-file I/O is still done,
! but without I/O servers.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
#ifdef DM_PARALLEL
  multi_files = ( io_form >= 100 .and. io_form<200 )
#else
  multi_files = .FALSE.
#endif
END FUNCTION multi_files

INTEGER FUNCTION use_package ( io_form )
!<DESCRIPTION>
!<PRE>
! Returns the ID of the external I/O package referenced by io_form.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
  use_package = MOD( io_form, 100 )
END FUNCTION use_package


SUBROUTINE collect_fld_and_call_pkg (    fcn, donotcollect_arg,                                       &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! This routine handle cases where collection can be skipped and deals with 
! different data types for Field.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "wrf_io_flags.h"
  EXTERNAL fcn
  LOGICAL,        INTENT(IN)    :: donotcollect_arg
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  LOGICAL donotcollect
  INTEGER ndims, nproc

  CALL dim_from_memorder( MemoryOrder , ndims)
  CALL wrf_get_nproc( nproc )
  donotcollect = donotcollect_arg .OR. (nproc .EQ. 1)

  IF ( donotcollect ) THEN

    CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,                &
               DomainStart , DomainEnd ,                                      &
               MemoryStart , MemoryEnd ,                                      &
               PatchStart , PatchEnd ,                                        &
               Status )

  ELSE IF ( FieldType .EQ. WRF_DOUBLE  ) THEN

     CALL collect_double_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

     CALL collect_real_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

     CALL collect_int_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

     CALL collect_logical_and_call_pkg ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ENDIF
  RETURN
END SUBROUTINE collect_fld_and_call_pkg

SUBROUTINE collect_real_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! The sole purpose of this wrapper is to allocate a big real buffer and 
! pass it down to collect_generic_and_call_pkg() to do the actual work.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  REAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

#ifdef DEREF_KLUDGE
# define FRSTELEM (1)
#else
# define FRSTELEM
#endif
  
  CALL collect_generic_and_call_pkg (   fcn, globbuf FRSTELEM,                                    &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_real_and_call_pkg

SUBROUTINE collect_int_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! The sole purpose of this wrapper is to allocate a big integer buffer and 
! pass it down to collect_generic_and_call_pkg() to do the actual work.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  INTEGER, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf FRSTELEM ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_int_and_call_pkg

SUBROUTINE collect_double_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! The sole purpose of this wrapper is to allocate a big double precision 
! buffer and pass it down to collect_generic_and_call_pkg() to do the 
! actual work.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  DOUBLE PRECISION    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  DOUBLE PRECISION, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf FRSTELEM ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_double_and_call_pkg

SUBROUTINE collect_logical_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! The sole purpose of this wrapper is to allocate a big logical buffer 
! and pass it down to collect_generic_and_call_pkg() to do the actual work.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  LOGICAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  LOGICAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  CALL collect_generic_and_call_pkg (   fcn, globbuf FRSTELEM ,                                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN

END SUBROUTINE collect_logical_and_call_pkg


SUBROUTINE collect_generic_and_call_pkg ( fcn, globbuf,                                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The collect_*_and_call_pkg routines collect a distributed array onto one 
! processor and then call an I/O function to write the result (or in the 
! case of replicated data simply write monitor node's copy of the data)
! This routine calls the distributed memory communication routines that 
! collect the array and then calls I/O function fcn to write it to disk.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
#include "wrf_io_flags.h"
#if defined( DM_PARALLEL ) && ! defined(STUBMPI)
include "mpif.h"
#endif
  EXTERNAL fcn
  REAL , DIMENSION(*) , INTENT(INOUT) :: globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  CHARACTER*3 MemOrd
  LOGICAL, EXTERNAL :: has_char
  INTEGER ids, ide, jds, jde, kds, kde
  INTEGER ims, ime, jms, jme, kms, kme
  INTEGER ips, ipe, jps, jpe, kps, kpe
  INTEGER, ALLOCATABLE :: counts(:), displs(:)
  INTEGER nproc, communicator, mpi_bdyslice_type, ierr, my_displ
  INTEGER my_count
  INTEGER , dimension(3)                       :: dom_end_rev
  LOGICAL, EXTERNAL         :: wrf_dm_on_monitor
  INTEGER, EXTERNAL         :: wrf_dm_monitor_rank
  LOGICAL     distributed_field
  INTEGER i,j,k,idx,lx,idx2,lx2
  INTEGER collective_root

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )

  ALLOCATE( counts( nproc ) )
  ALLOCATE( displs( nproc ) )
  CALL lower_case( MemoryOrder, MemOrd )

  collective_root = wrf_dm_monitor_rank()

  dom_end_rev(1) = DomainEnd(1)
  dom_end_rev(2) = DomainEnd(2)
  dom_end_rev(3) = DomainEnd(3)

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'zxy' )
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xyz' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE (  'yxz' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'yx' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE DEFAULT
      ! do nothing; the boundary orders and others either dont care or set themselves
  END SELECT

  SELECT CASE (TRIM(MemOrd))
#ifndef STUBMPI
    CASE (  'xzy','zxy','xyz','yxz','xy','yx' )

      distributed_field = .TRUE.
      IF ( FieldType .EQ. WRF_DOUBLE ) THEN
        CALL wrf_patch_to_global_double ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
        CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
        CALL wrf_patch_to_global_logical ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ENDIF

#if defined(DM_PARALLEL) && !defined(STUBMPI)
    CASE ( 'xsz', 'xez' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  ! ns strip
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  ! levels
        ids = DomainStart(3) ; ide = DomainEnd(3) ; !  bdy_width
        dom_end_rev(1) = jde
        dom_end_rev(2) = kde
        dom_end_rev(3) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xsz' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xez' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do i = DomainStart(3),DomainEnd(3)    ! bdy_width
        do k = DomainStart(2),DomainEnd(2)    ! levels
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           IF ( FieldType .EQ. WRF_DOUBLE  ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'xs', 'xe' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        jds = DomainStart(1) ; jde = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) jde = jde+1  ! ns strip
        ids = DomainStart(2) ; ide = DomainEnd(2) ; !  bdy_width
        dom_end_rev(1) = jde
        dom_end_rev(2) = ide
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'xs' .AND. bdy_mask( P_XSB )) .OR.     &
             (MemOrd .eq. 'xe' .AND. bdy_mask( P_XEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do i = DomainStart(2),DomainEnd(2)    ! bdy_width
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(i-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(i-1)
           IF ( FieldType .EQ. WRF_DOUBLE ) THEN

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer ( Field, PatchStart(1)-MemoryStart(1)+1+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ENDIF

        enddo
      ENDIF
    CASE ( 'ysz', 'yez' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  ! ns strip
        kds = DomainStart(2) ; kde = DomainEnd(2) ; IF ( .NOT. has_char( Stagger, 'z' ) ) kde = kde+1  ! levels
        jds = DomainStart(3) ; jde = DomainEnd(3) ; !  bdy_width
        dom_end_rev(1) = ide
        dom_end_rev(2) = kde
        dom_end_rev(3) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ysz' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'yez' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(3),DomainEnd(3)    ! bdy_width
        do k = DomainStart(2),DomainEnd(2)    ! levels
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double ( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ENDIF

        enddo
        enddo
      ENDIF
    CASE ( 'ys', 'ye' )
      distributed_field = .FALSE.
      IF ( nproc .GT. 1 ) THEN
        ids = DomainStart(1) ; ide = DomainEnd(1) ; IF ( .NOT. has_char( Stagger, 'y' ) ) ide = ide+1  ! ns strip
        jds = DomainStart(2) ; jde = DomainEnd(2) ; !  bdy_width
        dom_end_rev(1) = ide
        dom_end_rev(2) = jde
        distributed_field = .TRUE.
        IF ( (MemOrd .eq. 'ys' .AND. bdy_mask( P_YSB )) .OR.     &
             (MemOrd .eq. 'ye' .AND. bdy_mask( P_YEB ))       ) THEN
          my_displ = PatchStart(1)-1
          my_count = PatchEnd(1)-PatchStart(1)+1
        ELSE
          my_displ = 0
          my_count = 0
        ENDIF
        CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
        CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )
        do j = DomainStart(2),DomainEnd(2)    ! bdy_width
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(j-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(j-1)

           IF ( FieldType .EQ. WRF_DOUBLE ) THEN 

             CALL wrf_gatherv_double( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)-MemoryStart(1)+1+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             collective_root                , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ENDIF

        enddo
      ENDIF
#endif
#endif
    CASE DEFAULT
      distributed_field = .FALSE.
  END SELECT
  IF ( wrf_dm_on_monitor() ) THEN
    IF ( distributed_field ) THEN
      CALL fcn ( Hndl , DateStr , VarName , globbuf , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 DomainStart , dom_end_rev ,                                      &  ! memory dims adjust out for unstag
                 DomainStart , DomainEnd ,                                        &
                 Status )
    ELSE
      CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 MemoryStart , MemoryEnd ,                                        &
                 PatchStart  , PatchEnd  ,                                        &
                 Status )
    ENDIF
  ENDIF
  CALL wrf_dm_bcast_bytes( Status , IWORDSIZE )
  DEALLOCATE( counts )
  DEALLOCATE( displs )
  RETURN
END SUBROUTINE collect_generic_and_call_pkg


SUBROUTINE call_pkg_and_dist (       fcn, donotdist_arg, update_arg,                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! This routine handle cases where distribution/replication can be skipped and 
! deals with different data types for Field.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "wrf_io_flags.h"
  EXTERNAL fcn
  LOGICAL,        INTENT(IN)    :: donotdist_arg, update_arg  ! update means collect old field update it and dist
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER                          :: Field(*)
  INTEGER                                      :: FieldType
  INTEGER                                      :: Comm
  INTEGER                                      :: IOComm
  INTEGER                                      :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                                :: MemoryOrder
  CHARACTER*(*)                                :: Stagger
  CHARACTER*(*) , dimension (*)                :: DimNames
  INTEGER ,dimension(*)                        :: DomainStart, DomainEnd
  INTEGER ,dimension(*)                        :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)                        :: PatchStart,  PatchEnd
  INTEGER                                      :: Status
  LOGICAL donotdist
  INTEGER ndims, nproc

  CALL dim_from_memorder( MemoryOrder , ndims)
  CALL wrf_get_nproc( nproc )
  donotdist = donotdist_arg .OR. (nproc .EQ. 1)

  IF ( donotdist ) THEN
    CALL fcn ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,                &
               DomainStart , DomainEnd ,                                      &
               MemoryStart , MemoryEnd ,                                      &
               PatchStart , PatchEnd ,                                        &
               Status )

  ELSE IF (FieldType .EQ. WRF_DOUBLE) THEN

     CALL call_pkg_and_dist_double ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF (FieldType .EQ. WRF_FLOAT) THEN

     CALL call_pkg_and_dist_real ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

     CALL call_pkg_and_dist_int ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

     CALL call_pkg_and_dist_logical ( fcn, update_arg,                            &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ENDIF
  RETURN
END SUBROUTINE call_pkg_and_dist

SUBROUTINE call_pkg_and_dist_real (  fcn, update_arg,                                             &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! The sole purpose of this wrapper is to allocate a big real buffer and
! pass it down to call_pkg_and_dist_generic() to do the actual work.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  REAL, ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER test
  CHARACTER*128 mess

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ), &
              STAT=test )
    IF ( test .NE. 0 ) THEN
      write(mess,*)"module_io.b",'allocating globbuf ',&
           (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3)
      CALL wrf_error_fatal(mess)
    ENDIF
  ELSE
    ALLOCATE( globbuf( 1 ), STAT=test )
    IF ( test .NE. 0 ) THEN
      write(mess,*)"module_io.b",'allocating globbuf ',1
      CALL wrf_error_fatal(mess)
    ENDIF
  ENDIF

  globbuf = 0.

  CALL call_pkg_and_dist_generic (   fcn, globbuf FRSTELEM , update_arg,                          &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_real


SUBROUTINE call_pkg_and_dist_double  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! The sole purpose of this wrapper is to allocate a big double precision buffer 
! and pass it down to call_pkg_and_dist_generic() to do the actual work.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  DOUBLE PRECISION   ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  DOUBLE PRECISION , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = 0

  CALL call_pkg_and_dist_generic (   fcn, globbuf FRSTELEM , update_arg ,                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_double


SUBROUTINE call_pkg_and_dist_int  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! The sole purpose of this wrapper is to allocate a big integer buffer and 
! pass it down to call_pkg_and_dist_generic() to do the actual work.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  INTEGER , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = 0

  CALL call_pkg_and_dist_generic (   fcn, globbuf FRSTELEM , update_arg ,                                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_int


SUBROUTINE call_pkg_and_dist_logical  (  fcn, update_arg ,                                            &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! The sole purpose of this wrapper is to allocate a big logical buffer and 
! pass it down to call_pkg_and_dist_generic() to do the actual work.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  logical    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  LOGICAL , ALLOCATABLE :: globbuf (:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  IF ( wrf_dm_on_monitor() ) THEN
    ALLOCATE( globbuf( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) ) )
  ELSE
    ALLOCATE( globbuf( 1 ) )
  ENDIF

  globbuf = .false.

  CALL call_pkg_and_dist_generic (   fcn, globbuf FRSTELEM , update_arg ,                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  DEALLOCATE ( globbuf )
  RETURN
END SUBROUTINE call_pkg_and_dist_logical

SUBROUTINE call_pkg_and_dist_generic (   fcn, globbuf , update_arg ,                                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )

!<DESCRIPTION>
!<PRE>
! The call_pkg_and_dist* routines call an I/O function to read a field and then 
! distribute or replicate the field across compute tasks.  
! This routine calls I/O function fcn to read the field from disk and then calls 
! the distributed memory communication routines that distribute or replicate the 
! array.  
!</PRE>
!</DESCRIPTION>
  USE module_state_description
  USE module_driver_constants
  USE module_io
  IMPLICIT NONE
#include "wrf_io_flags.h"
#if defined( DM_PARALLEL ) && ! defined(STUBMPI)
include "mpif.h"
#endif

  EXTERNAL fcn
  REAL, DIMENSION(*) ::  globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  LOGICAL ,       INTENT(IN)    :: update_arg
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL                           :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)                        :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
  CHARACTER*3 MemOrd
  LOGICAL, EXTERNAL :: has_char
  INTEGER ids, ide, jds, jde, kds, kde
  INTEGER ims, ime, jms, jme, kms, kme
  INTEGER ips, ipe, jps, jpe, kps, kpe
  INTEGER , dimension(3)                       :: dom_end_rev
  INTEGER memsize
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER, EXTERNAL :: wrf_dm_monitor_rank

  INTEGER lx, lx2, i,j,k ,idx,idx2
  INTEGER my_count, nproc, communicator, ierr, my_displ

  INTEGER, ALLOCATABLE :: counts(:), displs(:)

  LOGICAL distributed_field
  INTEGER collective_root

  CALL lower_case( MemoryOrder, MemOrd )

  collective_root = wrf_dm_monitor_rank()

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )

  ALLOCATE(displs( nproc ))
  ALLOCATE(counts( nproc ))

  dom_end_rev(1) = DomainEnd(1)
  dom_end_rev(2) = DomainEnd(2)
  dom_end_rev(3) = DomainEnd(3)

  SELECT CASE (TRIM(MemOrd))
    CASE (  'xzy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'zxy' )
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xyz' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'xy' )
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE (  'yxz' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
      IF ( .NOT. has_char( Stagger, 'z' ) ) dom_end_rev(3) = dom_end_rev(3) + 1
    CASE (  'yx' )
      IF ( .NOT. has_char( Stagger, 'y' ) ) dom_end_rev(1) = dom_end_rev(1) + 1
      IF ( .NOT. has_char( Stagger, 'x' ) ) dom_end_rev(2) = dom_end_rev(2) + 1
    CASE DEFAULT
      ! do nothing; the boundary orders and others either dont care or set themselves
  END SELECT

  data_ordering : SELECT CASE ( model_data_order )
    CASE  ( DATA_ORDER_XYZ )
       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(2); jde=dom_end_rev(2); kds=DomainStart(3); kde=dom_end_rev(3);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(2); jme=  MemoryEnd(2); kms=MemoryStart(3); kme=  MemoryEnd(3);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(2); jpe=   PatchEnd(2); kps= PatchStart(3); kpe=   PatchEnd(3);
    CASE  ( DATA_ORDER_YXZ )
       ids=DomainStart(2); ide=dom_end_rev(2); jds=DomainStart(1); jde=dom_end_rev(1); kds=DomainStart(3); kde=dom_end_rev(3);
       ims=MemoryStart(2); ime=  MemoryEnd(2); jms=MemoryStart(1); jme=  MemoryEnd(1); kms=MemoryStart(3); kme=  MemoryEnd(3);
       ips= PatchStart(2); ipe=   PatchEnd(2); jps= PatchStart(1); jpe=   PatchEnd(1); kps= PatchStart(3); kpe=   PatchEnd(3);
    CASE  ( DATA_ORDER_ZXY )
       ids=DomainStart(2); ide=dom_end_rev(2); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(1); kde=dom_end_rev(1);
       ims=MemoryStart(2); ime=  MemoryEnd(2); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(1); kme=  MemoryEnd(1);
       ips= PatchStart(2); ipe=   PatchEnd(2); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(1); kpe=   PatchEnd(1);
    CASE  ( DATA_ORDER_ZYX )
       ids=DomainStart(3); ide=dom_end_rev(3); jds=DomainStart(2); jde=dom_end_rev(2); kds=DomainStart(1); kde=dom_end_rev(1);
       ims=MemoryStart(3); ime=  MemoryEnd(3); jms=MemoryStart(2); jme=  MemoryEnd(2); kms=MemoryStart(1); kme=  MemoryEnd(1);
       ips= PatchStart(3); ipe=   PatchEnd(3); jps= PatchStart(2); jpe=   PatchEnd(2); kps= PatchStart(1); kpe=   PatchEnd(1);
    CASE  ( DATA_ORDER_XZY )
       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);
    CASE  ( DATA_ORDER_YZX )
       ids=DomainStart(3); ide=dom_end_rev(3); jds=DomainStart(1); jde=dom_end_rev(1); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(3); ime=  MemoryEnd(3); jms=MemoryStart(1); jme=  MemoryEnd(1); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(3); ipe=   PatchEnd(3); jps= PatchStart(1); jpe=   PatchEnd(1); kps= PatchStart(2); kpe=   PatchEnd(2);
  END SELECT data_ordering


  SELECT CASE (MemOrd)
#ifndef STUBMPI
    CASE ( 'xzy', 'yzx', 'xyz', 'yxz', 'zxy', 'zyx', 'xy', 'yx' )
      distributed_field = .TRUE.
    CASE ( 'xsz', 'xez', 'xs', 'xe' )
      CALL are_bdys_distributed( distributed_field )
    CASE ( 'ysz', 'yez', 'ys', 'ye' )
      CALL are_bdys_distributed( distributed_field )
#endif
    CASE DEFAULT
      ! all other memory orders are replicated
      distributed_field = .FALSE.
  END SELECT

  IF ( distributed_field ) THEN

! added 8/2004 for interfaces, like MCEL, that want the old values so they can be updated
    IF ( update_arg ) THEN
      SELECT CASE (TRIM(MemOrd))
        CASE (  'xzy','zxy','xyz','yxz','xy','yx' )
          IF ( FieldType .EQ. WRF_DOUBLE ) THEN
            CALL wrf_patch_to_global_double ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF (  FieldType .EQ. WRF_FLOAT ) THEN
            CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
            CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
            CALL wrf_patch_to_global_logical ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
               DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
               MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
               PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
          ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF

    IF ( wrf_dm_on_monitor()) THEN
      CALL fcn ( Hndl , DateStr , VarName , globbuf , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 DomainStart , dom_end_rev ,                                        &
                 DomainStart , DomainEnd ,                                          &
                 Status )
    ENDIF

    CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )

    CALL lower_case( MemoryOrder, MemOrd )

#if defined(DM_PARALLEL) && !defined(STUBMPI)
! handle boundaries separately
    IF ( TRIM(MemOrd) .EQ. 'xsz' .OR. TRIM(MemOrd) .EQ. 'xez' .OR. &
         TRIM(MemOrd) .EQ. 'xs'  .OR. TRIM(MemOrd) .EQ. 'xe'  .OR. &
         TRIM(MemOrd) .EQ. 'ysz' .OR. TRIM(MemOrd) .EQ. 'yez' .OR. &
         TRIM(MemOrd) .EQ. 'ys'  .OR. TRIM(MemOrd) .EQ. 'ye'    ) THEN

      IF ( TRIM(MemOrd) .EQ. 'xsz' .OR. TRIM(MemOrd) .EQ. 'xez' .OR. &
           TRIM(MemOrd) .EQ. 'xs'  .OR. TRIM(MemOrd) .EQ. 'xe'    ) THEN

       jds=DomainStart(1); jde=dom_end_rev(1); ids=DomainStart(3); ide=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       jms=MemoryStart(1); jme=  MemoryEnd(1); ims=MemoryStart(3); ime=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       jps= PatchStart(1); jpe=   PatchEnd(1); ips= PatchStart(3); ipe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);

        IF ( nproc .GT. 1 ) THEN

! Will assume that the i,j, and k dimensions correspond to the model_data_order specified by the registry -- 
! eg. i is (1), j is (3), and k is (2) for XZY -- and that when these are passed in for xs/xe boundary arrays (left and right
! sides of domain) the j is fully dimensioned, i is the bdy_width, and k is k. corresponding arrangement for ys/ye
! boundaries (bottom and top).  Note, however, that for the boundary arrays themselves, the innermost dimension is always
! the "full" dimension: for xs/xe, dimension 1 of the boundary arrays is j. For ys/ye, it's i. So there's a potential
! for confusion between the MODEL storage order, and which of the sd31:ed31/sd32:ed32/sd33:ed33 framework dimensions
! correspond to X/Y/Z as determined by the Registry dimespec definitions and what the storage order of the boundary
! slab arrays are (which depends on which boundaries they represent).  The k memory and domain dimensions must be set
! properly for 2d (ks=1, ke=1) versus 3d fields.

#if 1
          IF ( (MemOrd(1:2) .EQ. 'xs' .AND. bdy_mask( P_XSB )) .OR.     &
               (MemOrd(1:2) .EQ. 'xe' .AND. bdy_mask( P_XEB ))       ) THEN
            my_displ = jps-1         
            my_count = jpe-jps+1
          ELSE
            my_displ = 0
            my_count = 0
          ENDIF
#else
          IF ( (MemOrd(1:2) .EQ. 'xs' ) .OR.     &
               (MemOrd(1:2) .EQ. 'xe' )       ) THEN
            my_displ = jps-1         
            my_count = jpe-jps+1
          ELSE
            my_displ = 0
            my_count = 0
          ENDIF
#endif

          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )

          do i = ips,ipe    ! bdy_width
          do k = kds,kde    ! levels
             lx   = jme-jms+1
             lx2  = jde-jds+1
             idx  = lx*((k-1)+(i-1)*(kme-kms+1))
             idx2 = lx2*((k-1)+(i-1)*(kde-kds+1))
             IF ( FieldType .EQ. WRF_DOUBLE  ) THEN
               CALL wrf_scatterv_double (                        &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

               CALL wrf_scatterv_real (                          &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )

             ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
               CALL wrf_scatterv_integer (                       &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, jps-jms+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )
             ENDIF
          enddo
          enddo
        ENDIF
      ENDIF

      IF ( TRIM(MemOrd) .EQ. 'ysz' .OR. TRIM(MemOrd) .EQ. 'yez' .OR. &
           TRIM(MemOrd) .EQ. 'ys'  .OR. TRIM(MemOrd) .EQ. 'ye'    ) THEN


       ids=DomainStart(1); ide=dom_end_rev(1); jds=DomainStart(3); jde=dom_end_rev(3); kds=DomainStart(2); kde=dom_end_rev(2);
       ims=MemoryStart(1); ime=  MemoryEnd(1); jms=MemoryStart(3); jme=  MemoryEnd(3); kms=MemoryStart(2); kme=  MemoryEnd(2);
       ips= PatchStart(1); ipe=   PatchEnd(1); jps= PatchStart(3); jpe=   PatchEnd(3); kps= PatchStart(2); kpe=   PatchEnd(2);

        IF ( nproc .GT. 1 ) THEN

#if 1
          IF ( (MemOrd(1:2) .EQ. 'ys' .AND. bdy_mask( P_YSB )) .OR.     &
               (MemOrd(1:2) .EQ. 'ye' .AND. bdy_mask( P_YEB ))       ) THEN
            my_displ = ips-1
            my_count = ipe-ips+1
           ELSE
             my_displ = 0
             my_count = 0
          ENDIF
#else
          IF ( (MemOrd(1:2) .EQ. 'ys' ) .OR.     &
               (MemOrd(1:2) .EQ. 'ye' )       ) THEN
            my_displ = ips-1
            my_count = ipe-ips+1
          ELSE
            my_displ = 0
            my_count = 0
          ENDIF
#endif

          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, collective_root, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, collective_root, communicator, ierr )

          do j = jds,jde    ! bdy_width
          do k = kds,kde    ! levels
             lx   = ime-ims+1
             lx2  = ide-ids+1
             idx  = lx*((k-1)+(j-1)*(kme-kms+1))
             idx2 = lx2*((k-1)+(j-1)*(kde-kds+1))

             IF ( FieldType .EQ. WRF_DOUBLE  ) THEN
               CALL wrf_scatterv_double (                        &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN
               CALL wrf_scatterv_real (                          &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )
             ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
               CALL wrf_scatterv_integer (                       &
                               globbuf, 1+idx2 ,                &    ! sendbuf
                               counts                         , &    ! sendcounts
                               Field, ips-ims+1+idx ,       &
                               my_count ,                       &    ! recvcount
                               displs                         , &    ! displs
                               collective_root                , &    ! root
                               communicator                   , &    ! communicator
                               ierr )
             ENDIF
          enddo
          enddo
        ENDIF
      ENDIF

    ELSE  ! not a boundary 
  
      IF ( FieldType .EQ. WRF_DOUBLE ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_double (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_double (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_FLOAT ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_real (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_real (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_integer (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_integer (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN

        SELECT CASE (MemOrd)
        CASE ( 'xzy','xyz','yxz','zxy' )
          CALL wrf_global_to_patch_logical (  globbuf,  Field  , DomainDesc, Stagger, MemOrd ,    &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3)  )
        CASE ( 'xy','yx' )
          CALL wrf_global_to_patch_logical (  globbuf, Field ,  DomainDesc, Stagger, MemOrd ,  &
             DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), 1            , 1 , &
             MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), 1            , 1 , &
             PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , 1            , 1   )
        END SELECT

      ENDIF
    ENDIF
#endif

  ELSE ! not a distributed field

    IF ( wrf_dm_on_monitor()) THEN
      CALL fcn ( Hndl , DateStr , VarName , Field   , FieldType , Comm , IOComm , &
                 DomainDesc , MemoryOrder , Stagger , DimNames ,                  &
                 DomainStart , DomainEnd ,                                        &
                 MemoryStart , MemoryEnd ,                                        &
                 PatchStart  , PatchEnd  ,                                        &
                 Status )
    ENDIF
    CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
    memsize = (MemoryEnd(1)-MemoryStart(1)+1)*(MemoryEnd(2)-MemoryStart(2)+1)*(MemoryEnd(3)-MemoryStart(3)+1)
    IF ( FieldType .EQ. WRF_DOUBLE ) THEN 
      CALL wrf_dm_bcast_bytes( Field , DWORDSIZE*memsize )
    ELSE IF ( FieldType .EQ. WRF_FLOAT) THEN
      CALL wrf_dm_bcast_bytes( Field , RWORDSIZE*memsize )
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      CALL wrf_dm_bcast_bytes( Field , IWORDSIZE*memsize )
    ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
      CALL wrf_dm_bcast_bytes( Field , LWORDSIZE*memsize )
    ENDIF

  ENDIF

  DEALLOCATE(displs)
  DEALLOCATE(counts)
  RETURN
END SUBROUTINE call_pkg_and_dist_generic

!!!!!!  Miscellaneous routines

! stole these routines from io_netcdf external package; changed names to avoid collisions
SUBROUTINE dim_from_memorder(MemoryOrder,NDim)
!<DESCRIPTION>
!<PRE>
! Decodes array ranks from memory order.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*) ,INTENT(IN)  :: MemoryOrder
  INTEGER       ,INTENT(OUT) :: NDim
!Local
  CHARACTER*3                :: MemOrd
!
  CALL Lower_Case(MemoryOrder,MemOrd)
  SELECT CASE (MemOrd)
    CASE ('xyz','xzy','yxz','yzx','zxy','zyx')
      NDim = 3
    CASE ('xy','yx')
      NDim = 2
    CASE ('z','c','0')
      NDim = 1
    CASE DEFAULT
      NDim = 0
      RETURN
  END SELECT
  RETURN
END SUBROUTINE dim_from_memorder

SUBROUTINE lower_case(MemoryOrder,MemOrd)
!<DESCRIPTION>
!<PRE>
! Translates upper-case characters to lower-case.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*) ,INTENT(IN)  :: MemoryOrder
  CHARACTER*(*) ,INTENT(OUT) :: MemOrd
!Local
  CHARACTER*1                :: c
  INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
  INTEGER                    :: i,n,n1
!
  MemOrd = ' '
  N = len(MemoryOrder)
  N1 = len(MemOrd)
  N = MIN(N,N1)
  MemOrd(1:N) = MemoryOrder(1:N)
  DO i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  ENDDO
  RETURN
END SUBROUTINE Lower_Case

LOGICAL FUNCTION has_char( str, c )
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. iff string str contains character c.  Ignores character case.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
  CHARACTER*(*) str
  CHARACTER c, d
  CHARACTER*80 str1, str2, str3
  INTEGER i

  CALL lower_case( TRIM(str), str1 )
  str2 = ""
  str2(1:1) = c
  CALL lower_case( str2, str3 )
  d = str3(1:1)
  DO i = 1, LEN(TRIM(str1))
    IF ( str1(i:i) .EQ. d ) THEN
      has_char = .TRUE.
      RETURN
    ENDIF
  ENDDO
  has_char = .FALSE.
  RETURN
END FUNCTION has_char


!WRF:DRIVER_LAYER:IO
!
#define DEBUG_LVL 500

MODULE module_io

  LOGICAL :: is_inited = .FALSE.
  INTEGER :: wrf_io_handles(1000), how_opened(1000) 
  LOGICAL :: for_output(1000)

! WRF-specific package independent interface to package-dependent WRF specific
! I/O packages.

! These routines have the same names as those specified in the I/O API except that:
! Subroutines in this routine have the wrf_ prefix
! Subroutines in the packages have the ext_ prefix or int_ prefixes

! We wish to be able to link to different packages depending on whether
! the I/O is restart, initial, history, or boundary

!
! include the file generated from md_calls.m4 using the m4 preprocessor
! note that this file also includes the CONTAINS declaration for the module
!
#include <md_calls.inc>

!--- ioinit

SUBROUTINE wrf_ioinit( Status )
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status
!Local
  CHARACTER(len=80) :: SysDepInfo,inquiry,result
!
  Status = 0
  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioinit' )
  CALL init_io_handles    ! defined below
#ifdef NETCDF
  CALL ext_ncd_ioinit( SysDepInfo, Status )
#endif
#ifdef PHDF5
  CALL ext_phdf5_ioinit(SysDepInfo, Status )
#endif
END SUBROUTINE wrf_ioinit

!--- ioexit

SUBROUTINE wrf_ioexit( Status )
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status
!Local
  LOGICAL, EXTERNAL :: use_output_servers
!
  Status = 0
  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioexit' )
#ifdef NETCDF
  CALL ext_ncd_ioexit( Status )
#endif
#ifdef PHDF5
  CALL ext_phdf5_ioexit(Status)
#endif
  IF ( use_output_servers() ) CALL ext_quilt_ioexit( Status )
END SUBROUTINE wrf_ioexit

!--- open_for_write_begin

SUBROUTINE wrf_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , Status )
  USE module_state_description
  IMPLICIT NONE
#include <wrf_io_flags.h>
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*), INTENT(INOUT):: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 !Local 
  CHARACTER*128               :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  CHARACTER*128     :: LocFilename   ! for appending the process ID if necessary
  INTEGER           :: myproc
  CHARACTER*128     :: mess

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_write_begin' )

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )
  IF      ( DataSet .eq. 'RESTART' ) THEN
    CALL get_io_form_restart( io_form )
  ELSE IF ( DataSet .eq. 'INPUT' ) THEN
    CALL get_io_form_input( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT1' ) THEN
    CALL get_io_form_auxinput1( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT2' ) THEN
    CALL get_io_form_auxinput2( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT3' ) THEN
    CALL get_io_form_auxinput3( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT4' ) THEN
    CALL get_io_form_auxinput4( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT5' ) THEN
    CALL get_io_form_auxinput5( io_form )
  ELSE IF ( DataSet .eq. 'HISTORY' ) THEN
    CALL get_io_form_history( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST1' ) THEN
    CALL get_io_form_auxhist1( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST2' ) THEN
    CALL get_io_form_auxhist2( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST3' ) THEN
    CALL get_io_form_auxhist3( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST4' ) THEN
    CALL get_io_form_auxhist4( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST5' ) THEN
    CALL get_io_form_auxhist5( io_form )
  ELSE IF ( DataSet .eq. 'BOUNDARY' ) THEN
    CALL get_io_form_boundary( io_form )
  ELSE  ! default if nothing is set in SysDepInfo; use history
    CALL get_io_form_history( io_form )
  ENDIF

  Status = 0
  Hndl = -1
  IF ( multi_files( io_form ) .OR. .NOT. use_output_servers() ) THEN
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
          CALL ext_ncd_open_for_write_begin ( LocFileName , Comm_compute, Comm_io, SysDepInfo, &
                                              Hndl , Status )
        ENDIF
        IF ( .NOT. multi_files(io_form) ) THEN
          CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
        ENDIF
#endif
#ifdef HDF
      CASE ( IO_HDF   )
        CALL ext_hdf_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef PHDF5
      CASE (IO_PHDF5  )
        CALL ext_phdf5_open_for_write_begin( FileName, Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl, Status)
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
        CALL ext_yyy_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , Status )
#endif
#ifdef MCELIO
      CASE ( IO_MCEL )
        IF ( wrf_dm_on_monitor() ) THEN
          CALL ext_mcel_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                               Hndl , Status )
        ENDIF
        CALL wrf_dm_bcast_bytes( Hndl, IWORDSIZE )
        CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
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
          CALL ext_int_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
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
  ELSE IF ( use_output_servers() ) THEN
    IF ( io_form .GT. 0 ) THEN
      CALL ext_quilt_open_for_write_begin ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                            Hndl , io_form, Status )
    ENDIF
  ELSE
    Status = 0
  ENDIF
  CALL add_new_handle( Hndl, io_form, .TRUE., DataHandle )
END SUBROUTINE wrf_open_for_write_begin

!--- open_for_write_commit

SUBROUTINE wrf_open_for_write_commit( DataHandle , Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
 
  CHARACTER (128)             :: DataSet
  INTEGER                     :: io_form
  INTEGER                     :: Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
#include <wrf_io_flags.h>

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_write_commit' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
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
#ifdef HDF
      CASE ( IO_HDF   )
        CALL ext_hdf_open_for_write_commit ( Hndl , Status )
#endif
#ifdef PHDF5
      CASE ( IO_PHDF5  )
        CALL ext_phdf5_open_for_write_commit ( Hndl , Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
        CALL ext_xxx_open_for_write_commit ( Hndl , Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
        CALL ext_yyy_open_for_write_commit ( Hndl , Status )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
        CALL ext_zzz_open_for_write_commit ( Hndl , Status )
#endif
#ifdef INTIO
      CASE ( IO_INTIO   )
        CALL ext_int_open_for_write_commit ( Hndl , Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_open_for_write_commit ( Hndl , Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_open_for_write_commit

!--- open_for_read 

SUBROUTINE wrf_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
  USE module_state_description
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CHARACTER (128)             :: DataSet, LocFileName
  INTEGER                     :: io_form, myproc
  INTEGER                     :: Hndl
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_open_for_read' )

  CALL get_value_from_pairs ( "DATASET" , SysDepInfo , DataSet )
  IF      ( DataSet .eq. 'RESTART' ) THEN
    CALL get_io_form_restart( io_form )
  ELSE IF ( DataSet .eq. 'INPUT' ) THEN
    CALL get_io_form_input( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT1' ) THEN
    CALL get_io_form_auxinput1( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT2' ) THEN
    CALL get_io_form_auxinput2( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT3' ) THEN
    CALL get_io_form_auxinput3( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT4' ) THEN
    CALL get_io_form_auxinput4( io_form )
  ELSE IF ( DataSet .eq. 'AUXINPUT5' ) THEN
    CALL get_io_form_auxinput5( io_form )
  ELSE IF ( DataSet .eq. 'HISTORY' ) THEN
    CALL get_io_form_history( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST1' ) THEN
    CALL get_io_form_auxhist1( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST2' ) THEN
    CALL get_io_form_auxhist2( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST3' ) THEN
    CALL get_io_form_auxhist3( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST4' ) THEN
    CALL get_io_form_auxhist4( io_form )
  ELSE IF ( DataSet .eq. 'AUXHIST5' ) THEN
    CALL get_io_form_auxhist5( io_form )
  ELSE IF ( DataSet .eq. 'BOUNDARY' ) THEN
    CALL get_io_form_boundary( io_form )
  ELSE  ! default if nothing is set in SysDepInfo; use history
    CALL get_io_form_history( io_form )
  ENDIF

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
#ifdef PHDF5
    CASE ( IO_PHDF5  )
      CALL ext_phdf5_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef HDF
    CASE ( IO_HDF   )
      CALL ext_hdf_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef XXX
    CASE ( IO_XXX   )
      CALL ext_xxx_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef YYY
    CASE ( IO_YYY   )
      CALL ext_yyy_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
#endif
#ifdef ZZZ
    CASE ( IO_ZZZ   )
      CALL ext_zzz_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               Hndl , Status )
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

!--- intio_nextrec  (INT_IO only)
SUBROUTINE wrf_intio_nextrec ( DataHandle , NextRec , Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: DataHandle
  INTEGER               :: NextRec
  INTEGER               :: Status
  INTEGER io_form , Hndl
  LOGICAL for_out
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER, EXTERNAL           :: use_package
#include <wrf_io_flags.h>

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_intio_nextrec' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
! Note that this function is only defined for internal I/O
      SELECT CASE ( use_package(io_form) )
#ifdef INTIO_NOTYET
        CASE ( IO_INTIO )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) THEN
            CALL int_intio_nextrec ( Hndl , NextRec , Status )
          ENDIF
          IF ( .NOT. multi_files(io_form) ) CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
          Status = 0
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
  ELSE
    Status = 0
  ENDIF
  RETURN  
END SUBROUTINE wrf_intio_nextrec

!--- inquire_opened

SUBROUTINE wrf_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
#include <wrf_io_flags.h>
#include <wrf_status_codes.h>

  INTEGER io_form , Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_inquire_opened' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
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
#ifdef HDF
      CASE ( IO_HDF   )
          CALL ext_hdf_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef XXX
      CASE ( IO_XXX   )
          CALL ext_xxx_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef YYY
      CASE ( IO_YYY   )
          CALL ext_yyy_inquire_opened ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef ZZZ
      CASE ( IO_ZZZ   )
          CALL ext_zzz_inquire_opened ( Hndl, FileName , FileStatus, Status )
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
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_inquire_opened ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileStatus = WRF_FILE_NOT_OPENED
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_opened

!--- inquire_filename


SUBROUTINE wrf_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  LOGICAL                     :: for_out

  INTEGER io_form , Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_inquire_filename' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF (wrf_dm_on_monitor()) CALL ext_ncd_inquire_filename ( Hndl, FileName , FileStatus, Status )
          CALL wrf_dm_bcast_bytes( FileStatus, IWORDSIZE )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_inquire_filename ( Hndl, FileName , FileStatus, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
            CALL ext_zzz_inquire_filename ( Hndl, FileName , FileStatus, Status )
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
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_inquire_filename ( Hndl, FileName , FileStatus, Status )
    ENDIF
  ELSE
    FileName = ""
    Status = 0
  ENDIF
  RETURN
END SUBROUTINE wrf_inquire_filename

!--- sync

SUBROUTINE wrf_iosync ( DataHandle, Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  LOGICAL                     :: for_out

  INTEGER io_form , Hndl

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_iosync' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR. wrf_dm_on_monitor() ) CALL ext_ncd_iosync( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status    , IWORDSIZE )
#endif
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_iosync( Hndl, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_iosync( Hndl, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_iosync( Hndl, Status )
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
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_iosync( Hndl, Status )
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
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_ioclose' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
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
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_ncd_ioclose( Hndl, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_ioclose( Hndl, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_ioclose( Hndl, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_ioclose( Hndl, Status )
#endif
#ifdef MCELIO
        CASE ( IO_MCEL   )
          CALL ext_mcel_ioclose( Hndl, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_ioclose( Hndl, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_ioclose( Hndl, Status )
    ELSE
      Status = 0
    ENDIF
    CALL free_handle( DataHandle )
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_ioclose

!--- get_next_time (not defined for IntIO )

SUBROUTINE wrf_get_next_time ( DataHandle, DateStr, Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl, len_of_str
  LOGICAL                     :: for_out

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_next_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_bytes( len_of_str, IWORDSIZE )
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_next_time( Hndl, DateStr, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_bytes( len_of_str, IWORDSIZE )
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_get_next_time( Hndl, DateStr, Status )
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
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl, len_of_str
  LOGICAL                     :: for_out

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_previous_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_previous_time( Hndl, DateStr, Status )
          IF ( .NOT. multi_files(io_form) ) THEN
            CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
            len_of_str = LEN(DateStr)
            CALL wrf_dm_bcast_bytes( len_of_str, IWORDSIZE )
            CALL wrf_dm_bcast_string ( DateStr , len_of_str )
          ENDIF
#endif
#ifdef PHDF5
        CASE ( IO_PHDF5   )
          CALL ext_phdf5_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_previous_time( Hndl, DateStr, Status )
#endif
#ifdef INTIO
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_get_previous_time( Hndl, DateStr, Status )
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
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_set_time' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
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
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_set_time( Hndl, DateStr, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_set_time( Hndl, DateStr, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_set_time( Hndl, DateStr, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_set_time( Hndl, DateStr, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_set_time( Hndl, DateStr, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_set_time( Hndl, DateStr, Status )
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
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status
#include <wrf_status_codes.h>

  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_next_var' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_ncd_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
#ifdef HDF
        CASE ( IO_HDF   )
          CALL ext_hdf_get_next_var( Hndl, VarName, Status )
#endif
#ifdef XXX
        CASE ( IO_XXX   )
          CALL ext_xxx_get_next_var( Hndl, VarName, Status )
#endif
#ifdef YYY
        CASE ( IO_YYY   )
          CALL ext_yyy_get_next_var( Hndl, VarName, Status )
#endif
#ifdef ZZZ
        CASE ( IO_ZZZ   )
          CALL ext_zzz_get_next_var( Hndl, VarName, Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO   )
          IF ( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) CALL ext_int_get_next_var( Hndl, VarName, Status )
          CALL wrf_dm_bcast_bytes( Status, IWORDSIZE )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_get_next_var( Hndl, VarName, Status )
    ELSE
      Status = 0
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_get_next_var


SUBROUTINE wrf_read_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                            Comm       , IOComm  ,                                       &
                            DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
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
#include <wrf_status_codes.h>
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers, use_input_servers
#ifdef NETCDF
  EXTERNAL     ext_ncd_read_field
#endif
#ifdef INTIO
  EXTERNAL     ext_int_read_field
#endif
#ifdef HDF
  EXTERNAL ext_hdf_read_field
#endif
#ifdef XXX
  EXTERNAL ext_xxx_read_field
#endif
#ifdef YYY
  EXTERNAL ext_yyy_read_field
#endif

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_read_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( .NOT. use_input_servers() ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )

          CALL call_pkg_and_dist   ( ext_ncd_read_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )

#endif
#ifdef PHDF5
        CASE ( IO_PHDF5)
! this should call call_pkg_and_dist... but should pass true for multi-files  JM ZAP
          CALL ext_phdf5_read_field   (                   &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef HDF
        CASE ( IO_HDF )
          CALL call_pkg_and_dist   ( ext_hdf_read_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef XXX
        CASE ( IO_XXX )
          CALL call_pkg_and_dist   ( ext_xxx_read_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef YYY
        CASE ( IO_YYY )
          CALL call_pkg_and_dist   ( ext_yyy_read_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef INTIO
        CASE ( IO_INTIO )
          CALL call_pkg_and_dist   ( ext_int_read_field, multi_files(io_form),                  &
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
      CALL wrf_error_fatal('module_io.F: wrf_read_field: input_servers not inplemented yet')
    ENDIF
  ELSE
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
  ENDIF
  RETURN
END SUBROUTINE wrf_read_field

SUBROUTINE wrf_write_field ( DataHandle , DateStr , VarName , Field , FieldType ,         &
                             Comm       , IOComm  ,                                       &
                             DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )


  USE module_state_description
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  LOGICAL, DIMENSION(4)         ,INTENT(IN)    :: bdy_mask
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(OUT)   :: Status
#include <wrf_status_codes.h>
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers
#ifdef NETCDF
  EXTERNAL     ext_ncd_write_field
#endif
#ifdef MCELIO
  EXTERNAL     ext_mcel_write_field
#endif
#ifdef INTIO
  EXTERNAL     ext_int_write_field
#endif
#ifdef HDF
  EXTERNAL ext_hdf_write_field
#endif
#ifdef XXX
  EXTERNAL ext_xxx_write_field
#endif
#ifdef YYY
  EXTERNAL ext_yyy_write_field
#endif

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_write_field' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF ( multi_files( io_form ) .OR. .NOT. use_output_servers() ) THEN
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
#ifdef PHDF5
        CASE ( IO_PHDF5 )
! this should call collect_fld_and... but should pass true for multi-files  JM ZAP
          CALL ext_phdf5_write_field(                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
#endif
#ifdef HDF
        CASE ( IO_HDF )
          CALL collect_fld_and_call_pkg ( ext_hdf_write_field, multi_files(io_form),                  &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
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
    ELSE IF ( use_output_servers() ) THEN
      IF ( io_form .GT. 0 ) THEN
      CALL ext_quilt_write_field ( Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
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
END SUBROUTINE wrf_write_field

! wrf_get_var_info  (not implemented for IntIO)

SUBROUTINE wrf_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
  USE module_state_description
  IMPLICIT NONE
  INTEGER               ,INTENT(IN)     :: DataHandle
  CHARACTER*(*)         ,INTENT(IN)     :: VarName
  INTEGER               ,INTENT(OUT)    :: NDim
  CHARACTER*(*)         ,INTENT(OUT)    :: MemoryOrder
  CHARACTER*(*)         ,INTENT(OUT)    :: Stagger
  INTEGER ,dimension(*) ,INTENT(OUT)    :: DomainStart, DomainEnd
  INTEGER               ,INTENT(OUT)    :: Status
#include <wrf_status_codes.h>
  INTEGER io_form , Hndl
  LOGICAL                     :: for_out
  INTEGER, EXTERNAL           :: use_package
  LOGICAL, EXTERNAL           :: wrf_dm_on_monitor, multi_files, use_output_servers

  CALL wrf_debug( DEBUG_LVL, 'module_io.F: in wrf_get_var_info' )

  Status = 0
  CALL get_handle ( Hndl, io_form , for_out, DataHandle )
  IF ( Hndl .GT. -1 ) THEN
    IF (( multi_files(io_form) .OR.  wrf_dm_on_monitor() ) .AND. .NOT. (for_out .AND. use_output_servers()) ) THEN
      SELECT CASE ( use_package( io_form ) )
#ifdef NETCDF
        CASE ( IO_NETCDF   )
          CALL ext_ncd_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
#ifdef HDF
        CASE ( IO_HDF )
          CALL ext_hdf_get_var_info ( Hndl , VarName , NDim ,            &
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
#ifdef XXX
        CASE ( IO_XXX )
          CALL ext_xxx_get_var_info ( Hndl , VarName , NDim ,            &
                                      MemoryOrder , Stagger ,                  &
                                      DomainStart , DomainEnd ,                &
                                      Status )
#endif
        CASE DEFAULT
          Status = 0
      END SELECT
    ELSE IF ( io_form .GT. 0 .AND. for_out .AND. use_output_servers() ) THEN
      CALL ext_quilt_get_var_info ( Hndl , VarName , NDim ,            &
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
  IMPLICIT NONE
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    DO i = 1, 1000
      wrf_io_handles(i) = -999319
    ENDDO
    is_inited = .TRUE.
  ENDIF
  RETURN
END SUBROUTINE init_io_handles

! Stash the package specific handle and return a WRF handle
SUBROUTINE add_new_handle( Hndl, Hopened, for_out, DataHandle )
  IMPLICIT NONE
  INTEGER, INTENT(IN)     :: Hndl
  INTEGER, INTENT(IN)     :: Hopened
  LOGICAL, INTENT(IN)     :: for_out
  INTEGER, INTENT(OUT)    :: DataHandle
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'add_new_handle: not initialized' )
  ENDIF
  DataHandle = -1
  DO i = 1, 1000
    IF ( wrf_io_handles(i) .EQ. -999319 ) THEN
      DataHandle = i 
      wrf_io_handles(i) = Hndl
      how_opened(i)     = Hopened
      for_output(DataHandle) = for_out
      EXIT
    ENDIF
  ENDDO
  IF ( DataHandle .EQ. -1 ) THEN
    CALL wrf_error_fatal( 'add_new_handle: no handles left' )
  ENDIF
  RETURN
END SUBROUTINE add_new_handle

SUBROUTINE get_handle ( Hndl, Hopened, for_out, DataHandle )
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
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. 1000 ) THEN
    Hndl = wrf_io_handles(DataHandle)
    Hopened = how_opened(DataHandle)
    for_out = for_output(DataHandle)
  ELSE
    Hndl = -1
  ENDIF
  RETURN
END SUBROUTINE get_handle

! Trash a handle and return to pool
SUBROUTINE free_handle ( DataHandle )
  IMPLICIT NONE
  INTEGER, INTENT(IN)    :: DataHandle
  INTEGER i
  IF ( .NOT. is_inited ) THEN
    CALL wrf_error_fatal( 'free_handle: not initialized' )
  ENDIF
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. 1000 ) THEN
    wrf_io_handles(DataHandle) = -999319
  ENDIF
  RETURN
END SUBROUTINE free_handle

!--------------------------------------------------------------

SUBROUTINE init_module_io
  CALL init_io_handles
END SUBROUTINE init_module_io
END MODULE module_io

! parse comma separated list of VARIABLE=VALUE strings and return the
! value for the matching variable if such exists, otherwise return
! the empty string
SUBROUTINE get_value_from_pairs ( varname , str , retval )
  IMPLICIT NONE
  CHARACTER*(*) ::    varname
  CHARACTER*(*) ::    str
  CHARACTER*(*) ::    retval

  CHARACTER (128) varstr, tstr
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
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
#ifdef DM_PARALLEL
  multi_files = io_form > 99
#else
  multi_files = .FALSE.
#endif
END FUNCTION multi_files

INTEGER FUNCTION use_package ( io_form )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: io_form
  use_package = MOD( io_form, 100 )
END FUNCTION use_package

!!!  Routines that collect a distributed array onto one processor and then call
!!!  an I/O function to write the result (or in the case of replicated data
!!!  simply write monitor node's copy of the data)

SUBROUTINE collect_fld_and_call_pkg (    fcn, donotcollect_arg,                                       &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  IMPLICIT NONE
  include 'wrf_io_flags.h'
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

  ELSE IF ( FieldType .EQ. WRF_DOUBLE  .OR.  FieldType .EQ. WRF_REAL ) THEN

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

  ENDIF
  RETURN
END SUBROUTINE collect_fld_and_call_pkg

! sole purpose of this wrapper is to allocate a big real buffer
SUBROUTINE collect_real_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
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
  REAL globbuf ( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) )
  
  CALL collect_generic_and_call_pkg (   fcn, globbuf,                                             &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  RETURN

END SUBROUTINE collect_real_and_call_pkg

! sole purpose of this wrapper is to allocate a big integer buffer
SUBROUTINE collect_int_and_call_pkg (   fcn,                                                     &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
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
  INTEGER globbuf ( (DomainEnd(1)-DomainStart(1)+3)*(DomainEnd(2)-DomainStart(2)+3)*(DomainEnd(3)-DomainStart(3)+3) )

  CALL collect_generic_and_call_pkg (   fcn, globbuf,                                             &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  RETURN

END SUBROUTINE collect_int_and_call_pkg

SUBROUTINE collect_generic_and_call_pkg ( fcn, globbuf,                                           &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,    &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  USE module_state_description
  USE module_driver_constants
  IMPLICIT NONE
  include 'wrf_io_flags.h'
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
  INTEGER nproc, communicator, displs(10*1024), mpi_bdyslice_type, ierr, my_displ, recv_count, root_proc, send_count, itype
  INTEGER my_count, counts(10*1024)
  INTEGER , dimension(3)                       :: dom_end_rev
  LOGICAL, EXTERNAL         :: wrf_dm_on_monitor
  LOGICAL     distributed_field
  INTEGER i,j,k,idx,lx,idx2,lx2

  CALL wrf_get_nproc( nproc )
  CALL wrf_get_dm_communicator ( communicator )
  CALL lower_case( MemoryOrder, MemOrd )

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
      IF ( FieldType .EQ. WRF_DOUBLE .OR. FieldType .EQ. WRF_REAL ) THEN
        CALL wrf_patch_to_global_real ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
           DomainStart(1), DomainEnd(1), DomainStart(2), DomainEnd(2), DomainStart(3), DomainEnd(3), &
           MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), MemoryStart(3), MemoryEnd(3), &
           PatchStart(1) , PatchEnd(1) , PatchStart(2) , PatchEnd(2) , PatchStart(3) , PatchEnd(3) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        CALL wrf_patch_to_global_integer ( Field  , globbuf , DomainDesc, Stagger, MemOrd ,             &
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
          recv_count = 1
          root_proc = 0
          send_count = 1
          itype = MPI_INTEGER
          CALL mpi_gather( my_displ, send_count, itype, displs, recv_count, itype, root_proc, communicator, ierr )
          CALL mpi_gather( my_count, send_count, itype, counts, recv_count, itype, root_proc, communicator, ierr )
        ELSE
          my_displ = 0
          my_count = 0
          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, 0, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, communicator, ierr )
        ENDIF
        do i = DomainStart(3),DomainEnd(3)    ! bdy_width
        do k = DomainStart(2),DomainEnd(2)    ! levels
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(i-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           IF ( FieldType .EQ. WRF_DOUBLE  .OR. FieldType .EQ. WRF_REAL ) THEN
             CALL wrf_gatherv_real ( Field, PatchStart(1)+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
             CALL wrf_gatherv_integer ( Field, PatchStart(1)+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
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
          recv_count = 1
          root_proc = 0
          send_count = 1
          itype = MPI_INTEGER
          CALL mpi_gather( my_displ, send_count, itype, displs, recv_count, itype, root_proc, communicator, ierr )
          CALL mpi_gather( my_count, send_count, itype, counts, recv_count, itype, root_proc, communicator, ierr )
        ELSE
          my_displ = 0
          my_count = 0
          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, 0, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, communicator, ierr )
        ENDIF
        do i = DomainStart(2),DomainEnd(2)    ! bdy_width
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(i-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(i-1)
           IF ( FieldType .EQ. WRF_DOUBLE  .OR. FieldType .EQ. WRF_REAL ) THEN
             CALL wrf_gatherv_real ( Field, PatchStart(1)+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
                             communicator                   , &    ! communicator
                             ierr )

           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
             CALL wrf_gatherv_integer ( Field, PatchStart(1)+idx , &
                             my_count ,                       &    ! sendcount
                             globbuf, 1+idx2 ,                &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
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
          recv_count = 1
          root_proc = 0
          send_count = 1
          itype = MPI_INTEGER
          CALL mpi_gather( my_displ, send_count, itype, displs, recv_count, itype, root_proc, communicator, ierr )
          CALL mpi_gather( my_count, send_count, itype, counts, recv_count, itype, root_proc, communicator, ierr )
        ELSE
          my_displ = 0
          my_count = 0
          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, 0, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, communicator, ierr )
        ENDIF
        do j = DomainStart(3),DomainEnd(3)    ! bdy_width
        do k = DomainStart(2),DomainEnd(2)    ! levels
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx  = lx*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))
           idx2 = lx2*((k-1)+(j-1)*(MemoryEnd(2)-MemoryStart(2)+1))

           IF ( FieldType .EQ. WRF_DOUBLE .OR. FieldType .EQ. WRF_REAL ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
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
          recv_count = 1
          root_proc = 0
          send_count = 1
          itype = MPI_INTEGER
          CALL mpi_gather( my_displ, send_count, itype, displs, recv_count, itype, root_proc, communicator, ierr )
          CALL mpi_gather( my_count, send_count, itype, counts, recv_count, itype, root_proc, communicator, ierr )
        ELSE
          my_displ = 0
          my_count = 0
          CALL mpi_gather( my_displ, 1, MPI_INTEGER, displs, 1, MPI_INTEGER, 0, communicator, ierr )
          CALL mpi_gather( my_count, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, communicator, ierr )
        ENDIF
        do j = DomainStart(2),DomainEnd(2)    ! bdy_width
           lx   = MemoryEnd(1)-MemoryStart(1)+1
           idx  = lx*(j-1)
           lx2  = dom_end_rev(1)-DomainStart(1)+1
           idx2 = lx2*(j-1)

           IF ( FieldType .EQ. WRF_DOUBLE .OR. FieldType .EQ. WRF_REAL ) THEN

             CALL wrf_gatherv_real( Field, PatchStart(1)+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
                             communicator                   , &    ! communicator
                             ierr )
           ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

             CALL wrf_gatherv_integer( Field, PatchStart(1)+idx ,      &    ! sendbuf
                             my_count                       , &    ! sendcount
                             globbuf, 1+idx2                , &    ! recvbuf
                             counts                         , &    ! recvcounts
                             displs                         , &    ! displs
                             0                              , &    ! root
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
  RETURN
END SUBROUTINE collect_generic_and_call_pkg

!!!  Routines that call an I/O function and then distribute or replicate the result

SUBROUTINE call_pkg_and_dist (       fcn, donotdist_arg,                                       &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , bdy_mask, MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  IMPLICIT NONE
  include 'wrf_io_flags.h'
  EXTERNAL fcn
  LOGICAL,        INTENT(IN)    :: donotdist_arg
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

  ELSE IF ((FieldType .EQ. WRF_DOUBLE)  .OR. (FieldType .EQ. WRF_REAL)) THEN

     CALL call_pkg_and_dist_real ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN

     CALL call_pkg_and_dist_int ( fcn,                                        &
               Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
               DomainDesc , MemoryOrder , Stagger , DimNames ,              &
               DomainStart , DomainEnd ,                                    &
               MemoryStart , MemoryEnd ,                                    &
               PatchStart , PatchEnd ,                                      &
               Status )

  ENDIF
  RETURN
END SUBROUTINE call_pkg_and_dist

SUBROUTINE call_pkg_and_dist_real (  fcn,                                                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  REAL globbuf ( (DomainEnd(1)-DomainStart(1)+2)*(DomainEnd(2)-DomainStart(2)+2)*(DomainEnd(3)-DomainStart(3)+2) )

  globbuf = 0.
  CALL call_pkg_and_dist_generic (   fcn, globbuf ,                                               &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  RETURN
END SUBROUTINE call_pkg_and_dist_real

SUBROUTINE call_pkg_and_dist_int  (  fcn,                                                         &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  IMPLICIT NONE
  EXTERNAL fcn
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL    ,       INTENT(INOUT)    :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
  CHARACTER*(*)                 ,INTENT(IN)    :: MemoryOrder
  CHARACTER*(*)                 ,INTENT(IN)    :: Stagger
  CHARACTER*(*) , dimension (*) ,INTENT(IN)    :: DimNames
  INTEGER ,dimension(*)         ,INTENT(IN)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,INTENT(IN)    :: PatchStart,  PatchEnd
  INTEGER                       ,INTENT(INOUT)   :: Status
  INTEGER globbuf ( (DomainEnd(1)-DomainStart(1)+2)*(DomainEnd(2)-DomainStart(2)+2)*(DomainEnd(3)-DomainStart(3)+2) )

  globbuf = 0

  CALL call_pkg_and_dist_generic (   fcn, globbuf ,                                               &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )
  RETURN
END SUBROUTINE call_pkg_and_dist_int

SUBROUTINE call_pkg_and_dist_generic (   fcn, globbuf ,                                               &
                                     Hndl , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                                     DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                     DomainStart , DomainEnd ,                                    &
                                     MemoryStart , MemoryEnd ,                                    &
                                     PatchStart , PatchEnd ,                                      &
                                     Status )

  USE module_driver_constants
  IMPLICIT NONE
#include <wrf_io_flags.h>
  EXTERNAL fcn
  REAL, DIMENSION(*) ::  globbuf
  INTEGER ,       INTENT(IN)    :: Hndl
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  REAL                           :: Field(*)
  INTEGER                       ,INTENT(IN)    :: FieldType
  INTEGER                       ,INTENT(INOUT) :: Comm
  INTEGER                       ,INTENT(INOUT) :: IOComm
  INTEGER                       ,INTENT(IN)    :: DomainDesc
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
  LOGICAL distributed_field

  CALL lower_case( MemoryOrder, MemOrd )

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

  SELECT CASE (MemOrd)
    CASE ( 'xzy' )
      distributed_field = .TRUE.
    CASE ( 'xyz' )
      distributed_field = .TRUE.
    CASE ( 'yxz' )
      distributed_field = .TRUE.
    CASE ( 'zxy' )
      distributed_field = .TRUE.
    CASE ( 'xy' )
      distributed_field = .TRUE.
    CASE ( 'yx' )
      distributed_field = .TRUE.
    CASE DEFAULT
      ! all other memory orders are replicated
      distributed_field = .FALSE.
  END SELECT

  IF ( distributed_field ) THEN
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
    IF ( FieldType .EQ. WRF_DOUBLE .OR. FieldType .EQ. WRF_REAL ) THEN

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
    ENDIF
  ELSE
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
    IF ( FieldType .EQ. WRF_DOUBLE .OR. FieldType .EQ. WRF_REAL) THEN
      CALL wrf_dm_bcast_bytes( Field , RWORDSIZE*memsize )
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
      CALL wrf_dm_bcast_bytes( Field , IWORDSIZE*memsize )
    ENDIF
  ENDIF


  RETURN
END SUBROUTINE call_pkg_and_dist_generic

!!!!!!  Miscellaneous routines

! stole these routines from io_netcdf external package; changed names to avoid collisions
SUBROUTINE dim_from_memorder(MemoryOrder,NDim)
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
  CHARACTER*(*) ,INTENT(IN)  :: MemoryOrder
  CHARACTER*(*) ,INTENT(OUT) :: MemOrd
!Local
  CHARACTER*1                :: c
  INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
  INTEGER                    :: i,n
!
  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  DO i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  ENDDO
  RETURN
END SUBROUTINE Lower_Case

LOGICAL FUNCTION has_char( str, c )
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


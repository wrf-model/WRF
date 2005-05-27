!--- open_for_write_begin
SUBROUTINE ext_esmf_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                          DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  ! Local declarations
  INTEGER i

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_update(i) = .false.
  opened_for_read(i) = .false.
  opened_for_write(i) = .true.
!$$$here...  rename "mcel_*"
  mcel_grid_defined(i) = .false.
  mcel_finalized(i) = .false.
  DataHandle = i

! right now only 2d -- extend later to 3d, 1d, 0d

!$$$here...  may want to handle masks this way...  
!  LANDMASK_I(i) = ""
!  CALL get_value( "LANDMASK_I", SysDepInfo, LANDMASK_I(i) )

! TBH:  recall this bit hacked out of module_io::wrf_open_for_read_begin()...  
! $$$ tstr = TRIM(SysDepInfo) // ',' // 'READ_MODE=UPDATE,LAT_R=XLAT,LON_R=XLONG,LANDMASK_I=LU_MASK,FILTER_HANDLE=' // TRIM(fhand)

!$$$ Create an empty exportState here

  Status = 0
  RETURN  
END SUBROUTINE ext_esmf_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_esmf_open_for_write_commit( DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
      ! I do not think we need this MCEL stuff for ESMF 
      ! okay_to_read( DataHandle ) = .true.
      opened_for_update( DataHandle ) = .false.
    ENDIF
  ENDIF

!$$$ Finish exportState here, if needed.

  Status = 0

  RETURN  
END SUBROUTINE ext_esmf_open_for_write_commit

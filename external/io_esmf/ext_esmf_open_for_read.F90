!--- open_for_read_begin
SUBROUTINE ext_esmf_open_for_read_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
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
  CHARACTER*80 read_mode

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_read(i) = .true.
  opened_for_write(i) = .false.
!$$$here...  rename "mcel_*"
  mcel_grid_defined(i) = .false.
  mcel_finalized(i) = .false.
  DataHandle = i
  ListOfFields(i) = " "

!$$$ This is a noop for now.  Users of importState can point to it directly for the point of use.

! recover the names of the strings that contain georeference and mask data, if avail
!$$$here...  may want to handle masks this way...
!  LANDMASK_I(i) = ""
!  CALL get_value( "LANDMASK_I", SysDepInfo, LANDMASK_I(i) )
  ! I do not think we need this MCEL stuff for ESMF -- there is no server to 
  ! send things to.  
  opened_for_update( i ) = .false.

  Status = 0
  RETURN  
END SUBROUTINE ext_esmf_open_for_read_begin

!--- open_for_read_commit
SUBROUTINE ext_esmf_open_for_read_commit( DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      if ( opened_for_update( DataHandle ) ) okay_to_write( DataHandle ) = .true.
      okay_to_read( DataHandle ) = .true.
    ENDIF
  ENDIF

  Status = 0

  RETURN  
END SUBROUTINE ext_esmf_open_for_read_commit

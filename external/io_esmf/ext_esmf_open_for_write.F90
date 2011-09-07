!--- open_for_write_begin
SUBROUTINE ext_esmf_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                          DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  CHARACTER*(*)               :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*)               :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  ! Local declarations
  INTEGER :: i
  TYPE(ESMF_State), POINTER :: exportstate
  TYPE(ESMF_StateIntent_Flag) :: stateintent
  INTEGER :: rc, itemCount

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_read(i) = .false.
  opened_for_write(i) = .true.
  DataHandle = i

  ! Grab the current exportState and ensure that it is empty
  CALL ESMF_ExportStateGetCurrent(exportstate, rc)
  IF ( rc /= ESMF_SUCCESS ) THEN
    CALL wrf_error_fatal("ext_esmf_open_for_write:  ESMF_ExportStateGetCurrent failed" )
  ENDIF
  ! For now, If the export state is not empty, whine and die.
!TODO:  Eventually, use nested states to allow more than one auxhist stream 
!TODO:  to be supported via ESMF.  
!TODO:  Eventually, get smart about interacting with "needed" and "optional" 
!TODO:  named state items
  CALL ESMF_StateGet( exportstate, itemCount=itemCount, &
                      stateintent=stateintent, rc=rc )
  IF ( rc /= ESMF_SUCCESS ) THEN
    CALL wrf_error_fatal("ext_esmf_open_for_write:  ESMF_ExportStateGet failed" )
  ENDIF
  IF ( stateintent /= ESMF_STATEINTENT_EXPORT ) THEN
    CALL wrf_error_fatal("ext_esmf_open_for_write:  not an export state" )
  ENDIF
  IF ( itemCount /= 0 ) THEN
    CALL wrf_error_fatal("ext_esmf_open_for_write:  export state not empty, io_esmf is currently limited to only one auxhist stream" )
  ENDIF

  Status = 0
  RETURN  
END SUBROUTINE ext_esmf_open_for_write_begin


!--- open_for_write_commit
SUBROUTINE ext_esmf_open_for_write_commit( DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER , INTENT(IN ) :: DataHandle
  INTEGER , INTENT(OUT) :: Status

  IF ( int_valid_handle( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
    ENDIF
  ENDIF

  Status = 0
  RETURN  
END SUBROUTINE ext_esmf_open_for_write_commit


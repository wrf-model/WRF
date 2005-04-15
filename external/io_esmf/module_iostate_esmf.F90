
! "iostate_esmf" allows the io_esmf external I/O package to define and store 
! package-specific state without adding package-specific dependencies 
! to WRF framework types defined in module domain.  

! This module works, but does nothing useful when the embedded ESMF 
! library in external/esmf_time_f90/ is used instead of io_esmf.  

! TBH:  $$$here...  May need to use "#ifndef F90_STANDALONE" to switch off 
! TBH:  $$$here...  bits of code when esmf_time_f90 is used.  


MODULE module_iostate_esmf

  USE ESMF_Mod

  IMPLICIT NONE

  PRIVATE


  ! public routines
  PUBLIC set_iostate_esmf
  PUBLIC get_iostate_esmf


  ! private parameters
  ! get MAX_DOMAINS from cpp token
  INTEGER, PARAMETER :: MAX_IOSTATES = MAX_DOMAINS_F

  ! private typedefs
  TYPE iostate_esmf
    INTEGER                         :: id          ! unique ID for this iostate
    TYPE(ESMF_GridComp) , POINTER   :: gcomp
    TYPE(ESMF_State)    , POINTER   :: importState
    TYPE(ESMF_State)    , POINTER   :: exportState
    TYPE(ESMF_Clock)    , POINTER   :: clock
  END TYPE iostate_esmf

  ! private declarations
  LOGICAL, SAVE :: need_init = .TRUE.
  TYPE(iostate_esmf) , DIMENSION(MAX_IOSTATES) :: esmf_iostates


CONTAINS


  ! Allocate space for esmf_iostates
  SUBROUTINE init_iostate_esmf
    INTEGER :: id, stat
    CHARACTER (256) :: msg

    ! allocate space for each element of esmf_iostates
! TBH:  $$$here...  Optimize later to only allocate elements that are 
! TBH:  $$$here...  actually used!
    DO id = 1, MAX_IOSTATES
      ALLOCATE( esmf_iostates( id )%gcomp,       &
                esmf_iostates( id )%importState, &
                esmf_iostates( id )%exportState, &
                esmf_iostates( id )%clock,       &
                STAT=stat )
      IF ( stat /= 0 ) THEN
        WRITE( msg,* ) 'Could not allocate esmf_iostates in ', &
                       __FILE__ ,                              &
                       ', line',                               &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      esmf_iostates(id)%id = id
    ENDDO
    need_init = .FALSE.
  END SUBROUTINE init_iostate_esmf


  ! use lazy initialization to avoid exporting one more interface
  SUBROUTINE check_init
    IF ( need_init ) THEN
      CALL init_iostate_esmf
    ENDIF
  END SUBROUTINE check_init


  ! range check on id
  SUBROUTINE check_id( id, line )
    INTEGER, INTENT(IN   ) :: id     ! index into esmf_iostates
    INTEGER, INTENT(IN   ) :: line   ! line number in this file
    CHARACTER (256) :: msg
    IF ( ( id <= 0 ) .OR. ( id > MAX_IOSTATES ) ) THEN
      WRITE( msg,* ) 'iostate id out of range in ', &
                     __FILE__ ,                     &
                     ', line',                      &
                     line
      CALL wrf_error_fatal ( msg )
    ENDIF
  END SUBROUTINE check_id


  ! Set values in a selected element of esmf_iostates
  SUBROUTINE set_iostate_esmf( id, gcomp, importState, exportState, clock )
    INTEGER,                       INTENT(IN   ) :: id  ! index to esmf_iostates
    TYPE(ESMF_GridComp), OPTIONAL, INTENT(IN   ) :: gcomp
    TYPE(ESMF_State),    OPTIONAL, INTENT(IN   ) :: importState, exportState
    TYPE(ESMF_Clock),    OPTIONAL, INTENT(IN   ) :: clock

    CALL check_init                ! lazy initialization
    CALL check_id( id, __LINE__ )  ! range check on id

    ! set values of selected iostate
    IF ( PRESENT( gcomp ) )       esmf_iostates( id )%gcomp       = gcomp
    IF ( PRESENT( importState ) ) esmf_iostates( id )%importState = importState
    IF ( PRESENT( exportState ) ) esmf_iostates( id )%exportState = exportState
    IF ( PRESENT( clock ) )       esmf_iostates( id )%clock       = clock

  END SUBROUTINE set_iostate_esmf


  ! Get values from a selected element of esmf_iostates
  SUBROUTINE get_iostate_esmf( id, gcomp, importState, exportState, clock )
    INTEGER,                       INTENT(IN   ) :: id  ! index to esmf_iostates
    TYPE(ESMF_GridComp), OPTIONAL, INTENT(  OUT) :: gcomp
    TYPE(ESMF_State),    OPTIONAL, INTENT(  OUT) :: importState, exportState
    TYPE(ESMF_Clock),    OPTIONAL, INTENT(  OUT) :: clock

    CALL check_init                ! lazy initialization
    CALL check_id( id, __LINE__ )  ! range check on id

    ! get values of selected iostate
    IF ( PRESENT( gcomp ) )       gcomp       = esmf_iostates( id )%gcomp
    IF ( PRESENT( importState ) ) importState = esmf_iostates( id )%importState
    IF ( PRESENT( exportState ) ) exportState = esmf_iostates( id )%exportState
    IF ( PRESENT( clock ) )       clock       = esmf_iostates( id )%clock

  END SUBROUTINE get_iostate_esmf


END MODULE module_iostate_esmf


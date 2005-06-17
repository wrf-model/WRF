
! "module_esmf_extensions" is responsible for yet-to-be-implemented ESMF 
! features used by the io_esmf package.  Once ESMF development is complete, 
! this module may be removed.  

! NOTE for implementation of ESMF_*GetCurrent():  
!
! This implementation uses interfaces that pass Fortran POINTERs around 
! to avoid forcing use of overloaded assignment operators for shallow 
! copies.  The goal of this approach is to be as insulated as possible 
! from ESMF object implementations.  This avoids having to explicitly 
! copy-in *AND* copy-out through the standard component init(), run(), 
! and final() interfaces just to attach references to ESMF objects to 
! other objects.  The explicit CICO *might* be required if we 
! instead attached shallow copies of the objects to other objects!  
! "Might" means it is not required now because ESMF objects are 
! implemented as simple pointers.  However, Nancy Collins says that 
! the ESMF core team plans to add more state on the Fortran side of the 
! ESMF objects, so copy-out will eventually be required.  Thus we use 
! POINTERs to attach references, as in other languages.  Why ESMF 
! component interfaces aren't passing POINTERs to Fortran objects is 
! not clear (TBH)...  
!

MODULE module_esmf_extensions

  USE ESMF_Mod

  IMPLICIT NONE

  PRIVATE


  ! private data

  ! Data for ESMF_*GetCurrent()
  ! These flags are set to .TRUE. iff current objects are valid.  
  LOGICAL, SAVE                :: current_clock_valid = .FALSE.
  TYPE(ESMF_Clock), POINTER    :: current_clock
  LOGICAL, SAVE                :: current_importstate_valid = .FALSE.
  TYPE(ESMF_State), POINTER    :: current_importstate
  LOGICAL, SAVE                :: current_exportstate_valid = .FALSE.
  TYPE(ESMF_State), POINTER    :: current_exportstate
  LOGICAL, SAVE                :: current_gridcomp_valid = .FALSE.
  TYPE(ESMF_GridComp), POINTER :: current_gridcomp

  ! Flag for "is-initialized" inquiry
  ! NOTE:  esmf_is_initialized is not reset to .FALSE. when ESMF_Finalize is called
  LOGICAL, SAVE                :: esmf_is_initialized = .FALSE.


  ! public routines
  ! These convenience interfaces have been proposed to the ESMF core team.  
  ! "get current" variants
  PUBLIC ESMF_ClockGetCurrent
  PUBLIC ESMF_ImportStateGetCurrent
  PUBLIC ESMF_ExportStateGetCurrent
  PUBLIC ESMF_GridCompGetCurrent
  ! "is-initialized" inquiry
  PUBLIC WRF_UTIL_IsInitialized

  ! public routines to be replaced by ESMF internal implementations
  ! These interfaces will not be public because ESMF will always be able 
  ! to call them in the right places without user intervention.  
  ! "get current" variants
  PUBLIC ESMF_ClockSetCurrent
  PUBLIC ESMF_ImportStateSetCurrent
  PUBLIC ESMF_ExportStateSetCurrent
  PUBLIC ESMF_GridCompSetCurrent
  PUBLIC ESMF_SetCurrent
  ! "is-initialized" inquiry
  PUBLIC ESMF_SetInitialized


CONTAINS


! Add "is initialized" behavior to ESMF interface
  FUNCTION WRF_UTIL_IsInitialized()
    LOGICAL WRF_UTIL_IsInitialized
    WRF_UTIL_IsInitialized = esmf_is_initialized
  END FUNCTION WRF_UTIL_IsInitialized

! Add "is initialized" behavior to ESMF interface
! This interface will go away as it will be done inside ESMF_Initialize().  
  SUBROUTINE ESMF_SetInitialized()
    esmf_is_initialized = .TRUE.
  END SUBROUTINE ESMF_SetInitialized



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrent - Get current ESMF_Clock
! !INTERFACE:
  SUBROUTINE ESMF_ClockGetCurrent(clock, rc)
! !ARGUMENTS:
    TYPE(ESMF_Clock), POINTER      :: clock
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_Clock} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[clock]
!     Upon return this holds the {\tt ESMF\_Clock} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_clock_valid ) THEN
      clock => current_clock
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ClockGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ImportStateGetCurrent - Get current import ESMF_State
! !INTERFACE:
  SUBROUTINE ESMF_ImportStateGetCurrent(importstate, rc)
! !ARGUMENTS:
    TYPE(ESMF_State), POINTER      :: importstate
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the import {\tt ESMF\_State} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[importstate]
!     Upon return this holds the import {\tt ESMF\_State} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_importstate_valid ) THEN
      importstate => current_importstate
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ImportStateGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ExportStateGetCurrent - Get current export ESMF_State
! !INTERFACE:
  SUBROUTINE ESMF_ExportStateGetCurrent(exportstate, rc)
! !ARGUMENTS:
    TYPE(ESMF_State), POINTER      :: exportstate
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the export {\tt ESMF\_State} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[exportstate]
!     Upon return this holds the export {\tt ESMF\_State} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_exportstate_valid ) THEN
      exportstate => current_exportstate
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ExportStateGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGetCurrent - Get current ESMF_GridComp
! !INTERFACE:
  SUBROUTINE ESMF_GridCompGetCurrent(gridcomp, rc)
! !ARGUMENTS:
    TYPE(ESMF_GridComp), POINTER   :: gridcomp
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_GridComp} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[gridcomp]
!     Upon return this holds the {\tt ESMF\_GridComp} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_gridcomp_valid ) THEN
      gridcomp => current_gridcomp
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_GridCompGetCurrent
!------------------------------------------------------------------------------




! Temporary method, to be replaced by ESMF internal implementation
! Sets the current ESMF_Clock to clock.  
  SUBROUTINE ESMF_ClockSetCurrent(clock)
    TYPE(ESMF_Clock), POINTER :: clock
    current_clock => clock
    current_clock_valid = .TRUE.
  END SUBROUTINE ESMF_ClockSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current import ESMF_State to importstate.  
  SUBROUTINE ESMF_ImportStateSetCurrent(importstate)
    TYPE(ESMF_State), POINTER :: importstate
    current_importstate => importstate
    current_importstate_valid = .TRUE.
  END SUBROUTINE ESMF_ImportStateSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current export ESMF_State to exportstate.  
  SUBROUTINE ESMF_ExportStateSetCurrent(exportstate)
    TYPE(ESMF_State), POINTER :: exportstate
    current_exportstate => exportstate
    current_exportstate_valid = .TRUE.
  END SUBROUTINE ESMF_ExportStateSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current ESMF_GridComp to gridcomp.  
  SUBROUTINE ESMF_GridCompSetCurrent(gridcomp)
    TYPE(ESMF_GridComp), POINTER :: gridcomp
    current_gridcomp => gridcomp
    current_gridcomp_valid = .TRUE.
  END SUBROUTINE ESMF_GridCompSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Convenience interface to set everything at once...  
  ! This routine sets the current ESMF_GridComp, import and export
  ! ESMF_States, and the current ESMF_Clock.
  ! NOTE:  It will be possible to remove this routine once ESMF supports
  !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(),
  !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().
  SUBROUTINE ESMF_SetCurrent( gcomp, importState, exportState, clock )
    TYPE(ESMF_GridComp), OPTIONAL, POINTER :: gcomp
    TYPE(ESMF_State),    OPTIONAL, POINTER :: importState
    TYPE(ESMF_State),    OPTIONAL, POINTER :: exportState
    TYPE(ESMF_Clock),    OPTIONAL, POINTER :: clock
    IF ( PRESENT( gcomp ) ) THEN
      CALL ESMF_GridCompSetCurrent( gcomp )
      CALL ESMF_ImportStateSetCurrent( importState )
      CALL ESMF_ExportStateSetCurrent( exportState )
      CALL ESMF_ClockSetCurrent( clock )
    ENDIF
  END SUBROUTINE ESMF_SetCurrent
!------------------------------------------------------------------------------


END MODULE module_esmf_extensions


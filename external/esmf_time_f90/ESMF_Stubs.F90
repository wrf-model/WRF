! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer ESMF interface features without necessarily implementing 
! them.  

MODULE ESMF_Stubs

   IMPLICIT NONE

! Bogus typedefs
   TYPE ESMF_GridComp
      SEQUENCE
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_State
      SEQUENCE
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_VM
      SEQUENCE
      INTEGER :: dummy
   END TYPE


CONTAINS


! NOOP
   SUBROUTINE ESMF_Initialize( vm, defaultCalendar, rc )
      USE esmf_basemod
      USE esmf_calendarmod
      TYPE(ESMF_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(ESMF_CalendarType), INTENT(IN   ), OPTIONAL :: defaultCalendar
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_Initialize


! NOOP
   SUBROUTINE ESMF_Finalize( rc )
      USE esmf_basemod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_Finalize


END MODULE ESMF_Stubs



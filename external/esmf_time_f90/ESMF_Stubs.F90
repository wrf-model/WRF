! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer ESMF interface features without necessarily implementing 
! them.  

MODULE ESMF_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE ESMF_Grid
      SEQUENCE
      INTEGER :: dummy
   END TYPE

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

   TYPE ESMF_MsgType
      SEQUENCE
      INTEGER :: mtype
   END TYPE
   TYPE(ESMF_MsgType), PARAMETER  ::      &
      ESMF_LOG_INFO  =   ESMF_MsgType(1), &
      ESMF_LOG_WARNING = ESMF_MsgType(2), &
      ESMF_LOG_ERROR =   ESMF_MsgType(3)

   TYPE ESMF_LOG
      SEQUENCE
      INTEGER :: dummy
   END TYPE


   PUBLIC ESMF_Grid, ESMF_GridComp, ESMF_State, ESMF_VM
   PUBLIC ESMF_Initialize, ESMF_Finalize, ESMF_IsInitialized
   PUBLIC ESMF_LogWrite, ESMF_LOG, ESMF_MsgType
   PUBLIC ESMF_LOG_INFO, ESMF_LOG_WARNING, ESMF_LOG_ERROR

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


! NOOP for "is initialized" extension
   FUNCTION ESMF_IsInitialized()
      LOGICAL ESMF_IsInitialized
      ESMF_IsInitialized = .TRUE.
   END FUNCTION ESMF_IsInitialized


! NOOP
   SUBROUTINE ESMF_Finalize( rc )
      USE esmf_basemod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_Finalize

! NOOP
   SUBROUTINE ESMF_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE esmf_basemod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(ESMF_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(ESMF_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_LogWrite


END MODULE ESMF_Stubs



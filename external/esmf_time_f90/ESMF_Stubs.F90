! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer ESMF interface features without necessarily implementing 
! them.  

MODULE WRF_ESMF_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE ESMF_Grid
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_GridComp
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_State
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_VM
      INTEGER :: dummy
   END TYPE

   TYPE ESMF_MsgType
      INTEGER :: mtype
   END TYPE
   TYPE(ESMF_MsgType), PARAMETER  ::      &
      ESMF_LOG_INFO  =   ESMF_MsgType(1), &
      ESMF_LOG_WARNING = ESMF_MsgType(2), &
      ESMF_LOG_ERROR =   ESMF_MsgType(3)

   TYPE ESMF_LOG
      INTEGER :: dummy
   END TYPE

   LOGICAL, private, save :: initialized = .false.

   PUBLIC ESMF_Grid, ESMF_GridComp, ESMF_State, ESMF_VM
   PUBLIC ESMF_Initialize, ESMF_Finalize, ESMF_IsInitialized
   PUBLIC ESMF_LogWrite, ESMF_LOG, ESMF_MsgType
   PUBLIC ESMF_LOG_INFO, ESMF_LOG_WARNING, ESMF_LOG_ERROR

CONTAINS


! NOOP
   SUBROUTINE ESMF_Initialize( vm, defaultcalkind, rc )
      USE WRF_ESMF_BaseMod
      USE WRF_ESMF_CalendarMod
      TYPE(ESMF_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(ESMF_CalendarType), INTENT(IN   ), OPTIONAL :: defaultcalkind
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc

      TYPE(ESMF_CalendarType) :: defaultCalType
      INTEGER :: status

      IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ! Initialize the default time manager calendar
      IF ( PRESENT(defaultcalkind) )THEN
         defaultCalType = defaultcalkind
      ELSE
         defaultCalType = ESMF_CAL_NOLEAP
      END IF
      allocate( defaultCal )
      defaultCal = ESMF_CalendarCreate( calendarType=defaultCalType, &
                        rc=status)

      ! initialize tables in time manager
      CALL initdaym

      IF (status .ne. ESMF_SUCCESS) THEN
          PRINT *, "Error initializing the default time manager calendar"
          RETURN
      END IF
      initialized = .true.

      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_Initialize


   FUNCTION ESMF_IsInitialized()
      LOGICAL ESMF_IsInitialized
      ESMF_IsInitialized = initialized
   END FUNCTION ESMF_IsInitialized


! NOOP
   SUBROUTINE ESMF_Finalize( rc )
      USE WRF_ESMF_BaseMod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
#if (defined SPMD) || (defined COUP_CSM)
#include <mpif.h>
#endif
      LOGICAL :: flag
      INTEGER :: ier

      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
#if (defined SPMD) || (defined COUP_CSM)
      CALL MPI_Finalized( flag, ier )
      IF ( ier .ne. mpi_success )THEN
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      END IF
      IF ( .NOT. flag ) THEN
        CALL MPI_Finalize( ier ) 
        IF ( ier .ne. mpi_success )THEN
          IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
        END IF
      END IF
#endif
   END SUBROUTINE ESMF_Finalize

! NOOP
   SUBROUTINE ESMF_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE WRF_ESMF_BaseMod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(ESMF_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(ESMF_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
   END SUBROUTINE ESMF_LogWrite


END MODULE WRF_ESMF_Stubs



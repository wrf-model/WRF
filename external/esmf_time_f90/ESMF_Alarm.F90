!
!==============================================================================
!
!     ESMF Alarm Module
      module WRF_ESMF_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: WRF_ESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use WRF_ESMF_BaseMod

      ! associated derived types
      use WRF_ESMF_TimeIntervalMod, only : ESMF_TimeInterval, &
                                           ESMF_TimeIntervalAbsValue
      use WRF_ESMF_TimeMod,         only : ESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for ESMF_Alarm
      type ESMF_AlarmInt
        type(ESMF_TimeInterval) :: RingInterval
        type(ESMF_Time)  :: RingTime
        type(ESMF_Time)  :: PrevRingTime
        type(ESMF_Time)  :: StopTime
        integer :: ID
        integer :: AlarmMutex
        logical :: Ringing
        logical :: Enabled
        logical :: RingTimeSet
        logical :: RingIntervalSet
        logical :: StopTimeSet
      end type

! Actual public type:  this bit allows easy mimic of "deep" ESMF_AlarmCreate
! in ESMF 2.1.0+.  Note that ESMF_AlarmCreate is in a separate module to avoid 
! cyclic dependence.  
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF 
!        shallow-copy-masquerading-as-reference-copy insanity.  
      type ESMF_Alarm
        type(ESMF_AlarmInt), pointer :: alarmint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
      public ESMF_AlarmInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmDestroy
      public ESMF_AlarmSet
      public ESMF_AlarmGet
!      public ESMF_AlarmGetRingInterval
!      public ESMF_AlarmSetRingInterval
!      public ESMF_AlarmGetRingTime
!      public ESMF_AlarmSetRingTime
!      public ESMF_AlarmGetPrevRingTime
!      public ESMF_AlarmSetPrevRingTime
!      public ESMF_AlarmGetStopTime
!      public ESMF_AlarmSetStopTime
      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmRingerOn
      public ESMF_AlarmRingerOff
      public ESMF_AlarmIsRinging
!      public ESMF_AlarmCheckRingTime
      public operator(==)
 
! Required inherited and overridden ESMF_Base class methods

!      public ESMF_AlarmRead
!      public ESMF_AlarmWrite
      public ESMF_AlarmValidate
      public ESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmEQ

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt ESMF\_Alarm} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine ESMF_AlarmSet(alarm, RingTime, RingInterval, PrevRingTime, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      type(ESMF_Time), intent(in), optional :: RingTime, PrevRingTime
      type(ESMF_TimeInterval), intent(in), optional :: RingInterval
      type(ESMF_Time), intent(in), optional :: StopTime
      logical, intent(in), optional :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%RingTimeSet = .FALSE.
        alarm%alarmint%RingIntervalSet = .FALSE.
        alarm%alarmint%StopTimeSet = .FALSE.
        IF ( PRESENT( RingInterval ) ) THEN
          ! force RingInterval to be positive
          alarm%alarmint%RingInterval = &
            ESMF_TimeIntervalAbsValue( RingInterval )
          alarm%alarmint%RingIntervalSet = .TRUE.
        ENDIF
        IF ( PRESENT( PrevRingTime ) ) THEN
          alarm%alarmint%PrevRingTime = PrevRingTime
        ENDIF
        IF ( PRESENT( RingTime ) ) THEN
          alarm%alarmint%RingTime = RingTime
          alarm%alarmint%RingTimeSet = .TRUE.
        ENDIF
        IF ( PRESENT( StopTime ) ) THEN
          alarm%alarmint%StopTime = StopTime
          alarm%alarmint%StopTimeSet = .TRUE.
        ENDIF
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( Enabled ) ) THEN
          alarm%alarmint%Enabled = Enabled
        ENDIF
        IF ( PRESENT( rc ) ) THEN
          rc = ESMF_SUCCESS
        ENDIF
        alarm%alarmint%Ringing = .FALSE.
        alarm%alarmint%Enabled = .TRUE.
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF

      end subroutine ESMF_AlarmSet



! Deallocate memory for ESMF_Alarm
      SUBROUTINE ESMF_AlarmDestroy( alarm, rc )
         TYPE(ESMF_Alarm), INTENT(INOUT) :: alarm
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           DEALLOCATE( alarm%alarmint )
         ENDIF
         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      END SUBROUTINE ESMF_AlarmDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
      RingInterval = alarm%alarmint%RingInterval

      end subroutine ESMF_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmSetRingInterval not supported' )
      end subroutine ESMF_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmGetRingTime not supported' )
      end subroutine ESMF_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmSetRingTime not supported' )
      end subroutine ESMF_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGet - Get an alarm's parameters -- compatibility with ESMF 2.0.1
!
! !INTERFACE:
      subroutine ESMF_AlarmGet(alarm, PrevRingTime, RingInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out), optional :: PrevRingTime
      type(ESMF_TimeInterval), intent(out), optional :: RingInterval
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      ierr = ESMF_SUCCESS

      IF ( PRESENT(PrevRingTime) ) THEN
        CALL ESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingInterval) ) THEN
        CALL ESMF_AlarmGetRingInterval(alarm, RingInterval, rc=ierr)
      ENDIF

      IF ( PRESENT(rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine ESMF_AlarmGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        PrevRingTime = alarm%alarmint%PrevRingTime
        IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF
      end subroutine ESMF_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmSetPrevRingTime not supported' )
      end subroutine ESMF_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmGetStopTime not supported' )
      end subroutine ESMF_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmSetStopTime not supported' )
      end subroutine ESMF_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt ESMF\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF
      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .FALSE.
        IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF
      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmRingerOn - Turn on an alarm


! !INTERFACE:
      subroutine ESMF_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt ESMF\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          alarm%alarmint%Ringing = .TRUE.
          IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
        ELSE
          alarm%alarmint%Ringing = .FALSE.
          IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF

      end subroutine ESMF_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an alarm

! !INTERFACE:
      subroutine ESMF_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Ringing = .FALSE.
        IF ( alarm%alarmint%Enabled ) THEN
          IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
        ELSE
          IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF
      end subroutine ESMF_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function ESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          ESMF_AlarmIsRinging = alarm%alarmint%Ringing
          IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
        ELSE
          ESMF_AlarmIsRinging = .FALSE.
          IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
      ENDIF
      end function ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function ESMF_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt ESMF\_Clock} to check whether to trigger
!     the {\tt ESMF\_Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmCheckRingTime not supported' )
      ESMF_AlarmCheckRingTime = .FALSE.  ! keep compilers happy
      end function ESMF_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
      function ESMF_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmEQ

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm1
      type(ESMF_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmEQ not supported ' )
      ESMF_AlarmEQ = .FALSE.       ! keep compilers happy
      end function ESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmRead - restores an alarm

! !INTERFACE:
      subroutine ESMF_AlarmRead(alarm, RingInterval, RingTime, &
                           PrevRingTime, StopTime, Ringing, &
                           Enabled, ID, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in) :: RingInterval
      type(ESMF_Time), intent(in) :: RingTime
      type(ESMF_Time), intent(in) :: PrevRingTime
      type(ESMF_Time), intent(in) :: StopTime
      logical, intent(in) :: Ringing
      logical, intent(in) :: Enabled
      integer, intent(in) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmRead not supported' )
      end subroutine ESMF_AlarmRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmWrite - saves an alarm

! !INTERFACE:
      subroutine ESMF_AlarmWrite(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: RingInterval
      type(ESMF_Time), intent(out) :: RingTime
      type(ESMF_Time), intent(out) :: PrevRingTime
      type(ESMF_Time), intent(out) :: StopTime
      logical, intent(out) :: Ringing
      logical, intent(out) :: Enabled
      integer, intent(out) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmWrite not supported' )
      end subroutine ESMF_AlarmWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmValidate not supported' )
      end subroutine ESMF_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMF_AlarmPrint not supported' )
      end subroutine ESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module WRF_ESMF_AlarmMod

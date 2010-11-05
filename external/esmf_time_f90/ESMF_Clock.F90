!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
!==============================================================================
!
!     ESMF Clock Module
      module ESMF_ClockMod
!     
!==============================================================================
!     
! This file contains the Clock class definition and all Clock class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc> 

!==============================================================================
!BOPI
! !MODULE: ESMF_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!     
! See {\tt ../include/ESMC\_Clock.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod   ! , only : ESMF_TimeInterval, &
                                 !          ESMF_TimeIntervalIsPositive
      use ESMF_TimeMod           ! , only : ESMF_Time
      use ESMF_AlarmMod,        only : ESMF_Alarm

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock
!     
!     ! F90 class type to match C++ Clock class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for ESMF_Clock
      type ESMF_ClockInt
        type(ESMF_TimeInterval) :: TimeStep
        type(ESMF_Time)  :: StartTime
        type(ESMF_Time)  :: StopTime
        type(ESMF_Time)  :: RefTime
        type(ESMF_Time)  :: CurrTime
        type(ESMF_Time)  :: PrevTime
        integer(ESMF_KIND_I8) :: AdvanceCount
        integer :: ClockMutex
        integer :: NumAlarms
        ! Note:  to mimic ESMF 2.1.0+, AlarmList is maintained 
        ! within ESMF_Clock even though copies of each alarm are 
        ! returned from ESMF_AlarmCreate() at the same time they 
        ! are copied into the AlarmList!  This duplication is not 
        ! as hideous as it might be because the ESMF_Alarm type 
        ! has data members that are all POINTERs (thus the horrible 
        ! shallow-copy-masquerading-as-reference-copy hack works).  
        type(ESMF_Alarm), pointer, dimension(:) :: AlarmList
      end type

! Actual public type:  this bit allows easy mimic of "deep" ESMF_ClockCreate 
! in ESMF 2.1.0+
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF 
!        shallow-copy-masquerading-as-reference-copy.  
      type ESMF_Clock
        type(ESMF_ClockInt), pointer  :: clockint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Clock
      public ESMF_ClockInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_ClockCreate
      public ESMF_ClockDestroy
      public ESMF_ClockSet
!      public ESMF_ClockSetOLD
      public ESMF_ClockGet
!      public ESMF_ClockGetAdvanceCount
!      public ESMF_ClockGetTimeStep
!      public ESMF_ClockSetTimeStep
!      public ESMF_ClockGetCurrTime
!      public ESMF_ClockSetCurrTime
!      public ESMF_ClockGetStartTime
!      public ESMF_ClockGetStopTime
!      public ESMF_ClockGetRefTime
!      public ESMF_ClockGetPrevTime
!      public ESMF_ClockGetCurrSimTime
!      public ESMF_ClockGetPrevSimTime
! This must be public for ESMF_AlarmClockMod...  
      public ESMF_ClockAddAlarm
      public ESMF_ClockGetAlarmList
!      public ESMF_ClockGetNumAlarms
!      public ESMF_ClockSyncToWallClock
      public ESMF_ClockAdvance
      public ESMF_ClockIsStopTime
      public ESMF_ClockStopTimeDisable

! Required inherited and overridden ESMF_Base class methods

!      public ESMF_ClockRead
!      public ESMF_ClockWrite
      public ESMF_ClockValidate
      public ESMF_ClockPrint
!EOPI

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetOLD - Initialize a clockint

! !INTERFACE:
      subroutine ESMF_ClockSetOLD(clockint, TimeStep, StartTime, &
                                  StopTime, RefTime, rc)

! !ARGUMENTS:
      type(ESMF_ClockInt), intent(out) :: clockint
      type(ESMF_TimeInterval), intent(in), optional :: TimeStep
      type(ESMF_Time), intent(in) :: StartTime
      type(ESMF_Time), intent(in) :: StopTime
      type(ESMF_Time), intent(in), optional :: RefTime
      integer, intent(out), optional :: rc
! Local
      integer i
    
! !DESCRIPTION:
!     Initialize an {\tt ESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clockint]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      IF ( PRESENT(TimeStep) ) clockint%TimeStep = TimeStep
      IF ( PRESENT(RefTime) )THEN
         clockint%RefTime = RefTime
      ELSE
         clockint%RefTime = StartTime
      END IF
      clockint%CurrTime = StartTime
      clockint%StartTime = StartTime
      clockint%StopTime = StopTime
      clockint%NumAlarms = 0
      clockint%AdvanceCount = 0
      ALLOCATE(clockint%AlarmList(MAX_ALARMS))
      ! TBH:  This incredible hack can be removed once ESMF_*Validate() 
      ! TBH:  can tell if a deep ESMF_* was created or not.  
      DO i = 1, MAX_ALARMS
        NULLIFY( clockint%AlarmList( i )%alarmint )
      ENDDO
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockSetOLD


! !IROUTINE: ESMF_ClockSet - Set clock properties -- for compatibility with ESMF 2.0.1

! !INTERFACE:
      subroutine ESMF_ClockSet(clock, TimeStep, StartTime, StopTime, &
                               RefTime, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(in), optional :: TimeStep
      type(ESMF_Time), intent(in), optional :: StartTime
      type(ESMF_Time), intent(in), optional :: StopTime
      type(ESMF_Time), intent(in), optional :: RefTime
      type(ESMF_Time), intent(in), optional :: CurrTime
      integer, intent(out), optional :: rc
! Local
      integer ierr
    
! !DESCRIPTION:
!     Initialize an {\tt ESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      ierr = ESMF_SUCCESS
      IF ( PRESENT(TimeStep) ) THEN
        CALL ESMF_ClockSetTimeStep ( clock, TimeStep, rc=ierr )
      ENDIF
      IF ( PRESENT(RefTime) ) clock%clockint%RefTime = RefTime
      IF ( PRESENT(StartTime) ) clock%clockint%StartTime = StartTime
      IF ( PRESENT(StopTime) ) clock%clockint%StopTime = StopTime
      IF ( PRESENT(CurrTime) ) THEN
        CALL ESMF_ClockSetCurrTime(clock, CurrTime, rc=ierr)
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr

      end subroutine ESMF_ClockSet


! Create ESMF_Clock using ESMF 2.1.0+ semantics
      FUNCTION ESMF_ClockCreate( name, TimeStep, StartTime, StopTime, &
                                 RefTime, rc )
        ! return value
        type(ESMF_Clock) :: ESMF_ClockCreate
        ! !ARGUMENTS:
        character (len=*),       intent(in),  optional :: name
        type(ESMF_TimeInterval), intent(in), optional :: TimeStep
        type(ESMF_Time), intent(in) :: StartTime
        type(ESMF_Time), intent(in) :: StopTime
        type(ESMF_Time), intent(in), optional :: RefTime
        integer, intent(out), optional :: rc
        ! locals
        type(ESMF_Clock) :: clocktmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( clocktmp%clockint )
        CALL ESMF_ClockSetOLD( clocktmp%clockint,   &
                               TimeStep= TimeStep,  &
                               StartTime=StartTime, &
                               StopTime= StopTime,  &
                               RefTime=RefTime, rc=rc )
        ESMF_ClockCreate = clocktmp
      END FUNCTION ESMF_ClockCreate


! Deallocate memory for ESMF_Clock
      SUBROUTINE ESMF_ClockDestroy( clock, rc )
         TYPE(ESMF_Clock), INTENT(INOUT) :: clock
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         ! TBH:  ignore deallocate errors, for now
         DEALLOCATE( clock%clockint%AlarmList )
         DEALLOCATE( clock%clockint )
         IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      END SUBROUTINE ESMF_ClockDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGet - Get clock properties -- for compatibility with ESMF 2.0.1 

! !INTERFACE:
      subroutine ESMF_ClockGet(clock, StartTime, CurrTime,       &
                               AdvanceCount, StopTime, TimeStep, &
                               PrevTime, RefTime, &
                               rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out), optional :: StartTime
      type(ESMF_Time), intent(out), optional :: CurrTime
      type(ESMF_Time), intent(out), optional :: StopTime
      type(ESMF_Time), intent(out), optional :: PrevTime
      type(ESMF_Time), intent(out), optional :: RefTime
      integer(ESMF_KIND_I8), intent(out), optional :: AdvanceCount
      type(ESMF_TimeInterval), intent(out), optional :: TimeStep
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Returns the number of times the {\tt ESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[StartTime]
!          The start time
!     \item[CurrTime]
!          The current time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[{[TimeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[{[PrevTime]}]
!          The {\tt ESMF\_Clock}'s previous current time
!     \item[{[PrevTime]}]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP
      ierr = ESMF_SUCCESS

      IF ( PRESENT (StartTime) ) THEN
        CALL ESMF_ClockGetStartTime( clock, StartTime=StartTime, rc=ierr )
      ENDIF
      IF ( PRESENT (CurrTime) ) THEN
        CALL ESMF_ClockGetCurrTime( clock , CurrTime, ierr )
      ENDIF
      IF ( PRESENT (StopTime) ) THEN
        CALL ESMF_ClockGetStopTime( clock , StopTime, ierr )
      ENDIF
      IF ( PRESENT (AdvanceCount) ) THEN
        CALL ESMF_ClockGetAdvanceCount(clock, AdvanceCount, ierr)
      ENDIF
      IF ( PRESENT (TimeStep) ) THEN
        CALL ESMF_ClockGetTimeStep(clock, TimeStep, ierr)
      ENDIF
      IF ( PRESENT (PrevTime) ) THEN
        CALL ESMF_ClockGetPrevTime(clock, PrevTime, ierr)
      ENDIF
      IF ( PRESENT (RefTime) ) THEN
        CALL ESMF_ClockGetRefTime(clock, RefTime, ierr)
      ENDIF

      IF ( PRESENT (rc) ) THEN
        rc = ierr
      ENDIF
    
      end subroutine ESMF_ClockGet


! !IROUTINE: ESMF_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine ESMF_ClockGetAdvanceCount(clock, AdvanceCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer(ESMF_KIND_I8), intent(out) :: AdvanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt ESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP

      AdvanceCount = clock%clockint%AdvanceCount

      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockGetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: TimeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.2
!EOP

      TimeStep = clock%clockint%TimeStep
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockSetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(ESMF_TimeInterval), intent(in) :: TimeStep
      integer, intent(out), optional      :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.2
!EOP

      clock%clockint%TimeStep = TimeStep
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

      end subroutine ESMF_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current time     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

      CurrTime = clock%clockint%CurrTime
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(ESMF_Time), intent(in) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.3
!EOP

      clock%clockint%CurrTime = CurrTime
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine ESMF_ClockGetStartTime(clock, StartTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: StartTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s start time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from
!     \item[StartTime]
!          The start time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StartTime = clock%clockint%StartTime
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s stop time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from
!     \item[StopTime]
!          The stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StopTime = clock%clockint%StopTime
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine ESMF_ClockGetRefTime(clock, RefTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: RefTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s reference time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from
!     \item[RefTime]
!          The reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP
      refTime = clock%clockint%RefTime
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevTime(clock, PrevTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: PrevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from
!     \item[PrevTime]
!          The previous current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

! hack for bug in PGI 5.1-x
!      prevTime = Clock%clockint%CurrTime - Clock%clockint%TimeStep
      prevTime = ESMF_TimeDec( Clock%clockint%CurrTime, &
                               Clock%clockint%TimeStep )

      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrSimTime(clock, CurrSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: CurrSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current simulation time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from
!     \item[CurrSimTime]
!          The current simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockGetCurrSimTime not supported' )
      end subroutine ESMF_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevSimTime(clock, PrevSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: PrevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from
!     \item[PrevSimTime]
!          The previous simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockGetPrevSimTime not supported' )
      end subroutine ESMF_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), intent(inout) :: Alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt ESMF\_Alarm} to an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt ESMF\_Alarm} to
!     \item[Alarm]
!          The {\tt ESMF\_Alarm} to add to the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
!EOP
    
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
      clock%clockint%NumAlarms = clock%clockint%NumAlarms + 1
      IF ( clock%clockint%NumAlarms > SIZE (clock%clockint%AlarmList) ) THEN
        CALL wrf_error_fatal ( 'ESMF_ClockAddAlarm:  too many alarms' )
      ELSE IF ( .NOT. ASSOCIATED( Alarm%alarmint ) ) THEN
        CALL wrf_error_fatal ( &
               'ESMF_ClockAddAlarm:  alarm not created' )
      ELSE
        IF ( Alarm%alarmint%RingTimeSet ) THEN
           Alarm%alarmint%PrevRingTime = Alarm%alarmint%RingTime
        ELSE
!TBH:  This has the nasty side-effect of forcing us to explicitly turn on 
!TBH:  alarms that are created with RingInterval only, if we want them to start 
!TBH:  ringing right away.  And this is done (see 
!TBH:  COMPUTE_VORTEX_CENTER_ALARM).  Straighten this out...  
           Alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
        ENDIF
        Alarm%alarmint%Ringing = .FALSE.

        ! finally, load the alarm into the list
! write(0,*)'ESMF_ClockAddAlarm ',clock%clockint%NumAlarms
        clock%clockint%AlarmList(clock%clockint%NumAlarms) = Alarm
      ENDIF
    
      end subroutine ESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Alarm), pointer :: AlarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} list from
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      AlarmList => clock%clockint%AlarmList
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt ESMF\_Alarm}s in an {\tt ESMF\_Clock}'s
!       {\tt ESMF\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt ESMF\_Alarm}s from
!     \item[NumAlarms]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!            {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      NumAlarms = clock%clockint%NumAlarms
      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToWallClock - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToWallClock(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time to wall clock time     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG3.4.5
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockSyncToWallClock not supported' )
      end subroutine ESMF_ClockSyncToWallClock

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, RingingAlarmList, &
                                   NumRingingAlarms, rc)

use esmf_timemod

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
      integer, intent(out), optional :: NumRingingAlarms
      integer, intent(out), optional :: rc
! Local
      logical pred1, pred2, pred3
      integer i, n
      type(ESMF_Alarm) :: alarm
      logical :: positive_timestep
!   
! !DESCRIPTION:
!     Advance an {\tt ESMF\_Clock}'s current time by one time step
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance
!     \item[{[RingingAlarmList]}]
!          Return a list of any ringing alarms after the time step
!     \item[{[NumRingingAlarms]}]
!          The number of ringing alarms returned
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
! !REQUIREMENTS:
!     TMG3.4.1
!EOP
! hack for bug in PGI 5.1-x
!      clock%clockint%CurrTime = clock%clockint%CurrTime + &
!                                clock%clockint%TimeStep
      clock%clockint%CurrTime = ESMF_TimeInc( clock%clockint%CurrTime, &
                                              clock%clockint%TimeStep )
      positive_timestep = ESMF_TimeIntervalIsPositive( clock%clockint%TimeStep )

      IF ( Present(NumRingingAlarms) ) NumRingingAlarms = 0
      clock%clockint%AdvanceCount = clock%clockint%AdvanceCount + 1
      DO i = 1, MAX_ALARMS
        alarm = clock%clockint%AlarmList(i)
        ! TBH:  This is really dangerous.  We need to be able to NULLIFY 
        ! TBH:  alarmint at compile-time (F95 synax) to make this safe.  
!$$$TBH:  see if F95 compile-time pointer-nullification is supported by all 
!$$$TBH:  compilers we support
        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              ! alarm cannot ring if clock has passed the alarms stop time
              IF ( alarm%alarmint%StopTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                  PRED1 = clock%clockint%CurrTime > alarm%alarmint%StopTime
                  PRED1 = ESMF_TimeGT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
!                  PRED1 = clock%clockint%CurrTime < alarm%alarmint%StopTime
                  PRED1 = ESMF_TimeLT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ENDIF
              ENDIF
              ! one-shot alarm:  check for ring time 
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in the duplicated code below should be sufficient.  
              IF ( alarm%alarmint%RingTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime <= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime < alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( ESMF_TimeLE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. ESMF_TimeLT( clock%clockint%CurrTime, &
                               ESMF_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime >= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime > alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( ESMF_TimeGE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. ESMF_TimeGT( clock%clockint%CurrTime, &
                               ESMF_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ENDIF
              ENDIF
              ! repeating alarm:  check for ring interval
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime + alarm%alarmint%RingInterval <= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( ESMF_TimeLE( ESMF_TimeInc(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
                  ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime - alarm%alarmint%RingInterval >= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( ESMF_TimeGE( ESMF_TimeDec(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ENDIF
              ENDIF
              IF ( (.NOT. pred1) .AND. pred2 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                 alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ELSE IF ( (.NOT. pred1) .AND. pred3 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime + &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       ESMF_TimeInc( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ELSE
                   ! in this case time step is negative and stop time is
                   ! less than start time
                   ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime - &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       ESMF_TimeDec( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ENDIF
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in here should be sufficient.  
              IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime <= clock%clockint%CurrTime ) THEN
                IF ( ESMF_TimeLE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ELSE
                ! in this case time step is negative and stop time is 
                ! less than start time
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime >= clock%clockint%CurrTime ) THEN
                IF ( ESMF_TimeGE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF ( alarm%alarmint%StopTimeSet ) THEN
! TBH:  what is this for???  
            ENDIF
          ENDIF
        ENDIF
        clock%clockint%AlarmList(i) = alarm
      ENDDO
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockStopTimeDisable - NOOP for compatibility with ESMF 2.1.0+

! !INTERFACE:
      subroutine ESMF_ClockStopTimeDisable(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

      rc = ESMF_SUCCESS

      end subroutine ESMF_ClockStopTimeDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockIsStopTime - Has the clock reached its stop time ?

! !INTERFACE:
      function ESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc
      logical :: positive_timestep

! !DESCRIPTION:
!     Return true if {\tt ESMF\_Clock} has reached its stop time, false 
!     otherwise     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.6
!EOP

      positive_timestep = ESMF_TimeIntervalIsPositive( clock%clockint%TimeStep )
      IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .GE. clock%clockint%StopTime ) THEN
        if ( ESMF_TimeGE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          ESMF_ClockIsStopTime = .TRUE.
        else
          ESMF_ClockIsStopTime = .FALSE.
        endif
      ELSE
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .LE. clock%clockint%StopTime ) THEN
        if ( ESMF_TimeLE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          ESMF_ClockIsStopTime = .TRUE.
        else
          ESMF_ClockIsStopTime = .FALSE.
        endif
      ENDIF
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    
      end function ESMF_ClockIsStopTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockRead - Restores a clock

! !INTERFACE:
      subroutine ESMF_ClockRead(clock, TimeStep, StartTime, StopTime, &
                                RefTime, CurrTime, PrevTime, AdvanceCount, &
                                AlarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(out) :: clock
      type(ESMF_TimeInterval), intent(in) :: TimeStep
      type(ESMF_Time), intent(in) :: StartTime
      type(ESMF_Time), intent(in) :: StopTime
      type(ESMF_Time), intent(in) :: RefTime
      type(ESMF_Time), intent(in) :: CurrTime
      type(ESMF_Time), intent(in) :: PrevTime
      integer(ESMF_KIND_I8), intent(in) :: AdvanceCount
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(in) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Restore an {\tt ESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to restore
!     \item[TimeStep]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt ESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockRead not supported' )
      end subroutine ESMF_ClockRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockWrite - Saves a clock

! !INTERFACE:
      subroutine ESMF_ClockWrite(clock, TimeStep, StartTime, StopTime, &
                            RefTime, CurrTime, PrevTime, AdvanceCount, &
                            AlarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: TimeStep
      type(ESMF_Time), intent(out) :: StartTime
      type(ESMF_Time), intent(out) :: StopTime
      type(ESMF_Time), intent(out) :: RefTime
      type(ESMF_Time), intent(out) :: CurrTime
      type(ESMF_Time), intent(out) :: PrevTime
      integer(ESMF_KIND_I8), intent(out) :: AdvanceCount
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Save an {\tt ESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save
!     \item[TimeStep]
!          The {\tt ESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt ESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt ESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt ESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt ESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockWrite not supported' )
      end subroutine ESMF_ClockWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockValidate(clock, opts, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on an {\tt ESMF\_Clock}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockValidate not supported' )
      end subroutine ESMF_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockPrint - Print out a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockPrint(clock, opts, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_Clock}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'ESMF_ClockPrint not supported' )
      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------

      end module ESMF_ClockMod

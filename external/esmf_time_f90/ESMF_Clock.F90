! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
      use ESMF_TimeIntervalMod   ! , only : ESMF_TimeInterval
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

!     ! Equivalent sequence and kind to C++:

      type ESMF_Clock
#ifndef F90_STANDALONE
      sequence
      private
#endif
        type(ESMF_TimeInterval) :: TimeStep
        type(ESMF_Time)  :: StartTime
        type(ESMF_Time)  :: StopTime
        type(ESMF_Time)  :: RefTime
        type(ESMF_Time)  :: CurrTime
        type(ESMF_Time)  :: PrevTime
        integer(ESMF_IKIND_I8) :: AdvanceCount
        integer :: ClockMutex
        integer :: NumAlarms
        type(ESMF_Alarm), pointer, dimension(:) :: AlarmList
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_ClockSet
      public ESMF_ClockSetOLD
      public ESMF_ClockGet
      public ESMF_ClockGetAdvanceCount
      public ESMF_ClockGetTimeStep
      public ESMF_ClockSetTimeStep
      public ESMF_ClockGetCurrTime
      public ESMF_ClockSetCurrTime
      public ESMF_ClockGetStartTime
      public ESMF_ClockGetStopTime
      public ESMF_ClockGetRefTime
      public ESMF_ClockGetPrevTime
      public ESMF_ClockGetCurrSimTime
      public ESMF_ClockGetPrevSimTime
      public ESMF_ClockAddAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNumAlarms
      public ESMF_ClockSyncToWallClock
      public ESMF_ClockAdvance
      public ESMF_ClockIsStopTime
      public ESMF_ClockStopTimeDisable

! Required inherited and overridden ESMF_Base class methods

      public ESMF_ClockRead
      public ESMF_ClockWrite
      public ESMF_ClockValidate
      public ESMF_ClockPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetOLD - Initialize a clock

! !INTERFACE:
      subroutine ESMF_ClockSetOLD(clock, TimeStep, StartTime, StopTime, &
                               RefTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(out) :: clock
      type(ESMF_TimeInterval), intent(in), optional :: TimeStep
      type(ESMF_Time), intent(in) :: StartTime
      type(ESMF_Time), intent(in) :: StopTime
      type(ESMF_Time), intent(in), optional :: RefTime
      integer, intent(out) :: rc
! Local
      integer i
      type (ESMF_Alarm) :: alarm
    
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
      call c_ESMC_ClockSet(clock, TimeStep, StartTime, StopTime, &
                           RefTime, rc)
      IF ( PRESENT(TimeStep) ) clock%TimeStep = TimeStep
      IF ( PRESENT(RefTime) ) clock%RefTime = RefTime
      clock%CurrTime = StartTime
      clock%StartTime = StartTime
      clock%StopTime = StopTime
      clock%NumAlarms = 0
      clock%AdvanceCount = 0
      ALLOCATE(clock%AlarmList(MAX_ALARMS))
      DO i = 1, MAX_ALARMS
         alarm = clock%AlarmList(i)
         alarm%Enabled = .FALSE.
         clock%AlarmList(i) = alarm
      ENDDO
    
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
      integer i, ierr
      type (ESMF_Alarm) :: alarm
    
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
      IF ( PRESENT(RefTime) ) clock%RefTime = RefTime
      IF ( PRESENT(StartTime) ) clock%StartTime = StartTime
      IF ( PRESENT(StopTime) ) clock%StopTime = StopTime
      IF ( PRESENT(CurrTime) ) THEN
        CALL ESMF_ClockSetCurrTime(clock, CurrTime, rc=ierr)
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr

      end subroutine ESMF_ClockSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGet - Get clock properties -- for compatibility with ESMF 2.0.1

! !INTERFACE:
      subroutine ESMF_ClockGet(clock, StartTime, CurrTime, &
                               AdvanceCount, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out), optional :: StartTime
      type(ESMF_Time), intent(out), optional :: CurrTime
      type(ESMF_Time), intent(out), optional :: StopTime
      integer, intent(out), optional :: AdvanceCount
      integer, intent(out), optional :: rc
      integer :: ierr
      integer(ESMF_IKIND_I8) :: AdvanceCountLcl

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
        CALL ESMF_ClockGetAdvanceCount(clock, AdvanceCountLcl, ierr)
        AdvanceCount = AdvanceCountLcl
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
      integer(ESMF_IKIND_I8), intent(out) :: AdvanceCount
      integer, intent(out) :: rc

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

      call c_ESMC_ClockGetAdvanceCount(clock, AdvanceCount, rc)

      AdvanceCount = clock%AdvanceCount

      rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockGetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: TimeStep
      integer, intent(out)           :: rc

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

      call c_ESMC_ClockGetTimeStep(clock, TimeStep, rc)

      TimeStep = clock%TimeStep
      rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockSetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(out) :: clock
      type(ESMF_TimeInterval), intent(in) :: TimeStep
      integer, intent(out)                :: rc

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

      call c_ESMC_ClockSetTimeStep(clock, TimeStep, rc)

      clock%TimeStep = TimeStep
      rc = ESMF_SUCCESS

      end subroutine ESMF_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: CurrTime
      integer, intent(out) :: rc

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

      call c_ESMC_ClockGetCurrTime(clock, CurrTime, rc)
      CurrTime = clock%CurrTime
      rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(out) :: clock
      type(ESMF_Time), intent(in) :: CurrTime
      integer, intent(out) :: rc

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

      call c_ESMC_ClockSetCurrTime(clock, CurrTime, rc)

      clock%CurrTime = CurrTime
      rc = ESMF_SUCCESS
    
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

      call c_ESMC_ClockGetStartTime(clock, StartTime, rc)

      StartTime = clock%StartTime
      rc = ESMF_SUCCESS
    
      end subroutine ESMF_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: StopTime
      integer, intent(out)         :: rc

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

      call c_ESMC_ClockGetStopTime(clock, StopTime, rc)

      StopTime = clock%StopTime
      rc = ESMF_SUCCESS
    
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

      call c_ESMC_ClockGetRefTime(clock, RefTime, rc)
    
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

      call c_ESMC_ClockGetPrevTime(clock, PrevTime, rc)
    
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

      call c_ESMC_ClockGetCurrSimTime(clock, CurrSimTime, rc)
    
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

      call c_ESMC_ClockGetPrevSimTime(clock, PrevSimTime, rc)
   
      end subroutine ESMF_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), intent(inout) :: Alarm
      integer, intent(out) :: rc

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
    
      call c_ESMC_ClockAddAlarm(clock, Alarm, rc)
      rc = ESMF_SUCCESS
      clock%NumAlarms = clock%NumAlarms + 1
      IF ( Alarm%RingTimeSet ) THEN
         Alarm%PrevRingTime = Alarm%RingTime
      ELSE
         Alarm%PrevRingTime = clock%CurrTime
      ENDIF
      Alarm%Ringing = .FALSE.
    
      end subroutine ESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Alarm), pointer :: AlarmList(:)
      integer, intent(out) :: rc

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

!      call c_ESMC_ClockGetAlarmList(clock, AlarmList, rc)
      ! This HACK is for compatibility with ESMF 2.0.1+
      IF ( ASSOCIATED( AlarmList) ) THEN
         DEALLOCATE( AlarmList )
      ENDIF
      AlarmList => clock%AlarmList
      rc = ESMF_SUCCESS
      
    
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out) :: rc

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

      call c_ESMC_ClockGetNumAlarms(clock, NumAlarms, rc)
      NumAlarms = clock%NumAlarms
      rc = ESMF_SUCCESS
    
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

      call c_ESMC_ClockSyncToWallClock(clock, rc)
    
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
      integer, intent(out) :: rc
! Local
      logical pred1, pred2, pred3
      integer i, n
      type(ESMF_Alarm) :: alarm
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
      call c_ESMC_ClockAdvance(clock, RingingAlarmList, NumRingingAlarms, rc)
      clock%CurrTime = clock%CurrTime + clock%TimeStep

      IF ( Present(NumRingingAlarms) ) NumRingingAlarms = 0
      clock%AdvanceCount = clock%AdvanceCount + 1
      DO i = 1, MAX_ALARMS
        alarm = clock%AlarmList(i)
        IF ( alarm%Enabled ) THEN
          IF ( alarm%RingIntervalSet ) THEN
	    pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
            IF ( alarm%StopTimeSet ) THEN
	       PRED1 = clock%CurrTime > Alarm%StopTime
	    ENDIF
            IF ( alarm%RingTimeSet ) THEN
	       PRED2 = ( alarm%RingTime <= clock%CurrTime                            &
		         .AND. clock%CurrTime < alarm%RingTime + clock%TimeStep )
	    ENDIF
            IF ( alarm%RingIntervalSet ) THEN
	       PRED3 = ( alarm%PrevRingTime + alarm%RingInterval <= clock%CurrTime )
	    ENDIF

            IF (                                                                           &
                 ( .NOT.  ( pred1                                                   ))     &
                   .AND.                                                                   &
                 (                                                                         &
                   ( pred2 )                                                               &
	            .OR.                                                                   &
	 	   ( pred3 )                                                               &
                 )                                                                         &
               ) THEN
               alarm%Ringing = .TRUE.
               IF ( PRED3) alarm%PrevRingTime = alarm%PrevRingTime + alarm%RingInterval
               IF ( PRESENT( RingingAlarmList ) .AND. PRESENT ( NumRingingAlarms ) ) THEN
                 NumRingingAlarms = NumRingingAlarms + 1
                 RingingAlarmList( NumRingingAlarms ) = alarm
               ENDIF
            ENDIF
          ELSE IF ( alarm%RingTimeSet ) THEN
            IF ( alarm%RingTime <= clock%CurrTime ) THEN
               alarm%Ringing = .TRUE.
               IF ( PRESENT( RingingAlarmList ) .AND. PRESENT ( NumRingingAlarms ) ) THEN
                 NumRingingAlarms = NumRingingAlarms + 1
                 RingingAlarmList( NumRingingAlarms ) = alarm
               ENDIF
            ENDIF
          ENDIF
          IF ( alarm%StopTimeSet ) THEN
          ENDIF
        ENDIF
        clock%AlarmList(i) = alarm
      ENDDO
    
      end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockStopTimeDisable - NOOP for compatibility with ESMF 2.1.0+

! !INTERFACE:
      subroutine ESMF_ClockStopTimeDisable(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: rc

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
      integer, intent(out) :: rc

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

      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, rc)
      if ( clock%CurrTime .GE. clock%StopTime ) THEN
        ESMF_ClockIsStopTime = .TRUE.
      else
        ESMF_ClockIsStopTime = .FALSE.
      endif
    
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
      integer(ESMF_IKIND_I8), intent(in) :: AdvanceCount
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

      call c_ESMC_ClockRead(clock, TimeStep, StartTime, StopTime, &
                            RefTime, CurrTime, PrevTime, AdvanceCount, &
                            AlarmList, rc)
    
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
      integer(ESMF_IKIND_I8), intent(out) :: AdvanceCount
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

      call c_ESMC_ClockWrite(clock, TimeStep, StartTime, StopTime, &
                             RefTime, CurrTime, PrevTime, AdvanceCount, &
                             AlarmList, rc)
    
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
    
      call c_ESMC_ClockValidate(clock, opts, rc)
    
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
      
      call c_ESMC_ClockPrint(clock, opts, rc)   

      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------

      end module ESMF_ClockMod

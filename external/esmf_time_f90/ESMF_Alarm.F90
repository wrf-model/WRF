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
!     ESMF Alarm Module
      module ESMF_AlarmMod
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
! !MODULE: ESMF_AlarmMod
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
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod, only : ESMF_TimeInterval
      use ESMF_TimeMod,         only : ESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     private
!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_Alarm
      sequence
#ifndef F90_STANDALONE
      private
#endif
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

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmSet
      public ESMF_AlarmGetRingInterval
      public ESMF_AlarmSetRingInterval
      public ESMF_AlarmGetRingTime
      public ESMF_AlarmSetRingTime
      public ESMF_AlarmGetPrevRingTime
      public ESMF_AlarmSetPrevRingTime
      public ESMF_AlarmGetStopTime
      public ESMF_AlarmSetStopTime
      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmTurnOn
      public ESMF_AlarmTurnOff
      public ESMF_AlarmIsRinging
      public ESMF_AlarmCheckRingTime
 
! Required inherited and overridden ESMF_Base class methods

      public ESMF_AlarmRead
      public ESMF_AlarmWrite
      public ESMF_AlarmValidate
      public ESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

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
      subroutine ESMF_AlarmSet(alarm, RingTime, RingInterval, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in), optional :: RingTime
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
      call c_ESMC_AlarmSet(alarm, RingTime, RingInterval, &
                            StopTime, Enabled, rc)
      alarm%RingTimeSet = .FALSE.
      alarm%RingIntervalSet = .FALSE.
      alarm%StopTimeSet = .FALSE.
      IF ( PRESENT( RingInterval ) ) THEN
	alarm%RingInterval = RingInterval
        alarm%RingIntervalSet = .TRUE.
      ENDIF
      IF ( PRESENT( RingTime ) ) THEN
	alarm%RingTime = RingTime
        alarm%RingTimeSet = .TRUE.
      ENDIF
      IF ( PRESENT( StopTime ) ) THEN
	alarm%StopTime = StopTime
        alarm%StopTimeSet = .TRUE.
      ENDIF
      alarm%Enabled = .TRUE.
      IF ( PRESENT( Enabled ) ) THEN
	alarm%Enabled = Enabled
      ENDIF
      IF ( PRESENT( rc ) ) THEN
	rc = ESMF_SUCCESS
      ENDIF
      alarm%Ringing = .FALSE.
      alarm%Enabled = .TRUE.

      end subroutine ESMF_AlarmSet

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
    
      call c_ESMC_AlarmGetRingInterval(alarm, RingInterval, rc)

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
    
      call c_ESMC_AlarmSetRingInterval(alarm, RingInterval, rc)

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

      call c_ESMC_AlarmGetRingTime(alarm, RingTime, rc)

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
   
      call c_ESMC_AlarmSetRingTime(alarm, RingTime, rc)

      end subroutine ESMF_AlarmSetRingTime

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

      call c_ESMC_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

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

      call c_ESMC_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

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
   
      call c_ESMC_AlarmGetStopTime(alarm, StopTime, rc)

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

      call c_ESMC_AlarmSetStopTime(alarm, StopTime, rc)

      end subroutine ESMF_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out) :: rc

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

      call c_ESMC_AlarmEnable(alarm, rc)

      Alarm%Enabled = .TRUE.
      rc = ESMF_SUCCESS

      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out) :: rc

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
    
      call c_ESMC_AlarmDisable(alarm, rc)
      alarm%Enabled = .FALSE.
      rc = ESMF_SUCCESS

      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOn - Turn on an alarm


! !INTERFACE:
      subroutine ESMF_AlarmTurnOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out) :: rc
    
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

      call c_ESMC_AlarmTurnOn(alarm, rc)
      IF ( alarm%Enabled ) THEN
        alarm%Ringing = .TRUE.
        rc = ESMF_SUCCESS
      ELSE
        alarm%Ringing = .FALSE.
        rc = ESMF_FAILURE
      ENDIF

      end subroutine ESMF_AlarmTurnOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOff - Turn off an alarm

! !INTERFACE:
      subroutine ESMF_AlarmTurnOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out) :: rc
    
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

      call c_ESMC_AlarmTurnOff(alarm, rc)

      alarm%Ringing = .FALSE.
      IF ( alarm%Enabled ) THEN
        rc = ESMF_SUCCESS
      ELSE
        rc = ESMF_FAILURE
      ENDIF

      end subroutine ESMF_AlarmTurnOff

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
      integer, intent(out) :: rc

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
    
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, rc)
      IF ( alarm%Enabled ) THEN
        ESMF_AlarmIsRinging = alarm%Ringing
        rc = ESMF_SUCCESS
      ELSE
        ESMF_AlarmIsRinging = .FALSE.
        rc = ESMF_FAILURE
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

      call c_ESMC_AlarmCheckRingTime(alarm, ESMF_AlarmCheckRingTime, &
                                     ClockCurrTime, positive, rc)
      ! assumes positive is always positive

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

      call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

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
      call c_ESMC_AlarmRead(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

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
      call c_ESMC_AlarmWrite(alarm, RingInterval, RingTime, &
                             PrevRingTime, StopTime, Ringing, &
                             Enabled, ID, rc)

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
      
      call c_ESMC_AlarmValidate(alarm, opts, rc)
    
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
      
      call c_ESMC_AlarmPrint(alarm, opts, rc)   

      end subroutine ESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod

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
!     ESMF Alarm-Clock Module
      module ESMF_AlarmClockMod
!
!==============================================================================
!
! This file contains the AlarmCreate method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both ESMF_AlarmMod and ESMF_ClockMod.  
! Separation is needed to avoid cyclic dependence.  
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit ESMF_Alarm and ESMF_Clock
      use ESMF_AlarmMod, only : ESMF_Alarm, ESMF_AlarmSet
      use ESMF_ClockMod, only : ESMF_Clock, ESMF_ClockAddAlarm

      ! associated derived types
      use ESMF_TimeIntervalMod, only : ESMF_TimeInterval
      use ESMF_TimeMod,         only : ESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmCreate

!==============================================================================

      contains

!==============================================================================


! Create ESMF_Alarm using ESMF 2.1.0+ semantics
      FUNCTION ESMF_AlarmCreate( clock, RingTime, RingInterval, &
                                 StopTime, Enabled, rc )

        ! return value
        type(ESMF_Alarm) :: ESMF_AlarmCreate
        ! !ARGUMENTS:
        type(ESMF_Clock), intent(inout), optional :: clock
        type(ESMF_Time), intent(in), optional :: RingTime
        type(ESMF_TimeInterval), intent(in), optional :: RingInterval
        type(ESMF_Time), intent(in), optional :: StopTime
        logical, intent(in), optional :: Enabled
        integer, intent(out), optional :: rc
        ! locals
        type(ESMF_Alarm) :: alarmtmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( alarmtmp%alarmint )
        CALL ESMF_AlarmSet( alarmtmp,                  &
                            RingTime=RingTime,         &
                            RingInterval=RingInterval, &
                            StopTime=StopTime,         &
                            Enabled=Enabled,           &
                            rc=rc )
        IF ( PRESENT ( clock ) ) THEN
          CALL ESMF_ClockAddAlarm( clock, alarmtmp, rc )
        ENDIF
        ESMF_AlarmCreate = alarmtmp
      END FUNCTION ESMF_AlarmCreate


!------------------------------------------------------------------------------

      end module ESMF_AlarmClockMod

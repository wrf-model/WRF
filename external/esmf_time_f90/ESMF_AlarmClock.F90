!
!==============================================================================
!
!     ESMF Alarm-Clock Module
      module WRF_ESMF_AlarmClockMod
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
! !MODULE: WRF_ESMF_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both WRF_ESMF_AlarmMod and WRF_ESMF_ClockMod.  
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
      use WRF_ESMF_AlarmMod, only : ESMF_Alarm, ESMF_AlarmSet
      use WRF_ESMF_ClockMod, only : ESMF_Clock, ESMF_ClockAddAlarm

      ! associated derived types
      use WRF_ESMF_TimeIntervalMod, only : ESMF_TimeInterval
      use WRF_ESMF_TimeMod,         only : ESMF_Time

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

      end module WRF_ESMF_AlarmClockMod

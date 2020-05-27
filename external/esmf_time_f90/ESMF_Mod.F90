! TBH:  This version is for use with the ESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE ESMF_Mod
   USE WRF_ESMF_AlarmMod
   USE WRF_ESMF_BaseMod
   USE WRF_ESMF_BaseTimeMod
   USE WRF_ESMF_CalendarMod
   USE WRF_ESMF_ClockMod
   USE WRF_ESMF_FractionMod
   USE WRF_ESMF_TimeIntervalMod
   USE WRF_ESMF_TimeMod
   USE WRF_ESMF_AlarmClockMod
   USE WRF_ESMF_Stubs   ! add new dummy interfaces and typedefs here as needed
#include <ESMF_TimeMgr.inc>
   INTEGER, PARAMETER :: ESMF_MAX_ALARMS=MAX_ALARMS
!
END MODULE ESMF_Mod

! TBH:  This version is for use with the ESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE ESMF_Mod
   USE esmf_alarmmod
   USE esmf_basemod
   USE esmf_basetimemod
   USE esmf_calendarmod
   USE esmf_clockmod
   USE esmf_fractionmod
   USE esmf_timeintervalmod
   USE esmf_timemod
   USE esmf_alarmclockmod
   USE esmf_stubs   ! add new dummy interfaces and typedefs here as needed
#include <ESMF_TimeMgr.inc>
   INTEGER, PARAMETER :: ESMF_MAX_ALARMS=MAX_ALARMS
!
END MODULE ESMF_Mod

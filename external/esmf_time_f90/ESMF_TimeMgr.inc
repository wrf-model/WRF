#if 0
$Id$
 
Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the GPL.

Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.
#endif

#ifndef ESMF_TimeMgr_INC
#define ESMF_TimeMgr_INC

#if 0
!BOP
-------------------------------------------------------------------------

 !DESCRIPTION:

 ESMF TimeMgr include file for F90
 The code in this file implements constants and macros for the TimeMgr...

-------------------------------------------------------------------------
!EOP
#endif

#include <ESMF_Macros.inc>

#define SECONDS_PER_DAY 86400
#define SECONDS_PER_HOUR 3600
#define SECONDS_PER_MINUTE 60

! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  
#define MAX_ALARMS 20

! TBH:  TODO:  Hook this into the WRF build so WRF can use either "no-leap" or 
! TBH:         Gregorian calendar.  Now WRF is hard-wired to use Gregorian.  
#undef NO_LEAP_CALENDAR
#ifdef COUP_CSM
#define NO_LEAP_CALENDAR
#endif

#endif

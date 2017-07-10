#if 0
 
Earth System Modeling Framework
Copyright 2002-2003, University Corporation for Atmospheric Research,
Massachusetts Institute of Technology, Geophysical Fluid Dynamics
Laboratory, University of Michigan, National Centers for Environmental
Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
NASA Goddard Space Flight Center.
Licensed under the University of Illinois-NCSA license.

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

#include "ESMF_Macros.inc"

#define SECONDS_PER_DAY 86400_ESMF_KIND_I8
#define SECONDS_PER_HOUR 3600_ESMF_KIND_I8
#define SECONDS_PER_MINUTE 60_ESMF_KIND_I8
#define HOURS_PER_DAY 24_ESMF_KIND_I8

! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  
#include "../../inc/streams.h"
#define MAX_ALARMS (2*(MAX_HISTORY)+10)

! TBH:  TODO:  Hook this into the WRF build so WRF can use either "no-leap" or 
! TBH:         Gregorian calendar.  Now WRF is hard-wired to use Gregorian.  
!#undef NO_LEAP_CALENDAR
!#ifdef COUP_CSM
!#define NO_LEAP_CALENDAR
!#endif

! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers like PGI 5.x do not support this F95 
! TBH:  feature.
!#ifdef NO_LEAP_CALENDAR
!#undef NO_DT_COMPONENT_INIT
!#else
!#define NO_DT_COMPONENT_INIT
!#endif

#endif

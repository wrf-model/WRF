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

#define MAX_ALARMS 20

#endif

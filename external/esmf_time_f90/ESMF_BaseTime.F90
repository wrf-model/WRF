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
!     ESMF BaseTime Module
      module ESMF_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

#include <ESMF_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: ESMF_BaseTimeMod - Base ESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_BaseTime
      sequence                        ! for C++ interoperability
#ifndef F90_STANDALONE
      private
        integer(ESMF_KIND_I8) :: S   ! whole seconds
#else
        integer                :: S   ! whole seconds
#endif
        integer                :: Sn  ! fractional seconds, numerator
        integer                :: Sd  ! fractional seconds, denominator
#ifdef F90_STANDALONE
        integer                :: MS  ! milliseconds
#endif
        integer                :: pad1  ! to match halem C++ <vtbl> long[4]*
        integer                :: pad2  ! to match halem C++ <vtbl> long[6]*
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! None exposed at F90 API layer; inherited through
! ESMF_TimeInterval and ESMF_Time
!
!EOPI

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      end module ESMF_BaseTimeMod

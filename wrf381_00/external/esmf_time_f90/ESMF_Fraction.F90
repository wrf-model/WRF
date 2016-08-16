!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
! ESMF Fraction Module
!
!==============================================================================
!
!     ESMF Fraction Module
      module ESMF_FractionMod
!
!==============================================================================
!
! This file contains the Fraction class definition and all Fraction
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!
!===============================================================================
!BOPI
!
! !MODULE: ESMF_FractionMod
!
! !DESCRIPTION:
! Part of ESMF F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_Fraction}
!
! See {\tt ../include/ESMC\_Fraction.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Fraction
!
      type ESMF_Fraction
      private
        integer :: n    ! Integer fraction (exact) n/d; numerator
        integer :: d    ! Integer fraction (exact) n/d; denominator
      end type
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Fraction
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! !PRIVATE MEMBER FUNCTIONS:

!EOPI

!==============================================================================

!      contains

!==============================================================================
!
! Wrappers to C++ fraction routines
!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------

      end module ESMF_FractionMod

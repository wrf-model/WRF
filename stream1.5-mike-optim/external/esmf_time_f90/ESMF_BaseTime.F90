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

      type ESMF_BaseTime
        integer(ESMF_KIND_I8) :: S   ! whole seconds
        integer(ESMF_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(ESMF_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators
      public operator(+)
      private ESMF_BaseTimeSum
      public operator(-)
      private ESMF_BaseTimeDifference
      public operator(/)
      private ESMF_BaseTimeQuotI
      private ESMF_BaseTimeQuotI8
      public operator(.EQ.)
      private ESMF_BaseTimeEQ
      public operator(.NE.)
      private ESMF_BaseTimeNE
      public operator(.LT.)
      private ESMF_BaseTimeLT
      public operator(.GT.)
      private ESMF_BaseTimeGT
      public operator(.LE.)
      private ESMF_BaseTimeLE
      public operator(.GE.)
      private ESMF_BaseTimeGE

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
      interface operator(+)
        module procedure ESMF_BaseTimeSum
      end interface
      interface operator(-)
        module procedure ESMF_BaseTimeDifference
      end interface
      interface operator(/)
        module procedure ESMF_BaseTimeQuotI,ESMF_BaseTimeQuotI8
      end interface
      interface operator(.EQ.)
        module procedure ESMF_BaseTimeEQ
      end interface
      interface operator(.NE.)
        module procedure ESMF_BaseTimeNE
      end interface
      interface operator(.LT.)
        module procedure ESMF_BaseTimeLT
      end interface
      interface operator(.GT.)
        module procedure ESMF_BaseTimeGT
      end interface
      interface operator(.LE.)
        module procedure ESMF_BaseTimeLE
      end interface
      interface operator(.GE.)
        module procedure ESMF_BaseTimeGE
      end interface


!==============================================================================

      contains

!==============================================================================


! Add two basetimes
      FUNCTION ESMF_BaseTimeSum( basetime1, basetime2 )
        TYPE(ESMF_BaseTime) :: ESMF_BaseTimeSum
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        INTEGER (ESMF_KIND_I8) :: Sn1, Sd1, Sn2, Sd2, lcd
!  PRINT *,'DEBUG:  BEGIN ESMF_BaseTimeSum()'
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
        ESMF_BaseTimeSum   = basetime1
        ESMF_BaseTimeSum%S = ESMF_BaseTimeSum%S + basetime2%S
        Sn1 = basetime1%Sn
        Sd1 = basetime1%Sd
        Sn2 = basetime2%Sn
        Sd2 = basetime2%Sd
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sd2 = ',Sd2
        IF      ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  no fractions'
          ESMF_BaseTimeSum%Sn = 0
          ESMF_BaseTimeSum%Sd = 0
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
          ESMF_BaseTimeSum%Sn = Sn1
          ESMF_BaseTimeSum%Sd = Sd1
        ELSE IF ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          ESMF_BaseTimeSum%Sn = Sn2
          ESMF_BaseTimeSum%Sd = Sd2
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CALL compute_lcd( Sd1 , Sd2 , lcd )
          ESMF_BaseTimeSum%Sd = lcd
          ESMF_BaseTimeSum%Sn = (Sn1 * lcd / Sd1) + (Sn2 * lcd / Sd2)
        ENDIF
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%S = ',ESMF_BaseTimeSum%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%Sn = ',ESMF_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%Sd = ',ESMF_BaseTimeSum%Sd
        CALL normalize_basetime( ESMF_BaseTimeSum )
!  PRINT *,'DEBUG:  END ESMF_BaseTimeSum()'
      END FUNCTION ESMF_BaseTimeSum


! Subtract two basetimes
      FUNCTION ESMF_BaseTimeDifference( basetime1, basetime2 )
        TYPE(ESMF_BaseTime) :: ESMF_BaseTimeDifference
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        TYPE(ESMF_BaseTime) :: neg2

        neg2%S  = -basetime2%S
        neg2%Sn = -basetime2%Sn
        neg2%Sd =  basetime2%Sd

        ESMF_BaseTimeDifference = basetime1 + neg2

      END FUNCTION ESMF_BaseTimeDifference


! Divide basetime by 8-byte integer
      FUNCTION ESMF_BaseTimeQuotI8( basetime, divisor )
        TYPE(ESMF_BaseTime) :: ESMF_BaseTimeQuotI8
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER(ESMF_KIND_I8), INTENT(IN) :: divisor
        ! locals
        INTEGER(ESMF_KIND_I8) :: d, n, dinit

!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() A:  divisor = ', divisor
        IF ( divisor == 0_ESMF_KIND_I8 ) THEN
          CALL wrf_error_fatal( 'ESMF_BaseTimeQuotI8:  divide by zero' )
        ENDIF

!$$$ move to default constructor
        ESMF_BaseTimeQuotI8%S  = 0
        ESMF_BaseTimeQuotI8%Sn = 0
        ESMF_BaseTimeQuotI8%Sd = 0

        ! convert to a fraction and divide by multipling the denonminator by 
        ! the divisor
        IF ( basetime%Sd == 0 ) THEN
          dinit = 1_ESMF_KIND_I8
        ELSE
          dinit = basetime%Sd
        ENDIF
        n = basetime%S * dinit + basetime%Sn
        d = dinit * divisor
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() B:  n,d = ',n,d
        CALL simplify( n, d, ESMF_BaseTimeQuotI8%Sn, ESMF_BaseTimeQuotI8%Sd )
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  ESMF_BaseTimeQuotI8%S,ESMF_BaseTimeQuotI8%Sn,ESMF_BaseTimeQuotI8%Sd
        CALL normalize_basetime( ESMF_BaseTimeQuotI8 )
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  ESMF_BaseTimeQuotI8%S,ESMF_BaseTimeQuotI8%Sn,ESMF_BaseTimeQuotI8%Sd
      END FUNCTION ESMF_BaseTimeQuotI8

! Divide basetime by integer
      FUNCTION ESMF_BaseTimeQuotI( basetime, divisor )
        TYPE(ESMF_BaseTime) :: ESMF_BaseTimeQuotI
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER, INTENT(IN) :: divisor
        IF ( divisor == 0 ) THEN
          CALL wrf_error_fatal( 'ESMF_BaseTimeQuotI:  divide by zero' )
        ENDIF
        ESMF_BaseTimeQuotI = basetime / INT( divisor, ESMF_KIND_I8 )
      END FUNCTION ESMF_BaseTimeQuotI


! .EQ. for two basetimes
      FUNCTION ESMF_BaseTimeEQ( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeEQ
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeEQ = ( retval .EQ. 0 )
      END FUNCTION ESMF_BaseTimeEQ


! .NE. for two basetimes
      FUNCTION ESMF_BaseTimeNE( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeNE
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeNE = ( retval .NE. 0 )
      END FUNCTION ESMF_BaseTimeNE


! .LT. for two basetimes
      FUNCTION ESMF_BaseTimeLT( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeLT
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeLT = ( retval .LT. 0 )
      END FUNCTION ESMF_BaseTimeLT


! .GT. for two basetimes
      FUNCTION ESMF_BaseTimeGT( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeGT
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeGT = ( retval .GT. 0 )
      END FUNCTION ESMF_BaseTimeGT


! .LE. for two basetimes
      FUNCTION ESMF_BaseTimeLE( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeLE
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeLE = ( retval .LE. 0 )
      END FUNCTION ESMF_BaseTimeLE


! .GE. for two basetimes
      FUNCTION ESMF_BaseTimeGE( basetime1, basetime2 )
        LOGICAL :: ESMF_BaseTimeGE
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(ESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        ESMF_BaseTimeGE = ( retval .GE. 0 )
      END FUNCTION ESMF_BaseTimeGE


      end module ESMF_BaseTimeMod

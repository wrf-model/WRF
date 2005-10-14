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
!     ESMF TimeInterval Module
      module ESMF_TimeIntervalMod
!
!==============================================================================
!
! This file contains the TimeInterval class definition and all TimeInterval
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: ESMF_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_TimeInterval}
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod, only : ESMF_BaseTime

      ! associated derived types
      use ESMF_FractionMod, only : ESMF_Fraction
      use ESMF_CalendarMod, only : ESMF_Calendar

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_TimeInterval
!
!     ! F90 class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_TimeInterval
      sequence                           ! match C++ storage order
#ifndef F90_STANDALONE
      private                            !   (members opaque on F90 side)
        type(ESMF_BaseTime) :: basetime  ! inherit base class
        integer(ESMF_KIND_I8) :: YY     ! calendar interval number of years
        integer(ESMF_KIND_I8) :: MO     ! calendar interval number of months
#else
        type(ESMF_BaseTime) :: basetime  ! inherit base class
        logical                :: instant  ! false for instant, true for interval
        integer                :: YR     ! calendar interval number of days
        integer                :: MM     ! calendar interval number of days
        integer                :: DD     ! calendar interval number of days
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_TimeInterval
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_TimeIntervalGet
      public ESMF_TimeIntervalSet
      public ESMFold_TimeIntervalGetString
      public ESMF_TimeIntervalAbsValue
      public ESMF_TimeIntervalNegAbsValue

! Required inherited and overridden ESMF_Base class methods

!      public ESMF_TimeIntervalRead
!      public ESMF_TimeIntervalWrite
      public ESMF_TimeIntervalValidate
      public ESMF_TimeIntervalPrint
!!!!!!!!! added 20051012, JM
      public WRFADDITION_TimeIntervalDIVQuot 

! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 
      public operator(.DIV.)
      private ESMF_TimeIntervalFQuot

      public operator(/)
      private ESMF_TimeIntervalRQuot
      private ESMF_TimeIntervalQuotI
      private ESMF_TimeIntervalQuotR

      public operator(*)
      private ESMF_TimeIntervalProdI
      private ESMF_TimeIntervalProdF
      private ESMF_TimeIntervalProdR

! Inherited and overloaded from ESMF_BaseTime

      public operator(+)
      private ESMF_TimeIntervalSum

      public operator(-)
      private ESMF_TimeIntervalDiff

      public operator(.EQ.)
      private ESMF_TimeIntervalEQ

      public operator(.NE.)
      private ESMF_TimeIntervalNE

      public operator(.LT.)
      private ESMF_TimeIntervalLT

      public operator(.GT.)
      private ESMF_TimeIntervalGT

      public operator(.LE.)
      private ESMF_TimeIntervalLE

      public operator(.GE.)
      private ESMF_TimeIntervalGE
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(.DIV.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalFQuot

! !DESCRIPTION:
!     This interface defines a new .DIV. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!
!BOP
! !INTERFACE:
      interface operator(/)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalRQuot
      module procedure ESMF_TimeIntervalQuotI
      module procedure ESMF_TimeIntervalQuotR

! !DESCRIPTION:
!     This interface overloads the / operator for the {\tt ESMF\_TimeInterval}
!     class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(*)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdI
      module procedure ESMF_TimeIntervalProdF
      module procedure ESMF_TimeIntervalProdR

! !DESCRIPTION:
!     This interface overloads the * operator for the {\tt ESMF\_TimeInterval}
!     class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalSum

! !DESCRIPTION:
!     This interface overloads the + operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get value in user-specified units

! !INTERFACE:
      subroutine ESMF_TimeIntervalGet(timeinterval, YY, YYl, MO, MOl, D, Dl, &
                                      H, M, S, Sl, MS, US, NS, d_, h_, m_, s_, &
                                      ms_, us_, ns_, Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(out), optional :: YY
      integer(ESMF_KIND_I8), intent(out), optional :: YYl
      integer, intent(out), optional :: MO
      integer(ESMF_KIND_I8), intent(out), optional :: MOl
      integer, intent(out), optional :: D
      integer(ESMF_KIND_I8), intent(out), optional :: Dl
      integer, intent(out), optional :: H
      integer, intent(out), optional :: M
      integer, intent(out), optional :: S
      integer(ESMF_KIND_I8), intent(out), optional :: Sl
      integer, intent(out), optional :: MS
      integer, intent(out), optional :: US
      integer, intent(out), optional :: NS
      double precision, intent(out), optional :: d_
      double precision, intent(out), optional :: h_
      double precision, intent(out), optional :: m_
      double precision, intent(out), optional :: s_
      double precision, intent(out), optional :: ms_
      double precision, intent(out), optional :: us_
      double precision, intent(out), optional :: ns_
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the value of the {\tt ESMF\_TimeInterval} in units specified by the
!     user via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{[YY]}]
!          Integer years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer years (large, >= 64-bit)
!     \item[{[MO]}]
!          Integer months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.1
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeIntervalGet(timeinterval, YY, YYl, MO, MOl, D, Dl, &
                                  H, M, S, Sl, MS, US, NS, d_, h_, m_, s_, &
                                  ms_, us_, ns_, Sn, Sd, rc)
    
      end subroutine ESMF_TimeIntervalGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine ESMF_TimeIntervalSet(timeinterval, YY, YYl, MM, MOl, D, Dl, &
                                      H, M, S, Sl, MS, US, NS, &
                                      d_, h_, m_, s_, ms_, us_, ns_, &
                                      Sn, Sd, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      integer, intent(in), optional :: YY
      integer(ESMF_KIND_I8), intent(in), optional :: YYl
      integer, intent(in), optional :: MM
      integer(ESMF_KIND_I8), intent(in), optional :: MOl
      integer, intent(in), optional :: D
      integer(ESMF_KIND_I8), intent(in), optional :: Dl
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer(ESMF_KIND_I8), intent(in), optional :: Sl
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via F90 optional arguments
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer number of interval years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer number of interval years (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer number of interval months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer number of interval months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer number of interval days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer number of interval days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      ! use optional args for any subset
      call c_ESMC_TimeIntervalSet(timeinterval, YY, YYl, MM, MOl, D, Dl, &
                                  H, M, S, Sl, MS, US, NS, &
                                  d_, h_, m_, s_, ms_, us_, ns_, &
                                  Sn, Sd, rc)

#ifdef F90_STANDALONE
      timeinterval%instant = .false.
      timeinterval%YR = 0
      IF ( PRESENT( YY ) ) THEN
        timeinterval%YR = YY
      ENDIF
      timeinterval%MM = 0
      IF ( PRESENT( MM ) ) THEN
        timeinterval%MM = MM
      ENDIF
!
!      timeinterval%basetime%S = 0
!      IF ( PRESENT( H ) ) THEN
!        timeinterval%basetime%S = timeinterval%basetime%S + H * 3600
!      ENDIF
!      IF ( PRESENT( M ) ) THEN
!        timeinterval%basetime%S = timeinterval%basetime%S + M * 60
!      ENDIF
!      IF ( PRESENT( S ) ) THEN
!        timeinterval%basetime%S = timeinterval%basetime%S + S
!      ENDIF
!      IF ( PRESENT( MS ) ) THEN
!        timeinterval%basetime%MS = MS
!      ELSE IF ( PRESENT( Sd ) .AND. PRESENT( Sn ) ) THEN
!        timeinterval%basetime%Sd = Sd
!        timeinterval%basetime%Sn = Sn
!        timeinterval%basetime%MS = NINT( Sn*1.0D0 / Sd*1.0D0 * 1000 )
!      ENDIF

      timeinterval%DD = 0
      timeinterval%basetime%S = 0
      IF ( PRESENT( D ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + D * 24 * 3600
      ENDIF
      IF ( PRESENT( H ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + H * 3600
      ENDIF
      IF ( PRESENT( M ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + M * 60
      ENDIF
      IF ( PRESENT( S ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + S
      ENDIF
      timeinterval%basetime%MS = 0
      timeinterval%basetime%Sn = 0
      timeinterval%basetime%Sd = 1
      IF ( PRESENT( MS ) ) THEN
        timeinterval%basetime%MS = MS
      ELSE IF ( PRESENT( Sd ) .AND. PRESENT( Sn ) ) THEN
        timeinterval%basetime%Sn = Sn
        timeinterval%basetime%Sd = Sd
        if ( abs( Sn ) .GE. Sd ) THEN
          IF ( Sn .GE. 0 ) THEN
            timeinterval%basetime%S = timeinterval%basetime%S + Sn / Sd
          ELSE
            IF ( Sn .NE. Sd ) THEN
              timeinterval%basetime%S = timeinterval%basetime%S + Sn / Sd - 1
            ELSE
              timeinterval%basetime%S = timeinterval%basetime%S + Sn / Sd
            ENDIF
          ENDIF
        ENDIF
        timeinterval%basetime%MS = NINT( Sn*1.0D0 / Sd*1.0D0  * 1000 )
      ENDIF

      rc = ESMF_SUCCESS
#endif


      end subroutine ESMF_TimeIntervalSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFold_TimeIntervalGetString - Get time interval value in string format

! !INTERFACE:
      subroutine ESMFold_TimeIntervalGetString(timeinterval, TimeString, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character*(*),  intent(out) :: TimeString
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Convert {\tt ESMF\_TimeInterval}'s value into string format
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.9
!EOP

#ifdef F90_STANDALONE
      write(TimeString,'(I5.5"_"I2.2":"I2.2":"I2.2)') &
             timeinterval%basetime%S / (3600 * 24) , &
             mod( timeinterval%basetime%S / 3600 , 24 ) , &
             mod( timeinterval%basetime%S / 60 , 60 ), &
             mod( timeinterval%basetime%S  , 60 )

!write(0,*)'TimeIntervalGetString Sn ',timeinterval%basetime%Sn,' Sd ',timeinterval%basetime%Sd


      rc = ESMF_SUCCESS
#else
      call c_ESMC_TimeIntervalGetString(timeinterval, TimeString, rc)
#endif

      end subroutine ESMFold_TimeIntervalGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a time interval

! !INTERFACE:
      function ESMF_TimeIntervalAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the absolute value of.
!          Absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
    
      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalAbsValue, rc=rc )
      call c_ESMC_TimeIntervalAbsValue(timeinterval, ESMF_TimeIntervalAbsValue)

      end function ESMF_TimeIntervalAbsValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a time interval

! !INTERFACE:
      function ESMF_TimeIntervalNegAbsValue(timeinterval)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalNegAbsValue

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s negative absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the negative absolute value of.
!          Negative absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
    
      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalNegAbsValue, rc=rc )
      call c_ESMC_TimeIntervalNegAbsValue(timeinterval, &
                                          ESMF_TimeIntervalNegAbsValue)

      end function ESMF_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalFQuot - Divide two time intervals, return fraction result

! !INTERFACE:
      function ESMF_TimeIntervalFQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_Fraction) :: ESMF_TimeIntervalFQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a fraction quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

      call c_ESMC_TimeIntervalFQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalFQuot)

      end function ESMF_TimeIntervalFQuot

!!!!!!!!!!!!!!!!!! added jm 20051012
! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder
      function WRFADDITION_TimeIntervalDIVQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      INTEGER :: WRFADDITION_TimeIntervalDIVQuot 

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !LOCAL
      INTEGER :: retval, rc
      type(ESMF_TimeInterval) :: zero, i1,i2

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a fraction quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

      call c_ESMC_TimeIntervalFQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalFQuot)

      call ESMF_TimeIntervalSet( zero, rc=rc )
      i1 = timeinterval1
      i2 = timeinterval2
      if ( i1 .LT. zero ) then
        i1 = ESMF_TimeIntervalProdI(i1, -1)
      endif
      if ( i2 .LT. zero ) then
        i2 = ESMF_TimeIntervalProdI(i2, -1)
      endif
! repeated subtraction
      retval = 0
      DO WHILE (  i1 .GE. i2 )
        i1 = i1 - i2
        retval = retval + 1
      ENDDO

      WRFADDITION_TimeIntervalDIVQuot = retval

      end function WRFADDITION_TimeIntervalDIVQuot
!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRQuot - Divide two time intervals, return double precision result

! !INTERFACE:
      function ESMF_TimeIntervalRQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      double precision :: ESMF_TimeIntervalRQuot

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a double precision
!     quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP

      call c_ESMC_TimeIntervalRQuot(timeinterval1, timeinterval2, &
                                    ESMF_TimeIntervalRQuot)

      end function ESMF_TimeIntervalRQuot

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide time interval by an integer, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotI(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: divisor
      integer  d, n
!
      type(ESMF_TimeInterval) :: retval
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Divides a {\tt ESMF\_TimeInterval} by an integer divisor, returns
!     quotient as a {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Integer divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

      retval = timeinterval

      CALL ESMF_TimeIntervalSet( retval, rc=rc )
      call c_ESMC_TimeIntervalQuotI(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotI)

! convert timeinterval to a fraction and divide by multipling the denonminator by the divisor
      n = timeinterval%basetime%S * timeinterval%basetime%Sd + timeinterval%basetime%Sn
      d = timeinterval%basetime%Sd * divisor

      CALL simplify(n,d,retval%basetime%Sn,retval%basetime%Sd) 

      IF ( retval%basetime%Sn > retval%basetime%Sd ) THEN
        retval%basetime%S = retval%basetime%Sn / retval%basetime%Sd
        retval%basetime%Sn = mod( retval%basetime%Sn, retval%basetime%Sd )
      ENDIF

      ESMF_TimeIntervalQuotI = retval

      end function ESMF_TimeIntervalQuotI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotR - Divide time interval by a double precision, return time interval result 

! !INTERFACE:
      function ESMF_TimeIntervalQuotR(timeinterval, divisor)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalQuotR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      double precision, intent(in) :: divisor
! !LOCAL:
      integer    :: rc

! !DESCRIPTION:
!     Divides an {\tt ESMF\_TimeInterval} by a double precision divisor, returns
!     quotient as a {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Double precision divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalQuotR, rc=rc )
      call c_ESMC_TimeIntervalQuotR(timeinterval, divisor, &
                                    ESMF_TimeIntervalQuotR)

      end function ESMF_TimeIntervalQuotR

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdI - Multiply a time interval by an integer

! !INTERFACE:
      function ESMF_TimeIntervalProdI(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdI

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(in) :: multiplier
! !LOCAL:
      integer    :: rc
      integer    :: i

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by an integer, return product as a
!     {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Integer multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalProdI, rc=rc )
      call c_ESMC_TimeIntervalProdI(timeinterval, multiplier, &
                                    ESMF_TimeIntervalProdI)
      
      IF ( multiplier .GT. 0 ) THEN
        DO i = 1, multiplier
           ESMF_TimeIntervalProdI = ESMF_TimeIntervalProdI + timeinterval
        ENDDO
      ELSE
        DO i = 1, -1*multiplier
           ESMF_TimeIntervalProdI = ESMF_TimeIntervalProdI - timeinterval
        ENDDO
      ENDIF

      end function ESMF_TimeIntervalProdI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalProdF - Multiply a time interval by a fraction

! !INTERFACE:
      function ESMF_TimeIntervalProdF(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdF

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      type(ESMF_Fraction), intent(in) :: multiplier
! !LOCAL:
      integer :: rc

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by a fraction, return product as a
!     {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Fraction multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalProdF, rc=rc )
      call c_ESMC_TimeIntervalProdF(timeinterval, multiplier, &
                                    ESMF_TimeIntervalProdF)

      end function ESMF_TimeIntervalProdF
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdR - Multiply a time interval by a double precision

! !INTERFACE:
      function ESMF_TimeIntervalProdR(timeinterval, multiplier)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalProdR

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      double precision, intent(in) :: multiplier
! !LOCAL:
      integer :: rc

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by a double precision number,
!     return product as a {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Double precision multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalProdR, rc=rc )
      call c_ESMC_TimeIntervalProdR(timeinterval, multiplier, &
                                    ESMF_TimeIntervalProdR)

      end function ESMF_TimeIntervalProdR

!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum - Add two time intervals together

! !INTERFACE:
      function ESMF_TimeIntervalSum(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalSum

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2
! !LOCAL:
      integer                             :: rc
! !DESCRIPTION:
!     Add two {\tt ESMF\_TimeIntervals}, return sum as a
!     {\tt ESMF\_TimeInterval}.  Maps overloaded (+) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The augend 
!     \item[timeinterval2]
!          The addend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, 
!                 TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalSum, rc=rc )
      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntervalSum(timeinterval1, timeinterval2, &
                                      ESMF_TimeIntervalSum)

      end function ESMF_TimeIntervalSum

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one time interval from another
   
! !INTERFACE:
      function ESMF_TimeIntervalDiff(timeinterval1, timeinterval2)

! !RETURN VALUE:
      type(ESMF_TimeInterval) :: ESMF_TimeIntervalDiff

! !ARGUMENTS: 
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2
! !LOCAL:
      integer                             :: rc
! !DESCRIPTION:
!     Subtract timeinterval2 from timeinterval1, return remainder as a 
!     {\tt ESMF\_TimeInterval}.
!     Map overloaded (-) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The minuend 
!     \item[timeinterval2]
!          The subtrahend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalDiff, rc=rc )
      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntervalDiff(timeinterval1, timeinterval2, &
                                       ESMF_TimeIntervalDiff)

      end function ESMF_TimeIntervalDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalEQ - Compare two time intervals for equality

! !INTERFACE:
      function ESMF_TimeIntervalEQ(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalEQ

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

!DESCRIPTION:
!     Return true if both given time intervals are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeEQ(timeinterval1, timeinterval2, ESMF_TimeIntervalEQ)

      end function ESMF_TimeIntervalEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNE - Compare two time intervals for inequality

! !INTERFACE:
      function ESMF_TimeIntervalNE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalNE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if both given time intervals are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(timeinterval1, timeinterval2, ESMF_TimeIntervalNE)

      end function ESMF_TimeIntervalNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLT - Time interval 1 less than time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than second time interval,
!     false otherwise. Maps overloaded (<) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(timeinterval1, timeinterval2, ESMF_TimeIntervalLT)

      end function ESMF_TimeIntervalLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGT - Time interval 1 greater than time interval 2?

! !INTERFACE:
      function ESMF_TimeIntervalGT(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGT

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than second time interval,
!     false otherwise.  Maps overloaded (>) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(timeinterval1, timeinterval2, ESMF_TimeIntervalGT)

      end function ESMF_TimeIntervalGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalLE(timeinterval1, timeinterval2)

! !RETURN VALUE:
      logical :: ESMF_TimeIntervalLE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is less than or equal to second time
!     interval, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(timeinterval1, timeinterval2, ESMF_TimeIntervalLE)

      end function ESMF_TimeIntervalLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?

! !INTERFACE:
      function ESMF_TimeIntervalGE(timeinterval1, timeinterval2)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalGE

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !DESCRIPTION:
!     Return true if first time interval is greater than or equal to second
!     time interval, false otherwise. Maps overloaded (>=) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(timeinterval1, timeinterval2, ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalRead - Restore a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalRead(timeinterval, S, Sn, Sd, YY, MO, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(out) :: timeinterval
      integer(ESMF_KIND_I8), intent(in) :: S
      integer, intent(in) :: Sn
      integer, intent(in) :: Sd
      integer(ESMF_KIND_I8), intent(in) :: YY
      integer(ESMF_KIND_I8), intent(in) :: MO
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt ESMF\_TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          {\tt ESMF\_TimeInterval} to restore
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[YY]
!          64-bit integer calendar interval number of years
!     \item[MO]
!          64-bit integer calendar interval number of months
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeIntervalRead(timeinterval, S, Sn, Sd, YY, MO, rc)

      end subroutine ESMF_TimeIntervalRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalWrite - Save a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalWrite(timeinterval, S, Sn, Sd, YY, MO, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer(ESMF_KIND_I8), intent(out) :: S
      integer, intent(out) :: Sn
      integer, intent(out) :: Sd
      integer(ESMF_KIND_I8), intent(out) :: YY
      integer(ESMF_KIND_I8), intent(out) :: MO
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on an {\tt ESMF\_TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          {\tt ESMF\_TimeInterval} to save
!     \item[S]
!          64-bit integer seconds
!     \item[Sn]
!          Integer fractional seconds - numerator
!     \item[Sd]
!          Integer fractional seconds - denominator
!     \item[YY]
!          64-bit integer calendar interval number of years
!     \item[MO]
!          64-bit integer calendar interval number of months
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_TimeIntervalWrite(timeinterval, S, Sn, Sd, YY, MO, rc)

      end subroutine ESMF_TimeIntervalWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalValidate - Validate a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalValidate(timeinterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_TimeInterval}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          {\tt ESMF\_TimeInterval} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
      call c_ESMC_TimeIntervalValidate(timeinterval, opts, rc)

      end subroutine ESMF_TimeIntervalValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print out a time interval's properties

! !INTERFACE:
      subroutine ESMF_TimeIntervalPrint(timeinterval, opts, rc)

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_TimeInterval}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
      call c_ESMC_TimeIntervalPrint(timeinterval, opts, rc)

      end subroutine ESMF_TimeIntervalPrint

!------------------------------------------------------------------------------

      end module ESMF_TimeIntervalMod

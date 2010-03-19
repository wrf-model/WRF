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
      use ESMF_BaseTimeMod

      ! associated derived types
      use ESMF_FractionMod, only : ESMF_Fraction
      use ESMF_CalendarMod

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

      type ESMF_TimeInterval
        ! time interval is expressed as basetime
        type(ESMF_BaseTime) :: basetime  ! inherit base class
        ! Relative year and month fields support monthly or yearly time 
        ! intervals.  Many operations are undefined when these fields are 
        ! non-zero!  
        INTEGER :: YR                    ! relative year
   !jm Month has no meaning for an interval; get rid of it, 20100319
   !     INTEGER :: MM                    ! relative month
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

!!!!!!!!! added 20051012, JM
!      public WRFADDITION_TimeIntervalDIVQuot 
!!!!!!!!! renamed to simplify testing 20060320, TH
      public ESMF_TimeIntervalDIVQuot 

      ! This convenience routine is only used by other modules in 
      ! esmf_time_f90.  
      public ESMF_TimeIntervalIsPositive


! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 
      public operator(/)
      private ESMF_TimeIntervalQuotI

      public operator(*)
      private ESMF_TimeIntervalProdI

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

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(*)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalProdI

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
      interface operator(/)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_TimeIntervalQuotI

! !DESCRIPTION:
!     This interface overloads the / operator for the
!     {\tt ESMF\_TimeInterval} class
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
      subroutine ESMF_TimeIntervalGet(timeinterval, D, d_r8, S, S_i8, Sn, Sd, &
                                      TimeString, rc )

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval
      integer, intent(out), optional :: D
      real(ESMF_KIND_R8),     intent(out), optional :: d_r8
      integer(ESMF_KIND_I8),  intent(out), optional :: S_i8
      integer, intent(out), optional :: S
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      character*(*), optional, intent(out) :: TimeString
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
!
! Added argument to output double precision seconds, S_i8
! William.Gustafson@pnl.gov; 9-May-2008
!
!EOP
      INTEGER(ESMF_KIND_I8) :: seconds
      INTEGER :: ierr

      ierr = ESMF_SUCCESS
      seconds = timeinterval%basetime%S
      ! note that S is overwritten below (if present) if other args are also 
      ! present
      IF ( PRESENT(S) ) S = seconds
      IF ( PRESENT(S_i8) ) S_i8 = seconds
      IF ( PRESENT( D ) ) THEN
        D = seconds / SECONDS_PER_DAY
        IF ( PRESENT(S) )    S    = MOD( seconds, SECONDS_PER_DAY )
        IF ( PRESENT(S_i8) ) S_i8 = MOD( seconds, SECONDS_PER_DAY )
      ENDIF
      IF ( PRESENT( d_r8 ) ) THEN
        D_r8 = REAL( seconds, ESMF_KIND_R8 ) / &
               REAL( SECONDS_PER_DAY, ESMF_KIND_R8 )
        IF ( PRESENT(S) )    S    = MOD( seconds, SECONDS_PER_DAY )
        IF ( PRESENT(S_i8) ) S_i8 = MOD( seconds, SECONDS_PER_DAY )
      ENDIF
      IF ( PRESENT(Sn) ) THEN
        Sn = timeinterval%basetime%Sn
      ENDIF
      IF ( PRESENT(Sd) ) THEN
        Sd = timeinterval%basetime%Sd
      ENDIF
      IF ( PRESENT( timeString ) ) THEN
        CALL ESMFold_TimeIntervalGetString( timeinterval, timeString, rc=ierr )
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr
    
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
      ! locals
      INTEGER :: nfeb

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

      IF ( PRESENT(rc) ) rc = ESMF_FAILURE
      ! note that YR and MM are relative
      timeinterval%YR = 0
      IF ( PRESENT( YY ) ) THEN
        timeinterval%YR = YY
      ENDIF
!jm      timeinterval%MM = 0
!jm      IF ( PRESENT( MM ) ) THEN
!jm        timeinterval%MM = MM
!jm      ENDIF
!jm      ! Rollover months to years
!jm      IF      ( abs(timeinterval%MM) .GE. MONTHS_PER_YEAR ) THEN
!jm        timeinterval%YR = timeinterval%YR + timeinterval%MM/MONTHS_PER_YEAR
!jm        timeinterval%MM = mod(timeinterval%MM,MONTHS_PER_YEAR)
!jm      ENDIF

      timeinterval%basetime%S = 0
      ! For 365-day calendar, immediately convert years to days since we know 
      ! how to do it in this case.  
!$$$ replace this hack with something saner...
      IF ( nfeb( 2004 ) == 28 ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( 365_ESMF_KIND_I8 * &
            INT( timeinterval%YR, ESMF_KIND_I8 ) * SECONDS_PER_DAY )
        timeinterval%YR = 0
      ENDIF
      IF ( PRESENT( D ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_DAY * INT( D, ESMF_KIND_I8 ) )
      ENDIF
!$$$ Push H,M,S,Sn,Sd,MS down into BaseTime constructor from EVERYWHERE
!$$$ and THEN add ESMF scaling behavior when other args are present...  
      IF ( PRESENT( H ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_HOUR * INT( H, ESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( M ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          ( SECONDS_PER_MINUTE * INT( M, ESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( S ) ) THEN
        timeinterval%basetime%S = timeinterval%basetime%S + &
          INT( S, ESMF_KIND_I8 )
      ENDIF
      IF ( PRESENT( Sn ) .AND. ( .NOT. PRESENT( Sd ) ) ) THEN
        CALL wrf_error_fatal( &
          "ESMF_TimeIntervalSet:  Must specify Sd if Sn is specified")
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( MS ) ) THEN
        CALL wrf_error_fatal( &
          "ESMF_TimeIntervalSet:  Must not specify both Sd and MS")
      ENDIF
      timeinterval%basetime%Sn = 0
      timeinterval%basetime%Sd = 0
      IF ( PRESENT( MS ) ) THEN
        timeinterval%basetime%Sn = MS
        timeinterval%basetime%Sd = 1000_ESMF_KIND_I8
      ELSE IF ( PRESENT( Sd ) ) THEN
        timeinterval%basetime%Sd = Sd
        IF ( PRESENT( Sn ) ) THEN
          timeinterval%basetime%Sn = Sn
        ENDIF
      ENDIF
      CALL normalize_timeint( timeinterval )

      IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

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
      ! locals
      integer :: signnormtimeint
      LOGICAL :: negative
      INTEGER(ESMF_KIND_I8) :: iS, iSn, iSd, H, M, S
      character (len=1) :: signstr

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

! NOTE:  YR, MM, Sn, and Sd are not yet included in the returned string...  
!PRINT *,'DEBUG ESMFold_TimeIntervalGetString():  YR,MM,S,Sn,Sd = ', &
!        timeinterval%YR, &
!        timeinterval%MM, &
!        timeinterval%basetime%S, &
!        timeinterval%basetime%Sn, &
!        timeinterval%basetime%Sd

      negative = ( signnormtimeint( timeInterval ) == -1 )
      IF ( negative ) THEN
        iS = -timeinterval%basetime%S
        iSn = -timeinterval%basetime%Sn
        signstr = '-'
      ELSE
        iS = timeinterval%basetime%S
        iSn = timeinterval%basetime%Sn
        signstr = ''
      ENDIF 
      iSd = timeinterval%basetime%Sd

      H = mod( iS, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
      M = mod( iS, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
      S = mod( iS, SECONDS_PER_MINUTE )

!$$$here...  need to print Sn and Sd when they are used ???

      write(TimeString,FMT="(A,I10.10,'_',I3.3,':',I3.3,':',I3.3)") &
        TRIM(signstr), ( iS / SECONDS_PER_DAY ), H, M, S

!write(0,*)'TimeIntervalGetString Sn ',timeinterval%basetime%Sn,' Sd ',timeinterval%basetime%Sd

      rc = ESMF_SUCCESS

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
      CALL timeintchecknormalized( timeinterval, 'ESMF_TimeIntervalAbsValue arg1' )
      ESMF_TimeIntervalAbsValue = timeinterval
!$$$here...  move implementation into BaseTime
      ESMF_TimeIntervalAbsValue%basetime%S  = &
        abs(ESMF_TimeIntervalAbsValue%basetime%S)
      ESMF_TimeIntervalAbsValue%basetime%Sn = &
        abs(ESMF_TimeIntervalAbsValue%basetime%Sn )

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
      CALL timeintchecknormalized( timeinterval, 'ESMF_TimeIntervalNegAbsValue arg1' )
    
      ESMF_TimeIntervalNegAbsValue = timeinterval
!$$$here...  move implementation into BaseTime
      ESMF_TimeIntervalNegAbsValue%basetime%S  = &
        -abs(ESMF_TimeIntervalNegAbsValue%basetime%S)
      ESMF_TimeIntervalNegAbsValue%basetime%Sn = &
        -abs(ESMF_TimeIntervalNegAbsValue%basetime%Sn )

      end function ESMF_TimeIntervalNegAbsValue

!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!! added jm 20051012
! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder
      function ESMF_TimeIntervalDIVQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      INTEGER :: ESMF_TimeIntervalDIVQuot 

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval1
      type(ESMF_TimeInterval), intent(in) :: timeinterval2

! !LOCAL
      INTEGER :: retval, isgn, rc
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

      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalDIVQuot arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalDIVQuot arg2' )

      call ESMF_TimeIntervalSet( zero, rc=rc )
      i1 = timeinterval1
      i2 = timeinterval2
      isgn = 1
      if ( i1 .LT. zero ) then
        i1 = ESMF_TimeIntervalProdI(i1, -1)
        isgn = -isgn
      endif
      if ( i2 .LT. zero ) then
        i2 = ESMF_TimeIntervalProdI(i2, -1)
        isgn = -isgn
      endif
! repeated subtraction
      retval = 0
      DO WHILE (  i1 .GE. i2 )
        i1 = i1 - i2
        retval = retval + 1
      ENDDO
      retval = retval * isgn

      ESMF_TimeIntervalDIVQuot = retval

      end function ESMF_TimeIntervalDIVQuot
!!!!!!!!!!!!!!!!!!



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

!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() A:  S,Sn,Sd = ', &
!  timeinterval%basetime%S,timeinterval%basetime%Sn,timeinterval%basetime%Sd
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() A:  divisor = ', divisor

      CALL timeintchecknormalized( timeinterval, 'ESMF_TimeIntervalQuotI arg1' )

      IF ( divisor == 0 ) THEN
        CALL wrf_error_fatal( 'ESMF_TimeIntervalQuotI:  divide by zero' )
      ENDIF
      ESMF_TimeIntervalQuotI = timeinterval
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() B:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd
      ESMF_TimeIntervalQuotI%basetime = &
        timeinterval%basetime / divisor
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() C:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd

      CALL normalize_timeint( ESMF_TimeIntervalQuotI )
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() D:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd

      end function ESMF_TimeIntervalQuotI

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
      CALL timeintchecknormalized( timeinterval, 'ESMF_TimeIntervalProdI arg1' )

      CALL ESMF_TimeIntervalSet( ESMF_TimeIntervalProdI, rc=rc )
!$$$move this into overloaded operator(*) in BaseTime
      ESMF_TimeIntervalProdI%basetime%S  = &
        timeinterval%basetime%S * INT( multiplier, ESMF_KIND_I8 )
      ESMF_TimeIntervalProdI%basetime%Sn = &
        timeinterval%basetime%Sn * INT( multiplier, ESMF_KIND_I8 )
      ! Don't multiply Sd
      ESMF_TimeIntervalProdI%basetime%Sd = timeinterval%basetime%Sd
      CALL normalize_timeint( ESMF_TimeIntervalProdI )

      end function ESMF_TimeIntervalProdI

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalSum arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalSum arg2' )

      ESMF_TimeIntervalSum = timeinterval1
      ESMF_TimeIntervalSum%basetime = ESMF_TimeIntervalSum%basetime + &
                                      timeinterval2%basetime

      CALL normalize_timeint( ESMF_TimeIntervalSum )

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalDiff arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalDiff arg2' )

      ESMF_TimeIntervalDiff = timeinterval1
      ESMF_TimeIntervalDiff%basetime = ESMF_TimeIntervalDiff%basetime - &
                                       timeinterval2%basetime
      CALL normalize_timeint( ESMF_TimeIntervalDiff )

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalEQ arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalEQ arg2' )

!$$$here...  move all this out of Meat.F90 ?  
      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntEQ(timeinterval1, timeinterval2, ESMF_TimeIntervalEQ)

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalNE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalNE arg2' )

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntNE(timeinterval1, timeinterval2, ESMF_TimeIntervalNE)

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalLT arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalLT arg2' )

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntLT(timeinterval1, timeinterval2, ESMF_TimeIntervalLT)

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalGT arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalGT arg2' )

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntGT(timeinterval1, timeinterval2, ESMF_TimeIntervalGT)

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalLE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalLE arg2' )

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntLE(timeinterval1, timeinterval2, ESMF_TimeIntervalLE)

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
      CALL timeintchecknormalized( timeinterval1, 'ESMF_TimeIntervalGE arg1' )
      CALL timeintchecknormalized( timeinterval2, 'ESMF_TimeIntervalGE arg2' )

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeIntGE(timeinterval1, timeinterval2, ESMF_TimeIntervalGE)

      end function ESMF_TimeIntervalGE


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalIsPositive - Time interval greater than zero?

! !INTERFACE:
      function ESMF_TimeIntervalIsPositive(timeinterval)
!
! !RETURN VALUE:
      logical :: ESMF_TimeIntervalIsPositive

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in) :: timeinterval

! !LOCALS:
      type(ESMF_TimeInterval) :: zerotimeint
      integer :: rcint

! !DESCRIPTION:
!     Return true if time interval is greater than zero,  
!     false otherwise. 
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to compare
!     \end{description}
!EOP
      CALL timeintchecknormalized( timeinterval, &
                                   'ESMF_TimeIntervalIsPositive arg' )

      CALL ESMF_TimeIntervalSet ( zerotimeint, rc=rcint )
      IF ( rcint /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal( &
          'ESMF_TimeIntervalIsPositive:  ESMF_TimeIntervalSet failed' )
      ENDIF
! hack for bug in PGI 5.1-x
!      ESMF_TimeIntervalIsPositive = timeinterval > zerotimeint
      ESMF_TimeIntervalIsPositive = ESMF_TimeIntervalGT( timeinterval, &
                                                         zerotimeint )
      end function ESMF_TimeIntervalIsPositive

      end module ESMF_TimeIntervalMod



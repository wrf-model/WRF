
! "module_esmf_extensions" is responsible for yet-to-be-implemented ESMF 
! features used by the io_esmf package.  Once ESMF development is complete, 
! this module may be removed.  

! NOTE for implementation of ESMF_*GetCurrent():  
!
! This implementation uses interfaces that pass Fortran POINTERs around 
! to avoid forcing use of overloaded assignment operators for shallow 
! copies.  The goal of this approach is to be as insulated as possible 
! from ESMF object implementations.  This avoids having to explicitly 
! copy-in *AND* copy-out through the standard component init(), run(), 
! and final() interfaces just to attach references to ESMF objects to 
! other objects.  The explicit CICO *might* be required if we 
! instead attached shallow copies of the objects to other objects!  
! "Might" means it is not required now because ESMF objects are 
! implemented as simple pointers.  However, Nancy Collins says that 
! the ESMF core team plans to add more state on the Fortran side of the 
! ESMF objects, so copy-out will eventually be required.  Thus we use 
! POINTERs to attach references, as in other languages.  Why ESMF 
! component interfaces aren't passing POINTERs to Fortran objects is 
! not clear (TBH)...  
!

MODULE module_esmf_extensions

  USE ESMF

  IMPLICIT NONE

  PRIVATE


  ! private data

  ! Data for ESMF_*GetCurrent()
  ! These flags are set to .TRUE. iff current objects are valid.  
  LOGICAL, SAVE                :: current_clock_valid = .FALSE.
  TYPE(ESMF_Clock), POINTER    :: current_clock
  LOGICAL, SAVE                :: current_importstate_valid = .FALSE.
  TYPE(ESMF_State), POINTER    :: current_importstate
  LOGICAL, SAVE                :: current_exportstate_valid = .FALSE.
  TYPE(ESMF_State), POINTER    :: current_exportstate
  LOGICAL, SAVE                :: current_gridcomp_valid = .FALSE.
  TYPE(ESMF_GridComp), POINTER :: current_gridcomp

  ! Flag for "is-initialized" inquiry
  ! NOTE:  esmf_is_initialized is not reset to .FALSE. when ESMF_Finalize is called
  LOGICAL, SAVE                :: esmf_is_initialized = .FALSE.


  ! public routines
  ! These convenience interfaces have been proposed to the ESMF core team.  
  ! "get current" variants
  PUBLIC ESMF_ClockGetCurrent
  PUBLIC ESMF_ImportStateGetCurrent
  PUBLIC ESMF_ExportStateGetCurrent
  PUBLIC ESMF_GridCompGetCurrent
  ! "is-initialized" inquiry
  PUBLIC WRFU_IsInitialized

  ! extensions to standard ESMF interfaces
  ! these extensions conform to documented plans for ESMF extensions
  ! they should be removed as ESMF implementations are released
  PUBLIC WRFU_TimeGet

  ! public routines to be replaced by ESMF internal implementations
  ! These interfaces will not be public because ESMF will always be able 
  ! to call them in the right places without user intervention.  
  ! "get current" variants
  PUBLIC ESMF_ClockSetCurrent
  PUBLIC ESMF_ImportStateSetCurrent
  PUBLIC ESMF_ExportStateSetCurrent
  PUBLIC ESMF_GridCompSetCurrent
  PUBLIC ESMF_SetCurrent
  ! "is-initialized" inquiry
  PUBLIC ESMF_SetInitialized

!!!!!!!!! added 20051012, JM
  ! Need to request that this interface be added...  
  PUBLIC WRFU_TimeIntervalDIVQuot

  ! duplicated routines from esmf_time_f90
  ! move these to a common shared location later...  
  PUBLIC fraction_to_string

  ! hack for bug in PGI 5.1-x
  PUBLIC ESMF_TimeLE
  PUBLIC ESMF_TimeGE

  ! convenience function
  PUBLIC ESMF_TimeIntervalIsPositive

CONTAINS


! Add "is initialized" behavior to ESMF interface
  FUNCTION WRFU_IsInitialized()
    LOGICAL WRFU_IsInitialized
    WRFU_IsInitialized = esmf_is_initialized
  END FUNCTION WRFU_IsInitialized

! Add "is initialized" behavior to ESMF interface
! This interface will go away as it will be done inside ESMF_Initialize().  
  SUBROUTINE ESMF_SetInitialized()
    esmf_is_initialized = .TRUE.
  END SUBROUTINE ESMF_SetInitialized



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrent - Get current ESMF_Clock
! !INTERFACE:
  SUBROUTINE ESMF_ClockGetCurrent(clock, rc)
! !ARGUMENTS:
    TYPE(ESMF_Clock), POINTER      :: clock
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_Clock} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[clock]
!     Upon return this holds the {\tt ESMF\_Clock} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_clock_valid ) THEN
      clock => current_clock
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ClockGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ImportStateGetCurrent - Get current import ESMF_State
! !INTERFACE:
  SUBROUTINE ESMF_ImportStateGetCurrent(importstate, rc)
! !ARGUMENTS:
    TYPE(ESMF_State), POINTER      :: importstate
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the import {\tt ESMF\_State} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[importstate]
!     Upon return this holds the import {\tt ESMF\_State} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_importstate_valid ) THEN
      importstate => current_importstate
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ImportStateGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_ExportStateGetCurrent - Get current export ESMF_State
! !INTERFACE:
  SUBROUTINE ESMF_ExportStateGetCurrent(exportstate, rc)
! !ARGUMENTS:
    TYPE(ESMF_State), POINTER      :: exportstate
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the export {\tt ESMF\_State} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[exportstate]
!     Upon return this holds the export {\tt ESMF\_State} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_exportstate_valid ) THEN
      exportstate => current_exportstate
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_ExportStateGetCurrent
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_GridCompGetCurrent - Get current ESMF_GridComp
! !INTERFACE:
  SUBROUTINE ESMF_GridCompGetCurrent(gridcomp, rc)
! !ARGUMENTS:
    TYPE(ESMF_GridComp), POINTER   :: gridcomp
    INTEGER, INTENT(OUT), OPTIONAL :: rc
!
! !DESCRIPTION:
!   Get the {\tt ESMF\_GridComp} object of the current execution context.
!
!   The arguments are:
!   \begin{description}
!   \item[gridcomp]
!     Upon return this holds the {\tt ESMF\_GridComp} object of the current context.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ! Assume failure until success
    IF ( PRESENT( rc ) ) rc = ESMF_FAILURE
    IF ( current_gridcomp_valid ) THEN
      gridcomp => current_gridcomp
      IF ( PRESENT( rc ) ) rc = ESMF_SUCCESS
    ENDIF
  END SUBROUTINE ESMF_GridCompGetCurrent
!------------------------------------------------------------------------------




! Temporary method, to be replaced by ESMF internal implementation
! Sets the current ESMF_Clock to clock.  
  SUBROUTINE ESMF_ClockSetCurrent(clock)
    TYPE(ESMF_Clock), POINTER :: clock
    current_clock => clock
    current_clock_valid = .TRUE.
  END SUBROUTINE ESMF_ClockSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current import ESMF_State to importstate.  
  SUBROUTINE ESMF_ImportStateSetCurrent(importstate)
    TYPE(ESMF_State), POINTER :: importstate
    current_importstate => importstate
    current_importstate_valid = .TRUE.
  END SUBROUTINE ESMF_ImportStateSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current export ESMF_State to exportstate.  
  SUBROUTINE ESMF_ExportStateSetCurrent(exportstate)
    TYPE(ESMF_State), POINTER :: exportstate
    current_exportstate => exportstate
    current_exportstate_valid = .TRUE.
  END SUBROUTINE ESMF_ExportStateSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Sets the current ESMF_GridComp to gridcomp.  
  SUBROUTINE ESMF_GridCompSetCurrent(gridcomp)
    TYPE(ESMF_GridComp), POINTER :: gridcomp
    current_gridcomp => gridcomp
    current_gridcomp_valid = .TRUE.
  END SUBROUTINE ESMF_GridCompSetCurrent
!------------------------------------------------------------------------------


! Temporary method, to be replaced by ESMF internal implementation
! Convenience interface to set everything at once...  
  ! This routine sets the current ESMF_GridComp, import and export
  ! ESMF_States, and the current ESMF_Clock.
  ! NOTE:  It will be possible to remove this routine once ESMF supports
  !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(),
  !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().
  SUBROUTINE ESMF_SetCurrent( gcomp, importState, exportState, clock )
    TYPE(ESMF_GridComp), OPTIONAL, POINTER :: gcomp
    TYPE(ESMF_State),    OPTIONAL, POINTER :: importState
    TYPE(ESMF_State),    OPTIONAL, POINTER :: exportState
    TYPE(ESMF_Clock),    OPTIONAL, POINTER :: clock
    IF ( PRESENT( gcomp ) ) THEN
      CALL ESMF_GridCompSetCurrent( gcomp )
      CALL ESMF_ImportStateSetCurrent( importState )
      CALL ESMF_ExportStateSetCurrent( exportState )
      CALL ESMF_ClockSetCurrent( clock )
    ENDIF
  END SUBROUTINE ESMF_SetCurrent
!------------------------------------------------------------------------------



! begin hack for bug in PGI 5.1-x
  function ESMF_TimeLE(time1, time2)
    logical :: ESMF_TimeLE
    type(ESMF_Time), intent(in) :: time1
    type(ESMF_Time), intent(in) :: time2
    ESMF_TimeLE = (time1.LE.time2)
  end function ESMF_TimeLE
  function ESMF_TimeGE(time1, time2)
    logical :: ESMF_TimeGE
    type(ESMF_Time), intent(in) :: time1
    type(ESMF_Time), intent(in) :: time2
    ESMF_TimeGE = (time1.GE.time2)
  end function ESMF_TimeGE
! end hack for bug in PGI 5.1-x

! convenience function
  function ESMF_TimeIntervalIsPositive(timeinterval)
    logical :: ESMF_TimeIntervalIsPositive
    type(ESMF_TimeInterval), intent(in) :: timeinterval
    type(ESMF_TimeInterval) :: zerotimeint
    integer :: rcint
    CALL ESMF_TimeIntervalSet ( zerotimeint, rc=rcint )
    ESMF_TimeIntervalIsPositive = (timeinterval .GT. zerotimeint)
  end function ESMF_TimeIntervalIsPositive




! Note:  this implementation is largely duplicated from external/esmf_time_f90
!!!!!!!!!!!!!!!!!! added jm 20051012
! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder
      function WRFU_TimeIntervalDIVQuot(timeinterval1, timeinterval2)

! !RETURN VALUE:
      INTEGER :: WRFU_TimeIntervalDIVQuot

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
      call ESMF_TimeIntervalSet( zero, rc=rc )
      i1 = timeinterval1
      i2 = timeinterval2
      isgn = 1
      if ( i1 .LT. zero ) then
        i1 = i1 * (-1)
        isgn = -isgn
      endif
      if ( i2 .LT. zero ) then
        i2 = i2 * (-1)
        isgn = -isgn
      endif
! repeated subtraction
      retval = 0
      DO WHILE (  i1 .GE. i2 )
        i1 = i1 - i2
        retval = retval + 1
      ENDDO
      retval = retval * isgn

      WRFU_TimeIntervalDIVQuot = retval

      end function WRFU_TimeIntervalDIVQuot
!!!!!!!!!!!!!!!!!!



  ! implementations of extensions to standard ESMF interfaces
  ! these extensions conform to documented plans for ESMF extensions
  ! they should be removed as ESMF implementations are released

      ! extend ESMF_TimeGet() to make dayOfYear_r8 work...  
      subroutine WRFU_TimeGet(time, yy, yy_i8, &
                              mm, dd, &
                              d, d_i8, &
                              h, m, &
                              s, s_i8, &
                              ms, us, ns, &
                              d_r8, h_r8, m_r8, s_r8, &
                              ms_r8, us_r8, ns_r8, &
                              sN, sD, &
! 5.2.0r                              calendar, calendarType, timeZone, &
                              calendar, timeZone, &
                              timeString, timeStringISOFrac, &
                              dayOfWeek, midMonth, &
                              dayOfYear,  dayOfYear_r8, &
                              dayOfYear_intvl, rc)
      type(ESMF_Time),         intent(inout)            :: time
      integer(ESMF_KIND_I4),   intent(out), optional :: yy
      integer(ESMF_KIND_I8),   intent(out), optional :: yy_i8
      integer,                 intent(out), optional :: mm
      integer,                 intent(out), optional :: dd
      integer(ESMF_KIND_I4),   intent(out), optional :: d
      integer(ESMF_KIND_I8),   intent(out), optional :: d_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: h
      integer(ESMF_KIND_I4),   intent(out), optional :: m
      integer(ESMF_KIND_I4),   intent(out), optional :: s
      integer(ESMF_KIND_I8),   intent(out), optional :: s_i8
      integer(ESMF_KIND_I4),   intent(out), optional :: ms
      integer(ESMF_KIND_I4),   intent(out), optional :: us
      integer(ESMF_KIND_I4),   intent(out), optional :: ns
      real(ESMF_KIND_R8),      intent(out), optional :: d_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: h_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: m_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: s_r8  ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: ms_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: us_r8 ! not implemented
      real(ESMF_KIND_R8),      intent(out), optional :: ns_r8 ! not implemented
      integer(ESMF_KIND_I4),   intent(out), optional :: sN
      integer(ESMF_KIND_I4),   intent(out), optional :: sD
      type(ESMF_Calendar),     intent(out), optional :: calendar
! 5.2.0r      type(ESMF_CalendarType), intent(out), optional :: calendarType
      integer,                 intent(out), optional :: timeZone
      character (len=*),       intent(out), optional :: timeString
      character (len=*),       intent(out), optional :: timeStringISOFrac
      integer,                 intent(out), optional :: dayOfWeek
      type(ESMF_Time),         intent(out), optional :: midMonth
      integer(ESMF_KIND_I4),   intent(out), optional :: dayOfYear
      real(ESMF_KIND_R8),      intent(out), optional :: dayOfYear_r8 ! NOW implemented
      type(ESMF_TimeInterval), intent(out), optional :: dayOfYear_intvl
      integer,                 intent(out), optional :: rc
      REAL(ESMF_KIND_R8) :: rsec
      INTEGER(ESMF_KIND_I4) :: year, seconds, Sn, Sd
      INTEGER(ESMF_KIND_I8), PARAMETER :: SECONDS_PER_DAY = 86400_ESMF_KIND_I8

      CALL ESMF_TimeGet(time=time, yy=yy, yy_i8=yy_i8, &
                                    mm=mm, dd=dd, &
                                    d=d, d_i8=d_i8, &
                                    h=h, m=m, &
                                    s=s, s_i8=s_i8, &
                                    ms=ms, us=us, ns=ns, &
                                    d_r8=d_r8, h_r8=h_r8, m_r8=m_r8, s_r8=s_r8, &
                                    ms_r8=ms_r8, us_r8=us_r8, ns_r8=ns_r8, &
                                    sN=sN, sD=sD, &
! 5.2.0r                                    calendar=calendar, calendarType=calendarType, timeZone=timeZone, &
                                    calendar=calendar,                            timeZone=timeZone, &
                                    timeString=timeString, timeStringISOFrac=timeStringISOFrac, &
                                    dayOfWeek=dayOfWeek, midMonth=midMonth, &
                                    dayOfYear=dayOfYear,  dayOfYear_R8=dayOfYear_r8, &
                                    dayOfYear_intvl=dayOfYear_intvl, rc=rc)
      IF ( rc == ESMF_SUCCESS ) THEN
        IF ( PRESENT( dayOfYear_r8 ) ) THEN
          ! get seconds since start of year and fractional seconds
          CALL ESMF_TimeGet( time, yy=year, s=seconds, sN=Sn, sD=Sd, rc=rc )
          IF ( rc == ESMF_SUCCESS ) THEN
            ! 64-bit IEEE 754 has 52-bit mantisssa -- only need 25 bits to hold
            ! number of seconds in a year...
            rsec = REAL( seconds, ESMF_KIND_R8 )
            IF ( PRESENT( Sd ) ) THEN
              IF ( Sd /= 0 ) THEN
                rsec = rsec + ( REAL( Sn, ESMF_KIND_R8 ) / REAL( Sd, ESMF_KIND_R8 ) )
              ENDIF
            ENDIF
            dayOfYear_r8 = rsec / REAL( SECONDS_PER_DAY, ESMF_KIND_R8 )
            ! start at 1
            dayOfYear_r8 = dayOfYear_r8 + 1.0_ESMF_KIND_R8
          ENDIF
        ENDIF
      ENDIF

      end subroutine WRFU_TimeGet

!------------------------------------------------------------------------------


! duplicated routines from esmf_time_f90
! move these to a common shared location later...  

! Convert fraction to string with leading sign.
! If fraction simplifies to a whole number or if
! denominator is zero, return empty string.
! INTEGER*8 interface.  
SUBROUTINE fraction_to_stringi8( numerator, denominator, frac_str )
  INTEGER(ESMF_KIND_I8), INTENT(IN) :: numerator
  INTEGER(ESMF_KIND_I8), INTENT(IN) :: denominator
  CHARACTER (LEN=*), INTENT(OUT) :: frac_str
  IF ( denominator > 0 ) THEN
    IF ( mod( numerator, denominator ) /= 0 ) THEN
      IF ( numerator > 0 ) THEN
        WRITE(frac_str,FMT="('+',I2.2,'/',I2.2)") abs(numerator), denominator
      ELSE   ! numerator < 0
        WRITE(frac_str,FMT="('-',I2.2,'/',I2.2)") abs(numerator), denominator
      ENDIF
    ELSE   ! includes numerator == 0 case
      frac_str = ''
    ENDIF
  ELSE   ! no-fraction case
    frac_str = ''
  ENDIF
END SUBROUTINE fraction_to_stringi8


! Convert fraction to string with leading sign.
! If fraction simplifies to a whole number or if
! denominator is zero, return empty string.
! INTEGER interface.  
SUBROUTINE fraction_to_string( numerator, denominator, frac_str )
  INTEGER, INTENT(IN) :: numerator
  INTEGER, INTENT(IN) :: denominator
  CHARACTER (LEN=*), INTENT(OUT) :: frac_str
  ! locals
  INTEGER(ESMF_KIND_I8) :: numerator_i8, denominator_i8
  numerator_i8 = INT( numerator, ESMF_KIND_I8 )
  denominator_i8 = INT( denominator, ESMF_KIND_I8 )
  CALL fraction_to_stringi8( numerator_i8, denominator_i8, frac_str )
END SUBROUTINE fraction_to_string

! end of duplicated routines from esmf_time_f90


END MODULE module_esmf_extensions


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
!     ESMF Calendar Module
      module ESMF_CalendarMod
!
!==============================================================================
!
! This file contains the Calendar class definition and all Calendar class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: ESMF_CalendarMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation
!
! See {\tt ../include/ESMC\_Calendar.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod,     only : ESMF_BaseTime

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------



      INTEGER, PARAMETER :: MONTHS_PER_YEAR = 12
      INTEGER, DIMENSION(MONTHS_PER_YEAR), PARAMETER :: mday   &
                          = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, DIMENSION(MONTHS_PER_YEAR), PARAMETER :: mdayleap &
                          = (/31,29,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, DIMENSION(365) :: daym
      INTEGER, DIMENSION(366) :: daymleap
      INTEGER, DIMENSION(0:MONTHS_PER_YEAR)             :: mdaycum
      INTEGER, DIMENSION(0:MONTHS_PER_YEAR)             :: mdayleapcum


!------------------------------------------------------------------------------
!     ! ESMF_CalendarType
!
!     ! F90 "enum" type to match C++ ESMC_CalendarType enum

      type ESMF_CalendarType
      sequence
      private
        integer :: caltype
      end type

      type(ESMF_CalendarType), parameter :: &
                               ESMF_CAL_GREGORIAN =  ESMF_CalendarType(1), &
                               ESMF_CAL_JULIAN =     ESMF_CalendarType(2), &
                           ! like Gregorian, except Feb always has 28 days
                               ESMF_CAL_NOLEAP =     ESMF_CalendarType(3), & 
                           ! 12 months, 30 days each
                               ESMF_CAL_360DAY =     ESMF_CalendarType(4), & 
                           ! user defined
                               ESMF_CAL_GENERIC =    ESMF_CalendarType(5), &
                           ! track base time seconds only
                               ESMF_CAL_NOCALENDAR = ESMF_CalendarType(6)

!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class type to match C++ Calendar class in size only;
!     !  all dereferencing within class is performed by C++ implementation
!
!     ! Equivalent sequence and kind to C++:
!------------------------------------------------------------------------------
!
!     ! ESMF_DaysPerYear
!
      type ESMF_DaysPerYear
      sequence
      private
        integer :: D = 0    ! whole days per year
        integer :: Dn = 0   ! fractional days per year numerator
        integer :: Dd = 1   ! fractional days per year denominator
      end type              ! e.g. for Venus, D=0, Dn=926, Dd=1000
!
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!     ! F90 class to match C++ Calendar class in size and sequence
!
      type ESMF_Calendar
      sequence
      private
        type(ESMF_CalendarType) :: Type
        logical :: Set = .false.
        integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth = 0
        integer :: SecondsPerDay = 0
        integer :: SecondsPerYear = 0
        type(ESMF_DaysPerYear) :: DaysPerYear
      end type


!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public mday
      public mdayleap
      public daym
      public daymleap
      public mdaycum
      public mdayleapcum
      public ESMF_CalendarType
      public ESMF_CAL_GREGORIAN, ESMF_CAL_NOLEAP, &
             ESMF_CAL_360DAY, ESMF_CAL_NOCALENDAR
!      public ESMF_CAL_JULIAN
!      public ESMF_CAL_GENERIC
      public ESMF_Calendar

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_CalendarCreate
      public ESMF_CalendarSet
!      public ESMF_CalendarSetGeneric

! Required inherited and overridden ESMF_Base class methods

!      public ESMF_CalendarRead
!      public ESMF_CalendarWrite
      public ESMF_CalendarInitialized ! Only in this implementation, intended
                                      ! to be private within ESMF methods
      public ESMF_CalendarValidate
      public ESMF_CalendarPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================

      contains


!==============================================================================
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new ESMF Calendar of built-in type

! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
      function ESMF_CalendarCreate(name, calendartype, rc)

! !RETURN VALUE:
      type(ESMF_Calendar) :: ESMF_CalendarCreate

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_CalendarType), intent(in)            :: calendartype
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates and sets a {\tt calendar} to the given built-in
!     {\tt ESMF\_CalendarType}. 
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_CalendarCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created calendar.  If not specified, a
!          default unique name will be generated: "CalendarNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[calendartype]
!          The built-in {\tt ESMF\_CalendarType}.  Valid values are:
!            {\tt ESMF\_CAL\_360DAY}, {\tt ESMF\_CAL\_GREGORIAN},
!            {\tt ESMF\_CAL\_JULIANDAY}, {\tt ESMF\_CAL\_NOCALENDAR}, and
!            {\tt ESMF\_CAL\_NOLEAP}.
!          See the "Time Manager Reference" document for a description of
!          each calendar type.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      type(ESMF_DaysPerYear) :: dayspy

      if ( present(rc) ) rc = ESMF_FAILURE
! Calendar is hard-coded.  Use ESMF library if more flexibility is needed.  
#ifdef NO_LEAP_CALENDAR
      if ( calendartype%caltype  /= ESMF_CAL_NOLEAP%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows ESMF_CAL_NOLEAP'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'NO_LEAP calendar type is = ', ESMF_CAL_NOLEAP%caltype
         return
      end if
      ESMF_CalendarCreate%Type = ESMF_CAL_NOLEAP
#else
      if ( calendartype%caltype  /= ESMF_CAL_GREGORIAN%caltype ) then
         write(6,*) 'Not a valid calendar type for this implementation'
         write(6,*) 'This implementation only allows ESMF_CAL_GREGORIAN'
         write(6,*) 'calender type set to     = ', calendartype%caltype
         write(6,*) 'GREGORIAN calendar type is = ', ESMF_CAL_GREGORIAN%caltype
         return
      end if
      ESMF_CalendarCreate%Type = ESMF_CAL_GREGORIAN
#endif
      ESMF_CalendarCreate%Set = .true.
      ESMF_CalendarCreate%SecondsPerDay = SECONDS_PER_DAY
! DaysPerYear and SecondsPerYear are incorrect for Gregorian calendars...  
      dayspy%D = size(daym)
      dayspy%Dn = 0
      dayspy%Dd = 1
      ESMF_CalendarCreate%DaysPerYear = dayspy
      ESMF_CalendarCreate%SecondsPerYear = ESMF_CalendarCreate%SecondsPerDay &
                                       * dayspy%D
      ESMF_CalendarCreate%DaysPerMonth(:) = mday(:)

      if ( present(rc) ) rc = ESMF_SUCCESS

      end function ESMF_CalendarCreate


!==============================================================================
!BOP
! !IROUTINE: ESMF_CalendarInitialized - check if calendar was created

! !INTERFACE:
      function ESMF_CalendarInitialized(calendar)

! !RETURN VALUE:
      logical ESMF_CalendarInitialized

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in)            :: calendar

! !DESCRIPTION:
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
        ESMF_CalendarInitialized = calendar%set
        if ( calendar%SecondsPerDay == 0 ) &
              ESMF_CalendarInitialized = .false.

     end function ESMF_CalendarInitialized


!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSet - Initializes the calendar type

! !INTERFACE:
      subroutine ESMF_CalendarSet(calendar, Type, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      type(ESMF_CalendarType), intent(in) :: Type
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Calendar} to the given {\tt ESMF\_Calendar} type
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize
!     \item[Type]
!          The {\tt CalendarType} ESMF\_CAL\_GREGORIAN, ESMF\_CAL\_JULIAN, etc.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!    
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
    
!     invoke C to C++ entry point
!      call c_ESMC_CalendarSet(calendar, Type, rc)
    
      end subroutine ESMF_CalendarSet
    
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CalendarSetGeneric - Initialize calendar to user-type

! !INTERFACE:
      subroutine ESMF_CalendarSetGeneric(calendar, DaysPerMonth, &
                                          SecondsPerDay, DaysPerYear, &
                                          DaysPerYearDn, DaysPerYearDd, rc)
! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      integer, dimension(MONTHS_PER_YEAR), intent(in) :: DaysPerMonth
      integer, intent(in) :: SecondsPerDay
      integer, intent(in) :: DaysPerYear
      integer, intent(in) :: DaysPerYearDn
      integer, intent(in) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Calendar} to a user-specified type
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          The object instance to initialize
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

!     invoke C to C++ entry point
!      call c_ESMC_CalendarSetGeneric(calendar, DaysPerMonth, &
!                                      SecondsPerDay, DaysPerYear, &
!                                      DaysPerYearDn, DaysPerYearDd, rc)
    
      end subroutine ESMF_CalendarSetGeneric
    
!------------------------------------------------------------------------------
! 
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarRead - Restore a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarRead(calendar, Type, DaysPerMonth, &
                           SecondsPerDay, DaysPerYear, &
                           DaysPerYearDn, DaysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(out) :: calendar
      type(ESMF_CalendarType), intent(in) :: Type
      integer, dimension(MONTHS_PER_YEAR), intent(in) :: DaysPerMonth
      integer, intent(in) :: SecondsPerDay
      integer, intent(in) :: DaysPerYear
      integer, intent(in) :: DaysPerYearDn
      integer, intent(in) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a restore on a {\tt ESMF\_Calendar}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to restore
!     \item[Type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!          ESMF\_CAL\_JULIAN, etc.
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
!      call c_ESMC_CalendarRead(calendar, Type, DaysPerMonth, &
!                               SecondsPerDay, DaysPerYear, &
!                               DaysPerYearDn, DaysPerYearDd, rc)

      end subroutine ESMF_CalendarRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarWrite - Save a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarWrite(calendar, Type, DaysPerMonth, &
                            SecondsPerDay, DaysPerYear, &
                            DaysPerYearDn, DaysPerYearDd, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      type(ESMF_CalendarType), intent(out) :: Type
      integer, dimension(MONTHS_PER_YEAR), intent(out) :: DaysPerMonth
      integer, intent(out) :: SecondsPerDay
      integer, intent(out) :: DaysPerYear
      integer, intent(out) :: DaysPerYearDn
      integer, intent(out) :: DaysPerYearDd
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a save on a {\tt ESMF\_Calendar}'s properties
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to save
!     \item[Type]
!          The {\tt ESMF\_CalendarType} ESMF\_CAL\_GREGORIAN,
!           ESMF\_CAL\_JULIAN, etc.
!     \item[DaysPerMonth]
!          Integer array of days per month, for each of the 12 months
!     \item[SecondsPerDay]
!          Integer number of seconds per day
!     \item[DaysPerYear]
!          Integer number of days per year
!     \item[DaysPerYearDn]
!          Integer fractional number of days per year (numerator)
!     \item[DaysPerYearDd]
!          Integer fractional number of days per year (denominator)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      call c_ESMC_CalendarWrite(calendar, Type, DaysPerMonth, &
                                SecondsPerDay, DaysPerYear, &
                                DaysPerYearDn, DaysPerYearDd, rc)

      end subroutine ESMF_CalendarWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarValidate - Validate a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarValidate(calendar, opts, rc)
 
! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Calendar}'s properties
! 
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
      call c_ESMC_CalendarValidate(calendar, opts, rc)

      end subroutine ESMF_CalendarValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_CalendarPrint - Print out a calendar's properties

! !INTERFACE:
      subroutine ESMF_CalendarPrint(calendar, opts, rc)

! !ARGUMENTS:
      type(ESMF_Calendar), intent(in) :: calendar
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Calendar}'s  
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[calendar]
!          {\tt ESMF\_Calendar} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
  
      call c_ESMC_CalendarPrint(calendar, opts, rc)

      end subroutine ESMF_CalendarPrint
      
!------------------------------------------------------------------------------

      end module ESMF_CalendarMod

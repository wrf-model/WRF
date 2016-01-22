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
      use ESMF_BaseTimeMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------



      INTEGER, PARAMETER :: MONTHS_PER_YEAR = 12
      INTEGER, PARAMETER :: mday(MONTHS_PER_YEAR)   &
                          = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, PARAMETER :: mdayleap(MONTHS_PER_YEAR) &
                          = (/31,29,31,30,31,30,31,31,30,31,30,31/)
      INTEGER, DIMENSION(365) :: daym
      INTEGER, DIMENSION(366) :: daymleap
      INTEGER :: mdaycum(0:MONTHS_PER_YEAR)
      INTEGER :: mdayleapcum(0:MONTHS_PER_YEAR)
      TYPE(ESMF_BaseTime), TARGET :: monthbdys(0:MONTHS_PER_YEAR)
      TYPE(ESMF_BaseTime), TARGET :: monthbdysleap(0:MONTHS_PER_YEAR)


!------------------------------------------------------------------------------
!     ! ESMF_CalendarType
!
!     ! F90 "enum" type to match C++ ESMC_CalendarType enum

      type ESMF_CalendarType
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
!------------------------------------------------------------------------------
!
!     ! ESMF_DaysPerYear
!
      type ESMF_DaysPerYear
      private
        integer :: D        ! whole days per year
! Fractional days-per-year are not yet used in this implementation.  
!        integer :: Dn       ! fractional days per year numerator
!        integer :: Dd       ! fractional days per year denominator
      end type              ! e.g. for Venus, D=0, Dn=926, Dd=1000
!
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!
      type ESMF_Calendar
      private
        type(ESMF_CalendarType) :: Type
! TBH:  When NO_DT_COMPONENT_INIT is set, code that uses F95 compile-time 
! TBH:  initialization of components of derived types is not included.  
! TBH:  Some older compilers, like PGI 5.x do not support this F95 feature.  
#ifdef NO_DT_COMPONENT_INIT
        logical :: Set
#else
        logical :: Set = .false.
#endif
        integer, dimension(MONTHS_PER_YEAR) :: DaysPerMonth
        integer :: SecondsPerDay
        integer :: SecondsPerYear
        type(ESMF_DaysPerYear) :: DaysPerYear
      end type

!------------------------------------------------------------------------------
! !PUBLIC DATA:
   TYPE(ESMF_Calendar), public, save, pointer :: defaultCal   ! Default Calendar


!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public MONTHS_PER_YEAR
      public mday
      public mdayleap
      public monthbdys
      public monthbdysleap
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

! Required inherited and overridden ESMF_Base class methods

      public ESMF_CalendarInitialized ! Only in this implementation, intended
                                      ! to be private within ESMF methods
!EOPI

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
! Calendar type is hard-coded.  Use ESMF library if more flexibility is 
! needed.  
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
! This is a bug on some systems -- need initial value set by compiler at 
! startup.  
! However, note that some older compilers do not support compile-time 
! initialization of data members of Fortran derived data types.  For example, 
! PGI 5.x compilers do not support this F95 feature.  See 
! NO_DT_COMPONENT_INIT.  
      ESMF_CalendarCreate%Set = .true.
      ESMF_CalendarCreate%SecondsPerDay = SECONDS_PER_DAY
! DaysPerYear and SecondsPerYear are incorrect for Gregorian calendars...  
      dayspy%D = size(daym)
!TBH:  TODO:  Replace DaysPerYear and SecondsPerYear with methods 
!TBH:  TODO:  since they only make sense for the NO_LEAP calendar!  
      ESMF_CalendarCreate%DaysPerYear = dayspy
      ESMF_CalendarCreate%SecondsPerYear = ESMF_CalendarCreate%SecondsPerDay &
                                       * dayspy%D
!TBH:  TODO:  use mdayleap for leap-year calendar
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
! Note that return value from this function will be bogus for older compilers 
! that do not support compile-time initialization of data members of Fortran 
! derived data types.  For example, PGI 5.x compilers do not support this F95 
! feature.  At the moment, the call to this fuction is #ifdefd out when the 
! leap-year calendar is used so this is not an issue for WRF (see 
! NO_DT_COMPONENT_INIT).  
        ESMF_CalendarInitialized = calendar%set

     end function ESMF_CalendarInitialized

      end module ESMF_CalendarMod

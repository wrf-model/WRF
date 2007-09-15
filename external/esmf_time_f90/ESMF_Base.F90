!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA license.
!
! ESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module ESMF_BaseMod
 
!BOP
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: ESMF_SUCCESS = 0, ESMF_FAILURE = -1
      integer, parameter :: ESMF_MAXSTR = 128
      integer, parameter :: ESMF_MAXDIM = 7, &
                            ESMF_MAXDECOMPDIM=3, &
                            ESMF_MAXGRIDDIM=2
     
      integer, parameter :: ESMF_MAJOR_VERSION = 2
      integer, parameter :: ESMF_MINOR_VERSION = 1
      integer, parameter :: ESMF_REVISION      = 1
      integer, parameter :: ESMF_PATCHLEVEL    = 0
      character(32), parameter :: ESMF_VERSION_STRING = "2.1.1"

!------------------------------------------------------------------------------
!
      type ESMF_Status
      private
          integer :: status
      end type

      type(ESMF_Status), parameter :: ESMF_STATE_UNINIT = ESMF_Status(1), &
                                      ESMF_STATE_READY = ESMF_Status(2), &
                                      ESMF_STATE_UNALLOCATED = ESMF_Status(3), &
                                      ESMF_STATE_ALLOCATED = ESMF_Status(4), &
                                      ESMF_STATE_BUSY = ESMF_Status(5), &
                                      ESMF_STATE_INVALID = ESMF_Status(6)
 
!------------------------------------------------------------------------------
!
      type ESMF_Pointer
      private
          integer*8 :: ptr
      end type

      type(ESMF_Pointer), parameter :: ESMF_NULL_POINTER = ESMF_Pointer(0), &
                                       ESMF_BAD_POINTER = ESMF_Pointer(-1)


!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      type ESMF_DataType
      !!private
          integer :: dtype
      end type

      type(ESMF_DataType), parameter :: ESMF_DATA_INTEGER = ESMF_DataType(1), &
                                        ESMF_DATA_REAL = ESMF_DataType(2), &
                                        ESMF_DATA_LOGICAL = ESMF_DataType(3), &
                                        ESMF_DATA_CHARACTER = ESMF_DataType(4)

!------------------------------------------------------------------------------

      integer, parameter :: &
                   ESMF_KIND_I1 = selected_int_kind(2), &
                   ESMF_KIND_I2 = selected_int_kind(4), &
                   ESMF_KIND_I4 = selected_int_kind(9), &
                   ESMF_KIND_I8 = selected_int_kind(18), &
                   ESMF_KIND_R4 = selected_real_kind(3,25), &
                   ESMF_KIND_R8 = selected_real_kind(6,45), &
                   ESMF_KIND_C8 = selected_real_kind(3,25), &
                   ESMF_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

      type ESMF_DataValue
      private
          type(ESMF_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=ESMF_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

!------------------------------------------------------------------------------
!
      type ESMF_Attribute
      private
          character (len=ESMF_MAXSTR) :: attr_name
          type (ESMF_DataType) :: attr_type
          type (ESMF_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
      type ESMF_AxisIndex
!     !!private
          integer :: l
          integer :: r
          integer :: max
          integer :: decomp
          integer :: gstart
      end type

      !! TODO: same comment as above.
      type ESMF_MemIndex
!     !!private
          integer :: l
          integer :: r
          integer :: str
          integer :: num
      end type

!------------------------------------------------------------------------------
!
      type ESMF_BasePointer
      private
          integer*8 :: base_ptr
      end type

      integer :: global_count = 0

!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h
      type ESMF_Logical
      private
          integer :: value
      end type

      type(ESMF_Logical), parameter :: ESMF_TF_UNKNOWN  = ESMF_Logical(1), &
                                       ESMF_TF_TRUE     = ESMF_Logical(2), &
                                       ESMF_TF_FALSE    = ESMF_Logical(3)

!------------------------------------------------------------------------------
!
      type ESMF_Base
      private
         integer :: ID
         integer :: ref_count
         type (ESMF_Status) :: base_status
         character (len=ESMF_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public ESMF_STATE_INVALID
!      public ESMF_STATE_UNINIT, ESMF_STATE_READY, &
!             ESMF_STATE_UNALLOCATED, ESMF_STATE_ALLOCATED, &
!             ESMF_STATE_BUSY

      public ESMF_DATA_INTEGER, ESMF_DATA_REAL, &
             ESMF_DATA_LOGICAL, ESMF_DATA_CHARACTER

      public ESMF_KIND_I1, ESMF_KIND_I2, ESMF_KIND_I4, ESMF_KIND_I8, & 
             ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_C8, ESMF_KIND_C16

      public ESMF_NULL_POINTER, ESMF_BAD_POINTER


      public ESMF_FAILURE, ESMF_SUCCESS
      public ESMF_MAXSTR
      public ESMF_MAXDIM, ESMF_MAXDECOMPDIM, ESMF_MAXGRIDDIM
     
      public ESMF_MAJOR_VERSION, ESMF_MINOR_VERSION, ESMF_REVISION
      public ESMF_VERSION_STRING 

      public ESMF_Status, ESMF_Pointer, ESMF_DataType
      public ESMF_DataValue, ESMF_Attribute
!      public ESMF_MemIndex
!      public ESMF_BasePointer
      public ESMF_Base

      public ESMF_AxisIndex, ESMF_AxisIndexGet
!      public ESMF_AxisIndexInit
      public ESMF_Logical
!      public ESMF_TF_TRUE, ESMF_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public ESMF_BaseInit
   
!      public ESMF_BaseGetConfig
!      public ESMF_BaseSetConfig

!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

!      public ESMF_BaseSetStatus
!      public ESMF_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Attribute methods
      public ESMF_AttributeSet
      public ESMF_AttributeGet
      public ESMF_AttributeGetCount
      public ESMF_AttributeGetbyNumber
      public ESMF_AttributeGetNameList
      public ESMF_AttributeSetList
      public ESMF_AttributeGetList
      public ESMF_AttributeSetObjectList
      public ESMF_AttributeGetObjectList
      public ESMF_AttributeCopy
      public ESMF_AttributeCopyAll
 
!  Misc methods
      public ESMF_SetName
      public ESMF_GetName
      public ESMF_SetPointer
      public ESMF_SetNullPointer
      public ESMF_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public ESMF_StatusString, ESMF_DataTypeString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!
!EOP

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
 module procedure ESMF_sfeq
 module procedure ESMF_dteq
 module procedure ESMF_pteq
 module procedure ESMF_tfeq
 module procedure ESMF_aieq
end interface

interface operator (.ne.)
 module procedure ESMF_sfne
 module procedure ESMF_dtne
 module procedure ESMF_ptne
 module procedure ESMF_tfne
 module procedure ESMF_aine
end interface

interface assignment (=)
 module procedure ESMF_dtas
 module procedure ESMF_ptas
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two ESMF_Status flags to see if they're the same or not

function ESMF_sfeq(sf1, sf2)
 logical ESMF_sfeq
 type(ESMF_Status), intent(in) :: sf1, sf2

 ESMF_sfeq = (sf1%status .eq. sf2%status)
end function

function ESMF_sfne(sf1, sf2)
 logical ESMF_sfne
 type(ESMF_Status), intent(in) :: sf1, sf2

 ESMF_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_DataTypes to see if they're the same or not

function ESMF_dteq(dt1, dt2)
 logical ESMF_dteq
 type(ESMF_DataType), intent(in) :: dt1, dt2

 ESMF_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function ESMF_dtne(dt1, dt2)
 logical ESMF_dtne
 type(ESMF_DataType), intent(in) :: dt1, dt2

 ESMF_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine ESMF_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(ESMF_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine


!------------------------------------------------------------------------------
! function to compare two ESMF_Pointers to see if they're the same or not

function ESMF_pteq(pt1, pt2)
 logical ESMF_pteq
 type(ESMF_Pointer), intent(in) :: pt1, pt2

 ESMF_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function ESMF_ptne(pt1, pt2)
 logical ESMF_ptne
 type(ESMF_Pointer), intent(in) :: pt1, pt2

 ESMF_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine ESMF_ptas(ptval, intval)
 type(ESMF_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

!------------------------------------------------------------------------------
! function to compare two ESMF_Logicals to see if they're the same or not
! also need assignment to real f90 logical?

function ESMF_tfeq(tf1, tf2)
 logical ESMF_tfeq
 type(ESMF_Logical), intent(in) :: tf1, tf2

 ESMF_tfeq = (tf1%value .eq. tf2%value)
end function

function ESMF_tfne(tf1, tf2)
 logical ESMF_tfne
 type(ESMF_Logical), intent(in) :: tf1, tf2

 ESMF_tfne = (tf1%value .ne. tf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two ESMF_AxisIndex to see if they're the same or not

function ESMF_aieq(ai1, ai2)
 logical ESMF_aieq
 type(ESMF_AxisIndex), intent(in) :: ai1, ai2

 ESMF_aieq = ((ai1%l .eq. ai2%l) .and. &
              (ai1%r .eq. ai2%r) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%decomp .eq. ai2%decomp) .and. &
              (ai1%gstart .eq. ai2%gstart))

end function

function ESMF_aine(ai1, ai2)
 logical ESMF_aine
 type(ESMF_AxisIndex), intent(in) :: ai1, ai2

 ESMF_aine = ((ai1%l .ne. ai2%l) .or. &
              (ai1%r .ne. ai2%r) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%decomp .ne. ai2%decomp) .or. &
              (ai1%gstart .ne. ai2%gstart))

end function

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BaseInit - initialize a Base object
!
! !INTERFACE:
      subroutine ESMF_BaseInit(base, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: base                 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      global_count = global_count + 1
      base%ID = global_count
      base%ref_count = 1
      base%base_status = ESMF_STATE_READY
      base%name = "undefined"

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BaseInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_SetName - set the name of this object
!
! !INTERFACE:
      subroutine ESMF_SetName(anytype, name, namespace, rc)
!
! !ARGUMENTS:
      type(ESMF_Base) :: anytype                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt ESMF_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
! 

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present   
      character (len = ESMF_MAXSTR) :: ournamespace ! Namespace if not given
      character (len = ESMF_MAXSTR) :: defaultname  ! Name if not given
      integer, save :: seqnum = 0       ! HACK - generate uniq names
                                        ! but not coordinated across procs

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!      
!     ! Construct a default namespace if one is not given
      if((.not. present(namespace)) .or. (namespace .eq. "")) then
          ournamespace = "global"
      else
          ournamespace = namespace
      endif
!     ! Construct a default name if one is not given
      if((.not. present(name)) .or. (name .eq. "")) then

          write(defaultname, 20) trim(ournamespace), seqnum
20        format(A,I3.3)
          seqnum = seqnum + 1
          anytype%name = defaultname
      else
          anytype%name = name
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GetName - get the name of this object
!
! !INTERFACE:
      subroutine ESMF_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine ESMF_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
      subroutine ESMF_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype           ! any ESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(ESMF_DataType), intent(out) :: type             ! all possible data types
      type(ESMF_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine ESMF_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
      subroutine ESMF_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine ESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an ESMF object's attribute by num ber
!
! !INTERFACE:
      subroutine ESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataType), intent(out) :: type               ! all possible data types
      type(ESMF_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 

      end subroutine ESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_AttributeGetNameList - get an ESMF object's attribute name list
!
! !INTERFACE:
      subroutine ESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine ESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine ESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetList - get an ESMF object's attributes
!
! !INTERFACE:
      subroutine ESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: anytype             ! any ESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine ESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
      subroutine ESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any ESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(ESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine ESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine ESMF_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(ESMF_Base), intent(in) :: source              ! any ESMF type
      type(ESMF_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine ESMF_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(ESMF_Base), intent(in) :: source              ! any ESMF type
      type(ESMF_Base), intent(in) :: destination         ! any ESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine ESMF_AttributeCopyAll

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine ESMF_AxisIndexInit(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      ai%l = l
      ai%r = r
      ai%max = max
      ai%decomp = decomp
      ai%gstart = gstart

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexInit

!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine ESMF_AxisIndexGet(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(ESMF_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      if (present(l)) l = ai%l
      if (present(r)) r = ai%r
      if (present(max)) max = ai%max
      if (present(decomp)) decomp = ai%decomp
      if (present(gstart)) gstart = ai%gstart

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMF_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine ESMF_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(ESMF_Pointer) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_SetNullPointer
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function ESMF_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: ESMF_GetPointer

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GetPointer

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine ESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(ESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (status .eq. ESMF_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. ESMF_STATE_READY) string = "Ready"
      if (status .eq. ESMF_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. ESMF_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. ESMF_STATE_BUSY) string = "Busy"
      if (status .eq. ESMF_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_StatusString

!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
      subroutine ESMF_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(ESMF_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (datatype .eq. ESMF_DATA_INTEGER) string = "Integer"
      if (datatype .eq. ESMF_DATA_REAL) string = "Real"
      if (datatype .eq. ESMF_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. ESMF_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DataTypeString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMF_BaseMod

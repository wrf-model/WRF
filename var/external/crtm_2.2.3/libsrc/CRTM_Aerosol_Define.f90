!
! CRTM_Aerosol_Define
!
! Module defining the CRTM Aerosol structure and containing
! routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       yong.han@noaa.gov
!                       Quanhua Liu,    quanhua.liu@noaa.gov
!                       Paul van Delst, paul.vandelst@noaa.gov
!                       22-Feb-2005
!

MODULE CRTM_Aerosol_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Intrinsic modules
  USE ISO_Fortran_Env      , ONLY: OUTPUT_UNIT
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Aerosol parameters
  PUBLIC :: N_VALID_AEROSOL_CATEGORIES
  PUBLIC :: INVALID_AEROSOL
  PUBLIC :: DUST_AEROSOL
  PUBLIC :: SEASALT_SSAM_AEROSOL
  PUBLIC :: SEASALT_SSCM1_AEROSOL
  PUBLIC :: SEASALT_SSCM2_AEROSOL
  PUBLIC :: SEASALT_SSCM3_AEROSOL
  PUBLIC :: ORGANIC_CARBON_AEROSOL
  PUBLIC :: BLACK_CARBON_AEROSOL
  PUBLIC :: SULFATE_AEROSOL
  PUBLIC :: AEROSOL_CATEGORY_NAME
  ! Datatypes
  PUBLIC :: CRTM_Aerosol_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Procedures
  PUBLIC :: CRTM_Aerosol_CategoryName
  PUBLIC :: CRTM_Aerosol_CategoryId
  PUBLIC :: CRTM_Aerosol_CategoryList
  PUBLIC :: CRTM_Aerosol_Associated
  PUBLIC :: CRTM_Aerosol_Destroy
  PUBLIC :: CRTM_Aerosol_Create
  PUBLIC :: CRTM_Aerosol_AddLayerCopy
  PUBLIC :: CRTM_Aerosol_Zero
  PUBLIC :: CRTM_Aerosol_IsValid
  PUBLIC :: CRTM_Aerosol_Inspect
  PUBLIC :: CRTM_Aerosol_DefineVersion
  PUBLIC :: CRTM_Aerosol_Compare
  PUBLIC :: CRTM_Aerosol_SetLayers
  PUBLIC :: CRTM_Aerosol_InquireFile
  PUBLIC :: CRTM_Aerosol_ReadFile
  PUBLIC :: CRTM_Aerosol_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Aerosol_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Aerosol_Add
  END INTERFACE OPERATOR(+)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_Aerosol_Subtract
  END INTERFACE OPERATOR(-)

  INTERFACE CRTM_Aerosol_Inspect
    MODULE PROCEDURE Scalar_Inspect
    MODULE PROCEDURE Rank1_Inspect
    MODULE PROCEDURE Rank2_Inspect
  END INTERFACE CRTM_Aerosol_Inspect


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Aerosol_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Aerosol types and names
  INTEGER, PARAMETER :: N_VALID_AEROSOL_CATEGORIES = 8
  INTEGER, PARAMETER ::        INVALID_AEROSOL = 0
  INTEGER, PARAMETER ::           DUST_AEROSOL = 1
  INTEGER, PARAMETER ::   SEASALT_SSAM_AEROSOL = 2
  INTEGER, PARAMETER ::  SEASALT_SSCM1_AEROSOL = 3
  INTEGER, PARAMETER ::  SEASALT_SSCM2_AEROSOL = 4
  INTEGER, PARAMETER ::  SEASALT_SSCM3_AEROSOL = 5
  INTEGER, PARAMETER :: ORGANIC_CARBON_AEROSOL = 6
  INTEGER, PARAMETER ::   BLACK_CARBON_AEROSOL = 7
  INTEGER, PARAMETER ::        SULFATE_AEROSOL = 8
  INTEGER, PARAMETER :: AEROSOL_CATEGORY_LIST(0:N_VALID_AEROSOL_CATEGORIES) = &
    [        INVALID_AEROSOL, &
                DUST_AEROSOL, &
        SEASALT_SSAM_AEROSOL, &
       SEASALT_SSCM1_AEROSOL, &
       SEASALT_SSCM2_AEROSOL, &
       SEASALT_SSCM3_AEROSOL, &
      ORGANIC_CARBON_AEROSOL, &
        BLACK_CARBON_AEROSOL, &
             SULFATE_AEROSOL  ]
  CHARACTER(*), PARAMETER :: AEROSOL_CATEGORY_NAME(0:N_VALID_AEROSOL_CATEGORIES) = &
    [ 'Invalid         ', &
      'Dust            ', &
      'Sea salt (SSAM) ', &
      'Sea salt (SSCM1)', &
      'Sea salt (SSCM2)', &
      'Sea salt (SSCM3)', &
      'Organic carbon  ', &
      'Black carbon    ', &
      'Sulfate         '  ]
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! ----------------------------
  ! Aerosol data type definition
  ! ----------------------------
  !:tdoc+:
  TYPE :: CRTM_Aerosol_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: Max_Layers = 0  ! K dimension.
    INTEGER :: n_Layers   = 0  ! Kuse dimension
    ! Number of added layers
    INTEGER :: n_Added_Layers = 0
    ! Aerosol type
    INTEGER :: Type = INVALID_AEROSOL
    ! Aerosol state variables
    REAL(fp), ALLOCATABLE :: Effective_Radius(:)  ! K. Units are microns
    REAL(fp), ALLOCATABLE :: Concentration(:)     ! K. Units are kg/m^2
  END TYPE CRTM_Aerosol_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  PURE FUNCTION CRTM_Aerosol_CategoryId(aerosol) RESULT(id)
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: aerosol
    INTEGER :: id
    id = aerosol%type
    IF ( id < 1 .OR. id > N_VALID_AEROSOL_CATEGORIES ) id = INVALID_AEROSOL
  END FUNCTION CRTM_Aerosol_CategoryId

  PURE FUNCTION CRTM_Aerosol_CategoryName(aerosol) RESULT(name)
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: aerosol
    CHARACTER(LEN(AEROSOL_CATEGORY_NAME(1))) :: name
    INTEGER  :: id
    id = CRTM_Aerosol_CategoryId(aerosol)
    name = AEROSOL_CATEGORY_NAME(id)
  END FUNCTION CRTM_Aerosol_CategoryName

  FUNCTION CRTM_Aerosol_CategoryList(list) RESULT(err_stat)
    INTEGER, ALLOCATABLE, INTENT(OUT) :: list(:)
    INTEGER :: err_stat
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_CategoryList'
    CHARACTER(ML) :: alloc_msg, msg
    INTEGER :: alloc_stat
    err_stat = SUCCESS
    ALLOCATE( list(0:N_VALID_AEROSOL_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Aerosol category list result not allocated -'//TRIM(alloc_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    list = AEROSOL_CATEGORY_LIST
  END FUNCTION CRTM_Aerosol_CategoryList


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM Aerosol object.
!
! CALLING SEQUENCE:
!       Status = CRTM_Aerosol_Associated( Aerosol )
!
! OBJECTS:
!       Aerosol: Aerosol structure which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       CRTM_Aerosol_type
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the Aerosol members.
!                  .TRUE.  - if the array components are allocated.
!                  .FALSE. - if the array components are not allocated.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input Aerosol argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_Associated( Aerosol ) RESULT( Status )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    LOGICAL :: Status
    Status = Aerosol%Is_Allocated
  END FUNCTION CRTM_Aerosol_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Aerosol objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_Destroy( Aerosol )
!
! OBJECTS:
!       Aerosol:      Re-initialized Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Aerosol_Destroy( Aerosol )
    TYPE(CRTM_Aerosol_type), INTENT(OUT) :: Aerosol
    Aerosol%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_Aerosol_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Aerosol object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_Create( Aerosol, n_Layers )
!
! OBJECTS:
!       Aerosol:      Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers for which there is Aerosol data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as Aerosol object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Aerosol_Create( Aerosol, n_Layers )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(OUT) :: Aerosol
    INTEGER,                 INTENT(IN)  :: n_Layers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( Aerosol%Effective_Radius( n_Layers ), &
              Aerosol%Concentration( n_Layers ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    Aerosol%Max_Layers = n_Layers
    Aerosol%n_Layers   = n_Layers
    ! ...Arrays
    Aerosol%Effective_Radius = ZERO
    Aerosol%Concentration    = ZERO

    ! Set allocation indicator
    Aerosol%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Aerosol_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_AddLayerCopy
!
! PURPOSE:
!       Elemental function to copy an instance of the CRTM Aerosol object
!       with additional layers added to the TOA of the input.
!
! CALLING SEQUENCE:
!       aer_out = CRTM_Aerosol_AddLayerCopy( aer, n_Added_Layers )
!
! OBJECTS:
!       aer:             Aerosol structure to copy.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Added_Layers:  Number of layers to add to the function result.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Same as Aerosol object
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       aer_out:         Copy of the input Aerosol structure with space for
!                        extra layers added to TOA.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Same as input.
!                        ATTRIBUTES: INTENT(OUT)
!
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_AddLayerCopy( &
    aer, &
    n_Added_Layers ) &
  RESULT( aer_out )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: aer
    INTEGER,               INTENT(IN) :: n_Added_Layers
    ! Function result
    TYPE(CRTM_Aerosol_type) :: aer_out
    ! Local variables
    INTEGER :: na, no, nt

    ! Set the number of extra layers
    na = MAX(n_Added_Layers,0)

    ! Create the output structure
    CALL CRTM_Aerosol_Create( aer_out, aer%n_Layers+na )
    IF ( .NOT. CRTM_Aerosol_Associated(aer_out) ) RETURN

    ! Assign data
    aer_out%n_Added_Layers = aer%n_Added_Layers+na
    ! ...Layer independent data
    aer_out%Type = aer%Type
    ! ...Layer dependent data
    no = aer%n_Layers
    nt = aer_out%n_Layers
    aer_out%Effective_Radius(na+1:nt) = aer%Effective_Radius(1:no)
    aer_out%Concentration(na+1:nt)    = aer%Concentration(1:no)

  END FUNCTION CRTM_Aerosol_AddLayerCopy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_Zero
!
! PURPOSE:
!       Elemental subroutine to zero out the data arrays in a CRTM Aerosol object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_Zero( Aerosol )
!
! OBJECTS:
!       Aerosol:       CRTM Aerosol object in which the data arrays are
!                      to be zeroed out.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The Aerosol type component is *NOT* reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Aerosol_Zero( Aerosol )
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    ! Do nothing if structure is unused
    IF ( .NOT. CRTM_Aerosol_Associated(Aerosol) ) RETURN
    ! Only zero out the data arrays
    Aerosol%Effective_Radius = ZERO
    Aerosol%Concentration    = ZERO
  END SUBROUTINE CRTM_Aerosol_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Aerosol object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Aerosol_IsValid( Aerosol )
!
!         or
!
!       IF ( CRTM_Aerosol_IsValid( Aerosol ) ) THEN....
!
! OBJECTS:
!       Aerosol:       CRTM Aerosol object which is to have its
!                      contents checked.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical variable indicating whether or not the input
!                      passed the check.
!                      If == .FALSE., Aerosol object is unused or contains
!                                     invalid data.
!                         == .TRUE.,  Aerosol object can be used in CRTM.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_IsValid( Aerosol ) RESULT( IsValid )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_IsValid'
    CHARACTER(ML) :: msg

    ! Setup
    IsValid = .FALSE.
    ! ...Check if structure is used
    IF ( .NOT. CRTM_Aerosol_Associated(Aerosol) ) THEN
      msg = 'Aerosol structure not allocated'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF
    IF ( Aerosol%n_Layers < 1 ) THEN
      msg = 'Aerosol structure dimension invalid'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF

    ! Check data
    ! ...Change default so all entries can be checked
    IsValid = .TRUE.
    ! ...The type of Aerosol
    IF ( Aerosol%Type < 1 .OR. Aerosol%Type > N_VALID_AEROSOL_CATEGORIES ) THEN
      msg = 'Invalid Aerosol type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data limits. Only checking negative values
    IF ( ANY(Aerosol%Effective_Radius < ZERO ) ) THEN
      msg = 'Negative Aerosol effective radius found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Aerosol%Concentration < ZERO ) ) THEN
      msg = 'Negative Aerosol concentration found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF

  END FUNCTION CRTM_Aerosol_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Aerosol object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_Inspect( Aerosol, Unit=unit )
!
! INPUTS:
!       Aerosol:  CRTM Aerosol object to display.
!                 UNITS:      N/A
!                 TYPE:       CRTM_Aerosol_type
!                 DIMENSION:  Scalar, Rank-1, or Rank-2 array
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Unit:     Unit number for an already open file to which the output
!                 will be written.
!                 If the argument is specified and the file unit is not
!                 connected, the output goes to stdout.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_Inspect( Aerosol, Unit )
    ! Arguments
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN) :: Unit
    ! Local variables
    INTEGER :: fid
    
    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF

    
    WRITE(fid,'(1x,"AEROSOL OBJECT")')
    ! Dimensions
    WRITE(fid,'(3x,"n_Layers :",1x,i0)') Aerosol%n_Layers
    WRITE(fid,'(3x,"Category :",1x,a)') CRTM_Aerosol_CategoryName(aerosol)
    IF ( .NOT. CRTM_Aerosol_Associated(Aerosol) ) RETURN
    WRITE(fid,'(3x,"Effective radius:")')
    WRITE(fid,'(5(1x,es13.6,:))') Aerosol%Effective_Radius
    WRITE(fid,'(3x,"Concentration:")')
    WRITE(fid,'(5(1x,es13.6,:))') Aerosol%Concentration
  END SUBROUTINE Scalar_Inspect

  SUBROUTINE Rank1_Inspect( Aerosol, Unit )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol(:)
    INTEGER,       OPTIONAL, INTENT(IN) :: Unit
    INTEGER :: fid
    INTEGER :: i
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    DO i = 1, SIZE(Aerosol)
      WRITE(fid, FMT='(1x,"RANK-1 INDEX:",i0," - ")', ADVANCE='NO') i
      CALL Scalar_Inspect(Aerosol(i), Unit=Unit)
    END DO
  END SUBROUTINE Rank1_Inspect

  SUBROUTINE Rank2_Inspect( Aerosol, Unit )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol(:,:)
    INTEGER,       OPTIONAL, INTENT(IN) :: Unit
    INTEGER :: fid
    INTEGER :: i, j
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    DO j = 1, SIZE(Aerosol,2)
      DO i = 1, SIZE(Aerosol,1)
        WRITE(fid, FMT='(1x,"RANK-2 INDEX:",i0,",",i0," - ")', ADVANCE='NO') i,j
        CALL Scalar_Inspect(Aerosol(i,j), Unit=Unit)
      END DO
    END DO
  END SUBROUTINE Rank2_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_DefineVersion( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Aerosol_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Aerosol_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_Aerosol_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_Aerosol objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_Aerosol_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM Aerosol objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: x, y
    INTEGER,       OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_Aerosol_Associated(x)) .OR. &
         (.NOT. CRTM_Aerosol_Associated(y)) ) RETURN

    ! Check scalars
    IF ( (x%n_Layers /= y%n_Layers) .OR. &
         (x%Type     /= y%Type    ) ) RETURN

    ! Check arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Effective_Radius,y%Effective_Radius,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Concentration   ,y%Concentration   ,n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_Aerosol_Compare


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_SetLayers
!
! PURPOSE:
!       Elemental subroutine to set the working number of layers to use
!       in a CRTM Aerosol object.
!
! CALLING SEQUENCE:
!      CALL CRTM_Aerosol_SetLayers( Aerosol, n_Layers )
!
! OBJECT:
!       Aerosol:      CRTM Aerosol object which is to have its working number
!                     of layers updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the
!                     Aerosol object.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Conformable with the Aerosol object argument
!                     ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       - The object is zeroed upon output.
!
!       - If n_Layers <= Aerosol%Max_Layers, then only the dimension value
!         of the object is changed.
!
!       - If n_Layers > Aerosol%Max_Layers, then the object is reallocated
!         to the required number of layers.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Aerosol_SetLayers( Aerosol, n_Layers )
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    INTEGER,               INTENT(IN)       :: n_Layers
    IF ( n_Layers < Aerosol%Max_Layers ) THEN
      Aerosol%n_Layers = n_Layers
      CALL CRTM_Aerosol_Zero(Aerosol)
    ELSE
      CALL CRTM_Aerosol_Create( Aerosol, n_Layers )
    END IF
  END SUBROUTINE CRTM_Aerosol_SetLayers


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_InquireFile( Filename               , &
!                                                n_Aerosols = n_Aerosols  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Aerosol data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Aerosols:     The number of Aerosol profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_InquireFile( &
    Filename  , &  ! Input
    n_Aerosols) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Aerosols
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: na

    ! Setup
    err_stat = SUCCESS


    ! Open the aerosol data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of aerosols dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Aerosols dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return arguments
    IF ( PRESENT(n_Aerosols) ) n_Aerosols = na

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Aerosol_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_ReadFile
!
! PURPOSE:
!       Function to read CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_ReadFile( Filename               , &
!                                             Aerosol                , &
!                                             Quiet      = Quiet     , &
!                                             No_Close   = No_Close  , &
!                                             n_Aerosols = n_Aerosols  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Aerosol:        CRTM Aerosol object array containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Aerosols:     The actual number of aerosol profiles read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_Readfile( &
    Filename  , &  ! Input
    Aerosol   , &  ! Output
    Quiet     , &  ! Optional input
    No_Close  , &  ! Optional input
    n_Aerosols, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(OUT) :: Aerosol(:)
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: No_Close
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Aerosols
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m
    INTEGER :: na

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check file close argument
    yes_close = .TRUE.
    IF ( PRESENT(No_Close) ) yes_close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      ! ...Get the file id
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its unit number'
        CALL Read_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      ! ...Open the file
      err_stat = Open_Binary_File( Filename, fid )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Read the number of aerosols dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Aerosols data dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if output array large enough
    IF ( na > SIZE(Aerosol) ) THEN
      WRITE( msg,'("Number of aerosols, ",i0," > size of the output ",&
             &"Aerosol object array, ",i0,".")' ) na, SIZE(Aerosol)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the aerosol data
    Aerosol_Loop: DO m = 1, na
      err_stat = Read_Record( fid, Aerosol(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Aerosol element #",i0," from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Aerosol_Loop


    ! Close the file
    IF ( yes_close ) THEN
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Aerosols) ) n_Aerosols = na


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of aerosols read from ",a,": ",i0)' ) TRIM(Filename), na
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Aerosol_Destroy( Aerosol )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CRTM_Aerosol_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_WriteFile
!
! PURPOSE:
!       Function to write CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_WriteFile( Filename           , &
!                                              Aerosol            , &
!                                              Quiet    = Quiet   , &
!                                              No_Close = No_Close  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Aerosol format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Aerosol:        CRTM Aerosol object array containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_WriteFile( &
    Filename, &  ! Input
    Aerosol , &  ! Input
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol(:)
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m, na

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check file close argument
    yes_close = .TRUE.
    IF ( PRESENT(No_Close) ) yes_close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug

    ! Check the Aerosol structure dimensions
    IF ( ANY(Aerosol%n_Layers < 1) ) THEN
      msg = 'Dimensions of Aerosol structures are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its unit number'
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the number of aerosols dimension
    na = SIZE(Aerosol)
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing n_Aerosols data dimension to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the aerosol data
    Aerosol_Loop: DO m = 1, na
      err_stat = Write_Record( fid, Aerosol(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Aerosol element #",i0," to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Aerosol_Loop


    ! Close the file (if error, no delete)
    IF ( yes_close ) THEN
      CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of aerosols written to ",a,": ",i0)' ) TRIM(Filename), na
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Aerosol_WriteFile



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Aerosol_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Aerosol structures.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Aerosol_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM_Aerosol objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Aerosol_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Variables
    INTEGER :: n

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( (.NOT. CRTM_Aerosol_Associated(x)) .OR. &
         (.NOT. CRTM_Aerosol_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Layers /= y%n_Layers) .OR. (x%Type /= y%Type) ) RETURN
    ! ...Arrays
    n = x%n_Layers
    IF ( ALL(x%Effective_Radius(1:n) .EqualTo. y%Effective_Radius(1:n) ) .AND. &
         ALL(x%Concentration(1:n)    .EqualTo. y%Concentration(1:n)    )       ) &
      is_equal = .TRUE.

  END FUNCTION CRTM_Aerosol_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Aerosol_Add
!
! PURPOSE:
!       Pure function to add two CRTM Aerosol objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       aersum = CRTM_Aerosol_Add( aer1, aer2 )
!
!         or
!
!       aersum = aer1 + aer2
!
!
! INPUTS:
!       aer1, aer2: The Aerosol objects to add.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Aerosol_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       aersum:     Aerosol object containing the added components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Aerosol_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_Add( aer1, aer2 ) RESULT( aersum )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: aer1, aer2
    TYPE(CRTM_Aerosol_type) :: aersum
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Aerosol_Associated( aer1 ) .OR. &
         .NOT. CRTM_Aerosol_Associated( aer2 )      ) RETURN
    ! ...If input structure for different aerosols, or sizes, do nothing
    IF ( aer1%Type           /= aer2%Type           .OR. &
         aer1%n_Layers       /= aer2%n_Layers       .OR. &
         aer1%n_Added_Layers /= aer2%n_Added_Layers      ) RETURN

    ! Copy the first structure
    aersum = aer1

    ! And add its components to the second one
    n = aer1%n_Layers
    aersum%Effective_Radius(1:n) = aersum%Effective_Radius(1:n) + aer2%Effective_Radius(1:n)
    aersum%Concentration(1:n)    = aersum%Concentration(1:n)    + aer2%Concentration(1:n)

  END FUNCTION CRTM_Aerosol_Add


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Aerosol_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM Aerosol objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       aerdiff = CRTM_Aerosol_Subtract( aer1, aer2 )
!
!         or
!
!       aersum = aer1 - aer2
!
!
! INPUTS:
!       aer1, aer2: The Aerosol objects to difference.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Aerosol_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       aerdiff:    Aerosol object containing the differenced components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Aerosol_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Aerosol_Subtract( aer1, aer2 ) RESULT( aerdiff )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: aer1, aer2
    TYPE(CRTM_Aerosol_type) :: aerdiff
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Aerosol_Associated( aer1 ) .OR. &
         .NOT. CRTM_Aerosol_Associated( aer2 )      ) RETURN
    ! ...If input structure for different aerosols, or sizes, do nothing
    IF ( aer1%Type           /= aer2%Type           .OR. &
         aer1%n_Layers       /= aer2%n_Layers       .OR. &
         aer1%n_Added_Layers /= aer2%n_Added_Layers      ) RETURN

    ! Copy the first structure
    aerdiff = aer1

    ! And subtract the second one's components from it
    n = aer1%n_Layers
    aerdiff%Effective_Radius(1:n) = aerdiff%Effective_Radius(1:n) - aer2%Effective_Radius(1:n)
    aerdiff%Concentration(1:n)    = aerdiff%Concentration(1:n)    - aer2%Concentration(1:n)

  END FUNCTION CRTM_Aerosol_Subtract


!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single CRTM Aerosol object in binary format
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, Aerosol )
!

  FUNCTION Read_Record( &
    fid    , &  ! Input
    aerosol) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER                , INTENT(IN)  :: fid
    TYPE(CRTM_Aerosol_type), INTENT(OUT) :: aerosol
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_ReadFile(Binary Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: n_layers

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Layers dimension - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the structure
    CALL CRTM_Aerosol_Create( aerosol, n_layers )
    IF ( .NOT. CRTM_Aerosol_Associated( aerosol ) ) THEN
      msg = 'Aerosol object allocation failed.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the aerosol data
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      aerosol%Type, &
      aerosol%Effective_Radius, &
      aerosol%Concentration
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Aerosol data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Aerosol_Destroy( aerosol )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single CRTM Aerosol object in binary format
!

  FUNCTION Write_Record( &
    fid    , &  ! Input
    aerosol) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER                , INTENT(IN)  :: fid
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: aerosol
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_WriteFile(Binary Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Aerosol_Associated( aerosol ) ) THEN
      msg = 'Input Aerosol object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) Aerosol%n_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      Aerosol%Type, &
      Aerosol%Effective_Radius(1:Aerosol%n_Layers), &
      Aerosol%Concentration(1:Aerosol%n_Layers)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Aerosol data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_Record_Cleanup

  END FUNCTION Write_Record

END MODULE CRTM_Aerosol_Define

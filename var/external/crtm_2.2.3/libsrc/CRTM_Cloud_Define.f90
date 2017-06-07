!
! CRTM_Cloud_Define
!
! Module defining the CRTM Cloud structure and containing routines to
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       yong.han@noaa.gov
!                       Quanhua Liu,    quanhua.liu@noaa.gov
!                       Paul van Delst, paul.vandelst@noaa.gov
!                       20-Feb-2004
!

MODULE CRTM_Cloud_Define


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
  ! Cloud Parameters
  PUBLIC :: N_VALID_CLOUD_CATEGORIES
  PUBLIC :: INVALID_CLOUD
  PUBLIC ::   WATER_CLOUD
  PUBLIC ::     ICE_CLOUD
  PUBLIC ::    RAIN_CLOUD
  PUBLIC ::    SNOW_CLOUD
  PUBLIC :: GRAUPEL_CLOUD
  PUBLIC ::    HAIL_CLOUD
  PUBLIC :: CLOUD_CATEGORY_NAME
  ! Datatypes
  PUBLIC :: CRTM_Cloud_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Procedures
  PUBLIC :: CRTM_Cloud_CategoryName
  PUBLIC :: CRTM_Cloud_CategoryId
  PUBLIC :: CRTM_Cloud_CategoryList
  PUBLIC :: CRTM_Cloud_Associated
  PUBLIC :: CRTM_Cloud_Destroy
  PUBLIC :: CRTM_Cloud_Create
  PUBLIC :: CRTM_Cloud_AddLayerCopy
  PUBLIC :: CRTM_Cloud_Zero
  PUBLIC :: CRTM_Cloud_IsValid
  PUBLIC :: CRTM_Cloud_Inspect
  PUBLIC :: CRTM_Cloud_DefineVersion
  PUBLIC :: CRTM_Cloud_Compare
  PUBLIC :: CRTM_Cloud_SetLayers
  PUBLIC :: CRTM_Cloud_InquireFile
  PUBLIC :: CRTM_Cloud_ReadFile
  PUBLIC :: CRTM_Cloud_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Cloud_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Cloud_Add
  END INTERFACE OPERATOR(+)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_Cloud_Subtract
  END INTERFACE OPERATOR(-)

  INTERFACE CRTM_Cloud_Inspect
    MODULE PROCEDURE Scalar_Inspect
    MODULE PROCEDURE Rank1_Inspect
    MODULE PROCEDURE Rank2_Inspect
  END INTERFACE CRTM_Cloud_Inspect


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Cloud_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! The valid cloud categories and names
  INTEGER, PARAMETER :: N_VALID_CLOUD_CATEGORIES = 6
  INTEGER, PARAMETER :: INVALID_CLOUD = 0
  INTEGER, PARAMETER ::   WATER_CLOUD = 1
  INTEGER, PARAMETER ::     ICE_CLOUD = 2
  INTEGER, PARAMETER ::    RAIN_CLOUD = 3
  INTEGER, PARAMETER ::    SNOW_CLOUD = 4
  INTEGER, PARAMETER :: GRAUPEL_CLOUD = 5
  INTEGER, PARAMETER ::    HAIL_CLOUD = 6
  INTEGER, PARAMETER :: CLOUD_CATEGORY_LIST(0:N_VALID_CLOUD_CATEGORIES) = &
    [ INVALID_CLOUD, &
        WATER_CLOUD, &
          ICE_CLOUD, &
         RAIN_CLOUD, &
         SNOW_CLOUD, &
      GRAUPEL_CLOUD, &
         HAIL_CLOUD  ]
  CHARACTER(*), PARAMETER :: CLOUD_CATEGORY_NAME(0:N_VALID_CLOUD_CATEGORIES) = &
    [ 'Invalid', &
      'Water  ', &
      'Ice    ', &
      'Rain   ', &
      'Snow   ', &
      'Graupel', &
      'Hail   '  ]
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! --------------------------
  ! Cloud data type definition
  ! --------------------------
  !:tdoc+:
  TYPE :: CRTM_Cloud_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: Max_Layers = 0  ! K dimension.
    INTEGER :: n_Layers   = 0  ! Kuse dimension.
    ! Number of added layers
    INTEGER :: n_Added_Layers = 0
    ! Cloud type
    INTEGER :: Type = INVALID_CLOUD
    ! Cloud state variables
    REAL(fp), ALLOCATABLE :: Effective_Radius(:)   ! K. Units are microns
    REAL(fp), ALLOCATABLE :: Effective_Variance(:) ! K. Units are microns^2
    REAL(fp), ALLOCATABLE :: Water_Content(:)      ! K. Units are kg/m^2
  END TYPE CRTM_Cloud_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  PURE FUNCTION CRTM_Cloud_CategoryId(cloud) RESULT(id)
    TYPE(CRTM_Cloud_type), INTENT(IN) :: cloud
    INTEGER :: id
    id = cloud%type
    IF ( id < 1 .OR. id > N_VALID_CLOUD_CATEGORIES ) id = INVALID_CLOUD
  END FUNCTION CRTM_Cloud_CategoryId

  PURE FUNCTION CRTM_Cloud_CategoryName(cloud) RESULT(name)
    TYPE(CRTM_Cloud_type), INTENT(IN) :: cloud
    CHARACTER(LEN(CLOUD_CATEGORY_NAME(1))) :: name
    INTEGER  :: id
    id = CRTM_Cloud_CategoryId(cloud)
    name = CLOUD_CATEGORY_NAME(id)
  END FUNCTION CRTM_Cloud_CategoryName

  FUNCTION CRTM_Cloud_CategoryList(list) RESULT(err_stat)
    INTEGER, ALLOCATABLE, INTENT(OUT) :: list(:)
    INTEGER :: err_stat
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_CategoryList'
    CHARACTER(ML) :: alloc_msg, msg
    INTEGER :: alloc_stat
    err_stat = SUCCESS
    ALLOCATE( list(0:N_VALID_CLOUD_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Cloud category list result not allocated -'//TRIM(alloc_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    list = CLOUD_CATEGORY_LIST
  END FUNCTION CRTM_Cloud_CategoryList
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM Cloud object.
!
! CALLING SEQUENCE:
!       Status = CRTM_Cloud_Associated( Cloud )
!
! OBJECTS:
!       Cloud:   Cloud structure which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       CRTM_Cloud_type
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the Cloud members.
!                  .TRUE.  - if the array components are allocated.
!                  .FALSE. - if the array components are not allocated.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input Cloud argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Cloud_Associated( Cloud ) RESULT( Status )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    LOGICAL :: Status
    Status = Cloud%Is_Allocated
  END FUNCTION CRTM_Cloud_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Cloud objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_Destroy( Cloud )
!
! OBJECTS:
!       Cloud:        Re-initialized Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Cloud_Destroy( Cloud )
    TYPE(CRTM_Cloud_type), INTENT(OUT) :: Cloud
    Cloud%Is_Allocated = .FALSE.
  END SUBROUTINE CRTM_Cloud_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM Cloud object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_Create( Cloud, n_Layers )
!
! OBJECTS:
!       Cloud:        Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers for which there is cloud data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as Cloud object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Cloud_Create( Cloud, n_Layers )
    ! Arguments
    TYPE(CRTM_Cloud_type), INTENT(OUT) :: Cloud
    INTEGER,               INTENT(IN)  :: n_Layers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( Cloud%Effective_Radius( n_Layers ), &
              Cloud%Effective_Variance( n_Layers ), &
              Cloud%Water_Content( n_Layers ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    Cloud%Max_Layers = n_Layers
    Cloud%n_Layers   = n_Layers
    ! ...Arrays
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO

    ! Set allocation indicator
    Cloud%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Cloud_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_AddLayerCopy
!
! PURPOSE:
!       Elemental function to copy an instance of the CRTM Cloud object
!       with additional layers added to the TOA of the input.
!
! CALLING SEQUENCE:
!       cld_out = CRTM_Cloud_AddLayerCopy( cld, n_Added_Layers )
!
! OBJECTS:
!       cld:             Cloud structure to copy.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Cloud_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Added_Layers:  Number of layers to add to the function result.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Same as Cloud object
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       cld_out:         Copy of the input Cloud structure with space for
!                        extra layers added to TOA.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Cloud_type
!                        DIMENSION:  Same as input.
!                        ATTRIBUTES: INTENT(OUT)
!
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Cloud_AddLayerCopy( &
    cld, &
    n_Added_Layers ) &
  RESULT( cld_out )
    ! Arguments
    TYPE(CRTM_Cloud_type), INTENT(IN) :: cld
    INTEGER,               INTENT(IN) :: n_Added_Layers
    ! Function result
    TYPE(CRTM_Cloud_type) :: cld_out
    ! Local variables
    INTEGER :: na, no, nt

    ! Set the number of extra layers
    na = MAX(n_Added_Layers,0)

    ! Create the output structure
    CALL CRTM_Cloud_Create( cld_out, &
                            cld%n_Layers+na )
    IF ( .NOT. CRTM_Cloud_Associated(cld_out) ) RETURN

    ! Assign data
    cld_out%n_Added_Layers = cld%n_Added_Layers+na
    ! ...Layer independent data
    cld_out%Type = cld%Type
    ! ...Layer dependent data
    no = cld%n_Layers
    nt = cld_out%n_Layers
    cld_out%Effective_Radius(na+1:nt)   = cld%Effective_Radius(1:no)
    cld_out%Effective_Variance(na+1:nt) = cld%Effective_Variance(1:no)
    cld_out%Water_Content(na+1:nt)      = cld%Water_Content(1:no)

  END FUNCTION CRTM_Cloud_AddLayerCopy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_Zero
!
! PURPOSE:
!       Elemental subroutine to zero out the data arrays in a CRTM Cloud object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_Zero( Cloud )
!
! OBJECTS:
!       Cloud:         CRTM Cloud structure in which the data arrays are
!                      to be zeroed out.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The cloud type component is *NOT* reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Cloud_Zero( Cloud )
    TYPE(CRTM_Cloud_type), INTENT(IN OUT) :: Cloud
    ! Do nothing if structure is unused
    IF ( .NOT. CRTM_Cloud_Associated(Cloud) ) RETURN
    ! Only zero out the data arrays
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO
  END SUBROUTINE CRTM_Cloud_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Cloud object.
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Cloud_IsValid( cloud )
!
!         or
!
!       IF ( CRTM_Cloud_IsValid( cloud ) ) THEN....
!
! OBJECTS:
!       cloud:         CRTM Cloud object which is to have its
!                      contents checked.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical variable indicating whether or not the input
!                      passed the check.
!                      If == .FALSE., Cloud object is unused or contains
!                                     invalid data.
!                         == .TRUE.,  Cloud object can be used in CRTM.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Cloud_IsValid( Cloud ) RESULT( IsValid )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_IsValid'
    CHARACTER(ML) :: msg

    ! Setup
    IsValid = .FALSE.
    ! ...Check if structure is used
    IF ( .NOT. CRTM_Cloud_Associated(Cloud) ) THEN
      msg = 'Cloud structure not allocated'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF
    IF ( Cloud%n_Layers < 1 ) THEN
      msg = 'Cloud structure dimension invalid'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      RETURN
    ENDIF

    ! Check data
    ! ...Change default so all entries can be checked
    IsValid = .TRUE.
    ! ...The type of cloud
    IF ( Cloud%Type < 1 .OR. Cloud%Type > N_VALID_CLOUD_CATEGORIES ) THEN
      msg = 'Invalid cloud type'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    ! ...Data limits. Only checking negative values
    IF ( ANY(Cloud%Effective_Radius < ZERO ) ) THEN
      msg = 'Negative cloud effective radius found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Cloud%Effective_Variance < ZERO ) ) THEN
      msg = 'Negative cloud effective variance found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF
    IF ( ANY(Cloud%Water_Content < ZERO ) ) THEN
      msg = 'Negative cloud water content found'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    ENDIF

  END FUNCTION CRTM_Cloud_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Cloud object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_Inspect( Cloud, Unit=unit )
!
! INPUTS:
!       Cloud:  CRTM Cloud object to display.
!               UNITS:      N/A
!               TYPE:       CRTM_Cloud_type
!               DIMENSION:  Scalar, Rank-1, or Rank-2 array
!               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Unit:   Unit number for an already open file to which the output
!               will be written.
!               If the argument is specified and the file unit is not
!               connected, the output goes to stdout.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_Inspect( Cloud, Unit )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    INTEGER,     OPTIONAL, INTENT(IN) :: Unit
    ! Local variables
    INTEGER :: fid
    
    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF

    
    WRITE(fid,'(1x,"CLOUD OBJECT")')
    WRITE(fid,'(3x,"n_Layers :",1x,i0)') Cloud%n_Layers
    WRITE(fid,'(3x,"Category :",1x,a)') CRTM_Cloud_CategoryName(cloud)
    IF ( .NOT. CRTM_Cloud_Associated(Cloud) ) RETURN
    WRITE(fid,'(3x,"Effective radius:")')
    WRITE(fid,'(5(1x,es13.6,:))') Cloud%Effective_Radius
    WRITE(fid,'(3x,"Water content:")')
    WRITE(fid,'(5(1x,es13.6,:))') Cloud%Water_Content
  END SUBROUTINE Scalar_Inspect

  SUBROUTINE Rank1_Inspect( Cloud, Unit )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud(:)
    INTEGER,     OPTIONAL, INTENT(IN) :: Unit
    INTEGER :: fid
    INTEGER :: i
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    DO i = 1, SIZE(Cloud)
      WRITE(fid, FMT='(1x,"RANK-1 INDEX:",i0," - ")', ADVANCE='NO') i
      CALL Scalar_Inspect(Cloud(i), Unit=Unit)
    END DO
  END SUBROUTINE Rank1_Inspect

  SUBROUTINE Rank2_Inspect( Cloud, Unit )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud(:,:)
    INTEGER,     OPTIONAL, INTENT(IN) :: Unit
    INTEGER :: fid
    INTEGER :: i, j
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    DO j = 1, SIZE(Cloud,2)
      DO i = 1, SIZE(Cloud,1)
        WRITE(fid, FMT='(1x,"RANK-2 INDEX:",i0,",",i0," - ")', ADVANCE='NO') i,j
        CALL Scalar_Inspect(Cloud(i,j), Unit=Unit)
      END DO
    END DO
  END SUBROUTINE Rank2_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Cloud_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Cloud_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Cloud_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_Cloud_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_Cloud objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_Cloud_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM Cloud objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
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

  ELEMENTAL FUNCTION CRTM_Cloud_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: x, y
    INTEGER,     OPTIONAL, INTENT(IN) :: n_SigFig
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
    IF ( (.NOT. CRTM_Cloud_Associated(x)) .OR. &
         (.NOT. CRTM_Cloud_Associated(y)) ) RETURN

    ! Check scalars
    IF ( (x%n_Layers /= y%n_Layers) .OR. &
         (x%Type     /= y%Type    ) ) RETURN

    ! Check arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Effective_Radius  ,y%Effective_Radius  ,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Effective_Variance,y%Effective_Variance,n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Water_Content     ,y%Water_Content     ,n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_Cloud_Compare


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_SetLayers
!
! PURPOSE:
!       Elemental subroutine to set the working number of layers to use
!       in a CRTM Cloud object.
!
! CALLING SEQUENCE:
!      CALL CRTM_Cloud_SetLayers( Cloud, n_Layers )
!
! OBJECT:
!       Cloud:        CRTM Cloud object which is to have its working number
!                     of layers updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the
!                     Cloud object.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Conformable with the Cloud object argument
!                     ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       - The object is zeroed upon output.
!
!       - If n_Layers <= Cloud%Max_Layers, then only the dimension value
!         of the object is changed.
!
!       - If n_Layers > Cloud%Max_Layers, then the object is reallocated
!         to the required number of layers.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Cloud_SetLayers( Cloud, n_Layers )
    TYPE(CRTM_Cloud_type), INTENT(IN OUT) :: Cloud
    INTEGER,               INTENT(IN)     :: n_Layers
    IF ( n_Layers < Cloud%Max_Layers ) THEN
      Cloud%n_Layers = n_Layers
      CALL CRTM_Cloud_Zero(Cloud)
    ELSE
      CALL CRTM_Cloud_Create( Cloud, n_Layers )
    END IF
  END SUBROUTINE CRTM_Cloud_SetLayers


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_InquireFile( Filename           , &
!                                              n_Clouds = n_Clouds  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Cloud data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Clouds:       The number of Cloud profiles in the data file.
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

  FUNCTION CRTM_Cloud_InquireFile( &
    Filename, &  ! Input
    n_Clouds) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Clouds
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: nc

    ! Setup
    err_stat = SUCCESS


    ! Open the cloud data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of clouds dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) nc
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Clouds dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return arguments
    IF ( PRESENT(n_Clouds) ) n_Clouds = nc

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

  END FUNCTION CRTM_Cloud_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_ReadFile
!
! PURPOSE:
!       Function to read CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_ReadFile( Filename           , &
!                                           Cloud              , &
!                                           Quiet    = Quiet   , &
!                                           No_Close = No_Close, &
!                                           n_Clouds = n_Clouds  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Cloud:          CRTM Cloud object array containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT), ALLOCATABLE
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
!       n_Clouds:       The actual number of cloud profiles read in.
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

  FUNCTION CRTM_Cloud_ReadFile( &
    Filename, &  ! Input
    Cloud   , &  ! Output
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    n_Clouds, &  ! Optional output
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                        INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type) , ALLOCATABLE, INTENT(OUT) :: Cloud(:)
    LOGICAL,      OPTIONAL,              INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL,              INTENT(IN)  :: No_Close
    INTEGER,      OPTIONAL,              INTENT(OUT) :: n_Clouds
    LOGICAL,      OPTIONAL,              INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: io_stat
    INTEGER :: alloc_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m
    INTEGER :: nc

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


    ! Read the number of clouds dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) nc
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Clouds data dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the return structure array
    ALLOCATE(Cloud(nc), STAT=alloc_stat, ERRMSG=alloc_msg)
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating Cloud array - '//TRIM(alloc_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the cloud data
    Cloud_Loop: DO m = 1, nc
      err_stat = Read_Record( fid, Cloud(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Cloud element #",i0," from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Cloud_Loop


    ! Close the file
    IF ( yes_close ) THEN
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Set the optional return values
    IF ( PRESENT(n_Clouds) ) n_Clouds = nc


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of clouds read from ",a,": ",i0)' ) TRIM(Filename), nc
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      IF ( ALLOCATED(Cloud) ) THEN 
        DEALLOCATE(Cloud, STAT=alloc_stat, ERRMSG=alloc_msg)
        IF ( alloc_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deallocating Cloud array during error cleanup - '//&
                TRIM(alloc_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CRTM_Cloud_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Cloud_WriteFile
!
! PURPOSE:
!       Function to write CRTM Cloud object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Cloud_WriteFile( Filename           , &
!                                            Cloud              , &
!                                            Quiet    = Quiet   , &
!                                            No_Close = No_Close  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Cloud format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Cloud:          CRTM Cloud object array containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
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

  FUNCTION CRTM_Cloud_WriteFile( &
    Filename, &  ! Input
    Cloud   , &  ! Input
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud(:)
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m, nc

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


    ! Check the Cloud structure dimensions
    IF ( ANY(Cloud%n_Layers < 1) ) THEN
      msg = 'Dimensions of Cloud structures are < or = 0.'
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


    ! Write the number of clouds dimension
    nc = SIZE(Cloud)
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) nc
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing n_Clouds data dimension to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the cloud data
    Cloud_Loop: DO m = 1, nc
      err_stat = Write_Record( fid, Cloud(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Cloud element #",i0," to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Cloud_Loop


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
      WRITE( msg,'("Number of clouds written to ",a,": ",i0)' ) TRIM(Filename), nc
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

  END FUNCTION CRTM_Cloud_WriteFile



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
!       CRTM_Cloud_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Cloud objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Cloud_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM Cloud objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
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

  ELEMENTAL FUNCTION CRTM_Cloud_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Variables
    INTEGER :: n

    ! Set up
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( (.NOT. CRTM_Cloud_Associated(x)) .OR. &
         (.NOT. CRTM_Cloud_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalars
    IF ( (x%n_Layers /= y%n_Layers) .OR. (x%Type /= y%Type) ) RETURN
    ! ...Arrays
    n = x%n_Layers
    IF ( ALL(x%Effective_Radius(1:n)   .EqualTo. y%Effective_Radius(1:n)  ) .AND. &
         ALL(x%Effective_Variance(1:n) .EqualTo. y%Effective_Variance(1:n)) .AND. &
         ALL(x%Water_Content(1:n)      .EqualTo. y%Water_Content(1:n)     )       ) &
      is_equal = .TRUE.

  END FUNCTION CRTM_Cloud_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Cloud_Add
!
! PURPOSE:
!       Pure function to add two CRTM Cloud objects.
!       Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!       cldsum = CRTM_Cloud_Add( cld1, cld2 )
!
!         or
!
!       cldsum = cld1 + cld2
!
!
! INPUTS:
!       cld1, cld2: The Cloud objects to add.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Cloud_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       cldsum:     Cloud structure containing the added components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Cloud_type
!                   DIMENSION:  Same as input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Cloud_Add( cld1, cld2 ) RESULT( cldsum )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: cld1, cld2
    TYPE(CRTM_Cloud_type) :: cldsum
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Cloud_Associated( cld1 ) .OR. &
         .NOT. CRTM_Cloud_Associated( cld2 )      ) RETURN
    ! ...If input structure for different clouds, or sizes, do nothing
    IF ( cld1%Type           /= cld2%Type           .OR. &
         cld1%n_Layers       /= cld2%n_Layers       .OR. &
         cld1%n_Added_Layers /= cld2%n_Added_Layers      ) RETURN

    ! Copy the first structure
    cldsum = cld1

    ! And add its components to the second one
    n = cld1%n_Layers
    cldsum%Effective_Radius(1:n)   = cldsum%Effective_Radius(1:n)   + cld2%Effective_Radius(1:n)
    cldsum%Effective_Variance(1:n) = cldsum%Effective_Variance(1:n) + cld2%Effective_Variance(1:n)
    cldsum%Water_Content(1:n)      = cldsum%Water_Content(1:n)      + cld2%Water_Content(1:n)

  END FUNCTION CRTM_Cloud_Add


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Cloud_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM Cloud objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       clddiff = CRTM_Cloud_Subtract( cld1, cld2 )
!
!         or
!
!       clddiff = cld1 - cld2
!
!
! INPUTS:
!       cld1, cld2: The Cloud objects to difference.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Cloud_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       clddiff:    Cloud structure containing the differenced components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Cloud_type
!                   DIMENSION:  Same as input
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Cloud_Subtract( cld1, cld2 ) RESULT( clddiff )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: cld1, cld2
    TYPE(CRTM_Cloud_type) :: clddiff
    ! Variables
    INTEGER :: n

    ! Check input
    ! ...If input structures not used, do nothing
    IF ( .NOT. CRTM_Cloud_Associated( cld1 ) .OR. &
         .NOT. CRTM_Cloud_Associated( cld2 )      ) RETURN
    ! ...If input structure for different clouds, or sizes, do nothing
    IF ( cld1%Type           /= cld2%Type           .OR. &
         cld1%n_Layers       /= cld2%n_Layers       .OR. &
         cld1%n_Added_Layers /= cld2%n_Added_Layers      ) RETURN

    ! Copy the first structure
    clddiff = cld1

    ! And subtract the second one's components from it
    n = cld1%n_Layers
    clddiff%Effective_Radius(1:n)   = clddiff%Effective_Radius(1:n)   - cld2%Effective_Radius(1:n)
    clddiff%Effective_Variance(1:n) = clddiff%Effective_Variance(1:n) - cld2%Effective_Variance(1:n)
    clddiff%Water_Content(1:n)      = clddiff%Water_Content(1:n)      - cld2%Water_Content(1:n)

  END FUNCTION CRTM_Cloud_Subtract


!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single CRTM Cloud object in binary format
!

  FUNCTION Read_Record( &
    fid  , &  ! Input
    cloud) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)  :: fid
    TYPE(CRTM_Cloud_type) , INTENT(OUT) :: cloud
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_ReadFile(Record)'
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
    CALL CRTM_Cloud_Create( cloud, n_layers )
    IF ( .NOT. CRTM_Cloud_Associated( cloud ) ) THEN
      msg = 'Cloud object allocation failed.'
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the cloud data
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      Cloud%Type, &
      Cloud%Effective_Radius, &
      Cloud%Effective_Variance, &
      Cloud%Water_Content
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Cloud data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Cloud_Destroy( cloud )
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
!       Function to write a single CRTM Cloud object in binary format
!

  FUNCTION Write_Record( &
    fid   , &  ! Input
    cloud ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)  :: fid
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: cloud
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Cloud_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Cloud_Associated( cloud ) ) THEN
      msg = 'Input Cloud object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) Cloud%n_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      Cloud%Type, &
      Cloud%Effective_Radius(1:Cloud%n_Layers), &
      Cloud%Effective_Variance(1:Cloud%n_Layers), &
      Cloud%Water_Content(1:Cloud%n_Layers)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Cloud data - '//TRIM(io_msg)
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

END MODULE CRTM_Cloud_Define

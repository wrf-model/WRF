!
! CRTM_Cloud_Define
!
! Module defining the CRTM Cloud structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       20-Feb-2004
!

MODULE CRTM_Cloud_Define


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, ONE, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Cloud Parameters
  PUBLIC :: N_VALID_CLOUD_TYPES
  PUBLIC :: INVALID_CLOUD 
  PUBLIC ::   WATER_CLOUD 
  PUBLIC ::     ICE_CLOUD 
  PUBLIC ::    RAIN_CLOUD 
  PUBLIC ::    SNOW_CLOUD 
  PUBLIC :: GRAUPEL_CLOUD 
  PUBLIC ::    HAIL_CLOUD 
  PUBLIC :: CLOUD_TYPE_NAME
  ! Datatypes
  PUBLIC :: CRTM_Cloud_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  ! Procedures
  PUBLIC :: CRTM_Cloud_Associated
  PUBLIC :: CRTM_Cloud_Destroy
  PUBLIC :: CRTM_Cloud_Create
  PUBLIC :: CRTM_Cloud_AddLayerCopy
  PUBLIC :: CRTM_Cloud_Zero
  PUBLIC :: CRTM_Cloud_IsValid
  PUBLIC :: CRTM_Cloud_Inspect
  PUBLIC :: CRTM_Cloud_DefineVersion
  PUBLIC :: CRTM_Cloud_Compare
  ! ...Vestige of old module.
  PUBLIC :: CRTM_SetLayers_Cloud
  


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Cloud_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Cloud_Add
  END INTERFACE OPERATOR(+)

  INTERFACE CRTM_SetLayers_Cloud
    MODULE PROCEDURE SetLayers_Scalar
    MODULE PROCEDURE SetLayers_Rank1
  END INTERFACE CRTM_SetLayers_Cloud

  

  ! -----------------
  ! Module parameters
  ! -----------------
  ! The valid cloud types and names
  INTEGER, PARAMETER :: N_VALID_CLOUD_TYPES = 6
  INTEGER, PARAMETER :: INVALID_CLOUD = 0
  INTEGER, PARAMETER ::   WATER_CLOUD = 1
  INTEGER, PARAMETER ::     ICE_CLOUD = 2
  INTEGER, PARAMETER ::    RAIN_CLOUD = 3
  INTEGER, PARAMETER ::    SNOW_CLOUD = 4
  INTEGER, PARAMETER :: GRAUPEL_CLOUD = 5
  INTEGER, PARAMETER ::    HAIL_CLOUD = 6
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_CLOUD_TYPES ) :: &
    CLOUD_TYPE_NAME = (/ 'Invalid', &
                         'Water  ', &
                         'Ice    ', &
                         'Rain   ', &
                         'Snow   ', &
                         'Graupel', &
                         'Hail   ' /)

  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Cloud_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! msg string length
  INTEGER, PARAMETER :: ML = 256


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
    IF ( Cloud%Type < 1 .OR. Cloud%Type > N_VALID_CLOUD_TYPES ) THEN
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
!       CALL CRTM_Cloud_Inspect( Cloud )
!
! INPUTS:
!       Cloud:         CRTM Cloud object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Cloud_Inspect( Cloud )
    TYPE(CRTM_Cloud_type), INTENT(IN) :: Cloud
    INTEGER :: lType
    ! Display components
    WRITE(*, '(5x,"Cloud n_Layers:",1x,i0)') Cloud%n_Layers
    lType = Cloud%Type
    IF ( lType < 1 .OR. lType > N_VALID_CLOUD_TYPES ) lType = INVALID_CLOUD
    WRITE(*, '(5x,"Cloud type    :",1x,a)') CLOUD_TYPE_NAME(lType)
    IF ( .NOT. CRTM_Cloud_Associated(Cloud) ) RETURN
    WRITE(*, '(5x,"Cloud Reff    :")') 
    WRITE(*, '(5(1x,es13.6,:))') Cloud%Effective_Radius
    WRITE(*, '(5x,"Cloud water content:")') 
    WRITE(*, '(5(1x,es13.6,:))') Cloud%Water_Content
  END SUBROUTINE CRTM_Cloud_Inspect


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
!       CRTM_SetLayers_Cloud
! 
! PURPOSE:
!       Function to set the number of layers to use in a CRTM Cloud
!       structure.
!
! CALLING SEQUENCE:
!       err_stat = CRTM_SetLayers_Cloud( n_Layers, &
!                                        Cloud     )
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the 
!                     Cloud structure, as well as those of any of its
!                     structure components.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Cloud:        Cloud structure in which the n_Layers dimension
!                     is to be updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar OR Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
! OUTPUTS:
!       Cloud:        On output, the Cloud structure with the updated
!                     n_Layers dimension.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       err_stat: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the layer reset was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The argument Cloud is INTENT(IN OUT) and is modified upon output. The
!       elements of the structure are reinitialised
!
! COMMENTS:
!       - Note that the n_Layers input is *ALWAYS* scalar. Thus, all Cloud
!         elements will be set to the same number of layers.
!
!       - If n_Layers <= Cloud%Max_Layers, then only the dimension value
!         of the structure and any sub-structures are changed.
!
!       - If n_Layers > Cloud%Max_Layers, then the entire structure is
!         reallocated to the required number of layers.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION SetLayers_Scalar( n_Layers   , &  ! Input
                             Cloud      ) &  ! In/Output
                           RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Cloud(scalar)'

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Set dimension or allocate based on current size
    ! -----------------------------------------------        
    IF ( n_Layers < Cloud%Max_Layers ) THEN
      Cloud%n_Layers = n_Layers
      CALL CRTM_Cloud_Zero(Cloud)
    ELSE
      ! Deallocate
      call CRTM_Cloud_Destroy( Cloud )
      ! Reallocate
      call CRTM_Cloud_Create( Cloud, n_Layers )
    END IF
    
  END FUNCTION SetLayers_Scalar

  FUNCTION SetLayers_Rank1( n_Layers   , &  ! Input
                            Cloud      ) &  ! In/Output
                          RESULT( err_stat )
    ! Arguments
    INTEGER,                INTENT(IN)     :: n_Layers
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Cloud(scalar)'
    ! Local variables
    INTEGER :: m, set_stat
    
    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Loop over elements. If an error is encountered,
    ! report it but continue with the reset.
    ! -----------------------------------------------
    DO m = 1, SIZE(Cloud)
      set_stat = SetLayers_Scalar( n_Layers, Cloud(m) )
    END DO
  END FUNCTION SetLayers_Rank1


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

END MODULE CRTM_Cloud_Define

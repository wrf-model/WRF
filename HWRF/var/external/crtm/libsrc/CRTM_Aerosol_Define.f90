!
! CRTM_Aerosol_Define
!
! Module defining the CRTM Aerosol structure and containing
! routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!                       Quanhua Liu, QSS
!                       Quanhua.Liu@noaa.gov
!

MODULE CRTM_Aerosol_Define

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
  ! Aerosol parameters
  PUBLIC :: N_VALID_AEROSOL_TYPES
  PUBLIC :: INVALID_AEROSOL       
  PUBLIC :: DUST_AEROSOL          
  PUBLIC :: SEASALT_SSAM_AEROSOL  
  PUBLIC :: SEASALT_SSCM1_AEROSOL 
  PUBLIC :: SEASALT_SSCM2_AEROSOL 
  PUBLIC :: SEASALT_SSCM3_AEROSOL 
  PUBLIC :: ORGANIC_CARBON_AEROSOL
  PUBLIC :: BLACK_CARBON_AEROSOL  
  PUBLIC :: SULFATE_AEROSOL       
  PUBLIC :: AEROSOL_TYPE_NAME
  ! Datatypes
  PUBLIC :: CRTM_Aerosol_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  ! Procedures
  PUBLIC :: CRTM_Aerosol_Associated
  PUBLIC :: CRTM_Aerosol_Destroy
  PUBLIC :: CRTM_Aerosol_Create
  PUBLIC :: CRTM_Aerosol_AddLayerCopy
  PUBLIC :: CRTM_Aerosol_Zero
  PUBLIC :: CRTM_Aerosol_IsValid
  PUBLIC :: CRTM_Aerosol_Inspect
  PUBLIC :: CRTM_Aerosol_DefineVersion
  PUBLIC :: CRTM_Aerosol_Compare
  ! ...Vestige of old module.
  PUBLIC :: CRTM_SetLayers_Aerosol


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Aerosol_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_Aerosol_Add
  END INTERFACE OPERATOR(+)

  INTERFACE CRTM_SetLayers_Aerosol
    MODULE PROCEDURE SetLayers_Scalar
    MODULE PROCEDURE SetLayers_Rank1
  END INTERFACE CRTM_SetLayers_Aerosol


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Aerosol types and names
  INTEGER, PARAMETER :: N_VALID_AEROSOL_TYPES = 8
  INTEGER, PARAMETER :: INVALID_AEROSOL        = 0
  INTEGER, PARAMETER :: DUST_AEROSOL           = 1
  INTEGER, PARAMETER :: SEASALT_SSAM_AEROSOL   = 2
  INTEGER, PARAMETER :: SEASALT_SSCM1_AEROSOL  = 3
  INTEGER, PARAMETER :: SEASALT_SSCM2_AEROSOL  = 4
  INTEGER, PARAMETER :: SEASALT_SSCM3_AEROSOL  = 5
  INTEGER, PARAMETER :: ORGANIC_CARBON_AEROSOL = 6
  INTEGER, PARAMETER :: BLACK_CARBON_AEROSOL   = 7
  INTEGER, PARAMETER :: SULFATE_AEROSOL        = 8
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_VALID_AEROSOL_TYPES ) :: &
    AEROSOL_TYPE_NAME = (/ 'Invalid         ', &
                           'Dust            ', &
                           'Sea salt (SSAM) ', &
                           'Sea salt (SSCM1)', &
                           'Sea salt (SSCM2)', &
                           'Sea salt (SSCM3)', &
                           'Organic carbon  ', &
                           'Black carbon    ', &
                           'Sulfate         ' /)
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Aerosol_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


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
    IF ( Aerosol%Type < 1 .OR. Aerosol%Type > N_VALID_Aerosol_TYPES ) THEN
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
!       CALL CRTM_Aerosol_Inspect( Aerosol )
!
! INPUTS:
!       Aerosol:       CRTM Aerosol object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Aerosol_Inspect( Aerosol )
    TYPE(CRTM_Aerosol_type), INTENT(IN) :: Aerosol
    INTEGER :: lType
    ! Display components
    WRITE(*, '(5x,"Aerosol n_Layers:",1x,i0)') Aerosol%n_Layers
    lType = Aerosol%Type
    IF ( lType < 1 .OR. lType > N_VALID_AEROSOL_TYPES ) lType = INVALID_AEROSOL
    WRITE(*, '(5x,"Aerosol type    :",1x,a)') Aerosol_TYPE_NAME(lType)
    IF ( .NOT. CRTM_Aerosol_Associated(Aerosol) ) RETURN
    WRITE(*, '(5x,"Aerosol Reff    :")') 
    WRITE(*, '(5(1x,es13.6,:))') Aerosol%Effective_Radius
    WRITE(*, '(5x,"Aerosol concentration:")') 
    WRITE(*, '(5(1x,es13.6,:))') Aerosol%Concentration
  END SUBROUTINE CRTM_Aerosol_Inspect


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
!       CRTM_SetLayers_Aerosol
! 
! PURPOSE:
!       Function to set the number of layers to use in a CRTM Aerosol
!       structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetLayers_Aerosol( n_Layers, &
!                                              Aerosol   )
!
! INPUTS:
!       n_Layers:     The value to set the n_Layers component of the 
!                     Aerosol structure, as well as those of any of its
!                     structure components.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Aerosol:      Aerosol structure in which the n_Layers dimension
!                     is to be updated.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
! OUTPUTS:
!       Aerosol:      On output, the Aerosol structure with the updated
!                     n_Layers dimension.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the layer reset was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The argument Aerosol is INTENT(IN OUT) and is modified upon output. The
!       elements of the structureare reinitialised
!
! COMMENTS:
!       - Note that the n_Layers input is *ALWAYS* scalar. Thus, all Aerosol
!         elements will be set to the same number of layers.
!
!       - If n_Layers <= Aerosol%Max_Layers, then only the dimension value
!         of the structure and any sub-structures are changed.
!
!       - If n_Layers > Aerosol%Max_Layers, then the entire structure is
!         reallocated to the required number of layers.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION SetLayers_Scalar( n_Layers   , &  ! Input
                             Aerosol    ) &  ! In/Output
                           RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Layers
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Aerosol(scalar)'

    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Set dimension or allocate based on current size
    ! -----------------------------------------------        
    IF ( n_Layers < Aerosol%Max_Layers ) THEN
      Aerosol%n_Layers = n_Layers
      CALL CRTM_Aerosol_Zero(Aerosol)
    ELSE
      ! Deallocate
      CALL CRTM_Aerosol_Destroy( Aerosol )
      ! Reallocate
      CALL CRTM_Aerosol_Create( Aerosol, n_Layers )
    END IF
    
  END FUNCTION SetLayers_Scalar


  FUNCTION SetLayers_Rank1( n_Layers   , &  ! Input
                            Aerosol    ) &  ! In/Output
                          RESULT( err_stat )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: n_Layers
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SetLayers_Aerosol(rank-1)'
    ! Local variables
    INTEGER :: m, set_stat
    
    ! Set up
    ! ------
    err_stat = SUCCESS


    ! Loop over elements. If an error is encountered,
    ! report it but continue with the reset.
    ! -----------------------------------------------
    DO m = 1, SIZE(Aerosol)
      set_stat = SetLayers_Scalar( n_Layers, Aerosol(m) )
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

END MODULE CRTM_Aerosol_Define

!
! NLTE_Predictor_Define
!
! Module defining the NLTE_Predictor data structure and containing routines to 
! manipulate it.
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, 16-Mar-2011
!                    paul.vandelst@noaa.gov
!

MODULE NLTE_Predictor_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE NLTE_Parameters      , ONLY: N_NLTE_LAYERS, N_NLTE_PREDICTORS
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: NLTE_Predictor_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: NLTE_Predictor_Destroy
  PUBLIC :: NLTE_Predictor_Inspect
  PUBLIC :: NLTE_Predictor_ValidRelease
  PUBLIC :: NLTE_Predictor_Info
  PUBLIC :: NLTE_Predictor_DefineVersion
  PUBLIC :: NLTE_Predictor_Compare
  PUBLIC :: NLTE_Predictor_IsActive


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE NLTE_Predictor_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NLTE_Predictor_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512
  ! Sensor id string length
  INTEGER, PARAMETER :: SL = 20
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: NLTE_PREDICTOR_RELEASE = 1
  INTEGER, PARAMETER :: NLTE_PREDICTOR_VERSION = 1


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: NLTE_Predictor_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .TRUE.
    ! Release and version information
    INTEGER :: Release = NLTE_PREDICTOR_RELEASE
    INTEGER :: Version = NLTE_PREDICTOR_VERSION
    ! Dimensions
    INTEGER :: n_Layers     = N_NLTE_LAYERS
    INTEGER :: n_Predictors = N_NLTE_LAYERS + 1
    ! Logical indicators
    ! ...In-use indicator
    LOGICAL :: Is_Active = .FALSE.
    ! ...Computation indicator
    LOGICAL :: Compute_Tm = .TRUE.
    ! Data
    INTEGER :: k1(N_NLTE_LAYERS) = 0  ! Indices of atmosphere for upper layer
    INTEGER :: k2(N_NLTE_LAYERS) = 0  ! Indices of atmosphere for lower layer
    INTEGER :: isen = 0               ! Indices of coefficients for user sensor zenith angle
    INTEGER :: isol = 0               ! Indices of coefficients for user solar  zenith angle
    REAL(fp) :: Tm(N_NLTE_LAYERS)            = ZERO  ! Mean layer temperature
    REAL(fp) :: Predictor(N_NLTE_LAYERS + 1) = ZERO  ! Predictors
    REAL(fp) :: w(2, 2)                      = ZERO  ! Coefficient bilinear interpolation weights
  END TYPE NLTE_Predictor_type


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
!       NLTE_Predictor_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize NLTE_Predictor objects.
!
! CALLING SEQUENCE:
!       CALL NLTE_Predictor_Destroy( NLTE_Predictor )
!
! OBJECTS:
!       NLTE_Predictor:  Re-initialized NLTE_Predictor structure.
!                        UNITS:      N/A
!                        TYPE:       NLTE_Predictor_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE NLTE_Predictor_Destroy( NLTE_Predictor )
    TYPE(NLTE_Predictor_type), INTENT(OUT) :: NLTE_Predictor
    ! Set logicals to avoid "unused argument" warnings
    NLTE_Predictor%Is_Active  = .FALSE.
    NLTE_Predictor%Compute_Tm = .TRUE.
  END SUBROUTINE NLTE_Predictor_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a NLTE_Predictor object to stdout.
!
! CALLING SEQUENCE:
!       CALL NLTE_Predictor_Inspect( NLTE_Predictor )
!
! OBJECTS:
!       NLTE_Predictor:  NLTE_Predictor object to display.
!                        UNITS:      N/A
!                        TYPE:       NLTE_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTE_Predictor_Inspect( NLTE_Predictor )
    TYPE(NLTE_Predictor_type), INTENT(IN) :: NLTE_Predictor
    WRITE(*,'(1x,"NLTE_Predictor OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') NLTE_Predictor%Release, NLTE_Predictor%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Layers         :",1x,i0)') NLTE_Predictor%n_Layers    
    WRITE(*,'(3x,"n_Predictors     :",1x,i0)') NLTE_Predictor%n_Predictors
    ! Logical indicators
    WRITE(*,'(3x,"Is_Active        :",1x,l1)') NLTE_Predictor%Is_Active 
    WRITE(*,'(3x,"Compute_Tm       :",1x,l1)') NLTE_Predictor%Compute_Tm
    ! Data
    WRITE(*,'(3x,"k1        :",4(1x,i0,:,","))') NLTE_Predictor%k1
    WRITE(*,'(3x,"k2        :",4(1x,i0,:,","))') NLTE_Predictor%k2
    WRITE(*,'(3x,"isen      :",1x,i0)') NLTE_Predictor%isen
    WRITE(*,'(3x,"isol      :",1x,i0)') NLTE_Predictor%isol
    WRITE(*,'(3x,"Tm        :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTE_Predictor%Tm
    WRITE(*,'(3x,"Predictor :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTE_Predictor%Predictor
    WRITE(*,'(3x,"w         :")')
    WRITE(*,'(5(1x,es13.6,:))') NLTE_Predictor%w
    
  END SUBROUTINE NLTE_Predictor_Inspect
  
  
!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_ValidRelease
!
! PURPOSE:
!       Function to check the NLTE_Predictor Release value.
!
! CALLING SEQUENCE:
!       IsValid = NLTE_Predictor_ValidRelease( NLTE_Predictor )
!
! INPUTS:
!       NLTE_Predictor:  NLTE_Predictor object for which the Release component
!                        is to be checked.
!                        UNITS:      N/A
!                        TYPE:       NLTE_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:         Logical value defining the release validity.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION NLTE_Predictor_ValidRelease( NLTE_Predictor ) RESULT( IsValid )
    ! Arguments
    TYPE(NLTE_Predictor_type), INTENT(IN) :: NLTE_Predictor
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'NLTE_Predictor_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( NLTE_Predictor%Release < NLTE_PREDICTOR_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An NLTE_Predictor data update is needed. ", &
                  &"NLTE_Predictor release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTE_Predictor%Release, NLTE_PREDICTOR_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( NLTE_Predictor%Release > NLTE_PREDICTOR_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An NLTE_Predictor software update is needed. ", &
                  &"NLTE_Predictor release is ",i0,". Valid release is ",i0,"." )' ) &
                  NLTE_Predictor%Release, NLTE_PREDICTOR_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION NLTE_Predictor_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a NLTE_Predictor object.
!
! CALLING SEQUENCE:
!       CALL NLTE_Predictor_Info( NLTE_Predictor, Info )
!
! OBJECTS:
!       NLTE_Predictor:  NLTE_Predictor object about which info is required.
!                        UNITS:      N/A
!                        TYPE:       NLTE_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:            String containing version and dimension information
!                        about the NLTE_Predictor object.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTE_Predictor_Info( NLTE_Predictor, Info )
    ! Arguments
    TYPE(NLTE_Predictor_type), INTENT(IN)  :: NLTE_Predictor
    CHARACTER(*),              INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"NLTE_Predictor RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_LAYERS=",i0,2x,&
           &"N_PREDICTORS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTE_Predictor%Release, NLTE_Predictor%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           NLTE_Predictor%n_Layers    , &
           NLTE_Predictor%n_Predictors
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE NLTE_Predictor_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NLTE_Predictor_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL NLTE_Predictor_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE NLTE_Predictor_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE NLTE_Predictor_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       NLTE_Predictor_Compare
!
! PURPOSE:
!       Elemental function to compare two NLTE_Predictor objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = NLTE_Predictor_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two NLTE_Predictor objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       NLTE_Predictor_type
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

  ELEMENTAL FUNCTION NLTE_Predictor_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(NLTE_Predictor_type), INTENT(IN) :: x, y
    INTEGER,         OPTIONAL, INTENT(IN) :: n_SigFig
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
    
    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Layers     /= y%n_Layers     ) .OR. &
         (x%n_Predictors /= y%n_Predictors ) ) RETURN
    ! ...Scalars
    IF ( (x%Is_Active  .NEQV. y%Is_Active ) .OR. &
         (x%Compute_Tm .NEQV. y%Compute_Tm) .OR. &
         (x%isen         /=   y%isen      ) .OR. &
         (x%isol         /=   y%isol      ) ) RETURN
    ! ...Integer arrays
    IF ( ANY(x%k1 /= y%k1) .AND. ANY(x%k2 /= y%k2) ) RETURN
    ! ...Floating point arrays
    IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Tm       , y%Tm       , n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%Predictor, y%Predictor, n))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance(x%w        , y%w        , n))) ) RETURN

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION NLTE_Predictor_Compare


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       NLTE_Predictor_IsActive
!
! PURPOSE:
!       Elemental function to determine if an NLTE_Predictor object is
!       active and valid for use in NLTE correction.
!
! CALLING SEQUENCE:
!       is_active = NLTE_Predictor_IsActive( NLTE_Predictor )
!
! OBJECTS:
!       NLTE_Predictor:  NLTE_Predictor object to be tested.
!                        UNITS:      N/A
!                        TYPE:       NLTE_Predictor_type
!                        DIMENSION:  Scalar or any rank
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_active:     Logical value indicating whether the input predictor
!                      is active and valid for use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NLTE_Predictor_IsActive( NLTE_Predictor ) RESULT( Is_Active )
    TYPE(NLTE_Predictor_type), INTENT(IN) :: NLTE_Predictor
    LOGICAL :: Is_Active
    Is_Active = NLTE_Predictor%Is_Active
  END FUNCTION NLTE_Predictor_IsActive


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
!       NLTE_Predictor_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two NLTE_Predictor objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = NLTE_Predictor_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two NLTE_Predictor objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       NLTE_Predictor_type
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

  ELEMENTAL FUNCTION NLTE_Predictor_Equal( x, y ) RESULT( is_equal )
    TYPE(NLTE_Predictor_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Layers     /= y%n_Layers     ) .OR. &
         (x%n_Predictors /= y%n_Predictors ) ) RETURN
    ! ...Scalars
    IF ( (x%Is_Active  .NEQV. y%Is_Active ) .OR. &
         (x%Compute_Tm .NEQV. y%Compute_Tm) .OR. &
         (x%isen         /=   y%isen      ) .OR. &
         (x%isol         /=   y%isol      ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%k1           ==     y%k1       ) .AND. &
         ALL(x%k2           ==     y%k2       ) .AND. &
         ALL(x%Tm        .EqualTo. y%Tm       ) .AND. &
         ALL(x%Predictor .EqualTo. y%Predictor) .AND. &
         ALL(x%w         .EqualTo. y%w        ) ) &
      is_equal = .TRUE.

  END FUNCTION NLTE_Predictor_Equal

END MODULE NLTE_Predictor_Define

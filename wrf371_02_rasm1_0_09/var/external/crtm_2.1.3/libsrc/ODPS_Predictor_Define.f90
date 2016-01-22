!
! ODPS_Predictor_Define
!
! Module defining the Predictor object for the ODPS algorithm and
! containing routines to manipulate it.
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 20-Jun-2008
!                       based on the content of CRTM_Predictor_Define.f90
!
!       Refactored:     Paul van Delst, 27-Mar-2012
!                       paul.vandelst@noaa.gov
!

MODULE ODPS_Predictor_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE PAFV_Define          , ONLY: PAFV_type      , &
                                   PAFV_Associated, &
                                   PAFV_Destroy   , &
                                   PAFV_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  ! Local entities
  ! ...Datatypes
  PUBLIC :: ODPS_Predictor_type
!  ! ...Operators
!  PUBLIC :: OPERATOR(==)
  ! ...Procedures
  PUBLIC :: ODPS_Predictor_Associated
  PUBLIC :: ODPS_Predictor_Destroy
  PUBLIC :: ODPS_Predictor_Create
  PUBLIC :: ODPS_Predictor_Inspect
  PUBLIC :: ODPS_Predictor_Zero
!  PUBLIC :: ODPS_Predictor_ValidRelease
!  PUBLIC :: ODPS_Predictor_Info
!  PUBLIC :: ODPS_Predictor_DefineVersion
!  PUBLIC :: ODPS_Predictor_InquireFile
!  PUBLIC :: ODPS_Predictor_ReadFile
!  PUBLIC :: ODPS_Predictor_WriteFile
  ! ...Parameter
  PUBLIC :: MAX_OPTRAN_ORDER
  PUBLIC :: MAX_OPTRAN_PREDICTORS
  PUBLIC :: MAX_OPTRAN_USED_PREDICTORS

  ! USE-associated entities to pass through
  ! ...Datatypes
  PUBLIC :: PAFV_type
  ! ...Procedures
  PUBLIC :: PAFV_Associated
  PUBLIC :: PAFV_Destroy
  PUBLIC :: PAFV_Create


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
!  INTERFACE OPERATOR(==)
!    MODULE PROCEDURE ODPS_Predictor_Equal
!  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: xODPS_Predictor_Define.f90 18500 2012-04-02 11:07:35Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: ODPS_PREDICTOR_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODPS_PREDICTOR_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256
  ! C-OPTRAN max. order and number of predictors
  INTEGER, PARAMETER :: MAX_OPTRAN_ORDER = 10
  INTEGER, PARAMETER :: MAX_OPTRAN_PREDICTORS = 14
  INTEGER, PARAMETER :: MAX_OPTRAN_USED_PREDICTORS = 6


  ! -----------------------------------
  ! ODPS_Predictor data type definition
  ! -----------------------------------
  !:tdoc+:
  TYPE :: ODPS_Predictor_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = ODPS_PREDICTOR_RELEASE
    INTEGER :: Version = ODPS_PREDICTOR_VERSION
    ! Dimension variables
    INTEGER :: Max_n_Layers  = 0 ! K   - maximum number of layers
    INTEGER :: n_Layers      = 0 ! K   - actual number of layers
    INTEGER :: n_User_Layers = 0 ! Ku  - number of layers of user input profile
    INTEGER :: n_Components  = 0 ! J   - number of tau components
    INTEGER :: n_Predictors  = 0 ! I   - predictor dimension (Maximum)
    ! Scalars
    REAL(fp) :: Secant_Zenith_Surface  ! Secant zenith angle at the surface
    REAL(fp) :: u, v, w                ! Algorithm specific variables
    ! Arrays
    REAL(fp), ALLOCATABLE :: Secant_Zenith(:)  ! K         ; secant zenith angle
    INTEGER,  ALLOCATABLE :: Component_ID(:)   ! J         ; Tau component Id
    INTEGER,  ALLOCATABLE :: n_CP(:)           ! J         ; No. of predictors for each component
    REAL(fp), ALLOCATABLE :: X(:,:,:)          ! K x I x J ; Predictor array
    REAL(fp), ALLOCATABLE :: Ref_Level_LnPressure(:)   ! 0:K ; Pressure arrays used for optical
    REAL(fp), ALLOCATABLE :: User_Level_LnPressure(:)  ! 0:Ku;   path profile interpolation
    ! Compact-OPTRAN predictors
    LOGICAL :: OPTRAN = .FALSE.
    INTEGER :: n_OPredictors  = 0 ! OI
    REAL(fp), ALLOCATABLE ::  Ap(:,:)  ! MAX_OPTRAN_ORDER x K; polynomial of the water vapor absorber level
    REAL(fp), ALLOCATABLE ::  dA(:)    ! K                   ; slant path layer integrated amount
    REAL(fp), ALLOCATABLE ::  OX(:,:)  ! K x OI              ; Predictor array
    ! Structure variable to hold predictor and absorption
    ! forward variables across FWD, TL and AD calls. It
    ! should be allocated only for the FWD Predictor variable.
    TYPE(PAFV_type) :: PAFV
  END TYPE ODPS_Predictor_type
  !:tdoc-:


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Predictor_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the ODPS_Predictor structure.
!
! CALLING SEQUENCE:
!       Status = ODPS_Predictor_Associated( ODPS_Predictor )
!
! OBJECTS:
!       ODPS_Predictor:
!         Structure which is to have its member's
!         status tested.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:
!         The return value is a logical value indicating the
!         status of the allocated members.
!          .TRUE.  - if the ODPS_Predictor object has been allocated.
!          .FALSE. - if the ODPS_Predictor object has NOT been allocated.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION ODPS_Predictor_Associated( self ) RESULT( Status )
    TYPE(ODPS_Predictor_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION ODPS_Predictor_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Predictor_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize ODPS_Predictor objects.
!
! CALLING SEQUENCE:
!       CALL ODPS_Predictor_Destroy( ODPS_Predictor )
!
! OBJECTS:
!       ODPS_Predictor:
!         Re-initialized ODPS_Predictor structure.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODPS_Predictor_Destroy( self )
    TYPE(ODPS_Predictor_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE ODPS_Predictor_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Predictor_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an ODPS_Predictor object.
!
! CALLING SEQUENCE:
!       CALL ODPS_Predictor_Create( &
!              ODPS_Predictor, &
!              n_Layers      , &
!              n_User_Layers , &
!              n_Components  , &
!              n_Predictors  , &
!              No_OPTRAN = No_OPTRAN )
!
! OBJECTS:
!       ODPS_Predictor:
!         ODPS_Predictor object structure.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:
!         Number of atmospheric layers.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODPS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Components:
!         Number of atmospheric absorption components.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODPS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Predictors:
!         Maximum number of absorption predictor.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODPS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_OPTRAN:
!         Logical switch to disable allocation of Compact-OPTRAN
!         arrays for use with water vapour absorption.
!         If == .FALSE., arrays are allocated [DEFAULT]
!            == .TRUE.,  arrays are NOT allocated
!         If not specified, arrays are allocated.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Conformable with the ODPS_Predictor object
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODPS_Predictor_Create( &
    self         , &  ! Output
    n_Layers     , &  ! Input
    n_User_Layers, &  ! Input
    n_Components , &  ! Input
    n_Predictors , &  ! Input
    No_OPTRAN      )  ! Optional Input
    ! Arguments
    TYPE(ODPS_Predictor_type), INTENT(OUT) :: self
    INTEGER,                   INTENT(IN)  :: n_Layers
    INTEGER,                   INTENT(IN)  :: n_User_Layers
    INTEGER,                   INTENT(IN)  :: n_Components
    INTEGER,                   INTENT(IN)  :: n_Predictors
    LOGICAL,         OPTIONAL, INTENT(IN)  :: No_OPTRAN
    ! Local variables
    LOGICAL :: use_optran
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers      < 1 .OR. &
         n_User_Layers < 1 .OR. &
         n_Components  < 1 .OR. &
         n_Predictors  < 1 ) RETURN
    ! ...Process options
    use_optran = .TRUE.
    IF ( PRESENT(No_OPTRAN) ) use_optran = .NOT. No_OPTRAN

    ! Perform the allocation
    ALLOCATE( self%Secant_Zenith(n_Layers), &
              self%Component_ID(n_Components), &
              self%n_CP(n_Components), &
              self%X(n_Layers, n_Predictors, n_Components), &
              self%Ref_Level_LnPressure(0:n_Layers), &
              self%User_Level_LnPressure(0:n_User_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise dimensions
    self%Max_n_Layers  = n_Layers
    self%n_Layers      = n_Layers
    self%n_User_Layers = n_User_Layers
    self%n_Components  = n_Components
    self%n_Predictors  = n_Predictors


    ! Allocate OPTRAN if required
    IF ( use_optran ) THEN
      ALLOCATE( self%OX(n_Layers, MAX_OPTRAN_PREDICTORS), &
                self%Ap(n_Layers, MAX_OPTRAN_ORDER), &
                self%dA(n_Layers), &
                STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) RETURN
      ! Initialise dimensions
      self%n_OPredictors = MAX_OPTRAN_PREDICTORS
      ! ...Flag OPTRAN section as usuable
      self%OPTRAN = .TRUE.
    END IF


    ! Set allocation indicator
    self%Is_Allocated = .TRUE.


    ! Initialise array data
    CALL ODPS_Predictor_Zero(self)

  END SUBROUTINE ODPS_Predictor_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Predictor_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a ODPS_Predictor object to stdout.
!
! CALLING SEQUENCE:
!       CALL ODPS_Predictor_Inspect( ODPS_Predictor )
!
! OBJECTS:
!       ODPS_Predictor:
!         ODPS_Predictor object to display.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Predictor_Inspect(self)
    TYPE(ODPS_Predictor_type), INTENT(IN) :: self
    INTEGER :: i, j
    WRITE(*,'(1x,"ODPS_Predictor OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Layers      :",1x,i0)') self%n_Layers
    WRITE(*,'(3x,"n_User_Layers :",1x,i0)') self%n_User_Layers
    WRITE(*,'(3x,"n_Components  :",1x,i0)') self%n_Components
    WRITE(*,'(3x,"n_Predictors  :",1x,i0)') self%n_Predictors
    IF ( .NOT. ODPS_Predictor_Associated(self) ) RETURN
    ! ODPS data arrays
    WRITE(*,'(3x,"ODPS data arrays :")')
    ! ...ODPS Forward variables
    WRITE(*,'(5x,"Secant_Zenith :")')
    WRITE(*,'(5(1x,es23.15e3,:))') self%Secant_Zenith(1:self%n_Layers)
    WRITE(*,'(5x,"Component_ID :")')
    WRITE(*,'(10(1x,i0,:))') self%Component_ID(1:self%n_Components)
    WRITE(*,'(5x,"n_CP :")')
    WRITE(*,'(10(1x,i0,:))') self%n_CP(1:self%n_Components)
    WRITE(*,'(5x,"X (predictor array) :")')
    DO j = 1, self%n_Components
      DO i = 1, self%n_Predictors
        WRITE(*,'(7x,"Component#: ",i0,"; Predictor#: ",i0)') j, i
        WRITE(*,'(5(1x,es23.15e3,:))') self%X(1:self%n_Layers,i,j)
      END DO
      WRITE(*,*)
    END DO
    ! ...Pressure profiles for interpolations
    WRITE(*,'(5x,"Ref_Level_LnPressure  :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Ref_Level_LnPressure
    WRITE(*,'(5x,"User_Level_LnPressure :")')
    WRITE(*,'(5(1x,es13.6,:))') self%User_Level_LnPressure
    ! Compact-OPTRAN Forward variables
    IF ( self%OPTRAN ) THEN
      WRITE(*,'(3x,"n_OPredictors :",1x,i0)') self%n_OPredictors
      WRITE(*,'(5x,"OX :")'); WRITE(*,'(5(1x,es13.6,:))') self%OX
      WRITE(*,'(5x,"Ap :")'); WRITE(*,'(5(1x,es13.6,:))') self%Ap
      WRITE(*,'(5x,"dA :")'); WRITE(*,'(5(1x,es13.6,:))') self%dA
    END IF
  END SUBROUTINE ODPS_Predictor_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Predictor_Zero
!
! PURPOSE:
!       Elementl subroutine to zero-out an instance of an ODPS predictor object.
!
! CALLING SEQUENCE:
!       CALL ODPS_Predictor_Zero( ODPS_Predictor )
!
! OUTPUTS:
!       ODPS_Predictor:
!         ODPS_Predictor object structure.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODPS_Predictor_Zero( self )
    TYPE(ODPS_Predictor_type),  INTENT(IN OUT) :: self
    IF ( .NOT. ODPS_Predictor_Associated(self) ) RETURN
    self%Secant_Zenith = ZERO
    self%Component_ID  = 0
    self%n_CP          = self%n_Predictors
    self%X             = ZERO
    self%Ref_Level_LnPressure  = ZERO
    self%User_Level_LnPressure = ZERO
    IF ( self%OPTRAN ) THEN
      self%OX = ZERO
      self%Ap = ZERO
      self%dA = ZERO
    END IF
  END SUBROUTINE ODPS_Predictor_Zero

END MODULE ODPS_Predictor_Define

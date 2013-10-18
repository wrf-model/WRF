!
! PAFV_Define
!
! Module defining the PAFV object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Mar-2012
!                       paul.vandelst@noaa.gov

MODULE PAFV_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
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
  ! Datatypes
  PUBLIC :: PAFV_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: PAFV_Associated
  PUBLIC :: PAFV_Destroy
  PUBLIC :: PAFV_Create
  PUBLIC :: PAFV_Inspect
  PUBLIC :: PAFV_ValidRelease
  PUBLIC :: PAFV_Info
  PUBLIC :: PAFV_DefineVersion
  PUBLIC :: PAFV_InquireFile
  PUBLIC :: PAFV_ReadFile
  PUBLIC :: PAFV_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE PAFV_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: PAFV_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: PAFV_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: PAFV_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  ! Compact-OPTRAN data indicator
  INTEGER, PARAMETER :: DATA_MISSING = 0
  INTEGER, PARAMETER :: DATA_PRESENT = 1
  ! Compact-OPTRAN max. order and number of predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_ORDER = 10
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_PREDICTORS = 14
  INTEGER, PUBLIC, PARAMETER :: MAX_OPTRAN_USED_PREDICTORS = 6


  ! -------------------------
  ! PAFV data type definition
  ! -------------------------
  !:tdoc+:
  TYPE :: PAFV_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = PAFV_RELEASE
    INTEGER :: Version = PAFV_VERSION
    ! Dimensions variables
    INTEGER :: n_ODPS_Layers = 0   ! K
    INTEGER :: n_Absorbers   = 0   ! J
    INTEGER :: n_User_Layers = 0   ! uK
    ! ODPS Forward variables
    ! ...Index array for ODPS to user profile interpolations
    INTEGER,  ALLOCATABLE :: ODPS2User_Idx(:,:) ! 2 x 0:uK
    ! ...Index array for user to ODPS profile interpolations
    INTEGER,  ALLOCATABLE :: interp_index(:,:) ! 2 x K
    ! ...Accumulated weighting factors array for user to ODPS profile interpolations
    REAL(fp), ALLOCATABLE :: Acc_Weighting(:,:) ! uK x K
    ! ...Profile data
    REAL(fp), ALLOCATABLE :: Temperature(:)   ! K
    REAL(fp), ALLOCATABLE :: Absorber(:,:)    ! K x J
    INTEGER,  ALLOCATABLE :: idx_map(:)       ! K
    INTEGER  :: H2O_idx = 0
    ! Pressure profiles for interpolations
    REAL(fp), ALLOCATABLE :: Ref_LnPressure(:)    ! K
    REAL(fp), ALLOCATABLE :: User_LnPressure(:)   ! uK
    ! Predictor Forward variables
    REAL(fp), ALLOCATABLE :: PDP(:)      ! K
    REAL(fp), ALLOCATABLE :: Tz_ref(:)   ! K
    REAL(fp), ALLOCATABLE :: Tz(:)       ! K
    REAL(fp), ALLOCATABLE :: Tzp_ref(:)  ! K
    REAL(fp), ALLOCATABLE :: Tzp(:)      ! K
    ! ...
    REAL(fp), ALLOCATABLE :: GAz_ref(:,:)    ! K x J
    REAL(fp), ALLOCATABLE :: GAz_sum(:,:)    ! K x J
    REAL(fp), ALLOCATABLE :: GAz(:,:)        ! K x J
    REAL(fp), ALLOCATABLE :: GAzp_ref(:,:)   ! K x J
    REAL(fp), ALLOCATABLE :: GAzp_sum(:,:)   ! K x J
    REAL(fp), ALLOCATABLE :: GAzp(:,:)       ! K x J
    REAL(fp), ALLOCATABLE :: GATzp_ref(:,:)  ! K x J
    REAL(fp), ALLOCATABLE :: GATzp_sum(:,:)  ! K x J
    REAL(fp), ALLOCATABLE :: GATzp(:,:)      ! K x J
    ! ...
    REAL(fp), ALLOCATABLE :: DT(:)          ! K
    REAL(fp), ALLOCATABLE :: T(:)           ! K
    REAL(fp), ALLOCATABLE :: T2(:)          ! K
    REAL(fp), ALLOCATABLE :: DT2(:)         ! K
    REAL(fp), ALLOCATABLE :: H2O(:)         ! K
    REAL(fp), ALLOCATABLE :: H2O_A(:)       ! K
    REAL(fp), ALLOCATABLE :: H2O_R(:)       ! K
    REAL(fp), ALLOCATABLE :: H2O_S(:)       ! K
    REAL(fp), ALLOCATABLE :: H2O_R4(:)      ! K
    REAL(fp), ALLOCATABLE :: H2OdH2OTzp(:)  ! K
    REAL(fp), ALLOCATABLE :: CO2(:)         ! K
    REAL(fp), ALLOCATABLE :: O3(:)          ! K
    REAL(fp), ALLOCATABLE :: O3_A(:)        ! K
    REAL(fp), ALLOCATABLE :: O3_R(:)        ! K
    REAL(fp), ALLOCATABLE :: CO(:)          ! K
    REAL(fp), ALLOCATABLE :: CO_A(:)        ! K
    REAL(fp), ALLOCATABLE :: CO_R(:)        ! K
    REAL(fp), ALLOCATABLE :: CO_S(:)        ! K
    REAL(fp), ALLOCATABLE :: CO_ACOdCOzp(:) ! K
    REAL(fp), ALLOCATABLE :: N2O(:)         ! K
    REAL(fp), ALLOCATABLE :: N2O_A(:)       ! K
    REAL(fp), ALLOCATABLE :: N2O_R(:)       ! K
    REAL(fp), ALLOCATABLE :: N2O_S(:)       ! K
    REAL(fp), ALLOCATABLE :: CH4(:)         ! K
    REAL(fp), ALLOCATABLE :: CH4_A(:)       ! K
    REAL(fp), ALLOCATABLE :: CH4_R(:)       ! K
    REAL(fp), ALLOCATABLE :: CH4_ACH4zp(:)  ! K
    ! Optical depth Forward variables
    REAL(fp), ALLOCATABLE :: OD(:)       ! K
    REAL(fp), ALLOCATABLE :: OD_Path(:)  ! K
    ! Zeeman specific Forward variables
    REAL(fp) :: w1, w2  ! weights for two-points linear interpolation
    INTEGER  :: inode   ! node position
    ! Compact-OPTRAN Forward variables
    LOGICAL :: OPTRAN = .FALSE.
    ! ...Dimensions
    INTEGER :: n_OUsed_Pred  = MAX_OPTRAN_USED_PREDICTORS   ! oI; No. of OPTRAN used predictors
    ! ...Predictor variables
    REAL(fp), ALLOCATABLE :: dPonG(:)       ! K
    REAL(fp), ALLOCATABLE :: d_Absorber(:)  ! K
    REAL(fp), ALLOCATABLE :: Int_vapor(:)   ! K
    REAL(fp), ALLOCATABLE :: AveA(:)        ! K
    REAL(fp), ALLOCATABLE :: Inverse(:)     ! K
    REAL(fp), ALLOCATABLE :: s_t(:)         ! K
    REAL(fp), ALLOCATABLE :: s_p(:)         ! K
    REAL(fp), ALLOCATABLE :: Ap1(:)         ! K
    ! ...Optical depth variables
    REAL(fp), ALLOCATABLE :: b(:,:)     ! K x 0:oI
    REAL(fp), ALLOCATABLE :: LN_Chi(:)  ! K
    REAL(fp), ALLOCATABLE :: Chi(:)     ! K
  END TYPE PAFV_type
  !:tdoc-:


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the PAFV structure.
!
! CALLING SEQUENCE:
!       Status = PAFV_Associated( PAFV )
!
! OBJECTS:
!       PAFV:
!         Structure which is to have its member's
!         status tested.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:
!         The return value is a logical value indicating the
!         status of the allocated members.
!          .TRUE.  - if ANY of the PAFV allocatable members
!                    are in use.
!          .FALSE. - if ALL of the PAFV allocatable members
!                    are not in use.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION PAFV_Associated( self ) RESULT( Status )
    TYPE(PAFV_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION PAFV_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize PAFV objects.
!
! CALLING SEQUENCE:
!       CALL PAFV_Destroy( PAFV )
!
! OBJECTS:
!       PAFV:
!         Re-initialized PAFV structure.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PAFV_Destroy( self )
    TYPE(PAFV_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE PAFV_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an PAFV object.
!
! CALLING SEQUENCE:
!       CALL PAFV_Create( &
!              PAFV         , &
!              n_ODPS_Layers, &
!              n_User_Layers, &
!              n_Absorbers  , &
!              No_OPTRAN = No_OPTRAN )
!
! OBJECTS:
!       PAFV:
!         PAFV object structure.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_ODPS_Layers:
!         Number of internal ODPS layers that are defined
!         in the ODPS TauCoeff data file.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the PAFV object
!         ATTRIBUTES: INTENT(IN)
!
!       n_User_Layers:
!         Number of atmospheric layers defined by user.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the PAFV object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:
!         Number of gaseous absorbers.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the PAFV object
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
!         DIMENSION:  Conformable with the PAFV object
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE PAFV_Create( &
    self         , &  ! Output
    n_ODPS_Layers, &  ! Input
    n_User_Layers, &  ! Input
    n_Absorbers  , &  ! Input
    No_OPTRAN      )  ! Optional Input

    ! Arguments
    TYPE(PAFV_type),   INTENT(OUT) :: self
    INTEGER,           INTENT(IN)  :: n_ODPS_Layers
    INTEGER,           INTENT(IN)  :: n_User_Layers
    INTEGER,           INTENT(IN)  :: n_Absorbers
    LOGICAL, OPTIONAL, INTENT(IN)  :: No_OPTRAN
    ! Local variables
    LOGICAL :: use_optran
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_ODPS_Layers < 1 .OR. &
         n_Absorbers   < 1 .OR. &
         n_User_Layers < 1 ) RETURN
    ! ...Process options
    use_optran = .TRUE.
    IF ( PRESENT(No_OPTRAN) ) use_optran = .NOT. No_OPTRAN

    ! Perform the ODPS allocations
    ! ...ODPS Forward variables
    ALLOCATE( self%ODPS2User_Idx(2, 0:n_User_Layers), &
              self%interp_index(2, n_ODPS_Layers),  &
              self%Acc_Weighting(n_User_Layers,n_ODPS_Layers), &
              self%Temperature(n_ODPS_Layers), &
              self%Absorber(n_ODPS_Layers, n_Absorbers), &
              self%idx_map(n_Absorbers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ! ...Pressure profiles for interpolations
    ALLOCATE( self%Ref_LnPressure(n_ODPS_Layers), &
              self%User_LnPressure(n_User_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ! Predictor forward variables
    ! ...
    ALLOCATE( self%PDP(n_ODPS_Layers), &
              self%Tz_ref(n_ODPS_Layers), &
              self%Tz(n_ODPS_Layers), &
              self%Tzp_ref(n_ODPS_Layers), &
              self%Tzp(n_ODPS_Layers), &
              self%GAz_ref(n_ODPS_Layers, n_Absorbers), &
              self%GAz_sum(n_ODPS_Layers, n_Absorbers), &
              self%GAz(n_ODPS_Layers, n_Absorbers), &
              self%GAzp_ref(n_ODPS_Layers, n_Absorbers), &
              self%GAzp_sum(n_ODPS_Layers, n_Absorbers), &
              self%GAzp(n_ODPS_Layers, n_Absorbers), &
              self%GATzp_ref(n_ODPS_Layers, n_Absorbers), &
              self%GATzp_sum(n_ODPS_Layers, n_Absorbers), &
              self%GATzp(n_ODPS_Layers, n_Absorbers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ! ...
    ALLOCATE( self%DT(n_ODPS_Layers),           &
              self%T(n_ODPS_Layers),            &
              self%T2(n_ODPS_Layers),           &
              self%DT2(n_ODPS_Layers),          &
              self%H2O(n_ODPS_Layers),          &
              self%H2O_A(n_ODPS_Layers),        &
              self%H2O_R(n_ODPS_Layers),        &
              self%H2O_S(n_ODPS_Layers),        &
              self%H2O_R4(n_ODPS_Layers),       &
              self%H2OdH2OTzp(n_ODPS_Layers),   &
              self%CO2(n_ODPS_Layers),          &
              self%O3(n_ODPS_Layers),           &
              self%O3_A(n_ODPS_Layers),         &
              self%O3_R(n_ODPS_Layers),         &
              self%CO(n_ODPS_Layers),           &
              self%CO_A(n_ODPS_Layers),         &
              self%CO_R(n_ODPS_Layers),         &
              self%CO_S(n_ODPS_Layers),         &
              self%CO_ACOdCOzp(n_ODPS_Layers),  &
              self%N2O(n_ODPS_Layers),          &
              self%N2O_A(n_ODPS_Layers),        &
              self%N2O_R(n_ODPS_Layers),        &
              self%N2O_S(n_ODPS_Layers),        &
              self%CH4(n_ODPS_Layers),          &
              self%CH4_A(n_ODPS_Layers),        &
              self%CH4_R(n_ODPS_Layers),        &
              self%CH4_ACH4zp(n_ODPS_Layers),   &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ! ...Optical depth variables
    ALLOCATE( self%OD(n_ODPS_Layers),           &
              self%OD_path(0:n_ODPS_Layers),    &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise dimensions (not arrays)
    self%n_ODPS_Layers = n_ODPS_Layers
    self%n_Absorbers   = n_Absorbers
    self%n_User_Layers = n_User_Layers


    ! Allocate OPTRAN if required
    IF ( use_optran ) THEN
      ALLOCATE( self%dPonG(n_ODPS_Layers), &
                self%d_Absorber(n_ODPS_Layers), &
                self%Int_vapor(n_ODPS_Layers), &
                self%AveA(n_ODPS_Layers), &
                self%Inverse(n_ODPS_Layers), &
                self%s_t(n_ODPS_Layers), &
                self%s_p(n_ODPS_Layers), &
                self%Ap1(n_ODPS_Layers), &
                self%b(n_ODPS_Layers, 0:MAX_OPTRAN_USED_PREDICTORS), &
                self%LN_Chi(n_ODPS_Layers), &
                self%Chi(n_ODPS_Layers), &
                STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) RETURN
      ! ...Initialise dimensions
      self%n_OUsed_Pred  = MAX_OPTRAN_USED_PREDICTORS
      ! ...Flag OPTRAN section as usuable
      self%OPTRAN = .TRUE.
    END IF

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE PAFV_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a PAFV object to stdout.
!
! CALLING SEQUENCE:
!       CALL PAFV_Inspect( PAFV )
!
! OBJECTS:
!       PAFV:
!         PAFV object to display.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE PAFV_Inspect(self)
    TYPE(PAFV_type), INTENT(IN) :: self
    WRITE(*,'(1x,"PAFV OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version     :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_ODPS_Layers :",1x,i0)') self%n_ODPS_Layers
    WRITE(*,'(3x,"n_User_Layers :",1x,i0)') self%n_User_Layers
    WRITE(*,'(3x,"n_Absorbers   :",1x,i0)') self%n_Absorbers
    IF ( .NOT. PAFV_Associated(self) ) RETURN
    ! ODPS data arrays
    WRITE(*,'(3x,"ODPS data arrays :")')
    ! ...ODPS Forward variables
    WRITE(*,'(5x,"ODPS2User_Idx :")'); WRITE(*,'(10(1x,i3,:))') self%ODPS2User_Idx
    WRITE(*,'(5x,"interp_index  :")'); WRITE(*,'(10(1x,i3,:))') self%interp_index
    WRITE(*,'(5x,"Acc_Weighting :")'); WRITE(*,'(5(1x,es13.6,:))') self%Acc_Weighting
    WRITE(*,'(5x,"Temperature   :")'); WRITE(*,'(5(1x,es13.6,:))') self%Temperature
    WRITE(*,'(5x,"Absorber      :")'); WRITE(*,'(5(1x,es13.6,:))') self%Absorber
    WRITE(*,'(5x,"idx_map       :")'); WRITE(*,'(10(1x,i3,:))') self%idx_map
    WRITE(*,'(5x,"H2O_idx       :",1x,i0)') self%H2O_idx
    ! ...Pressure profiles for interpolations
    WRITE(*,'(5x,"Ref_LnPressure  :")'); WRITE(*,'(5(1x,es13.6,:))') self%Ref_LnPressure
    WRITE(*,'(5x,"User_LnPressure :")'); WRITE(*,'(5(1x,es13.6,:))') self%User_LnPressure
    ! ...Predictor forward variables
    WRITE(*,'(5x,"PDP         :")'); WRITE(*,'(5(1x,es13.6,:))') self%PDP
    WRITE(*,'(5x,"Tz_ref      :")'); WRITE(*,'(5(1x,es13.6,:))') self%Tz_ref
    WRITE(*,'(5x,"Tz          :")'); WRITE(*,'(5(1x,es13.6,:))') self%Tz
    WRITE(*,'(5x,"Tzp_ref     :")'); WRITE(*,'(5(1x,es13.6,:))') self%Tzp_ref
    WRITE(*,'(5x,"Tzp         :")'); WRITE(*,'(5(1x,es13.6,:))') self%Tzp
    WRITE(*,'(5x,"GAz_ref     :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAz_ref
    WRITE(*,'(5x,"GAz_sum     :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAz_sum
    WRITE(*,'(5x,"GAz         :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAz
    WRITE(*,'(5x,"GAzp_ref    :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAzp_ref
    WRITE(*,'(5x,"GAzp_sum    :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAzp_sum
    WRITE(*,'(5x,"GAzp        :")'); WRITE(*,'(5(1x,es13.6,:))') self%GAzp
    WRITE(*,'(5x,"GATzp_ref   :")'); WRITE(*,'(5(1x,es13.6,:))') self%GATzp_ref
    WRITE(*,'(5x,"GATzp_sum   :")'); WRITE(*,'(5(1x,es13.6,:))') self%GATzp_sum
    WRITE(*,'(5x,"GATzp       :")'); WRITE(*,'(5(1x,es13.6,:))') self%GATzp
    WRITE(*,'(5x,"DT          :")'); WRITE(*,'(5(1x,es13.6,:))') self%DT
    WRITE(*,'(5x,"T           :")'); WRITE(*,'(5(1x,es13.6,:))') self%T
    WRITE(*,'(5x,"T2          :")'); WRITE(*,'(5(1x,es13.6,:))') self%T2
    WRITE(*,'(5x,"DT2         :")'); WRITE(*,'(5(1x,es13.6,:))') self%DT2
    WRITE(*,'(5x,"H2O         :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2O
    WRITE(*,'(5x,"H2O_A       :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2O_A
    WRITE(*,'(5x,"H2O_R       :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2O_R
    WRITE(*,'(5x,"H2O_S       :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2O_S
    WRITE(*,'(5x,"H2O_R4      :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2O_R4
    WRITE(*,'(5x,"H2OdH2OTzp  :")'); WRITE(*,'(5(1x,es13.6,:))') self%H2OdH2OTzp
    WRITE(*,'(5x,"CO2         :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO2
    WRITE(*,'(5x,"O3          :")'); WRITE(*,'(5(1x,es13.6,:))') self%O3
    WRITE(*,'(5x,"O3_A        :")'); WRITE(*,'(5(1x,es13.6,:))') self%O3_A
    WRITE(*,'(5x,"O3_R        :")'); WRITE(*,'(5(1x,es13.6,:))') self%O3_R
    WRITE(*,'(5x,"CO          :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO
    WRITE(*,'(5x,"CO_A        :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO_A
    WRITE(*,'(5x,"CO_R        :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO_R
    WRITE(*,'(5x,"CO_S        :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO_S
    WRITE(*,'(5x,"CO_ACOdCOzp :")'); WRITE(*,'(5(1x,es13.6,:))') self%CO_ACOdCOzp
    WRITE(*,'(5x,"N2O         :")'); WRITE(*,'(5(1x,es13.6,:))') self%N2O
    WRITE(*,'(5x,"N2O_A       :")'); WRITE(*,'(5(1x,es13.6,:))') self%N2O_A
    WRITE(*,'(5x,"N2O_R       :")'); WRITE(*,'(5(1x,es13.6,:))') self%N2O_R
    WRITE(*,'(5x,"N2O_S       :")'); WRITE(*,'(5(1x,es13.6,:))') self%N2O_S
    WRITE(*,'(5x,"CH4         :")'); WRITE(*,'(5(1x,es13.6,:))') self%CH4
    WRITE(*,'(5x,"CH4_A       :")'); WRITE(*,'(5(1x,es13.6,:))') self%CH4_A
    WRITE(*,'(5x,"CH4_R       :")'); WRITE(*,'(5(1x,es13.6,:))') self%CH4_R
    WRITE(*,'(5x,"CH4_ACH4zp  :")'); WRITE(*,'(5(1x,es13.6,:))') self%CH4_ACH4zp
    ! Optical depth variables
    WRITE(*,'(3x,"ODPS optical depth arrays :")')
    WRITE(*,'(5x,"OD      :")'); WRITE(*,'(5(1x,es13.6,:))') self%OD
    WRITE(*,'(5x,"OD_path :")'); WRITE(*,'(5(1x,es13.6,:))') self%OD_path
    ! Zeeman specific Forward variables
    WRITE(*,'(3x,"Zeeman-specific data :")')
    WRITE(*,'(5x,"w1, w2 :")'); WRITE(*,'(2(1x,es13.6))') self%w1, self%w2
    WRITE(*,'(5x,"inode  :")'); WRITE(*,'(1x,i0)') self%inode
    ! Compact-OPTRAN Forward variables
    IF ( self%OPTRAN ) THEN
      WRITE(*,'(3x,"Compact-OPTRAN option :")')
      WRITE(*,'(3x,"n_OUsed_Pred :",1x,i0)') self%n_OUsed_Pred
      WRITE(*,'(5x,"dPonG      :")'); WRITE(*,'(5(1x,es13.6,:))') self%dPonG
      WRITE(*,'(5x,"d_Absorber :")'); WRITE(*,'(5(1x,es13.6,:))') self%d_Absorber
      WRITE(*,'(5x,"Int_vapor  :")'); WRITE(*,'(5(1x,es13.6,:))') self%Int_vapor
      WRITE(*,'(5x,"AveA       :")'); WRITE(*,'(5(1x,es13.6,:))') self%AveA
      WRITE(*,'(5x,"Inverse    :")'); WRITE(*,'(5(1x,es13.6,:))') self%Inverse
      WRITE(*,'(5x,"s_t        :")'); WRITE(*,'(5(1x,es13.6,:))') self%s_t
      WRITE(*,'(5x,"s_p        :")'); WRITE(*,'(5(1x,es13.6,:))') self%s_p
      WRITE(*,'(5x,"Ap1        :")'); WRITE(*,'(5(1x,es13.6,:))') self%Ap1
      WRITE(*,'(5x,"b          :")'); WRITE(*,'(5(1x,es13.6,:))') self%b
      WRITE(*,'(5x,"LN_Chi     :")'); WRITE(*,'(5(1x,es13.6,:))') self%LN_Chi
      WRITE(*,'(5x,"Chi        :")'); WRITE(*,'(5(1x,es13.6,:))') self%Chi
    END IF
  END SUBROUTINE PAFV_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_ValidRelease
!
! PURPOSE:
!       Function to check the PAFV Release value.
!
! CALLING SEQUENCE:
!       IsValid = PAFV_ValidRelease( PAFV )
!
! INPUTS:
!       PAFV:
!         PAFV object for which the Release component
!         is to be checked.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:
!         Logical value defining the release validity.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION PAFV_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(PAFV_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PAFV_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < PAFV_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An PAFV data update is needed. ", &
                  &"PAFV release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, PAFV_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > PAFV_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An PAFV software update is needed. ", &
                  &"PAFV release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, PAFV_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION PAFV_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a PAFV object.
!
! CALLING SEQUENCE:
!       CALL PAFV_Info( PAFV, Info )
!
! OBJECTS:
!       PAFV:
!         PAFV object about which info is required.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:
!         String containing version and dimension information
!         about the PAFV object.
!         UNITS:      N/A
!         TYPE:       CHARACTER(*)
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE PAFV_Info( self, Info )
    ! Arguments
    TYPE(PAFV_type), INTENT(IN)  :: self
    CHARACTER(*),          INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(1000) :: s1, s2
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( s1, &
           '(a,1x,"PAFV RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_ODPS_LAYERS=",i0,2x,&
           &"N_ABSORBERS  =",i0,2x,&
           &"N_USER_LAYERS=",i0,2x)' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_ODPS_Layers, &
           self%n_Absorbers, &
           self%n_User_Layers
    ! Compact-OPTRAN Forward variables
    IF ( self%OPTRAN ) THEN
      WRITE( s2, &
           '(a,1x,"PAFV ODAS Option",a,3x, &
           &"N_OUSED_PRED=",i0,2x)' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_OUsed_Pred
    END IF

    ! Trim the output based on the
    ! dummy argument string length
    Long_String = TRIM(s1)//TRIM(s2)
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE PAFV_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL PAFV_DefineVersion( Id )
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

  SUBROUTINE PAFV_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE PAFV_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_InquireFile
!
! PURPOSE:
!       Function to inquire PAFV object files.
!
! CALLING SEQUENCE:
!       Error_Status = PAFV_InquireFile( &
!                        Filename, &
!                        n_ODPS_Layers = n_ODPS_Layers, &
!                        n_Absorbers   = n_Absorbers  , &
!                        n_User_Layers = n_User_Layers, &
!                        Release       = Release      , &
!                        Version       = Version        )
!
! INPUTS:
!       Filename:
!         Character string specifying the name of the
!         data file to inquire.
!         UNITS:      N/A
!         TYPE:       CHARACTER(*)
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_ODPS_Layers:
!         Number of internal ODPS layers that are defined
!         in the ODPS TauCoeff data file.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:
!         Number of gaseous absorbers.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Layers:
!         Number of atmospheric layers defined by user.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:
!         The data/file release number. Used to check
!         for data/software mismatch.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:
!         The data/file version number. Used for
!         purposes only in identifying the dataset for
!         a particular release.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:
!         The return value is an integer defining the error
!         status. The error codes are defined in the
!         Message_Handler module.
!         If == SUCCESS the file inquire was successful
!            == FAILURE an unrecoverable error occurred.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION PAFV_InquireFile( &
    Filename     , &  ! Input
    n_ODPS_Layers, &  ! Optional output
    n_Absorbers  , &  ! Optional output
    n_User_Layers, &  ! Optional output
    Release      , &  ! Optional output
    Version      , &  ! Optional output
    Title        , &  ! Optional output
    History      , &  ! Optional output
    Comment      ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_ODPS_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_User_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PAFV_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: optran_present
    TYPE(PAFV_type) :: pafv


    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      pafv%Release, &
      pafv%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. PAFV_ValidRelease( pafv ) ) THEN
      msg = 'PAFV Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      pafv%n_ODPS_Layers, &
      pafv%n_Absorbers  , &
      pafv%n_User_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read Compact-OPTRAN data indicator
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) optran_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Compact-OPTRAN data indicator from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(n_ODPS_Layers) ) n_ODPS_Layers = pafv%n_ODPS_Layers
    IF ( PRESENT(n_Absorbers  ) ) n_Absorbers   = pafv%n_Absorbers
    IF ( PRESENT(n_User_Layers) ) n_User_Layers = pafv%n_User_Layers
    IF ( PRESENT(Release      ) ) Release       = pafv%Release
    IF ( PRESENT(Version      ) ) Version       = pafv%Version

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION PAFV_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_ReadFile
!
! PURPOSE:
!       Function to read PAFV object files.
!
! CALLING SEQUENCE:
!       Error_Status = PAFV_ReadFile( &
!                        PAFV    , &
!                        Filename, &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       PAFV:
!         PAFV object containing the data read from file.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:
!         Character string specifying the name of a
!         PAFV data file to read.
!         UNITS:      N/A
!         TYPE:       CHARACTER(*)
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:
!         Set this logical argument to *NOT* close the datafile
!         upon exiting this routine. This option is required if
!         the PAFV data is embedded within another file.
!         If == .FALSE., File is closed upon function exit [DEFAULT].
!            == .TRUE.,  File is NOT closed upon function exit
!         If not specified, default is .FALSE.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:
!         Set this logical argument to suppress INFORMATION
!         messages being printed to stdout
!         If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!            == .TRUE.,  INFORMATION messages are SUPPRESSED.
!         If not specified, default is .FALSE.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:
!         The return value is an integer defining the error status.
!         The error codes are defined in the Message_Handler module.
!         If == SUCCESS, the file read was successful
!            == FAILURE, an unrecoverable error occurred.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION PAFV_ReadFile( &
    PAFV, &  ! Output
    Filename  , &  ! Input
    No_Close  , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   , &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(PAFV_type),  INTENT(OUT) :: PAFV
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PAFV_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: optran_present
    TYPE(PAFV_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check if the file is open.
    IF ( File_Open( Filename ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file if it exists
      IF ( File_Exists( Filename ) ) THEN
        err_stat = Open_Binary_File( Filename, fid )
        IF ( err_Stat /= SUCCESS ) THEN
          msg = 'Error opening '//TRIM(Filename)
          CALL Read_CleanUp(); RETURN
        END IF
      ELSE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. PAFV_ValidRelease( dummy ) ) THEN
      msg = 'PAFV Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%n_ODPS_Layers, &
      dummy%n_Absorbers  , &
      dummy%n_User_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read Compact-OPTRAN data indicator
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) optran_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Compact-OPTRAN data indicator - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the object
    CALL PAFV_Create( &
           PAFV, &
           dummy%n_ODPS_Layers, &
           dummy%n_Absorbers  , &
           dummy%n_User_Layers , &
           No_OPTRAN = (optran_present == DATA_MISSING) )
    IF ( .NOT. PAFV_Associated( PAFV ) ) THEN
      msg = 'PAFV object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    PAFV%Version = dummy%Version


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the ODPS forward variables
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%ODPS2User_Idx, &
      PAFV%interp_index , &
      PAFV%Acc_Weighting, &
      PAFV%Temperature  , &
      PAFV%Absorber     , &
      PAFV%idx_map      , &
      PAFV%H2O_idx
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading ODPS forward variables - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the pressure profiles for interpolation
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%Ref_LnPressure, &
      PAFV%User_LnPressure
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading pressure profiles - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the predictor forward variables
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%PDP      , &
      PAFV%Tz_ref   , &
      PAFV%Tz       , &
      PAFV%Tzp_ref  , &
      PAFV%Tzp      , &
      PAFV%GAz_ref  , &
      PAFV%GAz_sum  , &
      PAFV%GAz      , &
      PAFV%GAzp_ref , &
      PAFV%GAzp_sum , &
      PAFV%GAzp     , &
      PAFV%GATzp_ref, &
      PAFV%GATzp_sum, &
      PAFV%GATzp
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading predictor forward variables (set1) - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%DT         , &
      PAFV%T          , &
      PAFV%T2         , &
      PAFV%DT2        , &
      PAFV%H2O        , &
      PAFV%H2O_A      , &
      PAFV%H2O_R      , &
      PAFV%H2O_S      , &
      PAFV%H2O_R4     , &
      PAFV%H2OdH2OTzp , &
      PAFV%CO2        , &
      PAFV%O3         , &
      PAFV%O3_A       , &
      PAFV%O3_R       , &
      PAFV%CO         , &
      PAFV%CO_A       , &
      PAFV%CO_R       , &
      PAFV%CO_S       , &
      PAFV%CO_ACOdCOzp, &
      PAFV%N2O        , &
      PAFV%N2O_A      , &
      PAFV%N2O_R      , &
      PAFV%N2O_S      , &
      PAFV%CH4        , &
      PAFV%CH4_A      , &
      PAFV%CH4_R      , &
      PAFV%CH4_ACH4zp
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading predictor forward variables (set2) - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the optical depth variables
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%OD, &
      PAFV%OD_path
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading optical depth variables - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the Zeeman specific Forward variables
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%w1, &
      PAFV%w2, &
      PAFV%inode
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Zeeman specific forward variables - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the compact-OPTRAN Forward variables if necessary
    IF ( PAFV%OPTRAN ) THEN
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        PAFV%dPonG     , &
        PAFV%d_Absorber, &
        PAFV%Int_vapor , &
        PAFV%AveA      , &
        PAFV%Inverse   , &
        PAFV%s_t       , &
        PAFV%s_p       , &
        PAFV%Ap1       , &
        PAFV%b         , &
        PAFV%LN_Chi    , &
        PAFV%Chi
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading compact-OPTRAN variables - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL PAFV_Info( PAFV, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS

     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file '//TRIM(Filename)//&
                 ' during error cleanup - '//TRIM(io_msg)
       END IF
       CALL PAFV_Destroy( PAFV )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION PAFV_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       PAFV_WriteFile
!
! PURPOSE:
!       Function to write PAFV object files.
!
! CALLING SEQUENCE:
!       Error_Status = PAFV_WriteFile( &
!                        PAFV    , &
!                        Filename, &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       PAFV:
!         PAFV object containing the data to write to file.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:
!         Character string specifying the name of a
!         PAFV format data file to write.
!         UNITS:      N/A
!         TYPE:       CHARACTER(*)
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:
!         Set this logical argument to *NOT* close the datafile
!         upon exiting this routine. This option is required if
!         the PAFV data is embedded within another file.
!         If == .FALSE., File is closed upon function exit [DEFAULT].
!            == .TRUE.,  File is NOT closed upon function exit
!         If not specified, default is .FALSE.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:
!         Set this logical argument to suppress INFORMATION
!         messages being printed to stdout
!         If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!            == .TRUE.,  INFORMATION messages are SUPPRESSED.
!         If not specified, default is .FALSE.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:
!         The return value is an integer defining the error status.
!         The error codes are defined in the Message_Handler module.
!         If == SUCCESS, the file write was successful
!            == FAILURE, an unrecoverable error occurred.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION PAFV_WriteFile( &
    PAFV    , &  ! Input
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(PAFV_type),  INTENT(IN) :: PAFV
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PAFV_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: optran_present


    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. PAFV_Associated( PAFV ) ) THEN
      msg = 'PAFV object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open.
    IF ( File_Open( FileName ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file for output
      err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_CleanUp(); RETURN
      END IF
    END IF


    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%Release, &
      PAFV%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%n_ODPS_Layers, &
      PAFV%n_Absorbers  , &
      PAFV%n_User_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimension values - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write Compact-OPTRAN data indicator
    IF ( PAFV%OPTRAN) THEN
      optran_present = DATA_PRESENT
    ELSE
      optran_present = DATA_MISSING
    END IF
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) optran_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Compact-OPTRAN data indicator - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the ODPS forward variables
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%ODPS2User_Idx, &
      PAFV%interp_index , &
      PAFV%Acc_Weighting, &
      PAFV%Temperature  , &
      PAFV%Absorber     , &
      PAFV%idx_map      , &
      PAFV%H2O_idx
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing ODPS forward variables - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the pressure profiles for interpolation
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%Ref_LnPressure , &
      PAFV%User_LnPressure
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing pressure profiles - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the predictor forward variables
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%PDP      , &
      PAFV%Tz_ref   , &
      PAFV%Tz       , &
      PAFV%Tzp_ref  , &
      PAFV%Tzp      , &
      PAFV%GAz_ref  , &
      PAFV%GAz_sum  , &
      PAFV%GAz      , &
      PAFV%GAzp_ref , &
      PAFV%GAzp_sum , &
      PAFV%GAzp     , &
      PAFV%GATzp_ref, &
      PAFV%GATzp_sum, &
      PAFV%GATzp
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing predictor forward variables (set1) - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%DT         , &
      PAFV%T          , &
      PAFV%T2         , &
      PAFV%DT2        , &
      PAFV%H2O        , &
      PAFV%H2O_A      , &
      PAFV%H2O_R      , &
      PAFV%H2O_S      , &
      PAFV%H2O_R4     , &
      PAFV%H2OdH2OTzp , &
      PAFV%CO2        , &
      PAFV%O3         , &
      PAFV%O3_A       , &
      PAFV%O3_R       , &
      PAFV%CO         , &
      PAFV%CO_A       , &
      PAFV%CO_R       , &
      PAFV%CO_S       , &
      PAFV%CO_ACOdCOzp, &
      PAFV%N2O        , &
      PAFV%N2O_A      , &
      PAFV%N2O_R      , &
      PAFV%N2O_S      , &
      PAFV%CH4        , &
      PAFV%CH4_A      , &
      PAFV%CH4_R      , &
      PAFV%CH4_ACH4zp
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing predictor forward variables (set2) - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the optical depth variables
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%OD, &
      PAFV%OD_path
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing optical depth variables - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Zeeman specific Forward variables
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      PAFV%w1, &
      PAFV%w2, &
      PAFV%inode
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Zeeman specific forward variables - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the compact-OPTRAN Forward variables if necessary
    IF ( PAFV%OPTRAN ) THEN
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        PAFV%dPonG     , &
        PAFV%d_Absorber, &
        PAFV%Int_vapor , &
        PAFV%AveA      , &
        PAFV%Inverse   , &
        PAFV%s_t       , &
        PAFV%s_p       , &
        PAFV%Ap1       , &
        PAFV%b         , &
        PAFV%LN_Chi    , &
        PAFV%Chi
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing compac-OPTRAN variables - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL PAFV_Info( PAFV, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS

     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file '//TRIM(Filename)//&
                 ' during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION PAFV_WriteFile


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       PAFV_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two PAFV objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = PAFV_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:
!         Two PAFV objects to be compared.
!         UNITS:      N/A
!         TYPE:       PAFV_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:
!         Logical value indicating whether the inputs are equal.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION PAFV_Equal( x, y ) RESULT( is_equal )
    TYPE(PAFV_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( (.NOT. PAFV_Associated(x)) .OR. &
         (.NOT. PAFV_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_ODPS_Layers /= y%n_ODPS_Layers ) .OR. &
         (x%n_Absorbers   /= y%n_Absorbers   ) .OR. &
         (x%n_User_Layers /= y%n_User_Layers ) ) RETURN
    ! ...Compact-OPTRAN data indicator
    IF ( x%OPTRAN .NEQV. y%OPTRAN ) RETURN
    ! ...Arrays
    IF ( ALL(x%ODPS2User_Idx    ==     y%ODPS2User_Idx ) .AND. &
         ALL(x%interp_index     ==     y%interp_index  ) .AND. &
         ALL(x%Acc_Weighting .EqualTo. y%Acc_Weighting ) .AND. &
         ALL(x%Temperature   .EqualTo. y%Temperature   ) .AND. &
         ALL(x%Absorber      .EqualTo. y%Absorber      ) .AND. &
         ALL(x%idx_map          ==     y%idx_map       ) ) &
      is_equal = .TRUE.
    IF ( ALL(x%Ref_LnPressure  .EqualTo. y%Ref_LnPressure  ) .AND. &
         ALL(x%User_LnPressure .EqualTo. y%User_LnPressure ) ) &
      is_equal = is_equal .EQV. .TRUE.
    IF ( ALL(x%PDP        .EqualTo. y%PDP        ) .AND. &
         ALL(x%Tz_ref     .EqualTo. y%Tz_ref     ) .AND. &
         ALL(x%Tz         .EqualTo. y%Tz         ) .AND. &
         ALL(x%Tzp_ref    .EqualTo. y%Tzp_ref    ) .AND. &
         ALL(x%Tzp        .EqualTo. y%Tzp        ) .AND. &
         ALL(x%GAz_ref    .EqualTo. y%GAz_ref    ) .AND. &
         ALL(x%GAz_sum    .EqualTo. y%GAz_sum    ) .AND. &
         ALL(x%GAz        .EqualTo. y%GAz        ) .AND. &
         ALL(x%GAzp_ref   .EqualTo. y%GAzp_ref   ) .AND. &
         ALL(x%GAzp_sum   .EqualTo. y%GAzp_sum   ) .AND. &
         ALL(x%GAzp       .EqualTo. y%GAzp       ) .AND. &
         ALL(x%GATzp_ref  .EqualTo. y%GATzp_ref  ) .AND. &
         ALL(x%GATzp_sum  .EqualTo. y%GATzp_sum  ) .AND. &
         ALL(x%GATzp      .EqualTo. y%GATzp      ) ) &
      is_equal = is_equal .EQV. .TRUE.
    IF ( ALL(x%DT          .EqualTo. y%DT          ) .AND. &
         ALL(x%T           .EqualTo. y%T           ) .AND. &
         ALL(x%T2          .EqualTo. y%T2          ) .AND. &
         ALL(x%DT2         .EqualTo. y%DT2         ) .AND. &
         ALL(x%H2O         .EqualTo. y%H2O         ) .AND. &
         ALL(x%H2O_A       .EqualTo. y%H2O_A       ) .AND. &
         ALL(x%H2O_R       .EqualTo. y%H2O_R       ) .AND. &
         ALL(x%H2O_S       .EqualTo. y%H2O_S       ) .AND. &
         ALL(x%H2O_R4      .EqualTo. y%H2O_R4      ) .AND. &
         ALL(x%H2OdH2OTzp  .EqualTo. y%H2OdH2OTzp  ) .AND. &
         ALL(x%CO2         .EqualTo. y%CO2         ) .AND. &
         ALL(x%O3          .EqualTo. y%O3          ) .AND. &
         ALL(x%O3_A        .EqualTo. y%O3_A        ) .AND. &
         ALL(x%O3_R        .EqualTo. y%O3_R        ) .AND. &
         ALL(x%CO          .EqualTo. y%CO          ) .AND. &
         ALL(x%CO_A        .EqualTo. y%CO_A        ) .AND. &
         ALL(x%CO_R        .EqualTo. y%CO_R        ) .AND. &
         ALL(x%CO_S        .EqualTo. y%CO_S        ) .AND. &
         ALL(x%CO_ACOdCOzp .EqualTo. y%CO_ACOdCOzp ) .AND. &
         ALL(x%N2O         .EqualTo. y%N2O         ) .AND. &
         ALL(x%N2O_A       .EqualTo. y%N2O_A       ) .AND. &
         ALL(x%N2O_R       .EqualTo. y%N2O_R       ) .AND. &
         ALL(x%N2O_S       .EqualTo. y%N2O_S       ) .AND. &
         ALL(x%CH4         .EqualTo. y%CH4         ) .AND. &
         ALL(x%CH4_A       .EqualTo. y%CH4_A       ) .AND. &
         ALL(x%CH4_R       .EqualTo. y%CH4_R       ) .AND. &
         ALL(x%CH4_ACH4zp  .EqualTo. y%CH4_ACH4zp  ) ) &
      is_equal = is_equal .EQV. .TRUE.
    ! ...Optical depth variables
    IF ( ALL(x%OD      .EqualTo. y%OD      ) .AND. &
         ALL(x%OD_path .EqualTo. y%OD_path ) ) &
      is_equal = is_equal .EQV. .TRUE.
    ! ...Zeeman variables
    IF ( (x%w1    .EqualTo. y%w1    ) .AND. &
         (x%w2    .EqualTo. y%w2    ) .AND. &
         (x%inode    ==     y%inode ) ) &
      is_equal = is_equal .EQV. .TRUE.
    ! ...Compact-OPTRAN data
    IF ( x%OPTRAN .AND. y%OPTRAN ) THEN
      IF ( ALL(x%dPonG      .EqualTo. y%dPonG      ) .AND. &
           ALL(x%d_Absorber .EqualTo. y%d_Absorber ) .AND. &
           ALL(x%Int_vapor  .EqualTo. y%Int_vapor  ) .AND. &
           ALL(x%AveA       .EqualTo. y%AveA       ) .AND. &
           ALL(x%Inverse    .EqualTo. y%Inverse    ) .AND. &
           ALL(x%s_t        .EqualTo. y%s_t        ) .AND. &
           ALL(x%s_p        .EqualTo. y%s_p        ) .AND. &
           ALL(x%Ap1        .EqualTo. y%Ap1        ) .AND. &
           ALL(x%b          .EqualTo. y%b          ) .AND. &
           ALL(x%LN_Chi     .EqualTo. y%LN_Chi     ) .AND. &
           ALL(x%Chi        .EqualTo. y%Chi        ) ) &
        is_equal = is_equal .EQV. .TRUE.
    END IF

  END FUNCTION PAFV_Equal

END MODULE PAFV_Define

!
! RTV_Define
!
! Module containing the intermediate variables for RTSolution module.
!
!
! NOTE: Modified as generic "bucket" for all RT-related algorithm,
!       ADA, AMOM, and SOI.
!       This is initial step in truly separating out the algorithms
!       into their own modules. Currently each algorithm ties into 
!       the same RTV components (not good.)
!       
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov 
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, paul.vandelst@noaa.gov
!                       08-Jun-2004

MODULE RTV_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,       ONLY: SET, ZERO, ONE, TWO, PI, &
                                   MAX_N_LAYERS, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, &
                                   DEGREES_TO_RADIANS, &
                                   SECANT_DIFFUSIVITY, &
                                   SCATTERING_ALBEDO_THRESHOLD, &
                                   OPTICAL_DEPTH_THRESHOLD, &
                                   RT_ADA
  USE SensorInfo_Parameters, ONLY: INVALID_SENSOR
  USE CRTM_SfcOptics,        ONLY: SOVar_type => iVar_type
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: ANGLE_THRESHOLD
  PUBLIC :: PHASE_THRESHOLD
  PUBLIC :: DELTA_OPTICAL_DEPTH
  PUBLIC :: MAX_ALBEDO
  PUBLIC :: SMALL_OD_FOR_SC
  PUBLIC :: MAX_N_DOUBLING
  PUBLIC :: MAX_N_SOI_ITERATIONS
  ! Datatypes
  PUBLIC :: aircraft_rt_type
  PUBLIC :: RTV_type
  ! Procedures
  PUBLIC :: RTV_Associated
  PUBLIC :: RTV_Destroy
  PUBLIC :: RTV_Create
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: RTV_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'

  ! Threshold for determing if an additional stream
  ! angle is required for the satellite zenith angle
  REAL(fp), PARAMETER :: ANGLE_THRESHOLD = 1.0e-7_fp

  ! Small positive value used to replace negative
  ! values in the computed phase function
  REAL(fp), PARAMETER :: PHASE_THRESHOLD = 1.0e-7_fp
  
  REAL(fp), PARAMETER :: DELTA_OPTICAL_DEPTH = 1.0e-8_fp
  REAL(fp), PARAMETER :: MAX_ALBEDO = 0.999999_fp
  
  ! Threshold layer optical depth for single scattering
  REAL(fp), PARAMETER :: SMALL_OD_FOR_SC = 1.E-5_fp

  ! The maximum number of doubling processes in the 
  ! the doubling-adding scheme. 
  INTEGER,  PARAMETER :: MAX_N_DOUBLING = 55
  
  ! The maximum number of iterations for the SOI solution method.
  INTEGER,  PARAMETER :: MAX_N_SOI_ITERATIONS = 75
  
  ! ---------------------
  ! Structure definitions
  ! ---------------------
  ! ...Aircraft model structure
  TYPE :: aircraft_rt_type
    ! The switch
    LOGICAL :: rt = .FALSE.
    ! The output level index
    INTEGER :: idx
  END TYPE aircraft_rt_type  

  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: RTV_type
  
    ! Type of sensor
    INTEGER :: Sensor_Type = INVALID_SENSOR
    
    ! Type of RT algorithm
    INTEGER :: RT_Algorithm_Id = RT_ADA
    
    ! Dimension information
    INTEGER :: n_Layers         = 0       ! Total number of atmospheric layers
    INTEGER :: n_Added_Layers   = 0       ! Number of layers appended to TOA
    INTEGER :: n_Angles         = 0       ! Number of angles to be considered
    INTEGER :: n_SOI_Iterations = 0       ! Number of SOI iterations
    

    REAL(fp):: COS_SUN = ZERO           ! Cosine of sun zenith angle
    REAL(fp):: Solar_irradiance = ZERO  ! channel solar iiradiance at TOA
    REAL(fp):: Cosmic_Background_Radiance = ZERO ! For background temp=2.7253 
            
    ! Variable to hold the various portions of the
    ! radiance for emissivity retrieval algorithms
    ! Passed to FWD RTSolution structure argument output
    REAL(fp) :: Up_Radiance         = ZERO
    REAL(fp) :: Down_Solar_Radiance = ZERO

    REAL(fp) :: Secant_Down_Angle = 0

    ! Overcast radiance for cloud detection
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Cloud_Radiance_UP= ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Source_UP        = ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Trans_UP   = ONE

    ! Emission model variables
    REAL(fp) :: Total_OD  = ZERO
    REAL(fp), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_UP   = ZERO
    REAL(fp), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_DOWN = ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_UP     = ZERO
    REAL(fp), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_DOWN   = ZERO

    ! Planck radiances
    REAL(fp)                               :: Planck_Surface    = ZERO
    REAL(fp), DIMENSION(  0:MAX_N_LAYERS ) :: Planck_Atmosphere = ZERO

    ! Quadrature information
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Angle  = ZERO  ! Gaussian quadrature abscissa
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Weight = ZERO  ! Gaussian quadrature weights

    ! Logical switches
    LOGICAL :: Diffuse_Surface = .TRUE.
    LOGICAL :: Is_Solar_Channel = .FALSE.
    
    ! Aircraft model RT information
    TYPE(aircraft_rt_type) :: aircraft

    ! Scattering, visible model variables    
    INTEGER :: n_Streams         = 0       ! Number of *hemispheric* stream angles used in RT    
    INTEGER :: mth_Azi                     ! mth fourier component
    INTEGER :: n_Azi                       ! number of fourier components
    LOGICAL :: Solar_Flag_true   = .FALSE.
    LOGICAL :: Visible_Flag_true = .FALSE. 
    LOGICAL :: Scattering_RT     = .FALSE.

    !-----------------------------------
    ! Variables used in the ADA routines
    !-----------------------------------
    ! Flag to indicate the following arrays have all been allocated
    LOGICAL :: Is_Allocated = .FALSE.
     
    ! Phase function variables
    ! Forward and backward scattering phase matrices
    REAL(fp), ALLOCATABLE :: Pff(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Pbb(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS

    ! Positive and negative cosine angle Legendre phase functions
    REAL(fp), ALLOCATABLE :: Pplus(:,:)  ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES
    REAL(fp), ALLOCATABLE :: Pminus(:,:) ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES
    REAL(fp), ALLOCATABLE :: Pleg(:,:)   ! 0:MAX_N_LEGENDRE_TERMS, MAX_N_ANGLES+1
    
    ! Original forward and backward scattering phase matrices.
    ! These may be slightly negative and, if so, need to be made
    ! positive and thus adjusted to ensure energy conservation
    REAL(fp), ALLOCATABLE :: Off(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Obb(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    
    ! Normalisation factor and intermediate sum used for original
    ! phase matrix energy conservation.
    REAL(fp), ALLOCATABLE :: n_Factor(:,:)  ! MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: sum_fac(:,:)   ! 0:MAX_N_ANGLES, MAX_N_LAYERS

    ! Adding-Doubling model variables
    REAL(fp), ALLOCATABLE :: Inv_Gamma(:,:,:)         ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Inv_GammaT(:,:,:)        ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Refl_Trans(:,:,:)        ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS

    REAL(fp), ALLOCATABLE :: s_Layer_Trans(:,:,:)     ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Layer_Refl(:,:,:)      ! MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS

    REAL(fp), ALLOCATABLE :: s_Level_Refl_UP(:,:,:)   ! MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Level_Rad_UP(:,:)      ! MAX_N_ANGLES, 0:MAX_N_LAYERS
     
    REAL(fp), ALLOCATABLE :: s_Layer_Source_UP(:,:)   ! MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Layer_Source_DOWN(:,:) ! MAX_N_ANGLES, MAX_N_LAYERS


    !------------------------------------
    ! Variables used in the AMOM routines
    !------------------------------------
    ! dimensions, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Thermal_C(:,:)
    REAL(fp), ALLOCATABLE :: EigVa(:,:)
    REAL(fp), ALLOCATABLE :: Exp_x(:,:)
    REAL(fp), ALLOCATABLE :: EigValue(:,:)
    
    ! dimensions, MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: HH(:,:,:)
    REAL(fp), ALLOCATABLE :: PM(:,:,:)
    REAL(fp), ALLOCATABLE :: PP(:,:,:)
    REAL(fp), ALLOCATABLE :: PPM(:,:,:)
    REAL(fp), ALLOCATABLE :: PPP(:,:,:)
    REAL(fp), ALLOCATABLE :: i_PPM(:,:,:)
    REAL(fp), ALLOCATABLE :: i_PPP(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVe(:,:,:)
    REAL(fp), ALLOCATABLE :: Gm(:,:,:)
    REAL(fp), ALLOCATABLE :: i_Gm(:,:,:)
    REAL(fp), ALLOCATABLE :: Gp(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVeF(:,:,:)
    REAL(fp), ALLOCATABLE :: EigVeVa(:,:,:)
    REAL(fp), ALLOCATABLE :: A1(:,:,:)
    REAL(fp), ALLOCATABLE :: A2(:,:,:)
    REAL(fp), ALLOCATABLE :: A3(:,:,:)
    REAL(fp), ALLOCATABLE :: A4(:,:,:)
    REAL(fp), ALLOCATABLE :: A5(:,:,:)
    REAL(fp), ALLOCATABLE :: A6(:,:,:)
    REAL(fp), ALLOCATABLE :: Gm_A5(:,:,:)
    REAL(fp), ALLOCATABLE :: i_Gm_A5(:,:,:)


    !-----------------------------------
    ! Variables used in the SOI routines
    !-----------------------------------
    INTEGER :: Number_SOI_Iter = 0
    REAL(fp), ALLOCATABLE :: e_Layer_Trans(:,:)           ! MAX_N_ANGLES, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: s_Level_IterRad_DOWN(:,:,:)  ! MAX_N_ANGLES, 0:MAX_N_LAYERS, MAX_N_SOI_ITERATIONS
    REAL(fp), ALLOCATABLE :: s_Level_IterRad_UP(:,:,:)    ! MAX_N_ANGLES, 0:MAX_N_LAYERS, MAX_N_SOI_ITERATIONS

    INTEGER , ALLOCATABLE :: Number_Doubling(:)  ! n_Layers
    REAL(fp), ALLOCATABLE :: Delta_Tau(:)        ! n_Layers
    REAL(fp), ALLOCATABLE :: Refl(:,:,:,:)       ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: Trans(:,:,:,:)      ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: Inv_BeT(:,:,:,:)    ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: C1(:,:)             ! n_Angles, n_Layers
    REAL(fp), ALLOCATABLE :: C2(:,:)             ! n_Angles, n_Layers


    ! The surface optics forward variables
    TYPE(SOVar_type) :: SOV

  END TYPE RTV_type


CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RTV_Associated
!
! PURPOSE:
!       Elemental function to test if the allocatable components of an
!       RTV object have been allocated.
!
! CALLING SEQUENCE:
!       Status = RTV_Associated( RTV )
!
! OBJECTS:
!       RTV:     RTV structure which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       RTV_type
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the RTV members.
!                .TRUE.  - if ANY of the RTV allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the RTV allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input RTV argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION RTV_Associated( RTV ) RESULT( Status )
    ! Arguments
    TYPE(RTV_type), INTENT(IN) :: RTV
    ! Function result
    LOGICAL :: Status

    ! Test the structure members
    Status = RTV%Is_Allocated

  END FUNCTION RTV_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RTV_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize RTV objects.
!
! CALLING SEQUENCE:
!       CALL RTV_Destroy( RTV )
!
! OBJECTS:
!       RTV:          Re-initialized RTV structure.
!                     UNITS:      N/A
!                     TYPE:       RTV_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RTV_Destroy( RTV )
    TYPE(RTV_type), INTENT(OUT) :: RTV

    ! Belts and braces
    RTV%Is_Allocated = .FALSE.
    
  END SUBROUTINE RTV_Destroy

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       RTV_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the RTV object.
!
! CALLING SEQUENCE:
!       CALL RTV_Create( RTV, &
!                        n_Angles        , &
!                        n_Legendre_Terms, &
!                        n_Layers          )
!
! OBJECTS:
!       RTV:               RTV structure.
!                          UNITS:      N/A
!                          TYPE:       RTV_type
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Angles:          Number of 
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Same as RTV object
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  Number of 
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Same as RTV object
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Layers:          Number of atmospheric layers.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Same as RTV object
!                          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE RTV_Create( &
    RTV, &
    n_Angles        , &
    n_Legendre_Terms, &
    n_Layers          )
    ! Arguments
    TYPE(RTV_type), INTENT(OUT) :: RTV
    INTEGER       , INTENT(IN)  :: n_Angles        
    INTEGER       , INTENT(IN)  :: n_Legendre_Terms
    INTEGER       , INTENT(IN)  :: n_Layers        
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles < 1 .OR. n_Legendre_Terms < 1 .OR. n_Layers < 1 ) RETURN
    
    ! Perform the allocation for phase function variables
    ALLOCATE( RTV%Pff(n_Angles, n_Angles+1, n_Layers) , &
              RTV%Pbb(n_Angles, n_Angles+1, n_Layers) , &
              RTV%Pplus( 0:n_Legendre_Terms, n_Angles), &
              RTV%Pminus(0:n_Legendre_Terms, n_Angles), &
              RTV%Pleg(0:n_Legendre_Terms, n_Angles+1), &
              RTV%Off(n_Angles, n_Angles+1, n_Layers) , &
              RTV%Obb(n_Angles, n_Angles+1, n_Layers) , &
              RTV%n_Factor (n_Angles, n_Layers)       , &
              RTV%sum_fac(0:n_Angles, n_Layers)       , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Perform the allocation for adding-doubling variables
    ALLOCATE( RTV%Inv_Gamma( n_Angles, n_Angles, n_Layers)       , &
              RTV%Inv_GammaT(n_Angles, n_Angles, n_Layers)       , &
              RTV%Refl_Trans(n_Angles, n_Angles, n_Layers)       , &
              RTV%s_Layer_Trans(n_Angles, n_Angles, n_Layers)    , &
              RTV%s_Layer_Refl( n_Angles, n_Angles, n_Layers)    , &
              RTV%s_Level_Refl_UP(n_Angles, n_Angles, 0:n_Layers), &
              RTV%s_Level_Rad_UP(n_Angles, 0:n_Layers)           , &
              RTV%s_Layer_Source_UP(  n_Angles, n_Layers)        , &
              RTV%s_Layer_Source_DOWN(n_Angles, n_Layers)        , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Perform the allocation for AMOM variables
    ALLOCATE( RTV%Thermal_C(n_Angles, n_Layers)        , &
              RTV%EigVa(n_Angles, n_Layers)            , &
              RTV%Exp_x(n_Angles, n_Layers)            , &
              RTV%EigValue(n_Angles, n_Layers)         , &
              RTV%HH(n_Angles, n_Angles, n_Layers)     , &
              RTV%PM(n_Angles, n_Angles, n_Layers)     , &
              RTV%PP(n_Angles, n_Angles, n_Layers)     , &
              RTV%PPM(n_Angles, n_Angles, n_Layers)    , &
              RTV%PPP(n_Angles, n_Angles, n_Layers)    , &
              RTV%i_PPM(n_Angles, n_Angles, n_Layers)  , &
              RTV%i_PPP(n_Angles,n_Angles,n_Layers)    , &
              RTV%EigVe(n_Angles, n_Angles, n_Layers)  , &
              RTV%Gm(n_Angles, n_Angles, n_Layers)     , &
              RTV%i_Gm(n_Angles, n_Angles, n_Layers)   , &
              RTV%Gp(n_Angles, n_Angles, n_Layers)     , &
              RTV%EigVeF(n_Angles, n_Angles, n_Layers) , &
              RTV%EigVeVa(n_Angles,n_Angles,n_Layers)  , &
              RTV%A1(n_Angles,n_Angles, n_Layers)      , &
              RTV%A2(n_Angles, n_Angles, n_Layers)     , &
              RTV%A3(n_Angles, n_Angles, n_Layers)     , &
              RTV%A4(n_Angles, n_Angles, n_Layers)     , &
              RTV%A5(n_Angles, n_Angles, n_Layers)     , &
              RTV%A6(n_Angles, n_Angles, n_Layers)     , &
              RTV%Gm_A5(n_Angles, n_Angles, n_Layers)  , &
              RTV%i_Gm_A5(n_Angles, n_Angles, n_Layers), & 
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Perform the allocation for SOI variables
    ALLOCATE( RTV%e_Layer_Trans( n_Angles, n_Layers), &
              RTV%s_Level_IterRad_DOWN( n_Angles, 0:n_Layers, MAX_N_SOI_ITERATIONS ), &
              RTV%s_Level_IterRad_UP( n_Angles, 0:n_Layers, MAX_N_SOI_ITERATIONS ), &  
              RTV%Number_Doubling(n_Layers), &
              RTV%Delta_Tau(n_Layers), &      
              RTV%Refl(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%Trans(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%Inv_BeT(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%C1(n_Angles, n_Layers), &
              RTV%C2(n_Angles, n_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Set dimensions
    RTV%n_Layers         = n_Layers
    RTV%n_Angles         = n_Angles
    
    RTV%n_SOI_Iterations = 0

    ! Set the allocate flag
    RTV%Is_Allocated = .TRUE.
    
  END SUBROUTINE RTV_Create
  
END MODULE RTV_Define

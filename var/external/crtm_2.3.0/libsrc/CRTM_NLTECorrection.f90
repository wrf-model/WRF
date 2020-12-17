!
! CRTM_NLTECorrection
!
! Module containing routines to correct radiances for the NLTE effect
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 05-Aug-2010
!                       yong.han@noaa.gov
!

MODULE CRTM_NLTECorrection

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds              , ONLY: fp
  USE CRTM_Parameters         , ONLY: ZERO, ONE
  USE CRTM_Atmosphere_Define  , ONLY: CRTM_Atmosphere_type   
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE NLTECoeff_Define        , ONLY: NLTECoeff_type, &
                                      NLTECoeff_Associated
  USE NLTE_Parameters         , ONLY: N_NLTE_PREDICTORS, &
                                      N_NLTE_LAYERS
  USE NLTE_Predictor_Define   , ONLY: NLTE_Predictor_type, &
                                      NLTE_Predictor_IsActive

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Inherited entities
  PUBLIC :: NLTE_Predictor_type
  PUBLIC :: NLTE_Predictor_IsActive
  ! Entities defined in this module
  PUBLIC :: Compute_NLTE_Predictor
  PUBLIC :: Compute_NLTE_Predictor_TL
  PUBLIC :: Compute_NLTE_Predictor_AD
  PUBLIC :: Compute_NLTE_Correction
  PUBLIC :: Compute_NLTE_Correction_TL
  PUBLIC :: Compute_NLTE_Correction_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_NLTECorrection.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  
  
CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Predictor
!
! PURPOSE:
!       Subroutine to compute the radiance NLTE correction predictors
!       and coefficient interpolation weights.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Predictor( NLTECoeff     , &
!                                    Atmosphere    , &
!                                    GeometryInfo  , &
!                                    NLTE_Predictor  )
!                                    
! INPUTS:
!       NLTECoeff:       Structure containing the NLTE correction coefficients.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:      Structure containing the atmospheric state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    Structure containing the view geometry data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTE_Predictor:  Structure containing predictors and interpolation
!                        weights for computing the radiance NLTE correction
!                        term.
!                        UNITS:      N/A                    
!                        TYPE:       NLTE_Predictor_type    
!                        DIMENSION:  Scalar                 
!                        ATTRIBUTES: INTENT(OUT)          
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Predictor( &
    NLTECoeff     , &  ! Input
    Atm           , &  ! Input
    gInfo         , &  ! Input
    NLTE_Predictor  )  ! Output
    ! Arguments
    TYPE(NLTECoeff_type)        , INTENT(IN)  :: NLTECoeff
    TYPE(CRTM_Atmosphere_type)  , INTENT(IN)  :: Atm
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: gInfo
    TYPE(NLTE_Predictor_type)   , INTENT(OUT) :: NLTE_Predictor
    ! Local variables
    REAL(fp) :: Secant_Sensor_Zenith, Secant_Solar_Zenith
    REAL(fp) :: x1, x2, y1, y2, xx1, xx2, yy1, yy2
    INTEGER  :: i, k1, k2, k, isen, isun
    INTEGER  :: n_Sensor_Angles, n_Solar_Angles


    ! Return if not an NLTE sensor
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) RETURN


    ! Check if the solar angle > 90 degree (nighttime)
    IF( gInfo%Secant_Source_Zenith < ZERO ) RETURN

    
    ! Determine the layer mean temperatures
    Layer_Loop: DO i = 1, N_NLTE_LAYERS
    
      ! If the user profile does not cover the pressure layer...
      IF ( NLTECoeff%Upper_PLevel(i) < Atm%Level_Pressure(0)            .OR. &
           NLTECoeff%Lower_PLevel(i) > Atm%Level_Pressure(Atm%n_Layers)      ) THEN
        ! A mean temperature value from the training set is used
        NLTE_Predictor%Tm(i) = NLTECoeff%Mean_Tm(i)
        NLTE_Predictor%Compute_Tm = .FALSE.
      ELSE
        ! Otherwise, the value is computed from the user profile.
        ! ...Find the level just above Upper_PLevel(i)
        k_Loop1: DO k = 1, Atm%n_Layers
          IF( atm%Level_Pressure(k) > NLTECoeff%Upper_PLevel(i) )THEN
            k1 = k-1
            EXIT k_Loop1
          END IF
        END DO k_Loop1
        ! ...Find the level just below Lower_PLevel(i) 
        k_Loop2: DO k = k1, Atm%n_Layers
          IF( Atm%Level_Pressure(k) > NLTECoeff%Lower_PLevel(i) )THEN
            k2 = k
            EXIT k_Loop2
          END IF
        END DO k_Loop2
        ! ...Compute mean temperature
        NLTE_Predictor%Tm(i) = ZERO
        DO k = k1+1, k2 
          NLTE_Predictor%Tm(i) = NLTE_Predictor%Tm(i) + Atm%Temperature(k)
        END DO
        NLTE_Predictor%Tm(i) = NLTE_Predictor%Tm(i) / REAL(k2-k1, fp)
        ! ...Save the level indices
        NLTE_Predictor%k1(i) = k1
        NLTE_Predictor%k2(i) = k2
        NLTE_Predictor%Compute_Tm = .TRUE.
       
        ! Check the temperature bounds and re-set if necessary
        IF ( NLTE_Predictor%Tm(i) < NLTECoeff%Min_Tm(i) ) THEN
          NLTE_Predictor%Tm(i) = NLTECoeff%Min_Tm(i)
          NLTE_Predictor%Compute_Tm = .FALSE.
        ELSE IF ( NLTE_Predictor%Tm(i) > NLTECoeff%Max_Tm(i) ) THEN
          NLTE_Predictor%Tm(i) = NLTECoeff%Max_Tm(i)
          NLTE_Predictor%Compute_Tm = .FALSE.
        END IF
      END IF

    END DO Layer_Loop

    ! Define the predictors
    NLTE_Predictor%Predictor(1)  = ONE  ! constant term
    DO i = 1, N_NLTE_LAYERS
      NLTE_Predictor%Predictor(i+1) = NLTE_Predictor%Tm(i)
    END DO


    ! Check the angle bounds, re-set them if necessay
    n_Sensor_Angles = NLTECoeff%n_Sensor_Angles
    n_Solar_Angles  = NLTECoeff%n_Solar_Angles
    ! ...Sensor angle
    IF ( gInfo%Secant_Sensor_Zenith > NLTECoeff%Secant_Sensor_Zenith(n_Sensor_Angles) ) THEN
      Secant_Sensor_Zenith = NLTECoeff%Secant_Sensor_Zenith(n_Sensor_Angles)
    ELSE IF( gInfo%Secant_Sensor_Zenith < NLTECoeff%Secant_Sensor_Zenith(1) )THEN
      Secant_Sensor_Zenith = NLTECoeff%Secant_Sensor_Zenith(1)
    ELSE
      Secant_Sensor_Zenith = gInfo%Secant_Sensor_Zenith
    END IF
    ! ...Solar angle
    IF(gInfo%Secant_Source_Zenith > NLTECoeff%Secant_Solar_Zenith(n_Solar_Angles))THEN
      Secant_Solar_Zenith = NLTECoeff%Secant_Solar_Zenith(n_Solar_Angles)
    ELSE IF( gInfo%Secant_Source_Zenith < NLTECoeff%Secant_Solar_Zenith(1) )THEN
      Secant_Solar_Zenith = NLTECoeff%Secant_Solar_Zenith(1)
    ELSE
      Secant_Solar_Zenith = gInfo%Secant_Source_Zenith
    END IF


    ! Find the indices used for the bilinear interpolation of coefficients
    ! ...Sensor angle slice of coefficient array
    Sensor_Angle_Loop: DO i = 1, n_Sensor_Angles-1
      IF ( Secant_Sensor_Zenith >= NLTECoeff%Secant_Sensor_Zenith(i)   .AND. &
           Secant_Sensor_Zenith <= NLTECoeff%Secant_Sensor_Zenith(i+1)       ) THEN
        isen = i
        EXIT Sensor_Angle_Loop
      END IF
    END DO Sensor_Angle_Loop
    ! ...Solar angle slice of coefficient array
    Solar_Angle_Loop: DO i = 1, n_Solar_Angles-1
      IF ( Secant_Solar_Zenith >= NLTECoeff%Secant_Solar_Zenith(i)   .AND. &
           Secant_Solar_Zenith <= NLTECoeff%Secant_Solar_Zenith(i+1)       ) THEN
        isun = i
        EXIT Solar_Angle_Loop
      END IF
    END DO Solar_Angle_Loop


    ! Compute weights for the bilinear interpolation of coefficients
    x1 = NLTECoeff%Secant_Sensor_Zenith(isen)
    x2 = NLTECoeff%Secant_Sensor_Zenith(isen+1)
    y1 = NLTECoeff%Secant_Solar_Zenith(isun)
    y2 = NLTECoeff%Secant_Solar_Zenith(isun+1)
    
    xx1 = (Secant_Sensor_Zenith - x2)/(x1-x2)
    xx2 = (Secant_Sensor_Zenith - x1)/(x2-x1)
    yy1 = (Secant_Solar_Zenith  - y2)/(y1-y2)
    yy2 = (Secant_Solar_Zenith  - y1)/(y2-y1)
    
    NLTE_Predictor%w(1,1) = xx1*yy1
    NLTE_Predictor%w(1,2) = xx1*yy2
    NLTE_Predictor%w(2,1) = xx2*yy1
    NLTE_Predictor%w(2,2) = xx2*yy2

    NLTE_Predictor%isen = isen
    NLTE_Predictor%isol = isun


    ! Flag the predictor structure as active
    NLTE_Predictor%Is_Active = .TRUE.

  END SUBROUTINE Compute_NLTE_Predictor


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Predictor_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear radiance NLTE correction
!       predictors.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Predictor_TL( NLTE_Predictor   , &
!                                       Atmosphere_TL    , &
!                                       NLTE_Predictor_TL  )
!                                    
! INPUTS:
!       NLTE_Predictor:    Structure containing predictors and interpolation
!                          weights for computing the radiance NLTE correction
!                          term. Returned from a previous call to the forward
!                          model subroutine Compute_NLTE_Predictor().
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar                 
!                          ATTRIBUTES: INTENT(IN)          
!
!       Atmosphere_TL:     Structure containing the tangent-linear atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       NLTE_Predictor_TL: Structure containing the tangent-linear predictors
!                          for the radiance NLTE correction term.
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Predictor_TL( &
    NLTE_Predictor   , &  ! FWD Input
    Atm_TL           , &  ! TL  Input
    NLTE_Predictor_TL  )  ! TL  Output
    ! Arguments
    TYPE(NLTE_Predictor_type),    INTENT(IN)  :: NLTE_Predictor
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)  :: Atm_TL                  
    TYPE(NLTE_Predictor_type),    INTENT(OUT) :: NLTE_Predictor_TL
    ! Local
    INTEGER :: i, k, k1, k2


    ! Return if NLTE correction is not active
    IF ( NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
      NLTE_Predictor_TL%Is_Active  = NLTE_Predictor%Is_Active
      NLTE_Predictor_TL%Compute_Tm = NLTE_Predictor%Compute_Tm
      NLTE_Predictor_TL%k1         = NLTE_Predictor%k1
      NLTE_Predictor_TL%k2         = NLTE_Predictor%k2
      NLTE_Predictor_TL%isen       = NLTE_Predictor%isen
      NLTE_Predictor_TL%isol       = NLTE_Predictor%isol
      NLTE_Predictor_TL%w          = NLTE_Predictor%w
    ELSE
      RETURN
    END IF


    ! Determine the tangent-linear layer mean temperatures
    Layer_Loop: DO i = 1, N_NLTE_LAYERS
    
      ! If the forward model value was computed from the user profile...
      IF ( NLTE_Predictor%Compute_Tm ) THEN
        ! Compute the tangent-linear value
        k1 = NLTE_Predictor%k1(i)
        k2 = NLTE_Predictor%k2(i)
        NLTE_Predictor_TL%Tm(i) = ZERO    
        DO k = k1+1, k2 
          NLTE_Predictor_TL%Tm(i) = NLTE_Predictor_TL%Tm(i) + Atm_TL%Temperature(k)
        END DO
        NLTE_Predictor_TL%Tm(i) = NLTE_Predictor_TL%Tm(i) / REAL(k2-k1, fp)
      ELSE
        ! Otherwise, the perturbation is always zero
        NLTE_Predictor_TL%Tm(i) = ZERO
      END IF     
    END DO Layer_Loop
   
   
    ! Define the tangent-linear predictors
    NLTE_Predictor_TL%Predictor(1)  = ZERO  ! constant term
    DO i = 1, N_NLTE_LAYERS
      NLTE_Predictor_TL%Predictor(i+1) = NLTE_Predictor_TL%Tm(i)
    END DO

  END SUBROUTINE Compute_NLTE_Predictor_TL


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Predictor_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint radiance NLTE correction
!       predictors.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Predictor_AD( NLTE_Predictor   , &
!                                       NLTE_Predictor_AD, &
!                                       Atmosphere_AD      )
!                                    
! INPUTS:
!       NLTE_Predictor:    Structure containing predictors and interpolation
!                          weights for computing the radiance NLTE correction
!                          term. Returned from a previous call to the forward
!                          model subroutine Compute_NLTE_Predictor().
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar                 
!                          ATTRIBUTES: INTENT(OUT)          
!
!       NLTE_Predictor_AD: Structure containing the adjoitn predictors
!                          for the radiance NLTE correction term.
!                          *** SET TO ZERO UPON EXIT ***
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
! OUTPUTS:
!       Atmosphere_AD:     Structure containing the adjoint atmospheric
!                          state data.
!                          *** MUST CONTAIN VALID DATA UPON ENTRY ***
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Predictor_AD( &
    NLTE_Predictor   , &  ! FWD Input 
    NLTE_Predictor_AD, &  ! AD  Input
    Atm_AD             )  ! AD  Output
    ! Arguments
    TYPE(NLTE_Predictor_type),    INTENT(IN)     :: NLTE_Predictor
    TYPE(NLTE_Predictor_type),    INTENT(IN OUT) :: NLTE_Predictor_AD
    TYPE(CRTM_Atmosphere_type),   INTENT(IN OUT) :: Atm_AD                  
    ! Local
    INTEGER  :: i, k1, k2, k


    ! Return if NLTE correction is not active
    IF ( NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
      NLTE_Predictor_AD%Is_Active  = NLTE_Predictor%Is_Active
      NLTE_Predictor_AD%Compute_Tm = NLTE_Predictor%Compute_Tm
      NLTE_Predictor_AD%k1         = NLTE_Predictor%k1
      NLTE_Predictor_AD%k2         = NLTE_Predictor%k2
      NLTE_Predictor_AD%isen       = NLTE_Predictor%isen
      NLTE_Predictor_AD%isol       = NLTE_Predictor%isol
      NLTE_Predictor_AD%w          = NLTE_Predictor%w
    ELSE
      RETURN
    END IF


    ! Define the predictor adjoints
    DO i = 1, N_NLTE_LAYERS
      NLTE_Predictor_AD%Tm(i) = NLTE_Predictor_AD%Tm(i) + NLTE_Predictor_AD%Predictor(i+1)
    END DO
    NLTE_Predictor_AD%Predictor = ZERO

                
    ! Determine the layer mean temperature adjoint
    Layer_Loop: DO i = N_NLTE_LAYERS, 1, -1

      ! If the forward model value was computed from the user profile...
      IF ( NLTE_Predictor%Compute_Tm ) THEN
        ! Compute the tangent-linear value
        k1 = NLTE_Predictor%k1(i)
        k2 = NLTE_Predictor%k2(i)
        NLTE_Predictor_AD%Tm(i) = NLTE_Predictor_AD%Tm(i) / REAL(k2-k1, fp)
        DO k = k2, k1+1, -1 
          Atm_AD%Temperature(k) = Atm_AD%Temperature(k) + NLTE_Predictor_AD%Tm(i)
        END DO
      END IF
      NLTE_Predictor_AD%Tm(i) = ZERO    
                
    END DO Layer_Loop
    
  END SUBROUTINE Compute_NLTE_Predictor_AD


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Correction
!
! PURPOSE:
!       Subroutine to compute the radiance NLTE correction term.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Correction( NLTECoeff     , &
!                                     ChannelIndex  , &
!                                     NLTE_Predictor, &
!                                     Radiance        )
!
! INPUTS:
!       NLTECoeff:       Structure containing the NLTE correction coefficients.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       NLTE_Predictor:  Structure containing predictors and interpolation
!                        weights for computing the radiance NLTE correction
!                        term.
!                        UNITS:      N/A                    
!                        TYPE:       NLTE_Predictor_type    
!                        DIMENSION:  Scalar                 
!                        ATTRIBUTES: INTENT(IN)          
!
!       Radiance:        The computed LTE radiance.
!                        UNITS:      mW/(m^2.sr.cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Radiance:        The non-LTE corrected radiance.
!                        UNITS:      mW/(m^2.sr.cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Correction( &
    NLTECoeff     , &  ! Input
    ChannelIndex  , &  ! Input
    NLTE_Predictor, &  ! Input
    Radiance        )  ! In/Output
    ! Arguments
    TYPE(NLTECoeff_type)     , INTENT(IN)     :: NLTECoeff
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(NLTE_Predictor_type), INTENT(IN)     :: NLTE_Predictor
    REAL(fp)                 , INTENT(IN OUT) :: Radiance
    ! Local variables
    INTEGER  :: i, isen, isun, ich
    REAL(fp) :: coeff, delta_radiance


    ! Check NLTE status
    ! ...Gross check
    IF ( .NOT. NLTE_Predictor_IsActive(NLTE_Predictor) ) RETURN
    ! ...Check sensor
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) RETURN
    ! ...and then channel
    IF ( .NOT. NLTECoeff%Is_NLTE_Channel(ChannelIndex) ) RETURN
    

    ! Extract the coefficient indices
    isen = NLTE_Predictor%isen
    isun = NLTE_Predictor%isol
    ich  = NLTECoeff%C_Index(ChannelIndex)


    ! Compute the non-LTE radiance correction
    delta_radiance = ZERO
    DO i = 1, NLTE_Predictor%n_Predictors                        
      coeff = NLTE_Predictor%w(1,1)*NLTECoeff%C(i, isen  , isun  , ich) + &
              NLTE_Predictor%w(1,2)*NLTECoeff%C(i, isen  , isun+1, ich) + & 
              NLTE_Predictor%w(2,1)*NLTECoeff%C(i, isen+1, isun  , ich) + &
              NLTE_Predictor%w(2,2)*NLTECoeff%C(i, isen+1, isun+1, ich)   
      delta_radiance = delta_radiance + coeff*NLTE_Predictor%Predictor(i)
    END DO


    ! Apply the correction to the input radiance
    Radiance = Radiance + delta_radiance

  END SUBROUTINE Compute_NLTE_Correction


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Correction_TL
!
! PURPOSE:
!       Function to compute the tangent-linear radiance NLTE correction term.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Correction_TL( NLTECoeff        , &
!                                        ChannelIndex     , &
!                                        NLTE_Predictor_TL, &
!                                        Radiance_TL        )
!
! INPUTS:
!       NLTECoeff:         Structure containing the NLTE correction coefficients.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:      Channel index id. This is a unique index associated
!                          with a (supported) sensor channel used to access the
!                          shared coefficient data for a particular sensor's
!                          channel.
!                          See the SensorIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       NLTE_Predictor_TL: Structure containing the tangent-linear predictors
!                          and interpolation weights for computing the radiance
!                          NLTE correction term.
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Radiance_TL:       The computed LTE tangent-linear radiance.
!                          UNITS:      mW/(m^2.sr.cm^-1)
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Radiance_TL:       The non-LTE corrected tangent-linear radiance.
!                          UNITS:      mW/(m^2.sr.cm^-1)
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Correction_TL( &
    NLTECoeff        , &  ! Input
    ChannelIndex     , &  ! Input
    NLTE_Predictor_TL, &  ! TL Input
    Radiance_TL        )  ! TL In/Output
    ! Arguments
    TYPE(NLTECoeff_type)     , INTENT(IN)     :: NLTECoeff
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(NLTE_Predictor_type), INTENT(IN)     :: NLTE_Predictor_TL
    REAL(fp)                 , INTENT(IN OUT) :: Radiance_TL
    ! Local variables
    INTEGER  :: i, isen, isun, ich
    REAL(fp) :: coeff, delta_radiance_TL
    
      
    ! Check NLTE status
    ! ...Gross check
    IF ( .NOT. NLTE_Predictor_IsActive(NLTE_Predictor_TL) ) RETURN
    ! ...Check sensor
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) RETURN
    ! ...and then channel
    IF ( .NOT. NLTECoeff%Is_NLTE_Channel(ChannelIndex) ) RETURN
    

    ! Extract the coefficient indices
    isen = NLTE_Predictor_TL%isen
    isun = NLTE_Predictor_TL%isol
    ich  = NLTECoeff%C_Index(ChannelIndex)


    ! Compute the tangent-liner non-LTE radiance correction
    delta_radiance_TL = ZERO
    DO i = 1, NLTE_Predictor_TL%n_Predictors                        
      coeff = NLTE_Predictor_TL%w(1,1)*NLTECoeff%C(i, isen,   isun,   ich) + &
              NLTE_Predictor_TL%w(1,2)*NLTECoeff%C(i, isen,   isun+1, ich) + &
              NLTE_Predictor_TL%w(2,1)*NLTECoeff%C(i, isen+1, isun,   ich) + &
              NLTE_Predictor_TL%w(2,2)*NLTECoeff%C(i, isen+1, isun+1, ich)
      delta_radiance_TL = delta_radiance_TL + coeff*NLTE_Predictor_TL%Predictor(i)
    END DO
    Radiance_TL = Radiance_TL + delta_radiance_TL

  END SUBROUTINE Compute_NLTE_Correction_TL


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_NLTE_Correction_AD
!
! PURPOSE:
!       Function to compute the adjoint radiance NLTE correction term.
!
! CALLING SEQUENCE:  
!       CALL Compute_NLTE_Correction_AD( NLTECoeff        , &
!                                        ChannelIndex     , &
!                                        Radiance_AD      , &
!                                        NLTE_Predictor_AD  )
!
! INPUTS:
!       NLTECoeff:         Structure containing the NLTE correction coefficients.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:      Channel index id. This is a unique index associated
!                          with a (supported) sensor channel used to access the
!                          shared coefficient data for a particular sensor's
!                          channel.
!                          See the SensorIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Radiance_AD:       The adjoint radiance.
!                          UNITS:      mW/(m^2.sr.cm^-1)
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       NLTE_Predictor_AD: Structure containing the interpolation weights for
!                          computing the radiance NLTE correction term.
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       NLTE_Predictor_AD: Structure containing the adjoint of the predictors
!                          for the radiance NLTE correction term.
!                          UNITS:      N/A                    
!                          TYPE:       NLTE_Predictor_type    
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_NLTE_Correction_AD( &
    NLTECoeff        , &  ! Input
    ChannelIndex     , &  ! Input
    Radiance_AD      , &  ! AD  Input
    NLTE_Predictor_AD  )  ! AD  Output
    ! Arguments
    TYPE(NLTECoeff_type)     , INTENT(IN)     :: NLTECoeff
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    REAL(fp)                 , INTENT(IN OUT) :: Radiance_AD
    TYPE(NLTE_Predictor_type), INTENT(IN OUT) :: NLTE_Predictor_AD
    
    ! Local
    INTEGER  :: i, isen, isun, ich
    REAL(fp) :: coeff, delta_radiance_AD
      
    ! Check NLTE status
    ! ...Gross check
    IF ( .NOT. NLTE_Predictor_IsActive(NLTE_Predictor_AD) ) RETURN
    ! ...Check sensor
    IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) RETURN
    ! ...and then channel
    IF ( .NOT. NLTECoeff%Is_NLTE_Channel(ChannelIndex) ) RETURN
    

    ! Extract the coefficient indices
    isen = NLTE_Predictor_AD%isen
    isun = NLTE_Predictor_AD%isol
    ich  = NLTECoeff%C_Index(ChannelIndex)


    ! Adjoint of bilinear interpolation of coefficients
    delta_radiance_AD = Radiance_AD
    DO i = NLTE_Predictor_AD%n_Predictors, 1, -1                        
      coeff  = NLTE_Predictor_AD%w(1,1)*NLTECoeff%C(i, isen,   isun,   ich) + &
               NLTE_Predictor_AD%w(1,2)*NLTECoeff%C(i, isen,   isun+1, ich) + &
               NLTE_Predictor_AD%w(2,1)*NLTECoeff%C(i, isen+1, isun,   ich) + &
               NLTE_Predictor_AD%w(2,2)*NLTECoeff%C(i, isen+1, isun+1, ich) 
      NLTE_Predictor_AD%Predictor(i) = NLTE_Predictor_AD%Predictor(i) + coeff*delta_radiance_AD
    END DO
    
  END SUBROUTINE Compute_NLTE_Correction_AD
  
END MODULE CRTM_NLTECorrection            

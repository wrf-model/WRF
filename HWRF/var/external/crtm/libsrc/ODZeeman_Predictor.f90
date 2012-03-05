!
! ODZeeman_Predictor
!
! Module containing routines to compute the predictors for calculations
! gaseous absorption affected by Zeeman spilitting in 
! the Optical Depth Pressure Space (ODPS).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, JCSDA, NOAA/NESDIS 20-Sep-2008
!

MODULE ODZeeman_Predictor

  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE ODPS_Predictor_Define, ONLY: Predictor_type
  USE ODPS_Define,           ONLY: ODPS_type

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public routines
  PUBLIC :: Compute_Predictors_zssmis
  PUBLIC :: Compute_Predictors_zssmis_TL
  PUBLIC :: Compute_Predictors_zssmis_AD
  PUBLIC :: Compute_Predictors_zamsua
  PUBLIC :: Compute_Predictors_zamsua_TL
  PUBLIC :: Compute_Predictors_zamsua_AD
  
  ! Public parameters
  PUBLIC :: N_ZCOMPONENTS
  PUBLIC :: N_ZABSORBERS
  PUBLIC :: MAX_N_PREDICTORS_ZSSMIS
  PUBLIC :: MAX_N_PREDICTORS_ZAMSUA
  PUBLIC :: ZSSMIS_ChannelMap
  PUBLIC :: ZAMSUA_ChannelMap
  PUBLIC :: ODPS_gINDEX_ZSSMIS
  PUBLIC :: ODPS_gINDEX_ZAMSUA
  
  ! ----------
  ! Parameters
  ! ----------
  REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp, TWO = 2.0_fp

  INTEGER, PARAMETER :: N_ZCOMPONENTS = 1
  ! set this variable to 1 although it should be 1 to consistant with the ODPS structure (for
  ! Zeeman applications only O2 need to be considered, which will be include as a fixed gas, decribed
  ! by pressure and temperature. 
  INTEGER, PARAMETER :: N_ZABSORBERS  = 1

  !----------------------------------------------------------------
  !  ZSSMIS parameters (SSMIS channels 19 - 22)
  !----------------------------------------------------------------
  INTEGER, PARAMETER :: ODPS_gINDEX_ZSSMIS = 4  ! ODPS group index    
  INTEGER, PARAMETER :: MAX_N_PREDICTORS_ZSSMIS = 18
  ! Four SSMIS upper air sounding channels affected by Zeeman splitting
  INTEGER, PARAMETER :: N_CHANNELS_ZSSMIS = 4
  INTEGER, PARAMETER :: CHANNEL_INDEX_SSMIS(N_CHANNELS_ZSSMIS) = &
                         (/19, 20, 21, 22/)  
  ! Global to ZSSMIS channel index mapping
  INTEGER, PARAMETER :: N_CHANNELS_SSMIS = 24
  INTEGER, PARAMETER :: ZSSMIS_ChannelMap(N_CHANNELS_SSMIS) = (/&
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                                      0, 0, 0, 0, 0, 0, 0, 0, 1, 2, &
                                      3, 4, 0, 0 /)  
  ! Predictor Mask: 1 - included; 0 - not included
  INTEGER, PARAMETER :: PREDICTOR_MASK(MAX_N_PREDICTORS_ZSSMIS, N_CHANNELS_ZSSMIS) = &
    RESHAPE((/ & 
             ! 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
               1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, &
               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, &
               1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, &
               1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 /), &
            (/MAX_N_PREDICTORS_ZSSMIS, N_CHANNELS_ZSSMIS/))

  !----------------------------------------------------------------
  !  ZAMSUA parameters (AMSUA channel 14)
  !----------------------------------------------------------------
  INTEGER, PARAMETER :: MAX_N_PREDICTORS_ZAMSUA = 7
  INTEGER, PARAMETER :: ODPS_gINDEX_ZAMSUA = 5  ! ODPS group index
  ! Global to ZSSMIS channel index mapping
  INTEGER, PARAMETER :: N_CHANNELS_AMSUA = 15
  INTEGER, PARAMETER :: ZAMSUA_ChannelMap(N_CHANNELS_AMSUA) = (/&
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                                      0, 0, 0, 1, 0/)
   
CONTAINS

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis
!
! PURPOSE:
!       Subroutine to compute predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22
!
! CALLING SEQUENCE:
!       CALL  Compute_Predictors_zssmis(Temperature,   &
!                                       Be,            &
!                                       CosBK,         &
!                                       Doppler_Shift, &
!                                       Secang,        &
!                                       Predictor )
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
      
  SUBROUTINE Compute_Predictors_zssmis(Temperature,   &
                                       Be,            &
                                       CosBK,         &
                                       Doppler_Shift, &
                                       Secang,        &
                                       Predictor )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor
    
    ! LOCAL 
    REAL(fp) :: Be2, Be3, OneOverBe, OneOverBe2, CosBK2, OneOverT
    INTEGER  :: k
   
    Be2         = Be  * Be
    Be3         = Be2 * Be
    OneOverBe   = ONE / Be
    OneOverBe2  = OneOverBe * OneOverBe 
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)                                    
      Predictor%X(k, 1,  1)  = OneOverT                ! 1/T                   
      Predictor%X(k, 2,  1)  = OneOverT * CosBK2       ! COS(BK_ang)**2 / T    
      Predictor%X(k, 3,  1)  = OneOverT / Be           ! 1 / (T*Be)            
      Predictor%X(k, 4,  1)  = OneOverT * CosBK / Be   ! COS(BK_ang) / (T*Be)  
      Predictor%X(k, 5,  1)  = OneOverT * CosBK / Be2  ! COS(BK_ang) / (T*Be**2)  
    END DO
    
    ! Constant predictor                                                        
    Predictor%X(:, 6,  1)  = ONE                       ! 1  - constant term
    Predictor%X(:, 7,  1)  = CosBK                     ! COS(BK_ang)              
    Predictor%X(:, 8,  1)  = CosBK2                    ! COS(BK_ang)**2           
    Predictor%X(:, 9,  1)  = OneOverBe                 ! 1 / Be                   
    Predictor%X(:, 10, 1)  = OneOverBe2                ! 1 / Be**2                
    Predictor%X(:, 11, 1)  = OneOverBe2 * OneOverBe    ! 1 / Be^3                 
    Predictor%X(:, 12, 1)  = CosBK * Be2               ! COS(BK_ang) * Be**2      
    Predictor%X(:, 13, 1)  = CosBK2 * Be2              ! (COS(BK_ang) * Be)**2    
    Predictor%X(:, 14, 1)  = CosBK * Be3               ! COS(BK_ang) * Be**3 
    Predictor%X(:, 15, 1)  = Be                        ! Be
    Predictor%X(:, 16, 1)  = Be3                       ! Be**3
    Predictor%X(:, 17, 1)  = CosBK  * Be               ! COS(BK_ang)*Be
    Predictor%X(:, 18, 1)  = CosBK2 * Be               ! COS(BK_ang)**2*Be
    
    Predictor%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 

    ! pass Doppler shift and Secant angle to Predicotr structre
    Predictor%u = Doppler_Shift   
    Predictor%Secant_Zenith = Secang                      
   
  END SUBROUTINE Compute_Predictors_zssmis    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis_TL
!
! PURPOSE:
!       Subroutine to compute TL predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22)
!
! CALLING SEQUENCE:
!
!       CALL Compute_Predictors_zssmis_TL(Temperature,   &
!                                         Be,            & 
!                                         CosBK,         & 
!                                         Doppler_Shift, &  
!                                         Secang,        &  
!                                         Temperature_TL,& 
!                                         Predictor_TL )   
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  TL Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   TL Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_zssmis_TL(Temperature,   &
                                          Be,            &
                                          CosBK,         &
                                          Doppler_Shift, & 
                                          Secang,        & 
                                          Temperature_TL,&
                                          Predictor_TL )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    REAL(fp), INTENT( IN ) :: Temperature_TL(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_TL
    
    ! LOCAL 
    REAL(fp) :: Be2, CosBK2, OneOverT, OneOverT_TL
    INTEGER  :: k
    
    Be2         = Be  * Be
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor_TL%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)      
      OneOverT_TL = -(OneOverT / Temperature(k))*Temperature_TL(k)                                   
      Predictor_TL%X(k, 1,  1)  = OneOverT_TL                
      Predictor_TL%X(k, 2,  1)  = OneOverT_TL * CosBK2       
      Predictor_TL%X(k, 3,  1)  = OneOverT_TL / Be           
      Predictor_TL%X(k, 4,  1)  = OneOverT_TL * CosBK / Be   
      Predictor_TL%X(k, 5,  1)  = OneOverT_TL * CosBK / Be2   
    END DO
    
    ! Constant predictor
    Predictor_TL%X(:, 6:18,  1)  = ZERO                                                        
    
    Predictor_TL%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 
    
  END SUBROUTINE Compute_Predictors_zssmis_TL    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zssmis_AD
!
! PURPOSE:
!       Subroutine to compute AD predictors for the ZSSMIS sensor (a virtual sensor
!       created for SSMIS Zeeman channels 19, 20, 21 and 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_Predictors_zssmis_AD(Temperature,   &
!                                          Be,            &
!                                          CosBK,         &
!                                          Doppler_Shift, & 
!                                          Secang,        & 
!                                          Predictor_AD,  &
!                                          Temperature_AD )
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Doppler_Shift :  Doppler shift due Earth ratation, positive when
!                        the emitter moves towards the sensor
!                        UNITS:      KHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    AD Predictor structure
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
! OUTPUT ARGUMENTS:
!       Temperature_AD:  AD Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_Predictors_zssmis_AD(Temperature,   &
                                          Be,            &
                                          CosBK,         &
                                          Doppler_Shift, & 
                                          Secang,        & 
                                          Predictor_AD,  &
                                          Temperature_AD )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Doppler_Shift
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_AD
    REAL(fp), INTENT( INOUT ) :: Temperature_AD(:)
    
    ! LOCAL 
    REAL(fp) :: Be2, CosBK2, OneOverT, OneOverT_AD
    INTEGER  :: k
    
    Be2         = Be  * Be
    CosBK2     = CosBK * CosBK 
    
    DO k = 1, Predictor_AD%n_Layers   
      ! Predictors associated Temperature  
      OneOverT = 300.0_fp / Temperature(k)
      
      OneOverT_AD = Predictor_AD%X(k, 1,  1) + &
                    Predictor_AD%X(k, 2,  1)*CosBK2 + &
                    Predictor_AD%X(k, 3,  1) / Be + &
                    Predictor_AD%X(k, 4,  1) * CosBK / Be + &
                    Predictor_AD%X(k, 5,  1) * CosBK / Be2
      Temperature_AD(k) = Temperature_AD(k) - &
                     (OneOverT / Temperature(k)) * OneOverT_AD      
    END DO
    
    ! Constant predictor
    Predictor_AD%X(:, :,  1)  = ZERO                                                        
    
    Predictor_AD%n_CP(1) = MAX_N_PREDICTORS_ZSSMIS 
    
  END SUBROUTINE Compute_Predictors_zssmis_AD    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zamsua
!
! PURPOSE:
!       Subroutine to compute predictors for the ZAMSUA sensor (a virtual sensor
!       with AMSUA channel 14)
!
! CALLING SEQUENCE:
!       CALL  Compute_Predictors_zamsua(Temperature,     &
!                                       Ref_Temperature, &
!                                       Be,            &
!                                       CosBK,         &
!                                       Secang,        &
!                                       Predictor )
!
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!   Ref_Temperature:     Reference temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!       
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
      
  SUBROUTINE Compute_Predictors_zamsua(Temperature,     &
                                       Ref_Temperature, &
                                       Be,              &
                                       CosBK,           &
                                       Secang,          &
                                       Predictor )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Ref_Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor
    
    ! LOCAL 
    REAL(fp) :: Be3,  CosBK2, CosBK2Be2, tk
    INTEGER  :: k
   
    Be3         = Be*Be*Be
    CosBK2     = CosBK*CosBK
    CosBK2Be2  = CosBK2*Be*Be 
    
    DO k = 1, Predictor%n_Layers   
      tk = Temperature(k)/Ref_Temperature(k)
      Predictor%X(k, 1,  1)  = Secang(k)                                   
      Predictor%X(k, 2,  1)  = tk 
      Predictor%X(k, 3,  1)  = tk*tk
      Predictor%X(k, 4,  1)  = CosBK2
      Predictor%X(k, 5,  1)  = Be*Secang(k)
      Predictor%X(k, 6,  1)  = Be3
      Predictor%X(k, 7,  1)  = CosBK2Be2*Secang(k)      
    END DO
    
    
    Predictor%n_CP(1) = MAX_N_PREDICTORS_ZAMSUA

    Predictor%Secant_Zenith = Secang                      
   
  END SUBROUTINE Compute_Predictors_zamsua    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zamsua_TL
!
! PURPOSE:
!       Subroutine to compute TL predictors for the ZAMSUA sensor (a virtual sensor
!       with AMSUA channel 14)
!
! CALLING SEQUENCE:
!
!       CALL Compute_Predictors_zamsua_TL(Temperature,     &
!                                         Ref_Temperature, &
!                                         Be,            & 
!                                         CosBK,         & 
!                                         Secang,        &  
!                                         Temperature_TL,& 
!                                         Predictor_TL )   
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!    RefTemperature:     Reference temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  TL Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   TL Predictor structure
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_zamsua_TL(Temperature,      &
                                          Ref_Temperature,  &
                                          Be,               &
                                          CosBK,            &
                                          Secang,           & 
                                          Temperature_TL,   &
                                          Predictor_TL )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Ref_Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Secang(:)
    REAL(fp), INTENT( IN ) :: Temperature_TL(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_TL
    
    ! LOCAL 
    REAL(fp) :: tk, tk_TL
    INTEGER  :: k
   
    
    DO k = 1, Predictor_TL%n_Layers   
      tk = Temperature(k)/Ref_Temperature(k)
      tk_TL = Temperature_TL(k)/Ref_Temperature(k)
      Predictor_TL%X(k, 1,  1)  = ZERO                                   
      Predictor_TL%X(k, 2,  1)  = tk_TL 
      Predictor_TL%X(k, 3,  1)  = TWO*tk*tk_TL
      Predictor_TL%X(k, 4,  1)  = ZERO
      Predictor_TL%X(k, 5,  1)  = ZERO
      Predictor_TL%X(k, 6,  1)  = ZERO
      Predictor_TL%X(k, 7,  1)  = ZERO      
    END DO
    
    Predictor_TL%n_CP(1) = MAX_N_PREDICTORS_ZAMSUA 

  END SUBROUTINE Compute_Predictors_zamsua_TL    

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_zamsua_AD
!
! PURPOSE:
!       Subroutine to compute TL predictors for the ZAMSUA sensor (a virtual sensor
!       with AMSUA channel 14)
!
! CALLING SEQUENCE:
!
!        CALL Compute_Predictors_zamsua_AD(Temperature,     &
!                                          Ref_Temperature, &
!                                          Be,            &
!                                          CosBK,         &
!                                          Secang,        & 
!                                          Predictor_AD,  &
!                                          Temperature_AD )
! INPUT ARGUMENTS:
!
!       Temperature:     Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!    Ref_Temperature:    Reference temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!         Be    :        Earth magnetic filed strength
!                        UNITS:      Guass (range 0.2 - 0.7)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         CosBK :        Cosine of the angle between the Earth magnetic
!                        field and the electromagnetic wave propagation
!                        direction
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!             Secang  :  Secant of the zenith angle profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    AD Predictor structure
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
! OUTPUT ARGUMENTS:
!       Temperature_AD:  AD Temperature profile
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1(n_Layers) array  
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_Predictors_zamsua_AD(Temperature,    &
                                          Ref_Temperature,&
                                          Be,            &
                                          CosBK,         &
                                          Secang,        & 
                                          Predictor_AD,  &
                                          Temperature_AD )
    REAL(fp), INTENT( IN ) :: Temperature(:)
    REAL(fp), INTENT( IN ) :: Ref_Temperature(:)
    REAL(fp), INTENT( IN ) :: Be
    REAL(fp), INTENT( IN ) :: CosBK
    REAL(fp), INTENT( IN ) :: Secang(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_AD
    REAL(fp), INTENT( INOUT ) :: Temperature_AD(:)
    
    ! LOCAL 
    REAL(fp) :: tk, tk_AD
    INTEGER  :: k
    
    DO k = Predictor_AD%n_Layers, 1, -1   
      tk = Temperature(k)/Ref_Temperature(k)
      
      tk_AD = TWO*tk*Predictor_AD%X(k, 3,  1)
      tk_AD = tk_AD + Predictor_AD%X(k, 2,  1)
      Temperature_AD(k) = Temperature_AD(k) + tk_AD/Ref_Temperature(k)
      Predictor_AD%X(k, :,  1) = ZERO
    END DO
    
    Predictor_AD%n_CP(1) = MAX_N_PREDICTORS_ZAMSUA 
    
  END SUBROUTINE Compute_Predictors_zamsua_AD    

END MODULE ODZeeman_Predictor

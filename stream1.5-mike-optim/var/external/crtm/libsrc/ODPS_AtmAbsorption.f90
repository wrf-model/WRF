!
! ODPS_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption using the Optical Depth Pressure Space 
! (ODPS) algorithm
!
!
! CREATION HISTORY:
!       Written by:     Yong Han & Yong Chen, JCSDA, NOAA/NESDIS 20-Jun-2008
!            TL,AD:     Tong Zhu, CIRA/CSU@NOAA/NESDIS 06-Jan-2009
!
 
MODULE ODPS_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, ONE, TWO, LIMIT_EXP, LIMIT_LOG
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type, H2O_ID
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type, &
                                       CRTM_GeometryInfo_GetValue
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmAbsorption_type => CRTM_AtmScatter_type
  USE ODPS_Define,               ONLY: ODPS_type,                &
                                       SIGNIFICANCE_OPTRAN
  USE ODPS_Predictor,            ONLY: Compute_Predictor,        &
                                       Compute_Predictor_TL,     &
                                       Compute_Predictor_AD,     &
                                       Destroy_Predictor,        &
                                       Allocate_Predictor,       &
                                       Destroy_PAFV,             &
                                       Allocate_PAFV,            &
                                       Predictor_type,           &
                                       ODPS_APVariables_type,    &
                                       Get_Component_ID,         &
                                       Compute_Predictor_OPTRAN, &
                                       Compute_Predictor_OPTRAN_TL, &
                                       Compute_Predictor_OPTRAN_AD, &
                                       GROUP_1                    , &
                                       GROUP_2                    , &
                                       GROUP_3                    , &
                                       MAX_OPTRAN_ORDER           ,  &
                                       MAX_OPTRAN_USED_PREDICTORS
  USE ODPS_CoordinateMapping,    ONLY: Map_Input, Map_Input_TL, Map_Input_AD, &
                                       Interpolate_Profile,    &
                                       Interpolate_Profile_F1_TL, &
                                       Interpolate_Profile_F1_AD, &
                                       Compute_Interp_Index

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! variables and routines from USE modules
  PUBLIC :: Predictor_type
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Destroy_PAFV
  PUBLIC :: Allocate_PAFV  
  PUBLIC :: ODPS_APVariables_type

  ! Science routines in this modules
  PUBLIC :: ODPS_Compute_AtmAbsorption
  PUBLIC :: ODPS_Compute_AtmAbsorption_TL
  PUBLIC :: ODPS_Compute_AtmAbsorption_AD
  PUBLIC :: ODPS_Compute_Predictors
  PUBLIC :: ODPS_Compute_Predictors_TL
  PUBLIC :: ODPS_Compute_Predictors_AD
    
  ! Internal variable structure
  PUBLIC :: ODPS_AAVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODPS_AtmAbsorption.f90 896 2008-07-1 yong.han@noaa.gov $'

  
  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: ODPS_AAVariables_type
    PRIVATE
    INTEGER :: dummy
  END TYPE ODPS_AAVariables_type

  ! Maximum allowed layer optical depth
  REAL(fp), PARAMETER :: MAX_OD     = 20.0_fp

! LOGICAL, PUBLIC, PARAMETER  :: ALLOW_OPTRAN = .FALSE.
  LOGICAL, PUBLIC, PARAMETER  :: ALLOW_OPTRAN = .TRUE.

  
CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption( TC           , &  ! Input
!                                        ChannelIndex , &  ! Input                    
!                                        Predictor    , &  ! Input                    
!                                        AtmAbsorption, &  ! Output                   
!                                        AAVariables    )  ! Internal variable output 
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption( TC           , &  ! Input
                                         ChannelIndex , &  ! Input
                                         Predictor    , &  ! Input
                                         AtmAbsorption)    ! Output
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD(Predictor%n_Layers)
    REAL(fp) :: OD_Path(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path(0:Predictor%n_User_Layers)
!    REAL(fp) :: Acc_Weighting_OD(Predictor%n_Layers, Predictor%n_User_Layers)
!    INTEGER  :: ODPS2User_Idx(2, Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    REAL(fp) :: OD_tmp
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js, ComID

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers

    !--------------------------------------------------------
    ! Compute optical path profile
    !--------------------------------------------------------
    ! Loop over each tau component for optical depth calculation                  
    OD = ZERO                                                                     
    Component_Loop: DO j = 1, Predictor%n_Components                              

      ! number of predictors for the current component and channel                 
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).               
      ! For example, if the upper m predictors have zero coefficients,             
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)          
      ! are stored and used used in the OD calculations.                           
      np = TC%n_Predictors(j, ChannelIndex)                                       

      ! Check if there is any absorption for the component&channel combination.   
      IF( np <= 0 ) CYCLE Component_Loop                                          

      ! set flag for possible OPTRAN algorithm                                    
      ! If this flag is set, this component is computed using OPTRAN algorithm.   
      ! Otherwise, it is computed using ODPS algorithm.                           
      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN                    
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN           
      ELSE                                                                        
         OPTRAN = .FALSE.                                                         
      END IF                                                                      
                                                                                  
      IF(OPTRAN)THEN  
        CALL Add_OPTRAN_wloOD(TC,          &                                      
                             ChannelIndex,  &                                     
                             Predictor,     &                                     
                             OD )                                                 
      ELSE                                                                        

        ! ODPS algorithm                                                                       
        j0 = TC%Pos_Index(j, ChannelIndex)                                        
        DO i = 1, np                                                              
          js = j0+(i-1)*n_Layers-1                                                
          DO k = 1, n_Layers                                                      
            OD(k) = OD(k) + TC%C(js+k)*Predictor%X(k, i, j)                       
          END DO                                                                  
        END DO                                                                    
                                                                                  
      END IF                                                                      

    END DO Component_Loop                                                         

    !------------------------------------------------------                       
    ! Compute optical path (level to space) profile                               
    !------------------------------------------------------                       
    OD_Path(0) = ZERO                                                             
    DO k = 1, n_layers                                                            
      OD_tmp = OD(k)                                                              
      IF(OD(k) < ZERO)THEN                                                        
        OD_tmp = ZERO                                                             
      ELSE IF(OD(k) > MAX_OD)THEN                                                 
        OD_tmp = MAX_OD                                                           
      END IF                                                                      
      OD_Path(k) = OD_Path(k-1) + OD_tmp                                          
    END DO                                                                        
      
    ! Save forward variables
    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)

    IF(Predictor%PAFV%Active)THEN  
      ! save forwad variables
      Predictor%PAFV%OD = OD
      Predictor%PAFV%OD_Path = OD_Path
      ! If interpolation indexes are known
      User_OD_Path(0) = ZERO
      CALL Interpolate_Profile(Predictor%PAFV%ODPS2User_Idx,    &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
!      DO k = 1, n_User_layers
!        User_OD_Path(k) = &
!          SUM(Predictor%PAFV%Acc_Weighting_OD(Predictor%PAFV%ODPS2User_Idx(1,k):Predictor%PAFV%ODPS2User_Idx(2,k), k)  &
!        * OD_Path(Predictor%PAFV%ODPS2User_Idx(1,k):Predictor%PAFV%ODPS2User_Idx(2,k)) )
!      END DO
    ELSE ! interpolation indexes are not known

      CALL Compute_Interp_Index(Predictor%Ref_Level_LnPressure, &
                                Predictor%User_Level_LnPressure,&
                                ODPS2User_Idx)  
      CALL Interpolate_Profile(ODPS2User_Idx,                   &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
!      CALL LayerAvg( Predictor%User_Level_LnPressure(1:n_User_Layers), & !Predictor%User_LnPressure, & 
!                     Predictor%Ref_Level_LnPressure(1:n_Layers)      , & !Predictor%Ref_LnPressure , & 
!                     Acc_Weighting_OD                                , &                                      
!                     ODPS2User_Idx)
 
!      User_OD_Path(0) = ZERO
!      DO k = 1, n_User_layers
!        User_OD_Path(k) = SUM(Acc_Weighting_OD(ODPS2User_Idx(1,k):ODPS2User_Idx(2,k), k)  &
!                             * OD_Path(ODPS2User_Idx(1,k):ODPS2User_Idx(2,k)) )
!      END DO
      
    END IF

    ! Optical depth profile scaled to zenith.  Note that the scaling
    ! factor is the surface secant zenith angle.
    AtmAbsorption%Optical_Depth = (User_OD_Path(1:n_User_Layers) - &
                                   User_OD_Path(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ODPS_Compute_AtmAbsorption

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption_TL( TC           ,   &  ! Input
!                                           ChannelIndex ,   &  ! Input                    
!                                           Predictor    ,   &  ! Input                    
!                                           Predictor_TL,    &  ! Input                    
!                                           AtmAbsorption_TL,&  ! Output
!                                           AAVariables)        ! Internal variable output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption_TL(TC         ,  &  ! Input
                                         ChannelIndex ,    &  ! Input
                                         Predictor    ,    &  ! Input
                                         Predictor_TL,     &  ! Input                    
                                         AtmAbsorption_TL)    ! Output
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(INOUT)  :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(INOUT)  :: AtmAbsorption_TL
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD_TL(Predictor%n_Layers)  
    REAL(fp) :: OD_Path_TL(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_TL(0:Predictor%n_User_Layers)
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers

    !-----------------------------
    ! Compute optical path profile
    !-----------------------------
    ! Loop over each tau component for optical depth calculation                 
    OD_TL = ZERO                                                                 
    Component_Loop: DO j = 1, Predictor%n_Components                             

      ! number of predictors for the current component and channel                 
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).               
      ! For example, if the upper m predictors have zero coefficients,             
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)          
      ! are stored and used used in the OD calculations.                           
      np = TC%n_Predictors(j, ChannelIndex)                                      

      ! Check if there is any absorption for the component&channel combination.  
      IF( np <= 0 ) CYCLE Component_Loop                                         

      ! set flag for possible OPTRAN algorithm                                   
      ! If this flag is set, this component is computed using OPTRAN algorithm.  
      ! Otherwise, it is computed using ODPS algorithm.                          
      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN                   
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN          
      ELSE                                                                       
         OPTRAN = .FALSE.                                                        
      END IF                                                                     
                                                                                 
      IF(OPTRAN)THEN                                                             
        CALL Add_OPTRAN_wloOD_TL(TC,       &                                     
                             ChannelIndex,  &                                    
                             Predictor,     &                                    
                             Predictor_TL,  &                                    
                             OD_TL)                                              
      ELSE                                                                       

        ! ODPS algorithm                                                                       
        j0 = TC%Pos_Index(j, ChannelIndex)                                       
        DO i = 1, np                                                             
          js = j0+(i-1)*n_Layers-1                                               
          DO k = 1, n_Layers                                                     
            OD_TL(k) = OD_TL(k) + TC%C(js+k)*Predictor_TL%X(k, i, j)             
          END DO                                                                 
        END DO                                                                   
                                                                                 
      END IF                                                                     
                                                                                 
    END DO Component_Loop                                                        

    !------------------------------------------------------                      
    ! Compute optical path (level to space) profile                              
    !------------------------------------------------------                      
    OD_Path_TL(0) = ZERO                                                         
    DO k = 1, n_layers                                                           
      IF(Predictor%PAFV%OD(k) < ZERO)THEN                                        
        OD_TL(k) = ZERO                                                          
      ELSE IF(Predictor%PAFV%OD(k) > MAX_OD)THEN                                 
        OD_TL(k) = ZERO                                                          
      END IF                                                                     
      OD_Path_TL(k) = OD_Path_TL(k-1) + OD_TL(k)                                 
    END DO                                                                       
      
    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    CALL Interpolate_Profile_F1_TL(Predictor%PAFV%ODPS2User_Idx,    &
                                   Predictor%PAFV%OD_Path,          &
                                   Predictor%Ref_Level_LnPressure,  &
                                   Predictor%User_Level_LnPressure, &
                                   OD_Path_TL,                      &
                                   User_OD_Path_TL)
!    User_OD_Path_TL(0) = ZERO
!    DO k = 1, n_User_layers                                                                
!      User_OD_Path_TL(k) = &
!      SUM(Predictor%PAFV%Acc_Weighting_OD(Predictor%PAFV%ODPS2User_Idx(1,k):Predictor%PAFV%ODPS2User_Idx(2,k), k)  &  
!          * OD_Path_TL(Predictor%PAFV%ODPS2User_Idx(1,k):Predictor%PAFV%ODPS2User_Idx(2,k)) )              
!    END DO                                                                                 
 
    AtmAbsorption_TL%Optical_Depth = (User_OD_Path_TL(1:n_User_Layers) - &
                                   User_OD_Path_TL(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ODPS_Compute_AtmAbsorption_TL

!------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption_AD( TC           ,    &  ! Input
!                                           ChannelIndex ,    &  ! Input                   
!                                           Predictor    ,    &  ! Input
!                                           AtmAbsorption_AD, &  ! Input
!                                           Predictor_AD    , &  ! Output
!                                           AAVariables )        ! Internal variable output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption_AD( TC           ,    &  ! Input
                                            ChannelIndex ,    &  ! Input
                                            Predictor    ,    &  ! Input
                                            AtmAbsorption_AD, &  ! Input
                                            Predictor_AD)        ! Output
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor_AD
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD_AD(Predictor%n_Layers)                 
    REAL(fp) :: OD_Path_AD(0:Predictor%n_Layers) 
    REAL(fp) :: User_OD_Path_AD(0:Predictor%n_User_Layers)
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers
 
    !------- Adjoint part ---------
    
    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    User_OD_Path_AD(n_User_Layers) = ZERO
    DO k = n_User_Layers, 1, -1
      User_OD_Path_AD(k) = User_OD_Path_AD(k) &
                           + AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
      ! combined with initilization
      User_OD_Path_AD(k-1) = -AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
    END DO
    AtmAbsorption_AD%Optical_Depth = ZERO

    OD_Path_AD = ZERO          
    CALL Interpolate_Profile_F1_AD(Predictor%PAFV%ODPS2User_Idx,       &
                                   Predictor%PAFV%OD_Path,             &
                                   Predictor%Ref_Level_LnPressure,     &
                                   Predictor%User_Level_LnPressure,    &
                                   User_OD_Path_AD,                    &
                                   OD_Path_AD )

 
    User_OD_Path_AD(0) = ZERO
 
    !-----------------------------
    ! Compute optical path profile
    !-----------------------------

    DO k = n_layers, 1, -1                                                           
      OD_Path_AD(k-1) = OD_Path_AD(k-1) + OD_Path_AD(k)                              
      ! combined with initialization                                                 
      OD_AD(k) = OD_Path_AD(k)                                                       
      OD_Path_AD(k) = ZERO                                                           
      IF(Predictor%PAFV%OD(k) < ZERO)THEN                                            
        OD_AD(k) = ZERO                                                              
      ELSE IF(Predictor%PAFV%OD(k) > MAX_OD)THEN                                     
        OD_AD(k) = ZERO                                                              
      END IF                                                                         
    END DO                                                                           
    OD_Path_AD(0) = ZERO                                                             

    ! Loop over each tau component for optical depth calculation                     

    Component_Loop_AD: DO j = 1, Predictor%n_Components                              

!      IF(j==4) cycle                                                                
      ! number of predictors for the current component and channel                   
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).                 
      ! For example, if the upper m predictors have zero coefficients,               
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)            
      ! are stored and used used in the OD calculations.                             
      np = TC%n_Predictors(j, ChannelIndex)                                          

      ! Check if there is any absorption for the component&channel combination.      
      IF( np <= 0 ) CYCLE Component_Loop_AD                                          

      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN                       
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN              
      ELSE                                                                           
         OPTRAN = .FALSE.                                                            
      END IF                                                                         

     IF(OPTRAN)THEN                                                                  
                                                                                     
       CALL Add_OPTRAN_wloOD_AD(TC,          &                                       
                                ChannelIndex, &                                      
                                Predictor,    &                                      
                                OD_AD,        &                                      
                                Predictor_AD )                                       
     ELSE                                                                            

       ! ODPS algorithm                                                                        
       j0 = TC%Pos_Index(j, ChannelIndex)                                            
       DO i = 1, np                                                                  
         js = j0+(i-1)*n_Layers-1                                                    
         DO k = n_Layers, 1, -1                                                      
           Predictor_AD%X(k, i, j) = Predictor_AD%X(k, i, j) + TC%C(js+k)*OD_AD(k)   
         END DO                                                                      
       END DO                                                                        
                                                                                     
     END IF                                                                          

    END DO Component_Loop_AD                                                         
      
  END SUBROUTINE ODPS_Compute_AtmAbsorption_AD

!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD
!
! PURPOSE:
!       Subroutine to calculate and add the layer optical depths due to water 
!       vapor line absorption.  Note the OD argument is an in/out argument, which
!       may hold optical depth from other absorbers.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD( TC,            &
!                               ChannelIndex,  &            
!                               Predictor,     &            
!                               OD )                        
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!        OD:             Slant path optical depth profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 array (n_Layers)
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD( TC,            &
                               ChannelIndex,  &            
                               Predictor,     &            
                               OD )                        
    TYPE(ODPS_type),      INTENT( IN )    :: TC
    INTEGER,              INTENT( IN )    :: ChannelIndex
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor
    REAL(fp),             INTENT( INOUT ) :: OD(:)                                           
  
    ! Local
    REAL(fp)           :: LN_Chi(TC%n_Layers), coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp)           :: b(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp)           :: Chi(TC%n_Layers)
    INTEGER            :: np, n_Layers, n_orders, js, i, j, k, ii, jj
    
      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------
      np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
      IF ( np <= 0 ) RETURN

      n_Layers = TC%n_Layers      
      js = TC%OPos_Index(ChannelIndex)
      n_orders = TC%Order(ChannelIndex)

      DO i = 0, np
        jj = js + i*(n_orders+1)
        coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
        DO k = 1, n_Layers
          b(k,i) = coeff(0)
          DO j = 1, n_orders
            b(k,i) = b(k,i) + coeff(j)*Predictor%Ap(k, j)
          END DO
        END DO

      END DO

      LN_Chi = b(:,0) 
      DO i = 1, np
        ii = TC%OP_Index(i,ChannelIndex)
        DO k = 1, n_Layers
          LN_Chi(k) = LN_Chi(k) + b(k, i)* Predictor%OX(k, ii) 
        END DO        
      END DO

      DO k = 1, n_Layers
        IF( LN_Chi(k) > LIMIT_EXP ) THEN
          Chi(k) = LIMIT_LOG
        ELSE IF( LN_Chi(k) < -LIMIT_EXP ) THEN
          Chi(k) = ZERO
        ELSE
          Chi(k) = EXP(LN_Chi(k))
        ENDIF
        OD(k) = OD(k) + Chi(k)*Predictor%dA(k)

      END DO

      IF(Predictor%PAFV%OPTRAN)THEN
        Predictor%PAFV%b      = b
        Predictor%PAFV%LN_Chi = LN_Chi 
        Predictor%PAFV%Chi    = Chi 
      END IF
            
   END SUBROUTINE Add_OPTRAN_wloOD

!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD_TL
!
! PURPOSE:
!       Subroutine to calculate and add the layer optical depths due to water 
!       vapor line absorption.  Note the OD argument is an in/out argument, which
!       may hold optical depth from other absorbers.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD_TL( TC,            &
!                                  ChannelIndex,  &            
!                                  Predictor,     &            
!                                  Predictor_TL,  &            
!                                  OD_TL )                        
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!       Predictor_TL:    Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!        OD_TL:          Slant path optical depth profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 array (n_Layers)
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD_TL( TC,            &
                               ChannelIndex,     &            
                               Predictor,        &            
                               Predictor_TL,     &
                               OD_TL)                        
    TYPE(ODPS_type),      INTENT( IN )    :: TC
    INTEGER,              INTENT( IN )    :: ChannelIndex
    TYPE(Predictor_type), INTENT( IN )    :: Predictor,       Predictor_TL
    REAL(fp),             INTENT( INOUT ) :: OD_TL(:)                   
  
    ! Local
    REAL(fp)           :: coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp)           :: LN_Chi_TL(TC%n_Layers), b_TL(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp)           :: chi_TL(TC%n_Layers)
    INTEGER            :: np, n_Layers, n_orders, js, i, j, k, ii, jj

      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------
      np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
      IF ( np <= 0 ) RETURN

      n_Layers = TC%n_Layers      
      js = TC%OPos_Index(ChannelIndex)
      n_orders = TC%Order(ChannelIndex)
   
      DO i = 0, np
        jj = js + i*(n_orders+1)
        coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
        DO k = 1, n_Layers
          b_TL(k,i) = ZERO
          DO j = 1, n_orders
            b_TL(k,i) = b_TL(k,i) + coeff(j)*Predictor_TL%Ap(k, j)
          END DO
        END DO

      END DO

      LN_Chi_TL = b_TL(:,0) 
      DO i = 1, np
        ii = TC%OP_Index(i,ChannelIndex)
        DO k = 1, n_Layers
          LN_Chi_TL(k) = LN_Chi_TL(k) + b_TL(k, i)* Predictor%OX(k, ii) + Predictor%PAFV%b(k, i)* Predictor_TL%OX(k, ii)
        END DO        
      END DO

      DO k = 1, n_Layers
        IF( Predictor%PAFV%LN_Chi(k) > LIMIT_EXP ) THEN
          Chi_TL(k) = ZERO
        ELSE IF( Predictor%PAFV%LN_Chi(k) < -LIMIT_EXP ) THEN
          Chi_TL(k) = ZERO
        ELSE
          Chi_TL(k) = Predictor%PAFV%Chi(k) * LN_Chi_TL(k)
        ENDIF
        OD_TL(k) = OD_TL(k) + Chi_TL(k)*Predictor%dA(k) + Predictor%PAFV%Chi(k)*Predictor_TL%dA(k)

      END DO
      
   END SUBROUTINE Add_OPTRAN_wloOD_TL


!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD_AD
!
! PURPOSE:
!       Subroutine to calculate and add the layer optical depths due to water 
!       vapor line absorption.  Note the OD argument is an in/out argument, which
!       may hold optical depth from other absorbers.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD_AD( TC,            &
!                                  ChannelIndex,  &            
!                                  Predictor,     &            
!                                  OD_AD,         &
!                                  Predictor_AD )                        
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!        OD_AD:          Slant path optical depth profile
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 array (n_Layers)
!                        ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!       Predictor_AD:    Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD_AD( TC,            &
                                  ChannelIndex,  &            
                                  Predictor,     &            
                                  OD_AD,         &
                                  Predictor_AD )                        
    TYPE(ODPS_type),      INTENT( IN )    :: TC
    INTEGER,              INTENT( IN )    :: ChannelIndex
    TYPE(Predictor_type), INTENT( IN )    :: Predictor
    REAL(fp),             INTENT( INOUT ) :: OD_AD(:)
    TYPE(Predictor_type), INTENT( INOUT ) :: Predictor_AD                             
  
    ! Local
    REAL(fp)           :: coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp)           :: LN_Chi_AD(TC%n_Layers), b_AD(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp)           :: Chi_AD(TC%n_Layers)
    INTEGER            :: np, n_Layers, n_orders, js, i, j, k, ii, jj

      !------ Forward part for LN_Chi, b -----------

      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------
      np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
      IF ( np <= 0 ) RETURN

      n_Layers = TC%n_Layers
      js = TC%OPos_Index(ChannelIndex)
      n_orders = TC%Order(ChannelIndex)

      !------ Adjoint part ----------------------
    
      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------

      Chi_AD = ZERO
      !LN_Chi_AD = ZERO
      DO k = n_Layers, 1, -1
      
        Chi_AD(k) = Chi_AD(k) + OD_AD(k) * Predictor%dA(k)
        Predictor_AD%dA(k) = Predictor_AD%dA(k) + OD_AD(k) * Predictor%PAFV%Chi(k)
        IF( Predictor%PAFV%LN_Chi(k) > LIMIT_EXP ) THEN
          Chi_AD(k) = ZERO
        ELSE IF( Predictor%PAFV%LN_Chi(k) < -LIMIT_EXP ) THEN
          Chi_AD(k) = ZERO
        ELSE
          LN_Chi_AD(k) = Predictor%PAFV%Chi(k) * Chi_AD(k)  ! combinded with initialization for LN_Chi_AD(k)
        ENDIF
      END DO

      DO i = 1, np
        ii = TC%OP_Index(i,ChannelIndex)
        DO k = n_Layers, 1, -1
          b_AD(k, i) = LN_Chi_AD(k) * Predictor%OX(k, ii)  ! Combinded with initialization for b_AD
          Predictor_AD%OX(k, ii) = Predictor_AD%OX(k, ii) + LN_Chi_AD(k)*Predictor%PAFV%b(k, i)
        END DO        
      END DO
      b_AD(:,0) = LN_Chi_AD
!!    LN_Chi_AD = ZERO  !! no need since it will not be used again

      DO i = 0, np
        jj = js + i*(n_orders+1)
        coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
        DO k = n_Layers, 1, -1
          DO j = 1, n_orders
            Predictor_AD%Ap(k, j) = Predictor_AD%Ap(k, j) + coeff(j)*b_AD(k,i)
          END DO
          b_AD(k,i) = ZERO
        END DO

      END DO

   END SUBROUTINE Add_OPTRAN_wloOD_AD

!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictors ( TC,           &  ! Input
!                                      Atm,          &  ! Input
!                                      GeoInfo,      &  ! Input                        
!                                      Predictor,    &  ! Output                  
!                                      APV           )  ! Internal variable output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictors(TC,           &
                                     Atm,          &    
                                     GeoInfo,      &  
                                     Predictor)
    ! Arguments
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor
    ! Local variables
    REAL(fp) :: Temperature(Predictor%n_Layers)
    REAL(fp) :: Absorber(Predictor%n_Layers, TC%n_Absorbers)
    INTEGER  :: H2O_idx
    REAL(fp) :: Secant_Sensor_Zenith
    
    
    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input(Atm,                             &  ! Input
                   TC,                              &  ! Input
                   GeoInfo,                         &  ! Input
                   Temperature,                     &  ! Output 
                   Absorber,                        &  ! output
                   Predictor%User_Level_LnPressure, &  ! Output, non variable
                   Predictor%Ref_Level_LnPressure,  &  ! Output, non variable
                   Predictor%Secant_Zenith,         &  ! Output, non variable
                   H2O_idx,                         &  ! Output, non variable
                   Predictor%PAFV)                     ! structure holding FW parameters
                   
    ! store the surface secant zenith angle
    CALL CRTM_GeometryInfo_GetValue( GeoInfo, Secant_Trans_Zenith = Secant_Sensor_Zenith )
    Predictor%Secant_Zenith_Surface = Secant_Sensor_Zenith
                                  
    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
    
    CALL Compute_Predictor( TC%Group_index,        &                          
                            Temperature,           &                         
                            Absorber,              &                         
                            TC%Ref_Level_Pressure, &                          
                            TC%Ref_Temperature,    &                          
                            TC%Ref_Absorber,       &                          
                            Predictor%Secant_Zenith,&                         
                            Predictor )                                       
    IF( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0 )THEN                             

       CALL Compute_Predictor_OPTRAN( Temperature, &                          
                                      Absorber(:, H2O_idx),  &  
                                      TC%Ref_Level_Pressure, &                
                                      TC%Ref_Pressure,       &                
                                      Predictor%Secant_Zenith,&               
                                      TC%Alpha,              &                
                                      TC%Alpha_C1,           &                
                                      TC%Alpha_C2,           &                
                                      Predictor )                             

     END IF                                                                   
     
    IF(Predictor%PAFV%Active)THEN
      ! Set and save the interpolation index array for absorption
      ! calculations. Since the indexes do not depend on channel but
      ! the absorption calculations do, put the index calculation here
      ! can improve efficency.
      CALL Compute_Interp_Index(Predictor%Ref_Level_LnPressure ,  &
                                Predictor%User_Level_LnPressure,  &
                                Predictor%PAFV%ODPS2User_Idx) 
!      CALL LayerAvg( Predictor%User_Level_LnPressure(1:n_User_Layers), & !Predictor%User_LnPressure , & 
!                     Predictor%Ref_Level_LnPressure(1:n_ODPS_Layers) , & !Predictor%Ref_LnPressure , &
!                     Predictor%PAFV%Acc_Weighting_OD, &                                      
!                     Predictor%PAFV%ODPS2User_Idx)
    END IF 
                    

  END SUBROUTINE ODPS_Compute_Predictors     

!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictors_TL ( TC,              &  ! Input
!                                         Atm,             &  ! Input
!                                         GeoInfo,         &  ! Input                     
!                                         Predictor,       &  ! Input
!                                         Atm_TL,          &  ! Input
!                                         Predictor_TL,    &  ! Output
!                                         APV)             &  ! Internal variable output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atm_TL    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictors_TL(TC,             &
                                        Atm,            &    
                                        GeoInfo,        &
                                        Predictor,      &
                                        Atm_TL,         & 
                                        Predictor_TL)
 
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm         
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm_TL
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor_TL

    ! Local variables
    REAL(fp) :: Absorber_TL(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_TL(Predictor%n_Layers)

    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input_TL(Atm,            &  ! Input
                      TC,             &  ! Input
                      Atm_TL,         &  ! Input
                      Temperature_TL, &  ! Output
                      Absorber_TL,    &  ! Output
                      Predictor%PAFV)    ! Input

    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
    CALL Compute_Predictor_TL(TC%Group_index,          &                                      
                            Predictor%PAFV%Temperature,&                                     
                            Predictor%PAFV%Absorber,   &                                     
                            TC%Ref_Level_Pressure,     &                                      
                            TC%Ref_Temperature,        &                                      
                            TC%Ref_Absorber,           &                                      
                            Predictor%Secant_Zenith,   &                                     
                            Predictor,                 &                                     
                            Temperature_TL,            &                                     
                            Absorber_TL,               &                                     
                            Predictor_TL )                                                   
    IF( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0)THEN                                             

       CALL Compute_Predictor_OPTRAN_TL( Predictor%PAFV%Temperature,       &                 
                                      Predictor%PAFV%Absorber(:, Predictor%PAFV%H2O_idx), &  
                                      TC%Ref_Level_Pressure, &                               
                                      TC%Ref_Pressure,       &                               
                                      Predictor%Secant_Zenith,&                              
                                      TC%Alpha,              &                               
                                      TC%Alpha_C1,           &                               
                                      TC%Alpha_C2,           &                               
                                      Predictor,              &                              
                                      Temperature_TL,         &                              
                                      Absorber_TL(:, Predictor%PAFV%H2O_idx),&               
                                      Predictor_TL )                                         

     END IF                                                                                  
    
  END SUBROUTINE ODPS_Compute_Predictors_TL     

!--------------------------------------------------------------------------------
!
! NAME:
!       ODPS_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_Predictors_AD ( TC,              &  ! Input
!                                         Atm,             &  ! Input
!                                         GeoInfo,         &  ! Input
!                                         Predictor,       &  ! Input                   
!                                         Predictor_AD,    &  ! Input
!                                         Atm_AD,          &  ! Output
!                                         APV )            &  ! Internal variable output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm       :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Atm_AD    :     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_Predictors_AD(TC,             &
                                        Atm,            &    
                                        GeoInfo,        &
                                        Predictor,      &
                                        Predictor_AD,   &
                                        Atm_AD)

    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(IN OUT) :: predictor_AD
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN OUT) :: Atm_AD

    ! Local variables
    REAL(fp) :: Absorber_AD(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_AD(Predictor%n_Layers)

    ! initialization
    Temperature_AD = ZERO
    Absorber_AD    = ZERO
     
    !-----------------------------------------------------------
    ! adjoint part
    !-----------------------------------------------------------

    !-------------------------------------------
    ! Compute predictor
    !-------------------------------------------
    ! If the C-OPTRAN water vapor line algorithm is set (indicated by n_PCeooffs > 0),         
    ! then compute predictors for OPTRAN water vapor line absorption.                          
    IF( ALLOW_OPTRAN .AND. TC%n_OCoeffs > 0 )THEN                                              

       CALL Compute_Predictor_OPTRAN_AD( Predictor%PAFV%Temperature,         &                 
                                      Predictor%PAFV%Absorber(:, Predictor%PAFV%H2O_idx),  &  
                                      TC%Ref_Level_Pressure, &                                 
                                      TC%Ref_Pressure,       &                                 
                                      Predictor%Secant_Zenith,&                                
                                      TC%Alpha,              &                                 
                                      TC%Alpha_C1,           &                                 
                                      TC%Alpha_C2,           &                                 
                                      Predictor,              &                                
                                      Predictor_AD,           &                                
                                      Temperature_AD,         &                                
                                      Absorber_AD(:, Predictor%PAFV%H2O_idx) )                 

    END IF                                                                                     

    CALL Compute_Predictor_AD( TC%Group_index,         &                                        
                            Predictor%PAFV%Temperature,&                                       
                            Predictor%PAFV%Absorber,   &                                       
                            TC%Ref_Level_Pressure,     &                                        
                            TC%Ref_Temperature,        &                                        
                            TC%Ref_Absorber,           &                                        
                            Predictor%Secant_Zenith,   &                                       
                            Predictor,                 &                                       
                            Predictor_AD,              &                                       
                            Temperature_AD,            &                                       
                            Absorber_AD )                                                      

    !------------------------------------------------------------------
    ! Mapping data from user to internal fixed pressure layers/levels.
    !------------------------------------------------------------------
    CALL Map_Input_AD(Atm,            & ! Input
                      TC,             & ! Input
                      Temperature_AD, & ! Input  
                      Absorber_AD,    & ! Input
                      Atm_AD,         & ! output
                      Predictor%PAFV)   ! Input
         
  END SUBROUTINE ODPS_Compute_Predictors_AD

END MODULE ODPS_AtmAbsorption   

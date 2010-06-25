!
! ODAS_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption for the Optical Depth Absorber Space (ODAS)
! model.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!       Modifed by:     Yong Han, NESDIS/STAR 25-June-2008
!                       yong.han@noaa.gov
!

MODULE ODAS_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO,         &       
                                       MAX_N_LAYERS, &
                                       LIMIT_EXP,    &
                                       LIMIT_LOG       
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmAbsorption_type => CRTM_AtmScatter_type
  USE ODAS_Predictor,            ONLY: Predictor_type,        &
                                       MAX_N_ABSORBERS,       &
                                       MAX_N_ORDERS,          & 
                                       MAX_N_PREDICTORS_USED, &
                                       MAX_N_ORDERS
  USE ODAS_TauCoeff,             ONLY: TC, &
                                       ODAS_TauCoeff_type

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption structure data type
  ! in the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_AtmAbsorption_type
  ! Science routines in this modules
  PUBLIC :: Compute_AtmAbsorption
  PUBLIC :: Compute_AtmAbsorption_TL
  PUBLIC :: Compute_AtmAbsorption_AD
  ! Internal variable structure
  PUBLIC :: AAVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODAS_AtmAbsorption.f90 5959 2009-12-07 14:07:01Z paul.vandelst@noaa.gov $'

  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: AAVariables_type
    PRIVATE
    REAL(fp), DIMENSION(MAX_N_LAYERS, 0:MAX_N_PREDICTORS_USED,&
                        MAX_N_ABSORBERS)              :: b
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: LN_Chi
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Chi   
  END TYPE AAVariables_type

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
!       Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption( TC           , &  ! Input
!                                   ChannelIndex , &  ! Input                        
!                                   Predictor    , &  ! Input                        
!                                   AtmAbsorption, &  ! Output 
!                                   AAVariables    )  ! Internal variable output     
!
! INPUT ARGUMENTS:
!             TC:        Structure containing Tau coefficient data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(ODAS_TauCoeff_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption( TC           , &  ! Input
                                    ChannelIndex , &  ! Input                        
                                    Predictor    , &  ! Input                        
                                    AtmAbsorption, &  ! Output 
                                    AAV            )  ! Internal variable output     
    ! Arguments
    TYPE(ODAS_TauCoeff_type)     , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(AAVariables_type)       , INTENT(OUT)    :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption'
    ! Local variables
    INTEGER  :: l       ! Channel index
    INTEGER  :: k       ! Layer index
    INTEGER  :: j       ! Absorber index
    INTEGER  :: i, ip   ! Predictor index
    INTEGER  :: np      ! # of predictors
    INTEGER  :: ps      ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0    ! the index of the first coefficient in the C coeff subset for deriving the B coeff.
    INTEGER  :: ic      ! the index of the coefficients
    REAL(fp) :: c       ! a coefficient
    INTEGER  :: n_Layers
    

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers
          
    ! Initilise the optical depth
    AtmAbsorption%Optical_Depth = ZERO

    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! -----------------------------------------
      np = TC%Pre_Index(0,j,l)

      IF ( np < 0 ) CYCLE Absorber_Loop

        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          np
        !   b(i) =  > c(np,i).k
        !          /__
        !             np=0
        !
        ! ----------------------------------------------------------------

        ps = TC%Pos_Index(j,l)  ! starting position of the coefficient subset for given j and l
        n_orders = TC%Order(j,l)

        ! compute b(0)
        ic_0 = ps                                   
        AAV%b(1:n_Layers,0,j) = TC%C(ic_0)                       
        DO ic = 1, n_Orders                                         
          c = TC%C(ic_0 + ic)                                    
          DO k = 1, n_Layers                                            
            AAV%b(k,0,j) = AAV%b(k,0,j) + c*Predictor%Ap(k, ic, j)    
          END DO                                                        
        END DO                                                      

        ! ---------------------------------------------------------
        ! compute B(i) coefficients (i > 0)
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).X(i)
        !                    /__
        !                       i=1
        !
        ! ---------------------------------------------------------
        AAV%LN_Chi(1:n_Layers, j) = AAV%b(1:n_Layers,0,j)   ! the b(0) contribution
        DO i = 1, np
          ! b(i) term, i > 0
          ic_0 = ps + i*(n_orders+1)
          AAV%b(1:n_Layers,i,j) = TC%C(ic_0)
          DO ic = 1, n_Orders    
            c = TC%C(ic_0 + ic)
            DO k = 1, n_Layers                                          
              AAV%b(k,i,j) = AAV%b(k,i,j) + c*Predictor%Ap(k, ic, j)  
            END DO                                                      
          END DO
          ! b(i) term contribution
          ip = TC%Pre_Index(i,j,l)                                                
          DO k = 1, n_Layers                                                       
            AAV%LN_Chi(k,j) = AAV%LN_Chi(k,j) + AAV%b(k, i, j)* Predictor%X(k,ip)  
          END DO                                                                   
        END DO

        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! Compute the optical depth profile
        ! --------------------------------
        DO k = 1, n_Layers
          IF( AAV%LN_Chi(k,j) > LIMIT_EXP ) THEN
            AAV%Chi(k,j) = LIMIT_LOG
          ELSE IF( AAV%LN_Chi(k,j) < -LIMIT_EXP ) THEN
            AAV%Chi(k,j) = ZERO
          ELSE
            AAV%Chi(k,j) = EXP(AAV%LN_Chi(k,j))
          ENDIF
          
          AtmAbsorption%Optical_Depth(k) = AtmAbsorption%Optical_Depth(k) + AAV%Chi(k,j)*Predictor%dA(k,j)

        END DO

    END DO Absorber_Loop


    ! --------------------------------
    ! Scale the optical depth to nadir
    ! --------------------------------
    AtmAbsorption%Optical_Depth = AtmAbsorption%Optical_Depth / &
                                  Predictor%Secant_Sensor_Zenith

  END SUBROUTINE Compute_AtmAbsorption


!------------------------------------------------------------------------------
!
! NAME:
!       Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption_TL(  TC              , &  ! Input
!                                       ChannelIndex    , &  ! Input                      
!                                       Predictor       , &  ! FWD Input                  
!                                       Predictor_TL    , &  ! TL Input                   
!                                       AtmAbsorption_TL, &  ! TL Output                  
!                                       AAVariables       )  ! Internal variable input    
!
! INPUT ARGUMENTS:
!             TC:           Structure containing Tau coefficient data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODAS_TauCoeff_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:       Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  Structure containing the computed tangent-linear
!                           optical depth profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption_TL( TC              , &  ! Input
                                       ChannelIndex    , &  ! Input                       
                                       Predictor       , &  ! Input                       
                                       Predictor_TL    , &  ! Input                       
                                       AtmAbsorption_TL, &  ! Output                      
                                       AAV               )  ! Internal variable input     
    ! Arguments
    TYPE(ODAS_TauCoeff_type)     , INTENT(IN)     :: TC
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor
    TYPE(Predictor_type)         , INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_TL'
    ! Local variables
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER  :: np      ! # of predictors
    INTEGER  :: ps      ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0    ! the index of the first coefficient in the C coeff subset for deriving the B coeff.
    INTEGER  :: ic      ! the index of the coefficients
    REAL(fp) :: c       ! a coefficient
    REAL(fp) :: b_TL(Predictor%n_layers)
    REAL(fp) :: LN_Chi_TL(Predictor%n_layers)
    REAL(fp) :: Chi_TL
    INTEGER  :: n_Layers

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers

    ! Initilise the tangent-linear optical depth
    AtmAbsorption_TL%Optical_Depth = ZERO


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! -----------------------------------------
      np = TC%Pre_Index(0,j,l)

      IF ( np < 0 ) CYCLE Absorber_Loop

      ! ----------------------------------------------------------------          
      ! Compute the coefficients for use with the atmospheric predictors          
      !                                                                           
      ! For every atmospheric predictor, Pred(i), the coefficient                 
      ! associated with it, b(i), at a particular absorber amount                 
      ! level, k, is given by an N'th order polynomial,                           
      !                                                                           
      !           __ N                                                            
      !          \          np                                                    
      !   b(i) =  > c(np,i).k                                                     
      !          /__                                                              
      !             np=0                                                          
      !                                                                           
      ! ----------------------------------------------------------------          

      ps = TC%Pos_Index(j,l)  ! starting position of the coefficient subset f  or given j and l
      n_orders = TC%Order(j,l)                                                 
                                                                                  
      ! compute b(0)                                                              
      ic_0 = ps                                                                   
      b_TL(1:n_Layers) = ZERO                                                     
      DO ic = 1, n_Orders                                                         
        c = TC%C(ic_0 + ic)                                                    
        DO k = 1, n_Layers                                                        
          b_TL(k) = b_TL(k) + c*Predictor_TL%Ap(k, ic, j)                            
        END DO                                                                    
      END DO                                                                      

      ! ---------------------------------------------------------                 
      ! compute B(i) coefficients (i > 0)                                         
      ! Compute the logarithm of the absorption coefficient                       
      !                                                                           
      ! The logarithm of the absorption coefficient, LN(chi), is                  
      ! determined from the regression equation,                                  
      !                                                                           
      !                     __Iuse                                                
      !                    \                                                      
      !   LN(chi) = b(0) +  > b(i).X(i)                                           
      !                    /__                                                    
      !                       i=1                                                 
      !                                                                           
      ! ---------------------------------------------------------                 
      LN_Chi_TL(1:n_Layers) = b_TL(1:n_Layers)  ! b(0) term contribution          
      DO i = 1, np                                                                
        ! b(i) term, i > 0                                                        
        ic_0 = ps + i*(n_orders+1)                                                
        b_TL(1:n_Layers) = ZERO                                                   
        DO ic = 1, n_Orders                                                       
          c = TC%C(ic_0 + ic)                                                  
          DO k = 1, n_Layers                                                      
            b_TL(k) = b_TL(k) + c*Predictor_TL%Ap(k, ic, j)                       
          END DO                                                                  
        END DO                                                                    
        ! b(i) term contribution                                                  
        ip = TC%Pre_Index(i,j,l)                                                 
        DO k = 1, n_Layers                                                        
          LN_Chi_TL(k) = LN_Chi_TL(k) + b_TL(k)* Predictor%X(k,ip) &              
                                      + AAV%b(k,i,j)*Predictor_TL%X(k,ip)         
        END DO                                                                    
      END DO                                                                      


      ! --------------------------------                                          
      ! Check the value of the logarithm                                          
      ! of the absorption coefficient                                             
      ! --------------------------------                                          
      DO k = 1, n_Layers                                                          
        IF( AAV%LN_Chi(k,j) > LIMIT_EXP ) THEN                                    
          Chi_TL = ZERO                                                           
        ELSE IF( AAV%LN_Chi(k,j) < -LIMIT_EXP ) THEN                              
          Chi_TL = ZERO                                                           
        ELSE                                                                      
          Chi_TL = AAV%Chi(k,j) * LN_Chi_TL(k)                                    
        ENDIF                                                                     

        ! ------------------------------------------                              
        ! Calculate the tangent-linear optical depth                              
        ! ------------------------------------------                              
        AtmAbsorption_TL%Optical_Depth(k) = AtmAbsorption_TL%Optical_Depth(k) &   
                                          + Chi_TL*Predictor%dA(k,j) &            
                                          + AAV%Chi(k,j)*Predictor_TL%dA(k,j)     
      END DO                                                                      

    END DO Absorber_Loop


    ! -----------------------------------------------
    ! Scale the tangent-linear optical depth to nadir
    ! -----------------------------------------------
    AtmAbsorption_TL%Optical_Depth = AtmAbsorption_TL%Optical_Depth / &
                                     Predictor%Secant_Sensor_Zenith

  END SUBROUTINE Compute_AtmAbsorption_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depth adjoints due to
!       gaseous absorption for a given sensor and channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption_AD( TC              , &  ! Input
!                                      ChannelIndex    , &  ! Input                       
!                                      Predictor       , &  ! FWD Input                   
!                                      AtmAbsorption_AD, &  ! TL  Input                   
!                                      Predictor_AD    , &  ! TL  Output                  
!                                      AAVariables       )  ! Internal variable input     
!
! INPUT ARGUMENTS:
! OPTIONAL INPUT:
!             TC:           Structure containing Tau coefficient data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODAS_TauCoeff_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AtmAbsorption_AD:   Structure containing the computed adjoint
!                           optical depth profile data.
!                           Set to zero upon output.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:       Structure containing the adjoint integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_AtmAbsorption_AD( TC              , &  ! Input
                                       ChannelIndex    , &  ! Input                       
                                       Predictor       , &  ! FWD Input                   
                                       AtmAbsorption_AD, &  ! AD  Input                   
                                       Predictor_AD    , &  ! AD  Output                  
                                       AAV               )  ! Internal variable input     
    ! Arguments
    TYPE(ODAS_TauCoeff_type),      INTENT(IN)     :: TC
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(Predictor_type),          INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(Predictor_type),          INTENT(IN OUT) :: Predictor_AD
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_AD'
    ! Local variables
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER  :: np      ! # of predictors
    INTEGER  :: ps      ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0    ! the index of the first coefficient in the C coeff subset for deriving the B coeff.
    INTEGER  :: ic      ! the index of the coefficients
    REAL(fp) :: c       ! a coefficient
    REAL(fp) :: b_AD(Predictor%n_layers)
    REAL(fp) :: LN_Chi_AD(Predictor%n_layers)
    REAL(fp) :: Chi_AD
    INTEGER  :: n_Layers


    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers

    b_AD      = ZERO
    LN_Chi_AD = ZERO
    Chi_AD    = ZERO

    ! -------------------------------------------
    ! Compute adjoint nadir optical depth profile
    ! -------------------------------------------
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth / &
                                     Predictor%Secant_Sensor_Zenith


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! -----------------------------------------
      np = TC%Pre_Index(0,j,l)

      IF ( np < 0 ) CYCLE Absorber_Loop

      ps = TC%Pos_Index(j,l)  ! starting position of the coefficient subset f  or given j and l
      n_orders = TC%Order(j,l)                                                 

      DO k = n_Layers, 1, -1

        ! -----------------------------
        ! Adjoints of the optical depth
        ! -----------------------------
        Predictor_AD%dA(k,j) = Predictor_AD%dA(k,j) + &
                               (AAV%Chi(k,j) * AtmAbsorption_AD%Optical_Depth(k))
        Chi_AD = Chi_AD + Predictor%dA(k,j) * AtmAbsorption_AD%Optical_Depth(k)


        ! ----------------------------------------
        ! Initialise the LOCAL adjoint variable,
        !   LN_Chi_AD.
        ! Note that the reinitialisaiton of the
        ! LOCAL adjoint variable
        !   Absorption_Coefficient_AD
        ! is implied since for each layer it is
        ! reassigned in the preceding line of code
        ! ----------------------------------------
        IF( AAV%LN_Chi(k,j) > LIMIT_EXP ) THEN
          Chi_AD = ZERO
        ELSE IF( AAV%LN_Chi(k,j) < -LIMIT_EXP ) THEN                              
          Chi_AD = ZERO                                                           
        ELSE                                                                      
          LN_Chi_AD(k) = LN_Chi_AD(k) + AAV%Chi(k,j) * Chi_AD
          Chi_AD       = ZERO
        ENDIF
      END DO

      DO i = np, 1, -1
        ! b(i) term contribution                                                  
        ip = TC%Pre_Index(i,j,l)                                                 
        DO k = n_Layers, 1, -1 
          b_AD(k) = b_AD(k) + LN_Chi_AD(k)*Predictor%X(k,ip)
          Predictor_AD%X(k,ip) = Predictor_AD%X(k,ip) + AAV%b(k,i,j)*LN_Chi_AD(k)
        END DO
        
        ! b(i) term, i > 0                                                        
        ic_0 = ps + i*(n_orders+1)                                                
        DO ic = n_Orders, 1, -1 
                                                      
          c = TC%C(ic_0 + ic)                                                  
          DO k = n_Layers, 1, -1
            Predictor_AD%Ap(k, ic, j) = Predictor_AD%Ap(k, ic, j) + c*b_AD(k)                                                  
          END DO                                                                  
        END DO                                                                    
        b_AD(1:n_Layers) = ZERO                                                   
                                                                            
      END DO                                                                      
      b_AD(1:n_Layers) = b_AD(1:n_Layers) + LN_Chi_AD(1:n_Layers)
      LN_Chi_AD(1:n_Layers) = ZERO

      ic_0 = ps                                                                   
      DO ic = n_Orders, 1, -1                                                         
        c = TC%C(ic_0 + ic)                                                    
        DO k = n_Layers, 1, -1 
          Predictor_AD%Ap(k, ic, j) = Predictor_AD%Ap(k, ic, j) + c*b_AD(k)                                                       
        END DO                                                                    
      END DO                                                                      
      b_AD(1:n_Layers) = ZERO
      
    END DO Absorber_Loop

    AtmAbsorption_AD%Optical_Depth = ZERO

  END SUBROUTINE Compute_AtmAbsorption_AD

END MODULE ODAS_AtmAbsorption

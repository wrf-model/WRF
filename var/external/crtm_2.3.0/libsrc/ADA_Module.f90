!
! ADA_Module
!
! Module containing the Adding Doubling Adding (ADA) radiative
! transfer solution procedures used in the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu, QSS at JCSDA; quanhua.liu@noaa.gov
!                       Yong Han,    NOAA/NESDIS;  yong.han@noaa.gov
!                       Paul van Delst; CIMMS/SSEC; paul.vandelst@noaa.gov
!                       08-Jun-2004

MODULE ADA_Module

  ! ------------------
  ! Environemnt set up
  ! ------------------
  ! Module use statements
  USE RTV_Define
  USE CRTM_Parameters
  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Utility
    
  IMPLICIT NONE
  
  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  
  PUBLIC CRTM_ADA
  PUBLIC CRTM_ADA_TL
  PUBLIC CRTM_ADA_AD
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: $'
  
CONTAINS

!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  
  SUBROUTINE CRTM_ADA(n_Layers, & ! Input  number of atmospheric layers
                             w, & ! Input  layer scattering albedo
                          T_OD, & ! Input  layer optical depth
             cosmic_background, & ! Input  cosmic background radiance
                    emissivity, & ! Input  surface emissivity
                  reflectivity, & ! Input  surface reflectivity matrix 
           direct_reflectivity, & ! Input  surface direct reflectivity 
                           RTV)   ! IN/Output upward radiance and others
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW radiance at the top of the atmosphere  !
!   including atmospheric scattering. The scheme will include solar part.   !
!   The ADA algorithm computes layer reflectance and transmittance as well  !
!   as source function by the subroutine CRTM_Doubling_layer, then uses     !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n_Layers
    INTEGER nZ
    TYPE(RTV_type), INTENT( INOUT ) :: RTV
    REAL (fp), INTENT(IN), DIMENSION( : ) ::  w,T_OD
    REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity, direct_reflectivity
    REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
    REAL (fp), INTENT(IN) ::  cosmic_background

    ! -------------- internal variables --------------------------------- !
    !  Abbreviations:                                                     !
    !      s: scattering, rad: radiance, trans: transmission,             !
    !         refl: reflection, up: upward, down: downward                !
    ! --------------------------------------------------------------------!
    REAL (fp), DIMENSION(RTV%n_Angles, RTV%n_Angles) :: temporal_matrix
    REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
    REAL (fp), DIMENSION(0:n_Layers) :: total_opt
    INTEGER :: i, j, k, Error_Status
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_ADA'
    CHARACTER(256) :: Message
 
    total_opt(0) = ZERO
    DO k = 1, n_Layers
      total_opt(k) = total_opt(k-1) + T_OD(k)
    END DO

    nZ = RTV%n_Angles
    RTV%s_Layer_Trans = ZERO
    RTV%s_Layer_Refl = ZERO
    RTV%s_Level_Refl_UP = ZERO
    RTV%s_Level_Rad_UP = ZERO
    RTV%s_Layer_Source_UP = ZERO
    RTV%s_Layer_Source_DOWN = ZERO

    RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,n_Layers)=reflectivity(1:RTV%n_Angles,1:RTV%n_Angles)

    IF( RTV%mth_Azi == 0 ) THEN
      RTV%s_Level_Rad_UP(1:RTV%n_Angles,n_Layers ) = emissivity(1:RTV%n_Angles)*RTV%Planck_Surface
    END IF

    IF( RTV%Solar_Flag_true ) THEN
      RTV%s_Level_Rad_UP(1:nZ,n_Layers ) = RTV%s_Level_Rad_UP(1:nZ,n_Layers )+direct_reflectivity(1:nZ)* &
        RTV%COS_SUN*RTV%Solar_irradiance/PI*exp(-total_opt(n_Layers)/RTV%COS_SUN)       
    END IF

    ! UPWARD ADDING LOOP STARTS FROM BOTTOM LAYER TO ATMOSPHERIC TOP LAYER.
    DO 10 k = n_Layers, 1, -1

    ! Compute tranmission and reflection matrices for a layer
    IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

    !  ----------------------------------------------------------- !
    !    CALL  multiple-stream algorithm for computing layer       !
    !    transmission, reflection, and source functions.           !
    !  ----------------------------------------------------------- !

    CALL CRTM_AMOM_layer( &
           RTV%n_Streams,            &
           RTV%n_Angles,k,w(k),      &
           T_OD(k),                  &
           total_opt(k-1),           &
           RTV%COS_Angle,            & ! Input
           RTV%COS_Weight,           &
           RTV%Pff(:,:,k),           &
           RTV%Pbb(:,:,k),           & ! Input
           RTV%Planck_Atmosphere(k), & ! Input
           RTV                       ) ! Internal variable  
    !  ----------------------------------------------------------- !
    !    Adding method to add the layer to the present level       !
    !    to compute upward radiances and reflection matrix         !
    !    at new level.                                             !
    !  ----------------------------------------------------------- !

    temporal_matrix = -matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                       RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))
    DO i = 1, RTV%n_Angles 
      temporal_matrix(i,i) = ONE + temporal_matrix(i,i)
    END DO

    RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k) = matinv(temporal_matrix, Error_Status)
    IF( Error_Status /= SUCCESS  ) THEN
      WRITE( Message,'("Error in matrix inversion matinv(temporal_matrix, Error_Status) ")' ) 
      CALL Display_Message( ROUTINE_NAME,  &                                                    
                            TRIM(Message), &                                                   
                            Error_Status   )                                          
      RETURN                                                                                    
    END IF
         
    RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k) =   &
     matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))
    refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                                  RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

    RTV%s_Level_Rad_UP(1:RTV%n_Angles,k-1 )=RTV%s_Layer_Source_UP(1:RTV%n_Angles,k)+ &
    matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down(1:RTV%n_Angles,k) &
          +RTV%s_Level_Rad_UP(1:RTV%n_Angles,k ))
    RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k), &
          RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))
    RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k-1)=RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k) + &
    matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)) 

    ELSE
      DO i = 1, RTV%n_Angles 
        RTV%s_Layer_Trans(i,i,k) = exp(-T_OD(k)/RTV%COS_Angle(i))
        RTV%s_Layer_Source_UP(i,k) = RTV%Planck_Atmosphere(k) * (ONE - RTV%s_Layer_Trans(i,i,k) )
        RTV%s_Layer_Source_DOWN(i,k) = RTV%s_Layer_Source_UP(i,k)

      END DO

      ! Adding method
      DO i = 1, RTV%n_Angles 
        RTV%s_Level_Rad_UP(i,k-1 )=RTV%s_Layer_Source_UP(i,k)+ &
        RTV%s_Layer_Trans(i,i,k)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))  &
         +RTV%s_Level_Rad_UP(i,k ))
      ENDDO
        DO i = 1, RTV%n_Angles 
          DO j = 1, RTV%n_Angles 
            RTV%s_Level_Refl_UP(i,j,k-1)=RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
          ENDDO
        ENDDO
    ENDIF
    
    10     CONTINUE

    !  Adding reflected cosmic background radiation
    IF( RTV%mth_Azi == 0 ) THEN
       DO i = 1, RTV%n_Angles 
         RTV%s_Level_Rad_UP(i,0)=RTV%s_Level_Rad_UP(i,0)+sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,0))*cosmic_background
       ENDDO
    END IF

    RETURN
    
  END SUBROUTINE CRTM_ADA 
      
  SUBROUTINE CRTM_AMOM_layer(    n_streams, & ! Input, number of streams
                                  nZ, & ! Input, number of angles
                                  KL, & ! Input, KL-th layer 
                       single_albedo, & ! Input, single scattering albedo
                       optical_depth, & ! Input, layer optical depth
                           total_opt, & ! Input, accumulated optical depth from the top to current layer top
                           COS_Angle, & ! Input, COSINE of ANGLES
                          COS_Weight, & ! Input, GAUSSIAN Weights
                                  ff, & ! Input, Phase matrix (forward part)
                                  bb, & ! Input, Phase matrix (backward part)
                         Planck_Func, & ! Input, Planck for layer temperature
                                 RTV)   ! Output, layer transmittance, reflectance, and source 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Method and References
!    The transmittance and reflectance matrices is further derived from 
!    matrix operator method. The matrix operator method is referred to the paper by
!
!    Weng, F., and Q. Liu, 2003: Satellite Data Assimilation in Numerical Weather Prediction
!    Model: Part 1: Forward Radiative Transfer and Jacobian Modeling in Cloudy Atmospheres,
!    J. Atmos. Sci., 60, 2633-2646.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: n_streams,nZ,KL
   TYPE(RTV_type), INTENT( INOUT ) :: RTV
   REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
   REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
   REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt

   ! internal variables
   REAL(fp), DIMENSION(nZ,nZ) :: trans, refl, tempo
   REAL(fp) :: s, c, xx
   INTEGER :: i,j,N2,N2_1
   INTEGER :: Error_Status
   REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
   REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ)    
   CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer'
   CHARACTER(256) :: Message

   ! for small layer optical depth, single scattering is applied.  
   IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
     s = optical_depth * single_albedo
     DO i = 1, nZ
       RTV%Thermal_C(i,KL) = ZERO
       c = s/COS_Angle(i)
       DO j = 1, nZ
         RTV%s_Layer_Refl(i,j,KL) = c * bb(i,j) * COS_Weight(j)
         RTV%s_Layer_Trans(i,j,KL) = c * ff(i,j) * COS_Weight(j)
         IF( i == j ) THEN
           RTV%s_Layer_Trans(i,i,KL) = RTV%s_Layer_Trans(i,i,KL) + &
             ONE - optical_depth/COS_Angle(i)
         END IF
         IF( RTV%mth_Azi == 0 ) THEN
           RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + &
           ( RTV%s_Layer_Refl(i,j,KL) + RTV%s_Layer_Trans(i,j,KL) )
         END IF
       ENDDO

       IF( RTV%mth_Azi == 0 ) THEN
         RTV%s_Layer_Source_UP(i,KL) = ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func
         RTV%s_Layer_Source_DOWN(i,KL) = RTV%s_Layer_Source_UP(i,KL)
       END IF
     ENDDO

     RETURN

   END IF
   !
   ! for numerical stability, 
   IF( single_albedo < max_albedo ) THEN
     s = single_albedo
   ELSE
     s = max_albedo
   END IF
   !
   ! building phase matrices
   DO i = 1, nZ
     c = s/COS_Angle(i)
     DO j = 1, nZ
       RTV%PM(i,j,KL) = c * bb(i,j) * COS_Weight(j)
       RTV%PP(i,j,KL) = c * ff(i,j) * COS_Weight(j)
     ENDDO
       RTV%PP(i,i,KL) = RTV%PP(i,i,KL) - ONE/COS_Angle(i)
   ENDDO
   RTV%PPM(1:nZ,1:nZ,KL) = RTV%PP(1:nZ,1:nZ,KL) - RTV%PM(1:nZ,1:nZ,KL)
   RTV%i_PPM(1:nZ,1:nZ,KL) = matinv( RTV%PPM(1:nZ,1:nZ,KL), Error_Status )
   IF( Error_Status /= SUCCESS  ) THEN
     WRITE( Message,'("Error in matrix inversion matinv( RTV%PPM(1:nZ,1:nZ,KL), Error_Status ) ")' ) 
     CALL Display_Message( ROUTINE_NAME, &                                                    
                           TRIM(Message), &                                                   
                           Error_Status )                                          
     RETURN                                                                                    
   END IF

   RTV%PPP(1:nZ,1:nZ,KL) = RTV%PP(1:nZ,1:nZ,KL) + RTV%PM(1:nZ,1:nZ,KL)
   RTV%HH(1:nZ,1:nZ,KL) = matmul( RTV%PPM(1:nZ,1:nZ,KL), RTV%PPP(1:nZ,1:nZ,KL) )   
   !
   ! save phase element RTV%HH, call ASYMTX for calculating eigenvalue and vectors.
   tempo = RTV%HH(1:nZ,1:nZ,KL)
   CALL ASYMTX(tempo,nZ,nZ,nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL),Error_Status)
   DO i = 1, nZ
     IF( RTV%EigVa(i,KL) > ZERO ) THEN         
       RTV%EigValue(i,KL) = sqrt( RTV%EigVa(i,KL) )
     ELSE
       RTV%EigValue(i,KL) = ZERO
     END IF
   END DO

   DO i = 1, nZ
     DO j = 1, nZ
       RTV%EigVeVa(i,j,KL) = RTV%EigVe(i,j,KL) * RTV%EigValue(j,KL)
     END DO
   END DO       
   RTV%EigVeF(1:nZ,1:nZ,KL) = matmul( RTV%i_PPM(1:nZ,1:nZ,KL), RTV%EigVeVa(1:nZ,1:nZ,KL) )

   ! compute layer reflection, transmission and source function
   RTV%Gp(1:nZ,1:nZ,KL) = ( RTV%EigVe(1:nZ,1:nZ,KL) + RTV%EigVeF(1:nZ,1:nZ,KL) )/2.0_fp
   RTV%Gm(1:nZ,1:nZ,KL) = ( RTV%EigVe(1:nZ,1:nZ,KL) - RTV%EigVeF(1:nZ,1:nZ,KL) )/2.0_fp
   RTV%i_Gm(1:nZ,1:nZ,KL) = matinv( RTV%Gm(1:nZ,1:nZ,KL), Error_Status)

   IF( Error_Status /= SUCCESS  ) THEN
     WRITE( Message,'("Error in matrix inversion matinv( RTV%Gm(1:nZ,1:nZ,KL), Error_Status) ")' ) 
     CALL Display_Message( ROUTINE_NAME, &                                                    
                           TRIM(Message), &                                                   
                           Error_Status )                                          
     RETURN                                                                                    
   END IF             

   DO i = 1, nZ
     xx = RTV%EigValue(i,KL)*optical_depth
     RTV%Exp_x(i,KL) = exp(-xx)
   END DO

   DO i = 1, nZ
     DO j = 1, nZ
       RTV%A1(i,j,KL) = RTV%Gp(i,j,KL) * RTV%Exp_x(j,KL)
       RTV%A4(i,j,KL) = RTV%Gm(i,j,KL) * RTV%Exp_x(j,KL)
     END DO
   END DO

   RTV%A2(1:nZ,1:nZ,KL) = matmul( RTV%i_Gm(1:nZ,1:nZ,KL), RTV%A1(1:nZ,1:nZ,KL) )
   RTV%A3(1:nZ,1:nZ,KL) = matmul( RTV%Gp(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
   RTV%A5(1:nZ,1:nZ,KL) = matmul( RTV%A1(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
   RTV%A6(1:nZ,1:nZ,KL) = matmul( RTV%A4(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
   RTV%Gm_A5(1:nZ,1:nZ,KL) = RTV%Gm(1:nZ,1:nZ,KL) - RTV%A5(1:nZ,1:nZ,KL)     
   RTV%i_Gm_A5(1:nZ,1:nZ,KL) = matinv(RTV%Gm_A5(1:nZ,1:nZ,KL), Error_Status)
   IF( Error_Status /= SUCCESS  ) THEN
     WRITE( Message,'("Error in matrix inversion matinv(RTV%Gm_A5(1:nZ,1:nZ,KL), Error_Status) ")' ) 
     CALL Display_Message( ROUTINE_NAME, &                                                    
                           TRIM(Message), &                                                   
                           Error_Status )                                          
     RETURN                                                                                    
   END IF
   trans = matmul( RTV%A4(1:nZ,1:nZ,KL) - RTV%A3(1:nZ,1:nZ,KL), RTV%i_Gm_A5(1:nZ,1:nZ,KL) ) 
   refl = matmul( RTV%Gp(1:nZ,1:nZ,KL) - RTV%A6(1:nZ,1:nZ,KL), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )

   ! post processing  
   RTV%s_Layer_Trans(1:nZ,1:nZ,KL) = trans(:,:)
   RTV%s_Layer_Refl(1:nZ,1:nZ,KL) = refl(:,:)
   RTV%s_Layer_Source_UP(:,KL) = ZERO
   IF( RTV%mth_Azi == 0 ) THEN
     DO i = 1, nZ
       RTV%Thermal_C(i,KL) = ZERO
       DO j = 1, n_Streams
         RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + (trans(i,j) + refl(i,j) )
       END DO
       IF ( i == nZ .AND. nZ == (n_Streams+1) ) THEN
         RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + trans(nZ,nZ)
       END IF
       RTV%s_Layer_Source_UP(i,KL) = ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func
       RTV%s_Layer_Source_DOWN(i,KL) = RTV%s_Layer_Source_UP(i,KL)
     END DO
   END IF

   !  compute visible part for visible channels during daytime
   IF( RTV%Solar_Flag_true ) THEN
     N2 = 2 * nZ
     N2_1 = N2 - 1
     source_up = ZERO
     source_down = ZERO
     !
     ! Solar source  
     Sfactor = single_albedo*RTV%Solar_irradiance/PI
     IF( RTV%mth_Azi == 0 ) Sfactor = Sfactor/TWO
       EXPfactor = exp(-optical_depth/RTV%COS_SUN)
       s_transmittance = exp(-total_opt/RTV%COS_SUN)

       DO i = 1, nZ     
         Solar(i) = -bb(i,nZ+1)*Sfactor
         Solar(i+nZ) = -ff(i,nZ+1)*Sfactor

         DO j = 1, nZ
           V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
           V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
           V0(i,j+nZ) = V0(i+nZ,j)
           V0(nZ+i,j+nZ) = V0(i,j)
         ENDDO
         V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
         V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
       ENDDO

       V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF         

       Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
       Solar1(N2) = ZERO
       Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )


       DO i = 1, nZ
         source_up(i) = Solar1(i)
         source_down(i) = EXPfactor*Solar1(i+nZ)
         DO j = 1, nZ
          source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
          source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
         END DO
       END DO
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
       ELSE
         source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
       END IF

       source_up(1:nZ) = source_up(1:nZ)*s_transmittance
       source_down(1:nZ) = source_down(1:nZ)*s_transmittance

       RTV%s_Layer_Source_UP(1:nZ,KL) = RTV%s_Layer_Source_UP(1:nZ,KL)+source_up(1:nZ)
       RTV%s_Layer_Source_DOWN(1:nZ,KL) = RTV%s_Layer_Source_DOWN(1:nZ,KL)+source_down(1:nZ)
   END IF

   RETURN

  END SUBROUTINE CRTM_AMOM_layer
  
   SUBROUTINE CRTM_ADA_TL(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                              T_OD, & ! Input  layer optical depth
                 cosmic_background, & ! Input  cosmic background radiance
                        emissivity, & ! Input  surface emissivity
               direct_reflectivity, & ! Input  direct reflectivity
                               RTV, & ! Input  structure containing forward part results 
              Planck_Atmosphere_TL, & ! Input  tangent-linear atmospheric layer Planck radiance 
                 Planck_Surface_TL, & ! Input  TL surface Planck radiance
                              w_TL, & ! Input  TL layer scattering albedo
                           T_OD_TL, & ! Input  TL layer optical depth
                     emissivity_TL, & ! Input  TL surface emissivity
                   reflectivity_TL, & ! Input  TL  reflectivity
            direct_reflectivity_TL, & ! Input  TL  direct reflectivity
                            Pff_TL, & ! Input  TL forward phase matrix
                            Pbb_TL, & ! Input  TL backward phase matrix
                         s_rad_up_TL) ! Output TL upward radiance 
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW tangent-linear radiance at the top of  !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_TL algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w,T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity,direct_reflectivity
      REAL (fp), INTENT(IN) ::  cosmic_background

      REAL (fp),INTENT(IN),DIMENSION( :,:,: ) ::  Pff_TL, Pbb_TL
      REAL (fp),INTENT(IN),DIMENSION( : ) ::  w_TL,T_OD_TL
      REAL (fp),INTENT(IN),DIMENSION( 0: ) ::  Planck_Atmosphere_TL
      REAL (fp),INTENT(IN) ::  Planck_Surface_TL
      REAL (fp),INTENT(IN),DIMENSION( : ) ::  emissivity_TL
      REAL (fp),INTENT(IN),DIMENSION( :,: ) :: reflectivity_TL 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_TL 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: direct_reflectivity_TL
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL (fp), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_TL

      REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL (fp), DIMENSION( RTV%n_Angles ) :: s_source_up_TL,s_source_down_TL,refl_down_TL
 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_trans_TL,s_refl_TL,Refl_Trans_TL 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_refl_up_TL,Inv_Gamma_TL,Inv_GammaT_TL
      REAL (fp), DIMENSION(0:n_Layers) :: total_opt, total_opt_TL
      INTEGER :: i, j, k, nZ
!
       nZ = RTV%n_Angles
       
       total_opt(0) = ZERO
       total_opt_TL(0) = ZERO
       DO k = 1, n_Layers
         total_opt(k) = total_opt(k-1) + T_OD(k)
         total_opt_TL(k) = total_opt_TL(k-1) + T_OD_TL(k)
       END DO
       
       Refl_Trans_TL = ZERO
       s_rad_up_TL = ZERO
       s_refl_up_TL = reflectivity_TL
       IF( RTV%mth_Azi == 0 ) THEN
       s_rad_up_TL = emissivity_TL * RTV%Planck_Surface + emissivity * Planck_Surface_TL
       END IF
 
       IF( RTV%Solar_Flag_true ) THEN
         s_rad_up_TL = s_rad_up_TL+direct_reflectivity_TL*RTV%COS_SUN*RTV%Solar_irradiance/PI  &
                     * exp(-total_opt(n_Layers)/RTV%COS_SUN) &
                     - direct_reflectivity * RTV%Solar_irradiance/PI  &
                     * total_opt_TL(n_Layers) * exp(-total_opt(n_Layers)/RTV%COS_SUN)
       END IF 
     
       DO 10 k = n_Layers, 1, -1
         s_source_up_TL = ZERO
         s_source_down_TL = ZERO
         s_trans_TL = ZERO
         s_refl_TL = ZERO
         Inv_GammaT_TL = ZERO
         Inv_Gamma_TL = ZERO
         refl_down_TL = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !

      call CRTM_AMOM_layer_TL(RTV%n_Streams,RTV%n_Angles,k,w(k),T_OD(k),total_opt(k-1), & !Input
         RTV%COS_Angle(1:RTV%n_Angles),RTV%COS_Weight(1:RTV%n_Angles)                   , & !Input
         RTV%Pff(:,:,k), RTV%Pbb(:,:,k),RTV%Planck_Atmosphere(k)                        , & !Input
         w_TL(k),T_OD_TL(k),total_opt_TL(k-1),Pff_TL(:,:,k)                             , & !Input
         Pbb_TL(:,:,k),Planck_Atmosphere_TL(k),RTV                                      , & !Input
         s_trans_TL,s_refl_TL,s_source_up_TL,s_source_down_TL)                              !Output
   
!         Adding method
         temporal_matrix_TL = -matmul(s_refl_up_TL,RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                            - matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_refl_TL)

         temporal_matrix_TL = matmul(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k),temporal_matrix_TL)
         Inv_Gamma_TL = -matmul(temporal_matrix_TL,RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))

         Inv_GammaT_TL = matmul(s_trans_TL, RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), Inv_Gamma_TL)

         refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))
         refl_down_TL(:) = matmul(s_refl_up_TL,RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)) &
                           + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_source_down_TL(:))
         s_rad_up_TL(1:RTV%n_Angles)=s_source_up_TL(1:RTV%n_Angles)+ &
         matmul(Inv_GammaT_TL,refl_down(:,k)+RTV%s_Level_Rad_UP(1:RTV%n_Angles,k))  &
         +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down_TL(1:RTV%n_Angles)+s_rad_up_TL(1:RTV%n_Angles))

         Refl_Trans_TL = matmul(s_refl_up_TL,RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_trans_TL)

         s_refl_up_TL=s_refl_TL+matmul(Inv_GammaT_TL,RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                     +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),Refl_Trans_TL)

         Refl_Trans_TL = ZERO
         
      ELSE

         DO i = 1, RTV%n_Angles
           s_trans_TL(i,i) = -T_OD_TL(k)/RTV%COS_Angle(i) * RTV%s_Layer_Trans(i,i,k)
           s_source_up_TL(i) = Planck_Atmosphere_TL(k) * (ONE - RTV%s_Layer_Trans(i,i,k) ) &
                             - RTV%Planck_Atmosphere(k) * s_trans_TL(i,i)
           s_source_down_TL(i) = s_source_up_TL(i)
         ENDDO

!         Adding method
        DO i = 1, RTV%n_Angles 
        s_rad_up_TL(i)=s_source_up_TL(i) &
        +s_trans_TL(i,i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
        *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k)) &
        +RTV%s_Layer_Trans(i,i,k)  &
        *(sum(s_refl_up_TL(i,1:RTV%n_Angles)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)  &
        +RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*s_source_down_TL(1:RTV%n_Angles))+s_rad_up_TL(i))

        ENDDO
                                       
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_refl_up_TL(i,j)=s_trans_TL(i,i)*RTV%s_Level_Refl_UP(i,j,k)  &
        *RTV%s_Layer_Trans(j,j,k) &
        +RTV%s_Layer_Trans(i,i,k)*s_refl_up_TL(i,j)*RTV%s_Layer_Trans(j,j,k)  &
        +RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_trans_TL(j,j)
        ENDDO
        ENDDO

      ENDIF
   10     CONTINUE
!
!  Adding reflected cosmic background radiation
    IF( RTV%mth_Azi == 0 ) THEN
      DO i = 1, RTV%n_Angles 
      s_rad_up_TL(i)=s_rad_up_TL(i)+sum(s_refl_up_TL(i,:))*cosmic_background
      ENDDO
    END IF
!
      RETURN
      END SUBROUTINE CRTM_ADA_TL
      
      SUBROUTINE CRTM_AMOM_layer_TL( n_streams, & ! Input, number of streams
                                            nZ, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     total_opt, & ! Input, accumulated optical depth from the top to current layer top
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                              single_albedo_TL, & ! Input, tangent-linear single albedo
                              optical_depth_TL, & ! Input, TL layer optical depth
                                  total_opt_TL, & ! Input, accumulated TL optical depth from the top to current layer top
                                         ff_TL, & ! Input, TL forward Phase matrix
                                         bb_TL, & ! Input, TL backward Phase matrix
                                Planck_Func_TL, & ! Input, TL Planck for layer temperature
                                           RTV, & ! Input, structure containing forward results 
                                      trans_TL, & ! Output, layer tangent-linear trans 
                                       refl_TL, & ! Output, layer tangent-linear refl 
                                  source_up_TL, & ! Output, layer tangent-linear source_up 
                                source_down_TL)   ! Output, layer tangent-linear source_down 
                                                                
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute TL layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,nZ,KL
     TYPE(RTV_type), INTENT( IN ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt
     !
     ! internal variables
     REAL(fp) :: s, c, s_TL,c_TL,xx_TL
     INTEGER :: i,j
     INTEGER :: Error_Status
     !
     ! Tangent-Linear Part
     REAL(fp), INTENT(OUT), DIMENSION( :,: ) :: trans_TL,refl_TL
     REAL(fp), INTENT(OUT), DIMENSION( : ) :: source_up_TL,source_down_TL
     
     REAL(fp), INTENT(IN) :: single_albedo_TL
     REAL(fp), INTENT(IN) :: optical_depth_TL,Planck_Func_TL,total_opt_TL
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff_TL,bb_TL

     REAL(fp), DIMENSION(nZ) :: Exp_x_TL,EigVa_TL,EigValue_TL
     REAL(fp), DIMENSION(nZ,nZ) :: i_Gm_TL, Gm_TL, Gp_TL, EigVe_TL, EigVeF_TL, EigVeVa_TL
     REAL(fp), DIMENSION(nZ,nZ) :: A1_TL,A2_TL,A3_TL,A4_TL,A5_TL,A6_TL,Gm_A5_TL,i_Gm_A5_TL
     REAL(fp), DIMENSION(nZ,nZ) :: HH_TL,PM_TL,PP_TL,PPM_TL,i_PPM_TL,PPP_TL

     REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
     REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ) 
     REAL(fp) :: EXPfactor_TL,Sfactor_TL,s_transmittance_TL,Solar_TL(2*nZ),V0_TL(2*nZ,2*nZ),Solar1_TL(2*nZ)
     REAL(fp) :: Sfac2_TL
     REAL(fp), DIMENSION( nZ ) :: thermal_up_TL,thermal_down_TL
     REAL(fp), DIMENSION(nZ,nZ) :: trans, refl
     INTEGER :: N2, N2_1
     REAL(fp) :: Thermal_C_TL
     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer_TL'
     CHARACTER(256) :: Message
     !
     ! for small layer optical depth, single scattering is applied.  
     IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
       s = optical_depth * single_albedo
       s_TL = optical_depth_TL * single_albedo + optical_depth * single_albedo_TL
       DO i = 1, nZ
         Thermal_C_TL = ZERO
         c = s/COS_Angle(i)
         c_TL = s_TL/COS_Angle(i)
         DO j = 1, nZ
           refl_TL(i,j) = (c_TL * bb(i,j) + c*bb_TL(i,j) )* COS_Weight(j)
           trans_TL(i,j) = (c_TL * ff(i,j) + c*ff_TL(i,j) )* COS_Weight(j)
           IF( i == j ) THEN
             trans_TL(i,j) = trans_TL(i,j) - optical_depth_TL/COS_Angle(i)
           END IF
         
           IF( RTV%mth_Azi == 0 ) THEN
             Thermal_C_TL = Thermal_C_TL + refl_TL(i,j) + trans_TL(i,j)
           END IF
         ENDDO
         IF( RTV%mth_Azi == 0 ) THEN
           source_up_TL(i) = -Thermal_C_TL * Planck_Func + &
             ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func_TL
           source_down_TL(i) = source_up_TL(i)
         END IF
       ENDDO
       RETURN
     END IF

     !
     ! for numerical stability, 
     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
       s_TL = single_albedo_TL
     ELSE
       s = max_albedo
       s_TL = 0.0_fp
     END IF
     !
     ! building TL phase matrices
     DO i = 1, nZ
       c = s/COS_Angle(i)
       c_TL = s_TL/COS_Angle(i)
       DO j = 1, nZ
         PM_TL(i,j) = (c_TL * bb(i,j) + c*bb_TL(i,j) ) * COS_Weight(j)
         PP_TL(i,j) = (c_TL * ff(i,j) + c*ff_TL(i,j) ) * COS_Weight(j)
       END DO
     ENDDO

     PPM_TL(:,:) = PP_TL(:,:) - PM_TL(:,:)
     i_PPM_TL(:,:) = - matmul( RTV%i_PPM(1:nZ,1:nZ,KL), matmul(PPM_TL(:,:),RTV%i_PPM(1:nZ,1:nZ,KL)) )     
     PPP_TL(:,:) = PP_TL(:,:) + PM_TL(:,:)
     HH_TL(:,:) = matmul( PPM_TL(:,:), RTV%PPP(1:nZ,1:nZ,KL) )+matmul( RTV%PPM(1:nZ,1:nZ,KL), PPP_TL(:,:) )
     !
     ! compute TL eigenvectors EigVe, and eigenvalues EigVa
     CALL ASYMTX_TL(nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL),HH_TL, &
          EigVe_TL,EigVa_TL,Error_Status)

     DO i = 1, nZ
       IF( RTV%EigVa(i,KL) > ZERO ) THEN
         EigValue_TL(i) = 0.5_fp*EigVa_TL(i)/RTV%EigValue(i,KL)
       ELSE
         EigValue_TL(i) = ZERO 
       END IF
     END DO
     EigVeVa_TL = ZERO
     
     DO i = 1, nZ
       DO j = 1, nZ
         EigVeVa_TL(i,j) = EigVe_TL(i,j) * RTV%EigValue(j,KL)+RTV%EigVe(i,j,KL) * EigValue_TL(j)
       END DO
     END DO     
     EigVeF_TL(:,:) = matmul( i_PPM_TL(:,:), RTV%EigVeVa(1:nZ,1:nZ,KL) )  &
                    + matmul( RTV%i_PPM(1:nZ,1:nZ,KL), EigVeVa_TL(:,:) )
     !  
     ! compute TL reflection and transmission matrices, TL source function     
     Gp_TL(:,:) = ( EigVe_TL(:,:) + EigVeF_TL(:,:) )/2.0_fp
     Gm_TL(:,:) = ( EigVe_TL(:,:) - EigVeF_TL(:,:) )/2.0_fp
     i_Gm_TL = -matmul( RTV%i_Gm(1:nZ,1:nZ,KL), matmul(Gm_TL,RTV%i_Gm(1:nZ,1:nZ,KL)) )
     DO i = 1, nZ
       xx_TL = EigValue_TL(i)*optical_depth+RTV%EigValue(i,KL)*optical_depth_TL
       Exp_x_TL(i) = -xx_TL*RTV%Exp_x(i,KL)
     END DO

     DO i = 1, nZ
       DO j = 1, nZ
         A1_TL(i,j) = Gp_TL(i,j)* RTV%Exp_x(j,KL)+ RTV%Gp(i,j,KL)* Exp_x_TL(j)
         A4_TL(i,j) = Gm_TL(i,j)* RTV%Exp_x(j,KL)+ RTV%Gm(i,j,KL)* Exp_x_TL(j)
       END DO
     END DO        
     A2_TL(:,:) = matmul(i_Gm_TL(:,:),RTV%A1(1:nZ,1:nZ,KL))+matmul(RTV%i_Gm(1:nZ,1:nZ,KL),A1_TL(:,:))          
     A3_TL(:,:) = matmul(Gp_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%Gp(1:nZ,1:nZ,KL),A2_TL(:,:))
     A5_TL(:,:) = matmul(A1_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%A1(1:nZ,1:nZ,KL),A2_TL(:,:))
     A6_TL(:,:) = matmul(A4_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%A4(1:nZ,1:nZ,KL),A2_TL(:,:))
  
     Gm_A5_TL(:,:) = Gm_TL(:,:) - A5_TL(:,:)
     i_Gm_A5_TL(:,:) = -matmul( RTV%i_Gm_A5(1:nZ,1:nZ,KL),matmul(Gm_A5_TL,RTV%i_Gm_A5(1:nZ,1:nZ,KL)))
     !
     ! T = matmul( RTV%A4(:,:,KL) - RTV%A3(:,:,KL), RTV%i_Gm_A5(:,:,KL) )
     trans_TL = matmul( A4_TL(:,:) - A3_TL(:,:), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )  &
          + matmul( RTV%A4(1:nZ,1:nZ,KL) - RTV%A3(1:nZ,1:nZ,KL), i_Gm_A5_TL(:,:) )
     refl_TL = matmul( Gp_TL(:,:) - A6_TL(:,:), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )  &
          + matmul( RTV%Gp(1:nZ,1:nZ,KL) - RTV%A6(1:nZ,1:nZ,KL), i_Gm_A5_TL(:,:) )

     trans(1:nZ,1:nZ) = RTV%s_Layer_Trans(1:nZ,1:nZ,KL)
     refl(1:nZ,1:nZ) = RTV%s_Layer_Refl(1:nZ,1:nZ,KL)
     Source_UP_TL = ZERO
     Source_DOWN_TL = ZERO    
     !
     ! Thermal part
     IF( RTV%mth_Azi == 0 ) THEN  
       DO i = 1, nZ
         Thermal_C_TL = ZERO
         DO j = 1, n_Streams 
           Thermal_C_TL = Thermal_C_TL + (trans_TL(i,j) + refl_TL(i,j))
         ENDDO
         IF(i == nZ .AND. nZ == (n_Streams+1)) THEN
           Thermal_C_TL = Thermal_C_TL + trans_TL(nZ,nZ)
         ENDIF
         thermal_up_TL(i) = -Thermal_C_TL * Planck_Func  &
           + ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func_TL
         thermal_down_TL(i) = thermal_up_TL(i)
       ENDDO
     END IF
     
     !
     ! for visible channels at daytime
     IF( RTV%Solar_Flag_true ) THEN 
       N2 = 2 * nZ
       N2_1 = N2 - 1
       V0 = ZERO
       V1 = ZERO
       Solar = ZERO
       Solar1 = ZERO
       Sfac2 = ZERO
       V0_TL = ZERO
       Solar_TL = ZERO
       Solar1_TL = ZERO
       Sfac2_TL = ZERO
       !
       ! Solar source  
       Sfactor = single_albedo*RTV%Solar_irradiance/PI
       Sfactor_TL = single_albedo_TL*RTV%Solar_irradiance/PI
               
       IF( RTV%mth_Azi == 0 ) THEN
         Sfactor = Sfactor/TWO
         Sfactor_TL = Sfactor_TL/TWO
       END IF
       EXPfactor = exp(-optical_depth/RTV%COS_SUN)
       EXPfactor_TL = -optical_depth_TL/RTV%COS_SUN*EXPfactor
      
       s_transmittance = exp(-total_opt/RTV%COS_SUN)
       s_transmittance_TL = -total_opt_TL/RTV%COS_SUN*s_transmittance
       
       DO i = 1, nZ     
         Solar(i) = -bb(i,nZ+1)*Sfactor
         Solar_TL(i) = -bb_TL(i,nZ+1)*Sfactor-bb(i,nZ+1)*Sfactor_TL
         Solar(i+nZ) = -ff(i,nZ+1)*Sfactor
         Solar_TL(i+nZ) = -ff_TL(i,nZ+1)*Sfactor-ff(i,nZ+1)*Sfactor_TL
         DO j = 1, nZ
           V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
           V0_TL(i,j) = single_albedo_TL*ff(i,j)*COS_Weight(j)+single_albedo*ff_TL(i,j)*COS_Weight(j)
           V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
           V0_TL(i+nZ,j) = single_albedo_TL*bb(i,j)*COS_Weight(j)+single_albedo*bb_TL(i,j)*COS_Weight(j)
           V0(i,j+nZ) = V0(i+nZ,j)
           V0_TL(i,j+nZ) = V0_TL(i+nZ,j)
           V0(nZ+i,j+nZ) = V0(i,j)
           V0_TL(nZ+i,j+nZ) = V0_TL(i,j)
         ENDDO
         V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
         V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
       ENDDO

       V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF           
       
       Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
       
       Solar(1:N2_1) =  matmul( V1(1:N2_1,1:N2_1),Solar(1:N2_1) )
       Solar1_TL(1:N2_1) = matmul( V0_TL(1:N2_1,1:N2_1),Solar(1:N2_1) )
       Solar1_TL(1:N2_1) = -matmul(  V1(1:N2_1,1:N2_1),Solar1_TL(1:N2_1) )  &
                         + matmul( V1(1:N2_1,1:N2_1), Solar_TL(1:N2_1) )
       
       Solar1(N2) = ZERO
       Solar1_TL(N2) = ZERO
       Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )
       Sfac2_TL = Solar_TL(N2) - sum( V0_TL(N2,1:N2_1)*Solar1(1:N2_1) )  &
                - sum( V0(N2,1:N2_1)*Solar1_TL(1:N2_1) )
               
       DO i = 1, nZ
         source_up(i) = Solar1(i)
         source_up_TL(i) = Solar1_TL(i)
         source_down(i) = EXPfactor*Solar1(i+nZ)
         source_down_TL(i) = EXPfactor_TL*Solar1(i+nZ)+EXPfactor*Solar1_TL(i+nZ)
         DO j = 1, nZ
           source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
           source_up_TL(i) =source_up_TL(i)-refl_TL(i,j)*Solar1(j+nZ) -refl(i,j)*Solar1_TL(j+nZ) &
           - trans_TL(i,j)*EXPfactor*Solar1(j) - trans(i,j)*EXPfactor_TL*Solar1(j) -trans(i,j)*EXPfactor*Solar1_TL(j)
           source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
           source_down_TL(i) =source_down_TL(i) -trans_TL(i,j)*Solar1(j+nZ) -trans(i,j)*Solar1_TL(j+nZ) &
           -refl_TL(i,j)*EXPfactor*Solar1(j) -refl(i,j)*EXPfactor_TL*Solar1(j) -refl(i,j)*EXPfactor*Solar1_TL(j)
         END DO
       END DO
       !
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
         source_down_TL(nZ) =source_down_TL(nZ) +(EXPfactor_TL-trans_TL(nZ,nZ))*Sfac2/V0(N2,N2) &
          +(EXPfactor-trans(nZ,nZ))*Sfac2_TL/V0(N2,N2)-(EXPfactor-trans(nZ,nZ))*Sfac2*V0_TL(N2,N2)/V0(N2,N2)/V0(N2,N2)
       ELSE
         source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
         source_down_TL(nZ) =source_down_TL(nZ) -EXPfactor_TL*Sfac2*optical_depth/COS_Angle(nZ)  &
         -EXPfactor*Sfac2_TL*optical_depth/COS_Angle(nZ)-EXPfactor*Sfac2*optical_depth_TL/COS_Angle(nZ)
       END IF
        
       ! source_up(1:nZ) = source_up(1:nZ)*s_transmittance
        source_up_TL(1:nZ) = source_up_TL(1:nZ)*s_transmittance+source_up(1:nZ)*s_transmittance_TL
       ! source_down(1:nZ) = source_down(1:nZ)*s_transmittance
        source_down_TL(1:nZ) = source_down_TL(1:nZ)*s_transmittance + source_down(1:nZ)*s_transmittance_TL
     END IF

     source_up_TL(:) = source_up_TL(:) + thermal_up_TL(:)
     source_down_TL(:) = source_down_TL(:) + thermal_down_TL(:)
    
     RETURN

     END SUBROUTINE CRTM_AMOM_layer_TL      
  
   SUBROUTINE CRTM_ADA_AD(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                              T_OD, & ! Input  layer optical depth
                 cosmic_background, & ! Input  cosmic background radiance
                        emissivity, & ! Input  surface emissivity
               direct_reflectivity, & ! surface direct reflectivity
                               RTV, & ! Input  structure containing forward results 
                       s_rad_up_AD, & ! Input  adjoint upward radiance 
              Planck_Atmosphere_AD, & ! Output AD atmospheric layer Planck radiance
                 Planck_Surface_AD, & ! Output AD surface Planck radiance
                              w_AD, & ! Output AD layer scattering albedo
                           T_OD_AD, & ! Output AD layer optical depth
                     emissivity_AD, & ! Output AD surface emissivity
                   reflectivity_AD, & ! Output AD surface reflectivity
            direct_reflectivity_AD, & ! Output AD surface direct reflectivity
                            Pff_AD, & ! Output AD forward phase matrix
                            Pbb_AD)   ! Output AD backward phase matrix
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW adjoint radiance at the top of         !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_AD algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w,T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity,direct_reflectivity
      REAL (fp), INTENT(IN) ::  cosmic_background

      REAL (fp),INTENT(INOUT),DIMENSION( :,:,: ) ::  Pff_AD, Pbb_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  w_AD,T_OD_AD
      REAL (fp),INTENT(INOUT),DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL (fp),INTENT(INOUT) ::  Planck_Surface_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  emissivity_AD,direct_reflectivity_AD
      REAL (fp),INTENT(INOUT),DIMENSION( :,: ) :: reflectivity_AD 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_AD 
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL (fp), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_AD

      REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL (fp), DIMENSION( RTV%n_Angles ) :: s_source_up_AD,s_source_down_AD,refl_down_AD
 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_trans_AD,s_refl_AD,Refl_Trans_AD
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_refl_up_AD,Inv_Gamma_AD,Inv_GammaT_AD
      REAL (fp) :: sum_s_AD, sums_AD, xx
      REAL (fp), DIMENSION(0:n_Layers) :: total_opt, total_opt_AD
      INTEGER :: i, j, k,nZ
!
      nZ = RTV%n_Angles
      
       total_opt_AD = ZERO
       total_opt(0) = ZERO
       DO k = 1, n_Layers
         total_opt(k) = total_opt(k-1) + T_OD(k)
       END DO
!
       s_trans_AD = ZERO
       Planck_Atmosphere_AD = ZERO
       Planck_Surface_AD = ZERO
       s_refl_up_AD = ZERO

      Pff_AD = ZERO
      Pbb_AD = ZERO
!      T_OD_AD = ZERO
!  Adding reflected cosmic background radiation

      DO i = 1, RTV%n_Angles 
      sum_s_AD = s_rad_up_AD(i)*cosmic_background
      DO j = 1, RTV%n_Angles 
      s_refl_up_AD(i,j) = sum_s_AD
      ENDDO
      ENDDO

!
       DO 10 k = 1, n_Layers 
       s_source_up_AD = ZERO
       s_source_down_AD = ZERO
       s_trans_AD = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

        refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

        s_refl_AD = s_refl_up_AD
        Inv_GammaT_AD = matmul(s_refl_up_AD,transpose(RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Refl_Trans_AD = matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_refl_up_AD)

        s_refl_up_AD=matmul(Refl_Trans_AD,transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_trans_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),Refl_Trans_AD)
 
        s_source_up_AD(1:RTV%n_Angles) = s_rad_up_AD(1:RTV%n_Angles)

        DO i = 1, RTV%n_Angles 
        sums_AD = s_rad_up_AD(i)
        DO j = 1, RTV%n_Angles 
        Inv_GammaT_AD(i,j)=Inv_GammaT_AD(i,j)+sums_AD*(refl_down(j,k)+RTV%s_Level_Rad_UP(j,k))
        ENDDO 
        ENDDO

        refl_down_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))
        s_rad_up_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))

        DO i = 1, RTV%n_Angles 
        sums_AD = refl_down_AD(i)
        DO j = 1, RTV%n_Angles 
        s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sums_AD*RTV%s_Layer_Source_DOWN(j,k)
        ENDDO 
        ENDDO

        s_source_down_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),refl_down_AD(:)) 

        s_trans_AD=s_trans_AD+matmul(Inv_GammaT_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Inv_Gamma_AD= matmul(transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)),Inv_GammaT_AD)

        temporal_matrix_AD= -matmul(Inv_Gamma_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        temporal_matrix_AD=matmul(transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)

        s_refl_up_AD=s_refl_up_AD-matmul(temporal_matrix_AD,transpose(RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_refl_AD=s_refl_AD-matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !

      call CRTM_AMOM_layer_AD(RTV%n_Streams,RTV%n_Angles,k,w(k),T_OD(k),total_opt(k-1)      , & !Input
         RTV%COS_Angle,RTV%COS_Weight,RTV%Pff(:,:,k),RTV%Pbb(:,:,k),RTV%Planck_Atmosphere(k), & !Input
         s_trans_AD,s_refl_AD,s_source_up_AD,s_source_down_AD,RTV,w_AD(k),T_OD_AD(k)        , &
         total_opt_AD(k-1),Pff_AD(:,:,k),Pbb_AD(:,:,k),Planck_Atmosphere_AD(k))  !Output

      ELSE
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_trans_AD(j,j)=s_trans_AD(j,j)+RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_refl_up_AD(i,j)
        s_trans_AD(i,i)=s_trans_AD(i,i)+s_refl_up_AD(i,j)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
        s_refl_up_AD(i,j)=RTV%s_Layer_Trans(i,i,k)*s_refl_up_AD(i,j)*RTV%s_Layer_Trans(j,j,k)
        ENDDO
        ENDDO
!         Adding method
       DO i = 1, RTV%n_Angles 

         s_source_up_AD(i)=s_rad_up_AD(i)
         s_trans_AD(i,i)=s_trans_AD(i,i)+s_rad_up_AD(i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
         *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k))

         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sum_s_AD*RTV%s_Layer_Source_DOWN(j,k)
         ENDDO
                                            
         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_source_down_AD(j)=s_source_down_AD(j)+sum_s_AD*RTV%s_Level_Refl_UP(i,j,k)
         ENDDO
         s_rad_up_AD(i)=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)

       ENDDO

         DO i = RTV%n_Angles, 1, -1
           s_source_up_AD(i) = s_source_up_AD(i) +  s_source_down_AD(i)
           s_trans_AD(i,i) = s_trans_AD(i,i) - RTV%Planck_Atmosphere(k) * s_source_up_AD(i)
           Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + s_source_up_AD(i) * (ONE - RTV%s_Layer_Trans(i,i,k) )
           s_source_up_AD(i) = ZERO

           T_OD_AD(k)=T_OD_AD(k)-s_trans_AD(i,i)/RTV%COS_Angle(i)*RTV%s_Layer_Trans(i,i,k)
         ENDDO

      ENDIF
   10     CONTINUE

! 

       IF( RTV%Solar_Flag_true ) THEN         
         xx = RTV%Solar_irradiance/PI * exp(-total_opt(n_Layers)/RTV%COS_SUN)
         total_opt_AD(n_Layers) = total_opt_AD(n_Layers)  &
            - xx*sum(direct_reflectivity(1:RTV%n_Angles)*s_rad_up_AD(1:RTV%n_Angles))
         direct_reflectivity_AD = direct_reflectivity_AD + s_rad_up_AD * RTV%COS_SUN * xx
       END IF 
 
        emissivity_AD = s_rad_up_AD * RTV%Planck_Surface
        Planck_Surface_AD = sum(emissivity(:) * s_rad_up_AD(:) )

     
      reflectivity_AD = s_refl_up_AD
!
       s_rad_up_AD = ZERO
       s_refl_up_AD = ZERO
 
       
       DO k = n_Layers, 1, -1
         T_OD_AD(k) = T_OD_AD(k) + total_opt_AD(k)
         total_opt_AD(k-1) = total_opt_AD(k-1) + total_opt_AD(k)
       END DO
       
      RETURN
      END SUBROUTINE CRTM_ADA_AD

!
!
     SUBROUTINE CRTM_AMOM_layer_AD(  n_streams, & ! Input, number of streams
                                            nZ, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     total_opt, & ! Input, 
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                                      trans_AD, & ! Input, layer tangent-linear trans 
                                       refl_AD, & ! Input, layer tangent-linear refl 
                                  source_up_AD, & ! Input, layer tangent-linear source_up 
                                source_down_AD, & ! Input, layer tangent-linear source_down 
                                           RTV, & ! Input, structure containing forward results 
                              single_albedo_AD, & ! Output adjoint single scattering albedo
                              optical_depth_AD, & ! Output AD layer optical depth
                                  total_opt_AD, & ! Output AD accumulated optical depth ftom TOA to current layer top
                                         ff_AD, & ! Output AD forward Phase matrix
                                         bb_AD, & ! Output AD backward Phase matrix
                                Planck_Func_AD)   ! Output AD Planck for layer temperature
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute AD layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,nZ,KL
     TYPE(RTV_type), INTENT( IN ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt

     ! internal variables
     REAL(fp) :: s, c, s_AD,c_AD,xx_AD
     INTEGER :: i,j
     INTEGER :: Error_Status

     ! Tangent-Linear Part
     REAL(fp), INTENT( INOUT ), DIMENSION( :,: ) :: trans_AD,refl_AD
     REAL(fp), INTENT( INOUT ), DIMENSION( : ) :: source_up_AD,source_down_AD
     REAL(fp), INTENT( INOUT ) :: single_albedo_AD
     REAL(fp), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD,total_opt_AD
     REAL(fp), INTENT(INOUT), DIMENSION(:,:) :: ff_AD,bb_AD

     REAL(fp), DIMENSION(nZ) :: Exp_x_AD,EigVa_AD,EigValue_AD
     REAL(fp), DIMENSION(nZ,nZ) :: i_Gm_AD, Gm_AD, Gp_AD, EigVe_AD, EigVeF_AD, EigVeVa_AD
     REAL(fp), DIMENSION(nZ,nZ) :: A1_AD,A2_AD,A3_AD,A4_AD,A5_AD,A6_AD,Gm_A5_AD,i_Gm_A5_AD
     REAL(fp), DIMENSION(nZ,nZ) :: HH_AD,PM_AD,PP_AD,PPM_AD,i_PPM_AD,PPP_AD
    
     REAL(fp), DIMENSION(nZ) :: thermal_up_AD, thermal_down_AD
     REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
     REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ) 
     REAL(fp) :: EXPfactor_AD,Sfactor_AD,s_transmittance_AD,Solar_AD(2*nZ),V0_AD(2*nZ,2*nZ),Solar1_AD(2*nZ)
     REAL(fp) :: V1_AD(2*nZ,2*nZ),Sfac2_AD
     REAL(fp), DIMENSION(nZ,nZ) :: trans, refl
     INTEGER :: N2, N2_1
     REAL(fp) :: Thermal_C_AD
     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer_AD'
     CHARACTER(256) :: Message
          
     s_AD = ZERO
     c_AD = ZERO
     !
     ! for small layer optical depth, single scattering is applied.  
     IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
       s = optical_depth * single_albedo
       DO i = 1, nZ         
         c = s/COS_Angle(i)
         IF( RTV%mth_Azi == 0 ) THEN
           source_up_AD(i) = source_up_AD(i) + source_down_AD(i)
           source_down_AD(i) = ZERO
           Planck_Func_AD = Planck_Func_AD + (ONE - RTV%Thermal_C(i,KL))*source_up_AD(i)
           Thermal_C_AD = -source_up_AD(i) * Planck_Func
         END IF

         DO j = 1, nZ   
           IF( RTV%mth_Azi == 0 ) THEN
             refl_AD(i,j) = refl_AD(i,j) + Thermal_C_AD
             trans_AD(i,j) = trans_AD(i,j) + Thermal_C_AD
           END IF
           IF( i == j ) THEN
             optical_depth_AD = optical_depth_AD - trans_AD(i,j)/COS_Angle(i)
           END IF
            
           c_AD = c_AD + trans_AD(i,j) * ff(i,j) * COS_Weight(j)
           ff_AD(i,j) = ff_AD(i,j) + c * trans_AD(i,j) * COS_Weight(j)
           c_AD = c_AD + refl_AD(i,j) * bb(i,j) * COS_Weight(j)
           bb_AD(i,j) = bb_AD(i,j) + c * refl_AD(i,j) * COS_Weight(j)
         ENDDO

         source_up_AD(i) = ZERO
         s_AD = s_AD + c_AD/COS_Angle(i)
         c_AD = ZERO
       ENDDO
       optical_depth_AD = optical_depth_AD + s_AD * single_albedo
       single_albedo_AD = single_albedo_AD + optical_depth * s_AD
       RETURN
     END IF

     trans(1:nZ,1:nZ) = RTV%s_Layer_Trans(1:nZ,1:nZ,KL)
     refl(1:nZ,1:nZ) = RTV%s_Layer_Refl(1:nZ,1:nZ,KL)

     thermal_up_AD(:) = source_up_AD(:)
     thermal_down_AD(:) = source_down_AD(:)

     IF( RTV%Solar_Flag_true ) THEN 
       N2 = 2 * nZ
       N2_1 = N2 - 1

       ! forward part  start   ********
       source_up = ZERO
       source_down = ZERO
       Solar_AD = ZERO
       Solar1_AD = ZERO
       Sfactor_AD = ZERO
       Sfac2_AD = ZERO
       EXPfactor_AD = ZERO
       s_transmittance_AD = ZERO
       V0_AD = ZERO
       V1_AD = ZERO
       !
       ! Solar source  
       Sfactor = single_albedo*RTV%Solar_irradiance/PI
       IF( RTV%mth_Azi == 0 ) Sfactor = Sfactor/TWO
       EXPfactor = exp(-optical_depth/RTV%COS_SUN)
       s_transmittance = exp(-total_opt/RTV%COS_SUN)
       
       DO i = 1, nZ     
         Solar(i) = -bb(i,nZ+1)*Sfactor
         Solar(i+nZ) = -ff(i,nZ+1)*Sfactor

         DO j = 1, nZ
           V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
           V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
           V0(i,j+nZ) = V0(i+nZ,j)
           V0(nZ+i,j+nZ) = V0(i,j)
         ENDDO
       V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
       V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
       ENDDO
       
       V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF               
       
       Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
       Solar1(N2) = ZERO
       Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )

       DO i = 1, nZ
         source_up(i) = Solar1(i)
         source_down(i) = EXPfactor*Solar1(i+nZ)
         DO j = 1, nZ
           source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
           source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
         END DO
       END DO
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
       ELSE
         source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
       END IF
        
       ! forward part end  ********      
       !
       s_transmittance_AD = s_transmittance_AD+sum (source_down(1:nZ)*source_down_AD(1:nZ) )
       source_down_AD(1:nZ) = source_down_AD(1:nZ)*s_transmittance
       s_transmittance_AD = s_transmittance_AD + sum (source_up(1:nZ)*source_up_AD(1:nZ) )
       source_up_AD(1:nZ) = source_up_AD(1:nZ)*s_transmittance
       !
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         V0_AD(N2,N2)=V0_AD(N2,N2)-(EXPfactor-trans(nZ,nZ))*Sfac2*source_down_AD(nZ)/V0(N2,N2)/V0(N2,N2)
         Sfac2_AD = Sfac2_AD+(EXPfactor-trans(nZ,nZ))*source_down_AD(nZ)/V0(N2,N2)
         EXPfactor_AD = EXPfactor_AD+source_down_AD(nZ)*Sfac2/V0(N2,N2)
         trans_AD(nZ,nZ) = trans_AD(nZ,nZ)-source_down_AD(nZ)*Sfac2/V0(N2,N2)
       ELSE
         optical_depth_AD = optical_depth_AD -EXPfactor*Sfac2*source_down_AD(nZ)/COS_Angle(nZ)
         Sfac2_AD = Sfac2_AD-EXPfactor*source_down_AD(nZ)*optical_depth/COS_Angle(nZ)
         EXPfactor_AD = EXPfactor_AD-source_down_AD(nZ)*Sfac2*optical_depth/COS_Angle(nZ)
       END IF

       DO i = nZ, 1, -1
         DO j = nZ, 1, -1
           Solar1_AD(j)=Solar1_AD(j)-refl(i,j)*EXPfactor*source_down_AD(i)
           EXPfactor_AD = EXPfactor_AD -refl(i,j)*source_down_AD(i)*Solar1(j)
           refl_AD(i,j) = refl_AD(i,j) -source_down_AD(i)*EXPfactor*Solar1(j)
           Solar1_AD(j+nZ) = Solar1_AD(j+nZ) -trans(i,j)*source_down_AD(i)
           trans_AD(i,j) = trans_AD(i,j) -source_down_AD(i)*Solar1(j+nZ)
           
           Solar1_AD(j)=Solar1_AD(j)-trans(i,j)*EXPfactor*source_up_AD(i)
           EXPfactor_AD = EXPfactor_AD - trans(i,j)*source_up_AD(i)*Solar1(j)
           trans_AD(i,j)=trans_AD(i,j) - source_up_AD(i)*EXPfactor*Solar1(j)
           Solar1_AD(j+nZ) = Solar1_AD(j+nZ) -refl(i,j)*source_up_AD(i)
           refl_AD(i,j) = refl_AD(i,j) -source_up_AD(i)*Solar1(j+nZ)
         END DO
         
         Solar1_AD(i+nZ) = Solar1_AD(i+nZ) + EXPfactor * source_down_AD(i)
         EXPfactor_AD = EXPfactor_AD + source_down_AD(i)*Solar1(i+nZ)
         Solar1_AD(i) = Solar1_AD(i) + source_up_AD(i)          
       END DO

       Solar1_AD(1:N2_1)=Solar1_AD(1:N2_1) -Sfac2_AD*V0(N2,1:N2_1)
       V0_AD(N2,1:N2_1)=V0_AD(N2,1:N2_1) -Sfac2_AD*Solar1(1:N2_1)
       Solar_AD(N2) = Solar_AD(N2) + Sfac2_AD

       Solar1_AD(N2) = ZERO
       Solar_AD(1:N2_1)=Solar_AD(1:N2_1)+matmul( transpose(V1(1:N2_1,1:N2_1)),Solar1_AD(1:N2_1) )
       Solar(1:N2_1) =  matmul( V1(1:N2_1,1:N2_1),Solar(1:N2_1) )                  
       Solar1_AD(1:N2_1) = -matmul( transpose(V1(1:N2_1,1:N2_1)),Solar1_AD(1:N2_1) ) 
       DO i = 1, N2_1
       DO j = 1, N2_1
         V0_AD(i,j)=V0_AD(i,j)+Solar1_AD(i)*Solar(j)
       END DO
       END DO        
        
       ! Solar source            
       DO i = nZ, 1, -1                
         DO j = nZ, 1, -1    
           V0_AD(i,j)=V0_AD(i,j) + V0_AD(nZ+i,j+nZ)
           V0_AD(i+nZ,j)=V0_AD(i+nZ,j) + V0_AD(i,j+nZ)
           bb_AD(i,j)=bb_AD(i,j) + single_albedo*V0_AD(i+nZ,j)*COS_Weight(j)
           single_albedo_AD=single_albedo_AD + V0_AD(i+nZ,j)*bb(i,j)*COS_Weight(j)
           ff_AD(i,j)=ff_AD(i,j) + single_albedo*V0_AD(i,j)*COS_Weight(j)
           single_albedo_AD=single_albedo_AD +V0_AD(i,j)*ff(i,j)*COS_Weight(j)
         ENDDO
       
         Sfactor_AD = Sfactor_AD -ff(i,nZ+1)*Solar_AD(i+nZ)
         ff_AD(i,nZ+1) = ff_AD(i,nZ+1) -Solar_AD(i+nZ)*Sfactor
         Sfactor_AD = Sfactor_AD -bb(i,nZ+1)*Solar_AD(i)
         bb_AD(i,nZ+1)=bb_AD(i,nZ+1) - Solar_AD(i)*Sfactor
       ENDDO
        
       total_opt_AD = total_opt_AD -s_transmittance_AD/RTV%COS_SUN*s_transmittance
       optical_depth_AD = optical_depth_AD -EXPfactor_AD/RTV%COS_SUN*EXPfactor
       
       IF( RTV%mth_Azi == 0 ) THEN
         Sfactor_AD = Sfactor_AD/TWO
       END IF
             
       single_albedo_AD = single_albedo_AD + Sfactor_AD*RTV%Solar_irradiance/PI
       
     END IF

    ! Thermal part
     IF( RTV%mth_Azi == 0 ) THEN
       DO i = nZ, 1, -1
         thermal_up_AD(i) = thermal_up_AD(i) + thermal_down_AD(i)
         thermal_down_AD(i) = ZERO
         Planck_Func_AD = Planck_Func_AD + ( ONE - RTV%Thermal_C(i,KL) ) * thermal_up_AD(i)
         Thermal_C_AD = -thermal_up_AD(i) * Planck_Func

         IF ( i == nZ .AND. nZ == (n_Streams+1) ) THEN
           trans_AD(nZ,nZ) = trans_AD(nZ,nZ) + Thermal_C_AD
         END IF
       
         DO j = n_Streams, 1, -1
           trans_AD(i,j) = trans_AD(i,j) + Thermal_C_AD
           refl_AD(i,j) = refl_AD(i,j) + Thermal_C_AD
         ENDDO
         thermal_up_AD(i) = ZERO
       ENDDO
     END IF
!     
     i_Gm_A5_AD = matmul( transpose(RTV%Gp(1:nZ,1:nZ,KL)-RTV%A6(1:nZ,1:nZ,KL)),refl_AD )
     Gp_AD(:,:) = matmul( refl_AD, transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) )       
     
     A6_AD = - GP_AD  
     i_Gm_A5_AD = i_Gm_A5_AD + matmul( transpose(RTV%A4(1:nZ,1:nZ,KL)-RTV%A3(1:nZ,1:nZ,KL)),trans_AD )
     A4_AD(:,:) = matmul( trans_AD, transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) )
     A3_AD = - A4_AD
     Gm_A5_AD = -matmul( transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) ,matmul( i_Gm_A5_AD,transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) ) )       
     Gm_AD = Gm_A5_AD
     A5_AD = - Gm_A5_AD

     A4_AD = A4_AD + matmul( A6_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = matmul( transpose(RTV%A4(1:nZ,1:nZ,KL)),A6_AD(:,:) )

     A1_AD = matmul( A5_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = A2_AD + matmul( transpose(RTV%A1(1:nZ,1:nZ,KL)), A5_AD(:,:) )

     Gp_AD = Gp_AD + matmul( A3_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = A2_AD + matmul( transpose(RTV%Gp(1:nZ,1:nZ,KL)),A3_AD(:,:) )

     i_Gm_AD = matmul( A2_AD(:,:), transpose(RTV%A1(1:nZ,1:nZ,KL)) )
     A1_AD = A1_AD + matmul( transpose(RTV%i_Gm(1:nZ,1:nZ,KL)), A2_AD(:,:) )

     Exp_x_AD = ZERO
     
     DO i = nZ, 1, -1
       DO j = nZ, 1, -1
         Gm_AD(i,j) = Gm_AD(i,j) + A4_AD(i,j)* RTV%Exp_x(j,KL)
         Exp_x_AD(j) = Exp_x_AD(j) + RTV%Gm(i,j,KL)*A4_AD(i,j)
         Gp_AD(i,j) = Gp_AD(i,j) + A1_AD(i,j)* RTV%Exp_x(j,KL)
         Exp_x_AD(j) = Exp_x_AD(j) + RTV%Gp(i,j,KL)*A1_AD(i,j)
       END DO
     END DO
    
     DO i = nZ, 1, -1
       xx_AD = -Exp_x_AD(i)*RTV%Exp_x(i,KL)
       Exp_x_AD(i) = ZERO
       EigValue_AD(i) = xx_AD*optical_depth
       optical_depth_AD = optical_depth_AD + RTV%EigValue(i,KL)*xx_AD
     END DO

     Gm_AD = Gm_AD -matmul( transpose(RTV%i_Gm(1:nZ,1:nZ,KL)), matmul( i_Gm_AD, transpose(RTV%i_Gm(1:nZ,1:nZ,KL)) ) )
 
     EigVe_AD(:,:) = Gm_AD(:,:)/2.0_fp           
     EigVeF_AD(:,:) = - Gm_AD(:,:)/2.0_fp         

     EigVe_AD = EigVe_AD + Gp_AD(:,:)/2.0_fp
     EigVeF_AD = EigVeF_AD + Gp_AD(:,:)/2.0_fp

     i_PPM_AD(:,:) = matmul( EigVeF_AD(:,:), transpose(RTV%EigVeVa(1:nZ,1:nZ,KL)) )
     EigVeVa_AD(:,:) = matmul( transpose(RTV%i_PPM(1:nZ,1:nZ,KL)), EigVeF_AD(:,:) )           

     DO i = nZ, 1, -1
       DO j = nZ, 1, -1              
         EigVe_AD(i,j)=EigVe_AD(i,j)+EigVeVa_AD(i,j)* RTV%EigValue(j,KL)
         EigValue_AD(j) = EigValue_AD(j)+RTV%EigVe(i,j,KL)*EigVeVa_AD(i,j)
       END DO
     END DO
    
     DO i = nZ, 1, -1
       IF( RTV%EigVa(i,KL) > ZERO ) THEN
         EigVa_AD(i) = 0.5_fp*EigValue_AD(i)/RTV%EigValue(i,KL)
       ELSE
         EigValue_AD(i) = ZERO
         EigVa_AD(i) = ZERO 
       END IF
     END DO

     ! compute eigenvectors EigVe, and eigenvalues EigVa
     CALL ASYMTX_AD(nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL), &
          EigVe_AD,EigVa_AD,HH_AD,Error_Status) 

     PPM_AD(:,:) = matmul( HH_AD(:,:), transpose(RTV%PPP(1:nZ,1:nZ,KL)) )
     PPP_AD(:,:) = matmul( transpose(RTV%PPM(1:nZ,1:nZ,KL)), HH_AD(:,:) )

     PP_AD = PPP_AD
     PM_AD = PPP_AD
   
     PPM_AD(:,:) = PPM_AD(:,:)-matmul( transpose(RTV%i_PPM(1:nZ,1:nZ,KL)),matmul(i_PPM_AD(:,:),transpose(RTV%i_PPM(1:nZ,1:nZ,KL))) )

     PP_AD = PP_AD + PPM_AD
     PM_AD = PM_AD - PPM_AD

     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
     ELSE
       s = max_albedo
     END IF 
     
       c_AD = ZERO
       s_AD = ZERO
     DO i = nZ, 1, -1
       c = s/COS_Angle(i) 
       DO j = nZ, 1, -1
       c_AD = c_AD + PP_AD(i,j) * ff(i,j) * COS_Weight(j)
       ff_AD(i,j) = ff_AD(i,j) + c * PP_AD(i,j) * COS_Weight(j)
       c_AD = c_AD + PM_AD(i,j) * bb(i,j) * COS_Weight(j)
       bb_AD(i,j) = bb_AD(i,j) + c * PM_AD(i,j) * COS_Weight(j)
       END DO
       s_AD = s_AD + c_AD/COS_Angle(i)
       c_AD = ZERO
     ENDDO
!
     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
       single_albedo_AD = s_AD + single_albedo_AD       
     ELSE
       s = max_albedo
       s_AD = 0.0_fp
     END IF
!       
     RETURN

     END SUBROUTINE CRTM_AMOM_layer_AD
  
  
END MODULE ADA_Module

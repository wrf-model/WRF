!
! Emission_Module
!
! Module containing the emission radiative transfer
! solution procedures in the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu, QSS at JCSDA; quanhua.liu@noaa.gov
!                       Yong Han,    NOAA/NESDIS;  yong.han@noaa.gov
!                       Paul van Delst; CIMMS/SSEC; paul.vandelst@noaa.gov
!                       08-Jun-2004

MODULE Emission_Module

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE RTV_Define
  USE CRTM_Parameters
  USE Type_Kinds
  
  IMPLICIT NONE
  
  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  
  PUBLIC CRTM_Emission
  PUBLIC CRTM_Emission_TL
  PUBLIC CRTM_Emission_AD
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: $'
  
CONTAINS

!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


  SUBROUTINE CRTM_Emission(n_Layers, & ! Input  number of atmospheric layers
                           n_Angles, & ! number angles used in SfcOptics
                    Diffuse_Surface, & ! Input  TRUE: Lambertian, FALSE: specular
                                  u, & ! Input  cosine of local viewing angle
                               T_OD, & ! Input  nadir layer optical depth
                  Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                     Planck_Surface, & ! Input  surface Planck radiance 
                         emissivity, & ! Input  surface emissivity
                       reflectivity, & ! Input  surface reflectivity matrix
                direct_reflectivity, & ! Input  reflectivity for direct irradiance 
                  cosmic_background, & ! Input  cosmic background radiance
                   Solar_irradiance, & ! Input  Solar spectral irradiance
                   Is_Solar_Channel, & ! Input  Indicate solar affected channel
               Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                RTV)   ! Output TOA radiance and others
! ----------------------------------------------------------------------------- !
!  FUNCTION: Compute IR/MW upward radiance at the top of the profile.           !
!    This code heritages the concept from previous operational code.            !
!    It starts from cosmic background downward.                                 !
!    The downward radiance at the lower level is the transmitted radiance       !
!    from upper level adding the layer downward source function.                !
!    The downward angle is either the same as satellite viewing zenith for a    !
!    specular surface or the diffuse angle for a lambertian surface. The upward !
!    radiance at the surface is the surface emission term adding from surface   !
!    reflected downward radiance. Then, the upward radiance is the sum of       !
!    from the lower level transmitted radiance adding the upward layer          !
!    source function.                                                           !
!                                                                               !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                        !
! ----------------------------------------------------------------------------- !

    ! Arguments
    INTEGER,                     INTENT(IN)     :: n_Layers
    INTEGER,                     INTENT(IN)     :: n_Angles
    LOGICAL,                     INTENT(IN)     :: Diffuse_Surface
    REAL(fp),                    INTENT(IN)     :: u
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: T_OD
    REAL(fp), DIMENSION(0:),     INTENT(IN)     :: Planck_Atmosphere
    REAL(fp),                    INTENT(IN)     :: Planck_Surface
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: emissivity
    REAL(fp), DIMENSION(:,:),    INTENT(IN)     :: reflectivity 
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: direct_reflectivity 
    REAL(fp),                    INTENT(IN)     :: cosmic_background
    REAL(fp),                    INTENT(IN)     :: Solar_irradiance
    LOGICAL,                     INTENT(IN)     :: Is_Solar_Channel
    REAL(fp),                    INTENT(IN)     :: Source_Zenith_Radian
    TYPE(RTV_type),              INTENT(IN OUT) :: RTV
    ! Local variables
    REAL(fp) :: layer_source_up, cosine_u0 
    INTEGER :: k

    ! --------------------
    ! Downwelling radiance
    ! --------------------
    ! Determing secant downward angle from surface behavior
    IF( Diffuse_Surface ) THEN
      RTV%Secant_Down_Angle = SECANT_DIFFUSIVITY 
    ELSE
      RTV%Secant_Down_Angle = ONE/u
    END IF

    ! Start from the top of the atmosphere
    RTV%e_Level_Rad_DOWN(0) = cosmic_background 
    RTV%Total_OD = ZERO

    ! Loop from top layer to bottom layer
    DO k = 1, n_Layers
      ! Accumulate optical depth 
      RTV%Total_OD = RTV%Total_OD + T_OD(k)
      ! Layer downward transmittance
      RTV%e_Layer_Trans_DOWN(k) = EXP(-T_OD(k)*RTV%Secant_Down_Angle)
      ! Downward radiance  
      RTV%e_Level_Rad_DOWN(k) = (RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)) + &
                                (Planck_Atmosphere(k)*(ONE-RTV%e_Layer_Trans_DOWN(k)))
    END DO

    ! ----------------
    ! Surface radiance
    ! ----------------
    ! upward radiance at the surface ( emission part + reflection part)
    RTV%e_Level_Rad_UP(n_Layers) = (emissivity(n_Angles)*Planck_Surface) + &
                                   (reflectivity(1,1)*RTV%e_Level_Rad_DOWN(n_Layers))

    ! Solar contribution to the upward radiance at the surface
    RTV%Down_Solar_Radiance = ZERO
    IF( Is_Solar_Channel ) THEN
      cosine_u0 = COS(Source_Zenith_Radian)
      IF( cosine_u0 > ZERO) THEN
        RTV%Down_Solar_Radiance = cosine_u0*EXP(-RTV%Total_OD/cosine_u0)*Solar_Irradiance/PI
        RTV%e_Level_Rad_UP(n_Layers) = RTV%e_Level_Rad_UP(n_Layers) + &
          (RTV%Down_Solar_Radiance*direct_reflectivity(1))
      END IF
    END IF

    ! ------------------
    ! Upwelling radiance
    ! ------------------
    ! Initialise upwelling radiance
    RTV%Up_Radiance = ZERO

    ! Loop from SFC->TOA
    DO k = n_Layers, 1, -1
      ! layer upwelling transmittance
      RTV%e_Layer_Trans_UP(k) = EXP(-T_OD(k)/u)
      ! layer upwelling source function
      layer_source_up = Planck_Atmosphere(k) * ( ONE - RTV%e_Layer_Trans_UP(k) )
      ! upwelling radiance (including reflected downwelling and surface)
      RTV%e_Level_Rad_UP(k-1) = (RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)) + &
                                layer_source_up 
      ! upwelling radiance (atmospheric portion only)
      RTV%Up_Radiance = (RTV%Up_Radiance*RTV%e_Layer_Trans_UP(k)) + layer_source_up
    END DO

  END SUBROUTINE CRTM_Emission
  
  SUBROUTINE CRTM_Emission_TL(n_Layers, & ! Input  number of atmospheric layers
                              n_Angles, & ! number angles used in SfcOptics
                                     u, & ! Input  cosine of local viewing angle
                     Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                        Planck_Surface, & ! Input  surface Planck radiance 
                            emissivity, & ! Input  surface emissivity
                          reflectivity, & ! Input  surface reflectivity matrix
                   direct_reflectivity, & ! Input  reflectivity for direct irradiance 
                      Solar_irradiance, & ! Input  Solar spectral irradiance
                      Is_Solar_Channel, & ! Input  Indicate solar affected channel 
                  Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                   RTV, & ! Input  Structure containing forward part results 
                               T_OD_TL, & ! Input  tangent-linear of layer optical depth
                  Planck_Atmosphere_TL, & ! Input  TL atmospheric layer Planck radiance
                     Planck_Surface_TL, & ! Input  TL surface Planck radiance
                         emissivity_TL, & ! Input  TL surface emissivity
                       reflectivity_TL, & ! Input  TL surface reflectivity matrix
                direct_reflectivity_TL, & ! Input  TL surface ditrct reflectivity
                             up_rad_TL)   ! Output TL TOA radiance
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute tangent-linear upward radiance at the top of the         !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles
      LOGICAL, INTENT(IN) :: Is_Solar_Channel
      REAL (fp), INTENT(IN) :: Solar_irradiance, Source_Zenith_Radian
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity,T_OD_TL,emissivity_TL
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity ,reflectivity_TL
      REAL (fp), INTENT(IN), DIMENSION( : ) :: direct_reflectivity,direct_reflectivity_TL
      REAL (fp), INTENT(IN), DIMENSION( 0: ) :: Planck_Atmosphere,Planck_Atmosphere_TL
      REAL (fp), INTENT(IN) :: Planck_Surface,u,Planck_Surface_TL
      REAL (fp), INTENT(INOUT) :: up_rad_TL

    !   Structure RTV carried in variables from forward calculation. 
      TYPE(RTV_type), INTENT( IN) :: RTV
    !  internal variables
      REAL (fp) :: layer_source_up_TL, layer_source_down_TL,a_TL,down_rad_TL
      REAL (fp) :: Total_OD, Total_OD_TL
      INTEGER :: k
      REAL( fp) :: cosine_u0

    !#--------------------------------------------------------------------------#
    !#                -- Downwelling TL radiance   --                           #
    !#--------------------------------------------------------------------------#

      down_rad_TL = ZERO 
      Total_OD_TL = ZERO
    
      Total_OD = RTV%Total_OD
 
      DO k = 1, n_Layers
       ! accumulate tangent-linear optical depth
       Total_OD_TL = Total_OD_TL + T_OD_TL(k)
       a_TL = -T_OD_TL(k) * RTV%Secant_Down_Angle

       layer_source_down_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_DOWN(k) ) &
                            - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k) * a_TL
 
     ! downward tangent-linear radiance
     !    down_rad(k) = down_rad(k-1) * layer_trans(k) + layer_source_down 
       down_rad_TL = down_rad_TL*RTV%e_Layer_Trans_DOWN(k)  &
       +RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*a_TL+layer_source_down_TL
      ENDDO

    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

      ! upward tangent-linear radiance at the surface 
       up_rad_TL =emissivity_TL(n_Angles)*Planck_Surface+emissivity(n_Angles)*Planck_Surface_TL &
       +reflectivity_TL(1,1)*RTV%e_Level_Rad_DOWN(n_Layers)+reflectivity(1,1)*down_rad_TL

      ! point source (e.g. solar radiation)
       IF( Is_Solar_Channel ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        up_rad_TL = up_rad_TL + cosine_u0*Solar_Irradiance/PI &
                  * direct_reflectivity_TL(1) * exp(-Total_OD/cosine_u0)   &
                  - Solar_Irradiance/PI * direct_reflectivity(1)    &
                  * Total_OD_TL * exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

    !#--------------------------------------------------------------------------#
    !#            -- Upwelling TL radiance   --                                 #
    !#--------------------------------------------------------------------------#

      DO k = n_Layers, 1, -1
       a_TL = -T_OD_TL(k)/u 
       layer_source_up_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_UP(k) ) &
                          - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k) * a_TL
  
      ! upward tangent linear radiance
       up_rad_TL=up_rad_TL*RTV%e_Layer_Trans_UP(k)  &
       +RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*a_TL+layer_source_up_TL 
      ENDDO
!
      RETURN
      END SUBROUTINE CRTM_Emission_TL 
!
!
      SUBROUTINE CRTM_Emission_AD(n_Layers, & ! Input  number of atmospheric layers
                                  n_Angles, & ! number angles used in SfcOptics
                                         u, & ! Input  cosine of local viewing angle
                         Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                            Planck_Surface, & ! Input  surface Planck radiance 
                                emissivity, & ! Input  surface emissivity
                              reflectivity, & ! Input  surface reflectivity matrix 
                       direct_reflectivity, & ! Input  surface reflectivity matrix 
                          Solar_irradiance, & ! Input  Solar spectral irradiance
                          Is_Solar_Channel, & ! Input  Indicate solar affected channel 
                      Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                       RTV, & ! Input  Structure containing forward part results 
                              up_rad_AD_in, & ! Input  adjoint radiance at the top
                                   T_OD_AD, & ! Output AD layer optical depth
                      Planck_Atmosphere_AD, & ! Output AD atmospheric layer Planck radiance
                         Planck_Surface_AD, & ! Output AD surface Planck radiance
                             emissivity_AD, & ! Output AD surface emissivity
                           reflectivity_AD, & ! Output AD surface reflectivity matrix
                    direct_reflectivity_AD)   ! Output AD surface direct reflectivity
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute adjoint upward radiance at the top of the                !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles
      LOGICAL, INTENT(IN) :: Is_Solar_Channel
      REAL (fp), INTENT(IN) :: Solar_Irradiance, Source_Zenith_Radian
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL (fp), INTENT(IN), DIMENSION( : ) :: direct_reflectivity 
      REAL (fp), INTENT(IN), DIMENSION( 0: ) ::  Planck_Atmosphere
      REAL (fp), INTENT(IN) :: Planck_Surface,u
      REAL (fp), INTENT(IN) :: up_rad_AD_in
      REAL (fp), INTENT(IN OUT), DIMENSION( : ) ::  T_OD_AD,emissivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( :,: ) :: reflectivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( : ) :: direct_reflectivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL (fp), INTENT(IN OUT) :: Planck_Surface_AD
      TYPE(RTV_type), INTENT( IN) :: RTV
    !  internal variables
      REAL (fp) :: layer_source_up_AD, layer_source_down_AD,a_AD,down_rad_AD
      REAL (fp) :: cosine_u0, up_rad_AD, Total_OD, Total_OD_AD
      INTEGER :: k
!
    ! Initialize variables
      Total_OD_AD = ZERO
      T_OD_AD = ZERO
      Planck_Atmosphere_AD = ZERO
      Planck_Surface_AD = ZERO
      emissivity_AD = ZERO
      reflectivity_AD = ZERO
      direct_reflectivity_AD = ZERO
      up_rad_AD = up_rad_AD_in

    ! Total column optical depth carried from forward part
      Total_OD = RTV%Total_OD 

    !#--------------------------------------------------------------------------#
    !#                -- Upwelling adjoint radiance   --                        #
    !#--------------------------------------------------------------------------#
!
      DO k = 1, n_Layers
       a_AD = RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*up_rad_AD
       layer_source_up_AD = up_rad_AD
       up_rad_AD = up_rad_AD * RTV%e_Layer_Trans_UP(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + &
              layer_source_up_AD * (ONE - RTV%e_Layer_Trans_UP(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k)* layer_source_up_AD
 
       T_OD_AD(k) = T_OD_AD(k) - a_AD/u 
      ENDDO
    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

       IF( Is_Solar_Channel ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        Total_OD_AD = -Solar_Irradiance/PI * direct_reflectivity(1) &
                    * up_rad_AD * exp(-Total_OD/cosine_u0)
        direct_reflectivity_AD(1) = cosine_u0 * Solar_Irradiance/PI &
                    * up_rad_AD* exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

      emissivity_AD(n_Angles)=up_rad_AD*Planck_Surface
      Planck_Surface_AD = emissivity(n_Angles)*up_rad_AD
      reflectivity_AD(1,1)=up_rad_AD*RTV%e_Level_Rad_DOWN(n_Layers)
      down_rad_AD = reflectivity(1,1)*up_rad_AD
!
    !#--------------------------------------------------------------------------#
    !#                -- Downward adjoint radiance   --                         #
    !#--------------------------------------------------------------------------#
      DO k = n_Layers, 1, -1

       a_AD = RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*down_rad_AD
       layer_source_down_AD = down_rad_AD
       down_rad_AD = down_rad_AD*RTV%e_Layer_Trans_DOWN(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + layer_source_down_AD * &
                                 (ONE - RTV%e_Layer_Trans_DOWN(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k)* layer_source_down_AD
 

       T_OD_AD(k) = T_OD_AD(k) - a_AD * RTV%Secant_Down_Angle

       T_OD_AD(k) = T_OD_AD(k) + Total_OD_AD
      ENDDO

      down_rad_AD = ZERO 

      RETURN
      END SUBROUTINE CRTM_Emission_AD 
  
END MODULE Emission_Module  

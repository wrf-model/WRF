!
! CRTM_AntCorr
!
! Module containgin routines to apply the antenna correction to the 
! RTSolution brightness temperatures.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jul-2007
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_AntCorr

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds              , ONLY: fp
  USE CRTM_Parameters         , ONLY: SET, ZERO, TSPACE
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_GetValue
  USE CRTM_RTSolution_Define  , ONLY: CRTM_RTSolution_type
  USE CRTM_SpcCoeff           , ONLY: SC
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: CRTM_Compute_AntCorr
  PUBLIC :: CRTM_Compute_AntCorr_TL
  PUBLIC :: CRTM_Compute_AntCorr_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AntCorr.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'
  

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  SUBROUTINE CRTM_Compute_AntCorr( gI, &  ! Input
                                   n , &  ! Input
                                   l , &  ! Input
                                   RT  )  ! In/Output
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: gI
    INTEGER                     , INTENT(IN)     :: n  ! SensorIndex
    INTEGER                     , INTENT(IN)     :: l  ! ChannelIndex
    TYPE(CRTM_RTSolution_type)  , INTENT(IN OUT) :: RT
    ! Local variables
    INTEGER :: iFOV
    
    ! Get the FOV index value
    CALL CRTM_GeometryInfo_GetValue( gI, iFOV = iFOV )
    
    ! Compute the antenna temperature
    RT%Brightness_Temperature = SC(n)%AC%A_earth(   iFOV,l)*RT%Brightness_Temperature + &
                                SC(n)%AC%A_platform(iFOV,l)*RT%Brightness_Temperature + &
                                SC(n)%AC%A_space(   iFOV,l)*TSPACE
    
  END SUBROUTINE CRTM_Compute_AntCorr


  SUBROUTINE CRTM_Compute_AntCorr_TL( gI   , &  ! Input
                                      n    , &  ! Input
                                      l    , &  ! Input
                                      RT_TL  )  ! In/Output
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: gI
    INTEGER                     , INTENT(IN)     :: n  ! SensorIndex
    INTEGER                     , INTENT(IN)     :: l  ! ChannelIndex
    TYPE(CRTM_RTSolution_type)  , INTENT(IN OUT) :: RT_TL
    ! Local variables
    INTEGER :: iFOV
    REAL(fp) :: c

    ! Get the FOV index value
    CALL CRTM_GeometryInfo_GetValue( gI, iFOV = iFOV )
    
    ! Compute the tangent linear antenna temperature
    ! Note the A_platform term has to be included even
    ! though the earth temperature is used as a proxy
    ! for the platform temperature.
    c = SC(n)%AC%A_earth(iFOV,l) + SC(n)%AC%A_platform(iFOV,l)
    RT_TL%Brightness_Temperature = c * RT_TL%Brightness_Temperature
    
  END SUBROUTINE CRTM_Compute_AntCorr_TL


  SUBROUTINE CRTM_Compute_AntCorr_AD( gI   , &  ! Input
                                      n    , &  ! Input
                                      l    , &  ! Input
                                      RT_AD  )  ! In/Output
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: gI
    INTEGER                     , INTENT(IN)     :: n  ! SensorIndex
    INTEGER                     , INTENT(IN)     :: l  ! ChannelIndex
    TYPE(CRTM_RTSolution_type)  , INTENT(IN OUT) :: RT_AD
    ! Local variables
    INTEGER :: iFOV
    REAL(fp) :: c

    ! Get the FOV index value
    CALL CRTM_GeometryInfo_GetValue( gI, iFOV = iFOV )
    
    ! Compute the adjoint of the antenna temperature
    c = SC(n)%AC%A_earth(iFOV,l) + SC(n)%AC%A_platform(iFOV,l)
    RT_AD%Brightness_Temperature = c * RT_AD%Brightness_Temperature
    
  END SUBROUTINE CRTM_Compute_AntCorr_AD

END MODULE CRTM_AntCorr

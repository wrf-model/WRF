!
! CRTM_AntennaCorrection
!
! Module containgin routines to apply the antenna correction to the 
! RTSolution brightness temperatures.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jul-2007
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_AntennaCorrection

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds              , ONLY: fp
  USE CRTM_Parameters         , ONLY: TSPACE
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
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_AntennaCorrection.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_AntCorr
!
! PURPOSE:
!       Subroutine to compute the antenna correction and apply it the RTSolution
!       brightness temperatures
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AntCorr( GeometryInfo, &
!                                  SensorIndex , &
!                                  ChannelIndex, &
!                                  RTSolution    )
!
! INPUTS:
!       GeometryInfo:    Structure containing the view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
! OUTPUTS:
!       RTSolution:      Structure containing the radiative transfer solution.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

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
    ! Note the earth temperature is used as a proxy
    ! for the platform temperature.
    RT%Brightness_Temperature = SC(n)%AC%A_earth(   iFOV,l)*RT%Brightness_Temperature + &
                                SC(n)%AC%A_platform(iFOV,l)*RT%Brightness_Temperature + &
                                SC(n)%AC%A_space(   iFOV,l)*TSPACE
    
  END SUBROUTINE CRTM_Compute_AntCorr


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_AntCorr_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear antenna correction and
!       apply it to the tangent-linear RTSolution brightness temperatures.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AntCorr_TL( GeometryInfo, &
!                                     SensorIndex , &
!                                     ChannelIndex, &
!                                     RTSolution_TL )
!
! INPUTS:
!       GeometryInfo:    Structure containing the view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
! OUTPUTS:
!       RTSolution_TL:   Structure containing the tangent-linear radiative
!                        transfer solution.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

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


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_AntCorr_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint antenna correction and
!       apply it to the adjoint RTSolution brightness temperatures.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AntCorr_AD( GeometryInfo, &
!                                     SensorIndex , &
!                                     ChannelIndex, &
!                                     RTSolution_AD )
!
! INPUTS:
!       GeometryInfo:    Structure containing the view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
! OUTPUTS:
!       RTSolution_AD:   Structure containing the adjoint radiative
!                        transfer solution.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!------------------------------------------------------------------------------

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

END MODULE CRTM_AntennaCorrection

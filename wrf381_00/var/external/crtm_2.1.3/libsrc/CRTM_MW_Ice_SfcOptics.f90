!
! CRTM_MW_Ice_SfcOptics
!
! Module to compute the surface optical properties for ICE surfaces at
! microwave frequencies required for determining the ICE surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Jun-2005
!                       paul.vandelst@noaa.gov
!
!       Modified by:    Banghua Yan, 03-Oct-2007
!                       Banghua.Yan@noaa.gov
!

MODULE CRTM_MW_Ice_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                   ONLY: fp
  USE Message_Handler,              ONLY: SUCCESS
  USE CRTM_Parameters,              ONLY: ZERO, ONE
  USE CRTM_SpcCoeff,                ONLY: SC
  USE CRTM_Surface_Define,          ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define,     ONLY: CRTM_GeometryInfo_type, &
                                          CRTM_GeometryInfo_GetValue
  USE CRTM_SfcOptics_Define,        ONLY: CRTM_SfcOptics_type
  USE CRTM_SensorInfo,              ONLY: WMO_AMSUA, &
                                          WMO_AMSUB, &
                                          WMO_AMSRE, &
                                          WMO_SSMI , &
                                          WMO_MSU  , &
                                          WMO_MHS  , &
                                          WMO_SSMIS
  USE NESDIS_AMSU_SICEEM_Module,    ONLY: NESDIS_ICEEM_AMSU
  USE NESDIS_AMSRE_SICEEM_Module,   ONLY: NESDIS_AMSRE_SSICEEM
  USE NESDIS_SSMI_SICEEM_Module,    ONLY: NESDIS_SSMI_SIceEM
  USE NESDIS_SEAICE_PHYEM_Module,   ONLY: NESDIS_SIce_Phy_EM
  USE NESDIS_MHS_SICEEM_Module,     ONLY: NESDIS_ICEEM_MHS
  USE NESDIS_SSMIS_SeaIceEM_Module, ONLY: NESDIS_SSMIS_IceEM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_MW_Ice_SfcOptics
  PUBLIC :: Compute_MW_Ice_SfcOptics_TL
  PUBLIC :: Compute_MW_Ice_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_MW_Ice_SfcOptics.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE iVar_type


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Ice_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Ice_SfcOptics( &
!                        Surface     , &
!                        GeometryInfo, &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        SfcOptics     )
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
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
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Ice_SfcOptics( &
    Surface     , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   ) &  ! Output
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Ice_SfcOptics'
    REAL(fp),     PARAMETER :: DEFAULT_EMISSIVITY = 0.92_fp
    REAL(fp),     PARAMETER :: NOT_USED(4) = -99.9_fp
    INTEGER,      PARAMETER :: AMSRE_V_INDEX(6) = (/1, 3, 5, 7,  9, 11/) ! AMSRE channels with V pol.
    INTEGER,      PARAMETER :: AMSRE_H_INDEX(6) = (/2, 4, 6, 8, 10, 12/) ! AMSRE channels with H pol.
    INTEGER,      PARAMETER :: AMSUA_INDEX(4)   = (/1, 2, 3, 15/)
    INTEGER,      PARAMETER :: SSMIS_INDEX(8)   = (/13,12,14,16,15,17,18,8/)  ! With swapped polarisations
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Sensor_Zenith_Angle


    ! Set up
    Error_Status = SUCCESS
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, Sensor_Zenith_Angle = Sensor_Zenith_Angle )


    ! Compute the surface emissivities
    Sensor_Type: SELECT CASE( Surface%SensorData%WMO_Sensor_ID )

      ! AMSU-A emissivity model
      CASE( WMO_AMSUA )
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_ICEEM_AMSU( Sensor_Zenith_Angle,                     &  ! Input, Degree
                                  SfcOptics%Angle(i),                      &  ! Input, Degree
                                  SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                  Surface%Ice_Temperature,                 &  ! Input, K
                                  Surface%SensorData%Tb(AMSUA_INDEX),      &  ! Input, AMSUA
                                  NOT_USED(1:2),                           &  ! Input, AMSUB  *** NO AMSU-B DATA ***
                                  SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                  SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! AMSU-B emissivity model
      CASE( WMO_AMSUB )
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_ICEEM_AMSU( Sensor_Zenith_Angle,                     &  ! Input, Degree
                                  SfcOptics%Angle(i),                      &  ! Input, Degree
                                  SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                  Surface%Ice_Temperature,                 &  ! Input, K
                                  NOT_USED,                                &  ! Input  AMSUA  *** NO AMSU-A DATA ***
                                  Surface%SensorData%Tb(1:2),              &  ! Input, AMSUB
                                  SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                  SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! MHS emissivity model
      CASE (WMO_MHS)
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_ICEEM_MHS( Sensor_Zenith_Angle,                     &  ! Input, Degree
                                 SfcOptics%Angle(i),                      &  ! Input, Degree
                                 SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                 Surface%Ice_Temperature,                 &  ! Input, K
                                 Surface%SensorData%Tb(1:2),              &  ! Input, MHS
                                 SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                 SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! AMSR-E emissivity model
      CASE( WMO_AMSRE )
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_AMSRE_SSICEEM( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                     SfcOptics%Angle(i),                      &  ! Input, Degree
                                     Surface%SensorData%Tb(AMSRE_V_INDEX),    &  ! Input, Tb_V, K
                                     Surface%SensorData%Tb(AMSRE_H_INDEX),    &  ! Input, Tb_H, K
                                     Surface%Ice_Temperature,                 &  ! Input, Ts, K
                                     Surface%Ice_Temperature,                 &  ! Input, Tice, K
                                     SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                     SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! SSM/I emissivity model
      CASE( WMO_SSMI )
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_SSMI_SIceEM( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                   SfcOptics%Angle(i),                      &  ! Input, Degree
                                   Surface%Ice_Temperature,                 &  ! Input, K
                                   Surface%SensorData%Tb,                   &  ! Input, K
                                   Surface%Ice_Thickness,                   &  ! Input, mm
                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                   SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! SSMIS emissivity model
      CASE( WMO_SSMIS )
        DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_SSMIS_IceEM( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                   SfcOptics%Angle(i),                      &  ! Input, Degree
                                   Surface%Ice_Temperature,                 &  ! Input, K
                                   Surface%SensorData%Tb(SSMIS_INDEX),      &  ! Input, K
                                   Surface%Ice_Thickness,                   &  ! Input, mm
                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                   SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! Default physical model
      CASE DEFAULT
        DO i = 1, SfcOptics%n_Angles
!          CALL NESDIS_SIce_Phy_EM( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
!                                   SfcOptics%Angle(i),                      &  ! Input, Degree
!                                   Surface%Ice_Temperature,                 &  ! Input, K
!                                   Surface_Dummy%Salinity,                  &  ! Input
!                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component
!                                   SfcOptics%Emissivity(i,1)                )  ! Output, V component
          SfcOptics%Emissivity(i,1:2) = DEFAULT_EMISSIVITY
        END DO

    END SELECT Sensor_Type


    ! Compute the surface reflectivities,
    ! assuming a specular surface
    SfcOptics%Reflectivity = ZERO
    DO i = 1, SfcOptics%n_Angles
      SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
      SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
    END DO

  END FUNCTION Compute_MW_Ice_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Ice_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO TL
!           COMPONENTS IN THE MW ICE SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Ice_SfcOptics_TL( SfcOptics_TL )
!
! OUTPUTS:
!       SfcOptics_TL:    Structure containing the tangent-linear surface
!                        optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Ice_SfcOptics_TL( &
    SfcOptics_TL) &  ! TL  Output
  RESULT ( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Ice_SfcOptics_TL'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the tangent-linear surface optical parameters
    ! ***No TL models yet, so default TL output is zero***
    SfcOptics_TL%Reflectivity = ZERO
    SfcOptics_TL%Emissivity   = ZERO

  END FUNCTION Compute_MW_Ice_SfcOptics_TL



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Ice_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO AD
!           COMPONENTS IN THE MW ICE SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Ice_SfcOptics_AD( SfcOptics_AD )
!
! INPUTS:
!       SfcOptics_AD:    Structure containing the adjoint surface optical
!                        properties required for the adjoint radiative
!                        transfer calculation.
!                        *** COMPONENTS MODIFIED UPON OUTPUT ***
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input adjoint arguments are IN OUT regardless
!       of their specification as "input" or "output". This is because these
!       arguments may contain information on input, or need to be zeroed on
!       output (or both).
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Ice_SfcOptics_AD( &
    SfcOptics_AD) &  ! AD  Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Ice_SfcOptics_AD'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the adjoint surface optical parameters
    ! ***No AD models yet, so there is no impact on AD result***
    SfcOptics_AD%Reflectivity = ZERO
    SfcOptics_AD%Emissivity   = ZERO

  END FUNCTION Compute_MW_Ice_SfcOptics_AD

END MODULE CRTM_MW_Ice_SfcOptics

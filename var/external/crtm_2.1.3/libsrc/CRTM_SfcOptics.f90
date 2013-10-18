!
! CRTM_SfcOptics
!
! Module to compute the surface optical properties required for
! determining the surface contribution to the radiative transfer.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       02-Apr-2004
!

MODULE CRTM_SfcOptics

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, POINT_5, ONE, DEGREES_TO_RADIANS, MAX_N_STOKES
  USE CRTM_SpcCoeff,            ONLY: SC, &
                                      SpcCoeff_IsMicrowaveSensor  , &
                                      SpcCoeff_IsInfraredSensor   , &
                                      SpcCoeff_IsVisibleSensor    , &
                                      SpcCoeff_IsUltravioletSensor, &
                                      UNPOLARIZED, &
                                      INTENSITY, &
                                      FIRST_STOKES_COMPONENT, &
                                      SECOND_STOKES_COMPONENT, &
                                      THIRD_STOKES_COMPONENT, &
                                      FOURTH_STOKES_COMPONENT, &
                                      VL_POLARIZATION, &
                                      HL_POLARIZATION, &
                                      plus45L_POLARIZATION, &
                                      minus45L_POLARIZATION, &
                                      VL_MIXED_POLARIZATION, &
                                      HL_MIXED_POLARIZATION, &
                                      RC_POLARIZATION, &
                                      LC_POLARIZATION
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type      , &
                                      OPERATOR(==)             , &
                                      CRTM_SfcOptics_Associated, &
                                      CRTM_SfcOptics_Destroy   , &
                                      CRTM_SfcOptics_Create
  USE CRTM_MW_Land_SfcOptics,   ONLY: MWLSOVar_type => iVar_type, &
                                      Compute_MW_Land_SfcOptics, &
                                      Compute_MW_Land_SfcOptics_TL, &
                                      Compute_MW_Land_SfcOptics_AD
  USE CRTM_MW_Water_SfcOptics,  ONLY: MWWSOVar_type => iVar_type   , &
                                      Compute_MW_Water_SfcOptics   , &
                                      Compute_MW_Water_SfcOptics_TL, &
                                      Compute_MW_Water_SfcOptics_AD
  USE CRTM_MW_Snow_SfcOptics,   ONLY: MWSSOVar_type => iVar_type, &
                                      Compute_MW_Snow_SfcOptics, &
                                      Compute_MW_Snow_SfcOptics_TL, &
                                      Compute_MW_Snow_SfcOptics_AD
  USE CRTM_MW_Ice_SfcOptics,    ONLY: MWISOVar_type => iVar_type, &
                                      Compute_MW_Ice_SfcOptics, &
                                      Compute_MW_Ice_SfcOptics_TL, &
                                      Compute_MW_Ice_SfcOptics_AD
  USE CRTM_IR_Land_SfcOptics,   ONLY: IRLSOVar_type => iVar_type, &
                                      Compute_IR_Land_SfcOptics, &
                                      Compute_IR_Land_SfcOptics_TL, &
                                      Compute_IR_Land_SfcOptics_AD
  USE CRTM_IR_Water_SfcOptics,  ONLY: IRWSOVar_type => iVar_type, &
                                      Compute_IR_Water_SfcOptics, &
                                      Compute_IR_Water_SfcOptics_TL, &
                                      Compute_IR_Water_SfcOptics_AD
  USE CRTM_IR_Snow_SfcOptics,   ONLY: IRSSOVar_type => iVar_type, &
                                      Compute_IR_Snow_SfcOptics, &
                                      Compute_IR_Snow_SfcOptics_TL, &
                                      Compute_IR_Snow_SfcOptics_AD
  USE CRTM_IR_Ice_SfcOptics,    ONLY: IRISOVar_type => iVar_type, &
                                      Compute_IR_Ice_SfcOptics, &
                                      Compute_IR_Ice_SfcOptics_TL, &
                                      Compute_IR_Ice_SfcOptics_AD
  USE CRTM_VIS_Land_SfcOptics,  ONLY: VISLSOVar_type => iVar_type, &
                                      Compute_VIS_Land_SfcOptics, &
                                      Compute_VIS_Land_SfcOptics_TL, &
                                      Compute_VIS_Land_SfcOptics_AD
  USE CRTM_VIS_Water_SfcOptics, ONLY: VISWSOVar_type => iVar_type, &
                                      Compute_VIS_Water_SfcOptics, &
                                      Compute_VIS_Water_SfcOptics_TL, &
                                      Compute_VIS_Water_SfcOptics_AD
  USE CRTM_VIS_Snow_SfcOptics,  ONLY: VISSSOVar_type => iVar_type, &
                                      Compute_VIS_Snow_SfcOptics, &
                                      Compute_VIS_Snow_SfcOptics_TL, &
                                      Compute_VIS_Snow_SfcOptics_AD
  USE CRTM_VIS_Ice_SfcOptics,   ONLY: VISISOVar_type => iVar_type, &
                                      Compute_VIS_Ice_SfcOptics, &
                                      Compute_VIS_Ice_SfcOptics_TL, &
                                      Compute_VIS_Ice_SfcOptics_AD
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: CRTM_Compute_SurfaceT
  PUBLIC :: CRTM_Compute_SurfaceT_TL
  PUBLIC :: CRTM_Compute_SurfaceT_AD
  PUBLIC :: CRTM_Compute_SfcOptics
  PUBLIC :: CRTM_Compute_SfcOptics_TL
  PUBLIC :: CRTM_Compute_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SfcOptics.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Microwave
    TYPE(MWLSOVar_type)  :: MWLSOV ! Land
    TYPE(MWWSOVar_type)  :: MWWSOV ! Water
    TYPE(MWSSOVar_type)  :: MWSSOV ! Snow
    TYPE(MWISOVar_type)  :: MWISOV ! Ice
    ! Infrared
    TYPE(IRLSOVar_type)  :: IRLSOV ! Land
    TYPE(IRWSOVar_type)  :: IRWSOV ! Water
    TYPE(IRSSOVar_type)  :: IRSSOV ! Snow
    TYPE(IRISOVar_type)  :: IRISOV ! Ice
    ! Visible
    TYPE(VISLSOVar_type) :: VISLSOV ! Land
    TYPE(VISWSOVar_type) :: VISWSOV ! Water
    TYPE(VISSSOVar_type) :: VISSSOV ! Snow
    TYPE(VISISOVar_type) :: VISISOV ! Ice
  END TYPE iVar_type


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_SurfaceT
!
! PURPOSE:
!       Subroutine to compute the average of the various surface type
!       temperatures weighted by their coverage fraction.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT( Surface,  &  ! Input
!                                   SfcOptics )  ! Output
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT( Surface,  &  ! Input
                                    SfcOptics )  ! Output
    ! Arguments
    TYPE(CRTM_Surface_type),   INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics

    ! The weighted average surface temperature
    SfcOptics%Surface_Temperature = &
      ( Surface%Land_Coverage  * Surface%Land_Temperature  ) + &
      ( Surface%Water_Coverage * Surface%Water_Temperature ) + &
      ( Surface%Snow_Coverage  * Surface%Snow_Temperature  ) + &
      ( Surface%Ice_Coverage   * Surface%Ice_Temperature   )

  END SUBROUTINE CRTM_Compute_SurfaceT


!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_SurfaceT_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear average of the various
!       surface type temperatures weighted by their coverage fraction.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT_TL( Surface,     &  ! Input
!                                      Surface_TL,  &  ! Input
!                                      SfcOptics_TL )  ! In/Output
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linerar
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT_TL( Surface,     &  ! Input
                                       Surface_TL,  &  ! Input
                                       SfcOptics_TL )  ! Output
    ! Arguments
    TYPE(CRTM_Surface_type),   INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),   INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL

    ! The weighted average tangent-linear surface temperature
    SfcOptics_TL%Surface_Temperature = &
      ( Surface%Land_Coverage  * Surface_TL%Land_Temperature  ) + &
      ( Surface%Water_Coverage * Surface_TL%Water_Temperature ) + &
      ( Surface%Snow_Coverage  * Surface_TL%Snow_Temperature  ) + &
      ( Surface%Ice_Coverage   * Surface_TL%Ice_Temperature   )

  END SUBROUTINE CRTM_Compute_SurfaceT_TL


!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_SurfaceT_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the average of the various
!       surface type temperatures weighted by their coverage fraction.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT_AD( Surface,      &  ! Input
!                                      SfcOptics_AD, &  ! Input
!                                      Surface_AD    )  ! Output
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Even though the SfcOptics_AD argument is listed as an INPUT, its
!       INTENT is ( IN OUT ) as it is modified on output since the
!       Surface_Temperature component is set to zero after the adjoint
!       calculation.
!
!       Even though the Surface_AD argument is listed as an OUTPUT, its
!       INTENT is ( IN OUT ) as the components of the adjoint calculation
!       in this routine may already have a value from a previous adjoint
!       calculation performed on the structure.
!
! COMMENTS:
!       In addition to the input/output requirements described in the SIDE
!       EFFECTS section, the SfcOptics_AD and Surface_AD arguments require
!       an INTENT of IN OUT to prevent memory leaks.
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT_AD( Surface,      &  ! Input
                                       SfcOptics_AD, &  ! Input
                                       Surface_AD    )  ! Output
    ! Arguments
    TYPE(CRTM_Surface_type),   INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_Surface_type),   INTENT(IN OUT) :: Surface_AD

    ! The adjoint of the weighted average surface temperature
    Surface_AD%Land_Temperature  = Surface_AD%Land_Temperature + &
                                   (Surface%Land_Coverage *SfcOptics_AD%Surface_Temperature)
    Surface_AD%Water_Temperature = Surface_AD%Water_Temperature + &
                                   (Surface%Water_Coverage*SfcOptics_AD%Surface_Temperature)
    Surface_AD%Snow_Temperature  = Surface_AD%Snow_Temperature  + &
                                   (Surface%Snow_Coverage *SfcOptics_AD%Surface_Temperature)
    Surface_AD%Ice_Temperature   = Surface_AD%Ice_Temperature   + &
                                   (Surface%Ice_Coverage  *SfcOptics_AD%Surface_Temperature)
    SfcOptics_AD%Surface_Temperature = ZERO

  END SUBROUTINE CRTM_Compute_SurfaceT_AD


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_SfcOptics
!
! PURPOSE:
!       Function to compute the surface optical properties and populate
!       the output SfcOptics structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics( &
!                        Surface     , &  ! Input
!                        GeometryInfo, &  ! Input
!                        SensorIndex , &  ! Input
!                        ChannelIndex, &  ! Input
!                        SfcOptics   , &  ! Output
!                        iVar          )  ! Internal variable output
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
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument should be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics( &
    Surface     , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   , &  ! Output
    iVar        ) &  ! Internal variable output
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type)     , INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER                     , INTENT(IN)     :: SensorIndex
    INTEGER                     , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type)   , INTENT(IN OUT) :: SfcOptics
    TYPE(iVar_type)             , INTENT(OUT)    :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity
    INTEGER :: Polarization


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    Polarization = SC(SensorIndex)%Polarization(ChannelIndex)
    ! Initialise the local emissivity and reflectivities
    Emissivity   = ZERO
    Reflectivity = ZERO
    Direct_Reflectivity = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      Sensor_Select: IF ( SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------
        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics( &
                           Surface     , &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics     )  ! In/Output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW land SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on land coverage fraction
          Emissivity(1:nZ,1:2)            = SfcOptics%Emissivity(1:nZ,1:2)            * Surface%Land_Coverage
          Reflectivity(1:nZ,1:2,1:nZ,1:2) = SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Land_Coverage

        END IF Microwave_Land


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------
        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Water_SfcOptics( &
                           Surface     , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics   , &  ! In/Output
                           iVar%MWWSOV   )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW water SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF


          ! Accumulate the surface optics properties
          ! based on water coverage fraction
          Emissivity(1:nZ,1:2) = Emissivity(1:nZ,1:2) + &
            (SfcOptics%Emissivity(1:nZ,1:2)*Surface%Water_Coverage)
          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2)*Surface%Water_Coverage)

         END IF Microwave_Water


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------
        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Snow_SfcOptics( &
                           Surface     , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics     )  ! In/Output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW snow SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity(1:nZ,1:2) = Emissivity(1:nZ,1:2) + &
            (SfcOptics%Emissivity(1:nZ,1:2)*Surface%Snow_Coverage)
          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2)*Surface%Snow_Coverage)

        END IF Microwave_Snow


        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------
        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Ice_SfcOptics( &
                           Surface     , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics     )  ! In/Output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW ice SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity(1:nZ,1:2)            = Emissivity(1:nZ,1:2) + &
                                            (SfcOptics%Emissivity(1:nZ,1:2)*Surface%Ice_Coverage)
          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
                                            (SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2)*Surface%Ice_Coverage)

        END IF Microwave_Ice



        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#

        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------
          Polarization_Type: SELECT CASE( Polarization )

            ! The unpolarised case, I
            ! e = (eV + eH)/2
            ! r = (rV + rH)/2
            ! Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )
              SfcOptics%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity(1:nZ,1) + Emissivity(1:nZ,2) )
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity(1:nZ,1,1:nZ,1) + Reflectivity(1:nZ,2,1:nZ,2) )

            ! The second Stokes component, Q, the polarisation difference.
            ! e = (eV - eH)/2
            ! r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT )
              SfcOptics%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity(1:nZ,1) - Emissivity(1:nZ,2) )
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity(1:nZ,1,1:nZ,1) - Reflectivity(1:nZ,2,1:nZ,2) )

            ! The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,3)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,3,1:nZ,3)

            ! The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,4)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,4,1:nZ,4)

            ! Vertical linear polarisation
            CASE ( VL_POLARIZATION )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)

            ! Horizontal linear polarisation
            CASE ( HL_POLARIZATION )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,2)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,2,1:nZ,2)

            ! +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION )

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)

            ! -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)

            ! Vertical, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! vertical, but the instrument scans cross-track.
            ! e = eV * (1-SIN^2(z))  +  eH * SIN^2(z)
            ! r = rV * (1-SIN^2(z))  +  rH * SIN^2(z)
            CASE ( VL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                SfcOptics%Emissivity(i,1) = (Emissivity(i,1)*(ONE-SIN2_Angle)) + &
                                            (Emissivity(i,2)*SIN2_Angle)
                SfcOptics%Reflectivity(i,1,i,1) = (Reflectivity(i,1,i,1)*(ONE-SIN2_Angle)) + &
                                                  (Reflectivity(i,2,i,2)*SIN2_Angle)
              END DO

            ! Horizontal, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! horizontal, but the instrument scans cross-track.
            ! e = eV * SIN^2(z)  +  eH * (1-SIN^2(z))
            ! r = rV * SIN^2(z)  +  rH * (1-SIN^2(z))
            CASE ( HL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                SfcOptics%Emissivity(i,1) = (Emissivity(i,1)*SIN2_Angle) + &
                                            (Emissivity(i,2)*(ONE-SIN2_Angle))
                SfcOptics%Reflectivity(i,1,i,1) = (Reflectivity(i,1,i,1)*SIN2_Angle) + &
                                                  (Reflectivity(i,2,i,2)*(ONE-SIN2_Angle))
              END DO

            ! Right circular polarisation
            CASE ( RC_POLARIZATION )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)

            ! Left circular polarisation
            CASE ( LC_POLARIZATION )
              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)

            ! Serious problem if we got to this points
            CASE DEFAULT
               Error_Status = FAILURE
               WRITE( Message,'("Unrecognised polarization flag for microwave ",&
                               &"channel index ",i0)' ) ChannelIndex
               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
               RETURN

           END SELECT Polarization_Type

        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------
          SfcOptics%Emissivity(1:nZ,1:nL)             = Emissivity(1:nZ,1:nL)
          SfcOptics%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = Reflectivity(1:nZ,1:nL,1:nZ,1:nL)

        END IF Decoupled_Polarization



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsInfraredSensor( SC(SensorIndex) ) ) THEN

        ! -------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------
        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics( &
                           Surface     , &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics   , &  ! In/Output
                           iVar%IRLSOV   )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR land SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on land coverage fraction
          Emissivity(1:nZ,1)          = SfcOptics%Emissivity(1:nZ,1)          * Surface%Land_Coverage
          Reflectivity(1:nZ,1,1:nZ,1) = SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage
          Direct_Reflectivity(1:nZ,1) = SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Land_Coverage
        END IF Infrared_Land


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------
        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Water_SfcOptics( &
                           Surface     , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics   , &  ! In/Output
                           iVar%IRWSOV   )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR water SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on water coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            ( SfcOptics%Emissivity(1:nZ,1) * Surface%Water_Coverage )
          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
            ( SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Water_Coverage )
          Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
            ( SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Water_Coverage )

        END IF Infrared_Water


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------
        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Snow_SfcOptics( &
                           Surface     , &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics   , &  ! In/Output
                           iVar%IRSSOV   )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR snow SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            (SfcOptics%Emissivity(1:nZ,1)*Surface%Snow_Coverage)
          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
            (SfcOptics%Reflectivity(1:nZ,1,1:nZ,1)*Surface%Snow_Coverage)
          Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
            ( SfcOptics%Direct_Reflectivity(1:nZ,1)*Surface%Snow_Coverage)

        ENDIF Infrared_Snow


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------
        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Ice_SfcOptics( &
                           Surface     , &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics   , &  ! In/Output
                           iVar%IRISOV   )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR ice SfcOptics at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on Ice coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            (SfcOptics%Emissivity(1:nZ,1) * Surface%Ice_Coverage)
          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
            (SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Ice_Coverage)
          Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
            ( SfcOptics%Direct_Reflectivity(1:nZ,1)*Surface%Ice_Coverage)

        END IF Infrared_Ice


        ! -----------------------
        ! Assign the final result
        ! -----------------------
        SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
        SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)
        SfcOptics%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1)


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !## Visible part shares using the IR code, in which visible              ##
      !## lambertian emissivity/reflectivity can be computed for visible       ##
      !## wavenumber.                                                          ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) THEN

        mth_Azi_Test: IF( SfcOptics%mth_Azi == 0 ) THEN

          !  ==================
          !  Lambertian surface
          !  ==================

          ! -------------------------------------
          ! Visible LAND emissivity/reflectivity
          ! -------------------------------------
          Visible_Land: IF( Surface%Land_Coverage > ZERO ) THEN

            ! Compute the surface optics
            Error_Status = Compute_VIS_Land_SfcOptics( &
                             Surface     , &  ! Input
                             SensorIndex , &  ! Input
                             ChannelIndex, &  ! Input
                             SfcOptics   , &  ! In/Output
                             iVar%VISLSOV  )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing VIS land SfcOptics at ", &
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF

            ! Accumulate the surface optics properties
            ! based on land coverage fraction
            Emissivity(1:nZ,1)          = SfcOptics%Emissivity(1:nZ,1)          * Surface%Land_Coverage
            Reflectivity(1:nZ,1,1:nZ,1) = SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage
            Direct_Reflectivity(1:nZ,1) = SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Land_Coverage

          END IF Visible_Land


          ! -------------------------------------
          ! Visible WATER emissivity/reflectivity
          ! -------------------------------------
          Visible_Water: IF( Surface%Water_Coverage > ZERO ) THEN

            ! Compute the surface optics
            Error_Status = Compute_VIS_Water_SfcOptics( &
                             Surface     , &  ! Input
                             SensorIndex , &  ! Input
                             ChannelIndex, &  ! Input
                             SfcOptics   , &  ! In/Output
                             iVar%VISWSOV  )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing VIS water SfcOptics at ",&
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF

            ! Accumulate the surface optics properties
            ! based on water coverage fraction
            Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
              ( SfcOptics%Emissivity(1:nZ,1) * Surface%Water_Coverage )
            Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
              ( SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Water_Coverage )
            Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
              ( SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Water_Coverage )

          END IF Visible_Water


          ! ------------------------------------
          ! Visible SNOW emissivity/reflectivity
          ! ------------------------------------
          Visible_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

            ! Compute the surface optics
            Error_Status = Compute_VIS_Snow_SfcOptics( &
                             Surface     , &  ! Input
                             SensorIndex , &  ! Input
                             ChannelIndex, &  ! Input
                             SfcOptics   , &  ! In/Output
                             iVar%VISSSOV  )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing VIS snow SfcOptics at ",&
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF

            ! Accumulate the surface optics properties
            ! based on snow coverage fraction
            Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
              (SfcOptics%Emissivity(1:nZ,1)*Surface%Snow_Coverage)
            Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
              (SfcOptics%Reflectivity(1:nZ,1,1:nZ,1)*Surface%Snow_Coverage)
            Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
              ( SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Snow_Coverage )

          ENDIF Visible_Snow


          ! -----------------------------------
          ! Visible ICE emissivity/reflectivity
          ! -----------------------------------
          Visible_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

            ! Compute the surface optics
            Error_Status = Compute_VIS_Ice_SfcOptics( &
                             Surface     , &  ! Input
                             SensorIndex , &  ! Input
                             ChannelIndex, &  ! Input
                             SfcOptics   , &  ! In/Output
                             iVar%VISISOV  )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing VIS ice SfcOptics at ",&
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF

            ! Accumulate the surface optics properties
            ! based on Ice coverage fraction
            Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
              (SfcOptics%Emissivity(1:nZ,1) * Surface%Ice_Coverage)
            Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + &
              (SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Ice_Coverage)
            Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1) + &
              ( SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Ice_Coverage )

          END IF Visible_Ice


          ! -----------------------
          ! Assign the final result
          ! -----------------------
          SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
          SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)
          SfcOptics%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity(1:nZ,1)

        ELSE

          SfcOptics%Emissivity(1:nZ,1)          = ZERO
          SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = ZERO
          SfcOptics%Direct_Reflectivity         = ZERO

        END IF mth_Azi_Test



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE Sensor_Select

        Error_Status = FAILURE
        WRITE( Message,'("Unrecognised sensor type for channel index ",i0)' ) &
                       ChannelIndex
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN

      END IF Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface optical properties
!       and populate the output SfcOptics_TL structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_TL( &
!                       Surface     , &  ! Input
!                       SfcOptics   , &  ! Input
!                       Surface_TL  , &  ! Input
!                       GeometryInfo, &  ! Input
!                       SensorIndex , &  ! Input
!                       ChannelIndex, &  ! Input
!                       SfcOptics_TL, &  ! In/Output
!                       iVar          )  ! Internal variable input
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear
!                        surface state data.
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
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the radiative
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument should be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_TL( &
    Surface     , &  ! Input
    SfcOptics   , &  ! Input
    Surface_TL  , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics_TL, &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type)     , INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type)   , INTENT(IN)     :: SfcOptics
    TYPE(CRTM_Surface_type)     , INTENT(IN)     :: Surface_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER                     , INTENT(IN)     :: SensorIndex
    INTEGER                     , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type)   , INTENT(IN OUT) :: SfcOptics_TL
    TYPE(iVar_type)             , INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_TL'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Polarization
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity_TL
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity_TL
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity_TL

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    Polarization = SC(SensorIndex)%Polarization( ChannelIndex )
    ! Initialise the local emissivity and reflectivities
    Emissivity_TL   = ZERO
    Reflectivity_TL = ZERO
    Direct_Reflectivity_TL = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      Sensor_Select: IF ( SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------
        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW land SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on land coverage fraction
          Emissivity_TL(1:nZ,1:2) = &
            SfcOptics_TL%Emissivity(1:nZ,1:2)*Surface%Land_Coverage
          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2)*Surface%Land_Coverage

        END IF Microwave_Land


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------
        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Water_SfcOptics_TL( &
                           SfcOptics   , &  ! Input
                           Surface_TL  , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics_TL, &  ! In/Output
                           iVar%MWWSOV   )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW water SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on water coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Water_Coverage )
          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Water_Coverage )

        END IF Microwave_Water


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------
        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Snow_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW snow SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Snow_Coverage )
          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Snow_Coverage )

        ENDIF Microwave_Snow


        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------

        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Ice_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW ice SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Ice_Coverage )
          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Ice_Coverage )

        ENDIF Microwave_Ice



        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#

        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------
          Polarization_Type: SELECT CASE( Polarization )

            ! The unpolarised case, I
            ! e = (eV + eH)/2
            ! r = (rV + rH)/2
            ! Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )
              SfcOptics_TL%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity_TL(1:nZ,1) + Emissivity_TL(1:nZ,2) )
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity_TL(1:nZ,1,1:nZ,1) + Reflectivity_TL(1:nZ,2,1:nZ,2) )

            ! The second Stokes component, Q, the polarisation difference.
            ! e = (eV - eH)/2
            ! r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT )
              SfcOptics_TL%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity_TL(1:nZ,1) - Emissivity_TL(1:nZ,2) )
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity_TL(1:nZ,1,1:nZ,1) - Reflectivity_TL(1:nZ,2,1:nZ,2) )

            ! The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,3)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,3,1:nZ,3)

            ! The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,4)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,4,1:nZ,4)

            ! Vertical linear polarisation
            CASE ( VL_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)

            ! Horizontal linear polarisation
            CASE ( HL_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,2)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(:,2,:,2)

            ! +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)

            ! -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)

            ! Vertical, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! vertical, but the instrument scans cross-track.
            ! e = eV * (1-SIN^2(z))  +  eH * SIN^2(z)
            ! r = rV * (1-SIN^2(z))  +  rH * SIN^2(z)
            CASE ( VL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                SfcOptics_TL%Emissivity(i,1) = (Emissivity_TL(i,1)*(ONE-SIN2_Angle)) + &
                                               (Emissivity_TL(i,2)*SIN2_Angle)
                SfcOptics_TL%Reflectivity(i,1,i,1) = (Reflectivity_TL(i,1,i,1)*(ONE-SIN2_Angle)) + &
                                                     (Reflectivity_TL(i,2,i,2)*SIN2_Angle)
              END DO

            ! Horizontal, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! horizontal, but the instrument scans cross-track.
            ! e = eV * SIN^2(z)  +  eH * (1-SIN^2(z))
            ! r = rV * SIN^2(z)  +  rH * (1-SIN^2(z))
            CASE ( HL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                SfcOptics_TL%Emissivity(i,1) = (Emissivity_TL(i,1)*SIN2_Angle) + &
                                               (Emissivity_TL(i,2)*(ONE-SIN2_Angle))
                SfcOptics_TL%Reflectivity(i,1,i,1) = (Reflectivity_TL(i,1,i,1)*SIN2_Angle) + &
                                                     (Reflectivity_TL(i,2,i,2)*(ONE-SIN2_Angle))
              END DO

            ! Right circular polarisation
            CASE ( RC_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)

            ! Left circular polarisation
            CASE ( LC_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)

            ! Serious problem if we got to this point
            CASE DEFAULT
              Error_Status = FAILURE
              WRITE( Message,'("Unrecognised polarization flag for microwave ",&
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN

           END SELECT Polarization_Type


        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------
          SfcOptics_TL%Emissivity   = Emissivity_TL(1:nZ,1:nL)
          SfcOptics_TL%Reflectivity = Reflectivity_TL(1:nZ,1:nL,1:nZ,1:nL)

        END IF Decoupled_Polarization



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsInfraredSensor( SC(SensorIndex) ) ) THEN


        ! -------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------
        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! Compute the surface optics
          ! **STUB PROCEDURE**
          Error_Status = Compute_IR_Land_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR land SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on land coverage fraction
          Emissivity_TL(1:nZ,1) = &
            SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Land_Coverage
          Reflectivity_TL(1:nZ,1,1:nZ,1) = &
            SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage

        END IF Infrared_Land


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------
        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Water_SfcOptics_TL( &
                           Surface     , &  ! Input
                           SfcOptics   , &  ! Input
                           Surface_TL  , &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           SfcOptics_TL, &  ! In/Output
                           iVar%IRWSOV   )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR water SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on water coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Water_Coverage )
          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Water_Coverage )
          Direct_Reflectivity_TL(1:nZ,1) = Direct_Reflectivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Direct_Reflectivity(1:nZ,1) * Surface%Water_Coverage )

        END IF Infrared_Water


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------
        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Snow_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR snow SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on snow coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Snow_Coverage )
          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Snow_Coverage )
          Direct_Reflectivity_TL(1:nZ,1) = Direct_Reflectivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Direct_Reflectivity(1:nZ,1) * Surface%Snow_Coverage )

        END IF Infrared_Snow


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------
        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Ice_SfcOptics_TL( SfcOptics_TL )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR ice SfcOptics_TL at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on Ice coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Ice_Coverage )
          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Ice_Coverage )
          Direct_Reflectivity_TL(1:nZ,1) = Direct_Reflectivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Direct_Reflectivity(1:nZ,1) * Surface%Ice_Coverage )

        END IF Infrared_Ice


        ! -----------------------
        ! Assign the final result
        ! -----------------------
        SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
        SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)
        SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity_TL(1:nZ,1)


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) THEN


        ! -------------------
        ! Default values only
        ! -------------------
        SfcOptics_TL%Emissivity(1:nZ,1)          = ZERO
        SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = ZERO
        SfcOptics_TL%Direct_Reflectivity = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE Sensor_Select

        Error_Status = FAILURE
        WRITE( Message,'("Unrecognised sensor type for channel index ",i0)' ) &
                       ChannelIndex
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN

      END IF Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_TL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface optical properties
!       for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_AD( &
!                        Surface     , &  ! Input
!                        SfcOptics   , &  ! Input
!                        SfcOptics_AD, &  ! Input
!                        GeometryInfo, &  ! Input
!                        SensorIndex , &  ! Input
!                        ChannelIndex, &  ! Input
!                        Surface_AD  , &  ! Output
!                        iVar          )  ! Internal variable input
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties.
!                        **NOTE: On EXIT from this function, the contents of
!                                this structure may be modified (e.g. set to
!                                zero.)
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        **NOTE: On ENTRY to this function, the contents of
!                                this structure should be defined (e.g.
!                                initialized to some value based on the
!                                position of this function in the call chain.)
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_AD( &
    Surface     , &  ! Input
    SfcOptics   , &  ! Input
    SfcOptics_AD, &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Surface_AD  , &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type)     , INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type)   , INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type)   , INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER                     , INTENT(IN)     :: SensorIndex
    INTEGER                     , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type)     , INTENT(IN OUT) :: Surface_AD
    TYPE(iVar_type)             , INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_AD'
    ! Local variables
    CHARACTER(256)  :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Polarization
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity_AD
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity_AD
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity_AD

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    Polarization = SC(SensorIndex)%Polarization( ChannelIndex )
    ! Initialise the local emissivity and reflectivity adjoints
    Emissivity_AD = ZERO
    Reflectivity_AD = ZERO
    Direct_Reflectivity_AD = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      Sensor_Select: IF ( SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN


        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#
        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------
          Polarization_Type: SELECT CASE( Polarization )

            ! The unpolarised case, I
            ! e = (eV + eH)/2
            ! r = (rV + rH)/2
            ! Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              Emissivity_AD(1:nZ,2) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              Reflectivity_AD(1:nZ,2,1:nZ,2) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! The second Stokes component, Q, the polarisation difference.
            ! e = (eV - eH)/2
            ! r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT )
              Emissivity_AD(1:nZ,1) =  SfcOptics_AD%Emissivity(1:nZ,1)
              Emissivity_AD(1:nZ,2) = -SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) =  SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              Reflectivity_AD(1:nZ,2,1:nZ,2) = -SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT )
              Emissivity_AD(1:nZ,3) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,3,1:nZ,3) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )
              Emissivity_AD(1:nZ,4) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,4,1:nZ,4) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! Vertical linear polarisation
            CASE ( VL_POLARIZATION )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! Horizontal linear polarisation
            CASE ( HL_POLARIZATION )
              Emissivity_AD(1:nZ,2) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,2,1:nZ,2) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! Vertical, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! vertical, but the instrument scans cross-track.
            ! e = eV * (1-SIN^2(z))  +  eH * SIN^2(z)
            ! r = rV * (1-SIN^2(z))  +  rH * SIN^2(z)
            CASE ( VL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                Emissivity_AD(i,1) = SfcOptics_AD%Emissivity(i,1)*(ONE-SIN2_Angle)
                Emissivity_AD(i,2) = SfcOptics_AD%Emissivity(i,1)*SIN2_Angle
                Reflectivity_AD(i,1,i,1) = SfcOptics_AD%Reflectivity(i,1,i,1)*(ONE-SIN2_Angle)
                Reflectivity_AD(i,2,i,2) = SfcOptics_AD%Reflectivity(i,1,i,1)*SIN2_Angle
              END DO
              SfcOptics_AD%Emissivity   = ZERO
              SfcOptics_AD%Reflectivity = ZERO

            ! Horizontal, mixed polarisation. This category of polarisation is
            ! for those microwave channels where the nadir polarisation is
            ! horizontal, but the instrument scans cross-track.
            ! e = eV * SIN^2(z)  +  eH * (1-SIN^2(z))
            ! r = rV * SIN^2(z)  +  rH * (1-SIN^2(z))
            CASE ( HL_MIXED_POLARIZATION )
              DO i = 1, nZ
                SIN2_Angle = (GeometryInfo%Distance_Ratio*SIN(DEGREES_TO_RADIANS*SfcOptics%Angle(i)))**2
                Emissivity_AD(i,1) = SfcOptics_AD%Emissivity(i,1)*SIN2_Angle
                Emissivity_AD(i,2) = SfcOptics_AD%Emissivity(i,1)*(ONE-SIN2_Angle)
                Reflectivity_AD(i,1,i,1) = SfcOptics_AD%Reflectivity(i,1,i,1)*SIN2_Angle
                Reflectivity_AD(i,2,i,2) = SfcOptics_AD%Reflectivity(i,1,i,1)*(ONE-SIN2_Angle)
              END DO
              SfcOptics_AD%Emissivity = ZERO
              SfcOptics_AD%Reflectivity = ZERO

            ! Right circular polarisation
            CASE ( RC_POLARIZATION )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! Left circular polarisation
            CASE ( LC_POLARIZATION )
              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO

            ! Serious problem if we got to this point
            CASE DEFAULT
              Error_Status = FAILURE
              WRITE( Message,'("Unrecognised polarization flag for microwave ",&
                              &"channel index ",i0)' ) ChannelIndex
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN

          END SELECT Polarization_Type


        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------
          Emissivity_AD(1:nZ,1:nL) = SfcOptics_AD%Emissivity(1:nZ,1:nL)
          SfcOptics_AD%Emissivity = ZERO
          Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) = SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL)
          SfcOptics_AD%Reflectivity = ZERO

        END IF Decoupled_Polarization


        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------
        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! The surface optics properties based on ice coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            (Emissivity_AD(1:nZ,1:2)*Surface%Ice_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (Reflectivity_AD(1:nZ,1:2,1:nZ,1:2)*Surface%Ice_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_MW_Ice_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW ice SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
        END IF Microwave_Ice


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------

        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! The surface optics properties based on snow coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            (Emissivity_AD(1:nZ,1:2)*Surface%Snow_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (Reflectivity_AD(1:nZ,1:2,1:nZ,1:2)*Surface%Snow_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_MW_Snow_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW snow SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Microwave_Snow


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------
        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! The surface optics properties based on water coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            (Emissivity_AD(1:nZ,1:2)*Surface%Water_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (Reflectivity_AD(1:nZ,1:2,1:nZ,1:2)*Surface%Water_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_MW_Water_SfcOptics_AD( &
                           SfcOptics   , &  ! Input
                           SfcOptics_AD, &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           Surface_AD  , &  ! Output
                           iVar%MWWSOV   )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW water SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Microwave_Water


        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------
        Microwave_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! The surface optics properties based on land coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            (Emissivity_AD(1:nZ,1:2)*Surface%Land_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            (Reflectivity_AD(1:nZ,1:2,1:nZ,1:2)*Surface%Land_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_MW_Land_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing MW land SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Microwave_Land



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsInfraredSensor( SC(SensorIndex) ) ) THEN


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------
        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! The surface optics properties based on ice coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            (Emissivity_AD(1:nZ,1:nL)*Surface%Ice_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            (Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL)*Surface%Ice_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_IR_Ice_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR ice SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Infrared_Ice


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------
        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! The surface optics properties based on snow coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            (Emissivity_AD(1:nZ,1:nL)*Surface%Snow_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            (Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL)*Surface%Snow_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_IR_Snow_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR snow SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Infrared_Snow


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------
        Infrared_Water: IF ( Surface%Water_Coverage > ZERO ) THEN

          ! The surface optics properties based on water coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            (Emissivity_AD(1:nZ,1:nL)*Surface%Water_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            (Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL)*Surface%Water_Coverage)

          ! Compute the surface optics adjoints
          Error_Status = Compute_IR_Water_SfcOptics_AD( &
                           Surface     , &  ! Input
                           SfcOptics   , &  ! Input
                           SfcOptics_AD, &  ! Input
                           GeometryInfo, &  ! Input
                           SensorIndex , &  ! Input
                           ChannelIndex, &  ! Input
                           Surface_AD  , &  ! Output
                           iVar%IRWSOV   )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR water SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Infrared_Water


        ! --------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! --------------------------------------
        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! The surface optics properties based on land coverage fraction
          ! Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            (Emissivity_AD(1:nZ,1:nL)*Surface%Land_Coverage)
          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            (Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL)*Surface%Land_Coverage)

          ! Compute the surface optics adjoints
          ! **STUB PROCEDURE**
          Error_Status = Compute_IR_Land_SfcOptics_AD( SfcOptics_AD )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error computing IR land SfcOptics_AD at ",&
                            &"channel index ",i0)' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF

        END IF Infrared_Land



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE IF ( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) THEN


        ! -------------------
        ! Default values only
        ! -------------------
        SfcOptics_AD%Emissivity(1:nZ,1)          = ZERO
        SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1) = ZERO
        SfcOptics_AD%Direct_Reflectivity         = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      ELSE Sensor_Select
        Error_Status = FAILURE
        WRITE( Message,'("Unrecognised sensor type for channel index ",i0)' ) &
                       ChannelIndex
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN

      END IF Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_AD

END MODULE CRTM_SfcOptics

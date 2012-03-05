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
                                      MICROWAVE_SENSOR, &
                                      INFRARED_SENSOR, &
                                      VISIBLE_SENSOR, &
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
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type, &
                                      CRTM_Associated_SfcOptics, &
                                      CRTM_Destroy_SfcOptics, &
                                      CRTM_Allocate_SfcOptics, &
                                      CRTM_Assign_SfcOptics
  USE CRTM_MW_Land_SfcOptics,   ONLY: MWLSOVariables_type, &
                                      Compute_MW_Land_SfcOptics, &
                                      Compute_MW_Land_SfcOptics_TL, &
                                      Compute_MW_Land_SfcOptics_AD
  USE CRTM_MW_Water_SfcOptics,  ONLY: MWWSOVariables_type, &
                                      Compute_MW_Water_SfcOptics, &
                                      Compute_MW_Water_SfcOptics_TL, &
                                      Compute_MW_Water_SfcOptics_AD
  USE CRTM_MW_Snow_SfcOptics,   ONLY: MWSSOVariables_type, &
                                      Compute_MW_Snow_SfcOptics, &
                                      Compute_MW_Snow_SfcOptics_TL, &
                                      Compute_MW_Snow_SfcOptics_AD
  USE CRTM_MW_Ice_SfcOptics,    ONLY: MWISOVariables_type, &
                                      Compute_MW_Ice_SfcOptics, &
                                      Compute_MW_Ice_SfcOptics_TL, &
                                      Compute_MW_Ice_SfcOptics_AD
  USE CRTM_IR_Land_SfcOptics,   ONLY: IRLSOVariables_type, &
                                      Compute_IR_Land_SfcOptics, &
                                      Compute_IR_Land_SfcOptics_TL, &
                                      Compute_IR_Land_SfcOptics_AD
  USE CRTM_IR_Water_SfcOptics,  ONLY: IRWSOVariables_type, &
                                      Compute_IR_Water_SfcOptics, &
                                      Compute_IR_Water_SfcOptics_TL, &
                                      Compute_IR_Water_SfcOptics_AD
  USE CRTM_IR_Snow_SfcOptics,   ONLY: IRSSOVariables_type, &
                                      Compute_IR_Snow_SfcOptics, &
                                      Compute_IR_Snow_SfcOptics_TL, &
                                      Compute_IR_Snow_SfcOptics_AD
  USE CRTM_IR_Ice_SfcOptics,    ONLY: IRISOVariables_type, &
                                      Compute_IR_Ice_SfcOptics, &
                                      Compute_IR_Ice_SfcOptics_TL, &
                                      Compute_IR_Ice_SfcOptics_AD
  USE CRTM_VIS_Water_SfcOptics
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_SfcOptics structure data type
  ! in the CRTM_SfcOptics_Define module
  PUBLIC :: CRTM_SfcOptics_type
  ! CRTM_SfcOptics structure routines inherited
  ! from the CRTM_SfcOptics_Define module
  PUBLIC :: CRTM_Associated_SfcOptics
  PUBLIC :: CRTM_Destroy_SfcOptics
  PUBLIC :: CRTM_Allocate_SfcOptics
  PUBLIC :: CRTM_Assign_SfcOptics
  ! Science routines in this modules
  PUBLIC :: CRTM_Compute_SfcOptics
  PUBLIC :: CRTM_Compute_SfcOptics_TL
  PUBLIC :: CRTM_Compute_SfcOptics_AD
  PUBLIC :: CRTM_Compute_SurfaceT
  PUBLIC :: CRTM_Compute_SurfaceT_TL
  PUBLIC :: CRTM_Compute_SurfaceT_AD
  ! Data structure to hold forward model variables
  PUBLIC :: CRTM_SOVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SfcOptics.f90 6551 2010-02-02 00:05:49Z paul.vandelst@noaa.gov $'
  ! Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: CRTM_SOVariables_type
    PRIVATE
    ! Microwave
    TYPE(MWLSOVariables_type) :: MWLSOV ! Land
    TYPE(MWWSOVariables_type) :: MWWSOV ! Water
    TYPE(MWSSOVariables_type) :: MWSSOV ! Snow
    TYPE(MWISOVariables_type) :: MWISOV ! Ice
    ! Infrared
    TYPE(IRLSOVariables_type) :: IRLSOV ! Land
    TYPE(IRWSOVariables_type) :: IRWSOV ! Water
    TYPE(IRSSOVariables_type) :: IRSSOV ! Snow
    TYPE(IRISOVariables_type) :: IRISOV ! Ice
  END TYPE CRTM_SOVariables_type


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
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
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
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linerar
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
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
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
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
!
! NAME:
!       CRTM_Compute_SfcOptics
!
! PURPOSE:
!       Function to compute the surface optical properties and populate
!       the output SfcOptics structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics( Surface               , &  ! Input
!                                              GeometryInfo          , &  ! Input
!                                              SensorIndex           , &  ! Input
!                                              ChannelIndex          , &  ! Input
!                                              SfcOptics             , &  ! Output     
!                                              SOVariables           , &  ! Internal variable output
!                                              Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
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
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       SOVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SOVariables_type)
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
!S-
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics( Surface     , &  ! Input
                                   GeometryInfo, &  ! Input
                                   SensorIndex , &  ! Input
                                   ChannelIndex, &  ! Input
                                   SfcOptics   , &  ! Output
                                   SOV         , &  ! Internal variable output
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(CRTM_SOVariables_type),  INTENT(IN OUT) :: SOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity
    INTEGER :: Sensor_Type, Polarization


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    ! Sensor type and polarization
    Sensor_Type  = SC(SensorIndex)%Sensor_Type
    Polarization = SC(SensorIndex)%Polarization(ChannelIndex)
    ! Initialise the local emissivity and reflectivities
    Emissivity   = ZERO 
    Reflectivity = ZERO
    Direct_Reflectivity = ZERO

    ! ---------------------
    ! Branch on sensor type
    ! ---------------------
    Sensor_Select: SELECT CASE ( Sensor_type)

      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------
        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%MWLSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex                              
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Water_SfcOptics( Surface     , &  ! Input
                                                     GeometryInfo, &  ! Input
                                                     SensorIndex , &  ! Input
                                                     ChannelIndex, &  ! Input
                                                     SfcOptics   , &  ! In/Output
                                                     SOV%MWWSOV  , &  ! Internal variable output
                                                     Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Snow_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%MWSSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Ice_SfcOptics( Surface     , &  ! Input
                                                   GeometryInfo, &  ! Input
                                                   SensorIndex , &  ! Input
                                                   ChannelIndex, &  ! Input
                                                   SfcOptics   , &  ! In/Output
                                                   SOV%MWISOV  , &  ! Internal variable output
                                                   Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
               WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                 &"channel index ", i4 )' ) ChannelIndex
               CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
               RETURN
 
           END SELECT Polarization_Type

        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------
          SfcOptics%Emissivity(1:nZ,1:nL)             = Emissivity(1:nZ,1:nL)
          SfcOptics%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = Reflectivity(1:nZ,1:nL,1:nZ,1:nL)

        ENDIF Decoupled_Polarization



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( INFRARED_SENSOR )

        ! -------------------------------------          
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------
        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%IRLSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Water_SfcOptics( Surface     , &  ! Input
                                                     GeometryInfo, &  ! Input
                                                     SensorIndex , &  ! Input
                                                     ChannelIndex, &  ! Input
                                                     SfcOptics   , &  ! In/Output
                                                     SOV%IRWSOV    )  ! Internal variable output
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Snow_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%IRSSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Ice_SfcOptics( Surface     , &  ! Input
                                                   GeometryInfo, &  ! Input
                                                   SensorIndex , &  ! Input
                                                   ChannelIndex, &  ! Input
                                                   SfcOptics   , &  ! In/Output
                                                   SOV%IRISOV  , &  ! Internal variable output
                                                   Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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

      CASE ( VISIBLE_SENSOR )

      IF( SfcOptics%mth_Azi == 0 ) THEN
     !  Lambertian surface
        ! -------------------------------------          
        ! Infrared/Visible LAND emissivity/reflectivity
        ! -------------------------------------
        Visible_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%IRLSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF

          ! Accumulate the surface optics properties
          ! based on land coverage fraction

          Emissivity(1:nZ,1)          = SfcOptics%Emissivity(1:nZ,1)          * Surface%Land_Coverage
          Reflectivity(1:nZ,1,1:nZ,1) = SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage
          Direct_Reflectivity(1:nZ,1) = SfcOptics%Direct_Reflectivity(1:nZ,1) * Surface%Land_Coverage
        
        END IF Visible_Land


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------
        Visible_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_VIS_Water_SfcOptics( Surface,     &  ! Input
                                                    GeometryInfo,  &  ! Input
                                                    SensorIndex ,  &  ! Input
                                                    ChannelIndex,  &  ! Input
                                                    SfcOptics,     &  ! In/Output
                                                    Message_Log=Message_Log )
!          Error_Status = Compute_IR_Water_SfcOptics( Surface     , &  ! Input
!                                                     GeometryInfo, &  ! Input
!                                                     SensorIndex , &  ! Input
!                                                     ChannelIndex, &  ! Input
!                                                     SfcOptics   , &  ! In/Output
!                                                     SOV%IRWSOV  , &  ! Internal variable output
!                                                     Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------
        Visible_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Snow_SfcOptics( Surface     , &  ! Input
                                                    GeometryInfo, &  ! Input
                                                    SensorIndex , &  ! Input
                                                    ChannelIndex, &  ! Input
                                                    SfcOptics   , &  ! In/Output
                                                    SOV%IRSSOV  , &  ! Internal variable output
                                                    Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------
        Visible_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Ice_SfcOptics( Surface     , &  ! Input
                                                   GeometryInfo, &  ! Input
                                                   SensorIndex , &  ! Input
                                                   ChannelIndex, &  ! Input
                                                   SfcOptics   , &  ! In/Output
                                                   SOV%IRISOV  , &  ! Internal variable output
                                                   Message_Log=Message_Log )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
     
        SfcOptics%Emissivity(1:nZ,1) = ZERO
        SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = ZERO
        SfcOptics%Direct_Reflectivity = ZERO
        
     ENDIF


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE DEFAULT
        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        ChannelIndex
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics


!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface optical properties
!       and populate the output SfcOptics_TL structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_TL( Surface               , &  ! Input
!                                                 SfcOptics             , &  ! Input     
!                                                 Surface_TL            , &  ! Input
!                                                 GeometryInfo          , &  ! Input
!                                                 SensorIndex           , &  ! Input
!                                                 ChannelIndex          , &  ! Input
!                                                 SfcOptics_TL          , &  ! In/Output     
!                                                 SOVariables           , &  ! Internal variable input
!                                                 Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
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
!       SOVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SOVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the radiative
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
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
!
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_TL( Surface     , &  ! Input
                                      SfcOptics   , &  ! Input
                                      Surface_TL  , &  ! Input
                                      GeometryInfo, &  ! Input
                                      SensorIndex , &  ! Input
                                      ChannelIndex, &  ! Input
                                      SfcOptics_TL, &  ! Output
                                      SOV         , &  ! Internal variable input
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(CRTM_SOVariables_type),  INTENT(IN)     :: SOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_TL'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Sensor_Type, Polarization
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity_TL
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity_TL
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity_TL

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    ! Sensor type and polarization
    Sensor_Type  = SC(SensorIndex)%Sensor_Type
    Polarization = SC(SensorIndex)%Polarization( ChannelIndex )
    ! Initialise the local emissivity and reflectivities
    Emissivity_TL   = ZERO 
    Reflectivity_TL = ZERO
    Direct_Reflectivity_TL = ZERO

    ! ---------------------
    ! Branch on sensor type
    ! ---------------------
    Sensor_Select: SELECT CASE ( Sensor_type)

      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------
        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics_TL( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input
                                                       Surface_TL  , &  ! Input
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       SfcOptics_TL, &  ! In/Output
                                                       SOV%MWLSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Water_SfcOptics_TL( Surface     , &  ! Input
                                                        SfcOptics   , &  ! Input
                                                        Surface_TL  , &  ! Input
                                                        GeometryInfo, &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        SfcOptics_TL, &  ! In/Output
                                                        SOV%MWWSOV  , &  ! Internal variable input
                                                        Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Snow_SfcOptics_TL( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input
                                                       Surface_TL  , &  ! Input
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       SfcOptics_TL, &  ! In/Output
                                                       SOV%MWSSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Ice_SfcOptics_TL( Surface     , &  ! Input
                                                      SfcOptics   , &  ! Input
                                                      Surface_TL  , &  ! Input
                                                      GeometryInfo, &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      SfcOptics_TL, &  ! In/Output
                                                      SOV%MWISOV  , &  ! Internal variable input
                                                      Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
               WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                 &"channel index ", i4 )' ) &
                               ChannelIndex
               CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log=Message_Log )
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

      CASE ( INFRARED_SENSOR )


        ! -------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------
        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics_TL( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input
                                                       Surface_TL  , &  ! Input
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       SfcOptics_TL, &  ! In/Output
                                                       SOV%IRLSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Water_SfcOptics_TL( Surface     , &  ! Input
                                                        SfcOptics   , &  ! Input
                                                        Surface_TL  , &  ! Input
                                                        GeometryInfo, &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        SfcOptics_TL, &  ! In/Output
                                                        SOV%IRWSOV    )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Snow_SfcOptics_TL( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input
                                                       Surface_TL  , &  ! Input
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       SfcOptics_TL, &  ! In/Output
                                                       SOV%IRSSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Ice_SfcOptics_TL( Surface     , &  ! Input
                                                      SfcOptics   , &  ! Input
                                                      Surface_TL  , &  ! Input
                                                      GeometryInfo, &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      SfcOptics_TL, &  ! In/Output
                                                      SOV%IRISOV  , &  ! Internal variable input
                                                      Message_Log=Message_Log ) ! Error_message
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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

      CASE ( VISIBLE_SENSOR )


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

      CASE DEFAULT
        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        ChannelIndex
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_TL





!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface optical properties
!       for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_AD( Surface               , &  ! Input
!                                                 SfcOptics             , &  ! Input
!                                                 SfcOptics_AD          , &  ! Input
!                                                 GeometryInfo          , &  ! Input
!                                                 SensorIndex           , &  ! Input
!                                                 ChannelIndex          , &  ! Input
!                                                 Surface_AD            , &  ! Output
!                                                 SOVariables           , &  ! Internal variable input
!                                                 Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties.
!                        **NOTE: On EXIT from this function, the contents of
!                                this structure may be modified (e.g. set to
!                                zero.)
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
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
!       SOVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SOVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        **NOTE: On ENTRY to this function, the contents of
!                                this structure should be defined (e.g.
!                                initialized to some value based on the
!                                position of this function in the call chain.)
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
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
!
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_AD( Surface     , &  ! Input
                                      SfcOptics   , &  ! Input
                                      SfcOptics_AD, &  ! Input
                                      GeometryInfo, &  ! Input
                                      SensorIndex , &  ! Input
                                      ChannelIndex, &  ! Input
                                      Surface_AD  , &  ! Output
                                      SOV         , &  ! Internal variable input
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(CRTM_SOVariables_type),  INTENT(IN)     :: SOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_AD'
    ! Local variables
    CHARACTER(256)  :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Sensor_Type, Polarization
    REAL(fp) :: SIN2_Angle
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Emissivity_AD
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES, &
                        SfcOptics%n_Angles,MAX_N_STOKES) :: Reflectivity_AD
    REAL(fp), DIMENSION(SfcOptics%n_Angles,MAX_N_STOKES) :: Direct_Reflectivity_AD

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles
    ! Sensor type and polarization
    Sensor_Type  = SC(SensorIndex)%Sensor_Type
    Polarization = SC(SensorIndex)%Polarization( ChannelIndex )
    ! Initialise the local emissivity and reflectivity adjoints
    Emissivity_AD = ZERO 
    Reflectivity_AD = ZERO
    Direct_Reflectivity_AD = ZERO

    ! ---------------------
    ! Branch on sensor type
    ! ---------------------
    Sensor_Select: SELECT CASE ( Sensor_type)

      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )


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
              WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                &"channel index ", i4 )' ) &
                              ChannelIndex
              CALL Display_Message( ROUTINE_NAME, &
                               TRIM( Message ), &
                               Error_Status, &
                               Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Ice_SfcOptics_AD( Surface     , &  ! Input
                                                      SfcOptics   , &  ! Input     
                                                      SfcOptics_AD, &  ! Input     
                                                      GeometryInfo, &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      Surface_AD  , &  ! Output
                                                      SOV%MWISOV  , &  ! Internal variable input
                                                      Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Snow_SfcOptics_AD( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input     
                                                       SfcOptics_AD, &  ! Input     
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       Surface_AD  , &  ! Output
                                                       SOV%MWSSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Water_SfcOptics_AD( Surface     , &  ! Input
                                                        SfcOptics   , &  ! Input     
                                                        SfcOptics_AD, &  ! Input     
                                                        GeometryInfo, &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        Surface_AD  , &  ! Output
                                                        SOV%MWWSOV  , &  ! Internal variable input
                                                        Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_MW_Land_SfcOptics_AD( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input     
                                                       SfcOptics_AD, &  ! Input     
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       Surface_AD  , &  ! Output
                                                       SOV%MWLSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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

      CASE ( INFRARED_SENSOR )


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
          Error_Status = Compute_IR_Ice_SfcOptics_AD( Surface     , &  ! Input
                                                      SfcOptics   , &  ! Input     
                                                      SfcOptics_AD, &  ! Input     
                                                      GeometryInfo, &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      Surface_AD  , &  ! Output
                                                      SOV%IRISOV  , &  ! Internal variable input
                                                      Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Snow_SfcOptics_AD( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input     
                                                       SfcOptics_AD, &  ! Input     
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       Surface_AD  , &  ! Output
                                                       SOV%IRSSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          END IF

        END IF Infrared_Snow


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------
        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

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
          Error_Status = Compute_IR_Water_SfcOptics_AD( Surface     , &  ! Input
                                                        SfcOptics   , &  ! Input     
                                                        SfcOptics_AD, &  ! Input     
                                                        GeometryInfo, &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        Surface_AD  , &  ! Output
                                                        SOV%IRWSOV    )  ! Internal variable input
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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
          Error_Status = Compute_IR_Land_SfcOptics_AD( Surface     , &  ! Input
                                                       SfcOptics   , &  ! Input     
                                                       SfcOptics_AD, &  ! Input     
                                                       GeometryInfo, &  ! Input
                                                       SensorIndex , &  ! Input
                                                       ChannelIndex, &  ! Input
                                                       Surface_AD  , &  ! Output
                                                       SOV%IRLSOV  , &  ! Internal variable input
                                                       Message_Log=Message_Log )  ! Error messaging
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            ChannelIndex
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log=Message_Log )
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

      CASE ( VISIBLE_SENSOR )


        ! -------------------
        ! Default values only
        ! -------------------
        SfcOptics_AD%Emissivity(1:nZ,1)       = ZERO
        SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1) = ZERO
        SfcOptics_AD%Direct_Reflectivity = ZERO


      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE DEFAULT

        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        ChannelIndex
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_AD

END MODULE CRTM_SfcOptics

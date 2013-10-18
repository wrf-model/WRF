!
! ODSSU_AtmAbsorption
!
! Module containing routines to compute the optical depth profile for SSUs
!
!
! CREATION HISTORY:
!       Based on CRTM_AtmAbsorption_ssu     by:  Quanhua Liu, JCSDA,      Dec  1, 2007
!       Rewritten for CRTMv2.0              by:  Yong Han, NOAA/NESDIS,   Oct  6, 2009
!       Revised                             by:  Paul van Delst, , JCSDA, Oct 26, 2009
!       Revised                             by:  Quanhua Liu, JCSDA,      Oct 29, 2009
!       Revised                             by:  Yong Chen, JCSDA,        Nov  9, 2009
!

MODULE ODSSU_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Search_Utility,           ONLY: Bisection_Search
  USE CRTM_Parameters,          ONLY: ZERO, ONE
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_AtmOptics_Define,    ONLY: CRTM_AtmOptics_type
  USE SSU_Input_Define,         ONLY: SSU_Input_type, &
                                      SSU_Input_GetValue, &
                                      SSU_Input_CellPressureIsSet
  USE ODSSU_TauCoeff,           ONLY: TC
  ! ...ODAS modules
  USE ODAS_Predictor_Define,    ONLY: ODAS_Predictor_type
  USE ODAS_AtmAbsorption,       ONLY: ODAS_AAVar_type => iVar_type , &
                                      ODAS_Compute_AtmAbsorption   , &
                                      ODAS_Compute_AtmAbsorption_TL, &
                                      ODAS_Compute_AtmAbsorption_AD
  ! ...ODPS modules
  USE ODPS_Predictor_Define,    ONLY: ODPS_Predictor_type
  USE ODPS_AtmAbsorption,       ONLY: ODPS_Compute_AtmAbsorption   , &
                                      ODPS_Compute_AtmAbsorption_TL, &
                                      ODPS_Compute_AtmAbsorption_AD
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: ODSSU_Compute_Weights
  PUBLIC :: ODSSU_Compute_AtmAbsorption
  PUBLIC :: ODSSU_Compute_AtmAbsorption_TL
  PUBLIC :: ODSSU_Compute_AtmAbsorption_AD


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ODSSU_Compute_AtmAbsorption
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption
  END INTERFACE ODSSU_Compute_AtmAbsorption

  INTERFACE ODSSU_Compute_AtmAbsorption_TL
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption_TL
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption_TL
  END INTERFACE ODSSU_Compute_AtmAbsorption_TL

  INTERFACE ODSSU_Compute_AtmAbsorption_AD
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption_AD
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption_AD
  END INTERFACE ODSSU_Compute_AtmAbsorption_AD


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: ODSSU_AtmAbsorption.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: iVar_type
    PRIVATE
    TYPE(ODAS_AAVar_type) :: ODAS(2)
    REAL(fp) :: Weight(2) = ZERO
    REAL(fp) :: CO2_Cell = ZERO
    INTEGER  :: Index_low = 1
  END TYPE iVar_type



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
!       ODSSU_Compute_Weights
!
! PURPOSE:
!       Subroutine to calculate ODSSU algorithm linear interpolation weighting
!       factors for the SSU CO2 cell pressure.
!
! CALLING SEQUENCE:
!       CALL ODSSU_Compute_Weights( SSU_Input   , &
!                                   SensorIndex , &
!                                   ChannelIndex, &
!                                   iVar          )
!
! INPUTS:
!       SSU_Input:       Structure containing the SSU input data.
!                        UNITS:      N/A
!                        TYPE:       SSU_Input_type
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
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODSSU_Compute_Weights( &
    SSU_Input   , &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    iVar          )  ! Internal variable output
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN)     :: SSU_Input
    INTEGER             , INTENT(IN)     :: SensorIndex
    INTEGER             , INTENT(IN)     :: ChannelIndex
    TYPE(iVar_type)     , INTENT(IN OUT) :: iVar
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ODSSU_Compute_Weights'
    ! Variables
    CHARACTER(ML) :: msg
    REAL(fp) :: Time, Cell_Pressure

    ! Compute the CO2 cell pressure
    IF( SSU_Input_CellPressureIsSet(SSU_Input) ) THEN
      ! ...Interpolate the cell pressure data
      CALL SSU_Input_GetValue( SSU_Input, &
                               Channel = ChannelIndex, &
                               Cell_Pressure = Cell_Pressure )
      iVar%CO2_Cell  = Cell_Pressure
      iVar%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), iVar%CO2_Cell )
    ELSE
      ! ...Get the mission time
      CALL SSU_Input_GetValue( SSU_Input, Time=Time )
      IF( Time < TC(SensorIndex)%Ref_Time(1) )THEN
        Time = TC(SensorIndex)%Ref_Time(1)
        WRITE( msg,'("Invalid time. Reset to ",f8.2)' ) Time
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
      END IF
      ! ...Obtain CO2 cell pressure for given time
      CALL get_CO2_Cell_p( SensorIndex, ChannelIndex, Time, iVar%CO2_Cell )
      iVar%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), iVar%CO2_Cell )
    END IF


    ! Compute the interpolation weights
    iVar%Weight(1) = (iVar%CO2_Cell - TC(SensorIndex)%TC_CellPressure(iVar%Index_low,ChannelIndex))/ &
                      (TC(SensorIndex)%TC_CellPressure(iVar%Index_low+1,ChannelIndex) - &
                       TC(SensorIndex)%TC_CellPressure(iVar%Index_low  ,ChannelIndex)   )
    iVar%Weight(2) = ONE - iVar%Weight(1)

  END SUBROUTINE ODSSU_Compute_Weights
  

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODSSU_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for the SSU sensor for a given channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL ODSSU_Compute_AtmAbsorption( SensorIndex , &
!                                         ChannelIndex, &
!                                         Predictor   , &
!                                         AtmOptics   , &
!                                         iVar          )
!
! INPUTS:
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
! *********** INTENT NEEDS TO BE CORRECTED TO JUST (IN) ***********
! *********** PREDICTOR CAN BE MODIFIED IN ODPS_AtmAbsorption MODULE ************
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                                      or
!                                    ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
! *********** INTENT NEEDS TO BE CORRECTED TO JUST (IN) ***********
! *********** PREDICTOR CAN BE MODIFIED IN ODPS_AtmAbsorption MODULE ************
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!        AtmOptics:      Structure containing the computed optical depth
!                        profile.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output structure arguments are IN OUT rather
!       than just OUT. This is to prevent default reinitialisation upon entry.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption( &
    SensorIndex  , &  ! Input
    ChannelIndex , &  ! Input
    Predictor    , &  ! Input
    AtmOptics    , &  ! Output
    iVar           )  ! Internal variable In/output
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics
    TYPE(iVar_type)          , INTENT(IN OUT) :: iVar
    ! Variables
    REAL(fp) :: optical_depth( AtmOptics%n_Layers )


    ! Compute the optical depths
    ! ...At cell pressure 1
    CALL ODAS_Compute_AtmAbsorption( &
           TC(SensorIndex)%ODAS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           AtmOptics                           , &
           iVar%ODAS(1)                          )
    optical_depth = AtmOptics%Optical_Depth
    ! ...At cell pressure 2
    CALL ODAS_Compute_AtmAbsorption( &
           TC(SensorIndex)%ODAS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           AtmOptics                             , &
           iVar%ODAS(2)                            )
    ! ...Weighted average
    AtmOptics%Optical_Depth = iVar%Weight(1)*AtmOptics%Optical_Depth + &
                              iVar%Weight(2)*optical_depth

  END SUBROUTINE Compute_ODAS_AtmAbsorption


  SUBROUTINE Compute_ODPS_AtmAbsorption( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! Input
    AtmOptics   , &  ! Output
    iVar          )  ! Internal variable In/output
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor    ! INTENT! PREDICTOR CAN BE MODIFIED IN ODPS_AtmAbsorption MODULE
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics
    TYPE(iVar_type)          , INTENT(IN OUT) :: iVar
    ! Variables
    REAL(fp) :: optical_depth( AtmOptics%n_Layers )

    ! Compute the optical depths
    ! ...At cell pressure 1
    CALL ODPS_Compute_AtmAbsorption( &
           TC(SensorIndex)%ODPS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           AtmOptics                             )
    optical_depth = AtmOptics%Optical_Depth
    ! ...At cell pressure 2
    CALL ODPS_Compute_AtmAbsorption( &
           TC(SensorIndex)%ODPS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           AtmOptics                               )
    ! ...Weighted average
    AtmOptics%Optical_Depth = iVar%Weight(1)*AtmOptics%Optical_Depth + &
                              iVar%Weight(2)*optical_depth

  END SUBROUTINE Compute_ODPS_AtmAbsorption


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODSSU_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for the SSU sensor for a given channel and
!       atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODSSU_Compute_AtmAbsorption_TL( SensorIndex , &
!                                            ChannelIndex, &
!                                            Predictor   , &
!                                            Predictor_TL, &
!                                            AtmOptics_TL, &
!                                            iVar          )
!
! INPUTS:
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
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                                      or
!                                    ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! *********** INTENT NEEDS TO BE CORRECTED TO JUST (IN) ***********
! *********** PREDICTOR_TL IS SPECIFIED AS INTENT(IN OUT) IN ODPS_AtmAbsorption MODULE ************
!       Predictor_TL:    Structure containing the tangent-linear integrated
!                        absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                                      or
!                                    ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
! *********** PREDICTOR_TL IS SPECIFIED AS INTENT(IN OUT) IN ODPS_AtmAbsorption MODULE ************
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!        AtmOptics_TL:   Structure containing the computed tangent-linear
!                        optical depth profile.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output structure arguments are IN OUT rather
!       than just OUT. This is to prevent default reinitialisation upon entry.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption_TL( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    Predictor_TL, &  ! TL  Input
    AtmOptics_TL, &  ! TL  Output
    iVar          )  ! Internal variable input
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Variables
    REAL(fp) :: optical_depth_TL(AtmOptics_TL%n_Layers)

    ! Compute the tangent-linear optical depths
    ! ...At cell pressure 1
    CALL ODAS_Compute_AtmAbsorption_TL( &
           TC(SensorIndex)%ODAS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           Predictor_TL                        , &
           AtmOptics_TL                        , &
           iVar%ODAS(1)                          )
    optical_depth_TL = AtmOptics_TL%Optical_Depth
    ! ...At cell pressure 2
    CALL ODAS_Compute_AtmAbsorption_TL( &
           TC(SensorIndex)%ODAS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           Predictor_TL                          , &
           AtmOptics_TL                          , &
           iVar%ODAS(2)                            )
    ! ...Weighted average
    AtmOptics_TL%Optical_Depth = iVar%Weight(1)*AtmOptics_TL%Optical_Depth + &
                                 iVar%Weight(2)*optical_depth_TL

  END SUBROUTINE Compute_ODAS_AtmAbsorption_TL


  SUBROUTINE Compute_ODPS_AtmAbsorption_TL( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    Predictor_TL, &  ! TL  Input
    AtmOptics_TL, &  ! TL  Output
    iVar          )  ! Internal variable input
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_TL    ! INTENT! IS SPECIFIED AS (IN OUT) IN ODPS_AtmAbsorption MODULE
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Variables
    REAL(fp) :: optical_depth_TL(AtmOptics_TL%n_Layers)

    ! Compute the tangent-linear optical depths
    ! ...At cell pressure 1
    CALL ODPS_Compute_AtmAbsorption_TL( &
           TC(SensorIndex)%ODPS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           Predictor_TL                        , &
           AtmOptics_TL                          )
    optical_depth_TL = AtmOptics_TL%Optical_Depth
    ! ...At cell pressure 2
    CALL ODPS_Compute_AtmAbsorption_TL( &
           TC(SensorIndex)%ODPS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           Predictor_TL                          , &
           AtmOptics_TL                            )
    ! ...Weighted average
    AtmOptics_TL%Optical_Depth = iVar%Weight(1)*AtmOptics_TL%Optical_Depth + &
                                 iVar%Weight(2)*optical_depth_TL

  END SUBROUTINE Compute_ODPS_AtmAbsorption_TL


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODSSU_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the layer optical depths due
!       to gaseous absorption for the SSU sensor for a given channel and
!       atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODSSU_Compute_AtmAbsorption_AD( SensorIndex , &
!                                            ChannelIndex, &
!                                            Predictor   , &
!                                            AtmOptics_AD, &
!                                            Predictor_AD, &
!                                            iVar          )
!
! INPUTS:
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
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                                      or
!                                    ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:    Structure containing the adjoint optical
!                        depth profile.
!                        *** NOTE: Optical depth component may be set to
!                                  zero upon exit.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Predictor_AD:    Structure containing the adjoint integrated
!                        absorber and predictor profile data.
!                        *** NOTE: Must be defined upon entry.
!                        UNITS:      N/A
!                        TYPE:       Same as Predictor input argument.
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       The contents of the input adjoint arguments are modified upon exit.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption_AD( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    Predictor_AD, &  ! AD  Output
    iVar          )  ! Internal variable input
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(ODAS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Variables
    REAL(fp) :: optical_depth_AD( AtmOptics_AD%n_Layers)

    ! Adjoint of weighted average optical depth
    optical_depth_AD           = iVar%Weight(2)*AtmOptics_AD%Optical_Depth
    AtmOptics_AD%Optical_Depth = iVar%Weight(1)*AtmOptics_AD%Optical_Depth

    ! Compute the adjoint of the optical depths
    ! ...At cell pressure #2
    CALL ODAS_Compute_AtmAbsorption_AD( &
           TC(SensorIndex)%ODAS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           AtmOptics_AD                          , &
           Predictor_AD                          , &
           iVar%ODAS(2)                            )
    AtmOptics_AD%Optical_Depth = AtmOptics_AD%Optical_Depth + optical_depth_AD
    ! ...At cell pressure #1
    CALL ODAS_Compute_AtmAbsorption_AD( &
           TC(SensorIndex)%ODAS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           AtmOptics_AD                        , &
           Predictor_AD                        , &
           iVar%ODAS(1)                          )

  END SUBROUTINE Compute_ODAS_AtmAbsorption_AD


  SUBROUTINE Compute_ODPS_AtmAbsorption_AD( &
    SensorIndex , &
    ChannelIndex, &
    Predictor   , &
    AtmOptics_AD, &
    Predictor_AD, &
    iVar          )
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Variables
    REAL(fp) :: optical_depth_AD( AtmOptics_AD%n_Layers)

    ! Adjoint of weighted average optical depth
    optical_depth_AD           = iVar%Weight(2)*AtmOptics_AD%Optical_Depth
    AtmOptics_AD%Optical_Depth = iVar%Weight(1)*AtmOptics_AD%Optical_Depth

    ! Compute the adjoint of the optical depths
    ! ...At cell pressure #2
    CALL ODPS_Compute_AtmAbsorption_AD( &
           TC(SensorIndex)%ODPS(iVar%Index_low+1), &
           ChannelIndex                          , &
           Predictor                             , &
           AtmOptics_AD                          , &
           Predictor_AD                            )
    AtmOptics_AD%Optical_Depth = AtmOptics_AD%Optical_Depth + optical_depth_AD
    ! ...At cell pressure #1
    CALL ODPS_Compute_AtmAbsorption_AD( &
           TC(SensorIndex)%ODPS(iVar%Index_low), &
           ChannelIndex                        , &
           Predictor                           , &
           AtmOptics_AD                        , &
           Predictor_AD                          )

  END SUBROUTINE Compute_ODPS_AtmAbsorption_AD



    SUBROUTINE get_CO2_Cell_p(SensorIndex,ChannelIndex,u,y0)
! -------------------------------------------------------------------
!  Using an sensor "SensorIndex" and time "u" to find CO2 cell pressure "y0".
! -------------------------------------------------------------------
       INTEGER, INTENT( IN ) :: SensorIndex, ChannelIndex
       REAL(fp), INTENT( IN ) :: u
       REAL(fp), INTENT( OUT ) :: y0
       INTEGER :: n, jLower, jUpper, indx

       n = SIZE(TC(SensorIndex)%Ref_Time)
       jLower = 1
       jUpper = n

       if(u.ge.TC(SensorIndex)%Ref_Time(n)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(n,ChannelIndex)
       return
       else if(u.le.TC(SensorIndex)%Ref_Time(1)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(1,ChannelIndex)
       return
       endif

       indx = Bisection_Search( TC(SensorIndex)%Ref_Time, u )

       y0 = TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex) + &
          (TC(SensorIndex)%Ref_CellPressure(indx+1,ChannelIndex)- &
          TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex))/  &
          (TC(SensorIndex)%Ref_Time(indx+1)-TC(SensorIndex)%Ref_Time(indx))* &
          (u-TC(SensorIndex)%Ref_Time(indx))
       RETURN
    END SUBROUTINE get_CO2_Cell_p
!

END MODULE ODSSU_AtmAbsorption


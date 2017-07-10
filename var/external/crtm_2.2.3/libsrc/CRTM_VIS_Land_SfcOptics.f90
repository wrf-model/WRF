!
! CRTM_VIS_Land_SfcOptics
!
! Module to compute the surface optical properties for LAND surfaces at
! visible frequencies required for determining the LAND surface
! contribution to the radiative transfer.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Jan-2012
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_VIS_Land_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, Display_Message
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_Micron
  USE CRTM_Parameters          , ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff            , ONLY: SC
  USE CRTM_Surface_Define      , ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define , ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define    , ONLY: CRTM_SfcOptics_type
  USE CRTM_SEcategory          , ONLY: SEVar_type => iVar_type, &
                                       SEcategory_Emissivity
  USE CRTM_VISlandCoeff        , ONLY: VISlandC
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
  PUBLIC :: Compute_VIS_Land_SfcOptics
  PUBLIC :: Compute_VIS_Land_SfcOptics_TL
  PUBLIC :: Compute_VIS_Land_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_VIS_Land_SfcOptics.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    TYPE(SEVar_type) :: sevar
  END TYPE iVar_type


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_VIS_Land_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at UV/visible
!       frequencies over a land surface.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Land_SfcOptics(
!                        Surface      , &
!                        SensorIndex  , &
!                        Channel_Index, &
!                        SfcOptics    , &
!                        iVar           )
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
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
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the module containing this procedure.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
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

  FUNCTION Compute_VIS_Land_SfcOptics( &
    Surface     , &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   , &  ! Output
    iVar        ) &  ! Internal variable output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Surface_type),   INTENT(IN)     :: Surface
    INTEGER,                   INTENT(IN)     :: SensorIndex
    INTEGER,                   INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics
    TYPE(iVar_type),           INTENT(IN OUT) :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Land_SfcOptics'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: j
    REAL(fp) :: frequency, emissivity

    ! Set up
    err_stat = SUCCESS
    frequency = SC(SensorIndex)%Wavenumber(ChannelIndex)


    ! Compute Lambertian surface emissivity
    err_stat = SEcategory_Emissivity( &
                 VISlandC         , &  ! Input
                 frequency        , &  ! Input
                 Surface%Land_Type, &  ! Input
                 emissivity       , &  ! Output
                 iVar%sevar         )  ! Internal variable output
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error occurred in SEcategory_Emissivity()'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Solar direct component
    SfcOptics%Direct_Reflectivity(:,1) = ONE - emissivity


    ! Fill the return emissivity and reflectivity arrays
    SfcOptics%Emissivity(1:SfcOptics%n_Angles,1) = emissivity
    DO j = 1, SfcOptics%n_Angles
      SfcOptics%Reflectivity(1:SfcOptics%n_Angles,1,j,1) = (ONE - SfcOptics%Emissivity(j,1))*SfcOptics%Weight(j)
    END DO

  END FUNCTION Compute_VIS_Land_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_VIS_Land_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at UV/visible frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO TL
!           COMPONENTS IN THE VIS LAND SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Land_SfcOptics_TL( SfcOptics_TL )
!
! OUTPUTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
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
!       than just OUT as it may be defined upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_VIS_Land_SfcOptics_TL( &
    SfcOptics_TL ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Land_SfcOptics_TL'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the tangent-linear surface optical parameters
    ! ***No TL models yet, so default TL output is zero***
    SfcOptics_TL%Reflectivity        = ZERO
    SfcOptics_TL%Direct_Reflectivity = ZERO
    SfcOptics_TL%Emissivity          = ZERO

  END FUNCTION Compute_VIS_Land_SfcOptics_TL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_VIS_Land_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at UW/visible frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO AD
!           COMPONENTS IN THE VIS LAND SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Land_SfcOptics_AD( SfcOptics_AD )
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

  FUNCTION Compute_VIS_Land_SfcOptics_AD( &
    SfcOptics_AD ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Land_SfcOptics_AD'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the adjoint surface optical parameters
    ! ***No AD models yet, so there is no impact on AD result***
    SfcOptics_AD%Reflectivity        = ZERO
    SfcOptics_AD%Direct_Reflectivity = ZERO
    SfcOptics_AD%Emissivity          = ZERO

  END FUNCTION Compute_VIS_Land_SfcOptics_AD

END MODULE CRTM_VIS_Land_SfcOptics

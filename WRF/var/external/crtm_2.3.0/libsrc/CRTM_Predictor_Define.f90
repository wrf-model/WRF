!
! CRTM_Predictor_Define
!
! Module containing the definition of the container predictor structure for
! the gaseous absorption transmittance models
!


MODULE CRTM_Predictor_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,            ONLY: ODAS_ALGORITHM,  ODPS_ALGORITHM, ODSSU_ALGORITHM
  USE CRTM_TauCoeff,              ONLY: TC
  ! ODAS modules
  USE ODAS_Predictor_Define,      ONLY: ODAS_Predictor_type      , &
                                        ODAS_Predictor_Associated, &
                                        ODAS_Predictor_Create    , &
                                        ODAS_Predictor_Destroy   , &
                                        ODAS_Predictor_Inspect
  USE ODAS_Predictor,             ONLY: ODAS_MAX_N_PREDICTORS => MAX_N_PREDICTORS, &
                                        ODAS_MAX_N_ABSORBERS  => MAX_N_ABSORBERS , &
                                        ODAS_MAX_N_ORDERS     => MAX_N_ORDERS
  ! ODPS modules
  USE ODPS_Predictor_Define,      ONLY: ODPS_Predictor_type      , &
                                        ODPS_Predictor_Associated, &
                                        ODPS_Predictor_Destroy   , &
                                        ODPS_Predictor_Create    , &
                                        ODPS_Predictor_Inspect   , &
                                        PAFV_Associated          , &
                                        PAFV_Destroy             , &
                                        PAFV_Create
  USE ODPS_Predictor,             ONLY: ODPS_Get_n_Components    , &
                                        ODPS_Get_max_n_Predictors, &
                                        ODPS_Get_n_Absorbers     , &
                                        ODPS_Get_SaveFWVFlag     , &
                                        ALLOW_OPTRAN
  ! ODZeeman modules
  USE ODZeeman_AtmAbsorption,     ONLY: Get_NumOfZComponents, &
                                        Get_NumOfZAbsorbers,  &
                                        Get_NumOfZPredictors
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_Predictor_type
  ! Procedures
  PUBLIC :: CRTM_Predictor_Associated
  PUBLIC :: CRTM_Predictor_Destroy
  PUBLIC :: CRTM_Predictor_Create
  PUBLIC :: CRTM_Predictor_Inspect


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Predictor_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ---------------------
  ! Structure definitions
  ! ---------------------
  ! Predictor container structure definition
  TYPE :: CRTM_Predictor_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! The predictor sub-objects
    TYPE(ODAS_Predictor_type) :: ODAS
    TYPE(ODPS_Predictor_type) :: ODPS
    TYPE(ODPS_Predictor_type) :: ODZeeman
  END TYPE CRTM_Predictor_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Predictor_Associated
!
! PURPOSE:
!       Elemental function to test the association status of the
!       CRTM_Predictor structure.
!
! CALLING SEQUENCE:
!       Status = CRTM_Predictor_Associated( CRTM_Predictor )
!
! OBJECTS:
!       CRTM_Predictor:
!         Structure which is to have its member's
!         status tested.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:
!         The return value is a logical value indicating the
!         status of the allocated members.
!          .TRUE.  - if the CRTM_Predictor object has been allocated.
!          .FALSE. - if the CRTM_Predictor object has NOT been allocated.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Predictor_Associated(self) RESULT(status)
    TYPE(CRTM_Predictor_type), INTENT(IN) :: self
    LOGICAL :: status
    status = self%Is_Allocated
  END FUNCTION CRTM_Predictor_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Predictor_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM_Predictor container objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Predictor_Destroy( CRTM_Predictor )
!
! OBJECTS:
!       CRTM_Predictor:
!         Re-initialized CRTM_Predictor structure.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Predictor_Destroy(self)
    TYPE(CRTM_Predictor_type), INTENT(OUT) :: self
    self%Is_Allocated =.FALSE.
  END SUBROUTINE CRTM_Predictor_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Predictor_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of a CRTM_Predictor object.
!
! CALLING SEQUENCE:
!       CALL CRTM_Predictor_Create( &
!              CRTM_Predictor, &
!              n_Layers      , &
!              SensorIndex     )
!
! OBJECTS:
!       CRTM_Predictor:
!         CRTM_Predictor object structure.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:
!         Number of atmospheric layers.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the CRTM_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:
!         Sensor index id. This is a unique index associated
!         with a (supported) sensor used to access the
!         shared coefficient data for a particular sensor.
!         See the ChannelIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the CRTM_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Predictor_Create( &
    self         , &  ! Output
    n_Layers     , &  ! Input
    SensorIndex  , &  ! Input
    SaveFWV        )  ! Optional Input
    ! Arguments
    TYPE(CRTM_Predictor_type), INTENT(OUT) :: self
    INTEGER,                   INTENT(IN)  :: n_Layers
    INTEGER,                   INTENT(IN)  :: SensorIndex
    INTEGER,         OPTIONAL, INTENT(IN)  :: SaveFWV
    ! Local variables
    INTEGER :: i, idx
    LOGICAL :: no_optran
    LOGICAL :: allocate_success


    ! Check input
    IF ( n_Layers < 1 ) RETURN


    ! Call the required procedure
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )


      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
         CALL ODAS_Predictor_Create( &
                self%ODAS                     , &
                n_Layers                      , &
                ODAS_MAX_N_PREDICTORS         , &
                ODAS_MAX_N_ABSORBERS          , &
                MAXVAL(TC%ODAS(idx)%Max_Order)  )
         allocate_success = ODAS_Predictor_Associated(self%ODAS)


      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        i = TC%ODPS(idx)%Group_Index
        ! ...Set OPTRAN flag
        no_optran = .NOT. ((TC%ODPS(idx)%n_OCoeffs > 0) .AND. ALLOW_OPTRAN)
        ! ...Allocate main structure
        CALL ODPS_Predictor_Create( &
               self%ODPS                   , &
               TC%ODPS(idx)%n_Layers       , &
               n_Layers                    , &
               ODPS_Get_n_Components(i)    , &
               ODPS_Get_max_n_Predictors(i), &
               No_OPTRAN = no_optran         )
        allocate_success = ODPS_Predictor_Associated(self%ODPS)
        ! ...Allocate memory for saved forward variables
        ! *****FLAW*****
        ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
        IF ( PRESENT(SaveFWV) .AND. ODPS_Get_SaveFWVFlag() ) THEN
        ! *****FLAW*****
          CALL PAFV_Create( &
                 self%ODPS%PAFV         , &
                 TC%ODPS(idx)%n_Layers  , &
                 n_Layers               , &
                 ODPS_Get_n_Absorbers(i), &
                 No_OPTRAN = no_optran    )
          allocate_success = allocate_success .AND. &
                             PAFV_Associated(self%ODPS%PAFV)
        END IF


      ! Predictors for SSU instrument specific model
      CASE( ODSSU_ALGORITHM )

        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )

          ! Predictors for ODAS SSU transmittance model
          CASE( ODAS_ALGORITHM )
            CALL ODAS_Predictor_Create( &
                   self%ODAS            , &
                   n_Layers             , &
                   ODAS_MAX_N_PREDICTORS, &
                   ODAS_MAX_N_ABSORBERS , &
                   ODAS_MAX_N_ORDERS      )
           allocate_success = ODAS_Predictor_Associated(self%ODAS)

          ! Predictors for ODPS SSU transmittance model
          CASE( ODPS_ALGORITHM )
            i = TC%ODSSU(idx)%ODPS(1)%Group_Index
            ! ...Set OPTRAN flag
            no_optran = .NOT. ((TC%ODSSU(idx)%ODPS(1)%n_OCoeffs > 0) .AND. ALLOW_OPTRAN)
            ! ...Allocate main structure
            CALL ODPS_Predictor_Create( &
                   self%ODPS                     , &
                   TC%ODSSU(idx)%ODPS(1)%n_Layers, &
                   n_Layers                      , &
                   ODPS_Get_n_Components(i)      , &
                   ODPS_Get_max_n_Predictors(i)  , &
                   No_OPTRAN = no_optran           )
            allocate_success = ODPS_Predictor_Associated(self%ODPS)
            ! ...Allocate memory for saved forward variables
            ! *****FLAW*****
            ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
            IF ( PRESENT(SaveFWV) .AND. ODPS_Get_SaveFWVFlag() ) THEN
            ! *****FLAW*****
              CALL PAFV_Create( &
                     self%ODPS%PAFV                , &
                     TC%ODSSU(idx)%ODPS(1)%n_Layers, &
                     n_Layers                      , &
                     ODPS_Get_n_Absorbers(i)       , &
                     No_OPTRAN = no_optran           )
              allocate_success = allocate_success .AND. &
                                 PAFV_Associated(self%ODPS%PAFV)
            END IF
        END SELECT
    END SELECT


    ! Check status
    IF ( .NOT. allocate_success ) RETURN


    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    Zeeman_Block: IF ( idx > 0 ) THEN
      i = TC%ODZeeman(idx)%Group_index
      ! ...Set OPTRAN flag
      no_optran = .TRUE.
      ! ...Allocate main structure
      CALL ODPS_Predictor_Create( &
             self%ODZeeman            , &
             TC%ODZeeman(idx)%n_Layers, &
             n_Layers                 , &
             Get_NumOfZComponents()   , &
             Get_NumOfZPredictors(i)  , &
             No_OPTRAN = no_optran      )
      allocate_success = ODPS_Predictor_Associated(self%ODZeeman)
      ! ...Allocate memory for saved forward variables
      ! *****FLAW*****
      ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
      IF ( PRESENT(SaveFWV) ) THEN
      ! *****FLAW*****
        CALL PAFV_Create( &
               self%ODZeeman%PAFV       , &
               TC%ODZeeman(idx)%n_Layers, &
               n_Layers                 , &
               Get_NumOfZAbsorbers()    , &
               No_OPTRAN = no_optran      )
        allocate_success = allocate_success .AND. &
                           PAFV_Associated(self%ODZeeman%PAFV)
      END IF
      ! Check status
      IF ( .NOT. allocate_success ) RETURN
    END IF Zeeman_Block


    ! Explicitly set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_Predictor_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Predictor_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM_Predictor object to stdout
!
! CALLING SEQUENCE:
!       CALL CRTM_Predictor_Inspect( Predictor )
!
! OBJECTSS:
!       Predictor:
!         Object to display.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Predictor_Inspect(self)
    TYPE(CRTM_Predictor_type), INTENT(IN) :: self
    WRITE(*,'(1x,"CRTM_Predictor CONTAINER OBJECT -- BEGIN")')
!    ! Release/version info
!    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Container objects
    IF ( CRTM_Predictor_Associated(self) ) THEN
      IF ( ODAS_Predictor_Associated(self%ODAS) ) CALL ODAS_Predictor_Inspect(self%ODAS)
      IF ( ODPS_Predictor_Associated(self%ODPS) ) CALL ODPS_Predictor_Inspect(self%ODPS)
      IF ( ODPS_Predictor_Associated(self%ODZeeman) ) CALL ODPS_Predictor_Inspect(self%ODZeeman)
!      IF ( ODZeeman_Predictor_Associated(self%ODZeeman) ) CALL ODZeeman_Predictor_Inspect(self%ODZeeman)
    END IF
    WRITE(*,'(1x,"CRTM_Predictor CONTAINER OBJECT -- END")')
  END SUBROUTINE CRTM_Predictor_Inspect



END MODULE CRTM_Predictor_Define

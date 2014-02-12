!
! CRTM_MoleculeScatter
!
! Module to compute molecule optical properties.
!       
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu, 03-Oct-2008
!                       Quanhua.Liu@noaa.gov
!

MODULE CRTM_MoleculeScatter

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters       , ONLY: ZERO
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type
  USE CRTM_AtmOptics_Define , ONLY: CRTM_AtmOptics_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Compute_MoleculeScatter
  PUBLIC :: CRTM_Compute_MoleculeScatter_TL
  PUBLIC :: CRTM_Compute_MoleculeScatter_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_MoleculeScatter.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Rayleigh factor
  REAL(fp),     PARAMETER :: RFACTOR = 27.0363_fp  ! = 287.0/9.8*923.1907/1000.0


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_MoleculeScatter
!
! PURPOSE:
!       Function to compute molecular scattering and extinction
!
! CALLING SEQUENCE:
!       Error_Status =  CRTM_Compute_MoleculeScatter( Wavenumber             , &  ! Input
!                                                     Atmosphere             , &  ! Input
!                                                     AtmOptics              , &  ! In/Output
!                                                     Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Wavenumber:      Spectral frequency
!                        UNITS:      Inverse centimetres (cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:      Structure containing the atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmOptics:       Structure containing the atmospheric optics data
!                        to which the molecular scattering and extinction
!                        component is added.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_MoleculeScatter( Wavenumber,     &  ! Input
                                         Atmosphere,     &  ! Input
                                         AtmOptics,      &  ! In/Output
                                         Message_Log )   &  ! Error messaging
                                      RESULT( Error_Status )
    ! Arguments
    REAL(fp),                   INTENT(IN)     :: Wavenumber
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atmosphere
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics 
    CHARACTER(*), OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_MoleculeScatter'

    ! Local variables
    INTEGER :: k
    REAL(fp) :: Wavelength , Opt_unit, Optical_Depth

    ! Setup
    ! -----
    Error_Status = SUCCESS
    
    ! Check input
    IF( Wavenumber > ZERO ) THEN
      Wavelength = Compute_Wavelength( Wavenumber )
    ELSE
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME,'Invalid wavenumber',Error_Status,Message_Log=Message_Log)
      RETURN
    END IF


    ! Calculate the scattering parameters
    ! -----------------------------------
    ! Compute optical scaling unit                                                                   
    CALL RAYLO(Wavelength, Opt_unit)

    ! Loop over atmospheric layers
    DO k = 1, Atmosphere%n_Layers
      Optical_Depth = RFACTOR*Opt_unit*(Atmosphere%Level_Pressure(k)-Atmosphere%Level_Pressure(k-1))
      AtmOptics%Optical_Depth(k)         = AtmOptics%Optical_Depth(k)         + Optical_Depth 
      AtmOptics%Single_Scatter_Albedo(k) = AtmOptics%Single_Scatter_Albedo(k) + Optical_Depth 

      ! The Rayleigh spherical expansion coefficients are constant.
      AtmOptics%Phase_Coefficient(2,1,k) = AtmOptics%Phase_Coefficient(2,1,k) + &
                                           0.25_fp * Optical_Depth 

! FOR FUTURE IMPLEMENTATION
!      ! Compute atmospheric polarisatin component
!      AtmOptics%Phase_Coefficient(2,2,k) = AtmOptics%Phase_Coefficient(2,2,k) + &
!                                           1.5_fp * Optical_Depth
!      AtmOptics%Phase_Coefficient(1,4,k) = AtmOptics%Phase_Coefficient(1,4,k) + &
!                                           0.75_fp * Optical_Depth 
!      AtmOptics%Phase_Coefficient(2,5,k) = AtmOptics%Phase_Coefficient(2,5,k) + &
!                                           0.612372_fp * Optical_Depth   
    END DO
  END FUNCTION CRTM_Compute_MoleculeScatter


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_MoleculeScatter_TL
!
! PURPOSE:
!       Function to compute the tangent-linear molecular scattering and
!       extinction
!
! CALLING SEQUENCE:
!       Error_Status =  CRTM_Compute_MoleculeScatter_TL( Wavenumber             , &  ! Input
!                                                        Atmosphere_TL          , &  ! Input
!                                                        AtmOptics_TL           , &  ! In/Output
!                                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Wavenumber:      Spectral frequency
!                        UNITS:      Inverse centimetres (cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:   Structure containing the tangent-linear atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmOptics_TL:    Structure containing the tangent-linear atmospheric
!                        optics data to which the molecular scattering and
!                        extinction component is added.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_MoleculeScatter_TL( Wavenumber,     &  ! Input
                                            Atmosphere_TL,  &  ! TL Input
                                            AtmOptics_TL,   &  ! TL Output
                                            Message_Log )   &  ! Error messaging
                                         RESULT( Error_Status )
    ! Arguments
    REAL(fp),                   INTENT(IN)     :: Wavenumber
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    CHARACTER(*), OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_MoleculeScatter_TL'
    ! Local variables
    INTEGER :: k
    REAL(fp) :: Wavelength , Opt_unit, Optical_Depth_TL

    ! Setup
    ! -----
    Error_Status = SUCCESS
    
    ! Check input
    IF( Wavenumber > ZERO ) THEN
      Wavelength = Compute_Wavelength( Wavenumber )
    ELSE
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME,'Invalid wavenumber',Error_Status,Message_Log=Message_Log)
      RETURN
    END IF

    ! Calculate the TL scattering parameters
    ! --------------------------------------
    CALL RAYLO(Wavelength, Opt_unit)
    DO k = 1, Atmosphere_TL%n_Layers
      Optical_Depth_TL = RFACTOR*Opt_unit*(Atmosphere_TL%Level_Pressure(k)-Atmosphere_TL%Level_Pressure(k-1))
      AtmOptics_TL%Optical_Depth(k)         = AtmOptics_TL%Optical_Depth(k)         + Optical_Depth_TL
      AtmOptics_TL%Single_Scatter_Albedo(k) = AtmOptics_TL%Single_Scatter_Albedo(k) + Optical_Depth_TL
      ! The Rayleigh spherical expansion coefficients are constant.
      AtmOptics_TL%Phase_Coefficient(2,1,k) = AtmOptics_TL%Phase_Coefficient(2,1,k) + &
                                              0.25_fp * Optical_Depth_TL
    END DO

  END FUNCTION CRTM_Compute_MoleculeScatter_TL


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_MoleculeScatter_AD
!
! PURPOSE:
!       Function to compute the molecular scattering and extinction adjoint.
!
! CALLING SEQUENCE:
!       Error_Status =  CRTM_Compute_MoleculeScatter_AD( Wavenumber             , &  ! Input
!                                                        AtmOptics_AD           , &  ! In/Output
!                                                        Atmosphere_AD          , &  ! Input
!                                                        Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Wavenumber:      Spectral frequency
!                        UNITS:      Inverse centimetres (cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:    Structure containing the adjoint atmospheric
!                        optics data from which the molecular scattering and
!                        extinction component is taken.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:   Structure containing the adjoint atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_MoleculeScatter_AD( Wavenumber,     &  ! Input
                                            AtmOptics_AD,   &  ! AD Input
                                            Atmosphere_AD,  &  ! AD Output
                                            Message_Log )   &  ! Error messaging
                                         RESULT( Error_Status )
    ! Arguments
    REAL(fp),                   INTENT(IN)     :: Wavenumber
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere_AD
    CHARACTER(*), OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_MoleculeScatter_AD'
    INTEGER :: k
    REAL(fp) :: Wavelength , Opt_unit, Optical_Depth_AD

    ! Setup
    ! -----
    Error_Status = SUCCESS
    
    ! Check input
    IF( Wavenumber > ZERO ) THEN
      Wavelength = Compute_Wavelength( Wavenumber )
    ELSE
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME,'Invalid wavenumber',Error_Status,Message_Log=Message_Log)
      RETURN
    END IF

    ! Calculate the AD scattering parameters
    ! --------------------------------------
    CALL RAYLO(Wavelength, Opt_unit)
    DO k = 1, Atmosphere_AD%n_Layers
      Optical_Depth_AD = AtmOptics_AD%Single_Scatter_Albedo(k)
      Optical_Depth_AD = Optical_Depth_AD + 0.25_fp * AtmOptics_AD%Phase_Coefficient(2,1,k)
      Atmosphere_AD%Level_Pressure(k)   = Atmosphere_AD%Level_Pressure(k)   + &
                                            RFACTOR*Opt_unit*Optical_Depth_AD
      Atmosphere_AD%Level_Pressure(k-1) = Atmosphere_AD%Level_Pressure(k-1) - &
                                            RFACTOR*Opt_unit*Optical_Depth_AD
    END DO
    
  END FUNCTION CRTM_Compute_MoleculeScatter_AD


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! ---------------------------------------------------------------------
  ! Simple function to convert wavenumber (cm^-1) to wavelength (microns)
  ! ---------------------------------------------------------------------
  FUNCTION Compute_Wavelength( Wavenumber ) RESULT( Wavelength )
    REAL(fp), INTENT(IN) :: Wavenumber
    REAL(fp) :: Wavelength
    REAL(fp), PARAMETER :: WFACTOR = 10000.0_fp
    Wavelength = WFACTOR/Wavenumber
  END FUNCTION Compute_Wavelength

  
  !--------------------------------------------------------------------
  !   OPTICAL DEPTH FOR Molecule SCATTERING
  ! ARGUMENTS -
  !     WE      R*8    IN      WAVELENGTH ( MICRO METER)
  !     RAYLO   R*8    OUT     OPTICAL DEPTH PER KM
  !--------------------------------------------------------------------
  SUBROUTINE RAYLO(WE, OPT)
     REAL(fp), INTENT(IN)  :: WE
     REAL(fp), INTENT(OUT) :: OPT
     REAL(fp), PARAMETER :: DELT = 0.0279_fp
     REAL(fp) :: X1, DY, X2, AS
     X1=1.0_fp/(WE*WE)
     AS=(6432.8_fp+2949810.0_fp/(146.0_fp-X1)+25540.0_fp/(41.0_fp-X1))*1.0e-08_fp + 1.0_fp
     X2=(AS*AS - 1.0_fp)**2
     DY = (6.0_fp+3.0_fp*DELT)/(6.0_fp-7.0_fp*DELT)
     OPT = X2*DY/(WE**4)
  END SUBROUTINE RAYLO

END MODULE CRTM_MoleculeScatter

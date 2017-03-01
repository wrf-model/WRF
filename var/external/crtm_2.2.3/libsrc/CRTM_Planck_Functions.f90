!
! CRTM_Planck_Functions
!
! Module containing the sensor Planck function routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Planck_Functions

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use statements
  USE Type_Kinds     , ONLY: fp
  USE CRTM_Parameters, ONLY: ONE
  USE CRTM_SpcCoeff  , ONLY: SC
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC  :: CRTM_Planck_Radiance
  PUBLIC  :: CRTM_Planck_Radiance_TL
  PUBLIC  :: CRTM_Planck_Radiance_AD
  PUBLIC  :: CRTM_Planck_Temperature
  PUBLIC  :: CRTM_Planck_Temperature_TL
  PUBLIC  :: CRTM_Planck_Temperature_AD


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Planck_Functions.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
    
    
CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Radiance
!
! PURPOSE:
!       Subroutine to calculate the instrument channel radiance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance( SensorIndex , &  ! Input
!                                  ChannelIndex, &  ! Input
!                                  Temperature , &  ! Input
!                                  Radiance      )  ! Output
!
! INPUT ARGUMENTS:
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
!       Temperature:     Temperature for which the Planck radiance is
!                        to be calculated.
!                        UNITS:      Kelvin, K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Radiance:        Channel Planck radiance.
!                        UNITS:      mW/(m^2.sr.cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       Temperature,
!
!         T_eff = bc1 + ( bc2 * T )
!
!       The sensor radiance is then calculated using the effective temperature:
!
!                       pc1
!         R = ------------------------
!              EXP( pc2 / T_eff ) - 1
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance( n, l       , &  ! Input
                                   Temperature, &  ! Input
                                   Radiance     )  ! Output
    ! Arguments
    INTEGER,  INTENT(IN)  :: n ! SensorIndex
    INTEGER,  INTENT(IN)  :: l ! ChannelIndex
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(OUT) :: Radiance
    ! Local variables
    REAL(fp) :: Effective_Temperature

    ! Apply the polychromaticity correction
    ! to obtain an effective temperature
    Effective_Temperature = SC(n)%Band_C1(l) + ( SC(n)%Band_C2(l) * Temperature )

    ! Calculate the Planck radiance
    Radiance =                  SC(n)%Planck_C1(l)  / &
    !          -----------------------------------------------------------
               ( EXP( SC(n)%Planck_C2(l) / Effective_Temperature ) - ONE )

  END SUBROUTINE CRTM_Planck_Radiance


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Radiance_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear instrument channel radiance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance_TL( SensorIndex   , &  ! Input
!                                     ChannelIndex  , &  ! Input
!                                     Temperature   , &  ! Input
!                                     Temperature_TL, &  ! Input
!                                     Radiance_TL     )  ! Output
!
! INPUT ARGUMENTS:
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
!       Temperature:     Temperature for which the tangent-linear Planck radiance
!                        is to be calculated.
!                        UNITS:      Kelvin, K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:  Tangent-linear temperature for which the tangent-linear
!                        Planck radiance is required.
!                        UNITS:      Kelvin, K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Radiance_TL:     Tangent-linear Planck radiance.
!                        UNITS:      mW/(m^2.sr.cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       temperature,
!
!         T_eff = bc1 + ( bc2 . T )
!
!       The sensor tangent-linear radiance is then calculated by first computing
!       the exponential term,
!
!          exponential = EXP( pc2 / T_eff )
!
!       and then the actual operator,
!
!                 pc1 . pc2 . bc1 . exponential
!         F = ------------------------------------
!              ( T_eff . ( exponential - 1 ) )^2
!
!       which is the derivate of the Planck equation wrt temperature. The
!       tangent-linear radiance is then determined by,
!
!         dR = F . dT
!
!       where dT is the input tangent-linear temperature.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance_TL( n, l          , &  ! Input
                                      Temperature   , &  ! Input
                                      Temperature_TL, &  ! Input
                                      Radiance_TL     )  ! Output
    ! Arguments
    INTEGER,  INTENT(IN)  :: n ! SensorIndex
    INTEGER,  INTENT(IN)  :: l ! ChannelIndex
    REAL(fp), INTENT(IN)  :: Temperature
    REAL(fp), INTENT(IN)  :: Temperature_TL
    REAL(fp), INTENT(OUT) :: Radiance_TL
    ! Local variables
    REAL(fp) :: Effective_Temperature
    REAL(fp) :: Exponential
    REAL(fp) :: F

    ! Apply the polychromaticity correction
    Effective_Temperature = SC(n)%Band_C1(l) + ( SC(n)%Band_C2(l) * Temperature )

    ! Calculate the Planck function operator
    !
    ! The exponential term
    Exponential = EXP( SC(n)%Planck_C2(l) / Effective_Temperature )
    ! The operator, call it F
    F = SC(n)%Planck_C1(l) * SC(n)%Planck_C2(l) * Exponential * SC(n)%Band_C2(l) / &
    !   ------------------------------------------------------------------------
                  ( Effective_Temperature * ( Exponential - ONE ) )**2

    ! Calculate the tangent-linear radiance
    Radiance_TL = F * Temperature_TL

  END SUBROUTINE CRTM_Planck_Radiance_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Radiance_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint instrument channel radiance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Radiance_AD( SensorIndex  , &  ! Input
!                                     ChannelIndex , &  ! Input
!                                     Temperature  , &  ! Input
!                                     Radiance_AD  , &  ! Input
!                                     Temperature_AD )  ! In/Output
!
! INPUT ARGUMENTS:
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature:    Temperature for which the tangent-linear Planck radiance
!                       is to be calculated.
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Radiance_AD:    Adjoint Planck radiance.
!                       UNITS:      mW/(m2.sr.cm-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Temperature_AD: Adjoint Planck temperature
!                       UNITS:      Kelvin
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The input adjoint radiance argument, Radiance_AD, is NOT set to zero
!       before returning to the calling routine.
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only radiances for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First a polychromatic correction is applied to give an effective
!       temperature,
!
!         T_eff = bc1 + ( bc2 . T )
!
!       The sensor tangent-linear radiance is then calculated by first computing
!       the exponential term,
!
!          exponential = EXP( pc2 / T_eff )
!
!       and then the actual operator,
!
!                 pc1 . pc2 . bc1 . exponential
!         F = ------------------------------------
!              ( T_eff . ( exponential - 1 ) )^2
!
!       which is the derivate of the Planck equation wrt temperature. The
!       adjoint temperature is then determined from,
!
!         T_AD = T_AD + ( F . R_AD )
!
!       where T_AD and R_AD on the LHS are the input adjoint temperature and
!       radiance respectively.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Radiance_AD( n, l         , &  ! Input
                                      Temperature  , &  ! Input
                                      Radiance_AD  , &  ! Input
                                      Temperature_AD )  ! In/Output
    ! Arguments
    INTEGER,  INTENT(IN)     :: n ! SensorIndex
    INTEGER,  INTENT(IN)     :: l ! ChannelIndex
    REAL(fp), INTENT(IN)     :: Temperature
    REAL(fp), INTENT(IN)     :: Radiance_AD
    REAL(fp), INTENT(IN OUT) :: Temperature_AD
    ! Local variables
    REAL(fp) :: Effective_Temperature
    REAL(fp) :: Exponential
    REAL(fp) :: F

    ! Apply the polychromaticity correction
    Effective_Temperature = SC(n)%Band_C1(l) + ( SC(n)%Band_C2(l) * Temperature )

    ! Calculate the Planck function operator
    !
    ! The exponential term
    Exponential = EXP( SC(n)%Planck_C2(l) / Effective_Temperature )
    ! The operator, call it F
    F = SC(n)%Planck_C1(l) * SC(n)%Planck_C2(l) * Exponential * SC(n)%Band_C2(l) / &
    !   ------------------------------------------------------------------------
                  ( Effective_Temperature * ( Exponential - ONE ) )**2

    ! Calculate the adjoint temperature
    Temperature_AD = Temperature_AD + ( F * Radiance_AD )

  END SUBROUTINE CRTM_Planck_Radiance_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Temperature
!
! PURPOSE:
!       Subroutine to calculate the instrument channel brightness temperature.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature( SensorIndex , &  ! Input
!                                     ChannelIndex, &  ! Input
!                                     Radiance    , &  ! Input
!                                     Temperature   )  ! Output
!
! INPUT ARGUMENTS:
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
!       Channel:         Channel index id. This is a unique index
!                        to a (supported) sensor channel.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Radiance:        Radiance for which the Planck temperature is desired.
!                        UNITS:      mW/(m^2.sr.cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Temperature:     Planck temperature.
!                        UNITS:      Kelvin, K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the effective temperature is calculated from the inverse Planck function,
!
!                        pc2
!         T_eff = ------------------
!                  LOG( pc1/R + 1 )
!
!       The polychromatic correction is then removed to provide the brightness
!       temperature,
!
!              T_eff - bc1
!         T = -------------
!                  bc2
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Temperature( n, l      , &  ! Input
                                      Radiance  , &  ! Input
                                      Temperature )  ! Output
    ! Arguments
    INTEGER,  INTENT(IN)  :: n ! SensorIndex
    INTEGER,  INTENT(IN)  :: l ! ChannelIndex
    REAL(fp), INTENT(IN)  :: Radiance
    REAL(fp), INTENT(OUT) :: Temperature
    ! Local variables
    REAL(fp) :: Effective_Temperature

    ! Calculate the effective temperature
    Effective_Temperature =              SC(n)%Planck_C2(l)  / &
    !                       ----------------------------------------------
                            LOG( ( SC(n)%Planck_C1(l) / Radiance ) + ONE )

    ! Apply the polychromatic correction to 
    ! obtain the true temperature
    Temperature = ( Effective_Temperature - SC(n)%Band_C1(l) ) / &
    !             --------------------------------------------
                                SC(n)%Band_C2(l)

  END SUBROUTINE CRTM_Planck_Temperature


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Temperature_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear instrument channel
!       brightness temperature.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature_TL( SensorIndex  , &  ! Input
!                                        ChannelIndex , &  ! Input
!                                        Radiance     , &  ! Input
!                                        Radiance_TL  , &  ! Input
!                                        Temperature_TL )  ! Output
!
! INPUT ARGUMENTS:
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Radiance:       Radiance at which the tangent-linear Planck temperature
!                       is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Radiance_TL:    Tangent-linear radiance for which the tangent-linear
!                       Planck temperature is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Temperature_TL: Tangent-linear Planck temperature.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the logarithm argument is calculated,
!
!         a = pc1/R + 1
!
!       The inverse Planck function operator is then calculated,
!
!                      pc1 . pc2
!         F = ------------------------------
!              bc2 . a . ( R . LOG( a ) )^2
!
!       and the tangent-linear temperature is then given by,
!
!         dT = F . dR
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Temperature_TL( n, l         , &  ! Input
                                         Radiance     , &  ! Input
                                         Radiance_TL  , &  ! Input
                                         Temperature_TL )  ! Output
    ! Arguments
    INTEGER,  INTENT(IN)  :: n ! SensorIndex
    INTEGER,  INTENT(IN)  :: l ! ChannelIndex
    REAL(fp), INTENT(IN)  :: Radiance
    REAL(fp), INTENT(IN)  :: Radiance_TL
    REAL(fp), INTENT(OUT) :: Temperature_TL
    ! Local variables
    REAL(fp) :: Argument
    REAL(fp) :: F

    ! Calculate the Planck function operator
    !
    ! The logarithm argument
    Argument = ( SC(n)%Planck_C1(l) / Radiance ) + ONE
    ! The operator, call it F
    F =            SC(n)%Planck_C1(l) * SC(n)%Planck_C2(l) / &
    !   -------------------------------------------------------------------
        ( SC(n)%Band_C2(l) * Argument * ( Radiance * LOG( Argument ) )**2 )

    ! Calculate the tangent-linear temperature
    Temperature_TL = F * Radiance_TL

  END SUBROUTINE CRTM_Planck_Temperature_TL



!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Planck_Temperature_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint instrument channel
!       brightness temperature.
!
! CALLING SEQUENCE:
!       CALL CRTM_Planck_Temperature_AD( SensorIndex   , &  ! Input
!                                        ChannelIndex  , &  ! Input
!                                        Radiance      , &  ! Input
!                                        Temperature_AD, &  ! Input
!                                        Radiance_AD     )  ! In/Output
!
! INPUT ARGUMENTS:
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Radiance:       Radiance at which the adjoint radiance is desired.
!                       UNITS:      mW/(m^2.sr.cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature_AD: Adjoint Planck temperature.
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Radiance_AD:    Adjoint radiance.
!                       UNITS:      K.m^2.sr.cm^-1/mW
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The input adjoint temperature argument, Temperature_AD, is NOT set to zero
!       before returning to the calling routine.
!
! RESTRICTIONS:
!       Spectral coefficients are obtained from the CRTM_SpcCoeff module
!       so only temperatures for those sensors which are included in the spectral
!       coefficient data file can be calculated.
!
! PROCEDURE:
!       First the logarithm argument is calculated,
!
!         a = pc1/R + 1
!
!       The inverse Planck function operator is then calculated,
!
!                      pc1 . pc2
!         F = ------------------------------
!              bc2 . a . ( R . LOG( a ) )^2
!
!       which is the derivate of the Planck temperature wrt radiance. The
!       adjoint radiance is then determined from,
!
!         R_AD = R_AD + ( F . T_AD )
!
!       where R_AD and T_AD on the LHS are the input adjoint radiance and
!       temperature respectively.
!
!       The bc1, bc2, pc1, and pc2 values are obtained from the 
!       CRTM_SpcCoeff module which is filled during the initialisation
!       phase.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Planck_Temperature_AD( n, l          , &  ! Input
                                         Radiance      , &  ! Input
                                         Temperature_AD, &  ! Input
                                         Radiance_AD     )  ! In/Output
    ! Arguments
    INTEGER,  INTENT(IN)     :: n ! SensorIndex
    INTEGER,  INTENT(IN)     :: l ! ChannelIndex
    REAL(fp), INTENT(IN)     :: Radiance
    REAL(fp), INTENT(IN)     :: Temperature_AD
    REAL(fp), INTENT(IN OUT) :: Radiance_AD
    ! Local variables
    REAL(fp) :: Argument
    REAL(fp) :: F

    ! Calculate the Planck function operator
    !
    ! The logarithm Argument
    Argument = ( SC(n)%Planck_C1(l) / Radiance ) + ONE
    ! The operator, call it F
    F =            SC(n)%Planck_C1(l) * SC(n)%Planck_C2(l) / &
    !   -------------------------------------------------------------------
        ( SC(n)%Band_C2(l) * Argument * ( Radiance * LOG( Argument ) )**2 )

    ! Calculate the adjoint radiance
    Radiance_AD = Radiance_AD + ( F * Temperature_AD )

  END SUBROUTINE CRTM_Planck_Temperature_AD

END MODULE CRTM_Planck_Functions

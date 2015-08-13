!
! CRTM_AtmOptics
!
! Application module for AtmOptics objects.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    quanhua.liu@noaa.gov
!                       Yong Han,       yong.han@noaa.gov
!                       Paul van Delst, paul.vandelst@noaa.gov
!                       08-Jun-2005
!

MODULE CRTM_AtmOptics

  ! ---------------
  ! Environment setup
  ! ---------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE CRTM_Parameters      , ONLY: ZERO, ONE, POINT_5, &
                                   MAX_N_LAYERS, &
                                   BS_THRESHOLD
  USE CRTM_AtmOptics_Define, ONLY: CRTM_AtmOptics_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibilities
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_AOVariables_type
  ! Procedures
  PUBLIC :: CRTM_Compute_Transmittance
  PUBLIC :: CRTM_Compute_Transmittance_TL
  PUBLIC :: CRTM_Compute_Transmittance_AD
  PUBLIC :: CRTM_Combine_AtmOptics
  PUBLIC :: CRTM_Combine_AtmOptics_TL
  PUBLIC :: CRTM_Combine_AtmOptics_AD


  ! ---------------
  ! Module parameters
  ! ---------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_AtmOptics.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  ! ------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------
  TYPE :: CRTM_AOVariables_type
    PRIVATE
    ! Used in Compute_Transmittance procedures
    REAL(fp) :: transmittance
    ! Used in Combine_AtmOptics procedures
    REAL(fp), DIMENSION(MAX_N_LAYERS) :: Optical_Depth
    REAL(fp), DIMENSION(MAX_N_LAYERS) :: bs
    REAL(fp), DIMENSION(MAX_N_LAYERS) :: w
  END TYPE CRTM_AOVariables_type


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
!       CRTM_Compute_Transmittance
!
! PURPOSE:
!       Subroutine to compute the total atmospheric transmittance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Transmittance( AtmOptics, Transmittance )
!
! INPUTS:
!       AtmOptics:      The atmospheric optical properties
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Transmittance:  The total atmospheric transmittance derived from
!                       the optical depth component of AtmOptics.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Transmittance( atmoptics, transmittance )
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: atmoptics
    REAL(fp)                 , INTENT(OUT) :: transmittance
    INTEGER :: k
    k = atmoptics%n_layers
    transmittance = EXP(-ONE*SUM(atmoptics%optical_depth(1:k)))
  END SUBROUTINE CRTM_Compute_Transmittance


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_Transmittance_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear total atmospheric transmittance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Transmittance_TL( AtmOptics   , &
!                                           AtmOptics_TL, &
!                                           Transmittance_TL )
!
! INPUTS:
!       AtmOptics:         The atmospheric optical properties
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_TL:      The tangent-linear atmospheric optical properties
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Transmittance_TL:  The tangent-linear of the total atmospheric
!                          transmittance derived from the optical depth
!                          component of AtmOptics.
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Transmittance_TL( &
    atmoptics       , &  ! Input
    atmoptics_TL    , &  ! Input
    transmittance_TL  )  ! Output
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: atmoptics
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: atmoptics_TL
    REAL(fp)                 , INTENT(OUT) :: transmittance_TL
    ! Local variables
    INTEGER :: k
    REAL(fp) :: transmittance

    k = atmoptics%n_layers
    transmittance    = EXP(-ONE*SUM(atmoptics%optical_depth(1:k)))
    transmittance_TL = -transmittance * SUM(atmoptics_TL%optical_depth(1:k))
  END SUBROUTINE CRTM_Compute_Transmittance_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_Transmittance_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the total atmospheric transmittance.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Transmittance_AD( AtmOptics       , &
!                                           Transmittance_AD, &
!                                           AtmOptics_AD      )
!
! INPUTS:
!       AtmOptics:         The atmospheric optical properties
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Transmittance_AD:  The adjoint of the total atmospheric transmittance.
!                          *** Set to ZERO upon exit ***
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       AtmOptics_AD:      The adjoint atmospheric optical properties.
!                          *** Must be defined upon input ***
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Transmittance_AD( &
    atmoptics       , &  ! Input
    transmittance_AD, &  ! Input
    atmoptics_AD      )  ! Output
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: atmoptics
    REAL(fp)                 , INTENT(IN OUT) :: transmittance_AD
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: atmoptics_AD
    ! Local variables
    INTEGER :: k
    REAL(fp) :: transmittance, t_delstar_t

    k = atmoptics%n_layers
    transmittance = EXP(-ONE*SUM(atmoptics%optical_depth(1:k)))
    t_delstar_t   = transmittance*transmittance_AD
    DO k = 1, atmoptics%n_layers
      atmoptics_AD%optical_depth(k) = atmoptics_AD%optical_depth(k) - t_delstar_t
    END DO
    transmittance_AD = ZERO
  END SUBROUTINE CRTM_Compute_Transmittance_AD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Combine_AtmOptics
!
! PURPOSE:
!       Subroutine to combine the optical properties from AtmAbsorption,
!       CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics( AtmOptics  , &
!                                    AOVariables  )
!
! OUTPUTS:
!       AtmOptics:      The combined atmospheric optical properties
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       AOVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AOVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics( &
    AtmOptics, &  ! Output
    AOV        )  ! Internal variable output
    ! Arguments
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN OUT) :: AtmOptics
    TYPE(CRTM_AOVariables_type), INTENT(IN OUT) :: AOV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics'
    ! Local variables
    INTEGER :: i, k, l

    ! Initialise scattering optical depth sum
    AtmOptics%Scattering_Optical_Depth = ZERO


    ! No scattering case
    IF( (.NOT. AtmOptics%Include_Scattering) .OR. &
        AtmOptics%n_Legendre_Terms == 0 ) RETURN


    ! Loop over atmospheric layers
    Layer_Loop: DO k = 1, AtmOptics%n_Layers


      ! Save the unmodified optical parameters
      AOV%Optical_Depth(k) = AtmOptics%Optical_Depth(k)
      AOV%bs(k)            = AtmOptics%Single_Scatter_Albedo(k)
      ! ...Initialise scattering dependent terms
      AOV%w(k) = ZERO


      ! Only proceed if the total optical depth is significant
      Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD ) THEN

        AOV%w(k) = AtmOptics%Single_Scatter_Albedo(k) / AtmOptics%Optical_Depth(k)
        DO i = 1, AtmOptics%n_Phase_Elements
          DO l = 1, AtmOptics%n_Legendre_Terms
            AtmOptics%Phase_Coefficient(l,i,k) = AtmOptics%Phase_Coefficient(l,i,k) / &
                                                 AtmOptics%Single_Scatter_Albedo(k)
          END DO
          ! ...Normalization requirement for energy conservation
          AtmOptics%Phase_Coefficient(0,i,k) = POINT_5
        END DO
        AtmOptics%Delta_Truncation(k) = AtmOptics%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)


        ! Redfine the total optical depth and single scattering
        ! albedo for the delta-function adjustment
        AtmOptics%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) )) * &
                                     AtmOptics%Optical_Depth(k)
        AtmOptics%Single_Scatter_Albedo(k) = ( ONE - AtmOptics%Delta_Truncation(k) ) * AOV%w(k)   / &
                                             ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

      END IF Significant_Scattering


      ! Compute the vertically integrated scattering optical depth
      AtmOptics%Scattering_Optical_Depth = AtmOptics%Scattering_Optical_Depth + &
                                           (AOV%w(k) * AOV%Optical_Depth(k))

    END DO Layer_Loop


  END SUBROUTINE CRTM_Combine_AtmOptics



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Combine_AtmOptics_TL
!
! PURPOSE:
!       Subroutine to combine the tangent-linear optical properties from
!       AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_TL( AtmOptics   , &
!                                       AtmOptics_TL, &
!                                       AOVariables   )
! INPUTS:
!       AtmOptics:         Combined atmospheric optical properties.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of this module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUT:
!       AtmOptics_TL:      Tangent-linear combined atmospheric optical properties.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_TL( &
    AtmOptics   , &  ! FWD Input
    AtmOptics_TL, &  ! TL Output
    AOV           )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_AtmOptics_type),   INTENT(IN)     :: AtmOptics
    TYPE(CRTM_AtmOptics_type),   INTENT(IN OUT) :: AtmOptics_TL
    TYPE(CRTM_AOVariables_type), INTENT(IN)     :: AOV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_TL'
    ! Local variables
    INTEGER :: i, k, l
    REAL(fp) :: optical_depth_TL
    REAL(fp) :: w_TL


    ! Initialise tangent-linear scattering optical depth sum
    AtmOptics_TL%Scattering_Optical_Depth = ZERO


    ! No scattering case
    IF( (.NOT. AtmOptics%Include_Scattering) .OR. &
        AtmOptics%n_Legendre_Terms == 0 ) RETURN


    ! Loop over atmospheric layers
    Layer_Loop: DO k = 1, AtmOptics%n_Layers


      ! Save the unmodified optical parameters
      optical_depth_TL = AtmOptics_TL%Optical_Depth(k)
      ! ...Initialise scattering dependent terms
      w_TL = ZERO


      ! Only proceed if the total optical depth is significant
      Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD ) THEN


        w_TL = ( AtmOptics_TL%Single_Scatter_Albedo(k) / AOV%Optical_Depth(k) ) - &
               ( AtmOptics_TL%Optical_Depth(k) * AOV%w(k) / AOV%Optical_Depth(k) )
        DO i = 1, AtmOptics%n_Phase_Elements
          DO l = 1, AtmOptics%n_Legendre_Terms
             AtmOptics_TL%Phase_Coefficient(l,i,k) = &
               ( AtmOptics_TL%Phase_Coefficient(l,i,k) - &
                 (AtmOptics%Phase_Coefficient(l,i,k) * AtmOptics_TL%Single_Scatter_Albedo(k)) ) / AOV%bs(k)
          END DO
          ! ...Normalization requirement for energy conservation
          AtmOptics_TL%Phase_Coefficient(0,i,k) = ZERO
        END DO
        AtmOptics_TL%Delta_Truncation(k) = AtmOptics_TL%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)


        ! Redefine the tangent-linear total optical depth and
        ! single scattering albedo for the delta-function adjustment
        !
        ! The expressions below are ordered to make the adjoint
        ! form easy to determine from the TL form.


        !  The optical depth
        !
        !    tau = ( 1 - d.w ) . tau
        !
        !  so,
        !
        !    tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
        !
        !  Note that the optical depth from the AOV structure is
        !  used on the RHS of these expressions.
        AtmOptics_TL%Optical_Depth(k) = &
            ( ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * AtmOptics_TL%Optical_Depth(k) ) - &
            ( AtmOptics%Delta_Truncation(k) * AOV%Optical_Depth(k) * w_TL ) - &
            ( AOV%w(k) * AOV%Optical_Depth(k) * AtmOptics_TL%Delta_Truncation(k) )


        !  The single scatter albedo, SSA
        !
        !         (1 - d).w
        !  SSA = -----------
        !          1 - d.w
        !
        !  so,
        !
        !            ( 1 - d + SSA.d )              ( SSA - 1 ).w
        !  SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
        !                1 - d.w                       1 - d.w
        !
        AtmOptics_TL%Single_Scatter_Albedo(k) = &
          ( ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * w_TL ) + &
            ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_TL%Delta_Truncation(k) ) ) / &
          ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

      END IF Significant_Scattering


      ! Compute the tangent-linear vertically integrated scattering optical depth
      AtmOptics_TL%Scattering_Optical_Depth = AtmOptics_TL%Scattering_Optical_Depth + &
                                              (AOV%w(k) * optical_depth_TL) + &
                                              (AOV%Optical_Depth(k) * w_TL)

    END DO Layer_Loop

  END SUBROUTINE CRTM_Combine_AtmOptics_TL



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Combine_AtmOptics_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_AD( AtmOptics,    &
!                                       AtmOptics_AD, &
!                                       AOVariables   )
!
! INPUTS:
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:      Structure containing the combined adjoint atmospheric
!                          optical parameters.
!                          NOTE: The components of this structures are all zeroed
!                                upon exit from this routine.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_AD( &
    AtmOptics   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    AOV           )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN)     :: AtmOptics
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN OUT) :: AtmOptics_AD
    TYPE(CRTM_AOVariables_type), INTENT(IN)     :: AOV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_AD'
    ! Local variables
    INTEGER :: i, k, l
    REAL(fp) :: w_AD


    ! No scattering case
    IF( (.NOT. AtmOptics%Include_Scattering) .OR. &
        AtmOptics%n_Legendre_Terms == 0 ) RETURN

    ! Begin layer loop
    Layer_Loop: DO k = AtmOptics%n_Layers, 1, -1

      w_AD  =  ZERO


      ! Only proceed if the scattering is significant
      Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD) THEN


        ! Compute the adjoint total optical depth and single
        ! scattering albedo for the delta function adjustment

        !  The tangent-linear single scatter albedo, SSA_TL
        !
        !              ( 1 - d + SSA.d )              ( SSA - 1 ).w
        !    SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
        !                   1 - d.w                      1 - d.w
        !
        !  so,
        !                    ( SSA - 1 ).w
        !    d_AD = d_AD + ---------------- . SSA_AD
        !                       1 - d.w
        !
        !                   ( 1 - d + SSA.d )
        !    w_AD = w_AD + ------------------- . SSA_AD
        !                        1 - d.w
        !
        !    SSA_AD = 0

        AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) + &
          ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_AD%Single_Scatter_Albedo(k) / &
            ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

        w_AD = w_AD + ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                          ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * &
                        AtmOptics_AD%Single_Scatter_Albedo(k) / &
                        ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

        AtmOptics_AD%Single_Scatter_Albedo(k) = ZERO


        !  The tangent-linear optical depth, tau_TL
        !
        !    tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
        !
        !  so,
        !
        !    d_AD = d_AD - w.tau.tau_AD
        !
        !    w_AD = w_AD - d.tau.tau_AD
        !
        !    tau_AD = ( 1 - d.w ).tau_AD
        !
        !  Note that the optical depth from the AOV structure is
        !  used on the RHS of the above expressions.

        AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) - &
          ( AOV%w(k)                    * &  ! w
            AOV%Optical_Depth(k)        * &  ! tau
            AtmOptics_AD%Optical_Depth(k) )  ! tau_AD

        w_AD = w_AD - ( AtmOptics%Delta_Truncation(k) * &  ! d
                        AOV%Optical_Depth(k)          * &  ! tau
                        AtmOptics_AD%Optical_Depth(k)   )  ! tau_AD

        AtmOptics_AD%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * &
                                        AtmOptics_AD%Optical_Depth(k)


        !  Delta truncation adjoint
        l = AtmOptics%n_Legendre_Terms
        AtmOptics_AD%Phase_Coefficient(l,1,k) = AtmOptics_AD%Phase_Coefficient(l,1,k) + &
                                                AtmOptics_AD%Delta_Truncation(k)
        AtmOptics_AD%Delta_Truncation(k) = ZERO

        DO i = 1, AtmOptics%n_Phase_Elements
          ! Normalization requirement for energy conservation
          AtmOptics_AD%Phase_Coefficient(0,i,k) = ZERO
          DO l = 1, AtmOptics%n_Legendre_Terms
             AtmOptics_AD%Single_Scatter_Albedo(k) = AtmOptics_AD%Single_Scatter_Albedo(k) - &
               AtmOptics%Phase_Coefficient(l,i,k)*AtmOptics_AD%Phase_Coefficient(l,i,k)/AOV%bs(k)
             AtmOptics_AD%Phase_Coefficient(l,i,k) = ( AtmOptics_AD%Phase_Coefficient(l,i,k)/AOV%bs(k) )
          END DO
        END DO

        AtmOptics_AD%Single_Scatter_Albedo(k) = AtmOptics_AD%Single_Scatter_Albedo(k) + &
                                                w_AD / AOV%Optical_Depth(k)
        AtmOptics_AD%Optical_Depth(k) = AtmOptics_AD%Optical_Depth(k) - &
                                        w_AD*AOV%w(k) / AOV%Optical_Depth(k)

      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Combine_AtmOptics_AD

END MODULE CRTM_AtmOptics

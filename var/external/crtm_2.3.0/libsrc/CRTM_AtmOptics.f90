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
                                   BS_THRESHOLD, &
                                   SCATTERING_ALBEDO_THRESHOLD
  USE CRTM_AtmOptics_Define, ONLY: CRTM_AtmOptics_type      , &
                                   CRTM_AtmOptics_Associated, &
                                   CRTM_AtmOptics_Create    , &
                                   CRTM_AtmOptics_Zero
  ! Internal variable definition module
  USE AOvar_Define, ONLY: AOvar_type, &
                          AOvar_Associated, &
                          AOvar_Destroy   , &
                          AOvar_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibilities
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: AOvar_type
  ! Procedures
  PUBLIC :: AOvar_Create

  PUBLIC :: CRTM_No_Scattering
  PUBLIC :: CRTM_Include_Scattering

  PUBLIC :: CRTM_Compute_Transmittance
  PUBLIC :: CRTM_Compute_Transmittance_TL
  PUBLIC :: CRTM_Compute_Transmittance_AD

  PUBLIC :: CRTM_AtmOptics_Combine
  PUBLIC :: CRTM_AtmOptics_Combine_TL
  PUBLIC :: CRTM_AtmOptics_Combine_AD

  PUBLIC :: CRTM_AtmOptics_NoScatterCopy
  PUBLIC :: CRTM_AtmOptics_NoScatterCopy_TL
  PUBLIC :: CRTM_AtmOptics_NoScatterCopy_AD


  ! ---------------
  ! Module parameters
  ! ---------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_AtmOptics.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


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
!       CRTM_No_Scattering
!
! PURPOSE:
!       Pure function to determine if scattering calculations will NOT be 
!       performed.
!
! CALLING SEQUENCE:
!       result = CRTM_No_Scattering( AtmOptics )
!
! INPUTS:
!       AtmOptics: The atmospheric optical properties
!                  UNITS:      N/A
!                  TYPE:       CRTM_AtmOptics_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Returns
!                    TRUE -  if the maximum single scatter albedo profile
!                            value is less than or equal to the scattering
!                            albedo threshold, OR if the user explicitly
!                            disbles scattering via the Include_Scattering
!                            option.
!                    FALSE - otherwise.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CRTM_No_Scattering(atmoptics) RESULT(no_scattering)
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: atmoptics
    LOGICAL :: no_scattering
    no_scattering = .NOT. CRTM_Include_Scattering(atmoptics)
  END FUNCTION CRTM_No_Scattering


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Include_Scattering
!
! PURPOSE:
!       Pure function to determine if scattering calculations will be 
!       performed.
!
! CALLING SEQUENCE:
!       result = CRTM_Include_Scattering( AtmOptics )
!
! INPUTS:
!       AtmOptics: The atmospheric optical properties
!                  UNITS:      N/A
!                  TYPE:       CRTM_AtmOptics_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Returns
!                    TRUE -  if the maximum single scatter albedo profile
!                            value is greater than the scattering albedo
!                            threshold, AND if the user has NOT explicitly
!                            disbled scattering via the Include_Scattering
!                            option.
!                    FALSE - otherwise.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CRTM_Include_Scattering(atmoptics) RESULT(include_scattering)
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: atmoptics
    LOGICAL :: include_scattering
    ! This test is different from previous incarnations in that before the 
    !   test turned OFF scattering if the AtmOptics single scatter albedo was
    !   LESS THAN the threshold (i.e. greater than or equal to).
    ! Now we include scattering if the AtmOptics single scatter albedo is
    !   GREATER THAN (but NOT equal to) the threshold.
    ! Confused? Sorry.
    include_scattering = (MAXVAL(atmoptics%Single_Scatter_Albedo) > SCATTERING_ALBEDO_THRESHOLD) .AND. &
                         atmoptics%Include_Scattering
  END FUNCTION CRTM_Include_Scattering
  
  
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
!       CRTM_AtmOptics_Combine
!
! PURPOSE:
!       Subroutine to combine the optical properties from AtmAbsorption,
!       CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Combine( AtmOptics, &
!                                    AOvar      )
!
! OUTPUTS:
!       AtmOptics:      The combined atmospheric optical properties
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       AOvar:          Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       AOvar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_AtmOptics_Combine( &
    AtmOptics, &  ! Output
    AOvar      )  ! Internal variable output
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics
    TYPE(AOvar_type)         , INTENT(IN OUT) :: AOvar
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine'
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
      AOvar%Optical_Depth(k) = AtmOptics%Optical_Depth(k)
      AOvar%bs(k)            = AtmOptics%Single_Scatter_Albedo(k)
      ! ...Initialise scattering dependent terms
      AOvar%w(k) = ZERO


      ! Only proceed if the total optical depth is significant
      Significant_Scattering: IF( AOvar%bs(k) > BS_THRESHOLD ) THEN

        AOvar%w(k) = AtmOptics%Single_Scatter_Albedo(k) / AtmOptics%Optical_Depth(k)
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
        AtmOptics%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) )) * &
                                     AtmOptics%Optical_Depth(k)
        AtmOptics%Single_Scatter_Albedo(k) = ( ONE - AtmOptics%Delta_Truncation(k) ) * AOvar%w(k)   / &
                                             ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) )

      END IF Significant_Scattering


      ! Compute the vertically integrated scattering optical depth
      AtmOptics%Scattering_Optical_Depth = AtmOptics%Scattering_Optical_Depth + &
                                           (AOvar%w(k) * AOvar%Optical_Depth(k))

    END DO Layer_Loop


  END SUBROUTINE CRTM_AtmOptics_Combine



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Combine_TL
!
! PURPOSE:
!       Subroutine to combine the tangent-linear optical properties from
!       AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Combine_TL( AtmOptics   , &
!                                       AtmOptics_TL, &
!                                       AOvar         )
! INPUTS:
!       AtmOptics:         Combined atmospheric optical properties.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AOvar:             Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of this module.
!                          UNITS:      N/A
!                          TYPE:       AOvar_type
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

  SUBROUTINE CRTM_AtmOptics_Combine_TL( &
    AtmOptics   , &  ! FWD Input
    AtmOptics_TL, &  ! TL Output
    AOvar         )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: AtmOptics
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    TYPE(AOvar_type)         , INTENT(IN)     :: AOvar
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine_TL'
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
      Significant_Scattering: IF( AOvar%bs(k) > BS_THRESHOLD ) THEN


        w_TL = ( AtmOptics_TL%Single_Scatter_Albedo(k) / AOvar%Optical_Depth(k) ) - &
               ( AtmOptics_TL%Optical_Depth(k) * AOvar%w(k) / AOvar%Optical_Depth(k) )
        DO i = 1, AtmOptics%n_Phase_Elements
          DO l = 1, AtmOptics%n_Legendre_Terms
             AtmOptics_TL%Phase_Coefficient(l,i,k) = &
               ( AtmOptics_TL%Phase_Coefficient(l,i,k) - &
                 (AtmOptics%Phase_Coefficient(l,i,k) * AtmOptics_TL%Single_Scatter_Albedo(k)) ) / AOvar%bs(k)
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
        !  Note that the optical depth from the AOvar structure is
        !  used on the RHS of these expressions.
        AtmOptics_TL%Optical_Depth(k) = &
            ( ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) ) * AtmOptics_TL%Optical_Depth(k) ) - &
            ( AtmOptics%Delta_Truncation(k) * AOvar%Optical_Depth(k) * w_TL ) - &
            ( AOvar%w(k) * AOvar%Optical_Depth(k) * AtmOptics_TL%Delta_Truncation(k) )


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
            ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOvar%w(k) * AtmOptics_TL%Delta_Truncation(k) ) ) / &
          ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) )

      END IF Significant_Scattering


      ! Compute the tangent-linear vertically integrated scattering optical depth
      AtmOptics_TL%Scattering_Optical_Depth = AtmOptics_TL%Scattering_Optical_Depth + &
                                              (AOvar%w(k) * optical_depth_TL) + &
                                              (AOvar%Optical_Depth(k) * w_TL)

    END DO Layer_Loop

  END SUBROUTINE CRTM_AtmOptics_Combine_TL



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Combine_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Combine_AD( AtmOptics,    &
!                                       AtmOptics_AD, &
!                                       AOvar   )
!
! INPUTS:
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AOvar:             Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       AOvar_type
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

  SUBROUTINE CRTM_AtmOptics_Combine_AD( &
    AtmOptics   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    AOvar         )  ! Internal variable input
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: AtmOptics
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(AOvar_type)         , INTENT(IN)     :: AOvar
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine_AD'
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
      Significant_Scattering: IF( AOvar%bs(k) > BS_THRESHOLD) THEN


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
          ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOvar%w(k) * AtmOptics_AD%Single_Scatter_Albedo(k) / &
            ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) ) )

        w_AD = w_AD + ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                          ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * &
                        AtmOptics_AD%Single_Scatter_Albedo(k) / &
                        ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) ) )

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
        !  Note that the optical depth from the AOvar structure is
        !  used on the RHS of the above expressions.

        AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) - &
          ( AOvar%w(k)                    * &  ! w
            AOvar%Optical_Depth(k)        * &  ! tau
            AtmOptics_AD%Optical_Depth(k) )  ! tau_AD

        w_AD = w_AD - ( AtmOptics%Delta_Truncation(k) * &  ! d
                        AOvar%Optical_Depth(k)          * &  ! tau
                        AtmOptics_AD%Optical_Depth(k)   )  ! tau_AD

        AtmOptics_AD%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOvar%w(k) ) ) * &
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
               AtmOptics%Phase_Coefficient(l,i,k)*AtmOptics_AD%Phase_Coefficient(l,i,k)/AOvar%bs(k)
             AtmOptics_AD%Phase_Coefficient(l,i,k) = ( AtmOptics_AD%Phase_Coefficient(l,i,k)/AOvar%bs(k) )
          END DO
        END DO

        AtmOptics_AD%Single_Scatter_Albedo(k) = AtmOptics_AD%Single_Scatter_Albedo(k) + &
                                                w_AD / AOvar%Optical_Depth(k)
        AtmOptics_AD%Optical_Depth(k) = AtmOptics_AD%Optical_Depth(k) - &
                                        w_AD*AOvar%w(k) / AOvar%Optical_Depth(k)

      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_AtmOptics_Combine_AD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_NoScatterCopy
!
! PURPOSE:
!       Function to copy an instance of a CRTM AtmOptics object
!       but without the scattering information included.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
!
! INPUTS:
!       AtmOptics:       AtmOptics object to copy
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmOptics_Clear: Copy of the input AtmOptics object but without the
!                        scattering information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AtmOptics_NoScatterCopy( ao, ao_clear ) RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao
    TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: ao_clear
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy'
    ! Local variables
    CHARACTER(ML) :: err_msg


    ! Set up
    err_stat = SUCCESS
    ! ...Check input
    IF ( .NOT. CRTM_AtmOptics_Associated(ao) ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics structure not allocated'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF


    ! Create the output structure
    CALL CRTM_AtmOptics_Create( ao_clear   , &
                                ao%n_Layers, &
                                0          , &  ! No Legendre terms
                                0            )  ! No phase element terms
    IF ( .NOT. CRTM_AtmOptics_Associated(ao_clear) ) THEN
      err_stat = FAILURE
      err_msg = 'Error allocating output Clear-Sky AtmOptics structure'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF


    ! Set/Copy over the clear-sky data
    ao_clear%Include_Scattering = .FALSE.
    ao_clear%Optical_Depth      = ao%Optical_Depth(1:ao%n_Layers)

  END FUNCTION CRTM_AtmOptics_NoScatterCopy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_NoScatterCopy_TL
!
! PURPOSE:
!       Function to copy an instance of a tangent-linear CRTM AtmOptics object
!       but without the scattering information included.
!!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_NoScatterCopy_TL( ao, ao_TL, ao_clear_TL )
!
! INPUTS:
!       ao:              Forward AtmOptics object for consistency checking
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ao_TL:           Tangent-linear AtmOptics object to copy. This object
!                        must be the tangent-linear equivalent of the input
!                        forward AtmOptics object.
!                        This
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ao_clear_TL:     Copy of the input AtmOptics tangent-linear object but
!                        without scattering information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AtmOptics_NoScatterCopy_TL( &
    ao         , &  ! FWD input
    ao_TL      , &  ! TL  input
    ao_clear_TL) &  ! TL  output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao_TL
    TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: ao_clear_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy_TL'
    ! Local variables
    CHARACTER(ML) :: err_msg


    ! Set up
    err_stat = SUCCESS
    ! ...Check input allocation
    IF ( .NOT. CRTM_AtmOptics_Associated(ao   ) .OR. &
         .NOT. CRTM_AtmOptics_Associated(ao_TL) ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics structures not allocated'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF
    ! ...Dimension consistency
    IF ( (ao%n_Layers         /= ao_TL%n_Layers        ) .OR. &
         (ao%n_Legendre_Terms /= ao_TL%n_Legendre_Terms) .OR. &
         (ao%n_Phase_Elements /= ao_TL%n_Phase_Elements) ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics structures have incongruent dimensions'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF


    ! Create the output structure
    CALL CRTM_AtmOptics_Create( ao_clear_TL   , &
                                ao_TL%n_Layers, &
                                0             , &  ! No Legendre terms
                                0               )  ! No phase element terms
    IF ( .NOT. CRTM_AtmOptics_Associated(ao_clear_TL) ) THEN
      err_stat = FAILURE
      err_msg = 'Error allocating output Clear-Sky AtmOptics structure'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF


    ! Set/Copy over the clear-sky data
    ao_clear_TL%Include_Scattering = .FALSE.
    ao_clear_TL%Optical_Depth      = ao_TL%Optical_Depth(1:ao_TL%n_Layers)

  END FUNCTION CRTM_AtmOptics_NoScatterCopy_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_NoScatterCopy_AD
!
! PURPOSE:
!       Function to perform the adjoint copy of an instance of the CRTM
!       AtmOptics object without the scattering information included.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( ao, ao_clear_AD, ao_AD )
!
! INPUTS:
!       ao:              AtmOptics object for consistency checking
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ao_clear_AD:     Adjoint Clear-Sky AtmOptics structure to copy
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       ao_AD:           Adjoint copy of the input AtmOptics. This object
!                        must be the adjoint equivalent of the input
!                        forward AtmOptics object.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AtmOptics_NoScatterCopy_AD( &
    ao         , &  ! FWD input
    ao_clear_AD, &  ! AD  input
    ao_AD      ) &  ! AD  output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: ao
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: ao_clear_AD
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: ao_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy_AD'
    ! Local variables
    CHARACTER(ML) :: err_msg
    INTEGER :: k


    ! Set up
    err_stat = SUCCESS
    ! ...Check input allocation
    IF ( .NOT. CRTM_AtmOptics_Associated(ao         ) .OR. &
         .NOT. CRTM_AtmOptics_Associated(ao_clear_AD) .OR. &
         .NOT. CRTM_AtmOptics_Associated(ao_AD      ) ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics structures not allocated'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF
    ! ...Dimensional consistency
    IF ( (ao%n_Layers         /= ao_AD%n_Layers        ) .OR. &
         (ao%n_Legendre_Terms /= ao_AD%n_Legendre_Terms) .OR. &
         (ao%n_Phase_Elements /= ao_AD%n_Phase_Elements) ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics and AtmOptics_AD structures have incongruent dimensions'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF
    IF ( ao_clear_AD%n_Layers /= ao_AD%n_Layers ) THEN
      err_stat = FAILURE
      err_msg = 'Input AtmOptics_Clear_AD structure has incongruent dimensions'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF
    ! ...Non-layer dependent data consistency
    IF ( (ao%Include_Scattering .NEQV. ao_AD%Include_Scattering) .OR. &
         ao_clear_AD%Include_Scattering ) THEN
      err_stat = FAILURE
      err_msg = 'AtmOptics structures have incongruent Scattering flags'
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
      RETURN
    END IF


    ! Adjoint copy of data
    k = ao%n_Layers
    ao_AD%Optical_Depth(1:k) = ao_AD%Optical_Depth(1:k) + ao_clear_AD%Optical_Depth(1:k)      


    ! Zero the clear result, as it has no more impact
    CALL CRTM_AtmOptics_Zero( ao_clear_AD )

  END FUNCTION CRTM_AtmOptics_NoScatterCopy_AD

END MODULE CRTM_AtmOptics

!
! SOI_Module
!
! Module containing the Successive Order of Interation (SOI) radiative
! transfer solution procedures used in the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Tom Greenwald, CIMSS/SSEC; tom.greenwald@ssec.wisc.edu
!                       Paul van Delst;            paul.vandelst@noaa.gov
!                       20-Jan-2010

MODULE SOI_Module

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: SET, ZERO, ONE, TWO, PI, &
                             MAX_N_LAYERS, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, &
                             DEGREES_TO_RADIANS, &
                             SECANT_DIFFUSIVITY, &
                             SCATTERING_ALBEDO_THRESHOLD, &
                             OPTICAL_DEPTH_THRESHOLD
  USE CRTM_Utility
  USE RTV_Define     , ONLY: RTV_type, &
                             DELTA_OPTICAL_DEPTH, &
                             MAX_N_DOUBLING, &
                             MAX_N_SOI_ITERATIONS
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: CRTM_SOI
  PUBLIC :: CRTM_SOI_TL
  PUBLIC :: CRTM_SOI_AD
  PUBLIC :: CRTM_SOI_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: SOI_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

   SUBROUTINE CRTM_SOI(n_Layers, & ! Input  number of atmospheric layers
                              w, & ! Input  layer scattering albedo
                           T_OD, & ! Input  layer optical depth
              cosmic_background, & ! Input  cosmic background radiance
                     emissivity, & ! Input  surface emissivity
                   reflectivity, & ! Input  surface reflectivity matrix 
                Index_Sat_Angle, & ! Input  satellite angle index 
                            RTV)   ! IN/Output upward radiance and others
! ------------------------------------------------------------------------- !
!                                                                           !
! FUNCTION:                                                                 !
!                                                                           !
!   This subroutine calculates IR/MW radiance at the top of the atmosphere  !
!   including multiple scattering using the SOI (Successive Order of        !
!   Interaction) algorithm, which combines the Successive Order of          !
!   Scattering and doubling methods.                                        !
!                                                                           ! 
!   Written by Tom Greenwald    tomg@ssec.wisc.edu                          !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w, T_OD
      REAL (fp), INTENT(IN) ::  cosmic_background
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      INTEGER, INTENT( IN ) :: Index_Sat_Angle
      TYPE(RTV_type), INTENT( INOUT ) :: RTV

   ! -------------- internal variables --------------------------------- !

      INTEGER :: i, k, niter
      REAL(fp) :: rad, rad_change
      REAL(fp), PARAMETER :: initial_error = 1.E10   
      REAL(fp), PARAMETER :: SMALL = 1.E-15   
      REAL(fp), PARAMETER :: SNGL_SCAT_ALB_THRESH = 0.8
      REAL(fp), PARAMETER :: OPT_DEPTH_THRESH = 4.0
      REAL(fp) :: radiance_thresh
      REAL(fp), DIMENSION( MAX_N_ANGLES ) :: source
  
      ! Precompute layer R/T matrices and thermal sources
      DO k = 1, n_Layers
        ! Precompute simple layer properties
        RTV%e_Layer_Trans( 1 : RTV%n_Angles, k ) = EXP( -T_OD( k ) / RTV%COS_Angle( 1 : RTV%n_Angles ) )
        
        IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD ) THEN 
          IF ( w( k ) < SNGL_SCAT_ALB_THRESH .AND. T_OD( k ) < OPT_DEPTH_THRESH ) THEN 
            CALL CRTM_Truncated_Doubling( RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ), & ! Input
                                          RTV%COS_Angle, RTV%COS_Weight, RTV%Pff( :, :, k ), & ! Input
                                          RTV%Pbb( :, :, k ), RTV%Planck_Atmosphere( k ),    & ! Input 
                                          RTV )                       ! Output
          ELSE
            CALL CRTM_Doubling_Layer( RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ), & ! Input
                                      RTV%COS_Angle, RTV%COS_Weight, RTV%Pff( :, :, k ), & ! Input
                                      RTV%Pbb( :, :, k ), RTV%Planck_Atmosphere( k ),    & ! Input 
                                      RTV )                       ! Output
          END IF

! Subtract out direct transmission
          DO i = 1, RTV%n_angles
            RTV%s_Layer_Trans( i, i, k ) = RTV%s_Layer_Trans( i, i, k ) - RTV%e_layer_Trans( i, k )
          END DO

        ELSE       !   Thermal sources
          DO i = 1, RTV%n_Angles
            RTV%s_Layer_Source_UP( i, k ) = RTV%Planck_Atmosphere( k ) * ( ONE - RTV%e_Layer_Trans( i, k ) )
            RTV%s_Layer_Source_DOWN( i, k ) = RTV%s_Layer_Source_UP( i, k )
          END DO 
        END IF
      END DO

    !------------------------------------
    !  Arrays that *must* be zeroed out
    !------------------------------------
      RTV%s_level_Rad_UP( Index_Sat_Angle, 0 ) = ZERO
      
      IF ( RTV%Number_SOI_Iter > 0 ) THEN
        RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO
        RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO
      ELSE
        RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, 0 : n_Layers, 1 ) = ZERO
        RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, 0 : n_Layers, 1 ) = ZERO
      END IF
      
    !---------------------------------
    ! Set initial/boundary conditions      
    !---------------------------------
      RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, 0, 1 ) = cosmic_background
      niter = 0                       ! Set that we're on the zeroth order of scatter
      rad_change = initial_error      ! SOI loop uses this quantity to know when to stop iteration
      rad = SMALL
!      ACCEL = .FALSE. ! Turn on convergence acceleration

!      IF ( ACCEL ) THEN
!        radiance_thresh = 5.E-7
!        radsum = 0.
!        q_old = 0.
!      ELSE
         radiance_thresh = 1.E-4
!      END IF
    !-----------------------------------------
    ! This is the Order of Interaction loop
    !-----------------------------------------
      soi_loop: DO WHILE ( ( ( rad_change > radiance_thresh ) .AND. ( ( niter + 1 ) <= MAX_N_SOI_ITERATIONS ) ) .OR. ( niter < 2 ) )

        niter = niter + 1   ! Note: niter = 1 is the zeroth order of interaction

        IF ( niter > 1 ) RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, 0, niter ) = ZERO  ! No cosmic background contribution for 
                                                                                        ! higher orders of interaction  
        !---------------------------------------------
        ! Integrate down through atmospheric layers
        !--------------------------------------------- 
        layersdn_loop: DO k = 1, n_Layers
        
          IF ( niter == 1 ) THEN
            source( 1 : RTV%n_Angles ) = RTV%s_Layer_Source_DOWN( 1 : RTV%n_Angles, k )
          ELSE
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN
!              DO j = 1, RTV%n_Angles
!                source( j ) = ZERO
!                DO i = 1, RTV%n_Angles  ! Integrate over angle
!                  source( j ) = source( j ) + RTV%s_Layer_Trans( j, i, k ) * RTV%s_Level_IterRad_DOWN( i, k - 1, niter - 1 ) &
!                                            + RTV%s_Layer_Refl( j, i, k ) * RTV%s_Level_IterRad_UP( i, k, niter - 1 ) 
!                END DO
!              END DO

              source( 1 : RTV%n_Angles ) = matmul( RTV%s_Layer_Trans( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                           RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, niter - 1 ) ) + &
                                           matmul( RTV%s_Layer_Refl( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                             RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, niter - 1 ) )

            ELSE
              source( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
          RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k, niter ) = RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, niter ) &
                                                                     * RTV%e_Layer_Trans( 1 : RTV%n_Angles, k ) &
                                                                     + source( 1 : RTV%n_Angles )

        END DO layersdn_loop

        !-----------------------------------------------
        !  Account for surface reflection and emission
        !-----------------------------------------------

        DO i = 1, RTV%n_Angles
          RTV%s_Level_IterRad_UP( i, n_Layers, niter ) = reflectivity( i, i ) * RTV%s_Level_IterRad_DOWN( i, n_Layers, niter )
        END DO
        IF ( niter == 1 ) THEN  
          ! Add surface emission only for zeroth order of interaction

          RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, n_Layers, niter ) = &
                                                               RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, n_Layers, niter ) + &
                                                                      emissivity( 1 : RTV%n_Angles ) * RTV%Planck_Surface

        END IF 

        !-------------------------------------------------
        !  Integrate back up through atmospheric layers
        !-------------------------------------------------
        layersup_loop: DO k = n_Layers, 1, -1
        
          IF ( niter == 1 ) THEN
            source( 1 :RTV%n_Angles ) = RTV%s_Layer_Source_UP( 1 : RTV%n_Angles, k )
          ELSE  
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN
!              DO j = 1, RTV%n_Angles
!                source( j ) = ZERO
!                DO i = 1, RTV%n_Angles  ! Integrate over angle
!                  source( j ) = source( j ) + RTV%s_Layer_Refl( j, i, k ) * RTV%s_Level_IterRad_DOWN( i, k - 1, niter - 1 ) &
!                                            + RTV%s_Layer_Trans( j, i, k ) * RTV%s_Level_IterRad_UP( i, k, niter - 1 ) 
!                END DO
!              END DO

              source( 1 : RTV%n_Angles ) = matmul( RTV%s_Layer_Refl( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                           RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, niter - 1 ) ) + &
                                           matmul( RTV%s_Layer_Trans( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                             RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, niter - 1 ) )

            ELSE
              source( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
          RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k - 1, niter ) = RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, niter ) &
                                                                      * RTV%e_Layer_Trans( 1 : RTV%n_Angles, k ) &
                                                                      + source( 1 : RTV%n_Angles )
        END DO layersup_loop

        !-------------------------------------------------------------------
        ! Save solution at observation angle for this order of interaction
        !-------------------------------------------------------------------
        rad = RTV%s_Level_IterRad_UP( Index_Sat_Angle, 0, niter )

        !---------------------------------
        ! Add it to cumulative solution 
        !---------------------------------
        RTV%s_level_Rad_UP( Index_Sat_Angle, 0 ) = RTV%s_level_Rad_UP( Index_Sat_Angle, 0 ) + rad 

        !---------------------------------------
        ! Accelerate covergence of iterations?
        !---------------------------------------
!        IF ( ACCEL ) THEN
!          radsum = radsum + rad
!          IF ( niter > 1 ) THEN
!            diff = rad_old - rad            
!            IF ( diff == 0. ) RETURN
!            q = radsum + rad ** 2 / diff
!            rad_change = ABS( q - q_old )
!            q_old = q
!          END IF
!          rad_old = rad
!        ELSE
    ! Check how much cumulative solution has changed
          rad_change = rad / RTV%s_level_Rad_UP( Index_Sat_Angle, 0 )
!        END IF

      END DO soi_loop

      RTV%Number_SOI_Iter = niter

      RETURN
      END SUBROUTINE CRTM_SOI


   SUBROUTINE CRTM_SOI_TL(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                               T_OD, & ! Input  layer optical depth
                         emissivity, & ! Input  surface emissivity
                       reflectivity, & ! Input  surface reflectivity matrix 
                    Index_Sat_Angle, & ! Input  satellite angle index 
                                RTV, & ! Input  structure containing forward part results 
               Planck_Atmosphere_TL, & ! Input  tangent-linear atmospheric layer Planck radiance 
                  Planck_Surface_TL, & ! Input  TL surface Planck radiance
                               w_TL, & ! Input  TL layer scattering albedo
                            T_OD_TL, & ! Input  TL layer optical depth
                      emissivity_TL, & ! Input  TL surface emissivity
                    reflectivity_TL, & ! Input  TL  reflectivity
                             Pff_TL, & ! Input  TL forward phase matrix
                             Pbb_TL, & ! Input  TL backward phase matrix
                          s_rad_up_TL) ! Output TL upward radiance 
! ------------------------------------------------------------------------- !
!                                                                           !
! FUNCTION:                                                                 !
!                                                                           !
!   This subroutine calculates the tangent linear of the IR/MW radiance at  !
!   the top of the atmosphere including multiple scattering using the SOI   !
!   (Successive Order of Interaction) algorithm, which combines the         !
!   Successive Order of Scattering and doubling methods.                    !
!                                                                           ! 
!   Written by Tom Greenwald    tomg@ssec.wisc.edu                          !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w, T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL (fp), INTENT(IN), DIMENSION( :, : ) :: reflectivity 
      INTEGER, INTENT( IN ) :: Index_Sat_Angle
      TYPE(RTV_type), INTENT( IN ) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( 0: ) ::  Planck_Atmosphere_TL
      REAL (fp), INTENT(IN) ::  Planck_Surface_TL
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w_TL, T_OD_TL
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity_TL
      REAL (fp), INTENT(IN), DIMENSION( :, : ) :: reflectivity_TL 
      REAL (fp), INTENT(IN), DIMENSION( :, :, : ) ::  Pff_TL, Pbb_TL
      REAL (fp), INTENT(INOUT), DIMENSION( : ) :: s_rad_up_TL 

   ! -------------- internal variables --------------------------------- !

      INTEGER :: i, k, iter
      REAL(fp), PARAMETER :: SNGL_SCAT_ALB_THRESH = 0.8
      REAL(fp), PARAMETER :: OPT_DEPTH_THRESH = 4.0
      REAL(fp), DIMENSION( RTV%n_Angles ) :: source_TL
      REAL(fp), DIMENSION( RTV%n_Angles, n_Layers ) :: e_Trans_TL
      REAL(fp), DIMENSION( RTV%n_Angles, n_Layers ) :: s_source_up_TL, s_source_down_TL
      REAL(fp), DIMENSION( RTV%n_Angles, RTV%n_Angles, n_Layers ) :: s_trans_TL, s_refl_TL
      REAL(fp), DIMENSION( RTV%n_Angles, 0:n_Layers, RTV%Number_SOI_Iter ) :: s_IterRad_UP_TL, s_IterRad_DOWN_TL


      DO k = 1, n_Layers

        e_Trans_TL( 1 : RTV%n_Angles, k ) = -T_OD_TL(k) * (EXP( -T_OD( k ) / RTV%COS_Angle( 1 : RTV%n_Angles ) ) ) / &
                                             RTV%COS_Angle( 1 : RTV%n_Angles )

        IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD ) THEN 
          IF ( w( k ) < SNGL_SCAT_ALB_THRESH .AND. T_OD( k ) < OPT_DEPTH_THRESH ) THEN 
            CALL CRTM_Truncated_Doubling_TL(RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ),               & !Input
                                            RTV%COS_Angle( 1 : RTV%n_Angles ), RTV%COS_Weight( 1 : RTV%n_Angles ),   & !Input
                                            RTV%Pff( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ),                        & !Input
                                            RTV%Pbb( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), RTV%Planck_Atmosphere( k ), & !Input
                                            w_TL( k ), T_OD_TL( k ), Pff_TL( :, :, k ),                   & !Input
                                            Pbb_TL( :, :, k ), Planck_Atmosphere_TL( k ), RTV,                       & !Input
                                            s_trans_TL( :, :, k ), s_refl_TL( :, :, k ), s_source_up_TL( :, k ),     & !Output
                                            s_source_down_TL( :, k ) )                                                 !Output
          ELSE
            CALL CRTM_Doubling_layer_TL(RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ),               & !Input
                                        RTV%COS_Angle( 1 : RTV%n_Angles ), RTV%COS_Weight( 1 : RTV%n_Angles ),   & !Input
                                        RTV%Pff( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ),                        & !Input
                                        RTV%Pbb( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), RTV%Planck_Atmosphere( k ), & !Input
                                        w_TL( k ), T_OD_TL( k ), Pff_TL( :, :, k ),                   & !Input
                                        Pbb_TL( :, :, k ), Planck_Atmosphere_TL( k ), RTV,                       & !Input
                                        s_trans_TL( :, :, k), s_refl_TL( :, :, k ), s_source_up_TL( :, k ),      & !Output
                                        s_source_down_TL( :, k ) )                                                 !Output
          END IF

! Subtract out direct transmission
          DO i = 1, RTV%n_angles
            s_Trans_TL( i, i, k ) = s_Trans_TL( i, i, k ) - e_Trans_TL( i, k )
          END DO

        ELSE       !   Thermal sources
          DO i = 1, RTV%n_Angles
            s_Source_UP_TL( i, k ) = Planck_Atmosphere_TL( k ) * ( ONE - RTV%e_Layer_Trans( i, k ) ) - &
                                     RTV%PLanck_Atmosphere( k ) * e_Trans_TL( i, k )
            s_Source_DOWN_TL( i, k ) = s_Source_UP_TL( i, k )
          END DO 
        END IF

      END DO

      s_Rad_UP_TL( Index_Sat_Angle ) = ZERO
      s_IterRad_UP_TL( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO
      s_IterRad_DOWN_TL( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO

    !-----------------------------------------
    ! This is the Order of Interaction loop
    !-----------------------------------------
      DO iter = 1, RTV%Number_SOI_Iter


        IF ( iter > 1 ) s_IterRad_DOWN_TL( 1 : RTV%n_Angles, 0, iter ) = ZERO

        !---------------------------------------------
        ! Integrate down through atmospheric layers
        !--------------------------------------------- 
        layersdn_loop: DO k = 1, n_Layers
        
          IF ( iter == 1 ) THEN

            source_TL( 1 : RTV%n_Angles ) = s_Source_DOWN_TL( 1 : RTV%n_Angles, k )

          ELSE
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN
              source_TL( 1 : RTV%n_Angles ) = matmul( RTV%s_Layer_Trans( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      s_IterRad_DOWN_TL( 1 : RTV%n_Angles, k - 1, iter - 1 ) ) + &
                                              matmul( s_Trans_TL( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, iter - 1 ) ) + &
                                              matmul( RTV%s_Layer_Refl( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      s_IterRad_UP_TL( 1 : RTV%n_Angles, k, iter - 1 ) ) + &
                                              matmul( s_Refl_TL( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      RTV%s_level_IterRad_UP( 1 : RTV%n_Angles, k, iter - 1 ) )
            ELSE
              source_TL( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
          s_IterRad_DOWN_TL( 1 : RTV%n_Angles, k, iter ) = s_IterRad_DOWN_TL( 1 : RTV%n_Angles, k - 1, iter ) &
                                                                     * RTV%e_Layer_Trans( 1 : RTV%n_Angles, k ) + &
                                                           RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, iter ) &
                                                                     * e_Trans_TL( 1 : RTV%n_Angles, k ) &
                                                                     + source_TL( 1 : RTV%n_Angles )
        END DO layersdn_loop

        !-----------------------------------------------
        !  Account for surface reflection and emission
        !-----------------------------------------------

        DO i = 1, RTV%n_Angles
          s_IterRad_UP_TL( i, n_Layers, iter ) = reflectivity_TL( i, i ) * RTV%s_Level_IterRad_DOWN( i, n_Layers, iter ) + &
                                                 reflectivity( i, i ) * s_IterRad_DOWN_TL( i, n_Layers, iter )
        END DO
        IF ( iter == 1 ) THEN  
          ! Add surface emission only for zeroth order of interaction

          s_IterRad_UP_TL( 1 : RTV%n_Angles, n_Layers, iter ) = s_IterRad_UP_TL( 1 : RTV%n_Angles, n_Layers, iter ) + &
                                                                      emissivity( 1 : RTV%n_Angles ) * Planck_Surface_TL + &
                                                                      emissivity_TL( 1 : RTV%n_Angles ) * RTV%Planck_Surface
        END IF 

        !-------------------------------------------------
        !  Integrate back up through atmospheric layers
        !-------------------------------------------------
        layersup_loop: DO k = n_Layers, 1, -1
        
          IF ( iter == 1 ) THEN
            source_TL( 1 :RTV%n_Angles ) = s_Source_UP_TL( 1 : RTV%n_Angles, k )
          ELSE  
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN
              source_TL( 1 : RTV%n_Angles ) = matmul( RTV%s_Layer_Refl( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      s_IterRad_DOWN_TL( 1 : RTV%n_Angles, k - 1, iter - 1 ) ) + &
                                              matmul( s_Refl_TL( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, iter - 1 ) ) + &
                                              matmul( RTV%s_Layer_Trans( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      s_IterRad_UP_TL( 1 : RTV%n_Angles, k, iter - 1 ) ) + &
                                              matmul( s_Trans_TL( 1 : RTV%n_Angles, 1 : RTV%n_Angles, k ), &
                                                      RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, iter - 1 ) )
            ELSE
              source_TL( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
          s_IterRad_UP_TL( 1 : RTV%n_Angles, k - 1, iter ) = RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, iter ) &
                                                                      * e_Trans_TL( 1 : RTV%n_Angles, k ) + &
                                                             s_IterRad_UP_TL( 1 : RTV%n_Angles, k, iter ) &
                                                                      * RTV%e_Layer_Trans( 1 : RTV%n_Angles, k ) &
                                                                      + source_TL( 1 : RTV%n_Angles )
        END DO layersup_loop

        !----------------
        ! Add up terms
        !----------------
        s_Rad_UP_TL( Index_Sat_Angle ) = s_Rad_UP_TL( Index_Sat_Angle ) + s_IterRad_UP_TL( Index_Sat_Angle, 0, iter )

      END DO  

      RETURN
      END SUBROUTINE CRTM_SOI_TL

   SUBROUTINE CRTM_SOI_AD(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                              T_OD, & ! Input  layer optical depth
                        emissivity, & ! Input  surface emissivity
                      reflectivity, & ! Input  surface reflectivity matrix  
                   Index_Sat_Angle, & ! Input  satellite angle index
                               RTV, & ! Input  structure containing forward results 
                       s_rad_up_AD, & ! Input  adjoint upward radiance 
              Planck_Atmosphere_AD, & ! Output AD atmospheric layer Planck radiance
                 Planck_Surface_AD, & ! Output AD surface Planck radiance
                              w_AD, & ! Output AD layer scattering albedo
                           T_OD_AD, & ! Output AD layer optical depth
                     emissivity_AD, & ! Output AD surface emissivity
                   reflectivity_AD, & ! Output AD surface reflectivity
                            Pff_AD, & ! Output AD forward phase matrix
                            Pbb_AD)   ! Output AD backward phase matrix
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW adjoint radiance at the top of         !
!   the atmosphere including atmospheric scattering.                        !
!                                                                           ! 
!    Tom Greenwald tomg@ssec.wisc.edu                                       !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  w, T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL (fp), INTENT(IN), DIMENSION( :, : ) ::  reflectivity
      INTEGER, INTENT(IN) :: Index_Sat_Angle
      TYPE(RTV_type), INTENT(IN) :: RTV

      REAL (fp),INTENT(INOUT),DIMENSION( :, :, : ) ::  Pff_AD, Pbb_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  w_AD,T_OD_AD
      REAL (fp),INTENT(INOUT),DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL (fp),INTENT(INOUT) ::  Planck_Surface_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  emissivity_AD
      REAL (fp),INTENT(INOUT),DIMENSION( :, : ) :: reflectivity_AD 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_AD 

! Local variables 
      REAL(fp), PARAMETER :: SNGL_SCAT_ALB_THRESH = 0.8
      REAL(fp), PARAMETER :: OPT_DEPTH_THRESH = 4.0
      INTEGER :: iter, k, i, j
      REAL(fp), DIMENSION( RTV%n_Angles, 0:n_Layers, RTV%Number_SOI_Iter ) :: s_IterRad_UP_AD      
      REAL(fp), DIMENSION( RTV%n_Angles, 0:n_Layers, RTV%Number_SOI_Iter ) :: s_IterRad_DOWN_AD      
      REAL(fp), DIMENSION( RTV%n_Angles ) :: source_AD
      REAL(fp), DIMENSION( RTV%n_Angles, n_Layers ) :: s_source_UP_AD
      REAL(fp), DIMENSION( RTV%n_Angles, n_layers ) :: s_source_DOWN_AD
      REAL(fp), DIMENSION( RTV%n_Angles, n_Layers ) :: e_Trans_AD
      REAL(fp), DIMENSION( RTV%n_Angles, RTV%n_Angles, n_Layers ) :: s_Refl_AD
      REAL(fp), DIMENSION( RTV%n_Angles, RTV%n_Angles, n_Layers ) :: s_Trans_AD
      
! Zero out all local variables
      s_IterRad_UP_AD = ZERO
      s_IterRad_DOWN_AD = ZERO
      source_AD = ZERO
      s_source_UP_AD = ZERO
      s_source_DOWN_AD = ZERO
      e_Trans_AD = ZERO
      s_Refl_AD = ZERO
      s_Trans_AD = ZERO
      Pff_AD = ZERO
      Pbb_AD = ZERO


!--------------------------------------------------------------------
! Loop through each successive order of interaction in reverse order
!--------------------------------------------------------------------
      DO iter = RTV%Number_SOI_Iter, 1, -1
        s_IterRad_UP_AD( Index_Sat_Angle, 0, iter ) = s_Rad_UP_AD( Index_Sat_Angle )
!---------------------------------------
! Step down through upward integration
!---------------------------------------
        DO k = 1, n_Layers
          source_AD( 1 : RTV%n_Angles ) = source_AD( 1 : RTV%n_Angles ) + s_IterRad_UP_AD( 1 : RTV%n_Angles, k - 1, iter )
          e_Trans_AD( 1 : RTV%n_Angles, k ) = e_Trans_AD( 1 : RTV%n_Angles, k ) + &
                                                                s_IterRad_UP_AD( 1 : RTV%n_Angles, k - 1, iter ) * &
                                                                    RTV%s_Level_IterRad_UP( 1 : RTV%n_Angles, k, iter )  
          s_IterRad_UP_AD( 1 : RTV%n_Angles, k, iter ) = s_IterRad_UP_AD( 1 : RTV%n_Angles, k, iter ) + &
                                                           s_IterRad_UP_AD( 1 : RTV%n_Angles, k - 1, iter ) * &
                                                               RTV%e_Layer_Trans( 1 : RTV%n_Angles, k )
          s_IterRad_UP_AD( 1 : RTV%n_Angles, k - 1, iter ) = ZERO 
          IF ( iter == 1 ) THEN
            s_source_UP_AD( 1 : RTV%n_Angles, k ) = s_source_UP_AD( 1 : RTV%n_Angles, k ) + source_AD( 1 : RTV%n_Angles )
            source_AD( 1 : RTV%n_Angles ) = ZERO
          ELSE            
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN              

              DO j = 1, RTV%n_Angles
                DO i = 1, RTV%n_Angles  ! Integrate over angle
                  s_Refl_AD( j, i, k ) = s_Refl_AD( j, i, k ) + source_AD( j ) * RTV%s_Level_IterRad_DOWN( i, k - 1, iter - 1 )
                  s_IterRad_DOWN_AD( i, k - 1, iter - 1 ) = s_IterRad_DOWN_AD( i, k - 1, iter - 1 ) + source_AD( j ) * &
                                                                    RTV%s_Layer_Refl( j, i, k )
                  s_Trans_AD( j, i, k ) = s_Trans_AD( j, i, k ) + source_AD( j ) * RTV%s_Level_IterRad_UP( i, k, iter - 1 )
                  s_IterRad_UP_AD( i, k, iter - 1 ) = s_IterRad_UP_AD( i, k, iter - 1 ) + source_AD( j ) * &
                                                                    RTV%s_Layer_Trans( j, i, k )
                END DO
                source_AD( j ) = ZERO
              END DO
            ELSE
              source_AD( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
        END DO

        IF ( iter == 1 ) THEN             
        ! Add surface emission only for zeroth order of interaction     
        
          emissivity_AD( 1 : RTV%n_Angles ) = emissivity_AD( 1 : RTV%n_Angles ) + &
                                                   s_IterRad_UP_AD( 1 : RTV%n_Angles, n_Layers, iter ) * RTV%Planck_Surface
          Planck_Surface_AD = Planck_Surface_AD + SUM( s_IterRad_UP_AD( :, n_Layers, iter ) * emissivity )
        END IF
      !-----------------------------------------------
      !  Account for surface reflection and emission                                                   
      !-----------------------------------------------
        DO i = 1, RTV%n_Angles
          reflectivity_AD( i, i ) = reflectivity_AD( i, i ) + s_IterRad_UP_AD( i, n_Layers, iter ) * &
                                                                     RTV%s_Level_IterRad_DOWN( i, n_Layers, iter )   
          s_IterRad_DOWN_AD( i, n_Layers, iter ) = s_IterRad_DOWN_AD( i, n_Layers, iter ) + &
                                                          s_IterRad_UP_AD( i, n_Layers, iter ) * reflectivity( i, i )
          s_IterRad_UP_AD( i, n_Layers, iter ) = ZERO
        END DO
 
!---------------------------------------
! Step up through downward integration
!---------------------------------------
        DO k = n_Layers, 1, -1
          source_AD( 1 : RTV%n_Angles ) = source_AD( 1 : RTV%n_Angles ) + s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k, iter )
          e_Trans_AD( 1 : RTV%n_Angles, k ) = e_Trans_AD( 1 : RTV%n_Angles, k ) + &
                                                                  s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k, iter ) * &
                                                                      RTV%s_Level_IterRad_DOWN( 1 : RTV%n_Angles, k - 1, iter )  
          s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k - 1, iter ) = s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k - 1, iter ) + &
                                                                    s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k, iter ) * &
                                                                          RTV%e_Layer_Trans( 1 : RTV%n_Angles, k )
          s_IterRad_DOWN_AD( 1 : RTV%n_Angles, k, iter ) = ZERO 
          IF ( iter == 1 ) THEN
            s_source_DOWN_AD( 1 : RTV%n_Angles, k ) = s_source_DOWN_AD( 1 : RTV%n_Angles, k ) + source_AD( 1 : RTV%n_Angles )
            source_AD( 1 : RTV%n_Angles ) = ZERO
          ELSE            
            IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD .AND. T_OD( k ) >= OPTICAL_DEPTH_THRESHOLD ) THEN              
              DO j = 1, RTV%n_Angles
                DO i = 1, RTV%n_Angles  ! Integrate over angle
                  s_Trans_AD( j, i, k ) = s_Trans_AD( j, i, k ) + source_AD( j ) * RTV%s_Level_IterRad_DOWN( i, k - 1, iter - 1 )
                  s_IterRad_DOWN_AD( i, k - 1, iter - 1 ) = s_IterRad_DOWN_AD( i, k - 1, iter - 1 ) + source_AD( j ) * &
                                                                    RTV%s_Layer_Trans( j, i, k )
                  s_Refl_AD( j, i, k ) = s_Refl_AD( j, i, k ) + source_AD( j ) * RTV%s_Level_IterRad_UP( i, k, iter - 1 )
                  s_IterRad_UP_AD( i, k, iter - 1 ) = s_IterRad_UP_AD( i, k, iter - 1 ) + source_AD( j ) * &
                                                                    RTV%s_Layer_Refl( j, i, k )
                END DO
                source_AD( j ) = ZERO
              END DO
            ELSE
              source_AD( 1 : RTV%n_Angles ) = ZERO
            END IF
          END IF
        END DO

        IF ( iter > 1 ) s_IterRad_DOWN_AD( 1 : RTV%n_Angles, 0, iter ) = ZERO  ! No cosmic background contribution for
                                                                                ! higher orders of interaction
      END DO  ! SOI iteration
                     
      s_IterRad_DOWN_AD( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO
      s_IterRad_UP_AD( 1 : RTV%n_Angles, 0 : n_Layers, 1 : RTV%Number_SOI_Iter ) = ZERO
      s_Rad_UP_AD( Index_Sat_Angle ) = ZERO       
                  
      DO k = 1, n_Layers

        IF ( w( k ) > SCATTERING_ALBEDO_THRESHOLD ) THEN 

          DO i = 1, RTV%n_angles
            e_Trans_AD( i, k ) = e_Trans_AD( i, k ) - s_Trans_AD( i, i, k )
          END DO

          IF ( w( k ) < SNGL_SCAT_ALB_THRESH .AND. T_OD( k ) < OPT_DEPTH_THRESH ) THEN 

            CALL CRTM_Truncated_Doubling_AD(RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ),      &        !Input
                                            RTV%COS_Angle, RTV%COS_Weight, RTV%Pff( :, :, k ), RTV%Pbb( :, :, k ), & ! Input
                                            RTV%Planck_Atmosphere( k ),    & !Input
                                            s_trans_AD( :, :, k ), s_refl_AD( :, :, k ), s_source_up_AD( :, k ),   & 
                                            s_source_down_AD( :, k ), RTV, w_AD( k ), T_OD_AD( k ), Pff_AD( :, :, k ), & 
                                            Pbb_AD( :, :, k ), Planck_Atmosphere_AD( k ) )  !Output
          ELSE

            CALL CRTM_Doubling_layer_AD(RTV%n_Streams, RTV%n_Angles, k, w( k ), T_OD( k ),      &        !Input
                                        RTV%COS_Angle, RTV%COS_Weight, RTV%Pff( :, :, k ), RTV%Pbb( :, :, k ), & ! Input
                                        RTV%Planck_Atmosphere( k ),    & !Input
                                        s_trans_AD( :, :, k ), s_refl_AD( :, :, k ), s_source_up_AD( :, k ),   & 
                                        s_source_down_AD( :, k ), RTV, w_AD( k ), T_OD_AD( k ), Pff_AD( :, :, k ), & 
                                        Pbb_AD( :, :, k ), Planck_Atmosphere_AD( k ) )  !Output

          END IF

        ELSE       !   Thermal sources

          DO i = 1, RTV%n_Angles
            s_Source_UP_AD( i, k ) = s_Source_UP_AD( i, k ) + s_Source_DOWN_AD( i, k )
            s_Source_DOWN_AD( i, k ) = ZERO

            Planck_Atmosphere_AD( k ) = Planck_Atmosphere_AD( k ) + s_Source_UP_AD( i, k ) * ( ONE - RTV%e_Layer_Trans( i, k ) )
            e_Trans_AD( i, k ) = e_Trans_AD( i, k ) - RTV%Planck_Atmosphere( k ) * s_Source_UP_AD( i, k )
            s_Source_UP_AD( i, k ) = ZERO
          END DO 
        END IF

        T_OD_AD( k ) = T_OD_AD( k ) - SUM( e_Trans_AD( :, k ) * RTV%e_Layer_Trans( 1:RTV%n_Angles, k ) / &
                             RTV%COS_Angle(1:RTV%n_Angles) )
        e_Trans_AD( 1 : RTV%n_Angles, k ) = ZERO

      END DO

   END SUBROUTINE CRTM_SOI_AD

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SOI_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_SOI_Version( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_SOI_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_SOI_Version


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

      SUBROUTINE CRTM_Truncated_Doubling( n_streams, & ! Input, number of streams
                                               NANG, & ! Input, number of angles
                                                 KL, & ! Input, KL-th layer 
                                      single_albedo, & ! Input, single scattering albedo
                                      optical_depth, & ! Input, layer optical depth
                                          COS_Angle, & ! Input, COSINE of ANGLES
                                         COS_Weight, & ! Input, GAUSSIAN Weights
                                                 ff, & ! Input, Phase matrix (forward part)
                                                 bb, & ! Input, Phase matrix (backward part)
                                        Planck_Func, & ! Input, Planck for layer temperature
                                                RTV)   ! Output, layer transmittance, reflectance, and source 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Method and References
!   It is a common doubling method and its theoretical basis is referred to
!   Hansen, J. E., 1971: Multiple scattering of polarized light in 
!   planetary atmosphere. Part I. The doubling method, J. ATmos. Sci., 28, 120-125.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
!
!   Modified from CRTM_Doubling_Layer by Tom Greenwald tomg@ssec.wisc.edu
! ----------------------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_streams, NANG, KL
      REAL(fp), INTENT(IN) :: single_albedo, optical_depth, Planck_Func
      REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
      REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff, bb
      TYPE(RTV_type), INTENT( INOUT ) :: RTV

     ! internal variables
      REAL(fp), DIMENSION(NANG,NANG) :: term2,term3,trans,refl
      REAL(fp), DIMENSION(NANG) :: C1, C2, source_up,source_down 
      REAL(fp) :: s, c
      INTEGER :: i,j,k,L


      !  Check for tiny optical depth

      IF ( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = ZERO
        DO i = 1, NANG
          RTV%s_Layer_Trans(i,i,KL) = ONE
        ENDDO
        RTV%s_Layer_Refl( 1 : NANG, 1 : NANG, KL ) = ZERO 
        RTV%s_Layer_Source_DOWN( 1 : NANG, KL ) = ZERO 
        RTV%s_Layer_Source_UP( 1 : NANG, KL ) = ZERO 
        RETURN
      ENDIF

        ! -------------------------------------------------------------- !
        !  Determine number of doubling steps and construct              !
        !  initial transmission and reflection matrix                    !
        !  --------------------------------------------------------------!
      RTV%Number_Doubling( KL ) = INT( log( optical_depth / DELTA_OPTICAL_DEPTH ) / log( TWO ) ) + 1
      IF ( RTV%Number_Doubling( KL ) < 1 ) RTV%Number_Doubling( KL ) = 1
      IF ( RTV%Number_Doubling( KL ) <= MAX_N_DOUBLING ) THEN
        RTV%Delta_Tau( KL ) = optical_depth / ( TWO**RTV%Number_Doubling( KL ) )
      ELSE
        RTV%Number_Doubling( KL ) = MAX_N_DOUBLING
        RTV%Delta_Tau( KL ) = DELTA_OPTICAL_DEPTH
      ENDIF
      s = RTV%Delta_Tau( KL ) * single_albedo
      DO i = 1, NANG
        c = s / COS_Angle( i )
        DO j = 1, NANG
          RTV%Refl( i, j, 0, KL ) = c * bb( i, j ) * COS_Weight( j )
          RTV%Trans( i, j, 0, KL ) = c * ff( i, j ) * COS_Weight( j )
        ENDDO
        RTV%Trans( i, i, 0, KL ) = RTV%Trans( i, i, 0, KL ) + ONE - RTV%Delta_Tau( KL ) / COS_Angle( i )
      ENDDO

        ! -------------------------------------------------------------- !
        !  Doubling divided sub-layers                                   !
        !  --------------------------------------------------------------!
      DO L = 1, RTV%Number_Doubling( KL )
        DO i = 1, NANG
          DO j = 1, NANG
            RTV%Inv_BeT( i, j, L, KL ) = ZERO
            DO k = 1, NANG
! Add option to select two terms (i.e., RR+RR**2) if opd > 4 and ssa > 0.8
              RTV%Inv_BeT( i, j, L, KL ) = RTV%Inv_BeT( i, j, L, KL ) + RTV%Refl( i, k, L - 1, KL ) * RTV%Refl( k, j, L - 1, KL )
            ENDDO
          ENDDO
          RTV%Inv_BeT( i, i, L, KL ) = RTV%Inv_BeT( i, i, L, KL ) + ONE
        ENDDO

        term2 = matmul( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ), RTV%Inv_BeT( 1 : NANG, 1 : NANG, L, KL ) )
        term3 = matmul( term2, RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) )
        term3 = matmul( term3, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) )
 
        RTV%Refl( 1 : NANG, 1 : NANG, L, KL ) = RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) + term3
        RTV%Trans( 1 : NANG, 1 : NANG, L, KL ) = matmul( term2, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) ) 

      ENDDO

      trans = RTV%Trans( 1 : NANG, 1 : NANG, RTV%Number_Doubling( KL ), KL )
      refl  = RTV%Refl( 1 : NANG, 1 : NANG, RTV%Number_Doubling( KL ), KL )
!
!   computing source function at up and downward directions.
      DO i = 1, NANG
        C1( i ) = ZERO
        C2( i ) = ZERO
        DO j = 1, n_Streams 
          C1( i ) = C1( i ) + trans( i, j ) 
          C2( i ) = C2( i ) + refl( i, j ) 
        ENDDO
        IF ( i == NANG .AND. NANG == ( n_Streams + 1 ) ) THEN
          C1( i ) = C1( i ) + trans( NANG, NANG )
        ENDIF
      ENDDO

      DO i = 1, NANG
        source_up( i ) = ( ONE - C1( i ) - C2( i ) ) * Planck_Func
        source_down( i ) = source_up( i )
      ENDDO

      RTV%C1( 1 : NANG, KL ) = C1
      RTV%C2( 1 : NANG, KL ) = C2
      RTV%s_Layer_Trans( 1 : NANG, 1 : NANG, KL ) = trans
      RTV%s_Layer_Refl( 1 : NANG, 1 : NANG, KL ) = refl
      RTV%s_Layer_Source_DOWN( 1 : NANG, KL ) = source_down
      RTV%s_Layer_Source_UP( 1 : NANG, KL ) = source_up 
       
      RETURN

      END SUBROUTINE CRTM_Truncated_Doubling


      SUBROUTINE CRTM_Truncated_Doubling_TL(n_streams, & ! Input, number of streams
                                                 NANG, & ! Input, number of angles
                                                   KL, & ! Input, number of angles
                                        single_albedo, & ! Input, single scattering albedo
                                        optical_depth, & ! Input, layer optical depth
                                            COS_Angle, & ! Input, COSINE of ANGLES
                                           COS_Weight, & ! Input, GAUSSIAN Weights
                                                   ff, & ! Input, Phase matrix (forward part)
                                                   bb, & ! Input, Phase matrix (backward part)
                                          Planck_Func, & ! Input, Planck for layer temperature
                                     single_albedo_TL, & ! Input, tangent-linear single albedo
                                     optical_depth_TL, & ! Input, TL layer optical depth
                                                ff_TL, & ! Input, TL forward Phase matrix
                                                bb_TL, & ! Input, TL backward Phase matrix
                                       Planck_Func_TL, & ! Input, TL Planck for layer temperature
                                                  RTV, & ! Input, structure containing forward results 
                                             trans_TL, & ! Output, layer tangent-linear trans 
                                              refl_TL, & ! Output, layer tangent-linear refl 
                                         source_up_TL, & ! Output, layer tangent-linear source_up 
                                       source_down_TL)   ! Output, layer tangent-linear source_down 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute tangent-linear layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Tom Greenwald tomg@ssec.wisc.edu
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams, NANG, KL
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff, bb
      REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
      REAL(fp), INTENT(IN) :: single_albedo, optical_depth, Planck_Func

      ! Tangent-Linear Part
      REAL(fp), INTENT(OUT), DIMENSION( :, : ) :: trans_TL, refl_TL
      REAL(fp), INTENT(OUT), DIMENSION( : ) :: source_up_TL, source_down_TL
      REAL(fp), INTENT(IN) :: single_albedo_TL
      REAL(fp), INTENT(IN) :: optical_depth_TL, Planck_Func_TL
      REAL(fp), INTENT(IN), DIMENSION( : ,: ) :: ff_TL, bb_TL

      ! internal variables
      REAL(fp), DIMENSION( NANG, NANG ) :: term2, term3, term2_TL, term3_TL, ms_term_TL
      REAL(fp) :: s, c
      REAL(fp) :: s_TL, c_TL, Delta_Tau_TL
      REAL(fp), DIMENSION( NANG ) :: C1_TL, C2_TL
      INTEGER :: i, j, k, L

      ! Tangent-Linear Beginning

      IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        trans_TL = ZERO
        refl_TL = ZERO
        source_up_TL = ZERO
        source_down_TL = ZERO
        RETURN
      ENDIF
      
      s = RTV%Delta_Tau( KL ) * single_albedo
      IF ( RTV%Number_Doubling( KL ) <= MAX_N_DOUBLING ) THEN
        Delta_Tau_TL = optical_depth_TL / ( TWO**RTV%Number_Doubling( KL ) )
      ELSE  
        Delta_Tau_TL = ZERO 
      ENDIF
      s_TL = Delta_Tau_TL * single_albedo + RTV%Delta_Tau( KL ) * single_albedo_TL
      DO i = 1, NANG
        c = s/COS_Angle( i )
        c_TL = s_TL/COS_Angle( i )
        DO j = 1, NANG
          refl_TL( i, j ) = c_TL * bb( i, j ) * COS_Weight( j ) + c * bb_TL( i, j ) * COS_Weight( j )
          trans_TL( i, j ) = c_TL * ff( i, j ) * COS_Weight( j ) + c * ff_TL( i, j ) * COS_Weight( j )
        ENDDO
        trans_TL( i, i ) = trans_TL( i, i ) - Delta_Tau_TL / COS_Angle( i )
      ENDDO

      DO L = 1, RTV%Number_Doubling( KL ) 
        DO i = 1, NANG
          DO j = 1, NANG
            ms_term_TL( i, j ) = ZERO
            DO k = 1, NANG
              ms_term_TL( i, j ) = ms_term_TL( i, j ) + RTV%Refl( i, k, L - 1, KL ) * Refl_TL( k, j ) &
                                                      + Refl_TL( i, k ) * RTV%Refl( k, j, L - 1, KL )
            END DO
          END DO
        END DO

        term2 = matmul( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ), RTV%Inv_BeT( 1 : NANG, 1 : NANG, L, KL ) )
        term2_TL = matmul( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ), ms_term_TL ) + &
                   matmul( Trans_TL, RTV%Inv_BeT( 1 : NANG, 1 : NANG, L, KL ) )
        term3 = matmul( term2, RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) )
        term3_TL = matmul( term2, Refl_TL ) + matmul( term2_TL, RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) )
        term3 = matmul( term3, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) )
        term3_TL = matmul( term3, Trans_TL ) + matmul( term3_TL, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) )
        refl_TL = Refl_TL + term3_TL
        trans_TL = matmul( term2, Trans_TL ) + matmul( term2_TL, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) )

      ENDDO

!   compute TL of source function at up and downward directions.

      DO i = 1, NANG
        C1_TL( i ) = ZERO
        C2_TL( i ) = ZERO
        DO j = 1, n_Streams 
          C1_TL( i ) = C1_TL( i ) + trans_TL( i, j ) 
          C2_TL( i ) = C2_TL( i ) + refl_TL( i, j ) 
        ENDDO
        IF ( i == NANG .AND. NANG == ( n_Streams + 1 ) ) THEN
          C1_TL( i ) = C1_TL( i ) + trans_TL( NANG, NANG )
        ENDIF
      ENDDO

      DO i = 1, NANG
        source_up_TL( i ) = - ( C1_TL( i ) + C2_TL( i ) ) * Planck_Func  &
                            + ( ONE - RTV%C1( i, KL ) - RTV%C2( i, KL ) ) * Planck_Func_TL
        source_down_TL( i ) = source_up_TL( i )
      END DO

      RETURN

      END SUBROUTINE CRTM_Truncated_Doubling_TL

   SUBROUTINE CRTM_Truncated_Doubling_AD(n_streams, & ! Input, number of streams
                                              NANG, & ! Input, number of angles
                                                KL, & ! Input, number of angles
                                     single_albedo, & ! Input, single scattering albedo
                                     optical_depth, & ! Input, layer optical depth
                                         COS_Angle, & ! Input, COSINE of ANGLES
                                        COS_Weight, & ! Input, GAUSSIAN Weights
                                                ff, & ! Input, Phase matrix (forward part)
                                                bb, & ! Input, Phase matrix (backward part)
                                       Planck_Func, & ! Input, Planck for layer temperature
                                          trans_AD, & ! Input, layer tangent-linear trans 
                                           refl_AD, & ! Input, layer tangent-linear refl 
                                      source_up_AD, & ! Input, layer tangent-linear source_up 
                                    source_down_AD, & ! Input, layer tangent-linear source_down 
                                               RTV, & ! Input, structure containing forward results 
                                  single_albedo_AD, & ! Output adjoint single scattering albedo
                                  optical_depth_AD, & ! Output AD layer optical depth
                                             ff_AD, & ! Output AD forward Phase matrix
                                             bb_AD, & ! Output AD backward Phase matrix
                                    Planck_Func_AD)   ! Output AD Planck for layer temperature


    INTEGER, INTENT(IN) :: n_streams,NANG,KL
    TYPE(RTV_type), INTENT(IN) :: RTV
    REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
    REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
    REAL(fp), INTENT(IN) :: single_albedo,optical_depth,Planck_Func

    ! Tangent-Linear Part
    REAL(fp), INTENT( INOUT ), DIMENSION( :,: ) :: trans_AD,refl_AD
    REAL(fp), INTENT( INOUT ), DIMENSION( : ) :: source_up_AD,source_down_AD
    REAL(fp), INTENT( INOUT ) :: single_albedo_AD
    REAL(fp), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD
    REAL(fp), INTENT(INOUT), DIMENSION(:,:) :: ff_AD,bb_AD

    ! internal variables
    REAL(fp), DIMENSION( NANG, NANG ) :: term2, term3, term2_AD, term3_AD, ms_term_AD
    REAL(fp) :: s, c
    REAL(fp) :: s_AD, c_AD, Delta_Tau_AD
    REAL(fp), DIMENSION(NANG) :: C1_AD, C2_AD
    INTEGER :: i, j, L

    ! Adjoint Beginning

    IF ( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
      trans_AD = ZERO
      refl_AD = ZERO
      source_up_AD = ZERO
      source_down_AD = ZERO
      RETURN
    ENDIF

! Remember, all output adjoint variables should be set to zero
!   single_albedo_AD = 0, etc.

    DO i = NANG, 1, -1
      source_up_AD( i ) = source_up_AD( i ) + source_down_AD( i )
      source_down_AD( i ) = ZERO
      C2_AD( i ) = -source_up_AD( i ) * Planck_Func
      C1_AD( i ) = -source_up_AD( i ) * Planck_Func
      Planck_Func_AD = Planck_Func_AD + ( ONE - RTV%C1( i, KL ) - RTV%C2( i, KL ) ) * source_up_AD( i )  
    END DO

    ! Compute the source function in the up and downward directions.
    DO i = NANG, 1, -1

      IF(i == NANG .AND. NANG == ( n_Streams + 1 ) ) THEN
        trans_AD( NANG, NANG ) = trans_AD( NANG, NANG ) + C1_AD( i )
      ENDIF

      DO j = n_Streams, 1, -1 
        refl_AD( i, j ) = refl_AD( i, j ) + C2_AD( i )
        trans_AD( i, j ) = trans_AD( i, j ) + C1_AD( i )
      END DO

    END DO

    term2_AD = ZERO
    term3_AD = ZERO
    ms_term_AD = ZERO
    DO L = RTV%Number_Doubling(KL), 1, -1 

! Forward calculations
      term2 = matmul( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ), RTV%Inv_BeT( 1 : NANG, 1 : NANG, L, KL ) )
      term3 = matmul( term2, RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) ) 
      term3 = matmul( term3, RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) )

! Back to adjoint
      term2_AD = term2_AD + matmul( trans_AD, transpose( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) ) )
      trans_AD = matmul( transpose( term2 ), trans_AD )

      term3_AD = term3_AD + refl_AD

      trans_AD = trans_AD + matmul( transpose( term3 ), term3_AD )
      term3_AD = matmul( term3_AD, transpose( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) ) )

      term2_AD = term2_AD + matmul( term3_AD, transpose( RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) ) )
      refl_AD = refl_AD + matmul( transpose( term2 ), term3_AD )
      term3_AD = ZERO

      ms_term_AD = ms_term_AD + matmul( transpose( RTV%Trans( 1 : NANG, 1 : NANG, L - 1, KL ) ), term2_AD ) 
      trans_AD = trans_AD + matmul( term2_AD, transpose( RTV%Inv_BeT( 1 : NANG, 1 : NANG, L, KL ) ) )
      term2_AD = ZERO

      refl_AD = refl_AD + matmul( ms_term_AD, transpose( RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) ) ) + &
                          matmul( transpose( RTV%Refl( 1 : NANG, 1 : NANG, L - 1, KL ) ), ms_term_AD )
      ms_term_AD = ZERO
  
    ENDDO

    s = RTV%Delta_Tau( KL ) * single_albedo
    c_AD = ZERO
    s_AD = ZERO
    Delta_Tau_AD = ZERO

    DO i = NANG, 1, -1

      c = s / COS_Angle( i )
      Delta_Tau_AD = Delta_Tau_AD - trans_AD( i, i ) / COS_Angle( i )

      DO j = NANG, 1, -1
        c_AD = c_AD + trans_AD( i, j ) * ff( i, j ) * COS_Weight( j )
        ff_AD( i, j ) = ff_AD( i, j ) + trans_AD( i, j ) * c * COS_Weight( j )
        c_AD = c_AD + refl_AD( i, j ) * bb( i, j ) * COS_Weight( j )
        bb_AD( i, j ) = bb_AD( i, j ) + refl_AD( i, j ) * c * COS_Weight( j )
      END DO

      s_AD = s_AD + c_AD / COS_Angle( i ) 
      c_AD = ZERO

    ENDDO

    Delta_Tau_AD = Delta_Tau_AD + s_AD * single_albedo
    single_albedo_AD = single_albedo_AD + RTV%Delta_Tau( KL ) * s_AD
    optical_depth_AD = optical_depth_AD + Delta_Tau_AD / ( TWO**RTV%Number_Doubling( KL ) )

   END SUBROUTINE CRTM_Truncated_Doubling_AD


      SUBROUTINE CRTM_Doubling_layer(n_streams, & ! Input, number of streams
                                          NANG, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                                           RTV)   ! Output, layer transmittance, reflectance, and source 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Method and References
!   It is a common doubling method and its theoretical basis is referred to
!   Hansen, J. E., 1971: Multiple scattering of polarized light in 
!   planetary atmosphere. Part I. The doubling method, J. ATmos. Sci., 28, 120-125.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,NANG,KL
     TYPE(RTV_type), INTENT( INOUT ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp), INTENT(IN) :: single_albedo,optical_depth,Planck_Func

     ! internal variables
     REAL(fp), DIMENSION(NANG,NANG) :: term2,term3,term4,trans,refl
     REAL(fp), DIMENSION(NANG) :: C1, C2, source_up,source_down 
     REAL(fp) :: s, c
     INTEGER :: i,j,k,L
     INTEGER :: Error_Status
!

      !  Forward part beginning

      IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = ZERO  
        DO i = 1, NANG                   
          RTV%s_Layer_Trans(i,i,KL) = ONE 
        ENDDO 
        RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = ZERO 
        RTV%s_Layer_Source_DOWN(1:NANG,KL) = ZERO 
        RTV%s_Layer_Source_UP(1:NANG,KL) = ZERO 
        RETURN
      ENDIF

        ! -------------------------------------------------------------- !
        !  Determining number of doubling processes and constructing     !
        !  initial transmission and reflection matrix 
        !  --------------------------------------------------------------!
        RTV%Number_Doubling(KL)=INT(log(optical_depth/DELTA_OPTICAL_DEPTH)/log(TWO))+1
        IF( RTV%Number_Doubling(KL) < 1 ) RTV%Number_Doubling(KL) = 1
        IF( RTV%Number_Doubling(KL) <= MAX_N_DOUBLING ) THEN
          RTV%Delta_Tau(KL) = optical_depth/(TWO**RTV%Number_Doubling(KL))
        ELSE
          RTV%Number_Doubling(KL)=MAX_N_DOUBLING
          RTV%Delta_Tau(KL) = DELTA_OPTICAL_DEPTH
        ENDIF
        s = RTV%Delta_Tau(KL) * single_albedo
        DO i = 1, NANG
        c = s/COS_Angle(i)
        DO j = 1, NANG
        RTV%Refl(i,j,0,KL) = c * bb(i,j) * COS_Weight(j)
        RTV%Trans(i,j,0,KL) = c * ff(i,j) * COS_Weight(j)
        ENDDO
        RTV%Trans(i,i,0,KL) = RTV%Trans(i,i,0,KL) + ONE - RTV%Delta_Tau(KL)/COS_Angle(i)
        ENDDO

        ! -------------------------------------------------------------- !
        !  Doubling divided sub-layers                                   !
        !  --------------------------------------------------------------!
        DO L = 1, RTV%Number_Doubling(KL)
          DO i = 1, NANG
          DO j = 1, NANG
          term4(i,j) = ZERO
          DO k = 1, NANG
          term4(i,j) = term4(i,j) - RTV%Refl(i,k,L-1,KL)*RTV%Refl(k,j,L-1,KL)
          ENDDO
          ENDDO
          term4(i,i) = term4(i,i) + ONE
          ENDDO

        RTV%Inv_BeT(1:NANG,1:NANG,L,KL) = matinv(term4, Error_Status)
        IF( Error_Status /= SUCCESS ) THEN
          print *,' error at matinv in CRTM_Doubling_layer '
          RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = ZERO
          DO i = 1, NANG
            RTV%s_Layer_Trans(i,i,KL) = exp(-optical_depth/COS_Angle(i)) 
          ENDDO
          RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = ZERO 
          RTV%s_Layer_Source_DOWN(1:NANG,KL) = ZERO 
          RTV%s_Layer_Source_UP(1:NANG,KL) = ZERO 
          RETURN
        ENDIF

        term2 = matmul(RTV%Trans(1:NANG,1:NANG,L-1,KL), RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
        term3 = matmul(term2, RTV%Refl(1:NANG,1:NANG,L-1,KL))
        term3 = matmul(term3, RTV%Trans(1:NANG,1:NANG,L-1,KL))
 
        RTV%Refl(1:NANG,1:NANG,L,KL) = RTV%Refl(1:NANG,1:NANG,L-1,KL) + term3
        RTV%Trans(1:NANG,1:NANG,L,KL) = matmul(term2, RTV%Trans(1:NANG,1:NANG,L-1,KL))
        ENDDO

        trans = RTV%Trans(1:NANG,1:NANG,RTV%Number_Doubling(KL),KL)
        refl  = RTV%Refl(1:NANG,1:NANG,RTV%Number_Doubling(KL),KL)
!
!   computing source function at up and downward directions.
      DO i = 1, NANG
        C1(i) = ZERO
        C2(i) = ZERO
       DO j = 1, n_Streams 
        C1(i) = C1(i) + trans(i,j) 
        C2(i) = C2(i) + refl(i,j) 
       ENDDO
      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        C1(i) = C1(i)+trans(NANG,NANG)
      ENDIF
      ENDDO

      DO i = 1, NANG
        source_up(i) = (ONE-C1(i)-C2(i))*Planck_Func
        source_down(i) = source_up(i)
      ENDDO

        RTV%C1( 1:NANG,KL ) = C1
        RTV%C2( 1:NANG,KL ) = C2
        RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = trans
        RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = refl
        RTV%s_Layer_Source_DOWN(1:NANG,KL) = source_down
        RTV%s_Layer_Source_UP(1:NANG,KL) = source_up
       
      RETURN

      END SUBROUTINE CRTM_Doubling_layer
!
!
      SUBROUTINE CRTM_Doubling_layer_TL(n_streams, & ! Input, number of streams
                                             NANG, & ! Input, number of angles
                                               KL, & ! Input, number of angles
                                    single_albedo, & ! Input, single scattering albedo
                                    optical_depth, & ! Input, layer optical depth
                                        COS_Angle, & ! Input, COSINE of ANGLES
                                       COS_Weight, & ! Input, GAUSSIAN Weights
                                               ff, & ! Input, Phase matrix (forward part)
                                               bb, & ! Input, Phase matrix (backward part)
                                      Planck_Func, & ! Input, Planck for layer temperature
                                 single_albedo_TL, & ! Input, tangent-linear single albedo
                                 optical_depth_TL, & ! Input, TL layer optical depth
                                            ff_TL, & ! Input, TL forward Phase matrix
                                            bb_TL, & ! Input, TL backward Phase matrix
                                    Planck_Func_TL, & ! Input, TL Planck for layer temperature
                                              RTV, & ! Input, structure containing forward results 
                                         trans_TL, & ! Output, layer tangent-linear trans 
                                          refl_TL, & ! Output, layer tangent-linear refl 
                                     source_up_TL, & ! Output, layer tangent-linear source_up 
                                   source_down_TL)   ! Output, layer tangent-linear source_down 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute tangent-linear layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,NANG,KL
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
      REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
      REAL(fp), INTENT(IN) :: single_albedo,optical_depth,Planck_Func

      ! Tangent-Linear Part
      REAL(fp), INTENT(OUT), DIMENSION( :,: ) :: trans_TL,refl_TL
      REAL(fp), INTENT(OUT), DIMENSION( : ) :: source_up_TL,source_down_TL
      REAL(fp), INTENT(IN) :: single_albedo_TL
      REAL(fp), INTENT(IN) :: optical_depth_TL,Planck_Func_TL
      REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff_TL,bb_TL

      ! internal variables
      REAL(fp), DIMENSION(NANG,NANG) :: term1,term2,term3,term4,term5_TL
      REAL(fp) :: s, c
      REAL(fp) :: s_TL, c_TL, Delta_Tau_TL
      REAL(fp), DIMENSION(NANG) :: C1_TL, C2_TL
      INTEGER :: i,j,L
!
      ! Tangent-Linear Beginning

      IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        trans_TL = ZERO
        refl_TL = ZERO
        source_up_TL = ZERO
        source_down_TL = ZERO
        RETURN
      ENDIF

        s = RTV%Delta_Tau(KL) * single_albedo
        IF ( RTV%Number_Doubling( KL ) <= MAX_N_DOUBLING ) THEN
          Delta_Tau_TL = optical_depth_TL / (TWO**RTV%Number_Doubling( KL ) )        
        ELSE 
          Delta_Tau_TL = ZERO 
        ENDIF
        s_TL = Delta_Tau_TL * single_albedo + RTV%Delta_Tau(KL) * single_albedo_TL
        DO i = 1, NANG
        c = s/COS_Angle(i)
        c_TL = s_TL/COS_Angle(i)
        DO j = 1, NANG
        refl_TL(i,j) = c_TL*bb(i,j)*COS_Weight(j)+c*bb_TL(i,j)*COS_Weight(j)
        trans_TL(i,j) =c_TL*ff(i,j)*COS_Weight(j)+c*ff_TL(i,j)*COS_Weight(j)
        ENDDO
        trans_TL(i,i) =trans_TL(i,i) - Delta_Tau_TL/COS_Angle(i)
        ENDDO

        DO L = 1, RTV%Number_Doubling(KL) 

        term1 = matmul(RTV%Trans(1:NANG,1:NANG,L-1,KL),RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
        term2 = matmul(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Refl(1:NANG,1:NANG,L-1,KL))
        term3 = matmul(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Trans(1:NANG,1:NANG,L-1,KL))
        term4 = matmul(term2,RTV%Trans(1:NANG,1:NANG,L-1,KL))
        term5_TL = matmul(refl_TL,RTV%Refl(1:NANG,1:NANG,L-1,KL))  &
              + matmul(RTV%Refl(1:NANG,1:NANG,L-1,KL),refl_TL)

        refl_TL=refl_TL+matmul(matmul(term1,term5_TL),term4)+matmul(trans_TL,term4) &
               +matmul(matmul(term1,refl_TL),RTV%Trans(1:NANG,1:NANG,L-1,KL)) 
        refl_TL=refl_TL+matmul(matmul(term1,RTV%Refl(1:NANG,1:NANG,L-1,KL)),trans_TL)

        trans_TL=matmul(trans_TL,term3)  &
                +matmul(matmul(term1,term5_TL),term3)+matmul(term1,trans_TL)

        ENDDO

!   computing source function at up and downward directions.

      DO i = 1, NANG
        C1_TL(i) = ZERO
        C2_TL(i) = ZERO
       DO j = 1, n_Streams 
        C1_TL(i) = C1_TL(i) + trans_TL(i,j) 
        C2_TL(i) = C2_TL(i) + refl_TL(i,j) 
       ENDDO
      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        C1_TL(i) = C1_TL(i)+trans_TL(NANG,NANG)
      ENDIF
      ENDDO

      DO i = 1, NANG
        source_up_TL(i) = -(C1_TL(i)+C2_TL(i))*Planck_Func  &
         + (ONE-RTV%C1(i,KL)-RTV%C2(i,KL))*Planck_Func_TL
        source_down_TL(i) = source_up_TL(i)
      END DO

      RETURN

      END SUBROUTINE CRTM_Doubling_layer_TL


! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer adjoint transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------

  SUBROUTINE CRTM_Doubling_layer_AD(n_streams, & ! Input, number of streams
                                         NANG, & ! Input, number of angles
                                           KL, & ! Input, number of angles
                                single_albedo, & ! Input, single scattering albedo
                                optical_depth, & ! Input, layer optical depth
                                    COS_Angle, & ! Input, COSINE of ANGLES
                                   COS_Weight, & ! Input, GAUSSIAN Weights
                                           ff, & ! Input, Phase matrix (forward part)
                                           bb, & ! Input, Phase matrix (backward part)
                                  Planck_Func, & ! Input, Planck for layer temperature
                                     trans_AD, & ! Input, layer tangent-linear trans 
                                      refl_AD, & ! Input, layer tangent-linear refl 
                                 source_up_AD, & ! Input, layer tangent-linear source_up 
                               source_down_AD, & ! Input, layer tangent-linear source_down 
                                          RTV, & ! Input, structure containing forward results 
                             single_albedo_AD, & ! Output adjoint single scattering albedo
                             optical_depth_AD, & ! Output AD layer optical depth
                                        ff_AD, & ! Output AD forward Phase matrix
                                        bb_AD, & ! Output AD backward Phase matrix
                               Planck_Func_AD)   ! Output AD Planck for layer temperature


    INTEGER, INTENT(IN) :: n_streams,NANG,KL
    TYPE(RTV_type), INTENT(IN) :: RTV
    REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
    REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
    REAL(fp), INTENT(IN) :: single_albedo,optical_depth,Planck_Func

    ! Tangent-Linear Part
    REAL(fp), INTENT( INOUT ), DIMENSION( :,: ) :: trans_AD,refl_AD
    REAL(fp), INTENT( INOUT ), DIMENSION( : ) :: source_up_AD,source_down_AD
    REAL(fp), INTENT( INOUT ) :: single_albedo_AD
    REAL(fp), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD
    REAL(fp), INTENT(INOUT), DIMENSION(:,:) :: ff_AD,bb_AD

    ! internal variables
    REAL(fp), DIMENSION(NANG,NANG) :: term1,term2,term3,term4,term5_AD
    REAL(fp) :: s, c
    REAL(fp) :: s_AD, c_AD, Delta_Tau_AD
    REAL(fp), DIMENSION(NANG) :: C1_AD, C2_AD
    INTEGER :: i,j,L

    ! Tangent-Linear Beginning

    IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
      trans_AD = ZERO
      refl_AD = ZERO
      source_up_AD = ZERO
      source_down_AD = ZERO
      RETURN
    ENDIF

    DO i = NANG, 1, -1
      source_up_AD(i) = source_up_AD(i) + source_down_AD(i)
      source_down_AD(i) = ZERO
      C2_AD(i) = -source_up_AD(i)*Planck_Func
      C1_AD(i) = -source_up_AD(i)*Planck_Func
      Planck_Func_AD = Planck_Func_AD + (ONE-RTV%C1(i,KL)-RTV%C2(i,KL))*source_up_AD(i)
    END DO

    ! Compute the source function in the up and downward directions.
    DO i = NANG, 1, -1

      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        trans_AD(NANG,NANG)=trans_AD(NANG,NANG)+C1_AD(i)
      ENDIF

      DO j = n_Streams, 1, -1 
        refl_AD(i,j)=refl_AD(i,j)+C2_AD(i)
        trans_AD(i,j)=trans_AD(i,j)+C1_AD(i)
      END DO

    END DO

    DO L = RTV%Number_Doubling(KL), 1, -1 

      term1 = MATMUL(RTV%Trans(1:NANG,1:NANG,L-1,KL),RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
      term2 = MATMUL(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Refl(1:NANG,1:NANG,L-1,KL))
      term3 = MATMUL(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Trans(1:NANG,1:NANG,L-1,KL))
      term4 = MATMUL(term2,RTV%Trans(1:NANG,1:NANG,L-1,KL))

      term5_AD = MATMUL(MATMUL(TRANSPOSE(term1),trans_AD),TRANSPOSE(term3))
      trans_AD = MATMUL(trans_AD,TRANSPOSE(term3))+MATMUL(TRANSPOSE(term1),trans_AD)
    
      trans_AD=trans_AD+MATMUL(TRANSPOSE(MATMUL(term1,RTV%Refl(1:NANG,1:NANG,L-1,KL))),refl_AD) 

      term5_AD =term5_AD+MATMUL(MATMUL(TRANSPOSE(term1),refl_AD),TRANSPOSE(term4))
      trans_AD = trans_AD+MATMUL(refl_AD,TRANSPOSE(term4)) 
      refl_AD = refl_AD+MATMUL(MATMUL(TRANSPOSE(term1),refl_AD),TRANSPOSE(RTV%Trans(1:NANG,1:NANG,L-1,KL)))
      refl_AD = refl_AD+MATMUL(term5_AD,TRANSPOSE(RTV%Refl(1:NANG,1:NANG,L-1,KL)))
      refl_AD = refl_AD+MATMUL(TRANSPOSE(RTV%Refl(1:NANG,1:NANG,L-1,KL)),term5_AD)

    ENDDO

    s = RTV%Delta_Tau(KL) * single_albedo
    c_AD = ZERO
    s_AD = ZERO
    Delta_Tau_AD=ZERO

    DO i = NANG, 1, -1

      c = s/COS_Angle(i)
      Delta_Tau_AD = Delta_Tau_AD - trans_AD(i,i)/COS_Angle(i)

      DO j = NANG, 1, -1
        c_AD = c_AD + trans_AD(i,j)*ff(i,j)*COS_Weight(j)
        ff_AD(i,j)=ff_AD(i,j)+trans_AD(i,j)*c*COS_Weight(j)
        c_AD = c_AD + refl_AD(i,j)*bb(i,j)*COS_Weight(j)
        bb_AD(i,j)=bb_AD(i,j) + refl_AD(i,j)*c*COS_Weight(j)
      END DO

      s_AD = s_AD + c_AD/COS_Angle(i) 
      c_AD = ZERO

    ENDDO

    Delta_Tau_AD = Delta_Tau_AD + s_AD* single_albedo
    single_albedo_AD = single_albedo_AD+RTV%Delta_Tau(KL) * s_AD
    optical_depth_AD = optical_depth_AD + Delta_Tau_AD/(TWO**RTV%Number_Doubling(KL))

  END SUBROUTINE CRTM_Doubling_layer_AD

END MODULE SOI_Module


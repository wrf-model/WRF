!=======================================================================
!
   MODULE module_check_a_mundo

!<DESCRIPTION>
!
! Contains subroutines that check the consistency of some namelist 
! settings. Some namelist settings depend on other values in the 
! namelist. These subroutines reset the dependent values and write
! a message to stdout instead of detecting a fatal error and abort-
! ing on a parameter mis-match.  This works around depending on the
! user to set these specific settings in the namelist.
!
!   SUBROUTINE check_nml_consistency  :
!      Check namelist settings for consistency
!
!   SUBROUTINE set_physics_rconfigs:
!      Check namelist settings that determine memory allocations.
!
!</DESCRIPTION>

      USE module_state_description
      USE module_wrf_error
      USE module_configure

      IMPLICIT NONE

!=======================================================================

   CONTAINS

!=======================================================================

   SUBROUTINE  check_nml_consistency
 
!<DESCRIPTION>
!
! Check consistency of namelist settings
!
!</DESCRIPTION>

      IMPLICIT NONE

      INTEGER :: i

!-----------------------------------------------------------------------
! Check that all values of sf_surface_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_surface_physics(i)     .NE. &
              model_config_rec % sf_surface_physics(i-1) ) THEN
            wrf_err_message = '--- ERROR: sf_surface_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_surface_physics in namelist.input '
            CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         END IF
      ENDDO

#if ((EM_CORE == 1) && (DA_CORE != 1))
!-----------------------------------------------------------------------
! Check that if grid_sfdda is one, grid_fdda is also 1
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 1 ).AND. &
              ( model_config_rec%grid_fdda (i) .NE. 1 ) ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda = 1, then grid_fdda must also = 1 for that domain '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Change grid_fdda or grid_sfdda in namelist.input '
            CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         END IF
      ENDDO
#endif

   END SUBROUTINE 

!=======================================================================

   SUBROUTINE set_physics_rconfigs

!<DESCRIPTION>
!
! Some derived rconfig entries need to be set based on the value of other,
! non-derived entries before package-dependent memory allocation takes place.
! This works around depending on the user to set these specific settings in the
! namelist.
!
!</DESCRIPTION>

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Set the namelist parameters for the CAM radiation scheme if either 
! ra_lw_physics = CAMLWSCHEME or ra_sw_physics = CAMSWSCHEME.  
!-----------------------------------------------------------------------

      IF (( model_config_rec % ra_lw_physics(1) .EQ. CAMLWSCHEME ) .OR. & 
          ( model_config_rec % ra_sw_physics(1) .EQ. CAMSWSCHEME )) THEN
         model_config_rec % paerlev = 29
         model_config_rec % levsiz = 59
         model_config_rec % cam_abs_dim1 = 4 
         model_config_rec % cam_abs_dim2 = model_config_rec % e_vert(1)

         wrf_err_message = 'NOTE: CAM radiation is in use, setting:  ' // &
                           'paerlev=29, levsiz=59, cam_abs_dim1=4, cam_abs_dim2=e_vert'
         CALL wrf_message ( wrf_err_message )

      END IF

!-----------------------------------------------------------------------
! Set namelist parameter num_soil_levels depending on the value of 
! sf_surface_physics
!-----------------------------------------------------------------------

      IF ( model_config_rec % sf_surface_physics(1) .EQ. 0           ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. SLABSCHEME  ) &
           model_config_rec % num_soil_layers = 5
      IF ( model_config_rec % sf_surface_physics(1) .EQ. LSMSCHEME   ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME) &
           model_config_rec % num_soil_layers = 6
      IF ( model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME ) &
           model_config_rec % num_soil_layers = 2
      IF ( model_config_rec % sf_surface_physics(1) .EQ. 88          ) &
           model_config_rec % num_soil_layers = 4

      WRITE (wrf_err_message, FMT='(A,I6)') 'NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_message ( wrf_err_message )

   END SUBROUTINE set_physics_rconfigs

!=======================================================================

   END MODULE module_check_a_mundo

!=======================================================================

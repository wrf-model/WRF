!=======================================================================
!
   MODULE module_check_a_mundo

!<DESCRIPTION>
!
! Contains subroutines that check the consistency of some namelist
! settings. Some namelist settings depend on other values in the
! namelist. The routine check_nml_consistency can detect quite a
! few fatal inconsistencies. These are all bundled up as a convenience.
! The fatal errors are reported, and after the routine completes, then
! a single call to wrf_error_fatal is issued. The setup_physics_suite routine
! has only one fatal call, so that routine does not need this user-
! friendly concept of bundling errors. The set_physics_rconfigs
! routine does not detect any problems that would result in a fatal
! error, so the bundling of errors is also not required there.
!
!   SUBROUTINE check_nml_consistency :
!      Check namelist settings for consistency
!
!   SUBROUTINE setup_physics_suite :
!      Interpret user setting as referring to which supported schemes
!      Currently: conus and tropical
!
!   SUBROUTINE set_physics_rconfigs :
!      Check namelist settings that determine memory allocations.
!
!</DESCRIPTION>

      USE module_state_description
      USE module_model_constants
      USE module_wrf_error
      USE module_configure

      IMPLICIT NONE

!=======================================================================

   CONTAINS

!=======================================================================

   SUBROUTINE check_nml_consistency

!<DESCRIPTION>
!
! Check consistency of namelist settings
!
!</DESCRIPTION>

      IMPLICIT NONE

      LOGICAL :: exists, vnest
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      INTEGER :: i, j, oops, d1_value, EDMFMAX, SCHUMAX
      LOGICAL :: km_opt_already_done , diff_opt_already_done
      INTEGER :: count_opt
      LOGICAL :: lon_extent_is_global , lat_extent_is_global

!TWG2015
!
!FASDAS
!
      LOGICAL :: rinblw_already_done
!
!END FASDAS
!
      INTEGER :: count_fatal_error

      !  These functions are located with in the Urban Physics files, but
      !  not within the confines of the modules. Since we are in the share
      !  directory, we need to break possible circular build dependencies.

      INTERFACE
         INTEGER FUNCTION bep_nurbm()
         END FUNCTION bep_nurbm

         INTEGER FUNCTION bep_ndm()
         END FUNCTION bep_ndm

         INTEGER FUNCTION bep_nz_um()
         END FUNCTION bep_nz_um

         INTEGER FUNCTION bep_ng_u()
         END FUNCTION bep_ng_u

         INTEGER FUNCTION bep_nwr_u()
         END FUNCTION bep_nwr_u

         INTEGER FUNCTION bep_bem_nurbm()
         END FUNCTION bep_bem_nurbm

         INTEGER FUNCTION bep_bem_ndm()
         END FUNCTION bep_bem_ndm

         INTEGER FUNCTION bep_bem_nz_um()
         END FUNCTION bep_bem_nz_um

         INTEGER FUNCTION bep_bem_ng_u()
         END FUNCTION bep_bem_ng_u

         INTEGER FUNCTION bep_bem_nwr_u()
         END FUNCTION bep_bem_nwr_u

         INTEGER FUNCTION bep_bem_nf_u()
         END FUNCTION bep_bem_nf_u

         INTEGER FUNCTION bep_bem_ngb_u()
         END FUNCTION bep_bem_ngb_u

         INTEGER FUNCTION bep_bem_nbui_max() 
         END FUNCTION bep_bem_nbui_max
      END INTERFACE


!-----------------------------------------------------------------------
! Set up the WRF Hydro namelist option to allow dynamic allocation of
! variables.
!-----------------------------------------------------------------------
   count_fatal_error = 0
#ifdef WRF_HYDRO
   model_config_rec % wrf_hydro = 1
#else
   model_config_rec % wrf_hydro = 0
#endif

#if (NMM_CORE == 1) && (NMM_NEST == 1)
!-----------------------------------------------------------------------
! Ensure that minimum NMM corral distances are supplied for all domains.
!-----------------------------------------------------------------------
   do i=1,model_config_rec%max_dom
      if(model_config_rec%corral_x(i)<5) then
         call wrf_message("Corral X distance must be at least 5 due to intermediate domain halos.")
         model_config_rec%corral_x(i)=5
      endif
      if(model_config_rec%corral_y(i)<5) then
         call wrf_message("Corral Y distance must be at least 5 due to intermediate domain halos.")
         model_config_rec%corral_y(i)=5
      endif
    enddo
#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! AFWA diagnostics require each domain is treated the same. If
! any domain has an option activated, all domains must have that
! option activated.
!-----------------------------------------------------------------------
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_diag_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_diag_opt(:)   = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_ptype_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_ptype_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_vil_opt(i)    .EQ. 1 ) then
      model_config_rec%afwa_vil_opt(:)    = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_radar_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_radar_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_severe_opt(i) .EQ. 1 ) then
      model_config_rec%afwa_severe_opt(:) = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_icing_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_icing_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_cloud_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_cloud_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_vis_opt(i)    .EQ. 1 ) then
      model_config_rec%afwa_vis_opt(:)    = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_therm_opt(i)  .EQ. 1 ) then
      model_config_rec%afwa_therm_opt(:)  = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_turb_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_turb_opt(:)   = 1
      exit
    endif
  enddo
  do i=1,model_config_rec%max_dom
    if ( model_config_rec%afwa_buoy_opt(i)   .EQ. 1 ) then
      model_config_rec%afwa_buoy_opt(:)   = 1
      exit
    endif
  enddo

!-----------------------------------------------------------------------
! If any AFWA diagnostics are activated, there is a minimum that
! must always be activated.
!-----------------------------------------------------------------------
  do i=1,model_config_rec%max_dom
    if ( ( model_config_rec%afwa_ptype_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_vil_opt(i)    .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_radar_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_severe_opt(i) .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_icing_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_cloud_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_vis_opt(i)    .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_therm_opt(i)  .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_turb_opt(i)   .EQ. 1 ) .OR. &
         ( model_config_rec%afwa_buoy_opt(i)   .EQ. 1 ) ) then
      model_config_rec%afwa_diag_opt(i)=1
    endif
  enddo

!-----------------------------------------------------------------------
! LBC: Always the case, nested setup up: F, T, T, T
!-----------------------------------------------------------------------
   model_config_rec%nested(1)    = .FALSE.
   DO i=2,model_config_rec%max_dom
      model_config_rec%nested(i) = .TRUE.
   END DO 

!-----------------------------------------------------------------------
! LBC: Always the case, nested domain BCs are always false.
!-----------------------------------------------------------------------
   DO i=2,model_config_rec%max_dom
      model_config_rec%periodic_x(i)   = .FALSE.
      model_config_rec%symmetric_xs(i) = .FALSE.
      model_config_rec%symmetric_xe(i) = .FALSE.
      model_config_rec%open_xs(i)      = .FALSE.
      model_config_rec%open_xe(i)      = .FALSE.
      model_config_rec%periodic_y(i)   = .FALSE.
      model_config_rec%symmetric_ys(i) = .FALSE.
      model_config_rec%symmetric_ye(i) = .FALSE.
      model_config_rec%open_ys(i)      = .FALSE.
      model_config_rec%open_ye(i)      = .FALSE.
      model_config_rec%polar(i)        = .FALSE.
      model_config_rec%specified(i)    = .FALSE.
   END DO 

!-----------------------------------------------------------------------
! LBC: spec_bdy_width = spec_zone + relax_zone
!-----------------------------------------------------------------------
   IF ( model_config_rec%specified(1) ) THEN
      model_config_rec%spec_zone = 1
      model_config_rec%relax_zone = model_config_rec%spec_bdy_width - model_config_rec%spec_zone
   END IF
#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! Check that all values of diff_opt and km_opt are filled in.  A flag
! value of "-1" from the nml file means that this column (domain) is not
! filled as a max_doamins variable.  Since we changed these two variables
! from being single entries to max_domain entries, we need to do special
! checking.  If there are missing values (if we find any -1 entries), we
! fill those columns with the value from the entry from column (domain) #1.
!-----------------------------------------------------------------------

      km_opt_already_done = .FALSE.
      diff_opt_already_done = .FALSE.
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. -1 ) THEN
            model_config_rec % km_opt(i) = model_config_rec % km_opt(1)
            IF ( .NOT. km_opt_already_done ) THEN
               wrf_err_message = 'Setting blank km_opt entries to domain #1 values.'
               CALL wrf_debug ( 1, wrf_err_message )
               wrf_err_message = ' --> The km_opt entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, wrf_err_message )
            END IF
            km_opt_already_done = .TRUE.
         END IF
         IF ( model_config_rec % diff_opt(i) .EQ. -1 ) THEN
            model_config_rec % diff_opt(i) = model_config_rec % diff_opt(1)
            IF ( .NOT. diff_opt_already_done ) THEN
               wrf_err_message = 'Setting blank diff_opt entries to domain #1 values.'
               CALL wrf_debug ( 1, wrf_err_message )
               wrf_err_message = ' --> The diff_opt entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, wrf_err_message )
            END IF
            diff_opt_already_done = .TRUE.
         END IF
      ENDDO


!-----------------------------------------------------------------------
! Check that km_opt and diff_opt are not -1.  If the first column is set
! to -1, that means this entry is NOT in the namelist file at all.
!-----------------------------------------------------------------------

      IF ( ( model_config_rec %   km_opt(1) .EQ. -1 ) .OR. &
           ( model_config_rec % diff_opt(1) .EQ. -1 ) ) THEN
            wrf_err_message = '--- ERROR: Both km_opt and diff_opt need to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF


!-----------------------------------------------------------------------
! Check that SMS-3DTKE scheme (km_opt=5) Must work with diff_opt=2
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              model_config_rec % diff_opt(i) .NE. 2  ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme can only work with diff_opt=2 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or diff_opt in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Check that SMS-3DTKE scheme (km_opt=5) Must work with bl_pbl_physics=0
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              model_config_rec % bl_pbl_physics(i) .NE. 0  ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme can only work with bl_pbl_physics=0 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or bl_pbl_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Check that SMS-3DTKE scheme Must work with Revised MM5 surface layer 
! scheme (sf_sfclay_physics = 1), MYNN surface (sf_sfclay_physics = 5)
! and old MM5 surface scheme (sf_sfclay_physics = 91)
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % km_opt(i) .EQ. 5 .AND. &
              (model_config_rec % sf_sfclay_physics(i) .NE. sfclayscheme    .AND. &
               model_config_rec % sf_sfclay_physics(i) .NE. sfclayrevscheme .AND. &
               model_config_rec % sf_sfclay_physics(i) .NE. mynnsfcscheme  ) ) THEN
            wrf_err_message = '--- ERROR: SMS-3DTKE scheme works with sf_sfclay_physics = 1,5,91 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix km_opt or sf_sfclay_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Assign the dimensions for the urban options to the values defined in 
! each of those respective modules.
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % sf_urban_physics(i) == bepscheme     ) THEN
           model_config_rec % num_urban_ndm  = bep_ndm()
           model_config_rec % num_urban_nz   = bep_nz_um()
           model_config_rec % num_urban_ng   = bep_ng_u()
           model_config_rec % num_urban_nwr  = bep_nwr_u()
         END IF
         IF ( model_config_rec % sf_urban_physics(i) == bep_bemscheme ) THEN 
           model_config_rec % num_urban_ndm  = bep_bem_ndm()
           model_config_rec % num_urban_nz   = bep_bem_nz_um()
           model_config_rec % num_urban_ng   = bep_bem_ng_u()
           model_config_rec % num_urban_nwr  = bep_bem_nwr_u()
           model_config_rec % num_urban_nf   = bep_bem_nf_u()
           model_config_rec % num_urban_ngb  = bep_bem_ngb_u()
           model_config_rec % num_urban_nbui = bep_bem_nbui_max()
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Check that mosiac option cannot turn on when sf_urban_physics = 2 and 3
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % sf_surface_mosaic .EQ. 1 .AND. &
              (model_config_rec % sf_urban_physics(i) .EQ. 2 .OR. &
               model_config_rec % sf_urban_physics(i) .EQ. 3 ) ) THEN
            wrf_err_message = '--- ERROR: mosaic option cannot work with urban options 2 and 3 '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix sf_surface_mosaic and sf_urban_physics in namelist.input.'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Either: use Noah LSM without the mosaic option, OR change the urban option to 1 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Check that Deng Shallow Convection Must work with MYJ or MYNN PBL
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % shcu_physics(i) == dengshcuscheme .AND. &
              (model_config_rec % bl_pbl_physics(i) /= myjpblscheme .AND. &
               model_config_rec % bl_pbl_physics(i) /= mynnpblscheme2 ) ) THEN
            wrf_err_message = '--- ERROR: Deng shallow convection can only work with MYJ or MYNN PBL '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- ERROR: Fix shcu_physics or bl_pbl_physics in namelist.input.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! There is a binary file for Goddard radiation. It is single precision.
!-----------------------------------------------------------------------
# if ( defined(PROMOTE_FLOAT) || ( RWORDSIZE == DWORDSIZE ) )
      god_r8 : DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % ra_lw_physics(i) == goddardlwscheme ) .OR. &
              ( model_config_rec % ra_sw_physics(i) == goddardswscheme ) ) THEN
            wrf_err_message = '--- ERROR: Goddard radiation scheme cannot run with real*8 floats'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics and ra_sw_physics in namelist.input '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
            EXIT god_r8
         END IF
      ENDDO god_r8

# endif

!-----------------------------------------------------------------------
! Print a warning message for not using a combination of radiation and microphysics from Goddard
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( ( (model_config_rec % ra_lw_physics(i) == goddardlwscheme .OR. &
                 model_config_rec % ra_sw_physics(i) == goddardswscheme) .AND. &
                 model_config_rec % mp_physics(i) /= nuwrf4icescheme ) .OR. &
              (  model_config_rec % mp_physics(i) == nuwrf4icescheme .AND. &
                (model_config_rec % ra_lw_physics(i) /= goddardlwscheme .AND. &
                 model_config_rec % ra_sw_physics(i) /= goddardswscheme) ) ) THEN
            wrf_err_message = '--- WARNING: Goddard radiation and Goddard 4ice microphysics are not used together'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- WARNING: These options may be best to use together.'
            CALL wrf_message ( wrf_err_message )
         END IF
      ENDDO

#endif

!-----------------------------------------------------------------------
! Check that some microphysics is not allowed for WRF-NMM run
!-----------------------------------------------------------------------
#if (NMM_CORE == 1) || (HWRF == 1)
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % mp_physics(i) == nuwrf4icescheme .OR. &
              model_config_rec % mp_physics(i) == nssl_2mom .OR. &
              model_config_rec % mp_physics(i) == nssl_2momccn .OR. &
              model_config_rec % mp_physics(i) == nssl_1mom .OR. &
              model_config_rec % mp_physics(i) == nssl_1momlfo .OR. &
              model_config_rec % mp_physics(i) == nssl_2momg ) THEN
            wrf_err_message = '--- ERROR: Chosen microphysics scheme cannot run with WRF-NMM '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix mp_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
#endif

!-----------------------------------------------------------------------
! Check that all values of sf_surface_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_surface_physics(i)     .NE. &
              model_config_rec % sf_surface_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_surface_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_surface_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


!-----------------------------------------------------------------------
! Check that all values of sf_sfclay_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % sf_sfclay_physics(i)     .NE. &
              model_config_rec % sf_sfclay_physics(1) ) THEN
            wrf_err_message = '--- ERROR: sf_sfclay_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix sf_sfclay_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


!-----------------------------------------------------------------------
! Check that all values of mp_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % mp_physics(i)     .NE. &
              model_config_rec % mp_physics(1) ) THEN
            wrf_err_message = '--- NOTE: mp_physics must be equal for all domains '
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE:     ----> Setting all mp_physics entries to value defined in the inner most domain'
            CALL wrf_debug ( 1, wrf_err_message )
         END IF
      ENDDO
      d1_value = model_config_rec%mp_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%mp_physics(i) = d1_value
      END DO

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! There are restrictions on the AFWA diagnostics regarding the choice
! of microphysics scheme. These are hard coded in the AFWA diags driver,
! so while this is inelegant, it is about as good as we can do.
!-----------------------------------------------------------------------
      IF ( model_config_rec%afwa_diag_opt(1) .EQ. 1 ) THEN
         IF ( ( model_config_rec % mp_physics(1) .EQ. GSFCGCESCHEME   ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. ETAMPNEW        ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. THOMPSON        ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WSM5SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WSM6SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. WDM6SCHEME      ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. MORR_TWO_MOMENT ) .OR. &
              ( model_config_rec % mp_physics(1) .EQ. MORR_TM_AERO    ) ) THEN 
            !  All is OK
         ELSE
            wrf_err_message = '--- WARNING: the AFWA diagnostics option knows only about the following MP schemes:'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- GSFCGCESCHEME, ETAMPNEW, THOMPSON, WSM5SCHEME, WSM6SCHEME, MORR_TWO_MOMENT, MORR_TM_AERO, WDM6SCHEME'
            CALL wrf_message ( wrf_err_message )
         END IF
      END IF
#endif


!-----------------------------------------------------------------------
! Check that all values of ra_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_lw_physics(i)     .NE. &
              model_config_rec % ra_lw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_lw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_sw_physics(i)     .NE. &
              model_config_rec % ra_sw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_sw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_sw_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


!------------------------------------------------------------------------------
! Check that a value for time_step is given, and is not just set to default (-1)
!------------------------------------------------------------------------------

         IF ( ( model_config_rec % use_wps_input == 0 ) .AND. &
              ( model_config_rec % time_step .EQ. -1 ) ) THEN

            wrf_err_message = '--- ERROR: Known problem.  time_step must be set to a positive integer'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1

         END IF

!-----------------------------------------------------------------------
! Check that all values of bl_pbl_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. model_config_rec % bl_pbl_physics(1) ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. 0                                    ) ) THEN
            wrf_err_message = '--- ERROR: bl_pbl_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix bl_pbl_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


!-----------------------------------------------------------------------
! Check that all values of cu_physics are the same for all domains
! Note that a zero option is OK.
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( ( model_config_rec % cu_physics(i) .NE. model_config_rec % cu_physics(1) ) .AND. &
              ( model_config_rec % cu_physics(i) .NE. 0                                ) ) THEN
            wrf_err_message = '--- ERROR: cu_physics must be equal for all domains (or = zero)'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix cu_physics in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


#if ( defined NO_GAMMA_SUPPORT )
!-----------------------------------------------------------------------
! GF CU scheme requires an intrinsic gamma function. This is a 2008
! feature that not all compilers yet support.
!-----------------------------------------------------------------------

      GF_test : DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % cu_physics(i) .EQ. GFSCHEME ) THEN
            wrf_err_message = '--- ERROR: cu_physics GF uses an intrinsic gamma function that is not available with this compiler'
            CALL wrf_message ( TRIM( wrf_err_message ) )
            wrf_err_message = '--- Change compilers, or change cu_physics option in the namelist.input file.'
            CALL wrf_message ( TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
            EXIT GF_test
         END IF
      ENDDO GF_test
#endif

!-----------------------------------------------------------------------
! If fractional_seaice = 0, and tice2tsk_if2cold = .true, nothing will happen
!-----------------------------------------------------------------------

      IF ( ( model_config_rec%fractional_seaice .EQ. 0 ).AND. &
              ( model_config_rec%tice2tsk_if2cold ) ) THEN
            wrf_err_message = '--- WARNING: You set tice2tsk_if2cold = .true.,  but fractional_seaice = 0'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- WARNING: tice2tsk_if2cold will have no effect on results.'
            CALL wrf_debug ( 1, wrf_err_message )
      END IF

!-----------------------------------------------------------------------
! Check that if fine_input_stream /= 0, io_form_auxinput2 must also be in use
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%fine_input_stream(i) .NE. 0 ).AND. &
              ( model_config_rec%io_form_auxinput2 .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If fine_input_stream /= 0, io_form_auxinput2 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput2 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

#if (EM_CORE == 1)

!-----------------------------------------------------------------------
! Check that if num_metgrid_levels < 20, lagrange_order should be 1
!-----------------------------------------------------------------------
            IF  ( model_config_rec%num_metgrid_levels .LE. 20 ) THEN
            wrf_err_message = 'Linear vertical interpolation is recommended with input vertical resolution this coarse, changing lagrange_order to 1' 
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec%lagrange_order = 1
            END IF

!-----------------------------------------------------------------------
! Check for domain consistency for urban options.
!-----------------------------------------------------------------------

      d1_value = model_config_rec%sf_urban_physics(1)
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec%sf_urban_physics(i) /= d1_value ) THEN
            wrf_err_message = '--- NOTE:   sf_urban_physics option must be identical in each domain'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE:   ----> Resetting namelist values to that defined on the inner most domain'
            CALL wrf_debug ( 1, wrf_err_message )
         ENDIF
      END DO
      d1_value = model_config_rec%sf_urban_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%sf_urban_physics(i) = d1_value
      END DO

!------------------------------------------------------------------------
! Mills (2011) sea-ice albedo treatment only for Noah LSM and Noah-MP LSM
!------------------------------------------------------------------------
      IF ( model_config_rec%seaice_albedo_opt == 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( model_config_rec%sf_surface_physics(i) /= LSMSCHEME ) .AND. &
                 ( model_config_rec%sf_surface_physics(i) /= NOAHMPSCHEME ) ) THEN

               write (wrf_err_message, '(" --- ERROR:   seaice_albedo_opt == 1 works only with ")')
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               write (wrf_err_message, '("              sf_surface_physics == ", I2, " (Noah) or ", I2, " (Noah-MP).")') &
               LSMSCHEME, NOAHMPSCHEME
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1

            END IF
            
         END DO

      END IF


#endif

#if (NMM_CORE == 1) || (HWRF == 1)
!-----------------------------------------------------------------------
!           Check that NOAH-MP LSM is not allowed for WRF-NMM run
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%sf_surface_physics(i) == NOAHMPSCHEME ) THEN
            WRITE(wrf_err_message, '(" --- ERROR:   Noah-MP LSM scheme (sf_surface_physics==", I2, ")")') NOAHMPSCHEME
            CALL wrf_message ( TRIM ( wrf_err_message ) )
            WRITE(wrf_err_message, '("              does not work with NMM ")')
            CALL wrf_message ( TRIM ( wrf_err_message ) )
            WRITE(wrf_err_message, '("Select a different LSM scheme ")')
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END DO
#endif

!-----------------------------------------------------------------------
!           Check that NSAS shallow convection is not allowed to turn on simultaneously with NSAS
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) == nscvshcuscheme .AND. model_config_rec%cu_physics(i) == nsasscheme) THEN
            WRITE(wrf_err_message, '(" --- ERROR: NSCV shallow convection scheme is already included in NSAS ")')
            CALL wrf_message ( TRIM ( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END DO

#if (EM_CORE == 1)

!-----------------------------------------------------------------------
! Check if the bucket size for rain is > 0. If so, then we need to activate
! a derived namelist variable: bucketr_opt.
!-----------------------------------------------------------------------

   IF ( model_config_rec%bucket_mm .GT. 0. ) THEN
      model_config_rec%bucketr_opt = 1
   END IF

!-----------------------------------------------------------------------
! Check if the bucket size for radiation is > 0. If so, then we need to activate
! a derived namelist variable: bucketf_opt.
!-----------------------------------------------------------------------

   IF ( model_config_rec%bucket_J .GT. 0. ) THEN
      model_config_rec%bucketf_opt = 1
   END IF

!-----------------------------------------------------------------------
! Check if the precip bucket reset time interval > 0. If so, then we need to 
! activate a derived namelist variable: prec_acc_opt
!-----------------------------------------------------------------------

   DO i = 1, model_config_rec % max_dom
      IF ( model_config_rec%prec_acc_dt(i) .GT. 0. ) THEN
         model_config_rec%prec_acc_opt = 1
      END IF
   END DO

!-----------------------------------------------------------------------
! Check if any stochastic perturbation scheme is turned on in any domain,
! if so, set derived variable sppt_on=1 and/or rand_perturb_on and/or skebs_on=1
!-----------------------------------------------------------------------

   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % sppt(i) .ne. 0)  then
           model_config_rec % sppt_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc. are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for SPPT'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % rand_perturb(i) .ne. 0)  then
           model_config_rec % rand_perturb_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF (( model_config_rec % spp_conv(i) .ne. 0).or.( model_config_rec % spp_pbl(i) .ne. 0).or. (model_config_rec % spp_lsm(i) .ne. 0)  &
           .or. ( model_config_rec % spp(i) .ne. 0))  then
           model_config_rec % spp_on=1
           IF (( model_config_rec%KMINFORCT .ne. 1) .or. (model_config_rec%KMAXFORCT .ne. 1000000) .or.   &
               ( model_config_rec%LMINFORCT .ne. 1) .or. (model_config_rec%LMAXFORCT .ne. 1000000)) then    
               wrf_err_message = '--- Warning: the namelist parameter "kminforct" etc are for SKEBS only'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '             and should not be changed from their default value for RAND_PERTURB'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
           endif
         endif
         IF ( model_config_rec % spp(i) .ne. 0)  then
           model_config_rec % spp_conv=1
           model_config_rec % spp_pbl=1
           model_config_rec % spp_lsm=1
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_vertstruc_opt(i) ==1 )  then
           model_config_rec % skebs_vertstruc=1       ! parameter stoch_vertstruc_opt is being replaced with skebs_vertstruc
                                                      ! stoch_vertstruc_opt is obsolete starting with V3.7
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_vertstruc_opt" is obsolete.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
           wrf_err_message = '             Please replace with namelist parameter "skebs_vertstruc" in V3.7 and later versions.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         endif
   ENDDO

   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_force_opt(i) ==1 )  THEN
           model_config_rec % skebs(i)=1    ! parameter stoch_forc_opt is being replaced with skebs;
                                            ! stoch_vertstruc_opt is obsolete starting with V3.7
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_force_opt" is obsolete.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
           wrf_err_message = '             Please replace with namelist parameter "skebs" in V3.7 and later versions.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         ENDIF
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % skebs(i) .ne. 0)  then
           model_config_rec % skebs_on=1
         endif
   ENDDO

!-----------------------------------------------------------------------
! Random fields are by default thin 3D arrays (:,1,:).
! If random fields have vertical structures (stoch_vertstruc_opt .ne. 0)
! make them full 3D array arrays
!-----------------------------------------------------------------------
   IF ( model_config_rec % skebs_vertstruc     .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % sppt_vertstruc      .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF
   IF ( model_config_rec % rand_pert_vertstruc .ne. 99 )  then
      model_config_rec % num_stoch_levels = model_config_rec %e_vert(1)
   ENDIF

!--------------------------------------------------------------------------------
! Check if boundary perturbations is turned on and set to '1' (perturb_bdy=1).
! If so, make sure skebs_on is also turned on.
!--------------------------------------------------------------------------------
   IF ( model_config_rec % perturb_bdy .EQ. 1 ) then
        model_config_rec % skebs_on=1
         wrf_err_message = '--- WARNING: perturb_bdy=1 option uses SKEBS pattern and may'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         wrf_err_message = '             increase computation time.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
   ENDIF

!--------------------------------------------------------------------------------
! Check if chemistry boundary perturbations is turned on and set to '1' (perturb_chem_bdy=1).
! If so, make sure rand_perturb_on is also turned on.
! perturb_chem_bdy can be turned on only if WRF_CHEM is also compiled.
! If perturb_chem_bdy=1, then have_bcs_chem should be turned on as well.
!--------------------------------------------------------------------------------

   IF ( model_config_rec % perturb_chem_bdy .EQ. 1 ) then

#if (WRF_CHEM != 1)
      wrf_err_message = '--- ERROR: This option is only for WRF_CHEM.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
#endif

!NOTE      model_config_rec % rand_perturb_on=1
         wrf_err_message = '--- WARNING: perturb_chem_bdy=1 option uses RAND pattern and may'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         wrf_err_message = '             increase computation time.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

#if (WRF_CHEM == 1)
      IF ( .NOT. model_config_rec % have_bcs_chem(1) ) THEN
            wrf_err_message = '--- ERROR: This perturb_chem_bdy option needs '// &
                              'have_bcs_chem = .true. in chem.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      ENDIF
#endif

   ENDIF

!----------------------------------------------------------------------------
! If trajectory option is turned off, make sure the number of trajectories is
! zero.
!----------------------------------------------------------------------------
   IF ( ( model_config_rec%traj_opt .EQ. 0 ) .AND. &
        ( model_config_rec%num_traj .NE. 0 ) ) THEN
         wrf_err_message = '--- WARNING: traj_opt is zero, but num_traj is not zero; setting num_traj to zero.'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
         model_config_rec%num_traj = 0
   END IF

#elif( NMM_CORE == 1 )
!----------------------------------------------------------------------------
! If NMM core and trajectories are on then halt.
!----------------------------------------------------------------------------
   IF ( model_config_rec%traj_opt /= 0 ) THEN
         wrf_err_message = 'Trajectories not supported in NMM core '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
   END IF
#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! In program real, if hypsometric_opt = 2, adjust_heights cannot be set to .true.
!-----------------------------------------------------------------------
      IF ( model_config_rec%hypsometric_opt .EQ. 2 &
           .AND. model_config_rec%adjust_heights ) THEN
         wrf_err_message = '--- NOTE: hypsometric_opt is 2, setting adjust_heights = F'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%adjust_heights = .false.
      ENDIF
#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! cu_physics = 11 (scale-aware KF) only works with YSU PBL.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. YSUSCHEME ) .AND. &
              ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) ) THEN
            oops = oops + 1
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: bl_pbl_physics must be set to 1 for cu_physics = 11 '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Fix bl_pbl_physics in namelist.input OR use another cu_physics option '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF

      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) THEN
            wrf_err_message = '--- NOTE: cu_physics is 11, setting icloud = 1 and cu_rad_feedback = T'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%cu_rad_feedback(i) = .true.
            model_config_rec%icloud = 1
         END IF
      ENDDO
!-----------------------------------------------------------------------
! aercu_opt = 1 (CESM-aerosal) only works with MSKF, special Morrison
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%aercu_opt .GT. 0 .AND.       &
              ( model_config_rec%cu_physics(i) .NE. MSKFSCHEME .OR. &
              model_config_rec%mp_physics(i) .NE. MORR_TM_AERO ) ) THEN
              oops = oops + 1
         END IF
      ENDDO

      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: aercu_opt requires cu_physics = 11, and mp_physics = 40 '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Fix these options in namelist.input if you would like to use aercu_opt'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
! Set the namelist parameters for the aercu_opt > 0
!-----------------------------------------------------------------------

      IF ( model_config_rec % aercu_opt .GT. 0 ) THEN
         model_config_rec % alevsiz_cu = 30
         model_config_rec % no_src_types_cu = 10
         DO i = 1, model_config_rec % max_dom
            model_config_rec % scalar_pblmix(i) = 1
         END DO

         wrf_err_message = '--- NOTE: aercu_opt is in use, setting:  ' // &
                           'alevsiz_cu=30, no_src_types_cu=10, scalar_pblmix = 1'
         CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

      END IF

#endif

!-----------------------------------------------------------------------
! If sst_update = 0, set io_form_auxinput4 to 0 so WRF will not try to
! input the data; auxinput_interval must also be 0
!-----------------------------------------------------------------------

      IF ( model_config_rec%sst_update .EQ. 0 ) THEN
         model_config_rec%io_form_auxinput4 = 0
         DO i = 1, model_config_rec % max_dom
            wrf_err_message = '--- NOTE: sst_update is 0, ' // &
                  'setting io_form_auxinput4 = 0 and auxinput4_interval = 0 for all domains'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            model_config_rec%auxinput4_interval(i)   = 0
            model_config_rec%auxinput4_interval_y(i) = 0
            model_config_rec%auxinput4_interval_d(i) = 0
            model_config_rec%auxinput4_interval_h(i) = 0
            model_config_rec%auxinput4_interval_m(i) = 0
            model_config_rec%auxinput4_interval_s(i) = 0
         ENDDO
      ELSE
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF

!-----------------------------------------------------------------------
! If sst_update = 1, we need to make sure that two nml items are set:
!   1. io_form_auxinput4 = 2 (only for one domain)
!   2. auxinput4_interval = NON-ZERO (just check most coarse domain)
!-----------------------------------------------------------------------

      IF ( model_config_rec%sst_update .EQ. 1 ) THEN
         IF ( model_config_rec%io_form_auxinput4 .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, io_form_auxinput4 must be /= 0'
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            wrf_err_message = '--- Set io_form_auxinput4 in the time_control namelist (probably to 2).'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF

         IF ( ( model_config_rec%auxinput4_interval(1)   .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_y(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_d(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_h(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_m(1) .EQ. 0 ) .AND. &
              ( model_config_rec%auxinput4_interval_s(1) .EQ. 0 ) ) THEN
            wrf_err_message = '--- ERROR: If sst_update /= 0, one of the auxinput4_interval settings must be /= 0'
            CALL wrf_debug ( 0, TRIM(wrf_err_message) )
            wrf_err_message = '--- Set auxinput4_interval_s to the same value as interval_seconds (usually a pretty good guess).'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      END IF

!-----------------------------------------------------------------------
! The qndropsource relies on the flag PROGN (when not running chemistry)
! and is always allocated when running WRF Chem.
!-----------------------------------------------------------------------

#if ( (EM_CORE == 1) && (WRF_CHEM != 1) )
      model_config_rec%alloc_qndropsource = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%progn(i) .EQ. 1 ) THEN
            model_config_rec%alloc_qndropsource = 1
         END IF
      END DO

#elif (WRF_CHEM == 1)
      model_config_rec%alloc_qndropsource = 1
#endif

#if ((EM_CORE == 1) && (DA_CORE != 1))
!-----------------------------------------------------------------------
! Check that if grid_sfdda is one, grid_fdda is also 1
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%grid_sfdda(i) .GT. 0 ).AND. &
              ( model_config_rec%grid_fdda (i) .NE. 1 ) ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda >= 1, then grid_fdda must also = 1 for that domain '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Change grid_fdda or grid_sfdda in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! If grid_fdda or grid_sfdda is 0 for any domain, all interval and
! ending time information that domain must be set to zero.  For
! surface fdda, we also need to make sure that the PXLSM soil nudging
! switch is also zero.  Either surface fdda or soil nudging with the
! PX scheme are enough to allow the surface fdda file to be read.
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom

         IF ( model_config_rec%grid_fdda(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: grid_fdda is 0 for domain ', &
                         i, ', setting gfdda interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%gfdda_end_y(i) = 0
            model_config_rec%gfdda_end_d(i) = 0
            model_config_rec%gfdda_end_h(i) = 0
            model_config_rec%gfdda_end_m(i) = 0
            model_config_rec%gfdda_end_s(i) = 0
            model_config_rec%gfdda_interval(i)   = 0
            model_config_rec%gfdda_interval_y(i) = 0
            model_config_rec%gfdda_interval_d(i) = 0
            model_config_rec%gfdda_interval_h(i) = 0
            model_config_rec%gfdda_interval_m(i) = 0
            model_config_rec%gfdda_interval_s(i) = 0
         END IF

         IF ( ( model_config_rec%grid_sfdda(i) .EQ. 0 ) .AND. &
              ( model_config_rec%pxlsm_soil_nudge(i) .EQ. 0 ) ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') &
                         '--- NOTE: both grid_sfdda and pxlsm_soil_nudge are 0 for domain ', &
                         i, ', setting sgfdda interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%sgfdda_end_y(i) = 0
            model_config_rec%sgfdda_end_d(i) = 0
            model_config_rec%sgfdda_end_h(i) = 0
            model_config_rec%sgfdda_end_m(i) = 0
            model_config_rec%sgfdda_end_s(i) = 0
            model_config_rec%sgfdda_interval(i)   = 0
            model_config_rec%sgfdda_interval_y(i) = 0
            model_config_rec%sgfdda_interval_d(i) = 0
            model_config_rec%sgfdda_interval_h(i) = 0
            model_config_rec%sgfdda_interval_m(i) = 0
            model_config_rec%sgfdda_interval_s(i) = 0
         END IF

         IF ( model_config_rec%obs_nudge_opt(i) .EQ. 0 ) THEN
            WRITE (wrf_err_message, FMT='(A,I6,A)') '--- NOTE: obs_nudge_opt is 0 for domain ', &
                         i, ', setting obs nudging interval and ending time to 0 for that domain.'
            CALL wrf_debug ( 1, TRIM( wrf_err_message ) )

            model_config_rec%fdda_end(i) = 0
            model_config_rec%auxinput11_interval(i)   = 0
            model_config_rec%auxinput11_interval_y(i) = 0
            model_config_rec%auxinput11_interval_d(i) = 0
            model_config_rec%auxinput11_interval_h(i) = 0
            model_config_rec%auxinput11_interval_m(i) = 0
            model_config_rec%auxinput11_interval_s(i) = 0
            model_config_rec%auxinput11_end(i)   = 0
            model_config_rec%auxinput11_end_y(i) = 0
            model_config_rec%auxinput11_end_d(i) = 0
            model_config_rec%auxinput11_end_h(i) = 0
            model_config_rec%auxinput11_end_m(i) = 0
            model_config_rec%auxinput11_end_s(i) = 0
         END IF

      ENDDO      ! Loop over domains

!-----------------------------------------------------------------------
! If grid_sfdda = 2, we turn it into derived namelist fasdas
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         model_config_rec%fasdas(i) = 0
         IF ( model_config_rec%grid_sfdda(i) .EQ. 2 ) THEN
            model_config_rec%fasdas(i) = 1
         END IF
      ENDDO
!
!-----------------------------------------------------------------------
! FASDAS:  Check that rinblw is set for max_domains in the namelist if sffdda is active
!-----------------------------------------------------------------------
    rinblw_already_done = .FALSE.
    DO j = 1, model_config_rec%max_dom
    IF (model_config_rec%grid_sfdda(j) .EQ. 1 ) THEN
      DO i = 2, model_config_rec%max_dom
         IF ( model_config_rec%rinblw(i) .EQ. -1 ) THEN
            model_config_rec%rinblw(i) = model_config_rec % rinblw(1)
            IF ( .NOT. rinblw_already_done ) THEN
               wrf_err_message = 'Setting blank rinblw entries to domain #1 values.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
               wrf_err_message = ' --> The rinblw entry in the namelist.input is now max_domains.'
               CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
            END IF
            rinblw_already_done = .TRUE.
         END IF
       ENDDO

!------------------------------------------------------------------------
! Check that rinblw is not -1 if sfdda is active
!------------------------------------------------------------------------
       IF ( model_config_rec%rinblw(1) .EQ. -1 ) THEN
            wrf_err_message = '--- ERROR: rinblw needs to be set in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
       END IF
    END IF
    END DO

!------------------------------------------------------------------------
! Check to see if FASDAS is active
!------------------------------------------------------------------------
    DO i = 1, model_config_rec%max_dom
     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
        wrf_err_message = 'FASDAS is active. Mixed Layer fdda is inactive'
        CALL wrf_debug ( 1, TRIM( wrf_err_message ) )
     END IF

!------------------------------------------------------------------------
! Check to make sure sfdda is active if FASDAS is in namelist
!------------------------------------------------------------------------
!     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
!       IF (model_config_rec%grid_sfdda(i) .EQ. 0) THEN
!        wrf_err_message = '--- ERROR: sfdda needs to be set in the namelist.input file to run FASDAS.'
!        CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
!        count_fatal_error = count_fatal_error + 1
!       END IF
!     END IF
     END DO
!
!END FASDAS
!
!-----------------------------------------------------------------------
!  Only implement the mfshconv option if the QNSE PBL is activated.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .NE. QNSEPBLSCHEME ) .AND. &
              ( model_config_rec%mfshconv(i) .NE. 0 ) ) THEN
            model_config_rec%mfshconv(i) = 0
            oops = oops + 1
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 4, implies mfshconv must be 0, resetting'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  shcu_physics = 3 (grimsshcuscheme) only works with YSU & MYNN PBL.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .EQ. GRIMSSHCUSCHEME ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. SHINHONGSCHEME) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
               !NO PROBLEM
            ELSE
               model_config_rec%shcu_physics(i) = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6,11 implies shcu_physics cannot be 3, resetting'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

!-----------------------------------------------------------------------
! If MYNN PBL is not used, set bl_mynn_edmf = 0 so that shallow convection
! options can be set and we don't get additional output
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME2 ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME3 ) ) THEN
              model_config_rec % bl_mynn_edmf(i) = 0
         END IF
      ENDDO

!-----------------------------------------------------------------------
!  bl_mynn_edmf > 0 over-rules both shcu_physics & ishallow
!-----------------------------------------------------------------------

      oops = 0
      EDMFMAX = MAXVAL(model_config_rec%bl_mynn_edmf(1:model_config_rec%max_dom))
      SCHUMAX = MAXVAL(model_config_rec%shcu_physics(1:model_config_rec%max_dom))
         IF ( ( ( EDMFMAX .GT. 0 ) .AND. ( SCHUMAX .GT. 0 ) ) .OR. &
              ( ( EDMFMAX .GT. 0 ) .AND. ( model_config_rec%ishallow .GT. 0 ) ) ) THEN
            wrf_err_message = '--- ERROR: bl_mynn_edmf > 0 requires both shcu_physics=0 & ishallow=0' 
            CALL wrf_message(wrf_err_message)
            wrf_err_message = 'when using MYNN PBL, by default bl_mynn_edmf is turned on'
            CALL wrf_message(wrf_err_message)
            wrf_err_message = 'Modify namelist.input so that shcu_physics nor ishallow is used when bl_mynn_edmf is turned on'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF

!-----------------------------------------------------------------------
!  We need to know if any of the cumulus schemes are active. This
!  allows the model to allocate space.
!-----------------------------------------------------------------------

      model_config_rec%cu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .NE. NOCUSCHEME ) THEN
            model_config_rec%cu_used = 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
!  We need to know if any of the shallow cumulus schemes are active. This
!  allows the model to allocate space.
!-----------------------------------------------------------------------

      model_config_rec%shcu_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .NE. NOSHCUSCHEME ) THEN
            model_config_rec%shcu_used = 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! Make sure icloud_bl is only used when MYNN is chosen.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%icloud_bl .eq. 1) THEN
           IF ( model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2 .OR. &
                model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3 ) THEN
              !CORRECTLY CONFIGURED
           ELSE
              model_config_rec%icloud_bl = 0
              oops = oops + 1
           END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'Need MYNN PBL for icloud_bl = 1, resetting to 0'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

!-----------------------------------------------------------------------
! Make sure microphysics option without QICE array cannot be used with icloud=3
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%icloud .eq. 3) THEN
           IF ( model_config_rec%mp_physics(i) .EQ. WSM3SCHEME .OR. &
                model_config_rec%mp_physics(i) .EQ. KESSLERSCHEME ) THEN
                oops = oops + 1
           END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: Need microphysics schemes with QICE array for icloud = 3'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Choose a microphysics scheme other than WSM3 and Kessler'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
!  If analysis FDDA is turned off, reset the io_forms to zero so that
!  there is no chance that WRF tries to input the data.
!-----------------------------------------------------------------------

      IF ( MAXVAL( model_config_rec%grid_fdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_gfdda = 0
      ELSE
         IF ( model_config_rec%io_form_gfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_fdda /= 0, io_form_gfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_gfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF
      IF ( MAXVAL( model_config_rec%grid_sfdda ) .EQ. 0 ) THEN
         model_config_rec%io_form_sgfdda = 0
      ELSE
         IF ( model_config_rec%io_form_sgfdda .EQ. 0 ) THEN
            wrf_err_message = '--- ERROR: If grid_sfdda /= 0, io_form_sgfdda must be /= 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Set io_form_sgfdda in the time_control namelist (probably to 2).'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
         END IF
      END IF

!-----------------------------------------------------------------------
! If we have asked for the pressure-level diagnostics, make sure we can output them
!-----------------------------------------------------------------------

      IF ( model_config_rec%p_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( MAX ( model_config_rec%auxhist23_interval  (i) , &
                         model_config_rec%auxhist23_interval_d(i) , &
                         model_config_rec%auxhist23_interval_h(i) , &
                         model_config_rec%auxhist23_interval_m(i) , &
                         model_config_rec%auxhist23_interval_s(i) ) == 0 ) .OR. &
                 (  model_config_rec%io_form_auxhist23 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: p_lev_diags requires auxhist23 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist23_interval (max_dom) and io_form_auxhist23'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 23 for pressure-level diags'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            model_config_rec%p_lev_interval(i) = model_config_rec%auxhist23_interval  (i)*   60 + &
                                                 model_config_rec%auxhist23_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist23_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist23_interval_m(i)*   60 + &
                                                 model_config_rec%auxhist23_interval_s(i)
         END DO
      END IF


!-----------------------------------------------------------------------
! If we have asked for the height-level diagnostics, make sure we can output them
!-----------------------------------------------------------------------

      IF ( model_config_rec%z_lev_diags .EQ. 1 ) THEN
         DO i = 1, model_config_rec % max_dom
            IF ( ( MAX ( model_config_rec%auxhist22_interval  (i) , &
                         model_config_rec%auxhist22_interval_d(i) , &
                         model_config_rec%auxhist22_interval_h(i) , &
                         model_config_rec%auxhist22_interval_m(i) , &
                         model_config_rec%auxhist22_interval_s(i) ) == 0 ) .OR. &
                 (  model_config_rec%io_form_auxhist22 == 0 ) ) THEN
               wrf_err_message = '--- ERROR: z_lev_diags requires auxhist22 file information'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- ERROR: provide: auxhist22_interval (max_dom) and io_form_auxhist22'
               CALL wrf_message ( wrf_err_message )
               wrf_err_message = '--- Add supporting IO for stream 22 for height-level diags'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END DO
         DO i = 1, model_config_rec % max_dom
            model_config_rec%z_lev_interval(i) = model_config_rec%auxhist22_interval  (i)*   60 + &
                                                 model_config_rec%auxhist22_interval_d(i)*86400 + &
                                                 model_config_rec%auxhist22_interval_h(i)* 3600 + &
                                                 model_config_rec%auxhist22_interval_m(i)*   60 + &
                                                 model_config_rec%auxhist22_interval_s(i)
         END DO
      END IF

!-----------------------------------------------------------------------
! For RASM Diagnostics
! -verify that only one time interval is specified
! -change the intervals to values used in RASM Diagnotics
! -verify that a time interval has been set
!-----------------------------------------------------------------------

! 1. Only one time interval type specified

      DO i = 1, model_config_rec % max_dom
         count_opt = 0
         IF ( model_config_rec%mean_diag_interval_s (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_m (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_h (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_d (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval_mo(i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( model_config_rec%mean_diag_interval   (i) .GT. 0 ) THEN
            count_opt = count_opt + 1
         END IF
         IF ( count_opt .GT. 1 ) THEN
            wrf_err_message = '--- ERROR:  Only use one of: mean_diag_interval, _s, _m, _h, _d, _mo '
            CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
      END DO

! 2. Put canonical intervals into RASM expected form

      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mean_diag_interval_s (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_s (i)
            model_config_rec%mean_freq = 1
         END IF
         IF ( model_config_rec%mean_diag_interval_m (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_m (i)
            model_config_rec%mean_freq = 2
         END IF
         IF ( model_config_rec%mean_diag_interval_h (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_h (i)
            model_config_rec%mean_freq = 3
         END IF
         IF ( model_config_rec%mean_diag_interval_d (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_d (i)
            model_config_rec%mean_freq = 4
         END IF
         IF ( model_config_rec%mean_diag_interval_mo(i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval_mo(i)
            model_config_rec%mean_freq = 5
         END IF
         IF ( model_config_rec%mean_diag_interval   (i) .GT. 0 ) THEN
            model_config_rec%mean_interval(i) = model_config_rec%mean_diag_interval   (i)
            model_config_rec%mean_freq = 2
         END IF
      END DO

! 3. If requested, need an interval.

      IF ( model_config_rec%mean_diag .EQ. 1 ) THEN
         count_opt = 0
         DO i = 1, model_config_rec % max_dom
            IF ( model_config_rec%mean_interval   (i) .GT. 0 ) THEN
               count_opt = count_opt + 1
            END IF
         END DO
         IF ( count_opt .LT. 1 ) THEN
            wrf_err_message = '--- ERROR:  mean_diag = 1, but no computation interval given'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '            Use one of: mean_diag_interval, _s, _m, _h, _d, _mo '
            CALL wrf_message ( wrf_err_message )
            count_fatal_error = count_fatal_error + 1
         END IF
      END IF

!-----------------------------------------------------------------------
! For nwp_diagnostics = 1, history_interval must be used.           
!-----------------------------------------------------------------------

      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%history_interval(1) .EQ. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  nwp_diagnostics requires the use of "history_interval" namelist.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace interval variable with "history_interval".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
! If a user sets nwp_diagnostics = 1, then radar reflectivity computation
! needs to happen
!-----------------------------------------------------------------------

      IF ( model_config_rec % nwp_diagnostics == 1 ) model_config_rec % do_radar_ref = 1

!-----------------------------------------------------------------------
! If hailcast_opt = 1 for any domain, convective parameterization must be off for that domain.           
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%hailcast_opt(i) .NE. 0 ) .AND. &
              (model_config_rec%cu_physics(i) .NE. 0) ) THEN
              wrf_err_message = '--- hailcast_opt and cu_physics cannot both be turned on for the same domain. You must turn one of them off (=0).'
              CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
              count_fatal_error = count_fatal_error + 1
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! Name change in the namelist.input file.  We used to only have the
! ocean mixed layer option (omlcall=1).  With the addition of a 3D ocean,
! now let's change the name of the option.  If the old name is present,
! tell the user to swap their namelist, and then stop.
!-----------------------------------------------------------------------

      IF ( model_config_rec%omlcall .NE. 0 ) THEN
         wrf_err_message = '--- ERROR:  The namelist.input variable "omlcall" has been renamed.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace "omlcall" with the new name "sf_ocean_physics".'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
! For adaptive time stepping, certain physics schemes are not allowed
! to have intermittent frequencies.  So, for those schemes below, we just
! turn the frequencies so that the schemes are called for each time step.
!-----------------------------------------------------------------------

      IF ( model_config_rec%use_adaptive_time_step ) THEN
         IF ( ( model_config_rec%cu_physics(1) .EQ. BMJSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SCALESASSCHEME) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SASSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. OSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. KSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. NSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. TIEDTKESCHEME ) ) THEN
            wrf_err_message = '--- WARNING: If use_adaptive_time_step, must use cudt=0 for the following CU schemes:'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '---          BMJ, all SAS, Tiedtke'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '---          CUDT=0 has been done for you.'
            CALL wrf_debug ( 1, wrf_err_message )
            DO i = 1, model_config_rec % max_dom
               model_config_rec%cudt(i) = 0
            END DO
         END IF
      END IF

!-----------------------------------------------------------------------
! When digital filtering is turned on, if no specific time step is given to be
! used during the digitial filtering period, then the standard WRF time
! step is used.  If neither time steps are specified, then fatal error.
!-----------------------------------------------------------------------

      IF ( .NOT. model_config_rec%dfi_opt .EQ. DFI_NODFI ) THEN
         IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
            model_config_rec%time_step_dfi = model_config_rec%time_step
            IF ( model_config_rec%time_step_dfi .EQ. -1 ) THEN
               wrf_err_message = '--- ERROR: DFI Timestep or standard WRF time step must be specified.'
               CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
               count_fatal_error = count_fatal_error + 1
            END IF
         END IF
      END IF

!-----------------------------------------------------------------------
! The cu_rad_feedback namelist flag with the two Grell cumulus parameterization
! schemes needs to have the namelist flag cu_diag=1
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_rad_feedback(i) .EQV. .TRUE. )  .OR. &
              ( model_config_rec%cu_rad_feedback(i) .EQV. .true. ) ) THEN
            IF ( ( model_config_rec%cu_physics(1) .EQ. GFSCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. G3SCHEME     ) .OR. &
                 ( model_config_rec%cu_physics(1) .EQ. GDSCHEME     ) ) THEN
               wrf_err_message = '--- WARNING: Turning on cu_rad_feedback also requires setting cu_diag== 1'
               CALL wrf_debug ( 1, wrf_err_message )
               model_config_rec%cu_diag(i) = 1
            ELSE
               model_config_rec%cu_diag(i) = 0
            END IF
         END IF
      END DO

!-----------------------------------------------------------------------
! The namelist flag cu_diag=1 must have one of the two Grell cumulus parameterizations
! turned on.  All other cumulus parameterizations need to have cu_diag=0
!-----------------------------------------------------------------------

       DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_diag(i) .EQ. G3TAVE ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. GDSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. GFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFCUPSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. G3SCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using cu_diag=1 requires use of one of the following CU schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Freitas (GF) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell 3D (G3) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Kain–Fritsch Cumulus Potential (KF-CuP) CU scheme'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Devenyi (GD) CU scheme'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO

!-----------------------------------------------------------------------
! The namelist flag kf_edrates=1 must have one of the three KF cumulus parameterizations
! turned on.  All other cumulus parameterizations need to have kf_edrates=0
!-----------------------------------------------------------------------

       DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%kf_edrates(i) .EQ. KFEDRATES ) THEN
          IF ( ( model_config_rec%cu_physics(i) .NE. KFETASCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. MSKFSCHEME ) .AND. &
               ( model_config_rec%cu_physics(i) .NE. KFSCHEME ) ) THEN
                wrf_err_message = '--- ERROR: Using kf_edrates=1 requires use of one of the following KF schemes:'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Kain-Fritsch (cu_physics=1)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Multi-scale Kain-Fritsch (cu_physics=11)'
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          old Kain-Fritsch (cu_physics=99)'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
         END IF
       END DO

!-----------------------------------------------------------------------
! Test to see if we allocate space for the time series.
!-----------------------------------------------------------------------

      IF ( wrf_dm_on_monitor() ) THEN
         CALL wrf_tsin_exist ( exists )
         IF ( exists ) THEN
            model_config_rec%process_time_series = 1
         ELSE
            model_config_rec%process_time_series = 0
         END IF
      END IF
#ifdef DM_PARALLEL
      CALL wrf_dm_bcast_integer(model_config_rec%process_time_series, 1)
#endif
!-----------------------------------------------------------------------
! The three Grell cumulus parameterization schemes need to have the
! namelist flag cu_diag=1, and all other cumulus schemes must have
! cu_diag=0.
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%cu_physics(i) .EQ. GDSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. GFSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. KFCUPSCHEME ) .OR. &
              ( model_config_rec%cu_physics(i) .EQ. G3SCHEME ) ) THEN
            model_config_rec%cu_diag(i) = 1
         ELSE
            model_config_rec%cu_diag(i) = 0
         END IF
      END DO

!-----------------------------------------------------------------------
!  Only implement the TEMF PBL scheme with the TEMP SFCLAY scheme.  
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              ( model_config_rec%sf_sfclay_physics(i) .NE. TEMFSFCSCHEME ) )  THEN
            wrf_err_message = '--- ERROR: Using bl_pbl_physics=10 requires sf_sfclay_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         ELSEIF ( ( model_config_rec%bl_pbl_physics(i) .NE. TEMFPBLSCHEME ) .AND. &
                  ( model_config_rec%sf_sfclay_physics(i) .EQ. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using sf_sfclay_physics=10 requires bl_pbl_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO      ! Loop over domains

!-----------------------------------------------------------------------
!  Need to set lagday to 150 if tmn_update is 1
!-----------------------------------------------------------------------

      IF ( model_config_rec%tmn_update .EQ. 1 .AND. &
           model_config_rec%lagday .EQ. 1 ) THEN
           wrf_err_message = '--- ERROR: Using tmn_update=1 requires lagday=150 '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
!  Do not allow digital filtering to be run with TEMF.
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              (model_config_rec%dfi_opt .NE. DFI_NODFI) )  THEN
            wrf_err_message = '--- ERROR: DFI not available for bl_pbl_physics=10 '
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO      ! Loop over domains

!-----------------------------------------------------------------------
!  If this is a restart, shut off the DFI.
!-----------------------------------------------------------------------

      IF ( model_config_rec%restart ) THEN
         model_config_rec%dfi_opt = DFI_NODFI
      END IF

!-----------------------------------------------------------------------
!  The CLM scheme may not even be compiled, so make sure it is not allowed
!  to be run if the code is not available.
!-----------------------------------------------------------------------

#if !defined ( WRF_USE_CLM )
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%sf_surface_physics(i) .EQ. CLMSCHEME ) THEN
            oops = oops + 1
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- ERROR: The CLM surface scheme was requested in the namelist.input file.'
         CALL wrf_debug ( 0, TRIM(wrf_err_message) )
         wrf_err_message = '--- ERROR: However, the WRF CLM scheme was not compiled in WRF.'
         CALL wrf_debug ( 0, TRIM(wrf_err_message) )
         wrf_err_message = '--- ERROR: Please place the -DWRF_USE_CLM option in configure.wrf file, and recompile.'
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF
#endif

!-----------------------------------------------------------------------
!  grav_settling = 1 must be turned off for mp_physics=28.
!-----------------------------------------------------------------------
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%grav_settling(i) .NE. FOGSETTLING0 ) THEN
                model_config_rec%grav_settling(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: mp_physics == 28, already has gravitational fog settling; resetting grav_settling to 0'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  scalar_pblmix = 1 should be turned on for mp_physics=28. But can be off for MYNN (when bl_mynn_mixscalars = 1)
!-----------------------------------------------------------------------
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%mp_physics(i) .EQ. THOMPSONAERO ) THEN
            IF ( model_config_rec%use_aero_icbc .AND. model_config_rec%scalar_pblmix(i) .NE. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 1
                oops = oops + 1
            END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: For mp_physics == 28 and use_aero_icbc is true, recommend to turn on scalar_pblmix'
         CALL wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'resetting scalar_pblmix = 1'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

      !NOW CHECK FOR MYNN
      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ((model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2) .OR. &
             (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
            IF ( model_config_rec%bl_mynn_mixscalars(i) .EQ. 1 ) THEN
                model_config_rec%scalar_pblmix(i) = 0
                oops = oops + 1
            END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- WARNING: MYNN is set to mix scalars, turning off scalar_pblmix'
         CALL wrf_message ( wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  DJW Check that we're not using ndown and vertical nesting.
!-----------------------------------------------------------------------
     DO i=1,model_config_rec%max_dom
       IF ((model_config_rec%vert_refine_method(i) .NE. 0) .AND. (model_config_rec%vert_refine_fact .NE. 1)) THEN
         wrf_err_message = '--- ERROR: vert_refine_fact is ndown specific and cannot be used with vert_refine_method, and vice versa.'
         CALL wrf_debug ( 1, wrf_err_message )
       ENDIF
     ENDDO

!-----------------------------------------------------------------------
!  DJW Check that only one type of vertical nesting is enabled.
!-----------------------------------------------------------------------
     DO i=1,model_config_rec%max_dom
       IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
         DO j=1,model_config_rec%max_dom
           IF ((model_config_rec%vert_refine_method(i) .NE. model_config_rec%vert_refine_method(j)) .AND. (model_config_rec%vert_refine_method(j) .NE. 0)) THEN
             write(wrf_err_message,'(A,I1,A,I2,A,I1,A,I2,A)') '--- ERROR: vert_refine_method differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Only one type of vertical grid nesting can be used at a time.'
              CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
              count_fatal_error = count_fatal_error + 1
           ENDIF
         ENDDO
       ENDIF
     ENDDO

!-----------------------------------------------------------------------
!  DJW Check that e_vert is the same for nested domains not using
!  vertical nesting. Don't do this check if we're using ndown.
!-----------------------------------------------------------------------
      IF ((model_config_rec%max_dom .GT. 1) .AND. (model_config_rec%vert_refine_fact .EQ. 1)) THEN
        DO i=1,model_config_rec%max_dom
          IF (((model_config_rec%parent_id(i) .NE. 0) .AND. (model_config_rec%parent_id(i) .NE. model_config_rec%grid_id(i))) .AND. (model_config_rec%vert_refine_method(i) .EQ. 0)) THEN
            DO j=1,model_config_rec%max_dom
              IF ((i .NE. j) .AND. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(j))) THEN
                IF (model_config_rec%e_vert(i) .NE. model_config_rec%e_vert(j)) THEN
                  write(wrf_err_message,'(A,I2,A,I2,A)') '--- ERROR: e_vert differs on grid ids ',model_config_rec%grid_id(i),' and ',model_config_rec%grid_id(j),'. Set vert_refine_method or make e_vert consistent.'
                  CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
                  count_fatal_error = count_fatal_error + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!  Check that vertical levels are defined in a logical way.
!  DJW Check that domains without a parent do not have vertical
!  nesting enabled.
!-----------------------------------------------------------------------
      DO i=1,model_config_rec%max_dom
        IF ((model_config_rec%parent_id(i) .EQ. 0) .OR. (model_config_rec%parent_id(i) .EQ. model_config_rec%grid_id(i))) THEN
          IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
            write(wrf_err_message,'(A,I1,A,I2,A)') '--- ERROR: vert_refine_method=',model_config_rec%vert_refine_method(i),' for grid_id=',model_config_rec%grid_id(i),', must be 0 for a non-nested domain.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!  DJW Check that we've got appropriate e_vert for integer refinement.
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
        IF (model_config_rec%vert_refine_method(i) .EQ. 1) THEN
          j = model_config_rec%parent_id(i)
          IF (MOD(model_config_rec%e_vert(i)-1, model_config_rec%e_vert(j)-1) .NE. 0) THEN
            write(wrf_err_message,'(A,I2,A,I2,A)') "--- ERROR: grid_id=",i," and parent (grid_id=",j,") have incompatible e_vert's for vertical nesting with integer refinement."
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          ENDIF
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!  Consistency checks between vertical refinement and radiation
!  scheme selection.  For "choose any vertical levels" for the nest,
!  only RRTM is eligible.
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
        IF (model_config_rec%vert_refine_method(i) .NE. 0) THEN
          IF ( ( ( model_config_rec%ra_lw_physics(i) .EQ. 0                   ) .OR. &
                 ( model_config_rec%ra_lw_physics(i) .EQ. RRTMSCHEME          ) ) .AND. &
               ( ( model_config_rec%ra_sw_physics(i) .EQ. 0                   ) .OR. &
                 ( model_config_rec%ra_sw_physics(i) .EQ. SWRADSCHEME         ) ) ) THEN
             !  We are OK, I just hate writing backwards / negative / convoluted if tests
             !  that are not easily comprehensible.
          ELSE
            wrf_err_message = '--- ERROR: vert_refine_method=2 only works with ra_lw_physics=1 (RRTM) and ra_sw_physics=1 (Dudhia)'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            count_fatal_error = count_fatal_error + 1
          END IF
        END IF
      END DO

!-----------------------------------------------------------------------
! This WRF version does not support trajectories on a global domain
!-----------------------------------------------------------------------
      IF (  model_config_rec % polar(1) .AND. &
            model_config_rec % fft_filter_lat .LT. 90. .AND. &
            model_config_rec % traj_opt .NE. 0 ) THEN
         CALL wrf_debug ( 0, '--- ERROR: Trajectories not supported on global domain' )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
! If the user did not specify a global setting in the lateral BC
! portion of the namelist file (polar), but the distance around the
! equator is approximately equal to the entire globe, then it is likely
! that the user probably forgot to flip that polar switch on.
!-----------------------------------------------------------------------
      lon_extent_is_global = .FALSE.
      IF ( ABS ( model_config_rec % e_we(1) * model_config_rec % dx(1) - 2. * piconst / reradius ) .LT. model_config_rec % dx(1) ) THEN
         lon_extent_is_global = .TRUE.
      END IF

      lat_extent_is_global = .FALSE.
      IF ( ABS ( model_config_rec % e_sn(1) * model_config_rec % dy(1) -      piconst / reradius ) .LT. model_config_rec % dy(1) ) THEN
         lat_extent_is_global = .TRUE.
      END IF

      IF ( ( .NOT. model_config_rec % polar(1) ) .AND. &
           ( lon_extent_is_global .AND. lat_extent_is_global ) ) THEN
         CALL wrf_debug ( 0, '--- ERROR: Domain size is global, set &bdy_control polar=.TRUE.' )
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
!  Remapping namelist variables for gridded and surface fdda to aux streams 9 and 10.
!  Relocated here so that the remappings are after checking the namelist for inconsistencies.
!-----------------------------------------------------------------------

# include "../dyn_em/namelist_remappings_em.h"

#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
!  For the real program (ARW only), check that the vertical interpolation options
!  selected by the user are consistent.
!  1. If the user has turned-off using the surface level, do not allow the force
!     option to select how many layers the surface is to be used through.
!  2. If the user has turned-off using the surface level, do not allow the
!     lowest level from surface option to be activated.
!-----------------------------------------------------------------------

      IF ( model_config_rec % use_wps_input .EQ. 1 ) THEN
         IF ( ( .NOT. model_config_rec % use_surface )  .AND. &
              ( model_config_rec % force_sfc_in_vinterp .GT. 0 ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting force_sfc_in_vinterp=0.'
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec % force_sfc_in_vinterp = 0
         END IF
         IF ( ( .NOT. model_config_rec % use_surface )  .AND. &
              ( model_config_rec % lowest_lev_from_sfc ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_debug ( 1, wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting lowest_lev_from_sfc=F.'
            CALL wrf_debug ( 1, wrf_err_message )
            model_config_rec % lowest_lev_from_sfc = .FALSE.
         END IF
      END IF
#endif

#if (EM_CORE == 1 && WRFPLUS == 1 )
      IF ( ( model_config_rec%jcdfi_use ).AND. &
           ( model_config_rec%jcdfi_diag .NE. 1 ) ) THEN
         wrf_err_message = '--- ERROR: If jcdfi_use = 1, then jcdfi_diag must also = 1 '
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- Change jcdfi_diag in namelist.input '
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         count_fatal_error = count_fatal_error + 1
      END IF
      ! derived namelist for packaged a_/g_ variables
      model_config_rec%mp_physics_plus = 0
      DO i = 1, model_config_rec % max_dom
         model_config_rec%mp_physics_plus(i) = model_config_rec%mp_physics(i)
      ENDDO
      model_config_rec%cu_used_plus = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .NE. NOCUSCHEME ) THEN
            model_config_rec%cu_used_plus = 1
         END IF
      ENDDO
      model_config_rec%shcu_used_plus = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .NE. NOSHCUSCHEME ) THEN
            model_config_rec%shcu_used_plus = 1
         END IF
      ENDDO
#endif

!-----------------------------------------------------------------------
!  If the RRTMG FAST schemes are requested, check that the code with
!  built to use them.
!-----------------------------------------------------------------------

#if( BUILD_RRTMG_FAST != 1)
      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )  ) THEN
         wrf_err_message = '--- ERROR: RRTMG FAST schemes must be built with a default compile-time flag'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '--- ERROR: Run ./clean -a, ./configure, ./compile scripts again'
         CALL wrf_message ( wrf_err_message )
         count_fatal_error = count_fatal_error + 1
      END IF
#endif

!-----------------------------------------------------------------------
!  Set the namelist parameter o3input to 0 for the radiation schemes other
!  than RRTMG_LWSCHEME and RRTMG_SWSCHEME.
!-----------------------------------------------------------------------

      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_LWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_SWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )  ) THEN
         wrf_err_message = '--- NOTE: RRTMG radiation is used, namelist ' // &
                           'value for o3input (ozone input) is used '
         CALL wrf_debug ( 1, wrf_err_message )
      ELSE
         model_config_rec % o3input = 0
         wrf_err_message = '--- NOTE: RRTMG radiation is not used, setting:  ' // &
                           'o3input=0 to avoid data pre-processing'
         CALL wrf_debug ( 1, wrf_err_message )
      END IF

#if (WRF_CHEM == 1 && WRF_KPP == 1 )
!-----------------------------------------------------------------------
! Check for consistent chem_opt and irr_opt
!-----------------------------------------------------------------------
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%irr_opt(i) > 0 .and. &
              (model_config_rec%chem_opt(i) /= mozcart_kpp .and. &
               model_config_rec%chem_opt(i) /= t1_mozcart_kpp .and. &
               model_config_rec%chem_opt(i) /= mozart_mosaic_4bin_kpp .and. &
               model_config_rec%chem_opt(i) /= mozart_mosaic_4bin_aq_kpp ) ) THEN
           wrf_err_message = '--- ERROR: IRR diagnostics can only be used with the following chem_opt settings:'
           CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
           wrf_err_message = '    MOZCART_KPP, T1_MOZCART_KPP, MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP'
           CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
           write(wrf_err_message,'(''    chem_opt = '',i3,'', '',i3,'', '',i3,'', or '',i3)') &
                 MOZCART_KPP, T1_MOZCART_KPP, MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP 
           CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
           count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO
#endif

#if ( ( EM_CORE == 1) && ( defined(DM_PARALLEL) )&& ( ! defined(STUBMPI) ) )
!-----------------------------------------------------------------------
! Did the user ask for too many MPI tasks, or are those tasks poorly distributed.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % e_we(i) /  model_config_rec % nproc_x .LT. 10 ) .OR. &
              ( model_config_rec % e_sn(i) /  model_config_rec % nproc_y .LT. 10 ) ) THEN
            WRITE ( wrf_err_message , * ) 'For domain ',i,', the domain size is too small for this many processors, ', & 
                                          'or the decomposition aspect ratio is poor.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , * ) 'Minimum decomposed computational patch size, either x-dir or y-dir, is 10 grid cells.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , fmt='(a,i5,a,i4,a,i4)' ) &
                                          'e_we = ', model_config_rec % e_we(i),', nproc_x = ',model_config_rec % nproc_x, &
                                          ', with cell width in x-direction = ', &
                                          model_config_rec % e_we(i) /  model_config_rec % nproc_x
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            WRITE ( wrf_err_message , fmt='(a,i5,a,i4,a,i4)' ) &
                                          'e_sn = ', model_config_rec % e_sn(i),', nproc_y = ',model_config_rec % nproc_y, &
                                          ', with cell width in y-direction = ', &
                                          model_config_rec % e_sn(i) /  model_config_rec % nproc_y
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            wrf_err_message = '--- ERROR: Reduce the MPI rank count, or redistribute the tasks.'
            CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
            oops = oops + 1
         END IF
      ENDDO
      IF ( oops .GT. 0 ) THEN
         count_fatal_error = count_fatal_error + 1
      END IF
#endif




!---------------------------------------------------------------------
!  The "clean" atmosphere radiative flux diagnostics can only be used 
!     with WRF-Chem.
!---------------------------------------------------------------------

      IF ( model_config_rec%clean_atm_diag > 0 ) THEN

#if (WRF_CHEM != 1)
         wrf_err_message = '--- NOTE: "Clean" atmosphere diagnostics can only be used in WRF-Chem' 
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         model_config_rec%calc_clean_atm_diag = 0
#else
         model_config_rec%calc_clean_atm_diag = 1
#endif

      ENDIF

!-----------------------------------------------------------------------
!  MUST BE AFTER ALL OF THE PHYSICS CHECKS.
!-----------------------------------------------------------------------

      IF ( count_fatal_error .GT. 0 ) THEN
         WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  ', count_fatal_error, &
                                            ' namelist settings are wrong. Please check and reset these options'
         CALL wrf_error_fatal (  wrf_err_message  )
      END IF

   END SUBROUTINE check_nml_consistency

!=======================================================================

   SUBROUTINE setup_physics_suite

!<DESCRIPTION>
!
! Based on the selection of physics suite provided in the namelist, sets the
! values of other namelist options (mp_physics, cu_physics, ra_lw_physics,
! ra_sw_physics, bl_pbl_physics, sf_sfclay_physics, and sf_surface_physics)
! to reflect that suite.
!
!</DESCRIPTION>

      USE module_domain, ONLY : change_to_lower_case

      IMPLICIT NONE
#if ( EM_CORE == 1 )

      INTEGER :: i
      INTEGER :: max_dom
      LOGICAL :: have_mods
      INTEGER, DIMENSION( max_domains ) :: orig_mp_physics, orig_cu_physics, orig_ra_lw_physics, orig_ra_sw_physics, &
                                           orig_bl_pbl_physics, orig_sf_sfclay_physics, orig_sf_surface_physics
      CHARACTER, DIMENSION( max_domains ) :: modified_mp_option, modified_cu_option, modified_ra_lw_option, modified_ra_sw_option, &
                                             modified_bl_pbl_option, modified_sf_sfclay_option, modified_sf_surface_option
      CHARACTER (LEN=256) :: physics_suite_lowercase
      CHARACTER (LEN=32) :: formatstring

      !
      ! Initialize the debug level so that it can be used in the namelist testing.
      ! wrf_debug_level is a global value in module_wrf_error.
      !

      wrf_debug_level = model_config_rec%debug_level

      max_dom = model_config_rec % max_dom

      !
      ! Save physics selections as given by the user to later determine if the
      ! user has overridden any options
      !
      modified_mp_option(1:max_dom) = ' '
      orig_mp_physics(1:max_dom) = model_config_rec % mp_physics(1:max_dom)

      modified_cu_option(1:max_dom) = ' '
      orig_cu_physics(1:max_dom) = model_config_rec % cu_physics(1:max_dom)

      modified_ra_lw_option(1:max_dom) = ' '
      orig_ra_lw_physics(1:max_dom) = model_config_rec % ra_lw_physics(1:max_dom)

      modified_ra_sw_option(1:max_dom) = ' '
      orig_ra_sw_physics(1:max_dom) = model_config_rec % ra_sw_physics(1:max_dom)

      modified_bl_pbl_option(1:max_dom) = ' '
      orig_bl_pbl_physics(1:max_dom) = model_config_rec % bl_pbl_physics(1:max_dom)

      modified_sf_sfclay_option(1:max_dom) = ' '
      orig_sf_sfclay_physics(1:max_dom) = model_config_rec % sf_sfclay_physics(1:max_dom)

      modified_sf_surface_option(1:max_dom) = ' '
      orig_sf_surface_physics(1:max_dom) = model_config_rec % sf_surface_physics(1:max_dom)

      CALL change_to_lower_case(trim(model_config_rec % physics_suite), physics_suite_lowercase)

      !
      ! If physics suite is 'none', we can return early
      !
      IF ( trim(physics_suite_lowercase) == 'none' ) THEN
         wrf_err_message = '*************************************'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'No physics suite selected.'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = 'Physics options will be used directly from the namelist.'
         call wrf_debug ( 1, wrf_err_message )
         wrf_err_message = '*************************************'
         call wrf_debug ( 1, wrf_err_message )
         RETURN
      END IF

      CALL wrf_message ('*************************************')
      CALL wrf_message ('Configuring physics suite '''//trim(physics_suite_lowercase)//'''')
      CALL wrf_message ('')

      !
      ! Set options based on the suite selection
      !
      SELECT CASE ( trim(physics_suite_lowercase) )

      !
      ! CONUS suite
      !
      CASE ('conus')
         DO i = 1, max_dom

            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = TIEDTKESCHEME               ! Tiedtke
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = THOMPSON                    ! Thompson
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME        ! RRTMG LW
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME        ! RRTMG SW
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = MYJPBLSCHEME        ! MYJ
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = MYJSFCSCHEME  ! MYJ
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME   ! Noah

         END DO

      !
      ! Tropical suite
      !
      CASE ('tropical')
         DO i = 1, max_dom

            IF ( model_config_rec % cu_physics(i) == -1 ) model_config_rec % cu_physics(i) = NTIEDTKESCHEME              ! New Tiedtke
            IF ( model_config_rec % mp_physics(i) == -1 ) model_config_rec % mp_physics(i) = WSM6SCHEME                  ! WSM6
            IF ( model_config_rec % ra_lw_physics(i) == -1 ) model_config_rec % ra_lw_physics(i) = RRTMG_LWSCHEME        ! RRTMG LW
            IF ( model_config_rec % ra_sw_physics(i) == -1 ) model_config_rec % ra_sw_physics(i) = RRTMG_SWSCHEME        ! RRTMG SW
            IF ( model_config_rec % bl_pbl_physics(i) == -1 ) model_config_rec % bl_pbl_physics(i) = YSUSCHEME           ! YSU
            IF ( model_config_rec % sf_sfclay_physics(i) == -1 ) model_config_rec % sf_sfclay_physics(i) = SFCLAYSCHEME  ! MM5
            IF ( model_config_rec % sf_surface_physics(i) == -1 ) model_config_rec % sf_surface_physics(i) = LSMSCHEME   ! Noah

         END DO

      CASE DEFAULT
         CALL wrf_error_fatal ( 'Unrecognized physics suite' )

      END SELECT

      WRITE (formatstring, '(A,I3,A)') '(A21,', max_dom, '(I6,A1))'

      !
      ! Print microphysics options
      !
      WHERE (model_config_rec % mp_physics(1:max_dom) == orig_mp_physics(1:max_dom)) modified_mp_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'mp_physics: ', &
                                                    (model_config_rec % mp_physics(i), modified_mp_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print cumulus options
      !
      WHERE (model_config_rec % cu_physics(1:max_dom) == orig_cu_physics(1:max_dom)) modified_cu_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'cu_physics: ', &
                                                    (model_config_rec % cu_physics(i), modified_cu_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print LW radiation options
      !
      WHERE (model_config_rec % ra_lw_physics(1:max_dom) == orig_ra_lw_physics(1:max_dom)) modified_ra_lw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_lw_physics: ', &
                                                    (model_config_rec % ra_lw_physics(i), modified_ra_lw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print SW radiation options
      !
      WHERE (model_config_rec % ra_sw_physics(1:max_dom) == orig_ra_sw_physics(1:max_dom)) modified_ra_sw_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'ra_sw_physics: ', &
                                                    (model_config_rec % ra_sw_physics(i), modified_ra_sw_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print boundary layer options
      !
      WHERE (model_config_rec % bl_pbl_physics(1:max_dom) == orig_bl_pbl_physics(1:max_dom)) modified_bl_pbl_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) 'bl_pbl_physics: ', &
                                                    (model_config_rec % bl_pbl_physics(i), modified_bl_pbl_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print surface layer options
      !
      WHERE (model_config_rec % sf_sfclay_physics(1:max_dom) == orig_sf_sfclay_physics(1:max_dom)) &
            modified_sf_sfclay_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_sfclay_physics: ', (model_config_rec % sf_sfclay_physics(i), modified_sf_sfclay_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print surface options
      !
      WHERE (model_config_rec % sf_surface_physics(1:max_dom) == orig_sf_surface_physics(1:max_dom)) &
            modified_sf_surface_option(1:max_dom) = '*'
      WRITE (wrf_err_message, FMT=TRIM(formatstring)) &
            'sf_surface_physics: ', (model_config_rec % sf_surface_physics(i), modified_sf_surface_option(i), i=1,max_dom)
      CALL wrf_message (wrf_err_message)

      !
      ! Print footnote if any physics schemes were overridden by the user
      !
      have_mods = ANY (modified_mp_option(1:max_dom) == '*') &
             .OR. ANY (modified_cu_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_lw_option(1:max_dom) == '*') &
             .OR. ANY (modified_ra_sw_option(1:max_dom) == '*') &
             .OR. ANY (modified_bl_pbl_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_sfclay_option(1:max_dom) == '*') &
             .OR. ANY (modified_sf_surface_option(1:max_dom) == '*')

      IF (have_mods) THEN
         CALL wrf_message ('')
         CALL wrf_message ('(* = option overrides suite setting)')
      END IF

      CALL wrf_message ('*************************************')

#endif

   END SUBROUTINE setup_physics_suite

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

      INTEGER :: numsoiltemp , nummosaictemp
      INTEGER :: i


!-----------------------------------------------------------------------
! Set the namelist urban dimensions if sf_urban_physics > 0  
!-----------------------------------------------------------------------

      IF ( any(model_config_rec%sf_urban_physics > 0 ) ) THEN
      
         model_config_rec%urban_map_zrd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nwr * &
                                          model_config_rec%num_urban_nz
         model_config_rec%urban_map_zwd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nwr * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_gd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_ng
         model_config_rec%urban_map_zd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_zdf = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz 
         model_config_rec%urban_map_bd  = model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_wd  = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_nz  * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_gbd = model_config_rec%num_urban_ndm * &
                                          model_config_rec%num_urban_ngb * &
                                          model_config_rec%num_urban_nbui
         model_config_rec%urban_map_fbd = model_config_rec%num_urban_ndm       * &
                                          (model_config_rec%num_urban_nz - 1)  * &
                                          model_config_rec%num_urban_nf        * &
                                          model_config_rec%num_urban_nbui

      END IF     
      
!-----------------------------------------------------------------------
! Set the namelist mosaic_cat_soil parameter for the Noah-mosaic  scheme if sf_surface_mosaic == 1.  
!-----------------------------------------------------------------------

      IF ( model_config_rec % sf_surface_mosaic .EQ. 1 ) THEN
      
      numsoiltemp = model_config_rec % num_soil_layers
      nummosaictemp = model_config_rec % mosaic_cat
      
         model_config_rec % mosaic_cat_soil = numsoiltemp * nummosaictemp

         wrf_err_message = '--- NOTE: Noah-mosaic is in use, setting:  ' // &
                           'mosaic_cat_soil = mosaic_cat * num_soil_layers'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF     
      
#if ( (NMM_CORE != 1) && (DA_CORE != 1) )
!-----------------------------------------------------------------------
! How big to allocate random seed arrays.
!-----------------------------------------------------------------------

      CALL RANDOM_SEED ( SIZE = model_config_rec % seed_dim )

!-----------------------------------------------------------------------
! If this is a WRF run with polar boundary conditions, then this is a
! global domain. A global domain needs to have the FFT arrays allocated.
!-----------------------------------------------------------------------

      model_config_rec % fft_used = 0
      IF ( ( model_config_rec % polar(1) ) .AND. &
           ( model_config_rec % fft_filter_lat .LT. 90. ) ) THEN
         model_config_rec % fft_used = 1
      END IF

!-----------------------------------------------------------------------
! Need to know if this run has aercu_opt set to either 1 or 2,
! so that we can set a derived namelist for packaging arrays.
!-----------------------------------------------------------------------

      model_config_rec % aercu_used = 0
      IF ( model_config_rec %aercu_opt .GT. 0 ) THEN
         model_config_rec % aercu_used = 1
      END IF

!-----------------------------------------------------------------------
! If any CAM scheme is turned on, then there are a few shared variables.
! These need to be allocated when any CAM scheme is active.
!-----------------------------------------------------------------------

#if ( (EM_CORE == 1) && (WRF_CHEM != 1) )
      model_config_rec % cam_used = 0
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % mp_physics(i)     .EQ. CAMMGMPSCHEME   ) .OR. &
              ( model_config_rec % bl_pbl_physics(i) .EQ. CAMUWPBLSCHEME  ) .OR. &
              ( model_config_rec % shcu_physics(i)   .EQ. CAMUWSHCUSCHEME ) ) THEN
            model_config_rec % cam_used = 1
         END IF
      ENDDO

#elif (WRF_CHEM == 1)
      model_config_rec % cam_used = 1
#endif

#endif

      
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

         wrf_err_message = '--- NOTE: CAM radiation is in use, setting:  ' // &
                           'paerlev=29, levsiz=59, cam_abs_dim1=4, cam_abs_dim2=e_vert'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF

!-----------------------------------------------------------------------
! If a user requested to compute the radar reflectivity .OR. if this is
! one of the schemes that ALWAYS computes the radar reflectivity, then
! turn on the switch that says allocate the space for the refl_10cm array.
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % mp_physics(i) .EQ. MILBRANDT2MOM ) .OR. &
#if (EM_CORE == 1)
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOM     ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMG    ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_2MOMCCN  ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOM     ) .OR. &
              ( model_config_rec % mp_physics(i) .EQ. NSSL_1MOMLFO  ) .OR. &
#endif
              ( model_config_rec % do_radar_ref  .EQ. 1             ) ) THEN
            model_config_rec % compute_radar_ref = 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! If MYNN PBL is not used, set bl_mynn_edmf = 0 so that we don't get 
! additional output 
!-----------------------------------------------------------------------

#if (EM_CORE == 1)
      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME2 ) .AND. &
              ( model_config_rec % bl_pbl_physics(i) .NE. MYNNPBLSCHEME3 ) ) THEN
            model_config_rec % bl_mynn_edmf = 0
         END IF
      ENDDO
#endif

!-----------------------------------------------------------------------
! Set the namelist parameters for the RRTMG radiation scheme if either
! ra_lw_physics or ra_sw_physics is set to one of the RRTMG schemes.
!-----------------------------------------------------------------------

      IF (( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME      ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME      ) .OR. &
          ( model_config_rec % ra_lw_physics(1) .EQ. RRTMK_LWSCHEME      ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMK_SWSCHEME      ) .OR. &
          ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )) THEN
         model_config_rec % levsiz = 59
         model_config_rec % alevsiz = 12
         model_config_rec % no_src_types = 6

         wrf_err_message = '--- NOTE: One of the RRTMG radiation schemes is in use, setting:  ' // &
                           'levsiz=59, alevsiz=12, no_src_types=6'
         CALL wrf_debug ( 1, wrf_err_message )

      END IF

!-----------------------------------------------------------------------
! Set namelist parameter num_soil_levels depending on the value of
! sf_surface_physics
!-----------------------------------------------------------------------

#if ((EM_CORE == 1) || (NMM_CORE == 1))
      IF      (   model_config_rec % sf_surface_physics(1) .EQ. NOLSMSCHEME  ) THEN
         model_config_rec % num_soil_layers = 5
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. SLABSCHEME   ) THEN
         model_config_rec % num_soil_layers = 5
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. LSMSCHEME    ) THEN
         model_config_rec % num_soil_layers = 4
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. NOAHMPSCHEME ) THEN
         model_config_rec % num_soil_layers = 4
      ELSE IF ( ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) .AND. &
                ( model_config_rec % num_soil_layers .EQ. 6 ) ) THEN
         model_config_rec % num_soil_layers = 6
      ELSE IF ( ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) .AND. &
                ( model_config_rec % num_soil_layers .EQ. 9 ) ) THEN
         model_config_rec % num_soil_layers = 9
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME ) THEN
         model_config_rec % num_soil_layers = 6
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME  ) THEN
         model_config_rec % num_soil_layers = 2
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. CLMSCHEME    ) THEN
         model_config_rec % num_soil_layers = 10
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. SSIBSCHEME   ) THEN
         model_config_rec % num_soil_layers = 3
#if (NMM_CORE == 1)
      ELSE IF (   model_config_rec % sf_surface_physics(1) .EQ. GFDLSLAB     ) THEN
         model_config_rec % num_soil_layers = 4
#endif
      ELSE
         CALL wrf_debug       ( 0 , '--- ERROR: Unknown sf_surface_physics has no associated number of soil levels' )
         WRITE (wrf_err_message, FMT='(A,I6)') '--- ERROR: sf_surface_physics = ' , model_config_rec % sf_surface_physics(1)
         CALL wrf_error_fatal ( TRIM(wrf_err_message) )
      END IF 
#endif

      WRITE (wrf_err_message, FMT='(A,I6)') '--- NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_debug ( 1, wrf_err_message )

   END SUBROUTINE set_physics_rconfigs


!=======================================================================

   END MODULE module_check_a_mundo

!=======================================================================

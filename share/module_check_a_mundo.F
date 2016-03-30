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

      LOGICAL :: exists, vnest
      LOGICAL , EXTERNAL :: wrf_dm_on_monitor
      INTEGER :: i, j, oops, d1_value
      LOGICAL :: km_opt_already_done , diff_opt_already_done
!TWG2015
!
!FASDAS
!
      LOGICAL :: rinblw_already_done
!
!END FASDAS
!
      LOGICAL :: fatal_error
      INTEGER :: count_fatal_error

!-----------------------------------------------------------------------
! Set up the WRF Hydro namelist option to allow dynamic allocation of
! variables.
!-----------------------------------------------------------------------
   fatal_error = .false.
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
! There are restrictions for the use_theta_m option:
! 1. The option to include moist theta in the WRF solver only works with a single domain.
! 2. The option may not be used in conjunction with damp_opt=2.
! 3. The option may not be used with rad_nudge.
!-----------------------------------------------------------------------

      IF ( ( model_config_rec % use_theta_m .EQ. 1 ) .AND. &
           ( model_config_rec % damp_opt    .EQ. 2 ) ) THEN
         CALL wrf_message ( "The use_theta_m option may not be paired with damp_opt=2." )
         wrf_err_message = '--- ERROR: Either turn off use_theta_m, or select a different damp_opt option'
       !  CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF

      IF ( ( model_config_rec % use_theta_m .EQ. 1 ) .AND. &
           ( model_config_rec % rad_nudge   .EQ. 1 ) ) THEN
         CALL wrf_message ( "The use_theta_m option may not be paired with rad_nudge=1." )
         wrf_err_message = '--- ERROR: Either turn off use_theta_m, or turn off the rad_nudge option'
      !   CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF


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
               CALL wrf_message ( "Setting blank km_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The km_opt entry in the namelist.input is now max_domains." )
            END IF
            km_opt_already_done = .TRUE.
         END IF
         IF ( model_config_rec % diff_opt(i) .EQ. -1 ) THEN
            model_config_rec % diff_opt(i) = model_config_rec % diff_opt(1)
            IF ( .NOT. diff_opt_already_done ) THEN
               CALL wrf_message ( "Setting blank diff_opt entries to domain #1 values.")
               CALL wrf_message ( " --> The diff_opt entry in the namelist.input is now max_domains." )
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
       !  CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF


#endif

!-----------------------------------------------------------------------
! Check that NSSL microphysics is not allowed for WRF-NMM run 
!-----------------------------------------------------------------------
#if (NMM_CORE == 1) || (HWRF == 1) 
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % mp_physics(i) == nssl_2mom .OR. &
              model_config_rec % mp_physics(i) == nssl_2momccn .OR. & 
              model_config_rec % mp_physics(i) == nssl_1mom .OR. & 
              model_config_rec % mp_physics(i) == nssl_1momlfo .OR. & 
              model_config_rec % mp_physics(i) == nssl_2momg ) THEN 
            wrf_err_message = '--- ERROR: Chosen microphysics scheme cannot run with WRF-NMM '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix mp_physics in namelist.input '
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE:     ----> Setting all mp_physics entries to value defined in the inner most domain'
            CALL wrf_message ( TRIM( wrf_err_message ) )
         END IF
      ENDDO
      d1_value = model_config_rec%mp_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%mp_physics(i) = d1_value
      END DO


!-----------------------------------------------------------------------
! Check that all values of ra_physics are the same for all domains
!-----------------------------------------------------------------------

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_lw_physics(i)     .NE. &
              model_config_rec % ra_lw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_lw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_lw_physics in namelist.input '
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec % ra_sw_physics(i)     .NE. &
              model_config_rec % ra_sw_physics(1) ) THEN
            wrf_err_message = '--- ERROR: ra_sw_physics must be equal for all domains '
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- Fix ra_sw_physics in namelist.input '
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO


!------------------------------------------------------------------------------
! Check that a value for time_step is given, and is not just set to default (-1)
!------------------------------------------------------------------------------

         IF ( ( model_config_rec % use_wps_input == 0 ) .AND. &
              ( model_config_rec % time_step .EQ. -1 ) ) THEN

            wrf_err_message = '--- ERROR: Known problem.  time_step must be set to a positive integer'
         !       CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

!-----------------------------------------------------------------------
! If fractional_seaice = 0, and tice2tsk_if2cold = .true, nothing will happen
!-----------------------------------------------------------------------

      IF ( ( model_config_rec%fractional_seaice .EQ. 0 ).AND. &
              ( model_config_rec%tice2tsk_if2cold ) ) THEN
            wrf_err_message = '--- WARNING: You set tice2tsk_if2cold = .true.,  but fractional_seaice = 0'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- WARNING: tice2tsk_if2cold will have no effect on results.'
            CALL wrf_message ( wrf_err_message )
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO

#if (EM_CORE == 1)

!-----------------------------------------------------------------------
! Check that if num_metgrid_levels < 20, lagrange_order should be 1
!-----------------------------------------------------------------------
            IF  ( model_config_rec%num_metgrid_levels .LE. 20 ) THEN
            CALL wrf_message ( 'Linear vertical interpolation is recommended with input vertical resolution this coarse, changing lagrange_order to 1' )
            model_config_rec%lagrange_order = 1
            END IF

!-----------------------------------------------------------------------
! Check for domain consistency for urban options.
!-----------------------------------------------------------------------

      d1_value = model_config_rec%sf_urban_physics(1)
      DO i = 2, model_config_rec % max_dom
         IF ( model_config_rec%sf_urban_physics(i) /= d1_value ) THEN
            WRITE(wrf_err_message, * ) '--- NOTE:   sf_urban_physics option must be identical in each domain'
            CALL wrf_message ( TRIM ( wrf_err_message ) )
            WRITE(wrf_err_message, * ) '--- NOTE:   ----> Resetting namelist values to that defined on the inner most domain'
            CALL wrf_message ( TRIM ( wrf_err_message ) )
         ENDIF
      END DO
      d1_value = model_config_rec%sf_urban_physics(model_config_rec % max_dom)
      DO i = 1, model_config_rec % max_dom-1
         model_config_rec%sf_urban_physics(i) = d1_value
      END DO

!-----------------------------------------------------------------------
! Check for consistency in the Noah-MP options
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%sf_surface_physics(i) == NOAHMPSCHEME ) THEN

            ! Noah-MP does not work with the urban schemes

            IF ( model_config_rec%sf_urban_physics(i) /= 0 ) THEN
               WRITE(wrf_err_message, '(" --- ERROR:   Noah-MP LSM scheme (sf_surface_physics==", I2, ")")') NOAHMPSCHEME
               CALL wrf_message ( TRIM ( wrf_err_message ) )
               WRITE(wrf_err_message, '("              does not work with urban physics schemes")')
         !      CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
            ENDIF

         END IF
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
         !      call wrf_error_fatal ( TRIM ( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      END DO
#endif

#if (EM_CORE == 1)

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
         !      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
               wrf_err_message = ' ABORT. If you really want to modify "kminforct" etc.,  edit module_check a_mundo.'
         !      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
           endif
         endif
   ENDDO
   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_vertstruc_opt(i) ==1 )  then
           model_config_rec % skebs_vertstruc=1       ! parameter stoch_vertstruc_opt is being replaced with skebs_vertstruc
                                                      ! stoch_vertstruc_opt is obsolete starting with V3.7
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_vertstruc_opt" is obsolete.'
           CALL wrf_message ( wrf_err_message )
           wrf_err_message = '             Please replace with namelist parameter "skebs_vertstruc" in V3.7 and later versions.'
           CALL wrf_message ( wrf_err_message )
         endif
   ENDDO

   DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec % stoch_force_opt(i) ==1 )  then
           model_config_rec % skebs(i)=1    ! parameter stoch_forc_opt is being replaced with skebs;
                                            ! stoch_vertstruc_opt is obsolete starting with V3.7
           wrf_err_message = '--- WARNING: the namelist parameter "stoch_force_opt" is obsolete.'
           CALL wrf_message ( wrf_err_message )
           wrf_err_message = '             Please replace with namelist parameter "skebs" in V3.7 and later versions.'
           CALL wrf_message ( wrf_err_message )
         endif
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
      CALL wrf_message ( wrf_err_message )
      wrf_err_message = '             increase computation time.'
      CALL wrf_message ( wrf_err_message )
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
    !  CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
#endif

      model_config_rec % rand_perturb_on=1
      wrf_err_message = '--- WARNING: perturb_chem_bdy=1 option uses RAND pattern and may'
      CALL wrf_message ( wrf_err_message )
      wrf_err_message = '             increase computation time.'
      CALL wrf_message ( wrf_err_message )

#if (WRF_CHEM == 1)
      IF ( .NOT. model_config_rec % have_bcs_chem(1) ) THEN
            wrf_err_message = '--- ERROR: This perturb_chem_bdy option needs '// &
                              'have_bcs_chem = .true. in chem.'
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         WRITE (wrf_err_message, FMT='(A,A)') '--- WARNING: traj_opt is zero, but ', &
                'num_traj is not zero; setting num_traj to zero.'
         CALL wrf_message ( wrf_err_message )
         model_config_rec%num_traj = 0 
   END IF

#endif

#if (EM_CORE == 1)
!-----------------------------------------------------------------------
! In program real, if hypsometric_opt = 2, adjust_heights cannot be set to .true. 
!-----------------------------------------------------------------------
      IF ( model_config_rec%hypsometric_opt .EQ. 2 &
           .AND. model_config_rec%adjust_heights ) THEN
           WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: hypsometric_opt is 2, ', &
                  'setting adjust_heights = F'
            CALL wrf_message ( wrf_err_message )
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
        ! CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF

      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%cu_physics(i) .EQ. MSKFSCHEME ) THEN
            WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: cu_physics is 11, ', &
                  'setting icloud = 1 and cu_rad_feedback = T'
            CALL wrf_message ( wrf_err_message )
            model_config_rec%cu_rad_feedback(i) = .true.
            model_config_rec%icloud = 1
         END IF
      ENDDO

#endif

!-----------------------------------------------------------------------
! If sst_update = 0, set io_form_auxinput4 to 0 so WRF will not try to
! input the data; auxinput_interval must also be 0
!-----------------------------------------------------------------------

      IF ( model_config_rec%sst_update .EQ. 0 ) THEN
         model_config_rec%io_form_auxinput4 = 0
         DO i = 1, model_config_rec % max_dom
            WRITE (wrf_err_message, FMT='(A,A)') '--- NOTE: sst_update is 0, ', &
                  'setting io_form_auxinput4 = 0 and auxinput4_interval = 0 for all domains'
            CALL wrf_message ( wrf_err_message )
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
            CALL wrf_message ( wrf_err_message )

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
            CALL wrf_message ( wrf_err_message )

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
            CALL wrf_message ( wrf_err_message )

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
               CALL wrf_message ( "Setting blank rinblw entries to domain #1 values.")
               CALL wrf_message ( " --> The rinblw entry in the namelist.input is now max_domains." )
            END IF
            rinblw_already_done = .TRUE.
         END IF
       ENDDO

!------------------------------------------------------------------------
! Check that rinblw is not -1 if sfdda is active
!------------------------------------------------------------------------
       IF ( model_config_rec%rinblw(1) .EQ. -1 ) THEN
            wrf_err_message = '--- ERROR: rinblw needs to be set in the namelist.input file.'
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
       END IF
    END IF 
    END DO

!------------------------------------------------------------------------
! Check to see if FASDAS is active
!------------------------------------------------------------------------
    DO i = 1, model_config_rec%max_dom
     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
         CALL wrf_message ( "FASDAS is active. Mixed Layer fdda is inactive")
     END IF

!------------------------------------------------------------------------
! Check to make sure sfdda is active if FASDAS is in namelist
!------------------------------------------------------------------------
!     IF (model_config_rec%fasdas(i) .EQ. 1 ) THEN
!       IF (model_config_rec%grid_sfdda(i) .EQ. 0) THEN
!        wrf_err_message = '--- ERROR: sfdda needs to be set in the namelist.input file to run FASDAS.'
!        CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
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
         CALL wrf_message ( wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  shcu_physics = 3 (grimsshcuscheme) only works with YSU & MYNN PBL.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%shcu_physics(i) .EQ. GRIMSSHCUSCHEME ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME) .OR. &
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
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6 implies shcu_physics cannot be 3, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  bl_mynn_edmf > 0 over-rules both shcu_physics & ishallow
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF ( model_config_rec%bl_mynn_edmf(i) .EQ. MYNN_STEM_EDMF .OR. &
              model_config_rec%bl_mynn_edmf(i) .EQ. MYNN_TEMF_EDMF) THEN
               model_config_rec%shcu_physics(i) = 0  ! maxdom
               model_config_rec%ishallow = 0         ! not maxdom
               oops = oops + 1
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = 'bl_mynn_edmf > 0 requires both shcu_physics=0 & ishallow=0'
         CALL wrf_message ( wrf_err_message )
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
!  gwd_opt = 1 only works with YSU & MYNN PBL.
!-----------------------------------------------------------------------

      oops = 0
      DO i = 1, model_config_rec % max_dom
         IF (model_config_rec%gwd_opt == 1 ) THEN
            IF ( (model_config_rec%bl_pbl_physics(i) .EQ. YSUSCHEME ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME2 ) .OR. &
                 (model_config_rec%bl_pbl_physics(i) .EQ. MYNNPBLSCHEME3) ) THEN
                !NO PROBLEM
            ELSE
               model_config_rec%gwd_opt = 0
               oops = oops + 1
            END IF
         END IF
      ENDDO      ! Loop over domains
      IF ( oops .GT. 0 ) THEN
         wrf_err_message = '--- NOTE: bl_pbl_physics /= 1,5,6 implies gwd_opt cannot be 1, resetting'
         CALL wrf_message ( wrf_err_message )
      END IF

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
         CALL wrf_message ( wrf_err_message )
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
! For nwp_diagnostics = 1, history_interval must be used.           
!-----------------------------------------------------------------------

      IF ( ( model_config_rec%nwp_diagnostics .NE. 0 ) .AND. &
           ( model_config_rec%history_interval(1) .EQ. 0 ) ) THEN
         wrf_err_message = '--- ERROR:  nwp_diagnostics requires the use of "history_interval" namelist.'
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = '---         Replace interval variable with "history_interval".'
        ! CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF

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
         !CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
! For adaptive time stepping, certain physics schemes are not allowed
! to have intermittent frequencies.  So, for those schemes below, we just
! turn the frequencies so that the schemes are called for each time step.
!-----------------------------------------------------------------------

      IF ( model_config_rec%use_adaptive_time_step ) THEN
         IF ( ( model_config_rec%cu_physics(1) .EQ. BMJSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. MESO_SAS     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. SASSCHEME     ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. OSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. NSASSCHEME    ) .OR. &
              ( model_config_rec%cu_physics(1) .EQ. TIEDTKESCHEME ) ) THEN
            wrf_err_message = '--- WARNING: If use_adaptive_time_step, must use cudt=0 for the following CU schemes:'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          BMJ, all SAS, Tiedtke' 
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '---          CUDT=0 has been done for you.'
            CALL wrf_message ( wrf_err_message )
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
         !      CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
               CALL wrf_message ( wrf_err_message )
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
                wrf_err_message = '---          Grell (G3) CU scheme' 
                CALL wrf_message ( wrf_err_message )
                wrf_err_message = '---          Grell-Devenyi (GD) CU scheme' 
         !       CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !       CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         ELSEIF ( ( model_config_rec%bl_pbl_physics(i) .NE. TEMFPBLSCHEME ) .AND. &
                  ( model_config_rec%sf_sfclay_physics(i) .EQ. TEMFSFCSCHEME ) ) THEN
            wrf_err_message = '--- ERROR: Using sf_sfclay_physics=10 requires bl_pbl_physics=10 '
         !   CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
         END IF
      ENDDO      ! Loop over domains

!-----------------------------------------------------------------------
!  Need to set lagday to 150 if tmn_update is 1
!-----------------------------------------------------------------------

      IF ( model_config_rec%tmn_update .EQ. 1 .AND. &
           model_config_rec%lagday .EQ. 1 ) THEN 
           wrf_err_message = '--- ERROR: Using tmn_update=1 requires lagday=150 '
         !  CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
      END IF

!-----------------------------------------------------------------------
!  Do not allow digital filtering to be run with TEMF.
!-----------------------------------------------------------------------

      DO i = 1, model_config_rec % max_dom
         IF ( ( model_config_rec%bl_pbl_physics(i) .EQ. TEMFPBLSCHEME ) .AND. &
              (model_config_rec%dfi_opt .NE. DFI_NODFI) )  THEN
            wrf_err_message = '--- ERROR: DFI not available for bl_pbl_physics=10 '
         !   CALL wrf_error_fatal ( TRIM(wrf_err_message) )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         ! CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         CALL wrf_message ( wrf_err_message )
      END IF
 
!-----------------------------------------------------------------------
!  scalar_pblmix = 1 should be turned on for mp_physics=28.
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
         CALL wrf_message ( wrf_err_message )
         wrf_err_message = 'resetting scalar_pblmix = 1'
         CALL wrf_message ( wrf_err_message )
      END IF

!-----------------------------------------------------------------------
!  DJW Check that we're not using ndown and vertical nesting.
!-----------------------------------------------------------------------
     DO i=1,model_config_rec%max_dom
       IF ((model_config_rec%vert_refine_method(i) .NE. 0) .AND. (model_config_rec%vert_refine_fact .NE. 1)) THEN
         write(wrf_err_message,'(A)') '--- ERROR: vert_refine_fact is ndown specific and cannot be used with vert_refine_method, and vice versa.'
         CALL wrf_message( wrf_err_message )
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
         !    CALL wrf_error_fatal( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !         CALL wrf_error_fatal( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
         !   CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
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
            wrf_err_message = '--- ERROR: vert_refine_method=2 only works with either RRTM or RRTMG'
         !   CALL wrf_error_fatal ( wrf_err_message )
         CALL wrf_debug ( 0, TRIM( wrf_err_message ) )
         fatal_error = .true.
         count_fatal_error = count_fatal_error + 1
          END IF 
        END IF
      END DO

!-----------------------------------------------------------------------
!  Remapping namelist variables for gridded and surface fdda to aux streams 9 and 10.
!  Relocated here so that the remappings are after checking the namelist for inconsistencies.
!-----------------------------------------------------------------------

#     include "../dyn_em/namelist_remappings_em.h"

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
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting force_sfc_in_vinterp=0.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec % force_sfc_in_vinterp = 0
         END IF
         IF ( ( .NOT. model_config_rec % use_surface )  .AND. &
              ( model_config_rec % lowest_lev_from_sfc ) ) THEN
            wrf_err_message = '--- NOTE: Inconsistent vertical interpolation settings in program real.'
            CALL wrf_message ( wrf_err_message )
            wrf_err_message = '--- NOTE: With use_surface=F, automatically setting lowest_lev_from_sfc=F.'
            CALL wrf_message ( wrf_err_message )
            model_config_rec % lowest_lev_from_sfc = .FALSE.
         END IF
      END IF
#endif

!-----------------------------------------------------------------------
!  Set the namelist parameter o3input to 0 for the radiation schemes other
!  than RRTMG_LWSCHEME and RRTMG_SWSCHEME.
!-----------------------------------------------------------------------

      IF ( ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )  .OR. &
           ( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME_FAST )  .OR. &
           ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME_FAST )  ) THEN
         wrf_err_message = '--- NOTE: RRTMG radiation is used, namelist ' // &
                           'value for o3input (ozone input) is used '
!        CALL wrf_message ( wrf_err_message )
      ELSE
         model_config_rec % o3input = 0
         wrf_err_message = '--- NOTE: RRTMG radiation is not used, setting:  ' // &
                           'o3input=0 to avoid data pre-processing'
         CALL wrf_message ( wrf_err_message )
      END IF

      IF ( fatal_error ) THEN
      WRITE (wrf_err_message, FMT='(A,I6, A)') 'NOTE:  ', count_fatal_error, &
                                            ' namelist settings are wrong. Please check and reset these options'
                                                         
      CALL wrf_error_fatal ( wrf_err_message )
      END IF

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

      INTEGER :: numsoiltemp , nummosaictemp
      INTEGER :: i


!-----------------------------------------------------------------------
! Set the namelist mosaic_cat_soil parameter for the Noah-mosaic  scheme if sf_surface_mosaic == 1.  
!-----------------------------------------------------------------------

      IF ( model_config_rec % sf_surface_mosaic .EQ. 1 ) THEN
      
      numsoiltemp = model_config_rec % num_soil_layers
      nummosaictemp = model_config_rec % mosaic_cat
      
         model_config_rec % mosaic_cat_soil = numsoiltemp * nummosaictemp

         wrf_err_message = '--- NOTE: Noah-mosaic is in use, setting:  ' // &
                           'mosaic_cat_soil = mosaic_cat * num_soil_layers'
         CALL wrf_message ( wrf_err_message )

      END IF     
      
#if ( (NMM_CORE != 1) && (DA_CORE != 1) )
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
         CALL wrf_message ( wrf_err_message )

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
! Set the namelist parameters for the RRTMG radiation scheme if either
! ra_lw_physics = RRTMG_LWSCHEME or ra_sw_physics = RRTMG_SWSCHEME.
!-----------------------------------------------------------------------

      IF (( model_config_rec % ra_lw_physics(1) .EQ. RRTMG_LWSCHEME ) .OR. &
          ( model_config_rec % ra_sw_physics(1) .EQ. RRTMG_SWSCHEME )) THEN
         model_config_rec % levsiz = 59
         model_config_rec % alevsiz = 12
         model_config_rec % no_src_types = 6

         wrf_err_message = '--- NOTE: RRTMG radiation is in use, setting:  ' // &
                           'levsiz=59, alevsiz=12, no_src_types=6'
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
      IF ( model_config_rec % sf_surface_physics(1) .EQ. NOAHMPSCHEME   ) &
           model_config_rec % num_soil_layers = 4
      IF ( model_config_rec % sf_surface_physics(1) .EQ. RUCLSMSCHEME .AND. &
           (model_config_rec % num_soil_layers .NE. 6 .AND. model_config_rec % num_soil_layers .NE. 9) ) &
           model_config_rec % num_soil_layers = 6
      IF ( model_config_rec % sf_surface_physics(1) .EQ. PXLSMSCHEME ) &
           model_config_rec % num_soil_layers = 2
      IF ( model_config_rec % sf_surface_physics(1) .EQ. CLMSCHEME ) &
           model_config_rec % num_soil_layers = 10
      IF ( model_config_rec % sf_surface_physics(1) .EQ. 88          ) &
           model_config_rec % num_soil_layers = 4

      WRITE (wrf_err_message, FMT='(A,I6)') '--- NOTE: num_soil_layers has been set to ', &
                                             model_config_rec % num_soil_layers
      CALL wrf_message ( wrf_err_message )

   END SUBROUTINE set_physics_rconfigs


!=======================================================================

   END MODULE module_check_a_mundo

!=======================================================================

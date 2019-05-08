subroutine da_ad_model ()

   !-----------------------------------------------------------------------
   ! Purpose: Acrivate nonlinear model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 05/19/2010
   !-----------------------------------------------------------------------

   implicit none

   integer :: time_step, alarmid, gwd_opt

   integer :: end_year0, end_month0, end_day0, end_hour0

   if (trace_use_dull) call da_trace_entry("da_ad_model")

#ifdef _OPENMP
   call setfeenv()
#endif

   IF ( checkpoint_interval .LE. 0 ) THEN
      ! Set the file names and interval for reading basic states.
      model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
      call nl_get_time_step ( head_grid%id, time_step )
      call nl_set_auxinput6_interval_s (head_grid%id, time_step )
      call nl_set_io_form_auxinput6 ( head_grid%id, 2 )
      call nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

      ! Set the file names and interval for reading adjoint forcing.
      model_config_rec%auxinput17_inname = "af_d<domain>_<date>"
      call nl_set_auxinput17_interval_s ( head_grid%id, var4d_bin )
      call nl_set_io_form_auxinput17 ( head_grid%id, 2 )
      call nl_set_frames_per_auxinput17 ( head_grid%id, 1 )
   ENDIF
   call nl_get_gwd_opt ( head_grid%id, gwd_opt )

   ! Set all perturbation to ZERO

   head_grid%a_h_diabatic = 0.0

   head_grid%a_rainnc  = 0.0
   head_grid%a_rainncv = 0.0
   head_grid%a_rainc  = 0.0
   head_grid%a_raincv = 0.0
   head_grid%a_pratec = 0.0

! I&O variables
   head_grid%a_scalar = 0.0
   head_grid%a_tke_2 = 0.0
   head_grid%a_p=0.0
   head_grid%a_al=0.0

! O variables
   head_grid%a_tke_1 = 0.0

   head_grid%a_ru = 0.0
   head_grid%a_rv = 0.0
   head_grid%a_rw = 0.0
   head_grid%a_ww = 0.0
   head_grid%a_alt = 0.0
   head_grid%a_php = 0.0
   head_grid%a_mut = 0.0
   head_grid%a_muu = 0.0
   head_grid%a_muv = 0.0
   head_grid%a_p_hyd_w = 0.0
   head_grid%a_p_hyd = 0.0
   head_grid%a_z_at_w = 0.0
   head_grid%a_z = 0.0
   head_grid%a_zx = 0.0
   head_grid%a_zy = 0.0
   head_grid%a_rdz = 0.0
   head_grid%a_rdzw = 0.0
   head_grid%a_div = 0.0
   head_grid%a_nba_mij = 0.0
   head_grid%a_nba_rij = 0.0
   head_grid%a_defor11 = 0.0
   head_grid%a_defor22 = 0.0
   head_grid%a_defor33 = 0.0
   head_grid%a_defor12 = 0.0
   head_grid%a_defor13 = 0.0
   head_grid%a_defor23 = 0.0
   head_grid%a_bn2 = 0.0
   head_grid%a_xkmh = 0.0
   head_grid%a_xkmv = 0.0
   head_grid%a_xkhh = 0.0
   head_grid%a_xkhv = 0.0
   head_grid%a_ru_tend = 0.0
   head_grid%a_rv_tend = 0.0
   head_grid%a_u_save = 0.0
   head_grid%a_v_save = 0.0
   head_grid%a_t_save = 0.0
   head_grid%a_rthften = 0.0
   head_grid%a_rqvften = 0.0
   head_grid%a_rthcuten = 0.0
   head_grid%a_rqvcuten = 0.0
   head_grid%a_muts = 0.0
   head_grid%a_mudf = 0.0
   head_grid%a_ru_m = 0.0
   head_grid%a_rv_m = 0.0
   head_grid%a_ww_m = 0.0

! I variables
   head_grid%a_ustm=0.0
   head_grid%a_hfx=0.0
   head_grid%a_qfx=0.0

   IF ( gwd_opt .EQ. 1 ) THEN
      head_grid%a_dtaux3d = 0.0
      head_grid%a_dtauy3d = 0.0
      head_grid%a_dusfcg = 0.0
      head_grid%a_dvsfcg = 0.0
   ENDIF

   head_grid%a_u_1 = 0.0
   head_grid%a_v_1 = 0.0
   head_grid%a_w_1 = 0.0
   head_grid%a_t_1 = 0.0
   head_grid%a_ph_1 = 0.0
   head_grid%a_mu_1 = 0.0

   head_grid%a_u_2 = 0.0
   head_grid%a_v_2 = 0.0
   head_grid%a_w_2 = 0.0
   head_grid%a_t_2 = 0.0
   head_grid%a_ph_2 = 0.0
   head_grid%a_mu_2 = 0.0

   head_grid%a_moist = 0.0
   head_grid%a_tracer = 0.0
#if (WRF_CHEM == 1)
   head_grid%a_chem = 0.0
   head_grid%a_scaleant = 0.0
   head_grid%a_scalebb = 0.0
#endif

   head_grid%a_pblh = 0.0
   head_grid%a_ust = 0.0
   head_grid%a_mol = 0.0
   head_grid%a_hfx = 0.0
   head_grid%a_qfx = 0.0
   head_grid%a_tslb = 0.0
   head_grid%a_smois = 0.0
   head_grid%a_canwat = 0.0

   head_grid%a_u_bxs = 0.0; head_grid%a_u_bxe = 0.0; head_grid%a_u_bys = 0.0; head_grid%a_u_bye = 0.0
   head_grid%a_v_bxs = 0.0; head_grid%a_v_bxe = 0.0; head_grid%a_v_bys = 0.0; head_grid%a_v_bye = 0.0
   head_grid%a_w_bxs = 0.0; head_grid%a_w_bxe = 0.0; head_grid%a_w_bys = 0.0; head_grid%a_w_bye = 0.0
   head_grid%a_ph_bxs = 0.0; head_grid%a_ph_bxe = 0.0; head_grid%a_ph_bys = 0.0; head_grid%a_ph_bye = 0.0
   head_grid%a_t_bxs = 0.0; head_grid%a_t_bxe = 0.0; head_grid%a_t_bys = 0.0; head_grid%a_t_bye = 0.0
   head_grid%a_mu_bxs = 0.0; head_grid%a_mu_bxe = 0.0; head_grid%a_mu_bys = 0.0; head_grid%a_mu_bye = 0.0
   head_grid%a_moist_bxs = 0.0; head_grid%a_moist_bxe = 0.0; head_grid%a_moist_bys = 0.0; head_grid%a_moist_bye = 0.0
   head_grid%a_scalar_bxs = 0.0; head_grid%a_scalar_bxe = 0.0; head_grid%a_scalar_bys = 0.0; head_grid%a_scalar_bye = 0.0
   head_grid%a_tracer_bxs = 0.0; head_grid%a_tracer_bxe = 0.0; head_grid%a_tracer_bys = 0.0; head_grid%a_tracer_bye = 0.0
#if (WRF_CHEM == 1)
   head_grid%a_chem_bxs = 0.0; head_grid%a_chem_bxe = 0.0; head_grid%a_chem_bys = 0.0; head_grid%a_chem_bye = 0.0
#endif

   head_grid%a_u_btxs = 0.0; head_grid%a_u_btxe = 0.0; head_grid%a_u_btys = 0.0; head_grid%a_u_btye = 0.0
   head_grid%a_v_btxs = 0.0; head_grid%a_v_btxe = 0.0; head_grid%a_v_btys = 0.0; head_grid%a_v_btye = 0.0
   head_grid%a_w_btxs = 0.0; head_grid%a_w_btxe = 0.0; head_grid%a_w_btys = 0.0; head_grid%a_w_btye = 0.0
   head_grid%a_ph_btxs = 0.0; head_grid%a_ph_btxe = 0.0; head_grid%a_ph_btys = 0.0; head_grid%a_ph_btye = 0.0
   head_grid%a_t_btxs = 0.0; head_grid%a_t_btxe = 0.0; head_grid%a_t_btys = 0.0; head_grid%a_t_btye = 0.0
   head_grid%a_mu_btxs = 0.0; head_grid%a_mu_btxe = 0.0; head_grid%a_mu_btys = 0.0; head_grid%a_mu_btye = 0.0
   head_grid%a_moist_btxs = 0.0; head_grid%a_moist_btxe = 0.0; head_grid%a_moist_btys = 0.0; head_grid%a_moist_btye = 0.0
   head_grid%a_scalar_btxs = 0.0; head_grid%a_scalar_btxe = 0.0; head_grid%a_scalar_btys = 0.0; head_grid%a_scalar_btye = 0.0
   head_grid%a_tracer_btxs = 0.0; head_grid%a_tracer_btxe = 0.0; head_grid%a_tracer_btys = 0.0; head_grid%a_tracer_btye = 0.0
#if (WRF_CHEM == 1)
   head_grid%a_chem_btxs = 0.0; head_grid%a_chem_btxe = 0.0; head_grid%a_chem_btys = 0.0; head_grid%a_chem_btye = 0.0
#endif

   head_grid%g_u_1 = 0.0  
   head_grid%g_v_1 = 0.0
   head_grid%g_w_1 = 0.0
   head_grid%g_t_1 = 0.0
   head_grid%g_ph_1 = 0.0
   head_grid%g_mu_1 = 0.0

   head_grid%g_u_2 = 0.0
   head_grid%g_v_2 = 0.0
   head_grid%g_w_2 = 0.0
   head_grid%g_t_2 = 0.0
   head_grid%g_ph_2 = 0.0
   head_grid%g_mu_2 = 0.0

   head_grid%g_p = 0.0
   head_grid%g_alt = 0.0

   head_grid%g_moist = 0.0
   head_grid%g_tracer = 0.0
#if (WRF_CHEM == 1)
   head_grid%g_chem = 0.0
   head_grid%g_scaleant = 0.0
   head_grid%g_scalebb = 0.0
#endif

   head_grid%g_pblh = 0.0
   head_grid%g_ust = 0.0
   head_grid%g_mol = 0.0
   head_grid%g_hfx = 0.0
   head_grid%g_qfx = 0.0
   head_grid%g_tslb = 0.0
   head_grid%g_smois = 0.0
   head_grid%g_canwat = 0.0

   head_grid%g_scalar = 0.0
   head_grid%g_rainnc  = 0.0
   head_grid%g_rainncv = 0.0
   head_grid%g_rainc  = 0.0
   head_grid%g_raincv = 0.0
   head_grid%g_pratec = 0.0

   ! Set the physics options to what they are in TL and AD
   if (original_mp_physics .NE. 0 .and. original_mp_physics .NE. 98 .and. original_mp_physics .NE. 99) &
      call nl_set_mp_physics (head_grid%id, original_mp_physics_ad) 
   if ( original_sf_surface_physics .ne. 1 .AND. &
        original_sf_surface_physics .ne. 7 ) then
      call nl_set_ra_lw_physics (head_grid%id, 0)
      call nl_set_ra_sw_physics (head_grid%id, 0)
   end if
   if ( original_sf_sfclay_physics .gt. 0 .and. &
        original_sf_sfclay_physics .ne. 7 .and. &
        original_sf_sfclay_physics .ne. 91 ) &
      call nl_set_sf_sfclay_physics (head_grid%id, 0)
   if ( original_bl_pbl_physics .gt. 0 .and. original_bl_pbl_physics .ne. 7) &
      call nl_set_bl_pbl_physics (head_grid%id, 98)
   if ( original_cu_physics .gt. 0 ) then
      call nl_set_cu_physics (head_grid%id, 98) 
      head_grid%cudt =0
   endif
   !call nl_set_mp_zero_out (head_grid%id, 0)
   !call nl_set_sf_surface_physics (head_grid%id, 1)
   call nl_set_ifsnow (head_grid%id, 1)
   call nl_set_icloud (head_grid%id, 0)
   !call nl_set_isfflx (head_grid%id, 0)

   if ( model_config_rec%var4d_detail_out ) gradient_out = .true.

   call nl_set_var4d_run (head_grid%id, .true.)

   ! WRF model time-stepping.  Calls integrate().
   IF ( checkpoint_interval .GT. 0 ) THEN

      end_year0 = model_config_rec%end_year(head_grid%id)
      end_month0 = model_config_rec%end_month(head_grid%id)
      end_day0 = model_config_rec%end_day(head_grid%id)
      end_hour0 = model_config_rec%end_hour(head_grid%id)

      call da_set_run_hours ()

      call wrf_run_ad_checkpt

      if ( run_hours .gt. adtl_run_hours .and. run_days .eq. 0 ) then
         model_config_rec%run_hours = run_hours

         model_config_rec%end_year(head_grid%id) = end_year0
         model_config_rec%end_month(head_grid%id) = end_month0
         model_config_rec%end_day(head_grid%id) = end_day0
         model_config_rec%end_hour(head_grid%id) = end_hour0

         IF ( ASSOCIATED( head_grid%domain_clock ) ) THEN
           IF ( head_grid%domain_clock_created ) THEN
             CALL WRFU_ClockDestroy( head_grid%domain_clock )
             head_grid%domain_clock_created = .FALSE.
           ENDIF
         ENDIF
         IF ( ASSOCIATED( head_grid%alarms ) .AND. &
              ASSOCIATED( head_grid%alarms_created ) ) THEN
           DO alarmid = 1, MAX_WRF_ALARMS
             IF ( head_grid%alarms_created( alarmid ) ) THEN
               CALL WRFU_AlarmDestroy( head_grid%alarms( alarmid ) )
               head_grid%alarms_created( alarmid ) = .FALSE.
             ENDIF
           ENDDO
         ENDIF
         call Setup_Timekeeping ( head_grid )

      end if 
   ELSE
      call wrf_run_ad
   ENDIF

   ! restore the physics options to the originals
   call nl_set_mp_physics (head_grid%id, original_mp_physics)
   call nl_set_ra_lw_physics (head_grid%id, original_ra_lw_physics)
   call nl_set_ra_sw_physics (head_grid%id, original_ra_sw_physics)
   call nl_set_sf_sfclay_physics (head_grid%id, original_sf_sfclay_physics)
   call nl_set_bl_pbl_physics (head_grid%id, original_bl_pbl_physics)
   call nl_set_cu_physics (head_grid%id, original_cu_physics)
   head_grid%cudt = original_cudt
   !call nl_set_mp_zero_out (head_grid%id, original_mp_zero_out)
   !call nl_set_sf_surface_physics (head_grid%id, original_sf_surface_physics)
   call nl_set_ifsnow (head_grid%id, original_ifsnow)
   call nl_set_icloud (head_grid%id, original_icloud)
   !call nl_set_isfflx (head_grid%id, original_isfflx)

   IF ( checkpoint_interval .LE. 0 ) THEN
   ! Turn off auxinput6,17 reading and auxhist17 output
      call nl_set_auxinput6_interval_s (head_grid%id, 0 )
      call nl_set_auxinput17_interval_s ( head_grid%id, 0 )
   ENDIF

   IF ( ASSOCIATED( head_grid%domain_clock ) ) THEN
     IF ( head_grid%domain_clock_created ) THEN
       CALL WRFU_ClockDestroy( head_grid%domain_clock )
       head_grid%domain_clock_created = .FALSE.
     ENDIF
   ENDIF
   IF ( ASSOCIATED( head_grid%alarms ) .AND. &
        ASSOCIATED( head_grid%alarms_created ) ) THEN
     DO alarmid = 1, MAX_WRF_ALARMS
       IF ( head_grid%alarms_created( alarmid ) ) THEN
         CALL WRFU_AlarmDestroy( head_grid%alarms( alarmid ) )
         head_grid%alarms_created( alarmid ) = .FALSE.
       ENDIF
     ENDDO
   ENDIF
   call Setup_Timekeeping ( head_grid )

   if (trace_use_dull) call da_trace_exit("da_ad_model")

end subroutine da_ad_model



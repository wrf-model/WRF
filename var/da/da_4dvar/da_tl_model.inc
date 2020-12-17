subroutine da_tl_model ()

   !-----------------------------------------------------------------------
   ! Purpose: Acrivate tangent linear  model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 05/19/2010
   !-----------------------------------------------------------------------

   implicit none

   integer :: time_step, fid, ierr, i, j, alarmid, gwd_opt
   character (len=80)     :: inpname 

   if (trace_use_dull) call da_trace_entry("da_tl_model")

#ifdef _OPENMP
   call setfeenv()
#endif

   ! Set up perturbation output at each slots
   model_config_rec%auxhist8_outname = "tl_d<domain>_<date>"
   call nl_set_auxhist8_interval_s ( head_grid%id, var4d_bin ) 
   call nl_set_auxhist8_begin_s ( head_grid%id, 0 )
   call nl_set_auxhist8_end_h ( head_grid%id, 999 )
   call nl_set_io_form_auxhist8 ( head_grid%id, 2 )
   call nl_set_frames_per_auxhist8 ( head_grid%id, 1 )


   ! Set up basic states reading
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   call nl_get_time_step ( head_grid%id, time_step )
   call nl_set_auxinput6_interval_s (head_grid%id, time_step )
   call nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   call nl_set_frames_per_auxinput6 ( head_grid%id, 1 )
   call nl_get_gwd_opt ( head_grid%id, gwd_opt )

   ! Set all perturbation to ZERO

   head_grid%g_scalar = 0.0
   head_grid%g_tke_2 = 0.0
   head_grid%g_al = 0.0
   head_grid%g_ustm = 0.0
   head_grid%g_hfx = 0.0
   head_grid%g_qfx = 0.0
   head_grid%g_h_diabatic = 0.0

   head_grid%g_u_1 = 0.0
   head_grid%g_v_1 = 0.0
   head_grid%g_w_1 = 0.0
   head_grid%g_t_1 = 0.0
   head_grid%g_ph_1 = 0.0
   head_grid%g_mu_1 = 0.0
   head_grid%g_tke_1 = 0.0

   head_grid%g_ru = 0.0
   head_grid%g_rv = 0.0
   head_grid%g_rw = 0.0
   head_grid%g_ww = 0.0
   head_grid%g_alt = 0.0
   head_grid%g_php = 0.0
   head_grid%g_mut = 0.0
   head_grid%g_muu = 0.0
   head_grid%g_muv = 0.0
   head_grid%g_p_hyd_w = 0.0
   head_grid%g_p_hyd = 0.0
   head_grid%g_z_at_w = 0.0
   head_grid%g_z = 0.0
   head_grid%g_zx = 0.0
   head_grid%g_zy = 0.0
   head_grid%g_rdz = 0.0
   head_grid%g_rdzw = 0.0
   head_grid%g_div = 0.0
   head_grid%g_nba_mij = 0.0
   head_grid%g_nba_rij = 0.0
   head_grid%g_defor11 = 0.0
   head_grid%g_defor22 = 0.0
   head_grid%g_defor33 = 0.0
   head_grid%g_defor12 = 0.0
   head_grid%g_defor13 = 0.0
   head_grid%g_defor23 = 0.0
   head_grid%g_bn2 = 0.0
   head_grid%g_xkmh = 0.0
   head_grid%g_xkmv = 0.0
   head_grid%g_xkhh = 0.0
   head_grid%g_xkhv = 0.0
   head_grid%g_ru_tend = 0.0
   head_grid%g_rv_tend = 0.0
   head_grid%g_u_save = 0.0
   head_grid%g_v_save = 0.0
   head_grid%g_t_save = 0.0
   head_grid%g_rthften = 0.0
   head_grid%g_rqvften = 0.0
   head_grid%g_rthcuten = 0.0
   head_grid%g_rqvcuten = 0.0
   head_grid%g_muts = 0.0
   head_grid%g_mudf = 0.0
   head_grid%g_ru_m = 0.0
   head_grid%g_rv_m = 0.0
   head_grid%g_ww_m = 0.0

   IF ( gwd_opt .EQ. 1 ) THEN
      head_grid%g_dtaux3d = 0.0
      head_grid%g_dtauy3d = 0.0
      head_grid%g_dusfcg = 0.0
      head_grid%g_dvsfcg = 0.0
   ENDIF

   head_grid%g_rainnc  = 0.0
   head_grid%g_rainncv = 0.0
   head_grid%g_rainc  = 0.0
   head_grid%g_raincv = 0.0
   head_grid%g_pratec = 0.0


   ! Set the physics options to what they are in TL and AD
   if (original_mp_physics .NE. 0 .and. original_mp_physics .NE. 98 .and. original_mp_physics .NE. 99) &
      call nl_set_mp_physics (head_grid%id, original_mp_physics_ad) 
   call nl_set_ra_lw_physics (head_grid%id, 0)
   call nl_set_ra_sw_physics (head_grid%id, 0)
   call nl_set_sf_sfclay_physics (head_grid%id, 0)
   if (original_bl_pbl_physics .gt. 0) &
      call nl_set_bl_pbl_physics (head_grid%id, 98)
   if (original_cu_physics .gt. 0) then
      call nl_set_cu_physics (head_grid%id, 98) 
      head_grid%cudt = 0
   endif
   !call nl_set_mp_zero_out (head_grid%id, 0)
   !call nl_set_sf_surface_physics (head_grid%id, 1)
   call nl_set_ifsnow (head_grid%id, 1)
   call nl_set_icloud (head_grid%id, 0)
   !call nl_set_isfflx (head_grid%id, 0)

   ! WRF model time-stepping.  Calls integrate().
   call nl_set_var4d_run (head_grid%id, .true.)
   call wrf_run_tl
	
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

   ! Turn off auxinput2 reading and auxhist8 output

   call nl_set_auxinput6_interval_s (head_grid%id, 0 )
   call nl_set_auxhist8_interval_s ( head_grid%id, 0 )

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

   if (trace_use_dull) call da_trace_exit("da_tl_model")

end subroutine da_tl_model



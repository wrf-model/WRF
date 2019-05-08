subroutine da_nl_model (it)

   !-----------------------------------------------------------------------
   ! Purpose: Acrivate nonlinear model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 04/28/2010
   !-----------------------------------------------------------------------

   implicit none
   
   integer, intent(in) :: it ! outer looper counter.
   integer :: time_step, alarmid
   character(len=256) :: rst_outname
#if (WRF_CHEM == 1)
   integer :: ioform8
   integer :: end_year0, end_month0, end_day0, end_hour0
#endif

   if (trace_use_dull) call da_trace_entry("da_nl_model")

   if ( it == 1 ) then
#ifdef _OPENMP
      call setfeenv()
#endif

#ifdef DM_PARALLEL
      ! We need the communicator generated in WRFDA
      local_communicator = comm
#endif

      ! Set up WRF model.
      call wrf_init ( .false. )
!      call nl_set_var4d_run (head_grid%id, .true.)
      model_config_rec%var4d_run = .true.
      config_flags%var4d_run = .true.

   endif
   
   ! 4dvar init
   !moved to the later part of the code
   !call init_4dvar

   ! Turn off  history output
   call nl_set_history_begin_y (head_grid%id, 9999)
   call nl_set_history_end_y (head_grid%id, 9999)
   call nl_set_history_interval (head_grid%id, 0)

   ! Set up basic states output
   if ( multi_inc == 1 ) then
     call nl_set_auxhist6_interval_s ( head_grid%id, var4d_bin )
   else
     call nl_get_time_step ( head_grid%id, time_step )
     call nl_set_auxhist6_interval_s ( head_grid%id, time_step ) 
   endif
   call nl_set_io_form_auxhist6 ( head_grid%id, 2 ) 
   call nl_set_frames_per_auxhist6 ( head_grid%id, 1 ) 

   if ( write_checkpoints .eq. 1 ) then
      !Turn off xtraj writing
      call nl_set_auxhist6_interval_s ( head_grid%id, 0 )

      if( .not. calc_hx_only) then
         !Turn on Output for checkpoint files
         call nl_get_restart_interval_h ( head_grid%id, original_restart_interval)
         call nl_set_restart_interval_m ( head_grid%id, checkpoint_interval)
         call nl_set_io_form_restart ( head_grid%id, 2 )
         rst_outname = "wrf_checkpoint_d<domain>_<date>"
         call nl_set_rst_outname ( head_grid%id, rst_outname)
         !call nl_set_restart_begin_m( head_grid%id, checkpoint_interval )
         call nl_set_restart_begin_m( head_grid%id, 0 )
!         if ( run_hours .gt. adtl_run_hours .and. run_days .eq. 0 .and. &
!              adtl_run_hours .gt. 0 ) &
!            call nl_set_restart_end_h( head_grid%id, adtl_run_hours )
      endif

#if (WRF_CHEM == 1)
   else if ( write_checkpoints .eq. 2 ) then
      !Turn off xtraj writing
      call nl_set_auxhist6_interval_s ( head_grid%id, 0 )

      end_year0 = model_config_rec%end_year(head_grid%id)
      end_month0 = model_config_rec%end_month(head_grid%id)
      end_day0 = model_config_rec%end_day(head_grid%id)
      end_hour0 = model_config_rec%end_hour(head_grid%id)

      call da_set_run_hours ()
   else
      ! Turn off gocart_bg field reading
      call nl_get_io_form_auxinput8(head_grid%id, ioform8)
      if ( model_config_rec%start_hour(head_grid%id) .ne. 0 ) then
         call nl_set_io_form_auxinput8(head_grid%id, 0)
      endif
#endif
   endif

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

   call domain_clock_get(head_grid, start_time=head_grid%start_subtime)
   call domain_clock_get(head_grid, stop_time=head_grid%stop_subtime)

   ! 4dvar init
   call init_4dvar

#if (WRF_CHEM == 1)
   if ( model_config_rec%chem_obs_cost ) then
      call da_copy_chem_to_model( head_grid, it )

      !Need a first guess file that includes scaleant and scalebb
      if ( it == 1 ) then
         call da_init_model_output
      end if
   end if
#endif

   ! Set the physics options to what they are in TL and AD
   !call nl_set_mp_physics (head_grid%id, 0)
   !call nl_set_ra_lw_physics (head_grid%id, 0)
   !call nl_set_ra_sw_physics (head_grid%id, 0)
   !call nl_set_sf_sfclay_physics (head_grid%id, 0)
   !call nl_set_bl_pbl_physics (head_grid%id, 0)
   !call nl_set_cu_physics (head_grid%id, 0)
   !call nl_set_mp_zero_out (head_grid%id, 0)
   !call nl_set_sf_surface_physics (head_grid%id, 1)
   !call nl_set_ifsnow (head_grid%id, 1)
   !call nl_set_icloud (head_grid%id, 0)
   !call nl_set_isfflx (head_grid%id, 0)

   ! WRF model time-stepping.  Calls integrate().
!   ! Automatic restarting after each cycle_interval.  Useful for long assimilation windows.
!   ! Not fully functional yet. -JJGuerrette
!   if ( cycle_interval .gt. 0 ) then 
!      call wrf_run_cycle
!   else
      call wrf_run
!   end if

   ! restore the physics options to the originals
   !call nl_set_mp_physics (head_grid%id, original_mp_physics)
   !call nl_set_ra_lw_physics (head_grid%id, original_ra_lw_physics)
   !call nl_set_ra_sw_physics (head_grid%id, original_ra_sw_physics)
   !call nl_set_sf_sfclay_physics (head_grid%id, original_sf_sfclay_physics)
   !call nl_set_bl_pbl_physics (head_grid%id, original_bl_pbl_physics)
   !call nl_set_cu_physics (head_grid%id, original_cu_physics)
   !call nl_set_mp_zero_out (head_grid%id, original_mp_zero_out)
   !call nl_set_sf_surface_physics (head_grid%id, original_sf_surface_physics)
   !call nl_set_ifsnow (head_grid%id, original_ifsnow)
   !call nl_set_icloud (head_grid%id, original_icloud)
   !call nl_set_isfflx (head_grid%id, original_isfflx)

   ! Turn off basic states output
   call nl_set_io_form_auxhist6 ( head_grid%id, 0 ) 
   call nl_set_auxhist6_interval_s ( head_grid%id, 0 ) 

   if ( write_checkpoints .eq. 1 ) then
      call nl_set_restart_interval_h ( head_grid%id, original_restart_interval)
      call nl_set_io_form_restart ( head_grid%id, 0 )
      rst_outname = "wrfrst_d<domain>_<date>"
      call nl_set_rst_outname ( head_grid%id, rst_outname)
      call nl_set_restart_begin_h( head_grid%id, original_restart_interval)
#if (WRF_CHEM == 1)
   else if ( write_checkpoints .eq. 2 ) then
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
   else
      call nl_set_io_form_auxinput8(head_grid%id, ioform8)
#endif
   end if

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

   ! Turn off boundary input from now.
   if ( checkpoint_interval .eq. 0 ) &
      CALL nl_set_io_form_boundary( head_grid%id, 0 )

   if (trace_use_dull) call da_trace_exit("da_nl_model")

end subroutine da_nl_model

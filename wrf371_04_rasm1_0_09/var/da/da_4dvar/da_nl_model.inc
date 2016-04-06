subroutine da_nl_model (it)

   !-----------------------------------------------------------------------
   ! Purpose: Acrivate nonlinear model
   !    Author: Xin Zhang,  MMM/ESSL/NCAR,  Date: 04/28/2010
   !-----------------------------------------------------------------------

   implicit none
   
   integer, intent(in) :: it ! outer looper counter.
   integer :: time_step, alarmid

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

   endif
   
   ! 4dvar init
   call init_4dvar

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
   call wrf_run

   ! restore the physics options to the originals
   !call nl_set_mp_physics (head_grid%id, original_ra_lw_physics)
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
   CALL nl_set_io_form_boundary( head_grid%id, 0 )
	
   if (trace_use_dull) call da_trace_exit("da_nl_model")

end subroutine da_nl_model

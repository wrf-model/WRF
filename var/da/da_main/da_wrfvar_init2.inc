subroutine da_wrfvar_init2

   !-------------------------------------------------------------------------
   ! Purpose: WRFVAR initialization routine, part 2
   !-------------------------------------------------------------------------

   implicit none

   integer :: i
   character(len=80) :: filename

   if (trace_use) call da_trace_entry("da_wrfvar_init2")

! Override the start time with the "analysis_date":
      read(analysis_date, fmt='(i4,5(1x,i2))') &
           start_year(1), start_month(1), start_day(1), start_hour(1), &
           start_minute(1), start_second(1)
      model_config_rec% start_year   = start_year
      model_config_rec% start_month  = start_month
      model_config_rec% start_day    = start_day
      model_config_rec% start_hour   = start_hour
      model_config_rec% start_minute = start_minute
      model_config_rec% start_second = start_second

   if (analysis_type(1:6) == "VERIFY" .or. analysis_type(1:6) == "verify") then
      anal_type_verify=.true.
   else
      anal_type_verify=.false.
   end if

   if (analysis_type(1:8) == "RANDOMCV" .or. analysis_type(1:8) == "randomcv") then
      anal_type_randomcv=.true.
   else
      anal_type_randomcv=.false.
   end if

   if (analysis_type(1:6) == "QC-OBS" .or. analysis_type(1:6) == "qc-obs") then
      anal_type_qcobs=.true.
   else
      anal_type_qcobs=.false.
   end if

   if (use_gpspwObs .and. use_gpsztdObs ) then
      call da_error(__FILE__,__LINE__, (/'can not assimilate gpspw and gpsztd simultaneously'/))
   end if

   if (fg_format==fg_format_kma_global .or. fg_format==fg_format_wrf_arw_global) then
      global = .true.
      nproc_x = 1
   else
      global = .false.
   end if

   !<DESCRIPTION>
   ! Among the configuration variables read from the namelist is
   ! debug_level. This is retrieved using nl_get_debug_level (Registry
   ! generated and defined in frame/module_configure.F).  The value is then
   ! used to set the debug-print information level for use by <a
   ! href=wrf_debug.html>wrf_debug</a> throughout the code. Debug_level
   ! of zero (the default) causes no information to be printed when the
   ! model runs. The higher the number (up to 1000) the more information is
   ! printed.
   ! 
   !</DESCRIPTION>

   call nl_get_debug_level (1, debug_level)
   call set_wrf_debug_level (debug_level)

   nullify(null_domain)


   if (max_dom > 1) then
      call da_error(__FILE__,__LINE__, (/'nesting not available for wrfvar'/))
   end if

   !<DESCRIPTION>
   ! The top-most domain in the simulation is then allocated and configured
   ! by calling <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a>.
   ! Here, in the case of this root domain, the routine is passed the
   ! globally accessible pointer to type(domain), head_grid, defined in
   ! frame/module_domain.F.  The parent is null and the child index is given
   ! as negative, signifying none.  Afterwards, because the call to
   ! alloc_and_configure_domain may modify the model configuration data
   ! stored in model_config_rec, the configuration information is again
   ! repacked into a buffer, broadcast, and unpacked on each task (for
   ! DM_PARALLEL compiles). The call to <a
   ! href=setup_timekeeping.html>setup_timekeeping</a> for head_grid relies
   ! on this configuration information, and it must occur after the second
   ! broadcast of the configuration information.
   ! 
   !</DESCRIPTION>

   call da_trace("da_wrfvar_init2",message="calling alloc_and_configure_domain")

   call alloc_and_configure_domain (domain_id=1, grid=head_grid, parent=null_domain, kid=-1)

   call da_trace("da_wrfvar_init2",message="calling model_to_grid_config_rec")
   call model_to_grid_config_rec (head_grid%id, model_config_rec, config_flags)

   call da_trace("da_wrfvar_init2",message="calling set_scalar_indices_from_config")
   call set_scalar_indices_from_config (head_grid%id , idum1, idum2)

   call da_trace("da_wrfvar_init2",message="calling init_wrfio")
   call init_wrfio

#ifdef DM_PARALLEL
   call get_config_as_buffer (configbuf, configbuflen, nbytes)
   call wrf_dm_bcast_bytes (configbuf, nbytes)
   call set_config_as_buffer (configbuf, configbuflen)
#endif

   call setup_timekeeping (head_grid)

   !<DESCRIPTION>
   ! The head grid is initialized with read-in data through the call to <a
   ! href=med_initialdata_input.html>med_initialdata_input</a>, which is
   ! passed the pointer head_grid and a locally declared configuration data
   ! structure, config_flags, that is set by a call to <a
   ! href=model_to_grid_config_rec.html>model_to_grid_config_rec</a>.  It is
   ! also necessary that the indices into the 4d tracer arrays such as
   ! moisture be set with a call to <a
   ! href=set_scalar_indices_from_config.html>set_scalar_indices_from_config</a>
   ! prior to the call to initialize the domain.  Both of these calls are
   ! told which domain they are setting up for by passing in the integer id
   ! of the head domain as <tt>head_grid%id</tt>, which is 1 for the
   ! top-most domain.
   ! 
   ! In the case that write_restart_at_0h is set to true in the namelist,
   ! the model simply generates a restart file using the just read-in data
   ! and then shuts down. This is used for ensemble breeding, and is not
   ! typically enabled.
   ! 
   !</DESCRIPTION>

   ! call med_initialdata_input(head_grid , config_flags,'fg')

   if ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_input (head_grid, config_flags, 'fg')
   end if

   ! FIX?
   ! call da_warning(__FILE__,__LINE__,(/"Fix me"/))
   ! head_grid%start_subtime = head_grid%start_time
   ! head_grid%stop_subtime = head_grid%stop_time

   if (rootproc) then
      call da_get_unit (cost_unit)
      call da_get_unit (grad_unit)
      call da_get_unit (jo_unit)
      call da_get_unit (check_max_iv_unit)
      call da_get_unit (check_buddy_unit)
      open(unit=cost_unit,file="cost_fn",status="replace")
      open(unit=grad_unit,file="grad_fn",status="replace")
      if (.not. print_detail_outerloop) then
         call da_get_unit (stats_unit)
         open(unit=stats_unit,file="statistics",status="replace")
      end if
      open(unit=jo_unit,file="jo",status="replace")
      open(unit=check_max_iv_unit,file="check_max_iv",status="replace")
      open(unit=check_buddy_unit ,file="buddy_check" ,status="replace")
   end if

   if (trace_use) call da_trace_exit("da_wrfvar_init2")

end subroutine da_wrfvar_init2



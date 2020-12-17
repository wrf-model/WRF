subroutine da_esmf_init( gcomp, importState, exportState, clock, rc )

   !-----------------------------------------------------------------------
   ! Purpose: WRFVAR init routine.
   !
   !     The arguments are:
   !       gcomp           Component
   !       importState     Importstate
   !       exportState     Exportstate
   !       clock           External clock
   !       rc              Return code; equals ESMF_SUCCESS if there are no
   !                       errors, otherwise ESMF_FAILURE.
   !
   ! Program_name, a global variable defined in frame/module_domain.F, is
   ! set, then a routine <a href=init_modules.html>init_modules</a> is
   ! called. This calls all the init programs that are provided by the
   ! modules that are linked into WRFVAR.  These include initialization of
   ! external I/O packages.   Also, some key initializations for
   ! distributed-memory parallelism occur here if DM_PARALLEL is specified
   ! in the compile: setting up I/O quilt processes to act as I/O servers
   ! and dividing up MPI communicators among those as well as initializing
   ! external communication packages.
   !-----------------------------------------------------------------------

   implicit none

   type(ESMF_GridComp), intent(inout) :: gcomp
   type(ESMF_State),    intent(inout) :: importState, exportState
   type(ESMF_Clock),    intent(inout) :: clock
   integer,             intent(out)   :: rc

   program_name = //"WRFVAR "//release_version

   !  Get the NAMELIST data for input.

   call init_modules(2)   ! Phase 2 resumes after mpi_init() (if it is called)
                          ! Phase 1 is called before ESMF starts up

   !<DESCRIPTION>
   ! The wrf namelist.input file is read and stored in the use associated
   ! structure model_config_rec, defined in frame/module_configure.F, by the
   ! call to <a href=initial_config.html>initial_config</a>.  On distributed
   ! memory parallel runs this is done only on one processor, and then
   ! broadcast as a buffer.  For distributed-memory, the broadcast of the
   ! configuration information is accomplished by first putting the
   ! configuration information into a buffer (<a
   ! href=get_config_as_buffer.html>get_config_as_buffer</a>), broadcasting
   ! the buffer, then setting the configuration information (<a
   ! href=set_config_as_buffer.html>set_config_as_buffer</a>).
   !
   !</DESCRIPTION>

#ifdef DM_PARALLEL
   if ( rootproc ) then
      call initial_config
   end if
   call get_config_as_buffer( configbuf, configbuflen, nbytes )
   call wrf_dm_bcast_bytes( configbuf, nbytes )
   call set_config_as_buffer( configbuf, configbuflen )
   call wrf_dm_initialize
#else
   call initial_config
#endif

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

   call nl_get_debug_level ( 1, debug_level )
   call set_wrf_debug_level ( debug_level )

   ! allocated and configure the mother domain

   nullify( null_domain )

   call nl_get_max_dom( 1, max_dom )
   if ( max_dom > 1 ) then
      call da_error(__FILE__,__LINE__, (/'nesting not available for wrfvar'/))
   end if

   !<DESCRIPTION>
   ! The top-most domain in the simulation is then allocated and configured
   ! by calling <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a>.
   ! Here, in the case of this root domain, the routine is passed the
   ! globally accessible pointer to type(domain), head_grid, defined in
   ! frame/module_domain.F.  The parent is null and the child index is given
   ! as negative, signifying none.  Afterwards, because the call to
   ! alloc_and_configure_domain may modify the model's configuration data
   ! stored in model_config_rec, the configuration information is again
   ! repacked into a buffer, broadcast, and unpacked on each task (for
   ! DM_PARALLEL compiles). The call to <a
   ! href=setup_timekeeping.html>setup_timekeeping</a> for head_grid relies
   ! on this configuration information, and it must occur after the second
   ! broadcast of the configuration information.
   ! 
   !</DESCRIPTION>
   call       da_message ((/program_name/))

   call da_trace("da_esmf_init",message="calling alloc_and_configure_domain")
   call alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                    grid       = head_grid ,          &
                                    parent     = null_domain ,        &
                                    kid        = -1                   )

   call da_trace("da_esmf_init",message="callingmodel_to_grid_config_rec")
   call model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )

   call da_trace("da_esmf_init",message="calling set_scalar_indices_from_config")
   call set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   call da_trace("da_esmf_init",message="calling init_wrfio")
   call init_wrfio

#ifdef DM_PARALLEL
   call get_config_as_buffer( configbuf, configbuflen, nbytes )
   call wrf_dm_bcast_bytes( configbuf, nbytes )
   call set_config_as_buffer( configbuf, configbuflen )
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

   if ((config_flags%real_data_init_type == 1) .OR. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_input( head_grid , config_flags,'fg' )
   end if

   !<DESCRIPTION>
   ! Once the top-level domain has been allocated, configured, and
   ! initialized, the model time integration is ready to proceed.  The start
   ! and stop times for the domain are set to the start and stop time of the
   ! model run, and then <a href=integrate.html>integrate</a> is called to
   ! advance the domain forward through that specified time interval.  On
   ! return, the simulation is completed.  A Mediation Layer-provided
   ! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
   ! to allow the the model to do any I/O specific cleanup and shutdown, and
   ! then the WRFVAR Driver Layer routine <a
   ! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
   ! directed to shut down here) is called to properly end the run,
   ! including shutting down the communications (for example, most comm
   ! layers would call mpi_finalize at this point if they're using MPI).
   ! 
   !</DESCRIPTION>


   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   ! FIX?
   call da_warning(__FILE__,__LINE__,(/"Fix me"/))
   !   head_grid%start_subtime = head_grid%start_time
   !   head_grid%stop_subtime = head_grid%stop_time

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_esmf_init



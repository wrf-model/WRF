!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
   USE esmf_mod
#ifdef DM_PARALLEL
   USE module_dm
#endif

!<DESCRIPTION>
! Main program of WRF model.  Responsible for starting up the model, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=integrate.html>integrate</a> routine to advance the domain
! to the ending time of the simulation. After the integration is completed, the model
! is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (domain)           :: dummy
   TYPE (grid_config_rec_type)              :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   INTEGER :: rc
   LOGICAL :: input_from_file

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)      :: rstname
   CHARACTER (LEN=80)      :: message

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

!<DESCRIPTION>
! Program_name, a global variable defined in frame/module_domain.F, is
! set, then a routine <a href=init_modules.html>init_modules</a> is
! called. This calls all the init programs that are provided by the
! modules that are linked into WRF.  These include initialization of
! external I/O packages.   Also, some key initializations for
! distributed-memory parallelism occur here if DM_PARALLEL is specified
! in the compile: setting up I/O quilt processes to act as I/O servers
! and dividing up MPI communicators among those as well as initializing
! external communication packages such as RSL or RSL_LITE.
!
!</DESCRIPTION>

   program_name = "WRF V2.0.2 MODEL"

   !  Get the NAMELIST data for input.

   CALL init_modules

!<DESCRIPTION>
! The wrf namelist.input file is read and stored in the USE associated
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
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL initial_config
#endif

!<DESCRIPTION>
! Among the configuration variables read from the namelist is
! debug_level. This is retrieved using get_debug_level (Registry
! generated and defined in frame/module_configure.F).  The value is then
! used to set the debug-print information level for use by <a
! href=wrf_debug.html>wrf_debug</a> throughout the code. Debug_level
! of zero (the default) causes no information to be printed when the
! model runs. The higher the number (up to 1000) the more information is
! printed.
! 
!</DESCRIPTION>

   CALL get_debug_level ( debug_level )
   CALL set_wrf_debug_level ( debug_level )

   ! allocated and configure the mother domain

   NULLIFY( null_domain )

!<DESCRIPTION>
! The top-most domain in the simulation is then allocated and configured
! by calling <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a>.
! Here, in the case of this root domain, the routine is passed the
! globally accessible pointer to TYPE(domain), head_grid, defined in
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
   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

#ifdef DM_PARALLEL
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   CALL Setup_Timekeeping (head_grid)

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

   CALL med_initialdata_input( head_grid , config_flags )

   IF ( config_flags%write_restart_at_0h ) THEN
      CALL med_restart_out ( head_grid, config_flags )
#ifndef AUTODOC_BUILD
! prevent this from showing up before the call to integrate in the autogenerated call tree
      CALL med_shutdown_io ( head_grid , config_flags )
      CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )
      CALL wrf_shutdown
#endif
   END IF

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  The start
! and stop times for the domain are set to the start and stop time of the
! model run, and then <a href=integrate.html>integrate</a> is called to
! advance the domain forward through that specified time interval.  On
! return, the simulation is completed.  A Mediation Layer-provided
! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
! to allow the the model to do any I/O specific cleanup and shutdown, and
! then the WRF Driver Layer routine <a
! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
! directed to shut down here) is called to properly end the run,
! including shutting down the communications (for example, most comm
! layers would call MPI_FINALIZE at this point if they're using MPI).
! 
!</DESCRIPTION>


   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   head_grid%start_subtime = head_grid%start_time
   head_grid%stop_subtime = head_grid%stop_time
   CALL       wrf_debug ( 100 , 'wrf: calling integrate' )
   CALL integrate ( head_grid )
   CALL       wrf_debug ( 100 , 'wrf: back from integrate' )

   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )
   CALL wrf_shutdown

END PROGRAM wrf


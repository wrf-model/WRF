!WRF:DRIVER_LAYER:COMP
!

MODULE module_wrf_comp_super
!<DESCRIPTION>
! This module defines wrf_init(), wrf_run(), and wrf_finalize() routines.  
! Note that module_comp is USEd by module_domain.  
!</DESCRIPTION>

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error

#ifdef DM_PARALLEL
   USE module_dm
#endif

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER                 :: number_at_same_level
   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
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


CONTAINS


   SUBROUTINE wrf_init( gcomp, importState, exportState, clock, rc )
     TYPE(WRF_COMP_GridComp), OPTIONAL, INTENT(INOUT) :: gcomp
     TYPE(WRF_COMP_State),    OPTIONAL, INTENT(INOUT) :: importState, exportState
     TYPE(WRF_COMP_Clock),    OPTIONAL, INTENT(INOUT) :: clock
     INTEGER,             OPTIONAL, INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF init routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals WRF_COMP_SUCCESS if there are no errors,
!                       otherwise WRF_COMP_FAILURE.
!
!     All arguments are optional and will only be present when the ESMF 
!     library is linked in during the WRF build.  
!</DESCRIPTION>

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

   program_name = "WRF V2.0.3.1 MODEL"

   !  Get the NAMELIST data for input.

   CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)
                          ! Phase 1 is called before ESMF starts up

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
! debug_level. This is retrieved using nl_get_debug_level (Registry
! generated and defined in frame/module_configure.F).  The value is then
! used to set the debug-print information level for use by <a
! href=wrf_debug.html>wrf_debug</a> throughout the code. Debug_level
! of zero (the default) causes no information to be printed when the
! model runs. The higher the number (up to 1000) the more information is
! printed.
! 
!</DESCRIPTION>

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   ! allocated and configure the mother domain

   NULLIFY( null_domain )

!<DESCRIPTION>
! RSL is required for WRF nesting options.
! The non-MPI build that allows nesting is only supported on machines
! with the -DSTUBMPI option.  Check to see if the WRF model is being asked 
! for a for a multi-domain run (max_dom > 1, from the namelist).  If so,
! then we check to make sure that we are under the parallel
! run option or we are on an acceptable machine.
!</DESCRIPTION>

   CALL nl_get_max_dom( 1, max_dom )
   IF ( max_dom > 1 ) THEN
#if ( ! defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
   CALL wrf_error_fatal( &
     'nesting requires either an MPI build or use of the -DSTUBMPI option' ) 
#endif
#if ( defined ( MOVE_NESTS ) )
   CALL wrf_message( '                      W A R N I N G                          ' )
   CALL wrf_message( '  MOVING NEST CAPABILITY IS EXPERIMENTAL AND UNSUPPORTED     ' )
   CALL wrf_message( '                 IN THIS VERSION OF WRF                      ' )
   CALL wrf_message( '          U S E   A T   Y O U R   O W N   R I S K            ' )
#endif
   END IF

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

! TBH:  $$$ Unscramble this later...  
! TBH:  $$$ Need to add state to avoid calling wrf_finalize() twice when ESMF 
! TBH:  $$$ library is used.  
   IF ( config_flags%write_restart_at_0h ) THEN
      CALL med_restart_out ( head_grid, config_flags )
#ifndef AUTODOC_BUILD
! prevent this from showing up before the call to integrate in the autogenerated call tree
      CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )
      CALL wrf_finalize( gcomp=gcomp, importState=importState, &
                         exportState=exportState, clock=clock, rc=rc )
#endif
   END IF

   ! Initialize coupling-related objects, if needed.  
   ! $$$ Cannot move this call beneath init_wrfio() at the moment because it 
   ! $$$ depends on config values being already broadcast.   
   ! Calls set_iostate_comp().  
   !$$$ TBH:  Get rid of [s|g]et_iostate_comp() once ESMF_StateGetCurrentImport(), 
   !$$$ TBH:  ESMF_StateGetCurrentExport(), ESMF_GridCompGetCurrent(), and ESMF_ClockGetCurrent() 
   !$$$ TBH:  are supported!  
   CALL wrf_init_coupling( head_grid, gcomp=gcomp, importState=importState, &
                                      exportState=exportState, clock=clock )

!$$$ TBH:  Need to connect head_grid%start_subtime and head_grid%stop_subtime to 
!$$$ TBH:  clock when building with ESMF library...  
   head_grid%start_subtime = head_grid%start_time
   head_grid%stop_subtime = head_grid%stop_time

   ! TBH:  Do we need this here?  And in wrf_finalize() too?  
   ! TBH:  NOTE:  copy-out is not required with the current implementation of 
   ! TBH:  ESMF objects (they contain only a Fortran90 POINTER).  HOWEVER, 
   ! TBH:  ESMF core team advises against relying on this behavior since it 
   ! TBH:  may differ in a future release.  
   !$$$ TBH:  Get rid of [s|g]et_iostate_comp() once ESMF_StateGetCurrentImport(), 
   !$$$ TBH:  ESMF_StateGetCurrentExport(), ESMF_GridCompGetCurrent(), and ESMF_ClockGetCurrent() 
   !$$$ TBH:  are supported!  
   CALL get_iostate_comp( head_grid%id, gcomp=gcomp, importState=importState, &
                                        exportState=exportState, clock=clock )

   ! return success status
   IF ( PRESENT( rc ) ) THEN
     rc = WRF_COMP_SUCCESS
   ENDIF

   END SUBROUTINE wrf_init



   SUBROUTINE wrf_run( gcomp, importState, exportState, clock, rc )
     TYPE(WRF_COMP_GridComp), OPTIONAL, INTENT(INOUT) :: gcomp
     TYPE(WRF_COMP_State),    OPTIONAL, INTENT(INOUT) :: importState, exportState
     TYPE(WRF_COMP_Clock),    OPTIONAL, INTENT(INOUT) :: clock
     INTEGER,             OPTIONAL, INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF run routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals WRF_COMP_SUCCESS if there are no errors,
!                       otherwise WRF_COMP_FAILURE.
!
!     All arguments are optional and will only be present when the ESMF 
!     library is linked in during the WRF build.  
!</DESCRIPTION>

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

   !$$$ TBH:  Probably want to move this call inside integrate() passing gcomp, 
   !$$$ TBH:  importState, exportState, and clock as optional arguments.  For 
   !$$$ TBH:  now, leave as-is (to make the code more explicit) since ESMF 
   !$$$ TBH:  types are only currently available for head_grid.  
   !$$$ TBH:  Get rid of [s|g]et_iostate_comp() once ESMF_StateGetCurrentImport(), 
   !$$$ TBH:  ESMF_StateGetCurrentExport(), ESMF_GridCompGetCurrent(), and ESMF_ClockGetCurrent() 
   !$$$ TBH:  are supported!  
   CALL set_iostate_comp( head_grid%id, gcomp=gcomp, importState=importState, &
                                        exportState=exportState, clock=clock )

   CALL       wrf_debug ( 100 , 'wrf: calling integrate' )
   CALL integrate ( head_grid )
   CALL       wrf_debug ( 100 , 'wrf: back from integrate' )

   !$$$ TBH:  Probably want to move this call inside integrate() passing gcomp, 
   !$$$ TBH:  importState, exportState, and clock as optional arguments.  For 
   !$$$ TBH:  now, leave as-is (to make the code more explicit) since ESMF 
   !$$$ TBH:  types are only currently available for head_grid.  
   !$$$ TBH:  NOTE:  copy-out is not required with the current implementation of 
   !$$$ TBH:  ESMF objects (they contain only a Fortran90 POINTER).  HOWEVER, 
   !$$$ TBH:  ESMF core team advises against relying on this behavior since it 
   !$$$ TBH:  may differ in a future release.  
   !$$$ TBH:  Get rid of [s|g]et_iostate_comp() once ESMF_StateGetCurrentImport(), 
   !$$$ TBH:  ESMF_StateGetCurrentExport(), ESMF_GridCompGetCurrent(), and ESMF_ClockGetCurrent() 
   !$$$ TBH:  are supported!  
   CALL get_iostate_comp( head_grid%id, gcomp=gcomp, importState=importState, &
                                        exportState=exportState, clock=clock )

   ! return success status
   IF ( PRESENT( rc ) ) THEN
     rc = WRF_COMP_SUCCESS
   ENDIF

   END SUBROUTINE wrf_run



   SUBROUTINE wrf_finalize( gcomp, importState, exportState, clock, rc )
     TYPE(WRF_COMP_GridComp), OPTIONAL, INTENT(INOUT) :: gcomp
     TYPE(WRF_COMP_State),    OPTIONAL, INTENT(INOUT) :: importState, exportState
     TYPE(WRF_COMP_Clock),    OPTIONAL, INTENT(INOUT) :: clock
     INTEGER,             OPTIONAL, INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF finalize routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals WRF_COMP_SUCCESS if there are no errors,
!                       otherwise WRF_COMP_FAILURE.
!
!     All arguments are optional and will only be present when the ESMF 
!     library is linked in during the WRF build.  
!</DESCRIPTION>

   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )

   ! Release coupling-related objects, if needed.  
   ! $$$ May move this beneath med_shutdown_io()
   CALL final_iostate_comp( head_grid%id )

   IF ( PRESENT( rc ) ) THEN
     ! If ESMF library is used, then skip wrf_shutdown() and rely on 
     ! WRF_COMP_Finalize() to call MPI_FINALIZE() for DM parallel runs.  
     rc = WRF_COMP_SUCCESS
   ELSE
     ! Otherwise, call wrf_shutdown() (which calls MPI_FINALIZE() 
     ! for DM parallel runs).  
     CALL wrf_shutdown
   ENDIF

   END SUBROUTINE wrf_finalize


   ! WRF-specific initialization required for coupling.  
   ! This is always called but does nothing useful unless linked with the ESMF 
   ! library.  
   SUBROUTINE wrf_init_coupling( grid, gcomp, importState, exportState, clock )
     TYPE(domain), POINTER                            :: grid           ! WRF domain
     TYPE(WRF_COMP_GridComp), OPTIONAL, INTENT(IN   ) :: gcomp
     TYPE(WRF_COMP_State),    OPTIONAL, INTENT(IN   ) :: importState, exportState
     TYPE(WRF_COMP_Clock),    OPTIONAL, INTENT(IN   ) :: clock
     ! Local variables
     INTEGER :: numprocsX, numprocsY
     LOGICAL :: periodicX, periodicY
     INTEGER :: i, j
     INTEGER :: myXcount, myYcount   ! process-local values for mass points only
     INTEGER :: myXend,   myYend     ! process-local values for mass points only
     INTEGER :: globalXcount, globalYcount  ! global values for mass points only
     REAL, POINTER :: coordX(:), coordY(:)
     INTEGER :: ids, ide, jds, jde, kds, kde
     INTEGER :: ims, ime, jms, jme, kms, kme
     INTEGER :: ips, ipe, jps, jpe, kps, kpe

     ! Extract information from WRF domain.  

     ! For distributed-memory parallel runs, get numbers of tasks in each 
     ! decomposed dimension.  This is identical for all WRF domains.  
#ifdef DM_PARALLEL
     CALL nl_get_nproc_x ( 1, numprocsX )
     CALL nl_get_nproc_y ( 1, numprocsY )
#else
     numprocsX = 1
     numprocsY = 1
#endif
     IF ( ( numprocsX < 1 ) .OR. ( numprocsY < 1 ) ) THEN
       CALL wrf_error_fatal( 'bad value for numprocsX or numprocsY' )
     ENDIF

     CALL get_ijk_from_grid ( grid ,                        &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe  )
     ! numbers of points in domain for non-staggered arrays
     globalXcount = (ide-1) - ids + 1
     globalYcount = (jde-1) - jds + 1

     !$$$ HACK:  ESMF does not yet support mercator, polar-stereographic, or 
     !$$$ HACK:  lambert-conformal projections.  Therefore, we're using fake 
     !$$$ HACK:  coordinates here.  This means that WRF will either have to 
     !$$$ HACK:  couple to models that run on the same coorindate such that 
     !$$$ HACK:  grid points are co-located or something else will have to 
     !$$$ HACK:  perform the inter-grid interpolation computations.  Replace 
     !$$$ HACK:  this once ESMF is upgraded to support the above map 
     !$$$ HACK:  projections.  
     CALL wrf_message( 'WARNING:  Using artificial coordinates for ESMF coupling.' )
     CALL wrf_message( 'WARNING:  ESMF coupling interpolation will be incorrect' )
     CALL wrf_message( 'WARNING:  unless grid points in the coupled components' )
     CALL wrf_message( 'WARNING:  are co-located.  This limitation will be removed' )
     CALL wrf_message( 'WARNING:  once ESMF coupling supports common map projections.' )
     ! Note that ESMF defines coordinates at *vertices*
     ALLOCATE( coordX( globalXcount + 1 ), coordY( globalYcount + 1 ) )
     coordX(1) = 0.0
     DO i = 2, globalXcount+1
       coordX(i) = coordX(i-1) + 1.0
     ENDDO
     coordY(1) = 0.0
     DO i = 2, globalYcount+1
       coordY(i) = coordY(i-1) + 1.0
     ENDDO

     ! task-local numbers of points in patch for non-staggered arrays
     myXend = min(ipe, ide-1)
     myYend = min(jpe, jde-1)
     myXcount = myXend - ips + 1
     myYcount = myYend - jps + 1
PRINT *, 'DEBUG:  WRF non-staggered     ips = ', ips
PRINT *, 'DEBUG:  WRF non-staggered     ipe = ', min(ipe, ide-1)
PRINT *, 'DEBUG:  WRF non-staggered i count = ', myXCount
PRINT *, 'DEBUG:  WRF non-staggered     jps = ', jps
PRINT *, 'DEBUG:  WRF non-staggered     jpe = ', min(jpe, jde-1)
PRINT *, 'DEBUG:  WRF non-staggered j count = ', myYCount
PRINT *, 'DEBUG:  WRF     staggered     ips = ', ips
PRINT *, 'DEBUG:  WRF     staggered     ipe = ', ipe
PRINT *, 'DEBUG:  WRF     staggered i count = ', ipe-ips+1
PRINT *, 'DEBUG:  WRF     staggered     jps = ', jps
PRINT *, 'DEBUG:  WRF     staggered     jpe = ', jpe
PRINT *, 'DEBUG:  WRF     staggered j count = ', jpe-jps+1

     ! periodicity
     CALL nl_get_periodic_x ( grid%id, periodicX )
     CALL nl_get_periodic_y ( grid%id, periodicY )

     ! Send WRF initialization information to ESMF for this grid
     CALL init_iostate_comp( grid%id, numprocsX,    numprocsY,    &
                                      periodicX,    periodicY,    &
                                      coordX,       coordY,       &
                                      globalXcount, globalYcount, &
                                      ips,          myXend,       &
                                      jps,          myYend )


     !$$$ TBH:  Get rid of [s|g]et_iostate_comp() once ESMF_StateGetCurrentImport(), 
     !$$$ TBH:  ESMF_StateGetCurrentExport(), ESMF_GridCompGetCurrent(), and ESMF_ClockGetCurrent() 
     !$$$ TBH:  are supported!  
     CALL set_iostate_comp( grid%id, gcomp=gcomp, importState=importState, &
                                     exportState=exportState, clock=clock )

     DEALLOCATE( coordX, coordY )

   END SUBROUTINE wrf_init_coupling


END MODULE module_wrf_comp_super



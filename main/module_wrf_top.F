!WRF:DRIVER_LAYER:TOP
!

!TBH:  $$$  move this to ../frame?  

MODULE module_wrf_top
!<DESCRIPTION>
! This module defines top-level wrf_init(), wrf_run(), and wrf_finalize() 
! routines.  
!</DESCRIPTION>

   USE module_machine
   USE module_domain
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_check_a_mundo

   USE module_timing
   USE module_wrf_error
   USE module_nesting

#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_initialize,wrf_get_hostid,domain_active_this_task,mpi_comm_allcompute
#else
   USE module_dm, ONLY : domain_active_this_task
#endif

   USE module_cpl, ONLY : coupler_on, cpl_finalize, cpl_defdomain

   IMPLICIT NONE

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (domain) , pointer :: parent_grid, new_nest
   LOGICAL                                :: a_nest_was_opened
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER        :: kid, nestid
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

   CHARACTER (LEN=256)     :: rstname
   CHARACTER (LEN=80)      :: message
   CHARACTER (LEN=256) , PRIVATE :: a_message

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping

! #if (EM_CORE == 1)
     SUBROUTINE wrf_dfi_write_initialized_state( )
     END SUBROUTINE wrf_dfi_write_initialized_state
 
     SUBROUTINE wrf_dfi_startfwd_init( )
     END SUBROUTINE wrf_dfi_startfwd_init
     
     SUBROUTINE wrf_dfi_startbck_init( )
     END SUBROUTINE wrf_dfi_startbck_init
     
     SUBROUTINE wrf_dfi_bck_init( )
     END SUBROUTINE wrf_dfi_bck_init
     
     SUBROUTINE wrf_dfi_fwd_init( )
     END SUBROUTINE wrf_dfi_fwd_init
     
     SUBROUTINE wrf_dfi_fst_init( )
     END SUBROUTINE wrf_dfi_fst_init
     
     SUBROUTINE wrf_dfi_array_reset ( )
     END SUBROUTINE wrf_dfi_array_reset
! #endif

     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial

   END INTERFACE


CONTAINS


   SUBROUTINE wrf_init( no_init1 )
!<DESCRIPTION>
!     WRF initialization routine.
!</DESCRIPTION>
#ifdef _OPENMP
     use omp_lib
#endif
#ifdef _ACCEL
     use accel_lib
#endif
     LOGICAL, OPTIONAL, INTENT(IN) :: no_init1
     INTEGER i, myproc, nproc, hostid, loccomm, ierr, buddcounter, mydevice, save_comm
     INTEGER, ALLOCATABLE :: hostids(:), budds(:)
     CHARACTER*512 hostname
     CHARACTER*512 mminlu_loc
#ifdef _ACCEL
     INTEGER :: it, nt, in, devnum
#endif
#if defined(DM_PARALLEL) && !defined(STUBMPI) && ( defined(RUN_ON_GPU) || defined(_ACCEL))
     include "mpif.h"
#endif
#include "version_decl"


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

   program_name = "WRF " // TRIM(release_version) // " MODEL"

   ! Initialize WRF modules:  
   ! Phase 1 returns after MPI_INIT() (if it is called)
   CALL init_modules(1)
   IF ( .NOT. PRESENT( no_init1 ) ) THEN
     ! Initialize utilities (time manager, etc.)
#ifdef NO_LEAP_CALENDAR
     CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_NOLEAP )
#else
     CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN )
#endif
   ENDIF
   ! Phase 2 resumes after MPI_INIT() (if it is called)
   CALL init_modules(2)

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
   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
   CALL wrf_set_dm_communicator( save_comm )
#else
   CALL initial_config
#endif

   CALL set_derived_rconfigs
   CALL check_nml_consistency
   CALL set_physics_rconfigs

#ifdef _ACCEL
   buddcounter = 1
   mydevice = 0
# if defined(DM_PARALLEL) && !defined(STUBMPI) 
   CALL wrf_get_myproc( myproc )
   CALL wrf_get_nproc( nproc )
   CALL wrf_get_hostid ( hostid )
   CALL wrf_get_dm_communicator ( loccomm )

   ALLOCATE( hostids(nproc) )
   ALLOCATE( budds(nproc) )
   CALL mpi_allgather( hostid, 1, MPI_INTEGER, hostids, 1, MPI_INTEGER, loccomm, ierr )
   if ( ierr .NE. 0 ) print * ,'error in mpi_allgather ',ierr
   budds = -1
   buddcounter = 0
   ! mark the ones i am on the same node with
   DO i = 1, nproc
      IF ( hostid .EQ. hostids(i) ) THEN
         budds(i) = buddcounter
         buddcounter = buddcounter + 1
      ENDIF
   ENDDO
   mydevice = budds(myproc+1)
   DEALLOCATE( hostids )
   DEALLOCATE( budds )
# endif
   in = acc_get_num_devices(acc_device_nvidia)
   if (in .le. 0) print *, 'error:  No GPUS present: ',in
# ifdef _OPENMP
   !$OMP PARALLEL SHARED(mydevice,in) PRIVATE(it,nt,devnum)
   it = omp_get_thread_num()
   nt = omp_get_num_threads()
   devnum = mod(mod(mydevice*nt,in) + it, in)
# ifdef _ACCEL_PROF
   print *, "Process, Thread, Device: ",mydevice, it, devnum
# endif
   call acc_set_device_num(devnum, acc_device_nvidia)

   !$OMP END PARALLEL
# else
   it = 0
   nt = 1
   devnum = mod(mod(mydevice*nt,in) + it, in)
#  ifdef _ACCEL_PROF
   print *, "Process, Thread, Device: ",mydevice, it, devnum
#  endif
   call acc_set_device_num(devnum, acc_device_nvidia)
# endif
#endif

#ifdef RUN_ON_GPU
   CALL wrf_get_myproc( myproc )
   CALL wrf_get_nproc( nproc )
# ifdef DM_PARALLEL
   CALL wrf_get_hostid ( hostid ) 
   CALL wrf_get_dm_communicator ( loccomm )
   ALLOCATE( hostids(nproc) )
   ALLOCATE( budds(nproc) )
   CALL mpi_allgather( hostid, 1, MPI_INTEGER, hostids, 1, MPI_INTEGER, loccomm, ierr )
   IF ( ierr .NE. 0 ) THEN
      write(a_message,*)__FILE__,__LINE__,'error in mpi_allgather ',ierr
      CALL wrf_message ( a_message ) 
   END IF
   budds = -1
   buddcounter = 0 
   ! mark the ones i am on the same node with
   DO i = 1, nproc 
      IF ( hostid .EQ. hostids(i) ) THEN
         budds(i) = buddcounter 
         buddcounter = buddcounter + 1
      ENDIF
   ENDDO
   mydevice = budds(myproc+1)
   DEALLOCATE( hostids )
   DEALLOCATE( budds )
# else
   mydevice = 0
# endif
   CALL wsm5_gpu_init( myproc, nproc, mydevice )
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
                               active_this_task = domain_active_this_task(1), &
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
   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_set_dm_communicator( save_comm )
#endif

! #if (EM_CORE == 1)
   ! In case we are doing digital filter initialization, set dfi_stage = DFI_SETUP 
   !   to indicate in Setup_Timekeeping that we want forecast start and
   !   end times at this point 
   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) head_grid%dfi_stage = DFI_SETUP
! #endif

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

 IF ( domain_active_this_task(1) ) THEN
   CALL med_initialdata_input( head_grid , config_flags )

   IF ( config_flags%write_restart_at_0h ) THEN
      CALL med_restart_out ( head_grid, config_flags )
#ifndef AUTODOC_BUILD
! prevent this from showing up before the call to integrate in the autogenerated call tree
      CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )
! TBH:  $$$ Unscramble this later...  
! TBH:  $$$ Need to add state to avoid calling wrf_finalize() twice when ESMF 
! TBH:  $$$ library is used.  Maybe just set clock stop_time=start_time and 
! TBH:  $$$ do not call wrf_finalize here...  
      CALL wrf_finalize( )
#endif
   END IF
  ENDIF  ! domain_active_this_task

   ! set default values for subtimes
   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )

 IF ( domain_active_this_task(1) ) THEN
   !  For EM (but not DA), if this is a DFI run, we can allocate some space.  We are
   !  not allowing anyting tricky for nested DFI.  If there are any nested domains,
   !  they all need to start at the same time.  Otherwise, why even do the DFI?  If
   !  the domains do not all start at the same time, then there will be inconsistencies,
   !  which is what DFI is supposed to address.

#if (EM_CORE == 1)
   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) THEN
      CALL alloc_doms_for_dfi ( head_grid )
   END IF
#endif

   IF (coupler_on) CALL cpl_defdomain( head_grid ) 
  ENDIF  ! domain_active_this_task

   END SUBROUTINE wrf_init



   SUBROUTINE wrf_run( )
!<DESCRIPTION>
!     WRF run routine.
!</DESCRIPTION>

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  The start
! and stop times for the domain are set to the start and stop time of the
! model run, and then <a href=integrate.html>integrate</a> is called to
! advance the domain forward through that specified time interval.  On
! return, the simulation is completed.  
! 
!</DESCRIPTION>

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   CALL       wrf_debug ( 100 , 'wrf: calling integrate' )
   CALL integrate ( head_grid )
   CALL       wrf_debug ( 100 , 'wrf: back from integrate' )

   END SUBROUTINE wrf_run



   SUBROUTINE wrf_finalize( no_shutdown )
!<DESCRIPTION>
!     WRF finalize routine.
!</DESCRIPTION>

!<DESCRIPTION>
! A Mediation Layer-provided
! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
! to allow the the model to do any I/O specific cleanup and shutdown, and
! then the WRF Driver Layer routine <a
! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
! directed to shut down here) is called to properly end the run,
! including shutting down the communications (for example, most comm
! layers would call MPI_FINALIZE at this point if they're using MPI).
! 
!</DESCRIPTION>
     LOGICAL, OPTIONAL, INTENT(IN) :: no_shutdown

   ! shut down I/O
   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )

   ! Call wrf_shutdown() (which calls MPI_FINALIZE() 
   ! for DM parallel runs).  
   IF ( .NOT. PRESENT( no_shutdown ) ) THEN
     ! Finalize time manager
      IF (coupler_on) THEN 
         CALL cpl_finalize() 
      ELSE
         CALL WRFU_Finalize
         CALL wrf_shutdown
      ENDIF
   ENDIF

   END SUBROUTINE wrf_finalize


   SUBROUTINE wrf_dfi()
!<DESCRIPTION>
! Runs a digital filter initialization procedure.
!</DESCRIPTION>
      IMPLICIT NONE

! #if (EM_CORE == 1)
      ! Initialization procedure
      IF ( config_flags%dfi_opt .NE. DFI_NODFI ) THEN
   
         SELECT CASE ( config_flags%dfi_opt ) 
     
            CASE (DFI_DFL)
               wrf_err_message = 'Initializing with DFL'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE (DFI_DDFI)
               wrf_err_message = 'Initializing with DDFI'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Integrating backward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_bck_init()
               CALL wrf_run()
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE (DFI_TDFI)
               wrf_err_message = 'Initializing with TDFI'
               CALL wrf_message(TRIM(wrf_err_message))
   
               wrf_err_message = '   Integrating backward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_bck_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               wrf_err_message = '   Filtering forward in time'
               CALL wrf_message(TRIM(wrf_err_message))
   
               CALL wrf_dfi_fwd_init()
               CALL wrf_run()
   
               CALL wrf_dfi_array_reset()
   
               CALL wrf_dfi_fst_init()
   
               IF ( config_flags%dfi_write_filtered_input ) THEN
                  CALL wrf_dfi_write_initialized_state()
               END IF
   
            CASE DEFAULT
               wrf_err_message = 'Unrecognized DFI_OPT in namelist'
               CALL wrf_error_fatal(TRIM(wrf_err_message))
   
         END SELECT
   
      END IF
! #endif

   END SUBROUTINE wrf_dfi

   SUBROUTINE set_derived_rconfigs
!<DESCRIPTION>
! Some derived rconfig entries need to be set based on the value of other,
! non-derived entries before package-dependent memory allocation takes place.
! This might be employed when, for example, we want to allocate arrays in
! a package that depends on the setting of two or more namelist variables.
! In this subroutine, we do just that.
!</DESCRIPTION>

      IMPLICIT NONE

      INTEGER :: i


! #if (EM_CORE == 1)
      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = model_config_rec % mp_physics(i)
        ENDDO
      END IF
! #endif

#if (EM_CORE == 1)
      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = model_config_rec % bl_pbl_physics(i)
        ENDDO
      END IF
#endif

#if (DA_CORE == 1)
      IF ( model_config_rec % dyn_opt .EQ. 2 ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_4dvar(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_4dvar(i) = model_config_rec % mp_physics(i)
        ENDDO
      END IF
#endif

   END SUBROUTINE set_derived_rconfigs

   RECURSIVE SUBROUTINE alloc_doms_for_dfi ( grid )
   
      !  Input variables.

      TYPE (domain) , pointer :: grid

      !  Local variables.

      TYPE (domain) , pointer :: new_nest_loc
      TYPE (grid_config_rec_type) :: parent_config_flags
      INTEGER :: nestid_loc , kid_loc
   
         !  Are there any subdomains from this level.  The output is the nestid (the domain
         !  ID of the nest), and kid (an index to which of the parent's children this new nested
         !  domain represents).
   
         DO WHILE ( nests_to_open( grid , nestid_loc , kid_loc ) )

            !  If we found another child domain, we continue on: allocate, set up time keeping, 
            !  initialize.
   
            CALL alloc_and_configure_domain ( domain_id  = nestid_loc   , &
                                              grid       = new_nest_loc , &
                                              parent     = grid         , &
                                              kid        = kid_loc        )
         
print *,'for parent domain id #',grid%id,', found child domain #',nestid_loc
            !  Since this is a DFI run, set the DFI switches to the same for all domains.

            new_nest_loc%dfi_opt = head_grid%dfi_opt
            new_nest_loc%dfi_stage = DFI_SETUP
         
            !  Set up time keeping for the fine grid space that was just allocated.

            CALL Setup_Timekeeping (new_nest_loc)

            !  With space allocated, and timers set, the fine grid can be initialized with data.

            CALL model_to_grid_config_rec ( grid%id , model_config_rec , parent_config_flags )
            CALL med_nest_initial ( grid , new_nest_loc , config_flags )

            !  Here's the recursive part.  For each of these child domains, we call this same routine.
            !  This will find all of "new_nest_loc" first generation progeny.
   
            CALL alloc_doms_for_dfi ( new_nest_loc )
   
         END DO
   
   END SUBROUTINE alloc_doms_for_dfi

END MODULE module_wrf_top

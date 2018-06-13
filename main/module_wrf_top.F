!WRF:DRIVER_LAYER:TOP
!

!TBH:  $$$  move this to ../frame?  

MODULE module_wrf_top
!<DESCRIPTION>
! This module defines top-level wrf_init(), wrf_adtl_check(), wrf_run(), and wrf_finalize() 
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
#if ( WRFPLUS == 1 )
   USE module_adtl_grid_utilities, ONLY : allocate_grid, deallocate_grid, copy_grid_to_s, copy_grid_to_b, &
                                          restore_grid, inner_dot_g, add_grid, inner_dot, init_domain_size, &
                                          copy_s_to_g_adjtest, copy_g_to_b_adjtest, inner_dot_g_adjtest, &
                                          copy_g_to_a_adjtest, inner_dot_a_b_adjtest, zero_out_ad, zero_out_tl, &
                                          get_forc_locations, allocate_locations, gen_scenario_matrix, &
                                          spot_force_ad, spot_force_tl, spot_force_nl, &
                                          extract_ad_der, extract_tl_der, extract_nl_den, extract_nl_num
   USE mediation_pertmod_io, ONLY : xtraj_pointer, xtraj_head, xtraj_tail, xtraj_io_initialize, adtl_initialize
   USE module_io_domain, ONLY: close_dataset, wrf_inquire_opened
#if (EM_CORE==1)
   USE module_state_description, ONLY : SURFDRAGSCHEME, PARAM_FIRST_SCALAR, num_moist, num_tracer
#endif
#endif

   USE module_nesting

#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_initialize,wrf_get_hostid,domain_active_this_task,mpi_comm_allcompute
#if ( WRFPLUS == 1 )
   USE module_dm, ONLY : wrf_dm_sum_real
#endif
#else
   USE module_dm, ONLY : domain_active_this_task
#endif

#if ( WRFPLUS == 1 )
   USE module_date_time, ONLY : current_date, start_date
   USE module_io_domain, ONLY : open_w_dataset, output_boundary
#endif

   USE module_cpl, ONLY : coupler_on, cpl_finalize, cpl_defdomain

   IMPLICIT NONE

#if ( WRFPLUS == 1 )
#include "wrf_io_flags.h"
#endif

   REAL    :: time

   INTEGER :: loop , &
              levels_to_process

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
#if ( WRFPLUS == 1 )
   TYPE (domain) , POINTER :: grid
#endif
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
#if ( WRFPLUS == 1 )
   INTEGER                 :: alarmid
   LOGICAL                 :: gradient_out = .FALSE.
#endif
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

   CALL setup_physics_suite
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

#if ( WRFPLUS == 1 )
!--------------AD/TL code begin---------------------------------------
   SUBROUTINE wrf_adtl_check( )

   IF ( config_flags%scenario_type .EQ. 0 ) THEN
      CALL wrf_adtl_check_sum
   ELSE
      CALL wrf_adtl_check_spot
   END IF

   END SUBROUTINE wrf_adtl_check


   SUBROUTINE wrf_adtl_check_sum( )
!<DESCRIPTION>
!     WRF adjoint and tangent linear code check routine.
!</DESCRIPTION>

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  
! 
!</DESCRIPTION>

   REAL :: alpha_m, factor, val_l, val_a, save_l, val_n, coef
   INTEGER :: nt, ij, time_step, rc
   CHARACTER*256 :: timestr

   ! Return immediately if not dyn_em_check
   IF ( config_flags%dyn_opt .NE. dyn_em_check ) RETURN

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   ! Force to read the lateral boundary condition here.
   CALL med_before_solve_io ( head_grid, config_flags )

   ! Close boundary file, as it will be read again from start later
   CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )

   CALL init_domain_size ( head_grid, config_flags )

   ! Release linked list for trajectory
   call xtraj_io_initialize

IF ( config_flags%check_TL .or. config_flags%check_AD ) THEN

   ! Save the initial condition and boundary condition, x
   CALL allocate_grid ( )

   CALL copy_grid_to_s ( head_grid , &
                         head_grid%i_start(1), head_grid%i_end(1),                &
                         head_grid%j_start(1), head_grid%j_end(1)                 )

   CALL       wrf_message ( "wrf: calling nonlinear integrate" )
   model_config_rec%dyn_opt = dyn_em

   ! Set up basic states output
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, time_step )
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxhist6 ( head_grid%id, 1 )
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
   CALL Setup_Timekeeping ( head_grid )

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )
   IF ( config_flags%check_TL ) THEN
      IF ( config_flags%mp_physics .NE. 0 .and. config_flags%mp_physics .NE. 99 .and. &
           config_flags%mp_physics .NE. 98 ) CALL nl_set_mp_physics (head_grid%id, config_flags%mp_physics_ad)
      IF ( config_flags%bl_pbl_physics .GT. 0 ) CALL nl_set_bl_pbl_physics (head_grid%id, 98)
      IF ( config_flags%cu_physics .GT. 0 ) THEN
         CALL nl_set_cu_physics (head_grid%id, 98)
         head_grid%cudt = 0
      ENDIF
      CALL nl_set_ra_lw_physics (head_grid%id, 0)
      CALL nl_set_ra_sw_physics (head_grid%id, 0)
      CALL nl_set_sf_sfclay_physics (head_grid%id, 0)

      ! Force to turn off boundary input as we can only perturb the initial boundary condition.
      CALL nl_set_io_form_boundary( head_grid%id, 0 )
   ENDIF

   CALL wrf_run

   ! Turn off basic states output
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 0 )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, 0 )
   if ( .not. head_grid%trajectory_io ) CALL nl_set_inputout_interval_s ( head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL       wrf_message ( "wrf: back from nonlinear integrate" )

   IF ( config_flags%check_TL ) THEN

   wrf_err_message = '=============== Tangent Linear Check ==================='
   CALL wrf_message(TRIM(wrf_err_message))
   
   wrf_err_message = 'check==== U === V === W == PH === T == MU == MOIST == TRACER ==='
   CALL wrf_message(TRIM(wrf_err_message))

   WRITE(wrf_err_message,'(A,8(1x,L5))') 'check',head_grid%check_u, head_grid%check_v,  &
                               head_grid%check_w, head_grid%check_ph, &
                               head_grid%check_t, head_grid%check_mu, &
                               head_grid%check_moist, head_grid%check_tracer
   CALL wrf_message(TRIM(wrf_err_message))

   ! Save the f(x)
   CALL copy_grid_to_b ( head_grid )

   ! Restore the x and assign the \delta x
   CALL restore_grid ( head_grid )
   CALL copy_s_to_g_adjtest ( head_grid, 1.0 )

   ! Set up basic states reading
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
   CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   CALL wrf_run_tl

   ! Turn off auxinput6 reading
   CALL nl_set_auxinput6_interval_s (head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   save_l = 0.0
   ! Calculate the inner dot.
   !$OMP PARALLEL DO    &
   !$OMP DEFAULT (SHARED) PRIVATE ( ij ) &           
   !$OMP REDUCTION (+:save_l) 
   DO ij = 1 , head_grid%num_tiles
     CALL inner_dot_g ( head_grid , save_l,                                        &
                        head_grid%i_start(ij), head_grid%i_end(ij),                &
                        head_grid%j_start(ij), head_grid%j_end(ij)                 )
   END DO
   !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
   save_l = wrf_dm_sum_real ( save_l )
#endif

   alpha_m = 1.
   tangentLinearCheck : DO nt = 1 , 11

      alpha_m = 0.1 * alpha_m
      factor = 1.0 + alpha_m

      CALL add_grid ( head_grid,  factor )

      CALL       wrf_message ( "wrf: calling nonlinear integrate" )
      model_config_rec%dyn_opt = dyn_em
      CALL domain_clock_get( head_grid, start_timestr=timestr )
      CALL domain_clock_set( head_grid, current_timestr=timestr )

      ! Force to turn off history output in this case
      CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )
      ! Force to turn off boundary input as it was perturbated in add_grid.
      CALL nl_set_io_form_boundary( head_grid%id, 0 )

      head_grid%dtbc = 0
      head_grid%itimestep = 0
      CALL wrf_run
      CALL       wrf_message ( "wrf: back from nonlinear integrate" )

      val_n = 0.0
      !$OMP PARALLEL DO    &
      !$OMP PRIVATE ( ij ) &
      !$OMP REDUCTION (+:val_n)
      DO ij = 1 , head_grid%num_tiles
        CALL inner_dot ( head_grid, val_n,                                          &
                         head_grid%i_start(ij), head_grid%i_end(ij),                &
                         head_grid%j_start(ij), head_grid%j_end(ij)                 )
      END DO
      !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
      val_n = wrf_dm_sum_real ( val_n )
#endif
      val_l=save_l*alpha_m**2
      coef=val_n/val_l
      WRITE(wrf_err_message, FMT='(A,E9.4,A,E23.14,A,E14.7,A,E14.7)') &
           'tl_check: alpha_m=',alpha_m,'  coef=',coef, &
           '  val_n=',val_n,'  val_l=',val_l
      CALL wrf_message(TRIM(wrf_err_message))

   ENDDO  tangentLinearCheck 

   END IF !end of Tangent linear check

   IF ( config_flags%check_AD ) THEN

   wrf_err_message = '==================== Adjoint check ====================='
   CALL wrf_message(TRIM(wrf_err_message))

   wrf_err_message = 'check==== U === V === W == PH === T == MU == MOIST == TRACER ==='
   CALL wrf_message(TRIM(wrf_err_message))

   WRITE(wrf_err_message,'(A,8(1x,L5))') 'check',head_grid%check_u, head_grid%check_v,  &
                               head_grid%check_w, head_grid%check_ph, &
                               head_grid%check_t, head_grid%check_mu, &
                               head_grid%check_moist, head_grid%check_tracer

   CALL wrf_message(TRIM(wrf_err_message))

   CALL restore_grid ( head_grid )
   CALL copy_s_to_g_adjtest ( head_grid, 0.01 )

   CALL copy_g_to_b_adjtest ( head_grid )

   ! Set up basic states reading
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
   CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   IF ( config_flags%mp_physics .NE. 0 .and. config_flags%mp_physics .NE. 99 .and. &
        config_flags%mp_physics .NE. 98 ) CALL nl_set_mp_physics (head_grid%id, config_flags%mp_physics_ad)
   IF ( config_flags%bl_pbl_physics .GT. 0 ) CALL nl_set_bl_pbl_physics (head_grid%id, 98)
   IF ( config_flags%cu_physics .GT. 0 ) THEN
      CALL nl_set_cu_physics (head_grid%id, 98)
      head_grid%cudt = 0
   ENDIF
   CALL nl_set_ra_lw_physics (head_grid%id, 0)
   CALL nl_set_ra_sw_physics (head_grid%id, 0)
   CALL nl_set_sf_sfclay_physics (head_grid%id, 0)

   CALL wrf_run_tl

   ! Turn off auxinput6 reading
   CALL nl_set_auxinput6_interval_s (head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   val_l = 0.0
   !$OMP PARALLEL DO    &
   !$OMP PRIVATE ( ij ) &
   !$OMP REDUCTION (+:val_l)
   DO ij = 1 , head_grid%num_tiles
     CALL inner_dot_g_adjtest ( head_grid , val_l,                                         &
                        head_grid%i_start(ij), head_grid%i_end(ij),                &
                        head_grid%j_start(ij), head_grid%j_end(ij)                 )
   END DO
   !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
   val_l =  wrf_dm_sum_real ( val_l )
#endif

!  CALL restore_grid ( head_grid )
   !$OMP PARALLEL DO    &
   !$OMP DEFAULT (SHARED) PRIVATE ( ij )
   DO ij = 1 , head_grid%num_tiles
      CALL copy_g_to_a_adjtest ( head_grid,                                           &
                        head_grid%i_start(ij), head_grid%i_end(ij),                &
                        head_grid%j_start(ij), head_grid%j_end(ij)                 )
   END DO
   !$OMP END PARALLEL DO

   ! Set the file names and interval for reading basic states.
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
   CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   CALL wrf_run_ad

   ! Turn off auxinput6 reading
   CALL nl_set_auxinput6_interval_s (head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   val_a = 0.0
   !$OMP PARALLEL DO    &
   !$OMP PRIVATE ( ij ) &
   !$OMP REDUCTION (+:val_a)
   DO ij = 1 , head_grid%num_tiles
     CALL inner_dot_a_b_adjtest ( head_grid, val_a,                                  &
                          head_grid%i_start(ij), head_grid%i_end(ij),                &
                          head_grid%j_start(ij), head_grid%j_end(ij)                 )
   END DO
   !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
   val_a =  wrf_dm_sum_real ( val_a )
#endif

   WRITE(wrf_err_message, FMT='(A,E23.14)') 'ad_check: VAL_TL: ', val_l
   CALL wrf_message(TRIM(wrf_err_message))
   WRITE(wrf_err_message, FMT='(A,E23.14)') 'ad_check: VAL_AD: ', val_a
   CALL wrf_message(TRIM(wrf_err_message))

   END IF !ADJ test

   ! Deallocate all working space
   CALL deallocate_grid()

ENDIF

   ! Release linked list for trajectory
   call xtraj_io_initialize

   ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.
   CALL wrf_finalize
   STOP
   END SUBROUTINE wrf_adtl_check_sum

   SUBROUTINE wrf_adtl_check_spot( )
!<DESCRIPTION>
!     WRF adjoint and tangent linear code check routine.
!</DESCRIPTION>

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  
! 
!</DESCRIPTION>

   REAL :: alpha_m, val_l, val_a, save_l, val_n, coef, nl_num, nl_den
   REAL, DIMENSION(:,:), ALLOCATABLE :: factors
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: ad_derivative, tl_derivative
   REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: nl_derivative
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: locations_f, locations_i, scenario_matrix
   INTEGER :: ni, nf, nsc, nvar, ij, time_step, rc, &
              ninverse, nforward, firatio, iter, check_type, psign, &
              iloc_f, jloc_f, kloc_f, iloc_i, jloc_i, kloc_i, &
              vars_count, nnumer, ndenom
   CHARACTER*10, DIMENSION(:), ALLOCATABLE :: check_name !large for expectation of many future chem variables
   CHARACTER*256 :: timestr

   ! Return immediately if not dyn_em_check
   IF ( config_flags%dyn_opt .NE. dyn_em_check ) RETURN

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   ! Force to read the lateral boundary condition here.
   CALL med_before_solve_io ( head_grid, config_flags )

   ! Close boundary file, as it will be read again from start later
   CALL close_dataset ( head_grid%lbc_fid , config_flags , &
                                            "DATASET=BOUNDARY" )

   CALL init_domain_size ( head_grid, config_flags )

   ! Release linked list for trajectory
   call xtraj_io_initialize

IF ( config_flags%check_TL .or. config_flags%check_AD ) THEN

   ! Save the initial condition and boundary condition, x
   CALL allocate_grid ( )

   !$OMP PARALLEL DO    &
   !$OMP DEFAULT (SHARED) PRIVATE ( ij ) &           
   DO ij = 1 , head_grid%num_tiles
      CALL copy_grid_to_s ( head_grid , &
                            head_grid%i_start(ij), head_grid%i_end(ij),                &
                            head_grid%j_start(ij), head_grid%j_end(ij)                 )
   ENDDO

   CALL       wrf_message ( "wrf: calling nonlinear integrate" )
   model_config_rec%dyn_opt = dyn_em

   ! Set up basic states output
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, time_step )
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxhist6 ( head_grid%id, 1 )
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
   CALL Setup_Timekeeping ( head_grid )

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )
   IF ( config_flags%mp_physics .NE. 0 .and. config_flags%mp_physics .NE. 99 .and. &
        config_flags%mp_physics .NE. 98 ) CALL nl_set_mp_physics (head_grid%id, config_flags%mp_physics_ad)
   IF ( config_flags%bl_pbl_physics .GT. 0 ) CALL nl_set_bl_pbl_physics (head_grid%id, 98)
   IF ( config_flags%cu_physics .GT. 0 ) THEN
      CALL nl_set_cu_physics (head_grid%id, 98)
      head_grid%cudt = 0
   ENDIF
   CALL nl_set_ra_lw_physics (head_grid%id, 0)
   CALL nl_set_ra_sw_physics (head_grid%id, 0)
   CALL nl_set_sf_sfclay_physics (head_grid%id, 0)

   ! Force to turn off boundary input as we can only perturb the initial boundary condition.
   CALL nl_set_io_form_boundary( head_grid%id, 0 )

   CALL wrf_run

   ! Turn off basic states output
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 0 )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, 0 )
   if ( .not. head_grid%trajectory_io ) CALL nl_set_inputout_interval_s ( head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL       wrf_message ( "wrf: back from nonlinear integrate" )

   CALL copy_grid_to_b ( head_grid )

   wrf_err_message = '============== Adjoint - Tangent Linear Check ==============='
   CALL wrf_message(TRIM(wrf_err_message))

   ! Get the locations of adjoint and tangent linear forcings
   CALL allocate_locations( 'locations_f', locations_f, ninverse, ierr)
   IF( ierr .GT. 0) THEN
      CALL wrf_error_fatal( 'adtl_check: error opening locations_f for reading' )
   ENDIF
   CALL allocate_locations( 'locations_i', locations_i, nforward, ierr)
   IF( ierr .GT. 0) THEN
      CALL wrf_error_fatal(&
                   'adtl_check: error opening locations_i for reading' )
   ENDIF

   firatio = nforward/ninverse
   ierr = 0
   CALL get_forc_locations( 'locations_f', locations_f, ninverse, ierr)
   CALL get_forc_locations( 'locations_i', locations_i, nforward, ierr)



   vars_count = 6 
   DO nsc = 1, num_moist
      vars_count = vars_count + 1
   ENDDO
   DO nsc = 1, num_tracer
      vars_count = vars_count + 1
   ENDDO
   ALLOCATE(factors(vars_count,2))
   ALLOCATE(scenario_matrix(vars_count,vars_count))
   scenario_matrix = 0
   CALL gen_scenario_matrix(config_flags, scenario_matrix, vars_count, &
                            model_config_rec%numer_vars, model_config_rec%denom_vars)

   ALLOCATE(check_name(vars_count))
   WRITE(check_name(1),FMT='(A)') 'U'
   WRITE(check_name(2),FMT='(A)') 'V'
   WRITE(check_name(3),FMT='(A)') 'W'
   WRITE(check_name(4),FMT='(A)') 'T'
   WRITE(check_name(5),FMT='(A)') 'PH'
   WRITE(check_name(6),FMT='(A)') 'MU'
   DO nsc = 1, num_moist
      IF( nsc .LT. PARAM_FIRST_SCALAR ) THEN
         WRITE(check_name(6+nsc),FMT='(A)')'DUMMY'
      ELSE
         WRITE(check_name(6+nsc),FMT='(A,I2.1)')'MOIST',nsc-PARAM_FIRST_SCALAR+1
      ENDIF
   ENDDO
   DO nsc = 1, num_tracer
      IF( nsc .LT. PARAM_FIRST_SCALAR ) THEN
         WRITE(check_name(6+nsc+num_moist),FMT='(A)')'DUMMY' 
      ELSE
         WRITE(check_name(6+nsc+num_moist),FMT='(A,I2.1)')'TRACER',nsc-PARAM_FIRST_SCALAR+1
      ENDIF
   ENDDO  

   CALL wrf_message("AVAILABLE CONTROL VARIABLES FOR VALIDATION")
   DO nvar = 1, vars_count
      WRITE(wrf_err_message, FMT='(I2,A,A)') nvar,'  ',check_name(nvar)
      CALL wrf_message(TRIM(wrf_err_message)) 
   ENDDO

   CALL wrf_message("")
   CALL wrf_message("SELECTED CONTROL VARIABLES")
   DO nnumer = 0, vars_count      
      WRITE(wrf_err_message, FMT='(I2,A)') nnumer, ' ,  '
      DO ndenom = 1, vars_count
         IF ( nnumer .EQ. 0 ) THEN
            WRITE(wrf_err_message, FMT='(A,I2,A)') TRIM(wrf_err_message),&
                                ndenom, ' ,  '
         ELSE
            WRITE(wrf_err_message, FMT='(A,I2,A)') TRIM(wrf_err_message),&
                                scenario_matrix(nnumer,ndenom), ' ,  '
         ENDIF
      ENDDO
      CALL wrf_message(TRIM(wrf_err_message))
   ENDDO

   ALLOCATE(ad_derivative(nforward,vars_count,vars_count))
   ALLOCATE(tl_derivative(nforward,vars_count,vars_count))
   ALLOCATE(nl_derivative(nforward,3,vars_count,vars_count))
   
   ad_derivative = 0.0
   tl_derivative = 0.0
   nl_derivative = 0.0

   !Find all the adjoint sensitivities
   numer_vars_Loop1: DO nnumer = 1, vars_count

      IF( ALL(scenario_matrix(nnumer,:) .EQ. 0) ) CYCLE numer_vars_Loop1
      factors(:,1) = 0.0
      factors(nnumer,1) = 1.0
   
   
      iter = 0
   
      DO nf = 1, ninverse
         iloc_f = locations_f(nf,1)
         jloc_f = locations_f(nf,2)
         kloc_f = locations_f(nf,3)

         CALL zero_out_ad( head_grid )
         !$OMP PARALLEL DO    &
         !$OMP PRIVATE ( ij )
         adforce: DO ij = 1 , head_grid%num_tiles
            IF( head_grid%i_start(ij) .le. iloc_f .AND. &
                head_grid%i_end(ij)  .ge. iloc_f .AND. &
                head_grid%j_start(ij) .le. jloc_f .AND. &
                head_grid%j_end(ij)  .ge. jloc_f ) THEN
                CALL spot_force_ad( head_grid, iloc_f, jloc_f , kloc_f, factors(:,1), vars_count )   
                exit adforce
            ENDIF
         ENDDO adforce
         !$OMP END PARALLEL DO

         ! Set the file names and interval for reading basic states.
         model_config_rec%auxinput6_inname = &
                                          "auxhist6_d<domain>_<date>"
         CALL nl_get_time_step ( head_grid%id, time_step )
         CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
         CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
         CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )
    
         CALL wrf_run_ad
         
   
         ! Turn off auxinput6 reading
         CALL nl_set_auxinput6_interval_s (head_grid%id, 0 )
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
         CALL Setup_Timekeeping ( head_grid )
   
         DO ni = 1,firatio
            iter = iter + 1
            iloc_i = locations_i(iter,1)
            jloc_i = locations_i(iter,2)
            kloc_i = locations_i(iter,3)
   
            denom_vars_Loop1: DO ndenom = 1,vars_count
               IF ( scenario_matrix(nnumer,ndenom) .EQ. 0) CYCLE denom_vars_Loop1
               factors(:,2)=0.0
               factors(ndenom,2) = 1.0
               WRITE(wrf_err_message, FMT='(2(A,I3),5(A))') 'nf = ',nf, &
                            ',  ni = ',ni, ',  check_type = d[', &
                            TRIM(check_name(nnumer)),']/d[',TRIM(check_name(ndenom)),']'
               CALL wrf_message(TRIM(wrf_err_message))
  
               CALL wrf_message("Extracting the AD sensitivity")
               !Store the AD derivative
               !$OMP PARALLEL DO    &
               !$OMP PRIVATE ( ij ) &
               !$OMP REDUCTION (+: ad_derivative(iter,nnumer,ndenom)) 
               adextract: DO ij = 1 , head_grid%num_tiles
                  IF( head_grid%i_start(ij) .le. iloc_i .AND. &
                     head_grid%i_end(ij)  .ge. iloc_i .AND. &
                     head_grid%j_start(ij) .le. jloc_i .AND. &
                     head_grid%j_end(ij)  .ge. jloc_i ) THEN
                     CALL extract_ad_der( head_grid, ad_derivative(iter,nnumer,ndenom), iloc_i, jloc_i, kloc_i, factors (:,2), vars_count)
                     exit adextract
                  ENDIF
               END DO adextract
               !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
               ad_derivative(iter,nnumer,ndenom) = wrf_dm_sum_real ( ad_derivative(iter,nnumer,ndenom) )
#endif

            ENDDO denom_vars_Loop1
         ENDDO      
      ENDDO
   ENDDO numer_vars_Loop1


   !Find all the nonlinear and TL sensitivities   
   iter = 0
   DO nf = 1, ninverse
      iloc_f = locations_f(nf,1)
      jloc_f = locations_f(nf,2)
      kloc_f = locations_f(nf,3)  
      DO ni = 1,firatio
         iter = iter + 1
         iloc_i = locations_i(iter,1)
         jloc_i = locations_i(iter,2)
         kloc_i = locations_i(iter,3)

         denom_vars_Loop2: DO ndenom = 1,vars_count
            IF ( ALL(scenario_matrix(:,ndenom) .EQ. 0) ) CYCLE denom_vars_Loop2
            factors(:,2)=0.0
            factors(ndenom,2) = 1.0
  
            ! Do Finite Difference test of nonlinear model
            IF ( head_grid%check_AD .AND. head_grid%check_TL ) THEN
               nonlinearLoop: DO psign=-1,1,2
                  CALL restore_grid ( head_grid )

                  !$OMP PARALLEL DO    &
                  !$OMP PRIVATE ( ij )
                  nlforce: DO ij = 1 , head_grid%num_tiles
                     IF( head_grid%i_start(ij) .le. iloc_i .AND. &
                         head_grid%i_end(ij)  .ge. iloc_i .AND. &
                         head_grid%j_start(ij) .le. jloc_i .AND. &
                         head_grid%j_end(ij)  .ge. jloc_i ) THEN 
                         CALL spot_force_nl( head_grid, iloc_i, jloc_i, kloc_i, &
                                             factors(:,2), vars_count, REAL(psign) * config_flags%nl_pert)
                         exit nlforce 
                     ENDIF
                  ENDDO nlforce
                  !$OMP END PARALLEL DO
  
                  CALL wrf_message ( "wrf: calling nonlinear integrate" )
                  model_config_rec%dyn_opt = dyn_em
                  CALL domain_clock_get( head_grid, start_timestr=timestr )
                  CALL domain_clock_set( head_grid, current_timestr=timestr )
   
                  ! Force to turn off history output in this case
                  CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )
                  ! Force to turn off boundary input as it was perturbated in add_grid.
                  CALL nl_set_io_form_boundary( head_grid%id, 0 )
   
                  head_grid%dtbc = 0
                  head_grid%itimestep = 0
                  CALL wrf_run
                  CALL wrf_message( "wrf: back from nonlinear integrate" ) 
                  numer_vars_Loop2: DO nnumer = 1, vars_count
                     nl_num = 0.0
                     nl_den = 0.0

                     IF( scenario_matrix(nnumer,ndenom) .EQ. 0 ) CYCLE numer_vars_Loop2
                     factors(:,1) = 0.0
                     factors(nnumer,1) = 1.0   
                     WRITE(wrf_err_message, FMT='(2(A,I3),5(A))')'nf = ',nf, &
                                  ',  ni = ',ni, ',  check_type = d[', &
                                  TRIM(check_name(nnumer)),']/d[',TRIM(check_name(ndenom)),']'
                     CALL wrf_message(TRIM(wrf_err_message))

                     CALL wrf_message("Extracting the FD sensitivity")
                     !Store the NL derivative
                     !$OMP PARALLEL DO    &
                     !$OMP PRIVATE ( ij ) &
                     !$OMP REDUCTION (+: nl_num) 
                     numextract: DO ij = 1 , head_grid%num_tiles
                        IF( head_grid%i_start(ij) .le. iloc_f .AND. &
                           head_grid%i_end(ij)  .ge. iloc_f .AND. &
                           head_grid%j_start(ij) .le. jloc_f .AND. &
                           head_grid%j_end(ij)  .ge. jloc_f ) THEN
                           CALL extract_nl_num( head_grid, nl_num, &
                                                iloc_f, jloc_f, kloc_f, &
                                                factors(:,1), vars_count)
                           exit numextract
                        ENDIF
                     END DO numextract
                     !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
                     nl_num = wrf_dm_sum_real ( nl_num )
#endif

                     !$OMP PARALLEL DO    &
                     !$OMP PRIVATE ( ij ) &
                     !$OMP REDUCTION (+: nl_den) 
                     denextract: DO ij = 1 , head_grid%num_tiles
                        IF( head_grid%i_start(ij) .le. iloc_i .AND. &
                           head_grid%i_end(ij)  .ge. iloc_i .AND. &
                           head_grid%j_start(ij) .le. jloc_i .AND. &
                           head_grid%j_end(ij)  .ge. jloc_i ) THEN
                           CALL extract_nl_den( nl_den, &
                                                iloc_i, jloc_i, kloc_i, &
                                                factors(:,2), vars_count, REAL(psign)*config_flags%nl_pert)
                           exit denextract
                        ENDIF
                     END DO denextract
                     !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
                     nl_den = wrf_dm_sum_real ( nl_den )
#endif

                     nl_derivative(iter,2+psign,nnumer,ndenom) = nl_num / nl_den
                  ENDDO numer_vars_Loop2

               ENDDO nonlinearLoop
               nl_derivative(iter,2,nnumer,ndenom) = (nl_derivative(iter,1,nnumer,ndenom) + nl_derivative(iter,3,nnumer,ndenom)) * 0.5
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
            CALL Setup_Timekeeping ( head_grid )
  
            !Set the single forcing value for each tangent linear run
            CALL zero_out_tl( head_grid )

            !$OMP PARALLEL DO    &
            !$OMP PRIVATE ( ij )
            tlforce: DO ij = 1 , head_grid%num_tiles
               IF( head_grid%i_start(ij) .le. iloc_i .AND. &
                  head_grid%i_end(ij)  .ge. iloc_i .AND. &
                  head_grid%j_start(ij) .le. jloc_i .AND. &
                  head_grid%j_end(ij)  .ge. jloc_i ) THEN
                  CALL spot_force_tl( head_grid, iloc_i, jloc_i, kloc_i, factors(:,2), vars_count) 
                  exit tlforce
               ENDIF
            ENDDO tlforce
            !$OMP END PARALLEL DO

            ! Set up basic states reading
            model_config_rec%auxinput6_inname = &
                                       "auxhist6_d<domain>_<date>"
            CALL nl_get_time_step ( head_grid%id, time_step )
            CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
            CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
            CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )
      
            CALL wrf_run_tl

            ! Turn off auxinput6 reading
            CALL nl_set_auxinput6_interval_s (head_grid%id, 0 )
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
            CALL Setup_Timekeeping ( head_grid )
            numer_vars_Loop3: DO nnumer = 1, vars_count
               IF( scenario_matrix(nnumer,ndenom) .EQ. 0 ) CYCLE numer_vars_Loop3
               factors(:,1) = 0.0
               factors(nnumer,1) = 1.0   
               WRITE(wrf_err_message, FMT='(2(A,I3),5(A))') 'nf = ',nf, &
                            ',  ni = ',ni, ',  check_type = d[', &
                            TRIM(check_name(nnumer)),']/d[',TRIM(check_name(ndenom)),']'
               CALL wrf_message(TRIM(wrf_err_message))
 
               CALL wrf_message("Extracting the TL sensitivity")    
               !Store the TL derivative
               !$OMP PARALLEL DO    &
               !$OMP PRIVATE ( ij ) &
               !$OMP REDUCTION (+: tl_derivative(iter,nnumer,ndenom))
               tlextract: DO ij = 1 , head_grid%num_tiles
                  IF( head_grid%i_start(ij) .le. iloc_f .AND. &
                     head_grid%i_end(ij)  .ge. iloc_f .AND. &
                     head_grid%j_start(ij) .le. jloc_f .AND. &
                     head_grid%j_end(ij)  .ge. jloc_f ) THEN
                     CALL extract_tl_der( head_grid, tl_derivative(iter,nnumer,ndenom), iloc_f, jloc_f, kloc_f, factors(:,1), vars_count)
                     exit tlextract
                  ENDIF
               END DO tlextract
               !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
               tl_derivative(iter,nnumer,ndenom) = wrf_dm_sum_real ( tl_derivative(iter,nnumer,ndenom) )
#endif
            ENDDO numer_vars_Loop3
         ENDDO denom_vars_Loop2
      ENDDO      
   ENDDO

   ! Print out the sensitivities
   CALL wrf_message(&
        "====================== Results =======================")
   numer_vars_Loop4: DO nnumer = 1, vars_count
      IF( ALL(scenario_matrix(nnumer,:) .EQ. 0) ) CYCLE numer_vars_Loop4
      iter = 0
      DO nf = 1, ninverse
         DO ni = 1, firatio
            iter = iter+1
            iloc_i = locations_i(iter,1)
            jloc_i = locations_i(iter,2)
            kloc_i = locations_i(iter,3)
            iloc_f = locations_f(nf,1)
            jloc_f = locations_f(nf,2)
            kloc_f = locations_f(nf,3)

            WRITE(wrf_err_message, FMT='(A,6(1x,I8))')&
                         'REPORT:',&
                         iloc_f,&
                         jloc_f,&
                         kloc_f,&
                         iloc_i,&
                         jloc_i,&
                         kloc_i
            CALL wrf_message(TRIM(wrf_err_message))
         ENDDO
      ENDDO

      DO ndenom = 1, vars_count
         IF( scenario_matrix(nnumer,ndenom) .EQ. 0) CYCLE
         WRITE(wrf_err_message, FMT='(5(A))') 'REPORT: ------check_type = d[',&
                           TRIM(check_name(nnumer)),']/d[',TRIM(check_name(ndenom)),']------'
         CALL wrf_message(TRIM(wrf_err_message))

         IF ( head_grid%check_TL .AND. head_grid%check_AD ) THEN
            WRITE(wrf_err_message, FMT='(A,4(1x,A17))') 'REPORT:','TL','AD','NL_BD','NL_FD'
            CALL wrf_message(TRIM(wrf_err_message))

            iter = 0
            DO nf = 1, ninverse
               DO ni = 1, firatio
                  iter = iter+1
   
                  WRITE(wrf_err_message, FMT='(A,4(E18.9))')&
                               'REPORT:',&
                               tl_derivative(iter,nnumer,ndenom), &
                               ad_derivative(iter,nnumer,ndenom), &
                               nl_derivative(iter,1,nnumer,ndenom), &
                               nl_derivative(iter,3,nnumer,ndenom)
                  CALL wrf_message(TRIM(wrf_err_message))
               ENDDO
            ENDDO
         ELSE
            WRITE(wrf_err_message, FMT='(A,2(1x,A17))') 'REPORT:  ','TL','AD'
            CALL wrf_message(TRIM(wrf_err_message))

            iter = 0
            DO nf = 1, ninverse
               DO ni = 1, firatio
                  iter = iter+1
                  WRITE(wrf_err_message, FMT='(A,2(E18.9))')&
                               'REPORT:',&
                               tl_derivative(iter,nnumer,ndenom), &
                               ad_derivative(iter,nnumer,ndenom)
                  CALL wrf_message(TRIM(wrf_err_message))
               ENDDO
            ENDDO
         ENDIF        
      ENDDO   
   ENDDO numer_vars_Loop4

   DEALLOCATE(ad_derivative)
   DEALLOCATE(tl_derivative)
   DEALLOCATE(nl_derivative)
  
   DEALLOCATE(check_name)
   DEALLOCATE(scenario_matrix)
   DEALLOCATE(factors)
   DEALLOCATE(locations_f)
   DEALLOCATE(locations_i)

   ! Deallocate all working space
   CALL deallocate_grid()

ENDIF

   ! Release linked list for trajectory
   call xtraj_io_initialize

   ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.
   CALL wrf_finalize
   STOP
   END SUBROUTINE wrf_adtl_check_spot


   SUBROUTINE wrf_run_tl( )
!<DESCRIPTION>
!     WRF tangent linear run routine.
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

   CHARACTER*256 :: timestr
   INTEGER       :: rc
   INTEGER       :: io_auxh7
   CHARACTER (LEN=80)      :: bdyname
   INTEGER                 :: open_status

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   CALL       wrf_message ( "wrf: calling tangent linear integrate" )
   model_config_rec%dyn_opt = dyn_em_tl
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
   CALL Setup_Timekeeping ( head_grid )
   CALL domain_clock_get( head_grid, start_timestr=timestr )
   CALL domain_clock_set( head_grid, current_timestr=timestr )

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   IF ( model_config_rec%bl_pbl_physics(head_grid%id) .EQ. SURFDRAGSCHEME ) THEN
      head_grid%rublten = 0.0
      head_grid%rvblten = 0.0
      head_grid%rthblten = 0.0
      head_grid%rqvblten = 0.0
   ENDIF
   IF ( model_config_rec%mp_physics(head_grid%id) .EQ. LSCONDSCHEME ) THEN
      head_grid%h_diabatic = 0.0
      head_grid%qv_diabatic = 0.0
      head_grid%qc_diabatic = 0.0
      head_grid%rainnc = 0.0
      head_grid%rainncv = 0.0
   ENDIF
   IF ( model_config_rec%mp_physics(head_grid%id) .EQ. MKESSLERSCHEME ) THEN
      head_grid%h_diabatic = 0.0
      head_grid%qv_diabatic = 0.0
      head_grid%qc_diabatic = 0.0
      head_grid%rainnc = 0.0
      head_grid%rainncv = 0.0
   ENDIF
   IF ( model_config_rec%cu_physics(head_grid%id) .EQ. DUCUSCHEME ) THEN
      head_grid%rthcuten = 0.0
      head_grid%rqvcuten = 0.0
      head_grid%rainc = 0.0
      head_grid%raincv = 0.0
      head_grid%pratec = 0.0
   ENDIF

   IF ( .NOT. config_flags%trajectory_io ) THEN
      CALL nl_get_io_form_auxhist7( head_grid%id, io_auxh7 )
      CALL nl_set_io_form_auxhist7( head_grid%id, 0 )
   ENDIF

   head_grid%itimestep = 0
   CALL start_domain ( head_grid , .TRUE. )

   IF ( ASSOCIATED(xtraj_tail) ) xtraj_pointer => xtraj_tail
   CALL integrate ( head_grid )

   IF ( .NOT. config_flags%trajectory_io ) THEN
      CALL nl_set_io_form_auxhist7( head_grid%id, io_auxh7 )
   ENDIF

   ! Close boundary file, as it will be read again from start
   CALL construct_filename2a ( bdyname , config_flags%bdy_inname , head_grid%id , 2 , 'dummydate' )
   CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
   IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
      CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
   ENDIF

   CALL       wrf_message ( "wrf: back from tangent linear integrate" )

   END SUBROUTINE wrf_run_tl

   SUBROUTINE wrf_run_tl_standalone( )
!<DESCRIPTION>
!     WRF tangent linear code standalone run
!</DESCRIPTION>

!</DESCRIPTION>

   INTEGER :: rc, time_step, id, ierr
   CHARACTER*256 :: timestr

   ! Return immediately if not dyn_em_tl
   IF ( config_flags%dyn_opt .NE. dyn_em_tl ) RETURN

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   IF ( head_grid%trajectory_io ) THEN

   ! Force to read the lateral boundary condition here.
   !CALL med_before_solve_io ( head_grid, config_flags )

   ! Close boundary file, as it will be read again from start later
   !CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )

   CALL init_domain_size ( head_grid, config_flags )

   ! Release linked list for trajectory
   call xtraj_io_initialize

   ! Initialize linked list for adjoint forcing and tl. pertbation
   call adtl_initialize

   CALL       wrf_message ( "wrf: calling nonlinear integrate" )

   ! Set up basic states output
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, time_step )
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxhist6 ( head_grid%id, 1 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL wrf_run

   ! Turn off basic states output
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 0 )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL       wrf_message ( "wrf: back from nonlinear integrate" )

   ENDIF 

   ! Set the file names and interval for reading basic states.
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
   CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   IF ( config_flags%mp_physics .NE. 0 .and. config_flags%mp_physics .NE. 99 .and. &
        config_flags%mp_physics .NE. 98 ) CALL nl_set_mp_physics (head_grid%id, config_flags%mp_physics_ad)
   IF ( config_flags%bl_pbl_physics .GT. 0 ) CALL nl_set_bl_pbl_physics (head_grid%id, 98)
   IF ( config_flags%cu_physics .GT. 0 ) THEN
      CALL nl_set_cu_physics (head_grid%id, 98)
      head_grid%cudt = 0
   ENDIF
   CALL nl_set_ra_lw_physics (head_grid%id, 0)
   CALL nl_set_ra_sw_physics (head_grid%id, 0)
   CALL nl_set_sf_sfclay_physics (head_grid%id, 0)

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
   
   head_grid%g_moist = 0.0
   head_grid%g_tracer = 0.0
   head_grid%g_scalar = 0.0 
   head_grid%g_rainnc  = 0.0 
   head_grid%g_rainncv = 0.0
   head_grid%g_rainc  = 0.0 
   head_grid%g_raincv = 0.0

   CALL domain_clock_get( head_grid, start_timestr=timestr )
   CALL domain_clock_set( head_grid, current_timestr=timestr )
   config_flags%auxinput9_inname = "init_pert_d01"
   config_flags%io_form_auxinput9 = 2
   CALL nl_set_io_form_auxinput9 ( head_grid%id, 2 )
   config_flags%frames_per_auxinput9 = 1
   CALL med_auxinput_in ( head_grid, auxinput9_alarm, config_flags )
   CALL  wrf_debug ( 0 , 'Read in initial perturbation' )
   config_flags%io_form_auxinput9 = 0

   CALL wrf_run_tl

   ! Release linked list for trajectory
   call xtraj_io_initialize

   END SUBROUTINE wrf_run_tl_standalone

   SUBROUTINE wrf_run_ad( )
!<DESCRIPTION>
!     WRF adjoint run routine.
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

   CHARACTER*256 :: timestr, timestr1
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second
   INTEGER :: rc, runad
   REAL    :: timestep
   TYPE(WRFU_TimeInterval) :: run_interval
   INTEGER       :: io_auxh8
   CHARACTER (LEN=80)      :: bdyname
   INTEGER                 :: open_status

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   CALL       wrf_message ( "wrf: calling adjoint integrate" )

   ! Seeting for AD model
   model_config_rec%dyn_opt = dyn_em_ad

   model_config_rec%run_days = -1 * model_config_rec%run_days
   model_config_rec%run_hours = -1 * model_config_rec%run_hours
   model_config_rec%run_minutes = -1 * model_config_rec%run_minutes
   model_config_rec%run_seconds = -1 * model_config_rec%run_seconds

   start_year = model_config_rec%start_year(head_grid%id)
   start_month = model_config_rec%start_month(head_grid%id)
   start_day = model_config_rec%start_day(head_grid%id)
   start_hour = model_config_rec%start_hour(head_grid%id)
   start_minute = model_config_rec%start_minute(head_grid%id)
   start_second = model_config_rec%start_second(head_grid%id)

   end_year = model_config_rec%end_year(head_grid%id)
   end_month = model_config_rec%end_month(head_grid%id)
   end_day = model_config_rec%end_day(head_grid%id)
   end_hour = model_config_rec%end_hour(head_grid%id)
   end_minute = model_config_rec%end_minute(head_grid%id)
   end_second = model_config_rec%end_second(head_grid%id)

   model_config_rec%start_year(head_grid%id)  = end_year
   model_config_rec%start_month(head_grid%id)  = end_month
   model_config_rec%start_day(head_grid%id)  = end_day
   model_config_rec%start_hour(head_grid%id)  = end_hour
   model_config_rec%start_minute(head_grid%id)  = end_minute
   model_config_rec%start_second(head_grid%id)  = end_second

   model_config_rec%end_year(head_grid%id)    = start_year
   model_config_rec%end_month(head_grid%id)    = start_month
   model_config_rec%end_day(head_grid%id)    = start_day
   model_config_rec%end_hour(head_grid%id)    = start_hour
   model_config_rec%end_minute(head_grid%id)    = start_minute
   model_config_rec%end_second(head_grid%id)    = start_second

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
   CALL Setup_Timekeeping ( head_grid )
   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   CALL domain_clock_get( head_grid, start_timestr=timestr )
   CALL domain_clock_set( head_grid, current_timestr=timestr )
   CALL domain_clock_set( head_grid, time_step_seconds=-1*model_config_rec%time_step )

   IF ( ASSOCIATED(xtraj_head) ) xtraj_pointer => xtraj_head%next

   ! head_grid%itimestep should be the last
   IF ( head_grid%itimestep .EQ. 0 ) THEN
      timestep=abs(head_grid%dt)
      run_interval = head_grid%stop_subtime - head_grid%start_subtime

      CALL WRFU_TimeIntervalGet( run_interval, S=runad, rc=rc )
      runad = abs(runad)

      head_grid%itimestep = ceiling(real(runad)/timestep)
   ENDIF

   IF ( .NOT. config_flags%trajectory_io ) THEN
      CALL nl_get_io_form_auxhist8( head_grid%id, io_auxh8 )
      CALL nl_set_io_form_auxhist8( head_grid%id, 0 )
   ENDIF

   CALL integrate ( head_grid )

   IF ( .NOT. config_flags%trajectory_io ) THEN
      CALL nl_set_io_form_auxhist8( head_grid%id, io_auxh8 )
   ENDIF

   CALL start_domain ( head_grid , .TRUE. )

   IF ( .NOT. config_flags%trajectory_io .OR. gradient_out ) THEN
      config_flags%auxhist7_outname = "gradient_wrfplus_d<domain>_<date>"
      config_flags%io_form_auxhist7 = 2
      CALL nl_set_io_form_auxhist7 ( head_grid%id, 2 )
      CALL  med_hist_out ( head_grid , AUXHIST7_ALARM , config_flags )
      CALL  wrf_debug ( 0 , 'Output gradient in WRFPLUS (not including LBC gradient)' )
   ENDIF

!  Recover to NL model 
   model_config_rec%dyn_opt = dyn_em

   model_config_rec%run_days = -1 * model_config_rec%run_days
   model_config_rec%run_hours = -1 * model_config_rec%run_hours
   model_config_rec%run_minutes = -1 * model_config_rec%run_minutes
   model_config_rec%run_seconds = -1 * model_config_rec%run_seconds

   model_config_rec%start_year(head_grid%id)  = start_year
   model_config_rec%start_month(head_grid%id)  = start_month
   model_config_rec%start_day(head_grid%id)  = start_day
   model_config_rec%start_hour(head_grid%id)  = start_hour
   model_config_rec%start_minute(head_grid%id)  = start_minute
   model_config_rec%start_second(head_grid%id)  = start_second

   model_config_rec%end_year(head_grid%id)    = end_year
   model_config_rec%end_month(head_grid%id)    = end_month
   model_config_rec%end_day(head_grid%id)    = end_day
   model_config_rec%end_hour(head_grid%id)    = end_hour
   model_config_rec%end_minute(head_grid%id)    = end_minute
   model_config_rec%end_second(head_grid%id)    = end_second

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
   CALL Setup_Timekeeping ( head_grid )
   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )
   CALL domain_clock_set( head_grid, time_step_seconds=model_config_rec%time_step )

   ! Close boundary file, as it will be read again from start
   CALL construct_filename2a ( bdyname , config_flags%bdy_inname , head_grid%id , 2 , 'dummydate' )
   CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr )
   IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
      CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
   ENDIF

   CALL       wrf_message ( "wrf: back from adjoint integrate" )

   END SUBROUTINE wrf_run_ad

   SUBROUTINE wrf_run_ad_standalone( )
!<DESCRIPTION>
!     WRF adjoint code standalone run
!</DESCRIPTION>

!</DESCRIPTION>

   INTEGER :: rc, time_step, id, ierr
   CHARACTER*256 :: timestr
   CHARACTER (LEN=80) :: bdyname

   ! Return immediately if not dyn_em_ad
   IF ( config_flags%dyn_opt .NE. dyn_em_ad ) RETURN

   ! Force to turn off history output in this case
   CALL WRFU_AlarmRingerOff( head_grid%alarms( HISTORY_ALARM ), rc=rc )

   IF ( head_grid%trajectory_io ) THEN

   ! Force to read the lateral boundary condition here.
   !CALL med_before_solve_io ( head_grid, config_flags )

   ! Close boundary file, as it will be read again from start later
   !CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )

   CALL init_domain_size ( head_grid, config_flags )

   ! Release linked list for trajectory
   call xtraj_io_initialize

   CALL       wrf_message ( "wrf: calling nonlinear integrate" )

   ! Set up basic states output
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, time_step )
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxhist6 ( head_grid%id, 1 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL wrf_run

   ! Turn off basic states output
   CALL nl_set_io_form_auxhist6 ( head_grid%id, 0 )
   CALL nl_set_auxhist6_interval_s ( head_grid%id, 0 )
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
   CALL Setup_Timekeeping ( head_grid )

   CALL       wrf_message ( "wrf: back from nonlinear integrate" )

   ENDIF 

   ! Set the file names and interval for reading basic states.
   model_config_rec%auxinput6_inname = "auxhist6_d<domain>_<date>"
   CALL nl_get_time_step ( head_grid%id, time_step )
   CALL nl_set_auxinput6_interval_s (head_grid%id, time_step )
   CALL nl_set_io_form_auxinput6 ( head_grid%id, 2 )
   CALL nl_set_frames_per_auxinput6 ( head_grid%id, 1 )

   IF ( config_flags%mp_physics .NE. 0 .and. config_flags%mp_physics .NE. 99 .and. &
        config_flags%mp_physics .NE. 98 ) CALL nl_set_mp_physics (head_grid%id, config_flags%mp_physics_ad)
   IF ( config_flags%bl_pbl_physics .GT. 0 ) CALL nl_set_bl_pbl_physics (head_grid%id, 98)
   IF ( config_flags%cu_physics .GT. 0 ) THEN
      CALL nl_set_cu_physics (head_grid%id, 98)
      head_grid%cudt = 0
   ENDIF
   CALL nl_set_ra_lw_physics (head_grid%id, 0)
   CALL nl_set_ra_sw_physics (head_grid%id, 0)
   CALL nl_set_sf_sfclay_physics (head_grid%id, 0)

   CALL zero_out_ad ( head_grid )

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
   
   head_grid%g_moist = 0.0
   head_grid%g_tracer = 0.0
   head_grid%g_scalar = 0.0 
   head_grid%g_rainnc  = 0.0 
   head_grid%g_rainncv = 0.0
   head_grid%g_rainc  = 0.0 
   head_grid%g_raincv = 0.0

   CALL domain_clock_get( head_grid, stop_timestr=timestr )
   CALL domain_clock_set( head_grid, current_timestr=timestr )
   config_flags%auxinput7_inname = "final_sens_d01"
   config_flags%io_form_auxinput7 = 2
   CALL nl_set_io_form_auxinput7 ( head_grid%id, 2 )
   config_flags%frames_per_auxinput7 = 1
   CALL med_auxinput_in ( head_grid, auxinput7_alarm, config_flags )
   CALL  wrf_debug ( 0 , 'Read in final sensitivuty' )
   config_flags%io_form_auxinput7 = 0
   CALL domain_clock_get( head_grid, start_timestr=timestr )
   CALL domain_clock_set( head_grid, current_timestr=timestr )

   gradient_out = .TRUE.

   CALL wrf_run_ad

!-- Output gradient in boundary
   CALL construct_filename1( bdyname , 'gradient_wrfbdy' , head_grid%id , 2 )
   CALL open_w_dataset ( id, TRIM(bdyname) , head_grid , config_flags , output_boundary , "DATASET=BOUNDARY", ierr )
   IF ( ierr .NE. 0 ) THEN
      CALL wrf_error_fatal( 'real: error opening wrfbdy for writing' )
   END IF
   print *,'Output LBC gradient valid between these times ',current_date, ' ',start_date
   CALL output_boundary ( id, head_grid , config_flags , ierr )

   ! Release linked list for trajectory
   call xtraj_io_initialize

   END SUBROUTINE wrf_run_ad_standalone
!------------------AD/TL code end--------------------
#endif

   SUBROUTINE wrf_run( )
!<DESCRIPTION>
!     WRF run routine.
!</DESCRIPTION>

#if (WRFPLUS == 1)
   integer :: io_auxh7, io_auxh8
   CHARACTER (LEN=80)      :: bdyname
   INTEGER                 :: open_status
#endif
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
#if (WRFPLUS == 1)
   model_config_rec%dyn_opt = dyn_em
   IF ( model_config_rec%bl_pbl_physics(head_grid%id) .EQ. SURFDRAGSCHEME ) THEN
      head_grid%rublten = 0.0
      head_grid%rvblten = 0.0
      head_grid%rthblten = 0.0
      head_grid%rqvblten = 0.0
   ENDIF
   IF ( model_config_rec%mp_physics(head_grid%id) .EQ. LSCONDSCHEME ) THEN
      head_grid%h_diabatic = 0.0
      head_grid%qv_diabatic = 0.0
      head_grid%qc_diabatic = 0.0
      head_grid%rainnc = 0.0
      head_grid%rainncv = 0.0
   ENDIF
   IF ( model_config_rec%mp_physics(head_grid%id) .EQ. MKESSLERSCHEME ) THEN
      head_grid%h_diabatic = 0.0
      head_grid%qv_diabatic = 0.0
      head_grid%qc_diabatic = 0.0
      head_grid%rainnc = 0.0
      head_grid%rainncv = 0.0
   ENDIF
   IF ( model_config_rec%cu_physics(head_grid%id) .EQ. DUCUSCHEME ) THEN
      head_grid%rthcuten = 0.0
      head_grid%rqvcuten = 0.0
      head_grid%rainc = 0.0
      head_grid%raincv = 0.0
   ENDIF

   CALL nl_get_io_form_auxhist7( head_grid%id, io_auxh7 )
   CALL nl_get_io_form_auxhist8( head_grid%id, io_auxh8 )
   CALL nl_set_io_form_auxhist7( head_grid%id, 0 )
   CALL nl_set_io_form_auxhist8( head_grid%id, 0 )

   head_grid%itimestep = 0
   CALL start_domain ( head_grid , .TRUE. )
#endif
   CALL integrate ( head_grid )

#if (WRFPLUS == 1)
   CALL nl_set_io_form_auxhist7( head_grid%id, io_auxh7 )
   CALL nl_set_io_form_auxhist8( head_grid%id, io_auxh8 )

   ! Close boundary file, as it will be read again from start
   CALL construct_filename2a ( bdyname , config_flags%bdy_inname , head_grid%id , 2 , 'dummydate' )
   CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr )
   IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
      CALL close_dataset ( head_grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
   ENDIF
#endif

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

!WRF:DRIVER_LAYER:INTEGRATION
!

MODULE module_integrate

CONTAINS

RECURSIVE SUBROUTINE integrate ( grid )



   USE module_domain
   USE module_driver_constants
   USE module_nesting
   USE module_configure
   USE module_timing
   USE module_utility
   USE module_cpl, ONLY : coupler_on, cpl_snd, cpl_defdomain
#ifdef DM_PARALLEL
! better if this did not need to be used here. Problem is that the definition of 
! domain_active_this_task comes from module_dm, and the routine it's being passed to (alloc_and_configure_domain)
! is defined in module_domain, which uses module_dm.  If that weren't the case, we could have the
! alloc_and_configure_domain routine get this form the module_dm module itself, but as it stands there
! would be a circular use association.  jm 20140828
   USE module_dm, ONLY:  domain_active_this_task !, push_communicators_for_domain, pop_communicators_for_domain
#endif

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , POINTER :: grid

! module_integrate:integrate
! <DESCRIPTION> 
! This is a driver-level routine that controls the integration of a
! domain and subdomains rooted at the domain. 
! 
! The integrate routine takes a domain pointed to by the argument
! <em>grid</em> and advances the domain and its associated nests from the
! grid's current time, stored within grid%domain_clock, to a given time
! forward in the simulation, stored as grid%stop_subtime. The
! stop_subtime value is arbitrary and does not have to be the same as
! time that the domain finished integrating.  The simulation stop time
! for the grid is known to the grid's clock (grid%domain_clock) and that
! is checked with a call to domain_clockisstoptime prior to beginning the
! loop over time period that is specified by the
! current time/stop_subtime interval.
! 
! The clock, the simulation stop time for the domain, and other timing
! aspects for the grid are set up in the routine
! (<a href="setup_timekeeping.html">setup_timekeeping</a>) at the time
! that the domain is initialized.
! The lower-level time library and the type declarations for the times
! and time intervals used are defined either in 
! external/esmf_time_f90/module_utility.F90 or in 
! external/io_esmf/module_utility.F90 depending on a build-time decision to 
! incorporate either the embedded ESMF subset implementation contained in 
! external/esmf_time_f90 or to use a site-specific installation of the ESMF 
! library.  This decision is made during the configuration step of the WRF 
! build process.  Note that arithmetic and comparison is performed on these 
! data types using F90 operator overloading, also defined in that library.
! 
! This routine is the lowest level of the WRF Driver Layer and for the most
! part the WRF routines that are called from here are in the topmost level
! of the Mediation Layer.  Mediation layer routines typically are not 
! defined in modules. Therefore, the routines that this routine calls
! have explicit interfaces specified in an interface block in this routine.
!
! As part of the Driver Layer, this routine is intended to be non model-specific
! and so a minimum of WRF-specific logic is coded at this level. Rather, there
! are a number of calls to mediation layer routines that contain this logic, some
! of which are merely stubs in WRF Mediation Layer that sits below this routine
! in the call tree.  The routines that integrate calls in WRF are defined in
! share/mediation_integrate.F.
! 
! Flow of control
! 
! 1. Check to see that the domain is not finished 
! by testing the value returned by domain_clockisstoptime for the
! domain.
! 
! 2. <a href=model_to_grid_config_rec.html>Model_to_grid_config_rec</a> is called to load the local config_flags
! structure with the configuration information for the grid stored
! in model_config_rec and indexed by the grid's unique integer id. These
! structures are defined in frame/module_configure.F.
! 
! 3. The current time of the domain is retrieved from the domain's clock
! using domain_get_current_time.  
! 
! 4. Iterate forward while the current time is less than the stop subtime.
! 
! 4.a. Start timing for this iteration (only on node zero in distributed-memory runs)
! 
! 4.b. Call <a href=med_setup_step.html>med_setup_step</a> to allow the mediation layer to 
! do anything that's needed to call the solver for this domain.  In WRF this means setting
! the indices into the 4D tracer arrays for the domain.
! 
! 4.c. Check for any nests that need to be started up at this time.  This is done 
! calling the logical function <a href=nests_to_open.html>nests_to_open</a> (defined in 
! frame/module_nesting.F) which returns true and the index into the current domain's list
! of children to use for the nest when one needs to be started.
! 
! 4.c.1  Call <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a> to allocate
! the new nest and link it as a child of this grid.
! 
! 4.c.2  Call <a href=setup_Timekeeping.html>setup_Timekeeping</a> for the nest.
! 
! 4.c.3  Initialize the nest's arrays by calling <a href=med_nest_initial.html>med_nest_initial</a>. This will
! either interpolate data from this grid onto the nest, read it in from a file, or both. In a restart run, this
! is also where the nest reads in its restart file.
! 
! 4.d  If a nest was opened above, check for and resolve overlaps (this is not implemented in WRF 2.0, which
! supports multiple nests on the same level but does not support overlapping).
! 
! 4.e  Give the mediation layer an opportunity to do something before the solver is called by
! calling <a href=med_before_solve_io.html>med_before_solve_io</a>. In WRF this is the point at which history and
! restart data is output.
! 
! 4.f  Call <a href=solve_interface.html>solve_interface</a> which calls the solver that advance the domain 
! one time step, then advance the domain's clock by calling domain_clockadvance.  
! The enclosing WHILE loop around this section is for handling other domains 
! with which this domain may overlap.  It is not active in WRF 2.0 and only 
! executes one trip.  
! 
! 4.g Call med_calc_model_time and med_after_solve_io, which are stubs in WRF.
! 
! 4.h Iterate over the children of this domain (<tt>DO kid = 1, max_nests</tt>) and check each child pointer to see
! if it is associated (and therefore, active).
! 
! 4.h.1  Force the nested domain boundaries from this domain by calling <a href=med_nest_force.html>med_nest_force</a>.
! 
! 4.h.2  Setup the time period over which the nest is to run. Sine the current grid has been advanced one time step
! and the nest has not, the start for the nest is this grid's current time minus one time step.  The nest's stop_subtime
! is the current time, bringing the nest up the same time level as this grid, its parent.
! 
! 4.h.3  Recursively call this routine, integrate, to advance the nest's time.  Since it is recursive, this will
! also advance all the domains who are nests of this nest and so on.  In other words, when this call returns, all
! the domains rooted at the nest will be at the current time.
! 
! 4.h.4  Feedback data from the nested domain back onto this domain by calling <a href=med_nest_feedback.html>med_nest_feedback</a>.
! 
! 4.i  Write the time to compute this grid and its subtree. This marks the end of the loop begun at step 4, above.
! 
! 5. Give the mediation layer an opportunity to do I/O at the end of the sequence of steps that brought the
! grid up to stop_subtime with a call to <a href=med_last_solve_io.html>med_last_solve_io</a>.  In WRF, this 
! is used to generate the final history and/or restart output when the domain reaches the end of it's integration.
! There is logic here to make sure this occurs correctly on a nest, since the nest may finish before its parent.
! </DESCRIPTION>

   !  Local data.

   CHARACTER*32                           :: outname, rstname
   TYPE(domain) , POINTER                 :: grid_ptr , new_nest
   TYPE(domain)                           :: intermediate_grid
   INTEGER                                :: step
   INTEGER                                :: nestid , kid
   LOGICAL                                :: a_nest_was_opened
   INTEGER                                :: fid , rid
   LOGICAL                                :: lbc_opened
   REAL                                   :: time, btime, bfrq
   CHARACTER*256                          :: message, message2,message3
   TYPE (grid_config_rec_type)            :: config_flags
   LOGICAL , EXTERNAL                     :: wrf_dm_on_monitor
   INTEGER                                :: idum1 , idum2 , ierr , open_status
   LOGICAL                                :: should_do_last_io
   LOGICAL                                :: may_have_moved


   ! interface
   INTERFACE
       ! mediation-supplied solver
     SUBROUTINE solve_interface ( grid )
       USE module_domain
       TYPE (domain) grid
     END SUBROUTINE solve_interface
       ! mediation-supplied routine to allow driver to pass time information
       ! down to mediation/model layer
     SUBROUTINE med_calc_model_time ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_calc_model_time
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O before the call to the solve routine
     SUBROUTINE med_before_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_before_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O after the call to the solve routine
     SUBROUTINE med_after_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_after_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O to initialize a new nest
     SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  parent
       INTEGER, INTENT(IN)    ::  newid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_pre_nest_initial
     SUBROUTINE med_nest_initial ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_initial
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest forcing
     SUBROUTINE med_nest_force ( parent , grid )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid, parent
     END SUBROUTINE med_nest_force

#ifdef MOVE_NESTS
     SUBROUTINE med_nest_move ( parent , grid )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
     END SUBROUTINE med_nest_move
     SUBROUTINE reconcile_nest_positions_over_tasks ( grid )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid
     END SUBROUTINE reconcile_nest_positions_over_tasks
#endif

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! provide parent->nest feedback
     SUBROUTINE med_nest_feedback ( parent , grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  grid , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback

       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform I/O prior to the close of this call to integrate
     SUBROUTINE med_last_solve_io ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_last_solve_io
       ! mediation-supplied routine that gives mediation layer opportunity to 
       ! perform setup before iteration over steps in this call to integrate
     SUBROUTINE med_setup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_setup_step
       ! mediation-supplied routine that gives mediation layer opportunity to
       ! perform setup before iteration over steps in this call to integrate
     SUBROUTINE med_endup_step ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) grid
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_endup_step
       ! mediation-supplied routine that intializes the nest from the grid
       ! by interpolation

     SUBROUTINE Setup_Timekeeping( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

     SUBROUTINE dfi_accumulate( grid )
       USE module_domain
       TYPE(domain), POINTER :: grid
     END SUBROUTINE

   END INTERFACE

   ! This allows us to reference the current grid from anywhere beneath 
   ! this point for debugging purposes.  
   CALL set_current_grid_ptr( grid )
   CALL push_communicators_for_domain( grid%id )

   IF ( .NOT. domain_clockisstoptime( grid ) ) THEN
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
      IF ( config_flags%grid_allowed ) THEN
         CALL domain_clockprint ( 150, grid, 'DEBUG:  top of integrate(),' )
         DO WHILE ( .NOT. domain_clockisstopsubtime(grid) )
            IF ( wrf_dm_on_monitor() ) THEN
IF ( grid%active_this_task ) THEN
               CALL start_timing
END IF
            END IF
            CALL med_setup_step ( grid , config_flags )
            a_nest_was_opened = .false.
            ! for each nest whose time has come...
            DO WHILE ( nests_to_open( grid , nestid , kid ) )
               ! nestid is index into model_config_rec (module_configure) of the grid
               ! to be opened; kid is index into an open slot in grid's list of children
               a_nest_was_opened = .true.
               CALL med_pre_nest_initial ( grid , nestid , config_flags )
               CALL alloc_and_configure_domain ( domain_id  = nestid ,   &
! better if this were not ifdef'd here, try moving into module_dm? see comment above. jm 20140828
#ifdef DM_PARALLEL
                                           active_this_task = domain_active_this_task( nestid ), &
#endif
                                                 grid       = new_nest , &
                                                 parent     = grid ,     &
                                                 kid        = kid        )
               CALL Setup_Timekeeping (new_nest)
               CALL med_nest_initial ( grid , new_nest , config_flags )
IF ( grid%active_this_task ) THEN
               IF ( grid%dfi_stage == DFI_STARTFWD ) THEN
                  CALL wrf_dfi_startfwd_init(new_nest)
               ENDIF
               IF (coupler_on) CALL cpl_defdomain( new_nest ) 
ENDIF ! active_this_task
            END DO
            IF ( a_nest_was_opened ) THEN
               CALL set_overlaps ( grid )   ! find overlapping and set pointers
            END IF

IF ( grid%active_this_task ) THEN
            ! Accumulation calculation for DFI
              CALL dfi_accumulate ( grid )

              CALL med_before_solve_io ( grid , config_flags )
ENDIF ! active_this_task

            grid_ptr => grid
            DO WHILE ( ASSOCIATED( grid_ptr ) )
#if ( NMM_CORE == 1 )
                  grid_ptr%mvnest = .FALSE.
#endif
IF ( grid_ptr%active_this_task ) THEN
                  CALL set_current_grid_ptr( grid_ptr )
                  CALL wrf_debug( 100 , 'module_integrate: calling solve interface ' )

                  CALL solve_interface ( grid_ptr ) 

ENDIF
               CALL domain_clockadvance ( grid_ptr )
               CALL wrf_debug( 100 , 'module_integrate: back from solve interface ' )
               ! print lots of time-related information for testing
               ! switch this on with namelist variable self_test_domain
               CALL domain_time_test( grid_ptr, 'domain_clockadvance' )
               grid_ptr => grid_ptr%sibling
            END DO
            CALL set_current_grid_ptr( grid )
            CALL med_calc_model_time ( grid , config_flags )
IF ( grid%active_this_task ) THEN
              CALL med_after_solve_io ( grid , config_flags )
ENDIF

            grid_ptr => grid
            DO WHILE ( ASSOCIATED( grid_ptr ) )
               DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                   CALL set_current_grid_ptr( grid_ptr%nests(kid)%ptr )
                   ! Recursive -- advance nests from previous time level to this time level.
                   CALL wrf_debug( 100 , 'module_integrate: calling med_nest_force ' )
                   CALL med_nest_force ( grid_ptr , grid_ptr%nests(kid)%ptr )
                   CALL wrf_debug( 100 , 'module_integrate: back from med_nest_force ' )
                   grid_ptr%nests(kid)%ptr%start_subtime = &
                     domain_get_current_time(grid) - domain_get_time_step(grid)
                   grid_ptr%nests(kid)%ptr%stop_subtime = &
                     domain_get_current_time(grid)
                 ENDIF
               ENDDO

               DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                   CALL set_current_grid_ptr( grid_ptr%nests(kid)%ptr )
                   WRITE(message,*)grid%id,' module_integrate: recursive call to integrate '
                   CALL wrf_debug( 100 , message )
                      CALL integrate ( grid_ptr%nests(kid)%ptr ) 
                   WRITE(message,*)grid%id,' module_integrate: back from recursive call to integrate '
                   CALL wrf_debug( 100 , message )
                 ENDIF
               ENDDO
               may_have_moved = .FALSE.
               DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
                   CALL set_current_grid_ptr( grid_ptr%nests(kid)%ptr )
                   IF ( .NOT. ( domain_clockisstoptime(head_grid              ) .OR. &
                                domain_clockisstoptime(grid                   ) .OR. &
                                domain_clockisstoptime(grid_ptr%nests(kid)%ptr) ) )  THEN
                     CALL wrf_debug( 100 , 'module_integrate: calling med_nest_feedback ' )
                     CALL med_nest_feedback ( grid_ptr , grid_ptr%nests(kid)%ptr , config_flags )
                     CALL wrf_debug( 100 , 'module_integrate: back from med_nest_feedback ' )
                   END IF
#ifdef MOVE_NESTS
                   IF ( .NOT. domain_clockisstoptime( head_grid ) ) THEN
                     CALL med_nest_move ( grid_ptr , grid_ptr%nests(kid)%ptr )
                     may_have_moved = .TRUE.
                   ENDIF
#endif
                 END IF
               END DO
#ifdef MOVE_NESTS
               IF ( may_have_moved ) THEN
                 CALL reconcile_nest_positions_over_tasks( grid_ptr )
                 CALL model_to_grid_config_rec ( grid_ptr%id , model_config_rec , config_flags )
               ENDIF
#endif
               IF (coupler_on) CALL cpl_snd( grid_ptr ) 
               grid_ptr => grid_ptr%sibling
            END DO
            CALL set_current_grid_ptr( grid )
            !  Report on the timing for a single time step.
            IF ( wrf_dm_on_monitor() ) THEN
IF ( grid%active_this_task ) THEN
               CALL domain_clock_get ( grid, current_timestr=message2 )
#if (EM_CORE == 1)
               if (config_flags%use_adaptive_time_step) then
                  WRITE ( message , FMT = '("main (dt=",F6.2,"): time ",A," on domain ",I3)' ) grid%dt, TRIM(message2), grid%id
               else
                  WRITE ( message , FMT = '("main: time ",A," on domain ",I3)' ) TRIM(message2), grid%id
               endif
#else
                  WRITE ( message , FMT = '("main: time ",A," on domain ",I3)' ) TRIM(message2), grid%id
#endif
               CALL end_timing ( TRIM(message) )
ENDIF ! active_this_task
            ENDIF
            CALL med_endup_step ( grid , config_flags )
         END DO

IF ( grid%active_this_task ) THEN
         ! Accumulation calculation for DFI
         CALL dfi_accumulate ( grid )
ENDIF ! active_this_task


         ! Avoid double writes on nests if this is not really the last time;
         ! Do check for write if the parent domain is ending.
         IF ( grid%id .EQ. 1 ) THEN               ! head_grid
            IF ( grid%active_this_task ) CALL med_last_solve_io ( grid , config_flags )
         ELSE
! zip up the tree and see if any ancestor is at its stop time
            should_do_last_io = domain_clockisstoptime( head_grid )
            grid_ptr => grid 
            DO WHILE ( grid_ptr%id .NE. 1 )
               IF ( domain_clockisstoptime( grid_ptr ) ) THEN
                  should_do_last_io = .TRUE. 
               END IF
               grid_ptr => grid_ptr%parents(1)%ptr
            ENDDO
            IF ( should_do_last_io ) THEN 
               grid_ptr => grid 
               CALL med_nest_feedback ( grid_ptr%parents(1)%ptr, grid , config_flags )
               IF ( grid%active_this_task ) CALL med_last_solve_io ( grid , config_flags )
            ENDIF
         ENDIF
      ENDIF
   END IF
   CALL pop_communicators_for_domain
   
END SUBROUTINE integrate

END MODULE module_integrate


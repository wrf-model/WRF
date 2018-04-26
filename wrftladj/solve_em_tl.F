!WRF+/TL:MEDIATION_LAYER:SOLVER FOR TL
!Created by Xin Zhang and Ning Pan, 2010-08 

SUBROUTINE solve_em_tl ( grid , config_flags  &
! Arguments generated from Registry
#include "dummy_new_args.inc"
!
                    )
! Driver layer modules
   USE module_state_description
   USE module_domain, ONLY : &
                  domain, get_ijk_from_grid, get_ijk_from_subgrid                          &
                 ,domain_get_current_time, domain_get_start_time                           &
                 ,domain_get_sim_start_time, domain_clock_get,is_alarm_tstep
   USE module_domain_type, ONLY : history_alarm, restart_alarm
   USE module_configure, ONLY : grid_config_rec_type
   USE module_driver_constants
   USE module_machine
   USE module_tiles, ONLY : set_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y                   &
                 ,local_communicator_periodic, wrf_dm_maxval
   USE module_comm_dm, ONLY : &
                  halo_em_a_tl_sub,halo_em_b_tl_sub,halo_em_c2_tl_sub,halo_em_chem_e_3_sub          &
                 ,halo_em_chem_e_5_sub,halo_em_chem_e_7_sub,halo_em_chem_old_e_5_sub       &
                 ,halo_em_chem_old_e_7_sub,halo_em_c_tl_sub,halo_em_d2_3_tl_sub                  &
                 ,halo_em_d2_5_tl_sub,halo_em_d3_3_tl_sub,halo_em_d3_5_tl_sub,halo_em_d_tl_sub         &
                 ,halo_em_e_3_tl_sub,halo_em_e_5_tl_sub,halo_em_hydro_uv_tl_sub                     &
                 ,halo_em_moist_e_3_tl_sub,halo_em_moist_e_5_tl_sub,halo_em_moist_e_7_tl_sub        &
                 ,halo_em_moist_old_e_5_tl_sub,halo_em_moist_old_e_7_tl_sub                      &
                 ,halo_em_scalar_e_3_sub,halo_em_scalar_e_5_sub,halo_em_scalar_e_7_sub     &
                 ,halo_em_scalar_old_e_5_sub,halo_em_scalar_old_e_7_sub,halo_em_tke_3_tl_sub  &
                 ,halo_em_tke_5_tl_sub,halo_em_tke_7_tl_sub,halo_em_tke_advect_3_tl_sub             &
                 ,halo_em_tke_advect_5_tl_sub,halo_em_tke_old_e_5_tl_sub                         &
                 ,halo_em_tke_old_e_7_tl_sub,halo_em_tracer_e_3_tl_sub,halo_em_tracer_e_5_tl_sub    &
                 ,halo_em_tracer_e_7_tl_sub,halo_em_tracer_old_e_5_tl_sub                        &
                 ,halo_em_tracer_old_e_7_tl_sub,period_bdy_em_a_sub,halo_em_sbm_tl_sub           &
                 ,period_bdy_em_b3_sub,period_bdy_em_b_sub,period_bdy_em_chem2_sub         &
                 ,period_bdy_em_chem_old_sub,period_bdy_em_chem_sub,period_bdy_em_d3_sub   &
                 ,period_bdy_em_d_sub,period_bdy_em_e_sub,period_bdy_em_moist2_sub         &
                 ,period_bdy_em_moist_old_sub,period_bdy_em_moist_sub                      &
                 ,period_bdy_em_scalar2_sub,period_bdy_em_scalar_old_sub                   &
                 ,period_bdy_em_scalar_sub,period_bdy_em_tke_old_sub,period_bdy_em_tke_sub &
                 ,period_bdy_em_tracer2_sub,period_bdy_em_tracer_old_sub                   &
                 ,period_bdy_em_tracer_sub,period_em_da_sub,period_em_hydro_uv_sub 
#endif
   USE module_utility
! Mediation layer modules
! Model layer modules
   USE module_model_constants
   USE g_module_small_step_em
   USE module_em
   USE g_module_em
   USE g_module_big_step_utilities_em
   USE g_module_bc
   USE module_bc_em
   USE g_module_bc_em
   USE module_solvedebug_em
   USE g_module_physics_addtendc
   USE g_module_diffusion_em
   USE module_polarfft
   USE g_module_microphysics_driver
   USE module_microphysics_zero_out
   USE g_module_microphysics_zero_out
   USE module_fddaobs_driver
!  USE module_diagnostics
#if (WRF_CHEM==1)
   USE module_input_chem_data
   USE module_input_tracer
   USE module_chem_utilities
#endif
   USE g_module_first_rk_step_part1 
   USE g_module_first_rk_step_part2
   USE module_after_all_rk_steps
   USE module_llxy, ONLY : proj_cassini
   USE module_avgflx_em, ONLY : zero_avgflx, upd_avgflx

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).
#include "dummy_new_decl.inc"

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

   INTEGER                         :: sids , side , sjds , sjde , skds , skde , &
                                      sims , sime , sjms , sjme , skms , skme , &
                                      sips , sipe , sjps , sjpe , skps , skpe


   INTEGER ::              imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c , is , num_3d_s
   INTEGER                         :: loop
   INTEGER                         :: sz
   INTEGER                         :: iswater

   LOGICAL                         :: specified_bdy, channel_bdy

   REAL                            :: t_new

   ! Changes in tendency at this timestep
   real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: h_tendency, &
                                                                                   z_tendency
   real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: g_h_tendency, &
                                                                                   g_z_tendency
                                                                                   
   ! Whether advection should produce decoupled horizontal and vertical advective tendency outputs
   LOGICAL                        :: tenddec

   ! Flag for producing diagnostic fields (e.g., radar reflectivity)
   LOGICAL                        :: diag_flag

#if (WRF_CHEM==1)
   ! Index cross-referencing array for tendency accumulation
   INTEGER, DIMENSION( num_chem ) :: adv_ct_indices
#endif

! storage for tendencies and decoupled state (generated from Registry)

#include "i1_decl.inc"
! Previous time level of tracer arrays now defined as i1 variables;
! the state 4d arrays now redefined as 1-time level arrays in Registry.
! Benefit: save memory in nested runs, since only 1 domain is active at a
! time.  Potential problem on stack-limited architectures: increases
! amount of data on program stack by making these automatic arrays.

   INTEGER :: rc
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dts, dtm, wmax
   REAL , ALLOCATABLE , DIMENSION(:)  :: max_vert_cfl_tmp, max_horiz_cfl_tmp
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk
   LOGICAL :: f_flux  ! flag for computing averaged fluxes in cu_gd
   REAL    :: curr_secs, curr_secs2
   INTEGER :: num_sound_steps
   INTEGER :: idex, jdex
   REAL    :: max_msft
   REAL    :: spacing

   INTEGER :: ii, jj !kk is above after l,kte
   REAL    :: dclat
   INTEGER :: debug_level

! urban related variables
   INTEGER :: NUM_ROOF_LAYERS, NUM_WALL_LAYERS, NUM_ROAD_LAYERS   ! urban

   TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval, tmpTimeInterval2
   REAL                                       :: real_time
   LOGICAL                                    :: adapt_step_flag
   LOGICAL                                    :: fill_w_flag

! variables for flux-averaging code 20091223
   CHARACTER*256                              :: message, message2
   REAL                                       :: old_dt
   TYPE(WRFU_Time)                            :: temp_time, CurrTime, restart_time
   INTEGER, PARAMETER                         :: precision = 100
   INTEGER                                    :: num, den
   TYPE(WRFU_TimeInterval)                    :: dtInterval, intervaltime,restartinterval

! Define benchmarking timers if -DBENCH is compiled
#include "bench_solve_em_def.h"

!----------------------
! Executable statements
!----------------------

!<DESCRIPTION>
!<pre>
! solve_em_tl is the main driver for advancing a grid a single timestep.
! It is a mediation-layer routine -> DM and SM calls are made where
! needed for parallel processing.
!
! solve_em_tl can integrate the equations using 3 time-integration methods
!
!    - 3rd order Runge-Kutta time integration (recommended)
!
!    - 2nd order Runge-Kutta time integration
!
! The main sections of solve_em_tl are
!
! (1) Runge-Kutta (RK) loop
!
! (2) Non-timesplit physics (i.e., tendencies computed for updating
!     model state variables during the first RK sub-step (loop)
!
! (3) Small (acoustic, sound) timestep loop - within the RK sub-steps
!
! (4) scalar advance for moist and chem scalar variables (and TKE)
!     within the RK sub-steps.
!
! (5) time-split physics (after the RK step), currently this includes
!     only microphyics
!
! A more detailed description of these sections follows.
!</pre>
!</DESCRIPTION>

! Initialize timers if compiled with -DBENCH
#include "bench_solve_em_init.h"

!  set runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  Obtain dimension information stored in the grid data structure.

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe,    &
                             imsx, imex, jmsx, jmex, kmsx, kmex,    &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                             imsy, imey, jmsy, jmey, kmsy, kmey,    &
                             ipsy, ipey, jpsy, jpey, kpsy, kpey )

   CALL get_ijk_from_subgrid (  grid ,                   &
                             sids, side, sjds, sjde, skds, skde,    &
                             sims, sime, sjms, sjme, skms, skme,    &
                             sips, sipe, sjps, sjpe, skps, skpe    )
   k_start         = kps
   k_end           = kpe

   num_3d_m        = num_moist
   num_3d_c        = num_chem
   num_3d_s        = num_scalar

   f_flux = config_flags%do_avgflx_cugd .EQ. 1

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
   CALL set_tiles ( ZONE_SOLVE_EM, grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
!   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

!  Max values of CFL for adaptive time step scheme

   ALLOCATE (max_vert_cfl_tmp(grid%num_tiles))
   ALLOCATE (max_horiz_cfl_tmp(grid%num_tiles))

  !
  ! Calculate current time in seconds since beginning of model run.
  !   Unfortunately, ESMF does not seem to have a way to return
  !   floating point seconds based on a TimeInterval.  So, we will
  !   calculate it here--but, this is not clean!!
  !
   tmpTimeInterval  = domain_get_current_time ( grid ) - domain_get_sim_start_time ( grid )
   tmpTimeInterval2 = domain_get_current_time ( grid ) - domain_get_start_time ( grid )
   curr_secs  = real_time(tmpTimeInterval)
   curr_secs2 = real_time(tmpTimeInterval2)

   old_dt = grid%dt   ! store old time step for flux averaging code at end of RK loop
!-----------------------------------------------------------------------------
! Adaptive time step: Added by T. Hutchinson, WSI  3/5/07
!   In this call, we do the time-step adaptation and set time-dependent lateral
!   boundary condition nudging weights.
!
   IF ( (config_flags%use_adaptive_time_step) .and. &
        ( (.not. grid%nested) .or. &
        ( (grid%nested) .and. (abs(grid%dtbc) < 0.0001) ) ) )THEN
      CALL adapt_timestep(grid, config_flags)
      adapt_step_flag = .TRUE.
   ELSE
      adapt_step_flag = .FALSE.
   ENDIF
! End of adaptive time step modifications
!-----------------------------------------------------------------------------
!
! Set diagnostic flag value history output time
!-----------------------------------------------------------------------------
!  if ( Is_alarm_tstep(grid_clock, grid_alarms(HISTORY_ALARM)) ) then
   diag_flag = .false.
   if ( Is_alarm_tstep(grid%domain_clock, grid%alarms(HISTORY_ALARM)) ) then
      diag_flag = .true.
   endif

   grid%itimestep = grid%itimestep + 1
   
   IF (config_flags%enable_identity) RETURN

   IF (config_flags%polar) dclat = 90./REAL(jde-jds) !(0.5 * 180/ny)

   rk_order = config_flags%rk_ord

   IF ( grid%time_step_sound == 0 ) THEN
! This function will give 4 for 6*dx and 6 for 10*dx and returns even numbers only
     spacing = min(grid%dx, grid%dy)
     IF ( ( config_flags%use_adaptive_time_step ) .AND. ( config_flags%map_proj == PROJ_CASSINI ) ) THEN
       max_msft=MIN ( MAX(grid%max_msftx, grid%max_msfty) , &
                      1.0/COS(config_flags%fft_filter_lat*degrad) )
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt / (spacing / max_msft) - 0.01 ) + 1 ), 4 )
     ELSE IF  ( config_flags%use_adaptive_time_step ) THEN
       max_msft= MAX(grid%max_msftx, grid%max_msfty)
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt / (spacing / max_msft) - 0.01 ) + 1 ), 4 )
     ELSE
       num_sound_steps = max ( 2 * ( INT (300. * grid%dt /  spacing             - 0.01 ) + 1 ), 4 )
     END IF
     WRITE(wrf_err_message,*)'grid spacing, dt, time_step_sound=',spacing,grid%dt,num_sound_steps
     CALL wrf_debug ( 50 , wrf_err_message )
   ELSE
     num_sound_steps = grid%time_step_sound
   ENDIF

   dts = grid%dt/float(num_sound_steps)

   IF (config_flags%use_adaptive_time_step) THEN

     CALL get_wrf_debug_level( debug_level )
     IF ((config_flags%time_step < 0) .AND. (debug_level.GE.50)) THEN
#ifdef DM_PARALLEL
       CALL wrf_dm_maxval(grid%max_vert_cfl, idex, jdex)
#endif
       WRITE(wrf_err_message,*)'variable dt, max horiz cfl, max vert cfl: ',&
            grid%dt, grid%max_horiz_cfl, grid%max_vert_cfl
       CALL wrf_debug ( 0 , wrf_err_message )
     ENDIF

     grid%max_cfl_val = 0
     grid%max_horiz_cfl = 0
     grid%max_vert_cfl = 0
   ENDIF

! setting bdy tendencies to zero for DFI if constant_bc = true

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

!      IF( config_flags%specified .AND. grid%dfi_opt .NE. DFI_NODFI   &
!          .AND. config_flags%constant_bc .AND. (grid%dfi_stage .EQ. DFI_BCK .OR. grid%dfi_stage .EQ. DFI_FWD) ) THEN
       IF( config_flags%specified .AND. config_flags%constant_bc ) THEN

       CALL g_zero_bdytend (grid%u_btxs,grid%g_u_btxs,grid%u_btxe,grid%g_u_btxe, &
                            grid%u_btys,grid%g_u_btys,grid%u_btye,grid%g_u_btye, &
                            grid%v_btxs,grid%g_v_btxs,grid%v_btxe,grid%g_v_btxe, &
                            grid%v_btys,grid%g_v_btys,grid%v_btye,grid%g_v_btye, &
                            grid%ph_btxs,grid%g_ph_btxs,grid%ph_btxe,grid%g_ph_btxe, &
                            grid%ph_btys,grid%g_ph_btys,grid%ph_btye,grid%g_ph_btye, &
                            grid%t_btxs,grid%g_t_btxs,grid%t_btxe,grid%g_t_btxe, &
                            grid%t_btys,grid%g_t_btys,grid%t_btye,grid%g_t_btye, &
                            grid%w_btxs,grid%g_w_btxs,grid%w_btxe,grid%g_w_btxe, &
                            grid%w_btys,grid%g_w_btys,grid%w_btye,grid%g_w_btye, &
                            grid%mu_btxs,grid%g_mu_btxs,grid%mu_btxe,grid%g_mu_btxe, &
                            grid%mu_btys,grid%g_mu_btys,grid%mu_btye,grid%g_mu_btye, &
                            moist_btxs,g_moist_btxs,moist_btxe,g_moist_btxe, &
                            moist_btys,g_moist_btys,moist_btye,g_moist_btye, &
                            scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe, &
                            scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye, &
                            grid%spec_bdy_width,num_3d_m,num_3d_s,       &
                            ids,ide, jds,jde, kds,kde,                   &
                            ims,ime, jms,jme, kms,kme,                   &
                            ips,ipe, jps,jpe, kps,kpe,                   &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                               )


       ENDIF
     ENDDO
     !$OMP END PARALLEL DO

!**********************************************************************
!
!  LET US BEGIN.......
!
!<DESCRIPTION>
!<pre>
! (1) RK integration loop is named the "Runge_Kutta_loop:"
!
!   Predictor-corrector type time integration.
!   Advection terms are evaluated at time t for the predictor step,
!   and advection is re-evaluated with the latest predicted value for
!   each succeeding time corrector step
!
!   2nd order Runge Kutta (rk_order = 2):
!   Step 1 is taken to the midpoint predictor, step 2 is the full step.
!
!   3rd order Runge Kutta (rk_order = 3):
!   Step 1 is taken to from t to dt/3, step 2 is from t to dt/2,
!   and step 3 is from t to dt.
!
!   non-timesplit physics are evaluated during first RK step and
!   these physics tendencies are stored for use in each RK pass.
!</pre>
!</DESCRIPTION>
!**********************************************************************

   Runge_Kutta_loop:  DO rk_step = 1, rk_order

   !  Set the step size and number of small timesteps for
   !  each part of the timestep

     dtm = grid%dt
     IF ( rk_order == 1 ) THEN

       write(wrf_err_message,*)' leapfrog removed, error exit for dynamics_option = ',dynamics_option
       CALL wrf_error_fatal( wrf_err_message )

     ELSE IF ( rk_order == 2 ) THEN   ! 2nd order Runge-Kutta timestep

       IF ( rk_step == 1) THEN
         dt_rk  = 0.5*grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps/2
       ELSE
         dt_rk = grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps
       ENDIF

     ELSE IF ( rk_order == 3 ) THEN ! third order Runge-Kutta

       IF ( rk_step == 1) THEN
         dt_rk = grid%dt/3.
         dts_rk = dt_rk
         number_of_small_timesteps = 1
       ELSE IF (rk_step == 2) THEN
         dt_rk  = 0.5*grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps/2
       ELSE
         dt_rk = grid%dt
         dts_rk = dts
         number_of_small_timesteps = num_sound_steps
       ENDIF

     ELSE

       write(wrf_err_message,*)' unknown solver, error exit for dynamics_option = ',dynamics_option
       CALL wrf_error_fatal( wrf_err_message )

     END IF

!  Ensure that polar meridional velocity is zero
     IF (config_flags%polar) THEN

       WRITE(wrf_err_message,*)'solve_em_tl: please revisit polar = ',config_flags%polar
       CALL wrf_error_fatal(TRIM(wrf_err_message))

!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
!      DO ij = 1 , grid%num_tiles
!        CALL zero_pole ( grid%v_1,                      &
!                         ids, ide, jds, jde, kds, kde,     &
!                         ims, ime, jms, jme, kms, kme,     &
!                         grid%i_start(ij), grid%i_end(ij), &
!                         grid%j_start(ij), grid%j_end(ij), &
!                         k_start, k_end                   )
!        CALL zero_pole ( grid%v_2,                      &
!                         ids, ide, jds, jde, kds, kde,     &
!                         ims, ime, jms, jme, kms, kme,     &
!                         grid%i_start(ij), grid%i_end(ij), &
!                         grid%j_start(ij), grid%j_end(ij), &
!                         k_start, k_end                   )
!      END DO
!      !$OMP END PARALLEL DO
     END IF
!
!  Time level t is in the *_2 variable in the first part
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
     CALL wrf_debug ( 200 , ' call g_rk_step_prep ' )

BENCH_START(g_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL g_rk_step_prep  ( config_flags, rk_step,                        &
                              grid%u_2, grid%g_u_2, grid%v_2, grid%g_v_2,   &
                              grid%w_2, grid%g_w_2, grid%t_2, grid%g_t_2,   &
                              grid%ph_2, grid%g_ph_2, grid%mu_2, grid%g_mu_2,  &
                              moist, g_moist,                               &
                              grid%ru, grid%g_ru, grid%rv, grid%g_rv, grid%rw, grid%g_rw, &
                              grid%ww, grid%g_ww, grid%php, grid%g_php, grid%alt, grid%g_alt, &
                              grid%muu, grid%g_muu, grid%muv, grid%g_muv,   &
                              grid%mub, grid%mut, grid%g_mut,               &
                              grid%phb, grid%pb, grid%p, grid%g_p, grid%al, grid%g_al, grid%alb, &
                              cqu, g_cqu, cqv, g_cqv, cqw, g_cqw,                 &
                              grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx, grid%msfty,                 &
                              grid%fnm, grid%fnp, grid%dnw, grid%rdx, grid%rdy,   &
                              num_3d_m,                         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )

     END DO
     !$OMP END PARALLEL DO
BENCH_END(g_step_prep_tim)

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the small size of this halo exchange reflects the
!         fact that we are carrying the uncoupled variables
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *
!
!  3D variables - note staggering!  ru(X), rv(Y), ww(Z), php(Z)
!
!  ru     x
!  rv     x
!  ww     x
!  php    x
!  alt    x
!  ph_2   x
!  phb    x
!
!  the following are 2D (xy) variables
!
!  muu    x
!  muv    x
!  mut    x
!--------------------------------------------------------------
#    include "HALO_EM_A_TL.inc"
#endif

! set boundary conditions on variables
! from big_step_prep for use in big_step_proc

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A.inc"
#endif

BENCH_START(g_set_phys_bc_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call g_rk_phys_bc_dry_1' )

       CALL g_rk_phys_bc_dry_1( config_flags, grid%ru,grid%g_ru, grid%rv,grid%g_rv, &
                              grid%rw,grid%g_rw, grid%ww,grid%g_ww,                 &
                              grid%muu,grid%g_muu, grid%muv,grid%g_muv, grid%mut,grid%g_mut, &
                              grid%php,grid%g_php, grid%alt,grid%g_alt, grid%p,grid%g_p,     &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                )
       CALL g_set_physical_bc3d( grid%al,grid%g_al, 'p', config_flags,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
       CALL g_set_physical_bc3d( grid%ph_2,grid%g_ph_2, 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start, k_end                )

       IF (config_flags%polar) THEN

       WRITE(wrf_err_message,*)'solve_em_tl: please revisit polar = ',config_flags%polar
       CALL wrf_error_fatal(TRIM(wrf_err_message))
!-------------------------------------------------------
! lat-lon grid pole-point (v) specification (extrapolate v, rv to the pole)
!-------------------------------------------------------

!        CALL pole_point_bc ( grid%v_1,                      &
!                             ids, ide, jds, jde, kds, kde,     &
!                             ims, ime, jms, jme, kms, kme,     &
!                             grid%i_start(ij), grid%i_end(ij), &
!                             grid%j_start(ij), grid%j_end(ij), &
!                             k_start, k_end                   )

!        CALL pole_point_bc ( grid%v_2,                      &
!                             ids, ide, jds, jde, kds, kde,     &
!                             ims, ime, jms, jme, kms, kme,     &
!                             grid%i_start(ij), grid%i_end(ij), &
!                             grid%j_start(ij), grid%j_end(ij), &
!                             k_start, k_end                   )

!-------------------------------------------------------
! end lat-lon grid pole-point (v) specification
!-------------------------------------------------------

       ENDIF
     END DO
     !$OMP END PARALLEL DO
BENCH_END(g_set_phys_bc_tim)

     rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

!<DESCRIPTION>
!<pre>
!(2) The non-timesplit physics begins with a call to "phy_prep"
!    (which computes some diagnostic variables such as temperature,
!    pressure, u and v at p points, etc).  This is followed by
!    calls to the physics drivers:
!
!              radiation,
!              surface,
!              pbl,
!              cumulus,
!              fddagd,
!              3D TKE and mixing.
!<pre>
!</DESCRIPTION>

  CALL g_first_rk_step_part1 (   grid , config_flags                   &
                             , moist , g_moist, moist_tend, g_moist_tend   &
                             , chem  , chem_tend     &
                             , tracer, g_tracer, tracer_tend, g_tracer_tend  &
                             , scalar , g_scalar, scalar_tend, g_scalar_tend &
                             , fdda3d, fdda2d           &
                             , aerod                    &
                             , ru_tendf,g_ru_tendf,rv_tendf,g_rv_tendf    &
                             , rw_tendf,g_rw_tendf, t_tendf ,g_t_tendf     &
                             , ph_tendf,g_ph_tendf, mu_tendf,g_mu_tendf    &
                             , tke_tend,g_tke_tend                         &
                             , config_flags%use_adaptive_time_step         &
                             , curr_secs                 &
                             , psim ,psih  &
                             , gz1oz0 , chklowq           &
                             , cu_act_flag , hol , th_phy ,g_th_phy                 &
                             , pi_phy ,g_pi_phy, p_phy ,g_p_phy, grid%t_phy ,grid%g_t_phy         &
                             , dz8w ,g_dz8w , p8w ,g_p8w , t8w ,g_t8w                   &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx,imex,jmsx,jmex,kmsx,kmex    &
                             , ipsx,ipex,jpsx,jpex,kpsx,kpex    &
                             , imsy,imey,jmsy,jmey,kmsy,kmey    &
                             , ipsy,ipey,jpsy,jpey,kpsy,kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                             )

#ifdef DM_PARALLEL
       IF ( config_flags%bl_pbl_physics == MYNNPBLSCHEME2 .OR. &
            config_flags%bl_pbl_physics == MYNNPBLSCHEME3 ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
       ENDIF
#endif

        CALL g_first_rk_step_part2 (   grid , config_flags   &
               , moist ,g_moist , moist_tend ,g_moist_tend   &
!!!!! USE THE STATEMENTS REMARKED WHEN chem is NEEDED. Ning Pan, 2010-08-20
!               , chem  ,g_chem  , chem_tend  ,g_chem_tend    &
               , chem  ,  chem  , chem_tend  ,  chem_tend    &
               , tracer,  g_tracer, tracer_tend, g_tracer_tend  &
               , scalar,g_scalar, scalar_tend,g_scalar_tend  &
!!!!! USE THE STATEMENT REMARKED WHEN fdda3d and fdda2d ARE NEEDED. Ning Pan, 2010-08-20
!               , fdda3d,g_fdda3d, fdda2d,g_fdda2d            &
!!!!! REMOVE THE FOLLOWING STATEMENT WHEN fdda3d and fdda2d ARE NEEDED. Ning Pan, 2010-08-20
               , fdda3d,  fdda3d, fdda2d,  fdda2d            &
               , ru_tendf,g_ru_tendf, rv_tendf,g_rv_tendf    &
               , rw_tendf,g_rw_tendf, t_tendf ,g_t_tendf     &
               , ph_tendf,g_ph_tendf, mu_tendf,g_mu_tendf    &
               , tke_tend,g_tke_tend              &
               , adapt_step_flag , curr_secs      &
!!!!! USE THE STATEMENTS REMARKED WHEN CODING TL OF PHYSICS. Ning Pan, 2010-08-20
!               , psim ,g_psim , psih ,g_psih , wspd ,g_wspd ,        &
!                 gz1oz0 ,g_gz1oz0 , br ,g_br , chklowq,g_chklowq     &
!               , cu_act_flag , hol ,g_hol, th_phy,g_th_phy           &
!!!!! REMOVE THE FOLLOWING 3 STATEMENTS WHEN CODING TL OF PHYSICS. Ning Pan, 2010-08-20
               , psim ,  psim , psih ,  psih ,         &
!201602                 gz1oz0 ,  gz1oz0 , br ,  br , chklowq,  chklowq      &
!201602: br became a state variable and was removed from the argument
                 gz1oz0 ,  gz1oz0 , chklowq,  chklowq      &
               , cu_act_flag , hol ,  hol, th_phy,g_th_phy           &
               , pi_phy ,g_pi_phy, p_phy ,g_p_phy , grid%t_phy ,grid%g_t_phy   &
               , dz8w ,g_dz8w , p8w ,g_p8w , t8w ,g_t8w              &
               , nba_mij,g_nba_mij, num_nba_mij   &
               , nba_rij,g_nba_rij, num_nba_rij   &
               , ids, ide, jds, jde, kds, kde     &
               , ims, ime, jms, jme, kms, kme     &
               , ips, ipe, jps, jpe, kps, kpe     &
               , imsx, imex, jmsx, jmex, kmsx, kmex    &
               , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
               , imsy, imey, jmsy, jmey, kmsy, kmey    &
               , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
               , k_start , k_end                  &
              )

     END IF rk_step_is_one

BENCH_START(g_rk_tend_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call g_rk_tendency' )
       CALL g_rk_tendency ( config_flags, rk_step, &
                         grid%ru_tend, grid%g_ru_tend, grid%rv_tend, grid%g_rv_tend, &
                         rw_tend, g_rw_tend, ph_tend, g_ph_tend, t_tend, g_t_tend, &
                         ru_tendf, g_ru_tendf, rv_tendf, g_rv_tendf, &
                         rw_tendf, g_rw_tendf, ph_tendf, g_ph_tendf, t_tendf, g_t_tendf, &
                         mu_tend, g_mu_tend, &
                         grid%u_save, grid%g_u_save, grid%v_save, grid%g_v_save, &
                         w_save, g_w_save, ph_save, g_ph_save, &
                         grid%t_save, grid%g_t_save, mu_save, g_mu_save, &
                         grid%rthften, grid%g_rthften,    &
                         grid%ru, grid%g_ru, grid%rv, grid%g_rv, grid%rw, grid%g_rw, grid%ww, grid%g_ww, &
                         grid%u_2, grid%g_u_2, grid%v_2, grid%g_v_2, grid%w_2, grid%g_w_2, &
                         grid%t_2, grid%g_t_2, grid%ph_2, grid%g_ph_2, &
                         grid%u_1, grid%g_u_1, grid%v_1, grid%g_v_1, grid%w_1, grid%g_w_1, &
                         grid%t_1, grid%g_t_1, grid%ph_1, grid%g_ph_1, &
                         grid%h_diabatic, grid%g_h_diabatic, grid%phb, grid%t_init, &
                         grid%mu_2, grid%g_mu_2, grid%mut, grid%g_mut, grid%muu, grid%g_muu, &
                         grid%muv, grid%g_muv, grid%mub, &
                         grid%al, grid%g_al, grid%alt, grid%g_alt, grid%p, grid%g_p, grid%pb, &
                         grid%php, grid%g_php, cqu, g_cqu, cqv, g_cqv, cqw, g_cqw, &
                         grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base, &
                         grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                         grid%msfvy, grid%msftx,grid%msfty, grid%clat, grid%f, grid%e, grid%sina, grid%cosa, &
                         grid%fnm, grid%fnp, grid%rdn, grid%rdnw, &
                         grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, &
                         grid%xkmh,grid%g_xkmh, grid%xkhh,grid%g_xkhh,        &
                         grid%diff_6th_opt, grid%diff_6th_factor, &
                         config_flags%momentum_adv_opt, &
                         grid%dampcoef,grid%zdamp,config_flags%damp_opt,config_flags%rad_nudge, &
                         grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m, &
                         config_flags%non_hydrostatic, config_flags%top_lid, &
                         grid%u_frame, grid%v_frame, &
                         ids, ide, jds, jde, kds, kde,   &
                         ims, ime, jms, jme, kms, kme,   &
                         grid%i_start(ij), grid%i_end(ij),  &
                         grid%j_start(ij), grid%j_end(ij),  &
                         k_start, k_end,   &
                         max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij) )

     END DO
     !$OMP END PARALLEL DO
BENCH_END(g_rk_tend_tim)

     IF (config_flags%use_adaptive_time_step) THEN
       DO ij = 1 , grid%num_tiles
         IF (max_horiz_cfl_tmp(ij) .GT. grid%max_horiz_cfl) THEN
           grid%max_horiz_cfl = max_horiz_cfl_tmp(ij)
         ENDIF
         IF (max_vert_cfl_tmp(ij) .GT. grid%max_vert_cfl) THEN
           grid%max_vert_cfl = max_vert_cfl_tmp(ij)
         ENDIF
       END DO

       IF (grid%max_horiz_cfl .GT. grid%max_cfl_val) THEN
         grid%max_cfl_val = grid%max_horiz_cfl
       ENDIF
       IF (grid%max_vert_cfl .GT. grid%max_cfl_val) THEN
         grid%max_cfl_val = grid%max_vert_cfl
       ENDIF
     ENDIF

BENCH_START(g_relax_bdy_dry_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN

         CALL g_relax_bdy_dry ( config_flags,                             &
                  grid%u_save, grid%g_u_save, grid%v_save, grid%g_v_save, &
                  ph_save, g_ph_save, grid%t_save, grid%g_t_save,  &
                  w_save, g_w_save, mu_tend, g_mu_tend,            &
                  grid%ru, grid%g_ru, grid%rv, grid%g_rv,          &
                  grid%ph_2, grid%g_ph_2, grid%t_2, grid%g_t_2,    &
                  grid%w_2, grid%g_w_2, grid%mu_2, grid%g_mu_2, grid%mut, grid%g_mut, &
                  grid%u_bxs, grid%g_u_bxs, grid%u_bxe, grid%g_u_bxe, &
                  grid%u_bys, grid%g_u_bys, grid%u_bye, grid%g_u_bye, &
                  grid%v_bxs, grid%g_v_bxs, grid%v_bxe, grid%g_v_bxe, &
                  grid%v_bys, grid%g_v_bys, grid%v_bye, grid%g_v_bye, &
                  grid%ph_bxs, grid%g_ph_bxs, grid%ph_bxe, grid%g_ph_bxe, &
                  grid%ph_bys, grid%g_ph_bys, grid%ph_bye, grid%g_ph_bye, &
                  grid%t_bxs, grid%g_t_bxs, grid%t_bxe, grid%g_t_bxe, &
                  grid%t_bys, grid%g_t_bys, grid%t_bye, grid%g_t_bye, &
                  grid%w_bxs, grid%g_w_bxs, grid%w_bxe, grid%g_w_bxe, &
                  grid%w_bys, grid%g_w_bys, grid%w_bye, grid%g_w_bye, &
                  grid%mu_bxs, grid%g_mu_bxs, grid%mu_bxe, grid%g_mu_bxe, &
                  grid%mu_bys, grid%g_mu_bys, grid%mu_bye, grid%g_mu_bye, &
                  grid%u_btxs, grid%g_u_btxs, grid%u_btxe, grid%g_u_btxe, &
                  grid%u_btys, grid%g_u_btys, grid%u_btye, grid%g_u_btye, &
                  grid%v_btxs, grid%g_v_btxs, grid%v_btxe, grid%g_v_btxe, &
                  grid%v_btys, grid%g_v_btys, grid%v_btye, grid%g_v_btye, &
                  grid%ph_btxs, grid%g_ph_btxs, grid%ph_btxe, grid%g_ph_btxe, &
                  grid%ph_btys, grid%g_ph_btys, grid%ph_btye, grid%g_ph_btye, &
                  grid%t_btxs, grid%g_t_btxs, grid%t_btxe, grid%g_t_btxe, &
                  grid%t_btys, grid%g_t_btys, grid%t_btye, grid%g_t_btye, &
                  grid%w_btxs, grid%g_w_btxs, grid%w_btxe, grid%g_w_btxe, &
                  grid%w_btys, grid%g_w_btys, grid%w_btye, grid%g_w_btye, &
                  grid%mu_btxs, grid%g_mu_btxs, grid%mu_btxe, grid%g_mu_btxe, &
                  grid%mu_btys, grid%g_mu_btys, grid%mu_btye, grid%g_mu_btye, &
                  config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                  grid%dtbc, grid%fcx, grid%gcx,      &
                  ids,ide, jds,jde, kds,kde,          &
                  ims,ime, jms,jme, kms,kme,          &
                  ips,ipe, jps,jpe, kps,kpe,          &
                  grid%i_start(ij), grid%i_end(ij),   &
                  grid%j_start(ij), grid%j_end(ij),   &
                  k_start, k_end )

       ENDIF

       CALL g_rk_addtend_dry ( grid%ru_tend, grid%g_ru_tend,  &
                               grid%rv_tend, grid%g_rv_tend,  &
                    rw_tend, g_rw_tend, ph_tend, g_ph_tend, t_tend, g_t_tend,         &
                    ru_tendf, g_ru_tendf, rv_tendf, g_rv_tendf, rw_tendf, g_rw_tendf, &
                    ph_tendf, g_ph_tendf, t_tendf, g_t_tendf,                         &
                    grid%u_save, grid%g_u_save, grid%v_save, grid%g_v_save,           &
                    w_save, g_w_save, ph_save, g_ph_save, grid%t_save, grid%g_t_save, &
                    mu_tend, g_mu_tend, mu_tendf, g_mu_tendf, rk_step,        &
                    grid%h_diabatic, grid%g_h_diabatic, grid%mut, grid%g_mut, &
                    grid%msftx, grid%msfty, grid%msfux, grid%msfuy,           &
                    grid%msfvx, grid%msfvx_inv, grid%msfvy,          &
                    ids,ide, jds,jde, kds,kde,                       &
                    ims,ime, jms,jme, kms,kme,                       &
                    ips,ipe, jps,jpe, kps,kpe,                       &
                    grid%i_start(ij), grid%i_end(ij),                &
                    grid%j_start(ij), grid%j_end(ij),                &
                    k_start, k_end )

       IF( config_flags%specified .or. config_flags%nested ) THEN
         CALL g_spec_bdy_dry ( config_flags,                                     &
                     grid%ru_tend, grid%g_ru_tend, grid%rv_tend, grid%g_rv_tend, &
                     ph_tend, g_ph_tend, t_tend, g_t_tend,               &
                     rw_tend, g_rw_tend, mu_tend, g_mu_tend,             &
                     grid%u_bxs, grid%g_u_bxs, grid%u_bxe, grid%g_u_bxe, &
                     grid%u_bys, grid%g_u_bys, grid%u_bye, grid%g_u_bye, &
                     grid%v_bxs, grid%g_v_bxs, grid%v_bxe, grid%g_v_bxe, &
                     grid%v_bys, grid%g_v_bys, grid%v_bye, grid%g_v_bye, &
                     grid%ph_bxs, grid%g_ph_bxs, grid%ph_bxe, grid%g_ph_bxe, &
                     grid%ph_bys, grid%g_ph_bys, grid%ph_bye, grid%g_ph_bye, &
                     grid%t_bxs, grid%g_t_bxs, grid%t_bxe, grid%g_t_bxe, &
                     grid%t_bys, grid%g_t_bys, grid%t_bye, grid%g_t_bye, &
                     grid%w_bxs, grid%g_w_bxs, grid%w_bxe, grid%g_w_bxe, &
                     grid%w_bys, grid%g_w_bys, grid%w_bye, grid%g_w_bye, &
                     grid%mu_bxs, grid%g_mu_bxs, grid%mu_bxe, grid%g_mu_bxe, &
                     grid%mu_bys, grid%g_mu_bys, grid%mu_bye, grid%g_mu_bye, &
                     grid%u_btxs, grid%g_u_btxs, grid%u_btxe, grid%g_u_btxe, &
                     grid%u_btys, grid%g_u_btys, grid%u_btye, grid%g_u_btye, &
                     grid%v_btxs, grid%g_v_btxs, grid%v_btxe, grid%g_v_btxe, &
                     grid%v_btys, grid%g_v_btys, grid%v_btye, grid%g_v_btye, &
                     grid%ph_btxs, grid%g_ph_btxs, grid%ph_btxe, grid%g_ph_btxe, &
                     grid%ph_btys, grid%g_ph_btys, grid%ph_btye, grid%g_ph_btye, &
                     grid%t_btxs, grid%g_t_btxs, grid%t_btxe, grid%g_t_btxe, &
                     grid%t_btys, grid%g_t_btys, grid%t_btye, grid%g_t_btye, &
                     grid%w_btxs, grid%g_w_btxs, grid%w_btxe, grid%g_w_btxe, &
                     grid%w_btys, grid%g_w_btys, grid%w_btye, grid%g_w_btye, &
                     grid%mu_btxs, grid%g_mu_btxs, grid%mu_btxe, grid%g_mu_btxe, &
                     grid%mu_btys, grid%g_mu_btys, grid%mu_btye, grid%g_mu_btye, &
                     config_flags%spec_bdy_width, grid%spec_zone,                &
                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                     grid%i_start(ij), grid%i_end(ij),  &
                     grid%j_start(ij), grid%j_end(ij),  &
                     k_start, k_end )

       ENDIF

     END DO
     !$OMP END PARALLEL DO
BENCH_END(g_relax_bdy_dry_tim)

!<DESCRIPTION>
!<pre>
! (3) Small (acoustic,sound) steps.
!
!    Several acoustic steps are taken each RK pass.  A small step
!    sequence begins with calculating perturbation variables
!    and coupling them to the column dry-air-mass mu
!    (call to small_step_prep).  This is followed by computing
!    coefficients for the vertically implicit part of the
!    small timestep (call to calc_coef_w).
!
!    The small steps are taken
!    in the named loop "small_steps:".  In the small_steps loop, first
!    the horizontal momentum (u and v) are advanced (call to advance_uv),
!    next mu and theta are advanced (call to advance_mu_t) followed by
!    advancing w and the geopotential (call to advance_w).  Diagnostic
!    values for pressure and inverse density are updated at the end of
!    each small_step.
!
!    The small-step section ends with the change of the perturbation variables
!    back to full variables (call to small_step_finish).
!</pre>
!</DESCRIPTION>

BENCH_START(g_small_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first
    ! predictor because we may have changed the small timestep -> grid%dts.

       CALL wrf_debug ( 200 , ' call g_small_step_prep ' )

       CALL g_small_step_prep( grid%u_1,grid%g_u_1,grid%u_2,grid%g_u_2, &
                               grid%v_1,grid%g_v_1,grid%v_2,grid%g_v_2, &
                               grid%w_1,grid%g_w_1,grid%w_2,grid%g_w_2, &
                               grid%t_1,grid%g_t_1,grid%t_2,grid%g_t_2, &
                               grid%ph_1,grid%g_ph_1,grid%ph_2,grid%g_ph_2, &
                               grid%mub, grid%mu_1,grid%g_mu_1, grid%mu_2,grid%g_mu_2,  &
                               grid%muu,grid%g_muu, grid%muus,grid%g_muus,  &
                               grid%muv,grid%g_muv, grid%muvs,grid%g_muvs,  &
                               grid%mut,grid%g_mut, grid%muts,grid%g_muts, grid%mudf,grid%g_mudf,  &
                               grid%u_save,grid%g_u_save, grid%v_save,grid%g_v_save, w_save,g_w_save, &
                               grid%t_save,grid%g_t_save, ph_save,g_ph_save, mu_save,g_mu_save,       &
                               grid%ww,grid%g_ww, ww1,g_ww1,                                          &
                               c2a,g_c2a, grid%pb, grid%p,grid%g_p, grid%alt,grid%g_alt,&
                               grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,       &
                               grid%msfvy, grid%msftx,grid%msfty,                       &
                               grid%rdx, grid%rdy, rk_step,                             &
                               ids, ide, jds, jde, kds, kde,                            &
                               ims, ime, jms, jme, kms, kme,                            &
                               grid%i_start(ij), grid%i_end(ij),                        &
                               grid%j_start(ij), grid%j_end(ij),                        &
                               k_start, k_end )

       CALL g_calc_p_rho( grid%al,grid%g_al, grid%p,grid%g_p, grid%ph_2,grid%g_ph_2, &
                          grid%alt,grid%g_alt, grid%t_2,grid%g_t_2,   &
                          grid%t_save,grid%g_t_save, c2a,g_c2a, pm1,g_pm1,  &
                          grid%mu_2,grid%g_mu_2, grid%muts,grid%g_muts, grid%znu, t0, & 
                          grid%rdnw, grid%dnw, grid%smdiv,            &
                          config_flags%non_hydrostatic, 0,            &
                          ids, ide, jds, jde, kds, kde,               &
                          ims, ime, jms, jme, kms, kme,               &
                          grid%i_start(ij), grid%i_end(ij),           &
                          grid%j_start(ij), grid%j_end(ij),           &
                          k_start, k_end )

       IF (config_flags%non_hydrostatic) THEN
         CALL g_calc_coef_w( a,g_a,alpha,g_alpha,gamma,g_gamma, &
                             grid%mut,grid%g_mut, cqw,g_cqw,    &
                             grid%rdn, grid%rdnw, c2a,g_c2a,    &
                             dts_rk, g, grid%epssm,            &
                             config_flags%top_lid,             &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start, k_end )

       ENDIF

     ENDDO
     !$OMP END PARALLEL DO
BENCH_END(g_small_step_prep_tim)

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the small size of this halo exchange reflects the
!         fact that we are carrying the uncoupled variables
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * *
!            *        * * *    * * * * *
!                              * * * * *
!
!  3D variables - note staggering!  ph_2(Z), u_save(X), v_save(Y)
!
!  ph_2      x
!  al        x
!  p         x
!  t_1       x
!  t_save    x
!  u_save    x
!  v_save    x
!
!  the following are 2D (xy) variables
!
!  mu_1      x
!  mu_2      x
!  mudf      x
!  php       x
!  alt       x
!  pb        x
!--------------------------------------------------------------
#      include "HALO_EM_B_TL.inc"
#      include "PERIOD_BDY_EM_B.inc"
#endif

BENCH_START(g_set_phys_bc2_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL g_set_physical_bc3d( grid%ru_tend,grid%g_ru_tend, 'u', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%rv_tend,grid%g_rv_tend, 'v', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%ph_2,grid%g_ph_2, 'w', config_flags,         &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%al,grid%g_al, 'p', config_flags,           &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%p,grid%g_p, 'p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%t_1,grid%g_t_1, 'p', config_flags,          &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc3d( grid%t_save,grid%g_t_save, 't', config_flags,       &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL g_set_physical_bc2d( grid%mu_1,grid%g_mu_1, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL g_set_physical_bc2d( grid%mu_2,grid%g_mu_2, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL g_set_physical_bc2d( grid%mudf,grid%g_mudf, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

     END DO
     !$OMP END PARALLEL DO
BENCH_END(g_set_phys_bc2_tim)
     small_steps : DO iteration = 1 , number_of_small_timesteps

       ! Boundary condition time (or communication time).
#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_B.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

BENCH_START(g_advance_uv_tim)
         CALL g_advance_uv( grid%u_2,grid%g_u_2, grid%ru_tend,grid%g_ru_tend, &
                            grid%v_2,grid%g_v_2, grid%rv_tend,grid%g_rv_tend, &
                            grid%p,grid%g_p, grid%pb,                         &
                            grid%ph_2,grid%g_ph_2, grid%php,grid%g_php,       &
                            grid%alt,grid%g_alt, grid%al,grid%g_al,           &
                            grid%mu_2,grid%g_mu_2,                            &
                            grid%muu,grid%g_muu, cqu,g_cqu, grid%muv,grid%g_muv, cqv,g_cqv, &
                            grid%mudf,grid%g_mudf,                                 &
                            grid%msfux, grid%msfuy, grid%msfvx,                    &
                            grid%msfvx_inv, grid%msfvy,                            &
                            grid%rdx, grid%rdy, dts_rk,                            &
                            grid%cf1, grid%cf2, grid%cf3, grid%fnm, grid%fnp,      &
                            grid%emdiv,                                            &
                            grid%rdnw, config_flags,grid%spec_zone,                &
                            config_flags%non_hydrostatic, config_flags%top_lid,    &
                            ids, ide, jds, jde, kds, kde,                          &
                            ims, ime, jms, jme, kms, kme,                          &
                            grid%i_start(ij), grid%i_end(ij),                      &
                            grid%j_start(ij), grid%j_end(ij),                      &
                            k_start, k_end )
BENCH_END(g_advance_uv_tim)

       END DO
       !$OMP END PARALLEL DO

!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep u, v
!-----------------------------------------------------------

       IF (config_flags%polar) THEN


         CALL pxft ( grid=grid                                              &
               ,lineno=__LINE__                                             &
               ,flag_uv            = 1                                      &
               ,flag_rurv          = 0                                      &
               ,flag_wph           = 0                                      &
               ,flag_ww            = 0                                      &
               ,flag_t             = 0                                      &
               ,flag_mu            = 0                                      &
               ,flag_mut           = 0                                      &
               ,flag_moist         = 0                                      &
               ,flag_chem          = 0                                      &
               ,flag_tracer        = 0                                      &
               ,flag_scalar        = 0                                      &
               ,actual_distance_average  = .FALSE.                          &
               ,pos_def            = .FALSE.                                &
               ,swap_pole_with_next_j = .FALSE.                             &
               ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
               ,fft_filter_lat = config_flags%fft_filter_lat                &
               ,dclat = dclat                                               &
               ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
               ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
               ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
               ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
               ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep u, v
!-----------------------------------------------------------

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

BENCH_START(g_spec_bdy_uv_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN
           CALL g_spec_bdyupdate ( grid%u_2, grid%g_u_2,       &
                                   grid%ru_tend, grid%g_ru_tend, dts_rk, &
                                   'u'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )

           CALL g_spec_bdyupdate ( grid%v_2, grid%g_v_2,       &
                                   grid%rv_tend, grid%g_rv_tend, dts_rk, &
                                   'v'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )

         ENDIF
BENCH_END(g_spec_bdy_uv_tim)

       END DO
       !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!         *                     *
!       * + *      * + *        +
!         *                     *
!
!  u_2               x
!  v_2                          x
!
#     include "HALO_EM_C_TL.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate ww

BENCH_START(g_advance_mu_t_tim)
         CALL g_advance_mu_t( grid%ww,grid%g_ww, ww1,g_ww1, &
                              grid%u_2,grid%g_u_2, grid%u_save,grid%g_u_save, &
                              grid%v_2,grid%g_v_2, grid%v_save,grid%g_v_save, &
                              grid%mu_2,grid%g_mu_2, grid%mut,grid%g_mut,     &
                              muave,g_muave, grid%muts,grid%g_muts,           &
                              grid%muu,grid%g_muu, grid%muv,grid%g_muv, grid%mudf,grid%g_mudf,     &
                              grid%ru_m,grid%g_ru_m, grid%rv_m,grid%g_rv_m, grid%ww_m,grid%g_ww_m, &
                              grid%t_2,grid%g_t_2, grid%t_save,grid%g_t_save, &
                              t_2save,g_t_2save, t_tend,g_t_tend,             &
                              mu_tend,g_mu_tend,                              &
                              grid%rdx, grid%rdy, dts_rk, grid%epssm,                       &
                              grid%dnw, grid%fnm, grid%fnp, grid%rdnw,                      &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,            &
                              grid%msfvy, grid%msftx,grid%msfty,                            &
                              iteration, config_flags,                                      &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end )

BENCH_END(g_advance_mu_t_tim)
       ENDDO
       !$OMP END PARALLEL DO

!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep mu, t
!-----------------------------------------------------------

       IF ( (config_flags%polar) ) THEN

         CALL pxft ( grid=grid                                               &
                ,lineno=__LINE__                                             &
                ,flag_uv            = 0                                      &
                ,flag_rurv          = 0                                      &
                ,flag_wph           = 0                                      &
                ,flag_ww            = 0                                      &
                ,flag_t             = 1                                      &
                ,flag_mu            = 1                                      &
                ,flag_mut           = 0                                      &
                ,flag_moist         = 0                                      &
                ,flag_chem          = 0                                      &
                ,flag_tracer        = 0                                      &
                ,flag_scalar        = 0                                      &
                ,actual_distance_average  = .FALSE.                          &
                ,pos_def            = .FALSE.                                &
                ,swap_pole_with_next_j = .FALSE.                             &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         grid%muts = grid%mut + grid%mu_2  ! reset muts using filtered mu_2

       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep mu, t
!-----------------------------------------------------------

BENCH_START(g_spec_bdy_t_tim)

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL g_spec_bdyupdate ( grid%t_2, grid%g_t_2,       &
                                   t_tend, g_t_tend, dts_rk,   &
                                   't'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )

           CALL g_spec_bdyupdate ( grid%mu_2, grid%g_mu_2,     &
                                   mu_tend, g_mu_tend, dts_rk, &
                                   'm'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, 1  ,1  ,        &
                                   ims,ime, jms,jme, 1  ,1  ,        &
                                   ips,ipe, jps,jpe, 1  ,1  ,        &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   1, 1 )

           CALL g_spec_bdyupdate ( grid%muts, grid%g_muts, mu_tend, g_mu_tend, dts_rk,      &
                                   'm'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                                   ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                                   ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij),         &
                                   grid%j_start(ij), grid%j_end(ij),         &
                                   1, 1 )

         ENDIF
BENCH_END(g_spec_bdy_t_tim)

         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.


BENCH_START(g_advance_w_tim)
         IF ( config_flags%non_hydrostatic ) THEN
           CALL g_advance_w( grid%w_2,grid%g_w_2, rw_tend,g_rw_tend, grid%ww,grid%g_ww, w_save,g_w_save, &
                             grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2,                              &
                             grid%mu_2,grid%mut,grid%g_mut,                            &
                             muave,g_muave, grid%muts,grid%g_muts,                                  &
                             t_2save,g_t_2save, grid%t_2,grid%g_t_2, grid%t_save,grid%g_t_save,     &
                             grid%ph_2,grid%g_ph_2, ph_save,g_ph_save, grid%phb, ph_tend,g_ph_tend, &
                             grid%ht, c2a,g_c2a, cqw,g_cqw, grid%alt,grid%g_alt, grid%alb,          &
                             a,g_a, alpha,g_alpha, gamma,g_gamma,                                   &
                             grid%rdx, grid%rdy, dts_rk, t0, grid%epssm, &
                             grid%dnw, grid%fnm, grid%fnp, grid%rdnw,    &
                             grid%rdn, grid%cf1, grid%cf2, grid%cf3,     &
                             grid%msftx, grid%msfty,                     &
                             config_flags,  config_flags%top_lid,        &
                             ids,ide, jds,jde, kds,kde,                  &
                             ims,ime, jms,jme, kms,kme,                  &
                             grid%i_start(ij), grid%i_end(ij),           &
                             grid%j_start(ij), grid%j_end(ij),           &
                             k_start, k_end )

         ENDIF
BENCH_END(g_advance_w_tim)

       ENDDO
       !$OMP END PARALLEL DO

!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep w, geopotential
!-----------------------------------------------------------

       IF ( (config_flags%polar) .AND. (config_flags%non_hydrostatic) ) THEN

         CALL pxft ( grid=grid                                               &
                ,lineno=__LINE__                                             &
                ,flag_uv            = 0                                      &
                ,flag_rurv          = 0                                      &
                ,flag_wph           = 1                                      &
                ,flag_ww            = 0                                      &
                ,flag_t             = 0                                      &
                ,flag_mu            = 0                                      &
                ,flag_mut           = 0                                      &
                ,flag_moist         = 0                                      &
                ,flag_chem          = 0                                      &
                ,flag_tracer        = 0                                      &
                ,flag_scalar        = 0                                      &
                ,actual_distance_average  = .FALSE.                          &
                ,pos_def            = .FALSE.                                &
                ,swap_pole_with_next_j = .FALSE.                             &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep w, geopotential
!-----------------------------------------------------------

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

BENCH_START(g_sumflux_tim)
         CALL g_sumflux ( grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2, grid%ww,grid%g_ww,            &
                          grid%u_save, grid%g_u_save, grid%v_save, grid%g_v_save, ww1, g_ww1,     &
                          grid%muu, grid%g_muu, grid%muv, grid%g_muv,                             &
                          grid%ru_m, grid%g_ru_m, grid%rv_m, grid%g_rv_m, grid%ww_m, grid%g_ww_m, &
                          grid%epssm,                          &
                          grid%msfux, grid% msfuy, grid%msfvx,  &
                          grid%msfvx_inv, grid%msfvy,           &
                          iteration, number_of_small_timesteps, &
                          ids, ide, jds, jde, kds, kde,         &
                          ims, ime, jms, jme, kms, kme,         &
                          grid%i_start(ij), grid%i_end(ij),     &
                          grid%j_start(ij), grid%j_end(ij),     &
                          k_start    , k_end                   )
BENCH_END(g_sumflux_tim)

         IF( config_flags%specified .or. config_flags%nested ) THEN

BENCH_START(g_spec_bdynhyd_tim)
           IF (config_flags%non_hydrostatic)  THEN
             CALL g_spec_bdyupdate_ph ( ph_save, g_ph_save, grid%ph_2, grid%g_ph_2,       &
                                        ph_tend, g_ph_tend,              &
                                        mu_tend, g_mu_tend, grid%muts, grid%g_muts, dts_rk, &
                                        'h'         , config_flags,      &
                                        grid%spec_zone,                  &
                                        ids,ide, jds,jde, kds,kde,       &
                                        ims,ime, jms,jme, kms,kme,       &
                                        ips,ipe, jps,jpe, kps,kpe,       &
                                        grid%i_start(ij), grid%i_end(ij),&
                                        grid%j_start(ij), grid%j_end(ij),&
                                        k_start, k_end )

             IF( config_flags%specified ) THEN
               CALL g_zero_grad_bdy ( grid%w_2, grid%g_w_2,             &
                                      'w'         , config_flags,       &
                                      grid%spec_zone,                   &
                                      ids,ide, jds,jde, kds,kde,        &
                                      ims,ime, jms,jme, kms,kme,        &
                                      ips,ipe, jps,jpe, kps,kpe,        &
                                      grid%i_start(ij), grid%i_end(ij), &
                                      grid%j_start(ij), grid%j_end(ij), &
                                      k_start, k_end )

             ELSE
               CALL g_spec_bdyupdate ( grid%w_2, grid%g_w_2,       &
                                       rw_tend, g_rw_tend, dts_rk, &
                                       'h'         , config_flags, &
                                       grid%spec_zone,             &
                                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                       grid%i_start(ij), grid%i_end(ij), &
                                       grid%j_start(ij), grid%j_end(ij), &
                                       k_start, k_end )
             ENDIF
           ENDIF
BENCH_END(g_spec_bdynhyd_tim)
         ENDIF

BENCH_START(g_cald_p_rho_tim)
         CALL g_calc_p_rho( grid%al, grid%g_al, grid%p, grid%g_p, grid%ph_2, grid%g_ph_2,           &
                            grid%alt, grid%g_alt, grid%t_2, grid%g_t_2, grid%t_save, grid%g_t_save, &
                            c2a, g_c2a, pm1, g_pm1, & 
                            grid%mu_2, grid%g_mu_2, grid%muts, grid%g_muts, grid%znu, t0,           &
                            grid%rdnw, grid%dnw, grid%smdiv,            &
                            config_flags%non_hydrostatic, iteration,    &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start, k_end )
BENCH_END(g_cald_p_rho_tim)

       ENDDO
       !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!         *                     *
!       * + *      * + *        +
!         *                     *
!
!  ph_2   x
!  al     x
!  p      x
!
!  2D variables (x,y)
!
!  mu_2   x
!  muts   x
!  mudf   x

#      include "HALO_EM_C2_TL.inc"
#      include "PERIOD_BDY_EM_B3.inc"
#endif

BENCH_START(g_phys_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

       ! boundary condition set for next small timestep

         CALL g_set_physical_bc3d( grid%ph_2,grid%g_ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL g_set_physical_bc3d( grid%al,grid%g_al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL g_set_physical_bc3d( grid%p,grid%g_p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL g_set_physical_bc2d( grid%muts,grid%g_muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL g_set_physical_bc2d( grid%mu_2,grid%g_mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL g_set_physical_bc2d( grid%mudf,grid%g_mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

       END DO
       !$OMP END PARALLEL DO
BENCH_END(g_phys_bc_tim)

     END DO small_steps

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call g_rk_small_finish' )

      ! change time-perturbation variables back to
      ! full perturbation variables.
      ! first get updated mu at u and v points

BENCH_START(g_calc_mu_uv_tim)
       CALL g_calc_mu_uv_1 ( config_flags,                     &
                             grid%muts, grid%g_muts,           &
                             grid%muus, grid%g_muus,           &
                             grid%muvs, grid%g_muvs,           &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start, k_end )
BENCH_END(g_calc_mu_uv_tim)
BENCH_START(g_small_step_finish_tim)
       CALL g_small_step_finish( grid%u_2, grid%g_u_2, grid%u_1, &
                                 grid%v_2, grid%g_v_2, grid%v_1, &
                                 grid%w_2, grid%g_w_2, grid%w_1, &
                                 grid%t_2, grid%g_t_2, grid%t_1, &
                                 grid%ph_2, grid%g_ph_2, grid%ph_1, &
                                 grid%ww, grid%g_ww, ww1, g_ww1,    &
                                 grid%mu_2, grid%g_mu_2, grid%mu_1, &
                                 grid%mut, grid%g_mut, grid%muts, grid%g_muts, &
                                 grid%muu, grid%g_muu, grid%muus, grid%g_muus, &
                                 grid%muv, grid%g_muv, grid%muvs, grid%g_muvs, &
                                 grid%u_save, grid%g_u_save, grid%v_save, grid%g_v_save, w_save, g_w_save, &
                                 grid%t_save, grid%g_t_save, ph_save, g_ph_save, mu_save, g_mu_save,       &
                                 grid%msfux,grid%msfuy,grid%msfvx,grid%msfvy,grid%msftx,grid%msfty, &
                                 grid%h_diabatic, grid%g_h_diabatic, &
                                 number_of_small_timesteps,dts_rk, &
                                 rk_step, rk_order,                &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start, k_end )

!  call  to set ru_m, rv_m and ww_m b.c's for PD advection

       IF (rk_step == rk_order) THEN

         CALL g_set_physical_bc3d( grid%ru_m,grid%g_ru_m, 'u', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL g_set_physical_bc3d( grid%rv_m,grid%g_rv_m, 'v', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL g_set_physical_bc3d( grid%ww_m,grid%g_ww_m, 'w', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL g_set_physical_bc2d( grid%mut,grid%g_mut, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL g_set_physical_bc2d( grid%muts,grid%g_muts, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

       END IF

BENCH_END(g_small_step_finish_tim)

     END DO
     !$OMP END PARALLEL DO

!-----------------------------------------------------------
!  polar filter for full dynamics variables and time-averaged mass fluxes
!-----------------------------------------------------------

     IF (config_flags%polar) THEN

       CALL pxft ( grid=grid                                                   &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 1                                      &
                  ,flag_rurv          = 1                                      &
                  ,flag_wph           = 1                                      &
                  ,flag_ww            = 1                                      &
                  ,flag_t             = 1                                      &
                  ,flag_mu            = 1                                      &
                  ,flag_mut           = 1                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,actual_distance_average  = .FALSE.                          &
                  ,pos_def            = .FALSE.                                &
                  ,swap_pole_with_next_j = .FALSE.                             &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

     END IF

!-----------------------------------------------------------
!  end polar filter for full dynamics variables and time-averaged mass fluxes
!-----------------------------------------------------------

!-----------------------------------------------------------------------
!  add in physics tendency first if positive definite advection is used.
!  pd advection applies advective flux limiter on last runge-kutta step
!-----------------------------------------------------------------------
! first moisture

     IF ((config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call g_rk_update_scalar_pd moist ' )
         DO im = PARAM_FIRST_SCALAR, num_3d_m
           CALL g_rk_update_scalar_pd( im, im,                                        &
                             moist_old(ims,kms,jms,im),g_moist_old(ims,kms,jms,im),   &
                             moist_tend(ims,kms,jms,im),g_moist_tend(ims,kms,jms,im), &
                             grid%mu_1,grid%g_mu_1, grid%mu_1,grid%g_mu_1, grid%mub,  &
                             rk_step, dt_rk, grid%spec_zone,           &
                             config_flags,                             &
                             ids, ide, jds, jde, kds, kde,             &
                             ims, ime, jms, jme, kms, kme,             &
                             grid%i_start(ij), grid%i_end(ij),         &
                             grid%j_start(ij), grid%j_end(ij),         &
                             k_start    , k_end                       )

         ENDDO
       END DO
       !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
       IF (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_MOIST_OLD_E_5_TL.inc"
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_MOIST_OLD_E_7_TL.inc"
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
#endif

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_MOIST_OLD.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
           DO im = PARAM_FIRST_SCALAR , num_3d_m
             CALL g_set_physical_bc3d( moist_old(ims,kms,jms,im),g_moist_old(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,                  &
                                     ims, ime, jms, jme, kms, kme,                  &
                                     ips, ipe, jps, jpe, kps, kpe,                  &
                                     grid%i_start(ij), grid%i_end(ij),              &
                                     grid%j_start(ij), grid%j_end(ij),              &
                                     k_start    , k_end                            )
           END DO
         ENDIF
       END DO
       !$OMP END PARALLEL DO

     END IF  ! end if for moist_adv_opt

! scalars

     IF ((config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call g_rk_update_scalar_pd scalar ' )
         DO im = PARAM_FIRST_SCALAR, num_3d_s
           CALL g_rk_update_scalar_pd( im, im,                                                 &
                                     scalar_old(ims,kms,jms,im),g_scalar_old(ims,kms,jms,im),  &
                                     scalar_tend(ims,kms,jms,im),g_scalar_tend(ims,kms,jms,im),&
                                     grid%mu_1,grid%g_mu_1, grid%mu_1,grid%g_mu_1, grid%mub,   &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                      )
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
       IF (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) THEN
#ifndef RSL
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_SCALAR_OLD_E_5.inc"
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_SCALAR_OLD_E_7.inc"
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
#else
         WRITE(wrf_err_message,*)'cannot use pd scheme with RSL - use RSL-LITE'
         CALL wrf_error_fatal(TRIM(wrf_err_message))
#endif
  endif
#endif

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_SCALAR_OLD.inc"
#endif
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
           IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_3d_s
               CALL g_set_physical_bc3d(  scalar_old(ims,kms,jms,im),g_scalar_old(ims,kms,jms,im), 'p', config_flags, &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       END IF  ! end if for scalar_adv_opt

! chem

       IF ((config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call g_rk_update_scalar_pd chem' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c

!!!!! REPLACE WITH g_rk_update_scalar_pd WHEN chem IS NEEDED. Ning Pan
             CALL rk_update_scalar_pd( im, im,                                  &
                                       chem_old(ims,kms,jms,im),                &
                                       chem_tend(ims,kms,jms,im),               &
                                       grid%c1h, grid%c2h,                      &
                                       grid%mu_1, grid%mu_1, grid%mub, &
                                       rk_step, dt_rk, grid%spec_zone,          &
                                       config_flags,                            &
                                       ids, ide, jds, jde, kds, kde,            &
                                       ims, ime, jms, jme, kms, kme,            &
                                       grid%i_start(ij), grid%i_end(ij),        &
                                       grid%j_start(ij), grid%j_end(ij),        &
                                       k_start    , k_end                      )

           ENDDO
         END DO
         !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_CHEM_OLD_E_5.inc"
           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_CHEM_OLD_E_7.inc"
           ELSE
             WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal(TRIM(wrf_err_message))
           ENDIF
         ENDIF
#endif

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_CHEM_OLD.inc"
#endif

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_3d_c

!!!!! REPLACE WITH g_set_physical_bc3d WHEN chem IS NEEDED. Ning Pan
               CALL set_physical_bc3d(  chem_old(ims,kms,jms,im), 'p', config_flags,     &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO

           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  ! end if for chem_adv_opt

! tracer

       IF ((config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call g_rk_update_scalar_pd tracer' )
           DO im = PARAM_FIRST_SCALAR, num_tracer

             CALL g_rk_update_scalar_pd( im, im,                                  &
                              tracer_old(ims,kms,jms,im),g_tracer_old(ims,kms,jms,im), &
                              tracer_tend(ims,kms,jms,im),g_tracer_tend(ims,kms,jms,im), &
                              grid%mu_1, grid%g_mu_1, grid%mu_1, grid%g_mu_1, grid%mub, &
                              rk_step, dt_rk, grid%spec_zone,          &
                              config_flags,                            &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start    , k_end                      )

           ENDDO
         END DO
         !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_TRACER_OLD_E_5_TL.inc"
           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_TRACER_OLD_E_7_TL.inc"
           ELSE
             WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal(TRIM(wrf_err_message))
           ENDIF
         ENDIF
#endif

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_TRACER_OLD.inc"
#endif

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           IF (num_tracer >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_tracer

               CALL g_set_physical_bc3d(  tracer_old(ims,kms,jms,im), &
                                        g_tracer_old(ims,kms,jms,im), 'p', config_flags,   &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )
             END DO

           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  ! end if for tracer_adv_opt

! tke

       IF ((config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order) &
           .and. (config_flags%km_opt .eq. 2)                ) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call g_rk_update_scalar_pd tke ' )
           CALL g_rk_update_scalar_pd( 1, 1,                                    &
                                     grid%tke_1,grid%g_tke_1,                   &
                                     tke_tend(ims,kms,jms),g_tke_tend(ims,kms,jms),          &
                                     grid%mu_1,grid%g_mu_1, grid%mu_1,grid%g_mu_1, grid%mub, &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                       )
         ENDDO
         !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_TKE_OLD_E_5_TL.inc"
           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_TKE_OLD_E_7_TL.inc"
           ELSE
             WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
             CALL wrf_error_fatal(TRIM(wrf_err_message))
           ENDIF
         ENDIF
#endif

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_TKE_OLD.inc"
#endif

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL g_set_physical_bc3d(  grid%tke_1,grid%g_tke_1, 'p', config_flags,  &
                                    ids, ide, jds, jde, kds, kde,      &
                                    ims, ime, jms, jme, kms, kme,      &
                                    ips, ipe, jps, jpe, kps, kpe,      &
                                    grid%i_start(ij), grid%i_end(ij),  &
                                    grid%j_start(ij), grid%j_end(ij),  &
                                    k_start    , k_end                )
         END DO
         !$OMP END PARALLEL DO

!---  end of positive definite physics tendency update

       END IF  ! end if for tke_adv_opt

#ifdef DM_PARALLEL
!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!          * * * * *
!          * * * * *
!          * * + * *
!          * * * * *
!          * * * * *
!
! ru_m         x
! rv_m         x
! ww_m         x
! mut          x
!
!--------------------------------------------------------------

#  include "HALO_EM_D_TL.inc"
! WCS addition 11/19/08
#  include "PERIOD_EM_DA.inc"
#endif

!<DESCRIPTION>
!<pre>
! (4) Still within the RK loop, the scalar variables are advanced.
!
!    For the moist and chem variables, each one is advanced
!    individually, using named loops "moist_variable_loop:"
!    and "chem_variable_loop:".  Each RK substep begins by
!    calculating the advective tendency, and, for the first RK step,
!    3D mixing (calling rk_scalar_tend) followed by an update
!    of the scalar (calling rk_update_scalar).
!</pre>
!</DESCRIPTION>


       moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

         moist_variable_loop: DO im = PARAM_FIRST_SCALAR, num_3d_m

! adv_moist_cond is set in module_physics_init based on mp_physics choice
!       true except for Ferrier scheme

           IF (grid%adv_moist_cond .or. im==p_qv ) THEN

             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij, tenddec )
             moist_tile_loop_1: DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 200 , ' call g_rk_scalar_tend moist' )
               tenddec = .false.

BENCH_START(g_rk_scalar_tend_tim)
               CALL g_rk_scalar_tend (  im, im, config_flags, tenddec,         &
                           rk_step, dt_rk,                                   &
                           grid%ru_m, grid%g_ru_m, &
                           grid%rv_m, grid%g_rv_m, &
                           grid%ww_m, grid%g_ww_m, &
                           grid%muts, grid%g_muts, grid%mub, grid%mu_1, grid%g_mu_1, &
                           grid%alt, grid%g_alt,                                     &
                           moist_old(ims,kms,jms,im), g_moist_old(ims,kms,jms,im),   &
                           moist(ims,kms,jms,im), g_moist(ims,kms,jms,im),           &
                           moist_tend(ims,kms,jms,im), g_moist_tend(ims,kms,jms,im), &
                           advect_tend, g_advect_tend, h_tendency, g_h_tendency,     &
                           z_tendency, g_z_tendency, grid%rqvften, grid%g_rqvften,   &
                           grid%qv_base, .true., grid%fnm, grid%fnp,         &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,&
                           grid%msfvy, grid%msftx,grid%msfty,                &
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,grid%g_xkhh,                &
                           grid%diff_6th_opt, grid%diff_6th_factor,          &
                           config_flags%moist_adv_opt,                       &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

!              IF( rk_step == 1 .AND. config_flags%use_q_diabatic == 1 )THEN
!              IF( im.eq.p_qv .or. im.eq.p_qc )THEN
!                CALL g_q_diabatic_add  ( im, im,                 &
!                          dt_rk, grid%mut, grid%g_mut,           &
!                          grid%qv_diabatic, grid%g_qv_diabatic,  &
!                          grid%qc_diabatic, grid%g_qc_diabatic,  &
!                          moist_tend(ims,kms,jms,im),            &
!                          g_moist_tend(ims,kms,jms,im),          &
!                          ids, ide, jds, jde, kds, kde,          &
!                          ims, ime, jms, jme, kms, kme,          &
!                          grid%i_start(ij), grid%i_end(ij),      &
!                          grid%j_start(ij), grid%j_end(ij),      &
!                          k_start    , k_end               )
!              ENDIF
!              ENDIF

BENCH_END(g_rk_scalar_tend_tim)

BENCH_START(g_rlx_bdy_scalar_tim)
               IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN
                 IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN
                   CALL g_relax_bdy_scalar ( moist_tend(ims,kms,jms,im), g_moist_tend(ims,kms,jms,im),     &
                                             moist(ims,kms,jms,im), g_moist(ims,kms,jms,im), &
                                             grid%mut, grid%g_mut, &
                                             moist_bxs(jms,kms,1,im),g_moist_bxs(jms,kms,1,im), &
                                             moist_bxe(jms,kms,1,im),g_moist_bxe(jms,kms,1,im), & 
                                             moist_bys(ims,kms,1,im),g_moist_bys(ims,kms,1,im), &
                                             moist_bye(ims,kms,1,im),g_moist_bye(ims,kms,1,im), &
                                             moist_btxs(jms,kms,1,im),g_moist_btxs(jms,kms,1,im), &
                                             moist_btxe(jms,kms,1,im),g_moist_btxe(jms,kms,1,im), & 
                                             moist_btys(ims,kms,1,im),g_moist_btys(ims,kms,1,im), &
                                             moist_btye(ims,kms,1,im),g_moist_btye(ims,kms,1,im), &
                                             config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                             grid%dtbc, grid%fcx, grid%gcx,  &
                                             config_flags,               &
                                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                             grid%i_start(ij), grid%i_end(ij),      &
                                             grid%j_start(ij), grid%j_end(ij),      &
                                             k_start, k_end )

                   CALL g_spec_bdy_scalar ( moist_tend(ims,kms,jms,im), g_moist_tend(ims,kms,jms,im), &
                                     moist_bxs(jms,kms,1,im),g_moist_bxs(jms,kms,1,im), &
                                     moist_bxe(jms,kms,1,im),g_moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),g_moist_bys(ims,kms,1,im), &
                                     moist_bye(ims,kms,1,im),g_moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),g_moist_btxs(jms,kms,1,im), &
                                     moist_btxe(jms,kms,1,im),g_moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),g_moist_btys(ims,kms,1,im), &
                                     moist_btye(ims,kms,1,im),g_moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone,                 &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),          &
                                     grid%j_start(ij), grid%j_end(ij),          &
                                     k_start, k_end                               )

                 ENDIF
               ENDIF
BENCH_END(g_rlx_bdy_scalar_tim)

             ENDDO moist_tile_loop_1
             !$OMP END PARALLEL DO

             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij, tenddec )
             moist_tile_loop_2: DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 200 , ' call g_rk_update_scalar' )
               tenddec = .false.

BENCH_START(g_update_scal_tim)
               CALL g_rk_update_scalar( scs=im, sce=im,                                &
                               scalar_1=moist_old(ims,kms,jms,im),                     &
                               g_scalar_1=g_moist_old(ims,kms,jms,im),                 &
                               scalar_2=moist(ims,kms,jms,im),                         &
                               g_scalar_2=g_moist(ims,kms,jms,im),                     &
                               sc_tend=moist_tend(ims,kms,jms,im),                     &
                               g_sc_tend=g_moist_tend(ims,kms,jms,im),                 &
                               advect_tend=advect_tend,g_advect_tend=g_advect_tend,    &
                               h_tendency=h_tendency, g_h_tendency=g_h_tendency,       & 
                               z_tendency=z_tendency, g_z_tendency=g_z_tendency,       & 
                               msftx=grid%msftx,msfty=grid%msfty,                      &
                               mu_old=grid%mu_1, g_mu_old=grid%g_mu_1,                 &
                               mu_new=grid%mu_2,g_mu_new=grid%g_mu_2,mu_base=grid%mub, &
                               rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                               config_flags=config_flags, tenddec=tenddec,             & 
                               ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                               ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                               its=grid%i_start(ij), ite=grid%i_end(ij),               &
                               jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                               kts=k_start    , kte=k_end                              )

!              IF( rk_step == rk_order .AND. config_flags%use_q_diabatic == 1 )THEN
!              IF( im.eq.p_qv .or. im.eq.p_qc )THEN
!                CALL g_q_diabatic_subtr( im, im,                &
!                          dt_rk,                                &
!                          grid%qv_diabatic, grid%g_qv_diabatic, &
!                          grid%qc_diabatic, grid%g_qc_diabatic, &
!                          moist(ims,kms,jms,im),                &
!                          g_moist(ims,kms,jms,im),              &
!                          ids, ide, jds, jde, kds, kde,         &
!                          ims, ime, jms, jme, kms, kme,         &
!                          grid%i_start(ij), grid%i_end(ij),     &
!                          grid%j_start(ij), grid%j_end(ij),     &
!                          k_start    , k_end               )
!              ENDIF
!              ENDIF

BENCH_END(g_update_scal_tim)

BENCH_START(g_flow_depbdy_tim)
               IF( config_flags%specified ) THEN
                 IF(im .ne. P_QV)THEN
                   CALL g_flow_dep_bdy ( moist(ims,kms,jms,im), g_moist(ims,kms,jms,im),  &
                                         grid%ru_m, grid%rv_m, config_flags, &
                                         grid%spec_zone,                     &
                                         ids,ide, jds,jde, kds,kde,          &
                                         ims,ime, jms,jme, kms,kme,          &
                                         ips,ipe, jps,jpe, kps,kpe,          &
                                         grid%i_start(ij), grid%i_end(ij),   &
                                         grid%j_start(ij), grid%j_end(ij),   &
                                         k_start, k_end )

                 ENDIF
               ENDIF
BENCH_END(g_flow_depbdy_tim)

             ENDDO moist_tile_loop_2
             !$OMP END PARALLEL DO

           ENDIF  !-- if (grid%adv_moist_cond .or. im==p_qv ) then

         ENDDO moist_variable_loop

       ENDIF moist_scalar_advance

BENCH_START(g_tke_adv_tim)
       TKE_advance: IF (config_flags%km_opt .eq. 2) then
#ifdef DM_PARALLEL
         IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_ADVECT_3_TL.inc"
         ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_ADVECT_5_TL.inc"
         ELSE
          WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
          CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
#endif
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         tke_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_rk_scalar_tend for tke' )

           tenddec = .false.
           CALL g_rk_scalar_tend ( 1, 1, config_flags, tenddec,                    &
                            rk_step, dt_rk,                                        &
                            grid%ru_m,grid%g_ru_m, grid%rv_m,grid%g_rv_m, grid%ww_m,grid%g_ww_m, &
                            grid%muts,grid%g_muts, grid%mub, grid%mu_1,grid%g_mu_1,              &
                            grid%alt,grid%g_alt,                                   &
                            grid%tke_1,grid%g_tke_1,                               &
                            grid%tke_2,grid%g_tke_2,                               &
                            tke_tend(ims,kms,jms),g_tke_tend(ims,kms,jms),         &
                            advect_tend,g_advect_tend,h_tendency,g_h_tendency,     &
                            z_tendency,g_z_tendency,grid%rqvften,grid%g_rqvften,   &
                            grid%qv_base, .false., grid%fnm, grid%fnp,             &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,     &
                            grid%msfvy, grid%msftx,grid%msfty,                     &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif,   &
                            grid%kvdif, grid%xkhh,grid%g_xkhh,                     &
                            grid%diff_6th_opt, grid%diff_6th_factor,               &
                            config_flags%tke_adv_opt,                              &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

         ENDDO tke_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         tke_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_rk_update_scalar tke' )

           tenddec = .false.
           CALL g_rk_update_scalar( scs=1,  sce=1,                                         &
                                  scalar_1=grid%tke_1, g_scalar_1=grid%g_tke_1,           &
                                  scalar_2=grid%tke_2, g_scalar_2=grid%g_tke_2,           &
                                  sc_tend=tke_tend(ims,kms,jms),                          &
                                  g_sc_tend=g_tke_tend(ims,kms,jms),                      &
                                  advect_tend=advect_tend,g_advect_tend=g_advect_tend,    &
                                  h_tendency=h_tendency, g_h_tendency=g_h_tendency,       &
                                  z_tendency=z_tendency, g_z_tendency=g_z_tendency,       &
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, g_mu_old=grid%g_mu_1,                 &
                                  mu_new=grid%mu_2,g_mu_new=grid%g_mu_2,mu_base=grid%mub, &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             &
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

! bound the tke (greater than 0, less than tke_upper_bound)

           CALL g_bound_tke( grid%tke_2, grid%g_tke_2, grid%tke_upper_bound,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           grid%i_start(ij), grid%i_end(ij),    &
                           grid%j_start(ij), grid%j_end(ij),    &
                           k_start    , k_end                  )

           IF( config_flags%specified .or. config_flags%nested ) THEN
              CALL g_flow_dep_bdy ( grid%tke_2,grid%g_tke_2,    &
                                    grid%ru_m, grid%rv_m, config_flags, &
                                    grid%spec_zone,                     &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),   &
                                    grid%j_start(ij), grid%j_end(ij),   &
                                    k_start, k_end )

           ENDIF
         ENDDO tke_tile_loop_2
         !$OMP END PARALLEL DO

       ENDIF TKE_advance
BENCH_END(g_tke_adv_tim)

#if (WRF_CHEM==1)
!  next the chemical species
BENCH_START(g_chem_adv_tim)
       chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

         chem_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_3d_c

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij, tenddec )
           chem_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 200 , ' call g_rk_scalar_tend in chem_tile_loop_1' )

!!!!! REPLACE WITH g_rk_scalar_tend WHEN chem IS NEEDED. Ning Pan
             tenddec = (( config_flags%chemdiag == USECHEMDIAG ) .and. &
                        ( adv_ct_indices(ic) >= PARAM_FIRST_SCALAR ))
             CALL rk_scalar_tend ( ic, ic, config_flags,tenddec,                 &
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%alt,                                          &
                              chem_old(ims,kms,jms,ic),                          &
                              chem(ims,kms,jms,ic),                              &
                              chem_tend(ims,kms,jms,ic),                         &
                              advect_tend,h_tendency,z_tendency,grid%rqvften,    &
                              grid%qv_base, .false., grid%fnm, grid%fnp,         &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx,grid%msfty,                 &
                              grid%rdx, grid%rdy, grid%rdn, grid%rdnw,           &
                              grid%khdif, grid%kvdif, grid%xkhh,                 &
                              grid%diff_6th_opt, grid%diff_6th_factor,           &
                              config_flags%chem_adv_opt,                         &
                              ids, ide, jds, jde, kds, kde,                      &
                              ims, ime, jms, jme, kms, kme,                      &
                              grid%i_start(ij), grid%i_end(ij),                  &
                              grid%j_start(ij), grid%j_end(ij),                  &
                              k_start    , k_end                                )


!
! Currently, chemistry species with specified boundaries (i.e. the mother
! domain)  are being over written by flow_dep_bdy_chem. So, relax_bdy and
! spec_bdy are only called for nests. For boundary conditions from global model or larger domain,
! chem is uncoupled, and only used for one row/column on inflow (if have_bcs_chem=.true.)
!
           IF( ( config_flags%nested ) .and. rk_step == 1 ) THEN
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_chem' )

!!!!! REPLACE WITH g_relax_bdy_scalar WHEN chem IS NEEDED. Ning Pan
             CALL relax_bdy_scalar ( chem_tend(ims,kms,jms,ic),                                    &
                                     chem(ims,kms,jms,ic),  grid%mut,                              &
                                     grid%c1h, grid%c2h,                                           &
                                     chem_bxs(jms,kms,1,ic),chem_bxe(jms,kms,1,ic),                &
                                     chem_bys(ims,kms,1,ic),chem_bye(ims,kms,1,ic),                &
                                     chem_btxs(jms,kms,1,ic),chem_btxe(jms,kms,1,ic),              &
                                     chem_btys(ims,kms,1,ic),chem_btye(ims,kms,1,ic),              &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,                                &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )

!!!!! REPLACE WITH g_spec_bdy_scalar WHEN chem IS NEEDED. Ning Pan
             CALL spec_bdy_scalar  ( chem_tend(ims,kms,jms,ic),                 &
                                     chem_bxs(jms,kms,1,ic),chem_bxe(jms,kms,1,ic),                &
                                     chem_bys(ims,kms,1,ic),chem_bye(ims,kms,1,ic),                &
                                     chem_btxs(jms,kms,1,ic),chem_btxe(jms,kms,1,ic),              &
                                     chem_btys(ims,kms,1,ic),chem_btye(ims,kms,1,ic),              &
                                     config_flags%spec_bdy_width, grid%spec_zone,                  &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )

           ENDIF

         ENDDO chem_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )

         chem_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )

!!!!! REPLACE WITH g_rk_update_scalar WHEN chem IS NEEDED. Ning Pan
           tenddec = (( config_flags%chemdiag == USECHEMDIAG ) .and. &
                      ( adv_ct_indices(ic) >= PARAM_FIRST_SCALAR ))
           CALL rk_update_scalar( scs=ic, sce=ic,                                         &
                                  scalar_1=chem_old(ims,kms,jms,ic),                      &
                                  scalar_2=chem(ims,kms,jms,ic),                          &
                                  sc_tend=chem_tend(ims,kms,jms,ic),                      &
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified  ) THEN

!!!!! REPLACE WITH g_flow_dep_bdy_chem WHEN chem IS NEEDED. Ning Pan
             CALL flow_dep_bdy_chem( chem(ims,kms,jms,ic),                          &
                                     chem_bxs(jms,kms,1,ic), chem_btxs(jms,kms,1,ic),  &
                                     chem_bxe(jms,kms,1,ic), chem_btxe(jms,kms,1,ic),  &
                                     chem_bys(ims,kms,1,ic), chem_btys(ims,kms,1,ic),  &
                                     chem_bye(ims,kms,1,ic), chem_btye(ims,kms,1,ic),  &
                                     dt_rk+grid%dtbc,                                  &
                                     config_flags%spec_bdy_width,grid%z,      &
                                     grid%have_bcs_chem,      &
                                     grid%ru_m, grid%rv_m, config_flags,grid%alt,       &
                                     grid%t_1,grid%pb,grid%p,t0,p1000mb,rcp,grid%ph_2,grid%phb,g, &
                                     grid%spec_zone,ic,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),   &
                                     grid%j_start(ij), grid%j_end(ij),   &
                                     k_start, k_end                      )

           ENDIF
         ENDDO chem_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO chem_variable_loop
     ENDIF chem_scalar_advance
BENCH_END(g_chem_adv_tim)
#endif
!  next the chemical species
BENCH_START(g_tracer_adv_tim)
       tracer_advance: IF (num_tracer >= PARAM_FIRST_SCALAR)  THEN

         tracer_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_tracer

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij, tenddec )
           tracer_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 15 , ' call g_rk_scalar_tend in tracer_tile_loop_1' )

             tenddec = .false.
             CALL g_rk_scalar_tend ( ic, ic, config_flags, tenddec,                   &
                             rk_step, dt_rk,         &
                             grid%ru_m, grid%g_ru_m, &
                             grid%rv_m, grid%g_rv_m, &
                             grid%ww_m, grid%g_ww_m, &
                             grid%muts, grid%g_muts, grid%mub, grid%mu_1, grid%g_mu_1, &
                             grid%alt, grid%g_alt,                                     &
                             tracer_old(ims,kms,jms,ic), g_tracer_old(ims,kms,jms,ic),   &
                             tracer(ims,kms,jms,ic), g_tracer(ims,kms,jms,ic),           &
                             tracer_tend(ims,kms,jms,ic), g_tracer_tend(ims,kms,jms,ic), &
                             advect_tend, g_advect_tend, h_tendency,g_h_tendency,        &
                             z_tendency, g_z_tendency, grid%rqvften, grid%g_rqvften, &
                             grid%qv_base, .false., grid%fnm, grid%fnp,       &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                             grid%msfvy, grid%msftx,grid%msfty,               &
                             grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                             grid%khdif, grid%kvdif, grid%xkhh,grid%g_xkhh,   &
                             grid%diff_6th_opt, grid%diff_6th_factor,         &
                             config_flags%tracer_adv_opt,                     &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

!
! Currently, chemistry species with specified boundaries (i.e. the mother
! domain)  are being over written by flow_dep_bdy_chem. So, relax_bdy and
! spec_bdy are only called for nests. For boundary conditions from global model or larger domain,
! chem is uncoupled, and only used for one row/column on inflow (if have_bcs_chem=.true.)
!
           IF( ( config_flags%nested ) .and. rk_step == 1 ) THEN
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_tracer' )

!!!!! REPLACE WITH g_relax_bdy_scalar WHEN tracer IS NEEDED. Ning Pan
             CALL relax_bdy_scalar ( tracer_tend(ims,kms,jms,ic),                                  &
                                     tracer(ims,kms,jms,ic),  grid%mut,                            &
                                     grid%c1h, grid%c2h,                                           &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),            &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),            &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),          &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),          &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,                                &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )

!!!!! REPLACE WITH g_spec_bdy_scalar WHEN tracer IS NEEDED. Ning Pan
             CALL spec_bdy_scalar  ( tracer_tend(ims,kms,jms,ic),                 &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),            &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),            &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),          &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),          &
                                     config_flags%spec_bdy_width, grid%spec_zone,                  &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )

           ENDIF

         ENDDO tracer_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )

         tracer_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_rk_update_scalar tracer ' )

           tenddec = .false.
           CALL g_rk_update_scalar( scs=ic, sce=ic,                                       &
                                  scalar_1=tracer_old(ims,kms,jms,ic),                    &
                                  g_scalar_1=g_tracer_old(ims,kms,jms,ic),                &
                                  scalar_2=tracer(ims,kms,jms,ic),                        &
                                  g_scalar_2=g_tracer(ims,kms,jms,ic),                    &
                                  sc_tend=tracer_tend(ims,kms,jms,ic),                    &
                                  g_sc_tend=g_tracer_tend(ims,kms,jms,ic),                &
                                  advect_tend=advect_tend,g_advect_tend=g_advect_tend,    &
                                  h_tendency=h_tendency, g_h_tendency=g_h_tendency,       & 
                                  z_tendency=z_tendency, g_z_tendency=g_z_tendency,       & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, g_mu_old=grid%g_mu_1,                 &
                                  mu_new=grid%mu_2, g_mu_new=grid%g_mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified  ) THEN
#if (WRF_CHEM==1)

!!!!! REPLACE WITH a_flow_dep_bdy_tracer WHEN tracer IS NEEDED. Ning Pan
             CALL flow_dep_bdy_tracer( tracer(ims,kms,jms,ic),                             &
                                     tracer_bxs(jms,kms,1,ic), tracer_btxs(jms,kms,1,ic),  &
                                     tracer_bxe(jms,kms,1,ic), tracer_btxe(jms,kms,1,ic),  &
                                     tracer_bys(ims,kms,1,ic), tracer_btys(ims,kms,1,ic),  &
                                     tracer_bye(ims,kms,1,ic), tracer_btye(ims,kms,1,ic),  &
                                     dt_rk+grid%dtbc,                                  &
                                     config_flags%spec_bdy_width,grid%z,      &
                                     grid%have_bcs_tracer,      &
                                     grid%ru_m, grid%rv_m, config_flags%tracer_opt,grid%alt,       &
                                     grid%t_1,grid%pb,grid%p,t0,p1000mb,rcp,grid%ph_2,grid%phb,g, &
                                     grid%spec_zone,ic,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),   &
                                     grid%j_start(ij), grid%j_end(ij),   &
                                     k_start, k_end                      )

#else
             CALL g_flow_dep_bdy  ( tracer(ims,kms,jms,ic),g_tracer(ims,kms,jms,ic),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
#endif
           ENDIF
         ENDDO tracer_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO tracer_variable_loop
     ENDIF tracer_advance
BENCH_END(g_tracer_adv_tim)

!  next the other scalar species
     other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

       scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_rk_scalar_tend scalar' )

           tenddec = .false.
           CALL g_rk_scalar_tend ( is, is, config_flags, tenddec,                   &
                           rk_step, dt_rk,         &
                           grid%ru_m, grid%g_ru_m, &
                           grid%rv_m, grid%g_rv_m, &
                           grid%ww_m, grid%g_ww_m, &
                           grid%muts, grid%g_muts, grid%mub, grid%mu_1, grid%g_mu_1, &
                           grid%alt, grid%g_alt,                                     &
                           scalar_old(ims,kms,jms,is), g_scalar_old(ims,kms,jms,is),   &
                           scalar(ims,kms,jms,is), g_scalar(ims,kms,jms,is),           &
                           scalar_tend(ims,kms,jms,is), g_scalar_tend(ims,kms,jms,is), &
                           advect_tend, g_advect_tend, h_tendency,g_h_tendency,        &
                           z_tendency, g_z_tendency, grid%rqvften, grid%g_rqvften, &
                           grid%qv_base, .false., grid%fnm, grid%fnp,       &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                           grid%msfvy, grid%msftx,grid%msfty,               &
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                           grid%khdif, grid%kvdif, grid%xkhh,grid%g_xkhh,   &
                           grid%diff_6th_opt, grid%diff_6th_factor,         &
                           config_flags%scalar_adv_opt,                     &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

           IF( config_flags%nested .and. (rk_step == 1) ) THEN

               CALL g_relax_bdy_scalar ( scalar_tend(ims,kms,jms,is), g_scalar_tend(ims,kms,jms,is),     &
                                       scalar(ims,kms,jms,is), g_scalar(ims,kms,jms,is), &
                                       grid%mut, grid%g_mut, &
                                       scalar_bxs(jms,kms,1,is),g_scalar_bxs(jms,kms,1,is), &
                                       scalar_bxe(jms,kms,1,is),g_scalar_bxe(jms,kms,1,is), & 
                                       scalar_bys(ims,kms,1,is),g_scalar_bys(ims,kms,1,is), &
                                       scalar_bye(ims,kms,1,is),g_scalar_bye(ims,kms,1,is), &
                                       scalar_btxs(jms,kms,1,is),g_scalar_btxs(jms,kms,1,is), &
                                       scalar_btxe(jms,kms,1,is),g_scalar_btxe(jms,kms,1,is), & 
                                       scalar_btys(ims,kms,1,is),g_scalar_btys(ims,kms,1,is), &
                                       scalar_btye(ims,kms,1,is),g_scalar_btye(ims,kms,1,is), &
                                       config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                       grid%dtbc, grid%fcx, grid%gcx,                          &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

               CALL g_spec_bdy_scalar ( scalar_tend(ims,kms,jms,is), g_scalar_tend(ims,kms,jms,is), &
                                       scalar_bxs(jms,kms,1,is),g_scalar_bxs(jms,kms,1,is), &
                                       scalar_bxe(jms,kms,1,is),g_scalar_bxe(jms,kms,1,is), &
                                       scalar_bys(ims,kms,1,is),g_scalar_bys(ims,kms,1,is), &
                                       scalar_bye(ims,kms,1,is),g_scalar_bye(ims,kms,1,is), &
                                       scalar_btxs(jms,kms,1,is),g_scalar_btxs(jms,kms,1,is), &
                                       scalar_btxe(jms,kms,1,is),g_scalar_btxe(jms,kms,1,is), &
                                       scalar_btys(ims,kms,1,is),g_scalar_btys(ims,kms,1,is), &
                                       scalar_btye(ims,kms,1,is),g_scalar_btye(ims,kms,1,is), &
                                       config_flags%spec_bdy_width, grid%spec_zone,            &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )


           ENDIF ! b.c test for chem nested boundary condition

         ENDDO scalar_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_rk_update_scalar scalar' )

           tenddec = .false.
           CALL g_rk_update_scalar( scs=is, sce=is,                                       &
                                  scalar_1=scalar_old(ims,kms,jms,is),                    &
                                  g_scalar_1=g_scalar_old(ims,kms,jms,is),                &
                                  scalar_2=scalar(ims,kms,jms,is),                        &
                                  g_scalar_2=g_scalar(ims,kms,jms,is),                    &
                                  sc_tend=scalar_tend(ims,kms,jms,is),                    &
                                  g_sc_tend=g_scalar_tend(ims,kms,jms,is),                &
                                  advect_tend=advect_tend,g_advect_tend=g_advect_tend,    &
                                  h_tendency=h_tendency, g_h_tendency=g_h_tendency,       & 
                                  z_tendency=z_tendency, g_z_tendency=g_z_tendency,       & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  mu_old=grid%mu_1, g_mu_old=grid%g_mu_1,                 &
                                  mu_new=grid%mu_2,g_mu_new=grid%g_mu_2,mu_base=grid%mub, &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified ) THEN

             IF(is .ne. P_QNN)THEN
               CALL g_flow_dep_bdy  ( scalar(ims,kms,jms,is),g_scalar(ims,kms,jms,is), &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,             &
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
             ELSE
               CALL g_flow_dep_bdy_qnn  ( scalar(ims,kms,jms,is), g_scalar(ims,kms,jms,is), &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  grid%ccn_conc,              & ! RAS
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
             ENDIF

           ENDIF

         ENDDO scalar_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO scalar_variable_loop

     ENDIF other_scalar_advance

 !  update the pressure and density at the new time level

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

BENCH_START(g_calc_p_rho_tim)

       CALL g_calc_p_rho_phi( moist,g_moist, num_3d_m, config_flags%hypsometric_opt,        &
                            grid%al,grid%g_al, grid%alb, grid%mu_2,grid%g_mu_2, grid%muts,grid%g_muts, &
                            grid%ph_2,grid%g_ph_2, grid%phb, grid%p, grid%g_p, grid%pb, grid%t_2,grid%g_t_2,      &
                            p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,           &
                            grid%rdn, config_flags%non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

BENCH_END(g_calc_p_rho_tim)

     ENDDO
     !$OMP END PARALLEL DO

!  Reset the boundary conditions if there is another corrector step.
!  (rk_step < rk_order), else we'll handle it at the end of everything
!  (after the split physics, before exiting the timestep).

     rk_step_1_check: IF ( rk_step < rk_order ) THEN

!-----------------------------------------------------------
!  rk3 substep polar filter for scalars (moist,chem,scalar)
!-----------------------------------------------------------

       IF (config_flags%polar) THEN
         IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter moist ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_m
             IF ( config_flags%coupled_filtering ) THEN
             CALL couple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)        &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
             CALL pxft ( grid=grid                                               &
                    ,lineno=__LINE__                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = im                                     &
                    ,flag_chem          = 0                                      &
                    ,flag_scalar        = 0                                      &
                    ,flag_tracer        = 0                                      &
                    ,actual_distance_average=config_flags%actual_distance_average&
                    ,pos_def            = config_flags%pos_def                   &
                    ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             IF ( config_flags%coupled_filtering ) THEN
             CALL uncouple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)      &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
           END DO
         END IF

         IF ( num_3d_c >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter chem ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c
             IF ( config_flags%coupled_filtering ) THEN
             CALL couple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)         &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
             END IF
             CALL pxft ( grid=grid                                               &
                    ,lineno=__LINE__                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = 0                                      &
                    ,flag_chem          = im                                     &
                    ,flag_tracer        = 0                                      &
                    ,flag_scalar        = 0                                      &
                    ,actual_distance_average=config_flags%actual_distance_average&
                    ,pos_def            = config_flags%pos_def                   &
                    ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             IF ( config_flags%coupled_filtering ) THEN
             CALL uncouple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)       &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
           END DO
         END IF
         IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter tracer ' )
           DO im = PARAM_FIRST_SCALAR, num_tracer
             IF ( config_flags%coupled_filtering ) THEN
             CALL couple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)       &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
             END IF
             CALL pxft ( grid=grid                                               &
                    ,lineno=__LINE__                                             &
                    ,flag_uv            = 0                                      &
                    ,flag_rurv          = 0                                      &
                    ,flag_wph           = 0                                      &
                    ,flag_ww            = 0                                      &
                    ,flag_t             = 0                                      &
                    ,flag_mu            = 0                                      &
                    ,flag_mut           = 0                                      &
                    ,flag_moist         = 0                                      &
                    ,flag_chem          = 0                                      &
                    ,flag_tracer        = im                                      &
                    ,flag_scalar        = 0                                      &
                    ,actual_distance_average=config_flags%actual_distance_average&
                    ,pos_def            = config_flags%pos_def                   &
                    ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             IF ( config_flags%coupled_filtering ) THEN
             CALL uncouple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)     &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
           END DO
         END IF

         IF ( num_3d_s >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter scalar ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_s
             IF ( config_flags%coupled_filtering ) THEN
             CALL couple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)     &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,C1=grid%c1h , C2=grid%c2h                                   &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
             CALL pxft ( grid=grid                                             &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = im                                     &
                  ,actual_distance_average=config_flags%actual_distance_average&
                  ,pos_def            = config_flags%pos_def                   &
                  ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             IF ( config_flags%coupled_filtering ) THEN
             CALL uncouple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)   &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,C1=grid%c1h , C2=grid%c2h                                   &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
             END IF
           END DO
         END IF
       END IF ! polar filter test

!-----------------------------------------------------------
!  END rk3 substep polar filter for scalars (moist,chem,scalar)
!-----------------------------------------------------------

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  here's where we need a wide comm stencil - these are the
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!
!                                  * * * * * * *
!                     * * * * *    * * * * * * *
!            *        * * * * *    * * * * * * *
!          * + *      * * + * *    * * * + * * *
!            *        * * * * *    * * * * * * *
!                     * * * * *    * * * * * * *
!                                  * * * * * * *
!
! al        x
!
!  2D variable
! mu_2      x
!
! (adv order <=4)
! u_2                     x
! v_2                     x
! w_2                     x
! t_2                     x
! ph_2                    x
!
! (adv order <=6)
! u_2                                    x
! v_2                                    x
! w_2                                    x
! t_2                                    x
! ph_2                                   x
!
!  4D variable
! moist                   x
! chem                    x
! scalar                  x

#ifdef DM_PARALLEL
       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_D2_3_TL.inc"
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D2_5_TL.inc"
       ELSE
         WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF
#  include "PERIOD_BDY_EM_D.inc"
#  include "PERIOD_BDY_EM_MOIST2.inc"
#  include "PERIOD_BDY_EM_CHEM2.inc"
#  include "PERIOD_BDY_EM_TRACER2.inc"
#  include "PERIOD_BDY_EM_SCALAR2.inc"
#endif

BENCH_START(g_bc_end_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       tile_bc_loop_1: DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call g_rk_phys_bc_dry_2' )

         CALL g_rk_phys_bc_dry_2( config_flags,                     &
                                grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2, grid%w_2,grid%g_w_2, &
                                grid%t_2,grid%g_t_2, grid%ph_2,grid%g_ph_2, grid%mu_2,grid%g_mu_2,   &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end               )

BENCH_START(g_diag_w_tim)
         IF (.not. config_flags%non_hydrostatic) THEN
           CALL g_diagnose_w( ph_tend,g_ph_tend, grid%ph_2,grid%g_ph_2,  grid%ph_1,grid%g_ph_1, &
                            grid%w_2,grid%g_w_2, grid%muts,grid%g_muts, dt_rk,  &
                            grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2, grid%ht,                           &
                            grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                            ids, ide, jds, jde, kds, kde,           &
                            ims, ime, jms, jme, kms, kme,           &
                            grid%i_start(ij), grid%i_end(ij),       &
                            grid%j_start(ij), grid%j_end(ij),       &
                            k_start    , k_end                     )
         ENDIF
BENCH_END(g_diag_w_tim)

         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

           moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m

             CALL g_set_physical_bc3d( moist(ims,kms,jms,im),g_moist(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     ips, ipe, jps, jpe, kps, kpe,             &
                                     grid%i_start(ij), grid%i_end(ij),                   &
                                     grid%j_start(ij), grid%j_end(ij),                   &
                                     k_start    , k_end                       )

           END DO moisture_loop_bdy_1

         ENDIF

         IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

           chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

!!!!! REPLACE WITH g_set_physical_bc3d WHEN chem IS NEEDED. Ning Pan
             CALL set_physical_bc3d( chem(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO chem_species_bdy_loop_1

         END IF

         IF (num_tracer >= PARAM_FIRST_SCALAR) THEN

           tracer_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_tracer

             CALL g_set_physical_bc3d( tracer(ims,kms,jms,ic),g_tracer(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO tracer_species_bdy_loop_1

         END IF

         IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN

           scalar_species_bdy_loop_1 : DO is = PARAM_FIRST_SCALAR , num_3d_s

             CALL g_set_physical_bc3d( scalar(ims,kms,jms,is),g_scalar(ims,kms,jms,is), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO scalar_species_bdy_loop_1

         END IF

         IF (config_flags%km_opt .eq. 2) THEN

           CALL g_set_physical_bc3d( grid%tke_2 ,grid%g_tke_2, 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,            &
                                   ims, ime, jms, jme, kms, kme,            &
                                   ips, ipe, jps, jpe, kps, kpe,            &
                                   grid%i_start(ij), grid%i_end(ij),        &
                                   grid%j_start(ij), grid%j_end(ij),        &
                                   k_start    , k_end                      )

         END IF

       END DO tile_bc_loop_1
       !$OMP END PARALLEL DO
BENCH_END(g_bc_end_tim)


#ifdef DM_PARALLEL

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist, chem, scalar, tke      x


       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_5_TL.inc"
         ELSE
#         include "HALO_EM_TKE_3_TL.inc"
         ENDIF
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_7_TL.inc"
         ELSE
#         include "HALO_EM_TKE_5_TL.inc"
         ENDIF
       ELSE
         WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF

       IF ( num_moist .GE. PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_5_TL.inc"
           ELSE
#        include "HALO_EM_MOIST_E_3_TL.inc"
           END IF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_7_TL.inc"
           ELSE
#        include "HALO_EM_MOIST_E_5_TL.inc"
           END IF
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_chem >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_5.inc"
           ELSE
#        include "HALO_EM_CHEM_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL .and. config_flags%chem_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_7.inc"
           ELSE
#        include "HALO_EM_CHEM_E_5.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_5_TL.inc"
           ELSE
#        include "HALO_EM_TRACER_E_3_TL.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_7_TL.inc"
           ELSE
#        include "HALO_EM_TRACER_E_5_TL.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
           ELSE
#        include "HALO_EM_SCALAR_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_7.inc"
           ELSE
#        include "HALO_EM_SCALAR_E_5.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em_tl: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
#endif

     ENDIF rk_step_1_check


!**********************************************************
!
!  end of RK predictor-corrector loop
!
!**********************************************************

   END DO Runge_Kutta_loop

   IF (config_flags%do_avgflx_em .EQ. 1) THEN
! Reinitialize time-averaged fluxes if history output was written after the previous time step:

      CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM ),prevringtime=temp_time)

      CALL domain_clock_get ( grid, current_time=CurrTime, &
           current_timestr=message2 )

! use overloaded -, .LT. operator to check whether to initialize avgflx:
! reinitialize after each history output (detect this here by comparing current time
! against last history time and time step - this code follows what's done in adapt_timestep_em):
      WRITE ( message , FMT = '("solve_em_tl: old_dt =",g15.6,", dt=",g15.6," on domain ",I3)' ) &
           & old_dt,grid%dt,grid%id
      CALL wrf_debug(200,message)
      old_dt=min(old_dt,grid%dt)
      num = INT(old_dt * precision)
      den = precision

      CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)

      IF (CurrTime .lt. temp_time + dtInterval) THEN
         WRITE ( message , FMT = '("solve_em_tl: initializing avgflx at time ",A," on domain ",I3)' ) &
              & TRIM(message2), grid%id
         CALL wrf_message(trim(message))
         grid%avgflx_count = 0
!tile-loop for zero_avgflx
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
            CALL wrf_debug(200,'In solve_em_tl, before zero_avgflx call')

            CALL zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
                 &   ids, ide, jds, jde, kds, kde,           &
                 &   ims, ime, jms, jme, kms, kme,           &
                 &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
                 &   k_start    , k_end, f_flux, &
                 &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
                 &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
            CALL wrf_debug(200,'In solve_em_tl, after zero_avgflx call')

         ENDDO
      ENDIF

! Update avgflx quantities
!tile-loop for upd_avgflx
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         CALL wrf_debug(200,'In solve_em_tl, before upd_avgflx call')

         CALL upd_avgflx(grid%avgflx_count,grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   grid%ru_m, grid%rv_m, grid%ww_m, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
              &   k_start    , k_end, f_flux, &
              &   grid%cfu1,grid%cfd1,grid%dfu1,grid%efu1,grid%dfd1,grid%efd1,          &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
         CALL wrf_debug(200,'In solve_em_tl, after upd_avgflx call')

      ENDDO
      grid%avgflx_count = grid%avgflx_count + 1
   ENDIF
!
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

BENCH_START(g_advance_ppt_tim)
     CALL wrf_debug ( 200 , ' call g_advance_ppt' )
     CALL g_advance_ppt(grid%rthcuten,grid%g_rthcuten,grid%rqvcuten,grid%g_rqvcuten, &
                      grid%rqccuten,grid%g_rqccuten,grid%rqrcuten,grid%g_rqrcuten, &
                      grid%rqicuten,grid%g_rqicuten,grid%rqscuten,grid%g_rqscuten, &
                      grid%rainc,grid%g_rainc,grid%raincv,grid%rainsh,grid%g_rainsh,&
                      grid%pratec,grid%g_pratec,grid%pratesh,grid%g_pratesh, &
                      grid%nca,grid%g_nca,grid%htop,grid%g_htop,grid%hbot,grid%g_hbot,&
                      grid%cutop,grid%g_cutop,grid%cubot,grid%g_cubot,  &
                      grid%cuppt, grid%g_cuppt, grid%dt, config_flags,                &
                      ids,ide, jds,jde, kds,kde,             &
                      ims,ime, jms,jme, kms,kme,             &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start    , k_end                    )
BENCH_END(g_advance_ppt_tim)

   ENDDO
  !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call g_phy_prep_part2' )
     CALL g_phy_prep_part2 ( config_flags,                           &
                        grid%mut,grid%g_mut, grid%muu, grid%g_muu, grid%muv, grid%g_muv, &
                        grid%rthraten, grid%g_rthraten,                       &
                        grid%rthblten, grid%g_rthblten,                       &
                        grid%rublten, grid%g_rublten, grid%rvblten, grid%g_rvblten,            &
                        grid%rqvblten, grid%g_rqvblten, grid%rqcblten, grid%g_rqcblten, grid%rqiblten, grid%g_rqiblten,         &
                        grid%rucuten,  grid%g_rucuten , grid%rvcuten, grid%g_rvcuten,  grid%rthcuten, grid%g_rthcuten,    &
                        grid%rqvcuten, grid%g_rqvcuten, grid%rqccuten, grid%g_rqccuten, grid%rqrcuten, grid%g_rqrcuten,    &
                        grid%rqicuten, grid%g_rqicuten, grid%rqscuten, grid%g_rqscuten,                    &
                        grid%rushten,  grid%g_rushten, grid%rvshten,  grid%g_rvshten, grid%rthshten, grid%g_rthshten,    &
                        grid%rqvshten, grid%g_rqvshten, grid%rqcshten, grid%g_rqcshten, grid%rqrshten, grid%g_rqrshten,    &
                        grid%rqishten, grid%g_rqishten, grid%rqsshten, grid%g_rqsshten, grid%rqgshten, grid%g_rqgshten,    &
                        grid%rthften,  grid%g_rthften, grid%rqvften, grid%g_rqvften,                    &
                        grid%RUNDGDTEN, grid%g_RUNDGDTEN, grid%RVNDGDTEN, grid%g_RVNDGDTEN, grid%RTHNDGDTEN, grid%g_RTHNDGDTEN, &
                        grid%RPHNDGDTEN,grid%g_RPHNDGDTEN,grid%RQVNDGDTEN, grid%g_RQVNDGDTEN,grid%RMUNDGDTEN,&
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        grid%i_start(ij), grid%i_end(ij),       &
                        grid%j_start(ij), grid%j_end(ij),       &
                        k_start, k_end                         )
   ENDDO
   !$OMP END PARALLEL DO

!<DESCRIPTION>
!<pre>
! (5) time-split physics.
!
!     Microphysics are the only time  split physics in the WRF model
!     at this time.  Split-physics begins with the calculation of
!     needed diagnostic quantities (pressure, temperature, etc.)
!     followed by a call to the microphysics driver,
!     and finishes with a clean-up, storing off of a diabatic tendency
!     from the moist physics, and a re-calulation of the  diagnostic
!     quantities pressure and density.
!</pre>
!</DESCRIPTION>

   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = grid%spec_zone
   ELSE
     sz = 0
   ENDIF

   IF (config_flags%mp_physics /= 0)  then

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )

     scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL wrf_debug ( 200 , ' call g_moist_physics_prep' )
BENCH_START(g_moist_physics_prep_tim)
       CALL g_moist_physics_prep_em( grid%t_2,grid%g_t_2, grid%t_1,            &
                                   t0, grid%rho,grid%g_rho,                    &
                                   grid%al,grid%g_al, grid%alb,                &
                                   grid%p,grid%g_p, p8w,g_p8w,                 &
                                   p0, grid%pb,                                &
                                   grid%ph_2,grid%g_ph_2, grid%phb,            &
                                   th_phy, g_th_phy, pi_phy, g_pi_phy,         &
                                   p_phy, g_p_phy,                             &
                                   grid%z, grid%g_z, grid%z_at_w, grid%g_z_at_w, &
                                   dz8w, g_dz8w,                               &
                                   dtm, grid%h_diabatic, grid%g_h_diabatic,    &
                                   moist(ims,kms,jms,P_QV),g_moist(ims,kms,jms,P_QV), &
                                   grid%qv_diabatic, grid%g_qv_diabatic,              &
                                   moist(ims,kms,jms,P_QC),g_moist(ims,kms,jms,P_QC), &
                                   grid%qc_diabatic, grid%g_qc_diabatic,              &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )
BENCH_END(g_moist_physics_prep_tim)
     END DO scalar_tile_loop_1a
     !$OMP END PARALLEL DO

     CALL wrf_debug ( 200 , ' call g_microphysics_driver' )

     grid%g_sr = 0.
     grid%sr = 0.
     specified_bdy = config_flags%specified .OR. config_flags%nested
     channel_bdy = config_flags%specified .AND. config_flags%periodic_x

BENCH_START(g_micro_driver_tim)

!
! WRFU_AlarmIsRinging always returned false, so using an alternate  method to find out if it is time 
! to dump history/restart files so microphysics can be told to calculate things like radar reflectivity.
!
!     diagflag = .false.
!     CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM
!     ),prevringtime=temp_time,RingInterval=intervaltime)
!     CALL WRFU_ALARMGET(grid%alarms( RESTART_ALARM
!     ),prevringtime=restart_time,RingInterval=restartinterval)
!     CALL domain_clock_get ( grid, current_time=CurrTime )
!     old_dt=min(old_dt,grid%dt)
!     num = INT(old_dt * precision)
!     den = precision
!     CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)
!     IF (CurrTime .ge. temp_time + intervaltime - dtInterval .or. &
!         CurrTime .ge. restart_time + restartinterval - dtInterval ) THEN
!       diagflag = .true.
!     ENDIF
!     WRITE(wrf_err_message,*)'diag_flag=',diag_flag
!     CALL wrf_debug ( 0 , wrf_err_message )

#ifdef DM_PARALLEL
#      include "HALO_EM_SBM_TL.inc"
#endif

     CALL g_microphysics_driver(                                            &
      &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy     &
      &        ,DZ8W=dz8w          ,DZ8WD=g_dz8w, F_ICE_PHY=grid%f_ice_phy &
      &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr  &
      &        ,P8W=p8w            ,P=p_phy            ,PD=g_p_phy &
      &        ,PI_PHY=pi_phy,PI_PHYD=g_pi_phy                          &
      &        ,RHO=grid%rho    ,RHOD=grid%g_rho, SPEC_ZONE=grid%spec_zone              &
      &        ,SR=grid%sr              ,TH=th_phy,THD=g_th_phy                        &
      &        ,refl_10cm=grid%refl_10cm                                  & ! hm, 9/22/09 for refl
      &        ,WARM_RAIN=grid%warm_rain                                  &
      &        ,T8W=t8w                                                   &
      &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
      &        ,NSOURCE=grid%qndropsource                                 &
      &        ,XLAND=grid%xland                                          &
      &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy       &
      &        ,F_RAIN_PHY=grid%f_rain_phy                                &
      &        ,F_RIMEF_PHY=grid%f_rimef_phy                              &
      &        ,MP_PHYSICS=config_flags%mp_physics                        &
      &        ,ID=grid%id                                                &
      &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde         &
      &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme         &
      &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe         &
      &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
      &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
      &        ,KTS=k_start, KTE=min(k_end,kde-1)                         &
      &        ,NUM_TILES=grid%num_tiles                                  &
      &        ,NAER=grid%naer                                            &
                 ! Optional
      &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv                 &
      &        , RAINNCD=grid%g_rainnc, RAINNCVD=grid%g_rainncv         &
      &        , SNOWNC=grid%snownc, SNOWNCV=grid%snowncv                 &
      &        , GRAUPELNC=grid%graupelnc, GRAUPELNCV=grid%graupelncv     & ! for milbrandt2mom
      &        , HAILNC=grid%hailnc, HAILNCV=grid%hailncv                 &
      &        , W=grid%w_2, Z=grid%z, HT=grid%ht                         &
      &        , MP_RESTART_STATE=grid%mp_restart_state                   &
      &        , TBPVS_STATE=grid%tbpvs_state                             & ! etampnew
      &        , TBPVS0_STATE=grid%tbpvs0_state                           & ! etampnew
      &        , QV_CURR=moist(ims,kms,jms,P_QV),QV_CURRD=g_moist(ims,kms,jms,P_QV), F_QV=F_QV  &
      &        , QC_CURR=moist(ims,kms,jms,P_QC),QC_CURRD=g_moist(ims,kms,jms,P_QC), F_QC=F_QC  &
      &        , QR_CURR=moist(ims,kms,jms,P_QR),QR_CURRD=g_moist(ims,kms,jms,P_QR), F_QR=F_QR  &
      &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI               &
      &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS               &
      &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG               &
      &        , QH_CURR=moist(ims,kms,jms,P_QH), F_QH=F_QH               & ! for milbrandt2mom
      &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
#if (WRF_CHEM==1)
      &        , RAINPROD=grid%rainprod, EVAPPROD=grid%evapprod           &
      &        , QV_B4MP=grid%qv_b4mp,QC_B4MP=grid%qc_b4mp                &
      &        , QI_B4MP=grid%qi_b4mp, QS_B4MP=grid%qs_b4mp               &
#endif
      &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT              &
      &        , QNN_CURR=scalar(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
      &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI          &
      &        , QNC_CURR=scalar(ims,kms,jms,P_QNC), F_QNC=F_QNC          &
      &        , QNR_CURR=scalar(ims,kms,jms,P_QNR), F_QNR=F_QNR          &
      &        , QNS_CURR=scalar(ims,kms,jms,P_QNS), F_QNS=F_QNS          &
      &        , QNG_CURR=scalar(ims,kms,jms,P_QNG), F_QNG=F_QNG          &
      &        , QNH_CURR=scalar(ims,kms,jms,P_QNH), F_QNH=F_QNH          & ! for milbrandt2mom and nssl_2mom
!       &        , QZR_CURR=scalar(ims,kms,jms,P_QZR), F_QZR=F_QZR          & ! for milbrandt3mom
!       &        , QZI_CURR=scalar(ims,kms,jms,P_QZI), F_QZI=F_QZI          & ! "
!       &        , QZS_CURR=scalar(ims,kms,jms,P_QZS), F_QZS=F_QZS          & ! "
!       &        , QZG_CURR=scalar(ims,kms,jms,P_QZG), F_QZG=F_QZG          & ! "
!       &        , QZH_CURR=scalar(ims,kms,jms,P_QZH), F_QZH=F_QZH          & ! "
      &        , QVOLG_CURR=scalar(ims,kms,jms,P_QVOLG), F_QVOLG=F_QVOLG    & ! for nssl_2mom
      &        , QVOLH_CURR=scalar(ims,kms,jms,P_QVOLH), F_QVOLH=F_QVOLH    & ! for nssl_2mom
      &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &
      &        , qicuten=grid%rqicuten                                    &
      &        , HAIL=config_flags%gsfcgce_hail                           & ! for gsfcgce
      &        , ICE2=config_flags%gsfcgce_2ice                           & ! for gsfcgce
!     &        , ccntype=config_flags%milbrandt_ccntype                   & ! for milbrandt (2mom)
! YLIN
! RI_CURR INPUT
      &        , RI_CURR=grid%rimi                                          &
      &        , diagflag=diag_flag, do_radar_ref=config_flags%do_radar_ref &
                                                                          )
BENCH_END(g_micro_driver_tim)

#if 0
BENCH_START(g_microswap_2)
! for load balancing; communication to redistribute the points
      IF ( config_flags%mp_physics .EQ. ETAMPNEW .OR. &
     &     config_flags%mp_physics .EQ. FER_MP_HIRES) THEN
#include "SWAP_ETAMP_NEW.inc"
     ELSE IF ( config_flags%mp_physics .EQ. WSM3SCHEME ) THEN
#include "SWAP_WSM3.inc"
     ENDIF
BENCH_END(g_microswap_2)
#endif

     CALL wrf_debug ( 200 , ' call g_moist_physics_finish' )
BENCH_START(g_moist_phys_end_tim)

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)

       CALL g_microphysics_zero_outb (                                    &
                      moist , g_moist, num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL g_microphysics_zero_outb (                                    &
                      scalar , g_scalar, num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

!!!!! REPLACE WITH g_microphysics_zero_outb WHEN CODING TL OF PHYSICS. Ning Pan
       CALL microphysics_zero_outb (                                    &
                      chem , num_chem , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL g_microphysics_zero_outb (                                    &
                      tracer , g_tracer, num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL g_microphysics_zero_outa (                                    &
                      moist , g_moist, num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL g_microphysics_zero_outa (                                    &
                      scalar ,g_scalar, num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

!!!!! REPLACE WITH g_microphysics_zero_outa WHEN CODING TL OF PHYSICS. Ning Pan
       CALL microphysics_zero_outa (                                    &
                      chem , num_chem , config_flags ,                  &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL g_microphysics_zero_outa (                                    &
                      tracer , g_tracer, num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL g_moist_physics_finish_em( grid%t_2, grid%g_t_2, grid%t_1,      &
                                      t0, grid%muts,                        &
                                      th_phy, g_th_phy,                     &
                                      grid%h_diabatic,grid%g_h_diabatic,    &
                                      moist(ims,kms,jms,P_QV),g_moist(ims,kms,jms,P_QV), &
                                      grid%qv_diabatic, grid%g_qv_diabatic,              &
                                      moist(ims,kms,jms,P_QC),g_moist(ims,kms,jms,P_QC), &
                                      grid%qc_diabatic, grid%g_qc_diabatic,              &
                                      dtm, config_flags,    &
                                      ids, ide, jds, jde, kds, kde,     &
                                      ims, ime, jms, jme, kms, kme,     &
                                      its, ite, jts, jte,               &
                                      k_start    , k_end               )

     END DO
     !$OMP END PARALLEL DO

   ENDIF  ! microphysics test

!-----------------------------------------------------------
!  filter for moist variables post-microphysics and end of timestep
!-----------------------------------------------------------

   IF (config_flags%polar) THEN
     IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
       CALL wrf_debug ( 200 , ' call filter moist' )
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF

         CALL pxft ( grid=grid                                                 &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = im                                     &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,actual_distance_average=config_flags%actual_distance_average&
                  ,pos_def            = config_flags%pos_def                   &
                  ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         ENDIF
       ENDDO
     ENDIF
   ENDIF

!-----------------------------------------------------------
!  end filter for moist variables post-microphysics and end of timestep
!-----------------------------------------------------------

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )
   scalar_tile_loop_1ba: DO ij = 1 , grid%num_tiles

     IF ( config_flags%periodic_x ) THEN
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
     ELSE
       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
     ENDIF
     jts = max(grid%j_start(ij),jds+sz)
     jte = min(grid%j_end(ij),jde-1-sz)

     CALL g_calc_p_rho_phi( moist,g_moist, num_3d_m, config_flags%hypsometric_opt,       &
                          grid%al,grid%g_al, grid%alb, grid%mu_2,grid%g_mu_2, grid%muts,grid%g_muts, &
                          grid%ph_2,grid%g_ph_2, grid%phb, grid%p,grid%g_p, grid%pb, grid%t_2,grid%g_t_2,      &
                          p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,           &
                          grid%rdn, config_flags%non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          its, ite, jts, jte,               &
                          k_start    , k_end               )

   END DO scalar_tile_loop_1ba
   !$OMP END PARALLEL DO
BENCH_END(g_moist_phys_end_tim)

   IF (.not. config_flags%non_hydrostatic) THEN
#ifdef DM_PARALLEL
#    include "HALO_EM_HYDRO_UV_TL.inc"
#    include "PERIOD_EM_HYDRO_UV.inc"
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
       CALL g_diagnose_w( ph_tend,g_ph_tend, grid%ph_2,grid%g_ph_2,  grid%ph_1,grid%g_ph_1, &
                       grid%w_2,grid%g_w_2, grid%muts,grid%g_muts, dt_rk,  &
                       grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2, grid%ht,                           &
                       grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                       ids, ide, jds, jde, kds, kde,           &
                       ims, ime, jms, jme, kms, kme,           &
                       grid%i_start(ij), grid%i_end(ij),       &
                       grid%j_start(ij), grid%j_end(ij),       &
                       k_start    , k_end                     )

     END DO
     !$OMP END PARALLEL DO

   END IF

   CALL wrf_debug ( 200 , ' call chem polar filter ' )

!-----------------------------------------------------------
!  filter for chem and scalar variables at end of timestep
!-----------------------------------------------------------

   IF (config_flags%polar) THEN

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then
       chem_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_c
         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF

         CALL pxft ( grid=grid                                                 &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = im                                     &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = 0                                      &
                  ,actual_distance_average=config_flags%actual_distance_average&
                  ,pos_def            = config_flags%pos_def                   &
                  ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF
       ENDDO chem_filter_loop
     ENDIF
     IF ( num_tracer >= PARAM_FIRST_SCALAR ) then
       tracer_filter_loop: DO im = PARAM_FIRST_SCALAR, num_tracer
         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF

         CALL pxft ( grid=grid                                                 &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = im                                    &
                  ,flag_scalar        = 0                                      &
                  ,actual_distance_average=config_flags%actual_distance_average&
                  ,pos_def            = config_flags%pos_def                   &
                  ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF
       ENDDO tracer_filter_loop
     ENDIF

     IF ( num_3d_s >= PARAM_FIRST_SCALAR ) then
       scalar_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_s
         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF

         CALL pxft ( grid=grid                                                 &
                  ,lineno=__LINE__                                             &
                  ,flag_uv            = 0                                      &
                  ,flag_rurv          = 0                                      &
                  ,flag_wph           = 0                                      &
                  ,flag_ww            = 0                                      &
                  ,flag_t             = 0                                      &
                  ,flag_mu            = 0                                      &
                  ,flag_mut           = 0                                      &
                  ,flag_moist         = 0                                      &
                  ,flag_chem          = 0                                      &
                  ,flag_tracer        = 0                                      &
                  ,flag_scalar        = im                                     &
                  ,actual_distance_average=config_flags%actual_distance_average&
                  ,pos_def            = config_flags%pos_def                   &
                  ,swap_pole_with_next_j = config_flags%swap_pole_with_next_j  &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

         IF ( config_flags%coupled_filtering ) THEN
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
         END IF
       ENDDO scalar_filter_loop
     ENDIF
   ENDIF

!-----------------------------------------------------------
!  end filter for chem and scalar variables at end of timestep
!-----------------------------------------------------------

   !  We're finished except for boundary condition (and patch) update

   ! Boundary condition time (or communication time).  At this time, we have
   ! implemented periodic and symmetric physical boundary conditions.

   ! b.c. routine for data within patch.

   ! we need to do both time levels of
   ! data because the time filter only works in the physical solution space.

   ! First, do patch communications for boundary conditions (periodicity)

!-----------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!  here's where we need a wide comm stencil - these are the
!  uncoupled variables so are used for high order calc in
!  advection and mixong routines.
!
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * *
!            *        * * *    * * * * *
!                              * * * * *
!
!   grid%u_1                            x
!   grid%u_2                            x
!   grid%v_1                            x
!   grid%v_2                            x
!   grid%w_1                            x
!   grid%w_2                            x
!   grid%t_1                            x
!   grid%t_2                            x
!  grid%ph_1                            x
!  grid%ph_2                            x
!  grid%tke_1                           x
!  grid%tke_2                           x
!
!    2D variables
!  grid%mu_1     x
!  grid%mu_2     x
!
!    4D variables
!  moist                         x
!   chem                         x
! scalar                         x
!----------------------------------------------------------


#ifdef DM_PARALLEL
   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_D3_3_TL.inc"
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D3_5_TL.inc"
   ELSE
      WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
      CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#  include "PERIOD_BDY_EM_D3.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
#  include "PERIOD_BDY_EM_TRACER.inc"
#  include "PERIOD_BDY_EM_SCALAR.inc"
#  include "PERIOD_BDY_EM_TKE.inc"
#endif

!  now set physical b.c on a patch

BENCH_START(g_bc_2d_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call g_set_phys_bc_dry_2' )

     CALL g_set_phys_bc_dry_2( config_flags,                           &
                             grid%u_1,grid%g_u_1, grid%u_2,grid%g_u_2, &
                             grid%v_1,grid%g_v_1, grid%v_2,grid%g_v_2, &
                             grid%w_1,grid%g_w_1, grid%w_2,grid%g_w_2, &
                             grid%t_1,grid%g_t_1, grid%t_2,grid%g_t_2, &
                             grid%ph_1,grid%g_ph_1, grid%ph_2,grid%g_ph_2, &
                             grid%mu_1,grid%g_mu_1, grid%mu_2,grid%g_mu_2, &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL g_set_physical_bc3d( grid%tke_1,grid%g_tke_1, 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )

     CALL g_set_physical_bc3d( grid%tke_2 ,grid%g_tke_2, 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL g_set_physical_bc3d( moist(ims,kms,jms,im),g_moist(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )


     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

!!!!! REPLACE WITH g_set_physical_bc3d WHEN chem IS NEEDED. Ning Pan
       CALL set_physical_bc3d( chem(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

     tracer_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_tracer

       CALL g_set_physical_bc3d( tracer(ims,kms,jms,ic) , g_tracer(ims,kms,jms,ic), 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO tracer_species_bdy_loop_2

     scalar_species_bdy_loop_2 : DO is = PARAM_FIRST_SCALAR , num_3d_s

       CALL g_set_physical_bc3d( scalar(ims,kms,jms,is) ,g_scalar(ims,kms,jms,is) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO scalar_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO
BENCH_END(g_bc_2d_tim)

   IF( config_flags%specified .or. config_flags%nested ) THEN

!  this code forces boundary values to specified values to avoid drift

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   tile_bc_loop_3: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call g_spec_bdy_final' )

     CALL g_spec_bdy_final   ( grid%u_2, grid%g_u_2, grid%muus, grid%g_muus, grid%msfuy, &
                                grid%u_bxs, grid%g_u_bxs, grid%u_bxe, grid%g_u_bxe,  &
                                grid%u_bys, grid%g_u_bys, grid%u_bye, grid%g_u_bye,  &
                                grid%u_btxs,grid%g_u_btxs,grid%u_btxe,grid%g_u_btxe, &
                                grid%u_btys,grid%g_u_btys,grid%u_btye,grid%g_u_btye, &
                                'u', config_flags,                                   &
                                config_flags%spec_bdy_width, grid%spec_zone,         &
                                grid%dtbc,                                           &
                                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                           & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                    &
                                grid%j_start(ij), grid%j_end(ij),                    &
                                k_start    , k_end                     )

     CALL g_spec_bdy_final   ( grid%v_2, grid%g_v_2, grid%muvs, grid%g_muvs, grid%msfvx, &
                                grid%v_bxs, grid%g_v_bxs, grid%v_bxe, grid%g_v_bxe,  &
                                grid%v_bys, grid%g_v_bys, grid%v_bye, grid%g_v_bye,  &
                                grid%v_btxs,grid%g_v_btxs,grid%v_btxe,grid%g_v_btxe, &
                                grid%v_btys,grid%g_v_btys,grid%v_btye,grid%g_v_btye, &
                                'v', config_flags,                                   &
                                config_flags%spec_bdy_width, grid%spec_zone,         &
                                grid%dtbc,                                           &
                                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                           & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                    &
                                grid%j_start(ij), grid%j_end(ij),                    &
                                k_start    , k_end                     )

     IF( config_flags%nested) THEN
       CALL g_spec_bdy_final ( grid%w_2, grid%g_w_2, grid%muts, grid%g_muts, grid%msfty, &
                                grid%w_bxs, grid%g_w_bxs, grid%w_bxe, grid%g_w_bxe,      &
                                grid%w_bys, grid%g_w_bys, grid%w_bye, grid%g_w_bye,      &
                                grid%w_btxs,grid%g_w_btxs,grid%w_btxe,grid%g_w_btxe,     &
                                grid%w_btys,grid%g_w_btys,grid%w_btye,grid%g_w_btye,     &
                                'w', config_flags,                                       &
                                config_flags%spec_bdy_width, grid%spec_zone,             &
                                grid%dtbc,                                               &
                                ids,ide, jds,jde, kds,kde,                               & ! domain dims
                                ims,ime, jms,jme, kms,kme,                               & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                               & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                        &
                                grid%j_start(ij), grid%j_end(ij),                        &
                                k_start    , k_end                     )
     ENDIF

     CALL g_spec_bdy_final ( grid%t_2, grid%g_t_2, grid%muts, grid%g_muts, grid%msfty, &
                                grid%t_bxs, grid%g_t_bxs, grid%t_bxe, grid%g_t_bxe,    &
                                grid%t_bys, grid%g_t_bys, grid%t_bye, grid%g_t_bye,    &
                                grid%t_btxs,grid%g_t_btxs,grid%t_btxe,grid%g_t_btxe,   &
                                grid%t_btys,grid%g_t_btys,grid%t_btye,grid%g_t_btye,   &
                                't', config_flags,                                     &
                                config_flags%spec_bdy_width, grid%spec_zone,           &
                                grid%dtbc,                                             &
                                ids,ide, jds,jde, kds,kde,                             & ! domain dims
                                ims,ime, jms,jme, kms,kme,                             & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                             & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                      &
                                grid%j_start(ij), grid%j_end(ij),                      &
                                k_start    , k_end                     )

     CALL g_spec_bdy_final ( grid%ph_2, grid%g_ph_2, grid%muts, grid%g_muts, grid%msfty, &
                                grid%ph_bxs, grid%g_ph_bxs, grid%ph_bxe, grid%g_ph_bxe,  &
                                grid%ph_bys, grid%g_ph_bys, grid%ph_bye, grid%g_ph_bye,  &
                                grid%ph_btxs,grid%g_ph_btxs,grid%ph_btxe,grid%g_ph_btxe, &
                                grid%ph_btys,grid%g_ph_btys,grid%ph_btye,grid%g_ph_btye, &
                                'h', config_flags,                                       &
                                config_flags%spec_bdy_width, grid%spec_zone,             &
                                grid%dtbc,                                               &
                                ids,ide, jds,jde, kds,kde,                               & ! domain dims
                                ims,ime, jms,jme, kms,kme,                               & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                               & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                        &
                                grid%j_start(ij), grid%j_end(ij),                        &
                                k_start    , k_end                     )

     IF( config_flags%spec_bdy_final_mu .EQ. 1 ) THEN
     CALL g_spec_bdy_final ( grid%mu_2, grid%g_mu_2, grid%muts, grid%g_muts, grid%msfty, &
                                grid%mu_bxs, grid%g_mu_bxs, grid%mu_bxe, grid%g_mu_bxe,  &
                                grid%mu_bys, grid%g_mu_bys, grid%mu_bye, grid%g_mu_bye,  &
                                grid%mu_btxs,grid%g_mu_btxs,grid%mu_btxe,grid%g_mu_btxe, &
                                grid%mu_btys,grid%g_mu_btys,grid%mu_btye,grid%g_mu_btye, &
                                'm', config_flags,                                       &
                                config_flags%spec_bdy_width, grid%spec_zone,             &
                                grid%dtbc,                                               &
                                ids,ide, jds,jde, 1,  1,                                 & ! domain dims
                                ims,ime, jms,jme, 1,  1,                                 & ! memory dims
                                ips,ipe, jps,jpe, 1,  1,                                 & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                        &
                                grid%j_start(ij), grid%j_end(ij),                        &
                                1  , 1                    )
     ENDIF

     moisture_loop_bdy_3 : DO im = PARAM_FIRST_SCALAR , num_3d_m

     IF ( im .EQ. P_QV .OR. config_flags%nested .OR. &
             ( config_flags%specified .AND. config_flags%have_bcs_moist ) ) THEN
        CALL g_spec_bdy_final ( moist(ims,kms,jms,im), g_moist(ims,kms,jms,im),       &
                                grid%muts, grid%g_muts, grid%msfty,                   &
                                moist_bxs(jms,kms,1,im),  g_moist_bxs(jms,kms,1,im),  &
                                moist_bxe(jms,kms,1,im),  g_moist_bxe(jms,kms,1,im),  &
                                moist_bys(ims,kms,1,im),  g_moist_bys(ims,kms,1,im),  &
                                moist_bye(ims,kms,1,im),  g_moist_bye(ims,kms,1,im),  &
                                moist_btxs(jms,kms,1,im), g_moist_btxs(jms,kms,1,im), &
                                moist_btxe(jms,kms,1,im), g_moist_btxe(jms,kms,1,im), &
                                moist_btys(ims,kms,1,im), g_moist_btys(ims,kms,1,im), &
                                moist_btye(ims,kms,1,im), g_moist_btye(ims,kms,1,im), &
                                't', config_flags,                                    &
                                config_flags%spec_bdy_width, grid%spec_zone,          &
                                grid%dtbc,                                            &
                                ids,ide, jds,jde, kds,kde,                            & ! domain dims
                                ims,ime, jms,jme, kms,kme,                            & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                            & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                     &
                                grid%j_start(ij), grid%j_end(ij),                     &
                                k_start    , k_end                     )
     ENDIF

     END DO moisture_loop_bdy_3

#if (WRF_CHEM == 1)
     IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN
         chem_species_bdy_loop_3 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

     IF( ( config_flags%nested ) ) THEN
!       CALL g_spec_bdy_final ( chem(ims,kms,jms,ic), g_chem(ims,kms,jms,ic),       &
!                               grid%muts, grid%g_muts, grid%msfty,                 &
!                               chem_bxs(jms,kms,1,ic),  g_chem_bxs(jms,kms,1,ic),  &
!                               chem_bxe(jms,kms,1,ic),  g_chem_bxe(jms,kms,1,ic),  &
!                               chem_bys(ims,kms,1,ic),  g_chem_bys(ims,kms,1,ic),  &
!                               chem_bye(ims,kms,1,ic),  g_chem_bye(ims,kms,1,ic),  &
!                               chem_btxs(jms,kms,1,ic), g_chem_btxs(jms,kms,1,ic), &
!                               chem_btxe(jms,kms,1,ic), g_chem_btxe(jms,kms,1,ic), &
!                               chem_btys(ims,kms,1,ic), g_chem_btys(ims,kms,1,ic), &
!                               chem_btye(ims,kms,1,ic), g_chem_btye(ims,kms,1,ic), &
!hcl-beg no g_chem for now
        CALL g_spec_bdy_final ( chem(ims,kms,jms,ic),   chem(ims,kms,jms,ic),       &
                                grid%muts, grid%g_muts, grid%msfty,                 &
                                chem_bxs(jms,kms,1,ic),    chem_bxs(jms,kms,1,ic),  &
                                chem_bxe(jms,kms,1,ic),    chem_bxe(jms,kms,1,ic),  &
                                chem_bys(ims,kms,1,ic),    chem_bys(ims,kms,1,ic),  &
                                chem_bye(ims,kms,1,ic),    chem_bye(ims,kms,1,ic),  &
                                chem_btxs(jms,kms,1,ic),   chem_btxs(jms,kms,1,ic), &
                                chem_btxe(jms,kms,1,ic),   chem_btxe(jms,kms,1,ic), &
                                chem_btys(ims,kms,1,ic),   chem_btys(ims,kms,1,ic), &
                                chem_btye(ims,kms,1,ic),   chem_btye(ims,kms,1,ic), &
!hcl-end no g_chem for now
                                't', config_flags,                                  &
                                config_flags%spec_bdy_width, grid%spec_zone,        &
                                grid%dtbc,                                          &
                                ids,ide, jds,jde, kds,kde,                          & ! domain dims
                                ims,ime, jms,jme, kms,kme,                          & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                          & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                   &
                                grid%j_start(ij), grid%j_end(ij),                   &
                                k_start    , k_end                     )
     ENDIF

         END DO chem_species_bdy_loop_3
     ENDIF
#endif

     tracer_species_bdy_loop_3 : DO im = PARAM_FIRST_SCALAR , num_tracer

     IF( ( config_flags%nested ) ) THEN
        CALL g_spec_bdy_final ( tracer(ims,kms,jms,im), g_tracer(ims,kms,jms,im),       &
                                grid%muts, grid%g_muts, grid%msfty,                     &
                                tracer_bxs(jms,kms,1,im),  g_tracer_bxs(jms,kms,1,im),  &
                                tracer_bxe(jms,kms,1,im),  g_tracer_bxe(jms,kms,1,im),  &
                                tracer_bys(ims,kms,1,im),  g_tracer_bys(ims,kms,1,im),  &
                                tracer_bye(ims,kms,1,im),  g_tracer_bye(ims,kms,1,im),  &
                                tracer_btxs(jms,kms,1,im), g_tracer_btxs(jms,kms,1,im), &
                                tracer_btxe(jms,kms,1,im), g_tracer_btxe(jms,kms,1,im), &
                                tracer_btys(ims,kms,1,im), g_tracer_btys(ims,kms,1,im), &
                                tracer_btye(ims,kms,1,im), g_tracer_btye(ims,kms,1,im), &
                                't', config_flags,                                      &
                                config_flags%spec_bdy_width, grid%spec_zone,            &
                                grid%dtbc,                                              &
                                ids,ide, jds,jde, kds,kde,                              & ! domain dims
                                ims,ime, jms,jme, kms,kme,                              & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                              & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                       &
                                grid%j_start(ij), grid%j_end(ij),                       &
                                k_start    , k_end                     )
     ENDIF

     END DO tracer_species_bdy_loop_3

     scalar_species_bdy_loop_3 : DO is = PARAM_FIRST_SCALAR , num_3d_s

     IF( ( config_flags%nested ) ) THEN
        CALL g_spec_bdy_final ( scalar(ims,kms,jms,is), g_scalar(ims,kms,jms,is), &
                                grid%muts, grid%g_muts, grid%msfty,    &
                                scalar_bxs(jms,kms,1,is),  g_scalar_bxs(jms,kms,1,is),  &
                                scalar_bxe(jms,kms,1,is),  g_scalar_bxe(jms,kms,1,is),  &
                                scalar_bys(ims,kms,1,is),  g_scalar_bys(ims,kms,1,is),  &
                                scalar_bye(ims,kms,1,is),  g_scalar_bye(ims,kms,1,is),  &
                                scalar_btxs(jms,kms,1,is), g_scalar_btxs(jms,kms,1,is), &
                                scalar_btxe(jms,kms,1,is), g_scalar_btxe(jms,kms,1,is), &
                                scalar_btys(ims,kms,1,is), g_scalar_btys(ims,kms,1,is), &
                                scalar_btye(ims,kms,1,is), g_scalar_btye(ims,kms,1,is), &
                                't', config_flags,                                      &
                                config_flags%spec_bdy_width, grid%spec_zone,            &
                                grid%dtbc,                                              &
                                ids,ide, jds,jde, kds,kde,                              & ! domain dims
                                ims,ime, jms,jme, kms,kme,                              & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,                              & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),                       &
                                grid%j_start(ij), grid%j_end(ij),                       &
                                k_start    , k_end                     )
     ENDIF

     END DO scalar_species_bdy_loop_3

   END DO tile_bc_loop_3
   !$OMP END PARALLEL DO

     CALL wrf_debug ( 200 , ' end call g_spec_bdy_final' )

   ENDIF

!  dtbc will be read from basic state at every time step, so it need not to be updated here.
!
!  IF( config_flags%specified .or. config_flags%nested ) THEN
!    grid%dtbc = grid%dtbc + grid%dt
!  ENDIF

! reset surface w for consistency

#ifdef DM_PARALLEL
#  include "HALO_EM_C_TL.inc"
#  include "PERIOD_BDY_EM_E.inc"
#endif

   CALL wrf_debug ( 10 , ' call g_set_w_surface' )
   fill_w_flag = .false.

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      CALL g_set_w_surface( config_flags, grid%znw, fill_w_flag,              &
                           grid%w_2,grid%g_w_2, grid%ht,                      &
                           grid%u_2,grid%g_u_2, grid%v_2,grid%g_v_2,          &
                           grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy,&
                           grid%msftx, grid%msfty,                          &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                   )

!                          its, ite, jts, jte, k_start, min(k_end,kde-1),   &

   END DO
   !$OMP END PARALLEL DO

!-----------------------------------------------------------
!  After all of the RK steps, after the microphysics, after p-rho-phi,
!  after w, after filtering, we have data ready to use.
!-----------------------------------------------------------

  CALL after_all_rk_steps ( grid, config_flags,                  &
                            moist, chem, tracer, scalar,         &
                            th_phy, pi_phy, p_phy, rho_phy,      &   
                            p8w, t8w, dz8w,                      &
                            REAL(curr_secs,8), curr_secs2,       &
                            diag_flag,                           &
                            ids,  ide,  jds,  jde,  kds,  kde,   &
                            ims,  ime,  jms,  jme,  kms,  kme,   &
                            ips,  ipe,  jps,  jpe,  kps,  kpe,   &
                            imsx, imex, jmsx, jmex, kmsx, kmex,  &
                            ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                            imsy, imey, jmsy, jmey, kmsy, kmey,  &
                            ipsy, ipey, jpsy, jpey, kpsy, kpey   )

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
   CALL wrf_debug ( 200 , ' call HALO_RK_E' )
   IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_E_3_TL.inc"
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_E_5_TL.inc"
   ELSE
     WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
     CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#endif

#ifdef DM_PARALLEL
   IF ( num_moist >= PARAM_FIRST_SCALAR  ) THEN
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_MOIST' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_MOIST_E_3_TL.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_MOIST_E_5_TL.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_chem >= PARAM_FIRST_SCALAR ) THEN
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_TRACER' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_TRACER_E_3_TL.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_TRACER_E_5_TL.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_SCALAR' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_SCALAR_E_3.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_SCALAR_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em_tl: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
#endif

!  Max values of CFL for adaptive time step scheme

   DEALLOCATE(max_vert_cfl_tmp)
   DEALLOCATE(max_horiz_cfl_tmp)

   CALL wrf_debug ( 200 , ' call end of solve_em_tl' )

! Finish timers if compiled with -DBENCH.
#include "bench_solve_em_end.h"

   RETURN

END SUBROUTINE solve_em_tl

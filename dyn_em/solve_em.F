!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em ( grid , config_flags  &
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
   USE module_domain_type, ONLY : history_alarm, restart_alarm, auxinput4_alarm            &
                 ,boundary_alarm
   USE module_configure, ONLY : grid_config_rec_type
   USE module_driver_constants
   USE module_machine
   USE module_tiles, ONLY : set_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y                   &
                 ,local_communicator_periodic, wrf_dm_maxval
   USE module_comm_dm, ONLY : &
                  halo_em_a_sub,halo_em_b_sub,halo_em_c2_sub,halo_em_chem_e_3_sub          &
                 ,halo_em_chem_e_5_sub,halo_em_chem_e_7_sub,halo_em_chem_old_e_5_sub       &
                 ,halo_em_chem_old_e_7_sub,halo_em_c_sub,halo_em_d2_3_sub                  &
                 ,halo_em_d2_5_sub,halo_em_d3_3_sub,halo_em_d3_5_sub,halo_em_d_sub         &
                 ,halo_em_e_3_sub,halo_em_e_5_sub,halo_em_hydro_uv_sub                     &
                 ,halo_em_moist_e_3_sub,halo_em_moist_e_5_sub,halo_em_moist_e_7_sub        &
                 ,halo_em_moist_old_e_5_sub,halo_em_moist_old_e_7_sub                      &
                 ,halo_em_scalar_e_3_sub,halo_em_scalar_e_5_sub,halo_em_scalar_e_7_sub     &
                 ,halo_em_scalar_old_e_5_sub,halo_em_scalar_old_e_7_sub,halo_em_tke_3_sub  &
                 ,halo_em_tke_5_sub,halo_em_tke_7_sub,halo_em_tke_advect_3_sub             &
                 ,halo_em_tke_advect_5_sub,halo_em_tke_old_e_5_sub                         &
                 ,halo_em_tke_old_e_7_sub,halo_em_tracer_e_3_sub,halo_em_tracer_e_5_sub    &
                 ,halo_em_tracer_e_7_sub,halo_em_tracer_old_e_5_sub                        &
                 ,halo_em_tracer_old_e_7_sub,halo_em_sbm_sub,period_bdy_em_a_sub                           &
                 ,period_bdy_em_b3_sub,period_bdy_em_b_sub,period_bdy_em_chem2_sub         &
                 ,period_bdy_em_chem_old_sub,period_bdy_em_chem_sub,period_bdy_em_d3_sub   &
                 ,period_bdy_em_d_sub,period_bdy_em_e_sub,period_bdy_em_moist2_sub         &
                 ,period_bdy_em_moist_old_sub,period_bdy_em_moist_sub                      &
                 ,period_bdy_em_scalar2_sub,period_bdy_em_scalar_old_sub                   &
                 ,period_bdy_em_scalar_sub,period_bdy_em_tke_old_sub, period_bdy_em_tke_sub  &
                 ,period_bdy_em_tracer2_sub,period_bdy_em_tracer_old_sub                   &
                 ,period_bdy_em_tracer_sub,period_em_da_sub,period_em_hydro_uv_sub         &
                 ,period_em_f_sub,period_em_g_sub                                          &
                 ,halo_em_f_1_sub,halo_em_init_4_sub,halo_em_thetam_sub,period_em_thetam_sub &
                 ,halo_em_d_pv_sub
#endif
   USE module_utility
! Mediation layer modules
! Model layer modules
   USE module_model_constants
   USE module_small_step_em
   USE module_em
   USE module_big_step_utilities_em
   USE module_bc
   USE module_bc_em
   USE module_solvedebug_em
   USE module_physics_addtendc
   USE module_diffusion_em
   USE module_polarfft
   USE module_microphysics_driver
   USE module_microphysics_zero_out
!   USE module_lightning_driver, ONLY : lightning_driver
   USE module_fddaobs_driver
!   USE module_diagnostics
#if (WRF_CHEM == 1)
   USE module_input_chem_data
   USE module_input_tracer
   USE module_chem_utilities
#endif
   USE module_dust_emis
   USE module_first_rk_step_part1
   USE module_first_rk_step_part2
   USE module_after_all_rk_steps
   USE module_llxy, ONLY : proj_cassini
   USE module_avgflx_em, ONLY : zero_avgflx, upd_avgflx
   USE module_cpl, ONLY : coupler_on, cpl_settime, cpl_store_input

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

   REAL                            :: t_new, time_duration_of_lbcs
   
   ! Changes in tendency at this timestep
   real ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: h_tendency, &
                                                                                   z_tendency
                                                                                   
   ! Whether advection should produce decoupled horizontal and vertical advective tendency outputs
   LOGICAL                        :: tenddec
   
   ! Flag for producing diagnostic fields (e.g., radar reflectivity)
   LOGICAL                        :: diag_flag
   
#if (WRF_CHEM == 1)
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
   CHARACTER*256                              :: message, message2, message3
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
! solve_em is the main driver for advancing a grid a single timestep.
! It is a mediation-layer routine -> DM and SM calls are made where 
! needed for parallel processing.  
!
! solve_em can integrate the equations using 3 time-integration methods
!      
!    - 3rd order Runge-Kutta time integration (recommended)
!      
!    - 2nd order Runge-Kutta time integration
!      
! The main sections of solve_em are
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

!  backward integration needs to advect only QV
   if (grid%dfi_stage .EQ. DFI_BCK) then
       num_3d_m    = P_QV
       num_3d_s    = PARAM_FIRST_SCALAR - 1
   endif

   f_flux = config_flags%do_avgflx_cugd .EQ. 1

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
   CALL set_tiles ( ZONE_SOLVE_EM, grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
!   CALL set_tiles (  grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

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
   grid%dtbc = grid%dtbc + grid%dt

   IF( coupler_on ) CALL cpl_store_input( grid, config_flags )

   IF (config_flags%polar) dclat = 90./REAL(jde-jds) !(0.5 * 180/ny)

#if (WRF_CHEM == 1)

   kte=min(k_end,kde-1)
# ifdef DM_PARALLEL
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see matching halo calls below for stencils
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
       IF( config_flags%progn > 0 ) THEN
#         include "HALO_EM_SCALAR_E_3.inc"
       ENDIF
       IF( config_flags%cu_physics == CAMZMSCHEME ) THEN
#         include "HALO_EM_SCALAR_E_3.inc"
       ENDIF
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
       IF( config_flags%cu_physics == CAMZMSCHEME ) THEN
#         include "HALO_EM_SCALAR_E_5.inc"
       ENDIF
       IF( config_flags%progn > 0 ) THEN
#         include "HALO_EM_SCALAR_E_5.inc"
      ENDIF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
   if ( num_tracer >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see matching halo calls below for stencils
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_tracer' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_TRACER_E_3.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_TRACER_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
# endif
!--------------------------------------------------------------
   adv_ct_indices(   :  ) = 1
   IF ( config_flags%chemdiag == USECHEMDIAG ) THEN
   ! modify tendency list here
   ! note that the referencing direction here is opposite of that in chem_driver
       adv_ct_indices(p_co  ) = p_advh_co
       adv_ct_indices(p_o3  ) = p_advh_o3
       adv_ct_indices(p_no  ) = p_advh_no
       adv_ct_indices(p_no2 ) = p_advh_no2
       adv_ct_indices(p_hno3) = p_advh_hno3
       adv_ct_indices(p_iso ) = p_advh_iso
       adv_ct_indices(p_ho  ) = p_advh_ho
       adv_ct_indices(p_ho2 ) = p_advh_ho2
   END IF
#endif

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

       CALL zero_bdytend (grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye,     &
                          grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye,     &
                          grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                          grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye,     &
                          grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye,     &
                          grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                          moist_btxs,moist_btxe,                               &
                          moist_btys,moist_btye,                               &
                          scalar_btxs,scalar_btxe,                              &
                          scalar_btys,scalar_btye,                              &
                          grid%spec_bdy_width,num_3d_m,num_3d_s,                &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          ips,ipe, jps,jpe, kps,kpe,                   &
                          grid%i_start(ij), grid%i_end(ij),            &
                          grid%j_start(ij), grid%j_end(ij),            &
                          k_start, k_end                               )

       ENDIF

       !  If the user has requested to optionally select the moist theta (use_theta_m==1)
       !  switch, the first setting of the "old" value of theta_m uses the "old"
       !  value of Qv.  The moist_old variable does not exist until after the advection
       !  towards the end of the RK loop.  For the first time in the RK loop, we need
       !  a reasonable value for moist_old.

       CALL initialize_moist_old ( moist_old(:,:,:,P_Qv),              &
                          moist(:,:,:,P_Qv) ,                          &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          grid%i_start(ij), grid%i_end(ij),            &
                          grid%j_start(ij), grid%j_end(ij),            &
                          k_start, k_end                               )
     ENDDO
     !$OMP END PARALLEL DO

     !  Now that we have initialized the moist_old values with P_Qv for
     !  computing a moist t_tendf after rk_step part2, fill in the halo 
     !  and period boundaries.

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
#  include "HALO_EM_MOIST_OLD_E_7.inc"
#  include "PERIOD_BDY_EM_MOIST_OLD.inc"
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
       im = P_Qv
       CALL set_physical_bc3d( moist_old(ims,kms,jms,im), 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,                  &
                               ims, ime, jms, jme, kms, kme,                  &
                               ips, ipe, jps, jpe, kps, kpe,                  &
                               grid%i_start(ij), grid%i_end(ij),              &
                               grid%j_start(ij), grid%j_end(ij),              &
                               k_start    , k_end                             )
     END DO
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
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL zero_pole ( grid%v_1,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )
         CALL zero_pole ( grid%v_2,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )
       END DO
       !$OMP END PARALLEL DO
     END IF
!
!  Time level t is in the *_2 variable in the first part 
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
     CALL wrf_debug ( 200 , ' call rk_step_prep ' )

BENCH_START(step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL rk_step_prep  ( config_flags, rk_step,            &
                            grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2, grid%mu_2,   &
                            grid%c1h, grid%c2h, grid%c1f, grid%c2f, moist,   &
                            grid%ru, grid%rv, grid%rw, grid%ww, grid%php, grid%alt, grid%muu, grid%muv,   &
                            grid%mub, grid%mut, grid%phb, grid%pb, grid%p, grid%al, grid%alb,    &
                            cqu, cqv, cqw,                    &
                            grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv,        &
                            grid%msfvy, grid%msftx, grid%msfty,                        &
                            grid%fnm, grid%fnp, grid%dnw, grid%rdx, grid%rdy,          &
                            num_3d_m,                         &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start, k_end                   )

     END DO
     !$OMP END PARALLEL DO
BENCH_END(step_prep_tim)

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
#    include "HALO_EM_A.inc"
#endif

! set boundary conditions on variables 
! from big_step_prep for use in big_step_proc

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A.inc"
#endif

BENCH_START(set_phys_bc_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

       CALL rk_phys_bc_dry_1( config_flags, grid%ru, grid%rv, grid%rw, grid%ww,      & 
                              grid%muu, grid%muv, grid%mut, grid%php, grid%alt, grid%p,        &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                )
       CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start, k_end                )

       IF (config_flags%polar) THEN 

!-------------------------------------------------------
! lat-lon grid pole-point (v) specification (extrapolate v, rv to the pole)
!-------------------------------------------------------

         CALL pole_point_bc ( grid%v_1,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )
 
         CALL pole_point_bc ( grid%v_2,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )
 
!-------------------------------------------------------
! end lat-lon grid pole-point (v) specification
!-------------------------------------------------------

       ENDIF
     END DO
     !$OMP END PARALLEL DO
BENCH_END(set_phys_bc_tim)

     rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

BENCH_START(calc_p_rho_tim)

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

       IF (coupler_on) CALL cpl_settime( curr_secs2 )

       CALL first_rk_step_part1 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , aerod                            &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , config_flags%use_adaptive_time_step &
                             , curr_secs                        &
                             , psim , psih , gz1oz0             &
                             , chklowq                          &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , grid%t_phy      &
                             , dz8w , p8w , t8w                 &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx, imex, jmsx, jmex, kmsx, kmex    &
                             , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
                             , imsy, imey, jmsy, jmey, kmsy, kmey    &
                             , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                             , aerocu                           &
                            )

#ifdef DM_PARALLEL
       IF ( config_flags%bl_pbl_physics == MYNNPBLSCHEME2 .OR. &
            config_flags%bl_pbl_physics == MYNNPBLSCHEME3 ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
       ENDIF
#endif

       CALL first_rk_step_part2 (    grid, config_flags         &
                             , moist , moist_old , moist_tend   &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , gz1oz0             &
                             , chklowq                          &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , grid%t_phy      &
                             , dz8w , p8w , t8w                 &
                             , nba_mij, num_nba_mij             & !JDM 
                             , nba_rij, num_nba_rij             & !JDM  
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

BENCH_START(rk_tend_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_tendency' )
       CALL rk_tendency ( config_flags, rk_step                                                                &
                         ,grid%ru_tend, grid%rv_tend, rw_tend, ph_tend, t_tend                                 &
                         ,ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf                                      &
                         ,mu_tend, grid%u_save, grid%v_save, w_save, ph_save                                   &
                         ,grid%t_save, mu_save, grid%rthften                                                   &
                         ,grid%ru, grid%rv, grid%rw, grid%ww                                                   &
                         ,grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2                                    &
                         ,grid%u_1, grid%v_1, grid%w_1, grid%t_1, grid%ph_1                                    &
                         ,grid%h_diabatic, grid%phb, grid%t_init                                               &
                         ,grid%mu_2, grid%mut, grid%muu, grid%muv, grid%mub                                    &
                         ,grid%c1h, grid%c2h, grid%c1f, grid%c2f                                               &
                         ,grid%al, grid%alt, grid%p, grid%pb, grid%php, cqu, cqv, cqw                          &
                         ,grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base                     &
                         ,grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv                                    &
                         ,grid%msfvy, grid%msftx,grid%msfty, grid%clat, grid%f, grid%e, grid%sina, grid%cosa   &
                         ,grid%fnm, grid%fnp, grid%rdn, grid%rdnw                                              &
                         ,grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, grid%xkmh, grid%xkhh            &
                         ,grid%diff_6th_opt, grid%diff_6th_factor                                              &
                         ,config_flags%momentum_adv_opt                                                        &
                         ,grid%dampcoef,grid%zdamp,config_flags%damp_opt,config_flags%rad_nudge                &
                         ,grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m                          &
                         ,config_flags%non_hydrostatic, config_flags%top_lid                                   &
                         ,grid%u_frame, grid%v_frame                                                           &
                         ,ids, ide, jds, jde, kds, kde                                                         &
                         ,ims, ime, jms, jme, kms, kme                                                         &
                         ,grid%i_start(ij), grid%i_end(ij)                                                     &
                         ,grid%j_start(ij), grid%j_end(ij)                                                     &
                         ,k_start, k_end                                                                       &
                         ,max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij)                                         )
     END DO
     !$OMP END PARALLEL DO
BENCH_END(rk_tend_tim)

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

BENCH_START(relax_bdy_dry_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       IF ( (config_flags%specified .or. config_flags%nested) .and. ( rk_step == 1 ) ) THEN 

         CALL relax_bdy_dry ( config_flags,                                &
                              grid%u_save, grid%v_save, ph_save, grid%t_save,             &
                              w_save, mu_tend, grid%c1h, grid%c2h, grid%c1f, grid%c2f, &
                              grid%ru, grid%rv, grid%ph_2, grid%t_2,                           &
                              grid%w_2, grid%mu_2, grid%mut,                              &
                              grid%u_bxs,grid%u_bxe,grid%u_bys,grid%u_bye, &
                              grid%v_bxs,grid%v_bxe,grid%v_bys,grid%v_bye, &
                              grid%ph_bxs,grid%ph_bxe,grid%ph_bys,grid%ph_bye, &
                              grid%t_bxs,grid%t_bxe,grid%t_bys,grid%t_bye, &
                              grid%w_bxs,grid%w_bxe,grid%w_bys,grid%w_bye, &
                              grid%mu_bxs,grid%mu_bxe,grid%mu_bys,grid%mu_bye, &
                              grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye, &
                              grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye, &
                              grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                              grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye, &
                              grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye, &
                              grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                              config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone,       &
                              grid%dtbc, grid%fcx, grid%gcx,                              &
                              ids,ide, jds,jde, kds,kde,                   &
                              ims,ime, jms,jme, kms,kme,                   &
                              ips,ipe, jps,jpe, kps,kpe,                   &
                              grid%i_start(ij), grid%i_end(ij),            &
                              grid%j_start(ij), grid%j_end(ij),            &
                              k_start, k_end                              )

       ENDIF

       CALL rk_addtend_dry( grid%ru_tend,  grid%rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            grid%u_save, grid%v_save, w_save, ph_save, grid%t_save, &
                            mu_tend, mu_tendf, rk_step,                      &
                            grid%c1h, grid%c2h,                              &
                            grid%h_diabatic, grid%mut, grid%msftx,           &
                            grid%msfty, grid%msfux,grid%msfuy,               &
                            grid%msfvx, grid%msfvx_inv, grid%msfvy,          &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            grid%i_start(ij), grid%i_end(ij),                &
                            grid%j_start(ij), grid%j_end(ij),                &
                            k_start, k_end                                  )

       IF( config_flags%specified .or. config_flags%nested ) THEN 
         CALL spec_bdy_dry ( config_flags,                                    &
                             grid%ru_tend, grid%rv_tend, ph_tend, t_tend,               &
                             rw_tend, mu_tend,                                &
                             grid%u_bxs,grid%u_bxe,grid%u_bys,grid%u_bye, &
                             grid%v_bxs,grid%v_bxe,grid%v_bys,grid%v_bye, &
                             grid%ph_bxs,grid%ph_bxe,grid%ph_bys,grid%ph_bye, &
                             grid%t_bxs,grid%t_bxe,grid%t_bys,grid%t_bye, &
                             grid%w_bxs,grid%w_bxe,grid%w_bys,grid%w_bye, &
                             grid%mu_bxs,grid%mu_bxe,grid%mu_bys,grid%mu_bye, &
                             grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye, &
                             grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye, &
                             grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                             grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye, &
                             grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye, &
                             grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                             config_flags%spec_bdy_width, grid%spec_zone,                       &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )


       ENDIF

!---------------------------------------------------------------------------------------------
! KRS 9/12/2012: Inserted new IF block calls to spec_bdy_dry_perturb. If peturb_bdy=1, SKEBS 
! pattern passed in for perturbing the specified boundry conditions.  If peturb_bdy=2, user
! must provide pattern.  mu_2, mub, msf* also passed in for coupling needed for tendecies.
!---------------------------------------------------------------------------------------------
       IF( config_flags%specified .and. config_flags%perturb_bdy==1 ) THEN 
         CALL spec_bdy_dry_perturb ( config_flags,                                 &
                             grid%ru_tend, grid%rv_tend, t_tend,                   &
                             grid%mu_2, grid%mub, grid%c1h, grid%c2h,              &
                             grid%msfux, grid%msfvx, grid%msft,	                   &
                             grid%ru_tendf_stoch, grid%rv_tendf_stoch, grid%rt_tendf_stoch, &
                             config_flags%spec_bdy_width, grid%spec_zone,                   &
                             grid%num_stoch_levels,      & ! stoch dims
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )
     
       ENDIF

       IF( config_flags%specified .and. config_flags%perturb_bdy==2 ) THEN
         CALL spec_bdy_dry_perturb ( config_flags,                                 &
                             grid%ru_tend, grid%rv_tend, t_tend,                   &
                             grid%mu_2, grid%mub, grid%c1h, grid%c2h,              &
                             grid%msfux, grid%msfvx, grid%msft,                    &
                             grid%field_u_tend_perturb, grid%field_v_tend_perturb, grid%field_t_tend_perturb, &
                             config_flags%spec_bdy_width, grid%spec_zone,                   &
                             grid%num_stoch_levels,      & ! stoch dims
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )
  
       ENDIF

     END DO
     !$OMP END PARALLEL DO
BENCH_END(relax_bdy_dry_tim)

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

BENCH_START(small_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first 
    ! predictor because we may have changed the small timestep -> grid%dts.

       CALL wrf_debug ( 200 , ' call small_step_prep ' )

       CALL small_step_prep( grid%u_1,grid%u_2,grid%v_1,grid%v_2,grid%w_1,grid%w_2,   &
                             grid%t_1,grid%t_2,grid%ph_1,grid%ph_2,                   &
                             grid%mub, grid%mu_1, grid%mu_2,                          &
                             grid%muu, grid%muus, grid%muv, grid%muvs,                &
                             grid%mut, grid%muts, grid%mudf,                          &
                             grid%c1h, grid%c2h, grid%c1f, grid%c2f,                  &
                             grid%c3h, grid%c4h, grid%c3f, grid%c4f,                  &
                             grid%u_save, grid%v_save, w_save,                        &
                             grid%t_save, ph_save, mu_save,                           &
                             grid%ww, ww1,                                            &
                             c2a, grid%pb, grid%p, grid%alt,                          &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,       &
                             grid%msfvy, grid%msftx,grid%msfty,                       &
                             grid%rdx, grid%rdy, rk_step,                             &
                             ids, ide, jds, jde, kds, kde,                            &
                             ims, ime, jms, jme, kms, kme,                            &
                             grid%i_start(ij), grid%i_end(ij),                        &
                             grid%j_start(ij), grid%j_end(ij),                        &
                             k_start    , k_end                                       )
 
       CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                        grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                        grid%mu_2, grid%muts,                       &
                        grid%c1h, grid%c2h, grid%c1f, grid%c2f,     &
                        grid%c3h, grid%c4h, grid%c3f, grid%c4f,     &
                        grid%znu, t0,                               &
                        grid%rdnw, grid%dnw, grid%smdiv,            &
                        config_flags%non_hydrostatic, 0,            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        grid%i_start(ij), grid%i_end(ij),           &
                        grid%j_start(ij), grid%j_end(ij),           &
                        k_start    , k_end                          )

       IF (config_flags%non_hydrostatic) THEN
         CALL calc_coef_w( a,alpha,gamma,                    &
                           grid%mut,                         &
                           grid%c1h, grid%c2h, grid%c1f, grid%c2f, &
                           grid%c3h, grid%c4h, grid%c3f, grid%c4f, &
                           cqw, grid%rdn, grid%rdnw, c2a,    &
                           dts_rk, g, grid%epssm,            &
                           config_flags%top_lid,             &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
       ENDIF

     ENDDO
     !$OMP END PARALLEL DO
BENCH_END(small_step_prep_tim)

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
#      include "HALO_EM_B.inc"
#      include "PERIOD_BDY_EM_B.inc"
#endif

BENCH_START(set_phys_bc2_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL set_physical_bc3d( grid%ru_tend, 'u', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%rv_tend, 'v', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,         &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%al, 'p', config_flags,           &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%p, 'p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%t_1, 'p', config_flags,          &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc3d( grid%t_save, 't', config_flags,       &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

       CALL set_physical_bc2d( grid%mu_1, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL set_physical_bc2d( grid%mu_2, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

       CALL set_physical_bc2d( grid%mudf, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

     END DO
     !$OMP END PARALLEL DO
BENCH_END(set_phys_bc2_tim)
     small_steps : DO iteration = 1 , number_of_small_timesteps

       ! Boundary condition time (or communication time).  
#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_B.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

BENCH_START(advance_uv_tim)
         CALL advance_uv ( grid%u_2, grid%ru_tend, grid%v_2, grid%rv_tend,        &
                           grid%p, grid%pb,                                       &
                           grid%ph_2, grid%php, grid%alt,  grid%al,               &
                           grid%mu_2, grid%muu, cqu, grid%muv, cqv, grid%mudf,    &
                           grid%c1h, grid%c2h, grid%c1f, grid%c2f,                &
                           grid%c3h, grid%c4h, grid%c3f, grid%c4f,                &
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
                           k_start    , k_end                                     )
BENCH_END(advance_uv_tim)

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

BENCH_START(spec_bdy_uv_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN
           CALL spec_bdyupdate(grid%u_2, grid%ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

           CALL spec_bdyupdate(grid%v_2, grid%rv_tend, dts_rk,      &
                                'v'         , config_flags, &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

         ENDIF
BENCH_END(spec_bdy_uv_tim)

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
#     include "HALO_EM_C.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate ww

BENCH_START(advance_mu_t_tim)
         CALL advance_mu_t( grid%ww, ww1, grid%u_2, grid%u_save, grid%v_2, grid%v_save, &
                          grid%mu_2, grid%mut, muave, grid%muts, grid%muu, grid%muv,    &
                          grid%mudf,                                                    &
                          grid%c1h, grid%c2h, grid%c1f, grid%c2f,                       &
                          grid%c3h, grid%c4h, grid%c3f, grid%c4f,                       &
                          grid%ru_m, grid%rv_m, grid%ww_m,                              &
                          grid%t_2, grid%t_save, t_2save, t_tend,                       &
                          mu_tend,                                                      &
                          grid%rdx, grid%rdy, dts_rk, grid%epssm,                       &
                          grid%dnw, grid%fnm, grid%fnp, grid%rdnw,                      &
                          grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,            &
                          grid%msfvy, grid%msftx,grid%msfty,                            &
                          iteration, config_flags,                                      &
                          ids, ide, jds, jde, kds, kde,      &
                          ims, ime, jms, jme, kms, kme,      &
                          grid%i_start(ij), grid%i_end(ij),  &
                          grid%j_start(ij), grid%j_end(ij),  &
                          k_start    , k_end                )
BENCH_END(advance_mu_t_tim)
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

BENCH_START(spec_bdy_t_tim)

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL spec_bdyupdate(grid%t_2, t_tend, dts_rk,        &
                               't'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end              )

           CALL spec_bdyupdate(grid%mu_2, mu_tend, dts_rk,       &
                               'm'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,       &
                               ims,ime, jms,jme, 1  ,1  ,       &
                               ips,ipe, jps,jpe, 1  ,1  ,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               1    , 1             )

           CALL spec_bdyupdate(grid%muts, mu_tend, dts_rk,      &
                              'm'         , config_flags, &
                              grid%spec_zone,                  &
                              ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                              ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                              ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                              grid%i_start(ij), grid%i_end(ij),         &
                              grid%j_start(ij), grid%j_end(ij),         &
                              1    , 1             )
         ENDIF
BENCH_END(spec_bdy_t_tim)

         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.


BENCH_START(advance_w_tim)
         IF ( config_flags%non_hydrostatic ) THEN
           CALL advance_w( grid%w_2, rw_tend, grid%ww, w_save,         &
                           grid%u_2, grid%v_2,                         &
                           grid%mu_2, grid%mut, muave, grid%muts,      &
                           grid%c1h, grid%c2h, grid%c1f, grid%c2f,     &
                           grid%c3h, grid%c4h, grid%c3f, grid%c4f,     &
                           t_2save, grid%t_2, grid%t_save,             &
                           grid%ph_2, ph_save, grid%phb, ph_tend,      &
                           grid%ht, c2a, cqw, grid%alt, grid%alb,      &
                           a, alpha, gamma,                            &
                           grid%rdx, grid%rdy, dts_rk, t0, grid%epssm, &
                           grid%dnw, grid%fnm, grid%fnp, grid%rdnw,    &
                           grid%rdn, grid%cf1, grid%cf2, grid%cf3,     &
                           grid%msftx, grid%msfty,                     &
                           config_flags,  config_flags%top_lid,        &
                           ids,ide, jds,jde, kds,kde,                  &
                           ims,ime, jms,jme, kms,kme,                  &
                           grid%i_start(ij), grid%i_end(ij),           &
                           grid%j_start(ij), grid%j_end(ij),           &
                           k_start    , k_end                          )
         ENDIF
BENCH_END(advance_w_tim)

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

BENCH_START(sumflux_tim)
         CALL sumflux ( grid%u_2, grid%v_2, grid%ww,          &
                        grid%u_save, grid%v_save, ww1,        &
                        grid%muu, grid%muv,                   &
                        grid%c1h, grid%c2h, grid%c1f, grid%c2f, &
                        grid%c3h, grid%c4h, grid%c3f, grid%c4f, &
                        grid%ru_m, grid%rv_m, grid%ww_m, grid%epssm,  &
                        grid%msfux, grid% msfuy, grid%msfvx,  &
                        grid%msfvx_inv, grid%msfvy,           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )
BENCH_END(sumflux_tim)

         IF( config_flags%specified .or. config_flags%nested ) THEN

BENCH_START(spec_bdynhyd_tim)
           IF (config_flags%non_hydrostatic)  THEN
             CALL spec_bdyupdate_ph( ph_save, grid%ph_2, ph_tend,     &
                                     mu_tend, grid%muts,              &
                                     grid%c1f, grid%c2f, dts_rk,      &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )
             IF( config_flags%specified ) THEN
               CALL zero_grad_bdy ( grid%w_2,                         &
                                    'w'         , config_flags,       &
                                    grid%spec_zone,                   &
                                    ids,ide, jds,jde, kds,kde,        &
                                    ims,ime, jms,jme, kms,kme,        &
                                    ips,ipe, jps,jpe, kps,kpe,        &
                                    grid%i_start(ij), grid%i_end(ij), &
                                    grid%j_start(ij), grid%j_end(ij), &
                                    k_start    , k_end                )
             ELSE
               CALL spec_bdyupdate ( grid%w_2, rw_tend, dts_rk,       &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )
             ENDIF
           ENDIF
BENCH_END(spec_bdynhyd_tim)
         ENDIF

BENCH_START(cald_p_rho_tim)
         CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                          grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                          grid%mu_2, grid%muts,                       &
                          grid%c1h, grid%c2h, grid%c1f, grid%c2f,     &
                          grid%c3h, grid%c4h, grid%c3f, grid%c4f,     &
                          grid%znu, t0,                               &
                          grid%rdnw, grid%dnw, grid%smdiv,            &
                          config_flags%non_hydrostatic, iteration,    &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
BENCH_END(cald_p_rho_tim)

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

#      include "HALO_EM_C2.inc"
#      include "PERIOD_BDY_EM_B3.inc"
#endif

BENCH_START(phys_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

       ! boundary condition set for next small timestep

         CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( grid%p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

       END DO
       !$OMP END PARALLEL DO
BENCH_END(phys_bc_tim)

     END DO small_steps

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to 
      ! full perturbation variables.
      ! first get updated mu at u and v points

BENCH_START(calc_mu_uv_tim)
       CALL calc_mu_uv_1 ( config_flags,                     &
                           grid%muts, grid%muus, grid%muvs,  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
BENCH_END(calc_mu_uv_tim)
BENCH_START(small_step_finish_tim)
       CALL small_step_finish( grid%u_2, grid%u_1, grid%v_2, grid%v_1, grid%w_2, grid%w_1,     &
                               grid%t_2, grid%t_1, grid%ph_2, grid%ph_1, grid%ww, ww1,    &
                               grid%mu_2, grid%mu_1,                       &
                               grid%mut, grid%muts, grid%muu, grid%muus, grid%muv, grid%muvs,  & 
                               grid%c1h, grid%c2h, grid%c1f, grid%c2f, &
                               grid%c3h, grid%c4h, grid%c3f, grid%c4f, &
                               grid%u_save, grid%v_save, w_save,           &
                               grid%t_save, ph_save, mu_save,         &
                               grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,grid%msfty, &
                               grid%h_diabatic,                       &
                               number_of_small_timesteps,dts_rk, &
                               rk_step, rk_order,                &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end               )
!  call  to set ru_m, rv_m and ww_m b.c's for PD advection

       IF (rk_step == rk_order) THEN

         CALL set_physical_bc3d( grid%ru_m, 'u', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc3d( grid%rv_m, 'v', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc3d( grid%ww_m, 'w', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

         CALL set_physical_bc2d( grid%mut, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )
 
       END IF

BENCH_END(small_step_finish_tim)

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

     IF ((config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. &
         (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_m
           CALL rk_update_scalar_pd( im, im,                                   &
                                     moist_old(ims,kms,jms,im),                &
                                     moist_tend(ims,kms,jms,im),               &
                                     grid%c1h, grid%c2h,                       &
                                     grid%mu_1, grid%mu_1, grid%mub,  &
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
#     include "HALO_EM_MOIST_OLD_E_5.inc"
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_MOIST_OLD_E_7.inc"
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
             CALL set_physical_bc3d( moist_old(ims,kms,jms,im), 'p', config_flags,   &
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

     IF ((config_flags%scalar_adv_opt /= ORIGINAL .and. config_flags%scalar_adv_opt /= WENO_SCALAR) .and. &
         (rk_step == rk_order)) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_s
           CALL rk_update_scalar_pd( im, im,                                  &
                                     scalar_old(ims,kms,jms,im),              &
                                     scalar_tend(ims,kms,jms,im),             &
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
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
               CALL set_physical_bc3d(  scalar_old(ims,kms,jms,im), 'p', config_flags, &
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
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c
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
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_tracer
             CALL rk_update_scalar_pd( im, im,                                  &
                                       tracer_old(ims,kms,jms,im),                &
                                       tracer_tend(ims,kms,jms,im),               &
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
         IF (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) THEN
           IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_TRACER_OLD_E_5.inc"
           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_TRACER_OLD_E_7.inc"
           ELSE
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
               CALL set_physical_bc3d(  tracer_old(ims,kms,jms,im), 'p', config_flags,   &
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
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           CALL rk_update_scalar_pd( 1, 1,                                    &
                                     grid%tke_1,                              &
                                     tke_tend(ims,kms,jms),                   &
                                     grid%c1h, grid%c2h,                      &
                                     grid%mu_1, grid%mu_1, grid%mub,          &
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
#     include "HALO_EM_TKE_OLD_E_5.inc"
           ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_TKE_OLD_E_7.inc"
           ELSE
             WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
           CALL set_physical_bc3d(  grid%tke_1, 'p', config_flags,  &
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

#  include "HALO_EM_D.inc"
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

               CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
               tenddec = .false.

BENCH_START(rk_scalar_tend_tim)
               CALL rk_scalar_tend (  im, im, config_flags, tenddec,         & 
                           rk_step, dt_rk,                                   &
                           grid%ru_m, grid%rv_m, grid%ww_m,                  &
                           grid%muts, grid%mub, grid%mu_1,                   &
                           grid%c1h, grid%c2h,                               &
                           grid%alt,                                         &
                           moist_old(ims,kms,jms,im),                        &
                           moist(ims,kms,jms,im),                            &
                           moist_tend(ims,kms,jms,im),                       &
                           advect_tend,h_tendency,z_tendency,grid%rqvften,   & 
                           grid%qv_base, .true., grid%fnm, grid%fnp,         &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,&
                           grid%msfvy, grid%msftx,grid%msfty,                & 
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,                            &
                           grid%diff_6th_opt, grid%diff_6th_factor,          &
                           config_flags%moist_adv_opt,                       &
                           grid%phb, grid%ph_2,                              &
                           config_flags%moist_mix2_off,                      &
                           config_flags%moist_mix6_off,                      &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

               IF( rk_step == 1 .AND. config_flags%use_q_diabatic == 1 )THEN
               IF( im.eq.p_qv .or. im.eq.p_qc )THEN
                   CALL q_diabatic_add  ( im, im,            & 
                           dt_rk, grid%mut,                  &
                           grid%c1h, grid%c2h,               &
                           grid%qv_diabatic,                 &
                           grid%qc_diabatic,                 &
                           moist_tend(ims,kms,jms,im),       &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
               ENDIF
               ENDIF

BENCH_END(rk_scalar_tend_tim)

BENCH_START(rlx_bdy_scalar_tim)
               IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN 
                 IF ( im .EQ. P_QV .OR. config_flags%nested .OR. &
                    ( config_flags%specified .AND. config_flags%have_bcs_moist ) ) THEN
                   CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                     moist(ims,kms,jms,im),  grid%mut,         &
                                     grid%c1h, grid%c2h,                       &
                                     moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,             &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),      &
                                     grid%j_start(ij), grid%j_end(ij),      &
                                     k_start, k_end                        )

                   CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                     moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
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
BENCH_END(rlx_bdy_scalar_tim)

             ENDDO moist_tile_loop_1
             !$OMP END PARALLEL DO

             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij, tenddec )
             moist_tile_loop_2: DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 200 , ' call rk_update_scalar' )
               tenddec = .false.

BENCH_START(update_scal_tim)
               CALL rk_update_scalar( scs=im, sce=im,                                  &
                               scalar_1=moist_old(ims,kms,jms,im),                     &
                               scalar_2=moist(ims,kms,jms,im),                         &
                               sc_tend=moist_tend(ims,kms,jms,im),                     &
                               advect_tend=advect_tend,                                &
                               h_tendency=h_tendency, z_tendency=z_tendency,           & 
                               msftx=grid%msftx,msfty=grid%msfty,                      &
                               c1=grid%c1h, c2=grid%c2h,                               &
                               mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                               rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                               config_flags=config_flags, tenddec=tenddec,             & 
                               ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                               ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                               its=grid%i_start(ij), ite=grid%i_end(ij),               &
                               jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                               kts=k_start    , kte=k_end                              )
               IF( rk_step == rk_order .AND. config_flags%use_q_diabatic == 1 )THEN
               IF( im.eq.p_qv .or. im.eq.p_qc )THEN
                   CALL q_diabatic_subtr( im, im,            & 
                           dt_rk,                            &
                           grid%qv_diabatic,                 &
                           grid%qc_diabatic,                 &
                           moist(ims,kms,jms,im),            &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
               ENDIF
               ENDIF
BENCH_END(update_scal_tim)

BENCH_START(flow_depbdy_tim)
               IF( config_flags%specified .AND. ( .NOT. config_flags%have_bcs_moist ) ) THEN
                 IF(im .ne. P_QV)THEN
                   CALL flow_dep_bdy  (  moist(ims,kms,jms,im),                 &
                                grid%ru_m, grid%rv_m, config_flags,             &
                                grid%spec_zone,                                 &
                                ids,ide, jds,jde, kds,kde,                      &
                                ims,ime, jms,jme, kms,kme,                      &
                                ips,ipe, jps,jpe, kps,kpe,                      &
                                grid%i_start(ij), grid%i_end(ij),               &
                                grid%j_start(ij), grid%j_end(ij),               &
                                k_start, k_end                               )
                 ENDIF
               ENDIF
BENCH_END(flow_depbdy_tim)

             ENDDO moist_tile_loop_2
             !$OMP END PARALLEL DO

           ENDIF  !-- if (grid%adv_moist_cond .or. im==p_qv ) then

         ENDDO moist_variable_loop

       ENDIF moist_scalar_advance

BENCH_START(tke_adv_tim)
       TKE_advance: IF (config_flags%km_opt .eq. 2) then
#ifdef DM_PARALLEL
         IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_ADVECT_3.inc"
         ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_ADVECT_5.inc"
         ELSE
          WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
          CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
#endif
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         tke_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
           tenddec = .false.
           CALL rk_scalar_tend ( 1, 1, config_flags, tenddec,                      & 
                            rk_step, dt_rk,                                        &
                            grid%ru_m, grid%rv_m, grid%ww_m,                       &
                            grid%muts, grid%mub, grid%mu_1,                        &
                            grid%c1h, grid%c2h,                                    &
                            grid%alt,                                              &
                            grid%tke_1,                                            &
                            grid%tke_2,                                            &
                            tke_tend(ims,kms,jms),                                 &
                            advect_tend,h_tendency,z_tendency,grid%rqvften,        & 
                            grid%qv_base, .false., grid%fnm, grid%fnp,             &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,     &
                            grid%msfvy, grid%msftx,grid%msfty,                     &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif,   &
                            grid%kvdif, grid%xkhh,                                 &
                            grid%diff_6th_opt, grid%diff_6th_factor,               &
                            config_flags%tke_adv_opt,                              &
                            grid%phb, grid%ph_2,                                   &
                            config_flags%tke_mix2_off,                             &
                            config_flags%tke_mix6_off,                             &
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

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=1,  sce=1,                                          &
                                  scalar_1=grid%tke_1,                                    &
                                  scalar_2=grid%tke_2,                                    &
                                  sc_tend=tke_tend(ims,kms,jms),                          &
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  c1=grid%c1h, c2=grid%c2h,                               &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

! bound the tke (greater than 0, less than tke_upper_bound)

           CALL bound_tke( grid%tke_2, grid%tke_upper_bound,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           grid%i_start(ij), grid%i_end(ij),    &
                           grid%j_start(ij), grid%j_end(ij),    &
                           k_start    , k_end                  )

           IF( config_flags%specified .or. config_flags%nested ) THEN
              CALL flow_dep_bdy (  grid%tke_2,                     &
                                   grid%ru_m, grid%rv_m, config_flags,               &
                                   grid%spec_zone,                              &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij),       &
                                   grid%j_start(ij), grid%j_end(ij),       &
                                   k_start, k_end                               )
           ENDIF
         ENDDO tke_tile_loop_2
         !$OMP END PARALLEL DO

       ENDIF TKE_advance
BENCH_END(tke_adv_tim)

#if (WRF_CHEM == 1)
!  next the chemical species
BENCH_START(chem_adv_tim)
       chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

         chem_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_3d_c

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij, tenddec )
           chem_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 200 , ' call rk_scalar_tend in chem_tile_loop_1' )
             tenddec = (( config_flags%chemdiag == USECHEMDIAG ) .and. &
                        ( adv_ct_indices(ic) >= PARAM_FIRST_SCALAR ))
             CALL rk_scalar_tend ( ic, ic, config_flags, tenddec,                & 
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%c1h, grid%c2h,                                &
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
                              grid%phb, grid%ph_2,                               &
                              config_flags%chem_mix2_off,                        &
                              config_flags%chem_mix6_off,                        &
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
             CALL relax_bdy_scalar ( chem_tend(ims,kms,jms,ic),                                    &
                                     chem(ims,kms,jms,ic),  grid%mut,                              &
                                     grid%c1h, grid%c2h,                       &
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

if ( config_flags%do_pvozone ) then
#ifdef DM_PARALLEL
#  include "HALO_EM_D_PV.inc"
#endif
end if

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )

         chem_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = (( config_flags%chemdiag == USECHEMDIAG ) .and. &
                      ( adv_ct_indices(ic) >= PARAM_FIRST_SCALAR ))
           CALL rk_update_scalar( scs=ic, sce=ic,                                         &
                                  scalar_1=chem_old(ims,kms,jms,ic),                      &
                                  scalar_2=chem(ims,kms,jms,ic),                          &
                                  sc_tend=chem_tend(ims,kms,jms,ic),                      &
                                  advh_t=advh_ct(ims,kms,jms,adv_ct_indices(ic)),         &
                                  advz_t=advz_ct(ims,kms,jms,adv_ct_indices(ic)),         &
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  c1=grid%c1h, c2=grid%c2h,                               &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified  ) THEN

             IF( config_flags%perturb_chem_bdy==1 ) THEN

                 IF(ic.eq.PARAM_FIRST_SCALAR .and. ij.eq.1) &
                 CALL wrf_debug (10 , ' spec_bdy_chem_perturb' )

                 CALL spec_bdy_chem_perturb ( config_flags%periodic_x,   &
                      chem_btxs(jms,kms,1,ic), chem_btxe(jms,kms,1,ic),  &
                      chem_btys(ims,kms,1,ic), chem_btye(ims,kms,1,ic),  &
                      grid%rand_pert,                                    &
                      config_flags%spec_bdy_width, grid%spec_zone,       &
                      grid%num_stoch_levels,      & ! stoch  dims
                      ids,ide, jds,jde, kds,kde,  & ! domain dims
                      ims,ime, jms,jme, kms,kme,  & ! memory dims
                      ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                      grid%i_start(ij), grid%i_end(ij),                  &
                      grid%j_start(ij), grid%j_end(ij),                  &
                      k_start, k_end                                  )
             ENDIF

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
                                     grid%spec_zone,ic,grid%julday,      &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),   &
                                     grid%j_start(ij), grid%j_end(ij),   &
                                     k_start, k_end,                     &
                                     grid%u_2,grid%v_2,grid%t_2,grid%znu,grid%msft, &
                                     grid%msfu,grid%msfv,grid%f,grid%mub,grid%dx,grid%xlat,grid%pv)

              ENDIF
         ENDDO chem_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO chem_variable_loop
     ENDIF chem_scalar_advance
BENCH_END(chem_adv_tim)
#endif
!  next the chemical species
BENCH_START(tracer_adv_tim)
       tracer_advance: IF (num_tracer >= PARAM_FIRST_SCALAR)  THEN

         tracer_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_tracer

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij, tenddec )
           tracer_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 15 , ' call rk_scalar_tend in tracer_tile_loop_1' )
             tenddec = .false.
             CALL rk_scalar_tend ( ic, ic, config_flags, tenddec,                & 
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%c1h, grid%c2h,                                &
                              grid%alt,                                          &
                              tracer_old(ims,kms,jms,ic),                        &
                              tracer(ims,kms,jms,ic),                            &
                              tracer_tend(ims,kms,jms,ic),                       &
                              advect_tend,h_tendency,z_tendency,grid%rqvften,    & 
                              grid%qv_base, .false., grid%fnm, grid%fnp,         &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx,grid%msfty,                 &
                              grid%rdx, grid%rdy, grid%rdn, grid%rdnw,           &
                              grid%khdif, grid%kvdif, grid%xkhh,                 &
                              grid%diff_6th_opt, grid%diff_6th_factor,           &
                              config_flags%tracer_adv_opt,                       &
                              grid%phb, grid%ph_2,                               &
                              config_flags%tracer_mix2_off,                      &
                              config_flags%tracer_mix6_off,                      &
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
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_tracer' )
             CALL relax_bdy_scalar ( tracer_tend(ims,kms,jms,ic),                                    &
                                     tracer(ims,kms,jms,ic),  grid%mut,                              &
                                     grid%c1h, grid%c2h,                       &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),                &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),                &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),              &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),              &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,                                &
                                     config_flags,                                                 &
                                     ids,ide, jds,jde, kds,kde,                                    &
                                     ims,ime, jms,jme, kms,kme,                                    &
                                     ips,ipe, jps,jpe, kps,kpe,                                    &
                                     grid%i_start(ij), grid%i_end(ij),                             &
                                     grid%j_start(ij), grid%j_end(ij),                             &
                                     k_start, k_end                                                )
             CALL spec_bdy_scalar  ( tracer_tend(ims,kms,jms,ic),                 &
                                     tracer_bxs(jms,kms,1,ic),tracer_bxe(jms,kms,1,ic),                &
                                     tracer_bys(ims,kms,1,ic),tracer_bye(ims,kms,1,ic),                &
                                     tracer_btxs(jms,kms,1,ic),tracer_btxe(jms,kms,1,ic),              &
                                     tracer_btys(ims,kms,1,ic),tracer_btye(ims,kms,1,ic),              &
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

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=ic, sce=ic,                                         &
                                  scalar_1=tracer_old(ims,kms,jms,ic),                    &
                                  scalar_2=tracer(ims,kms,jms,ic),                        &
                                  sc_tend=tracer_tend(ims,kms,jms,ic),                    &
!                                 advh_t=advh_t(ims,kms,jms,1),                           & 
!                                 advz_t=advz_t(ims,kms,jms,1),                           & 
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  c1=grid%c1h, c2=grid%c2h,                               &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF( config_flags%specified  ) THEN
#if (WRF_CHEM == 1)
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
             CALL flow_dep_bdy  ( tracer(ims,kms,jms,ic),     &
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
BENCH_END(tracer_adv_tim)

!  next the other scalar species
     other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

       scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
           tenddec = .false.
           CALL rk_scalar_tend ( is, is, config_flags, tenddec,                   & 
                                 rk_step, dt_rk,                                  &
                                 grid%ru_m, grid%rv_m, grid%ww_m,                 &
                                 grid%muts, grid%mub, grid%mu_1,                  &
                                 grid%c1h, grid%c2h,                              &
                                 grid%alt,                                        &
                                 scalar_old(ims,kms,jms,is),                      &
                                 scalar(ims,kms,jms,is),                          &
                                 scalar_tend(ims,kms,jms,is),                     &
                                 advect_tend,h_tendency,z_tendency,grid%rqvften,  & 
                                 grid%qv_base, .false., grid%fnm, grid%fnp,       &
                                 grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                                 grid%msfvy, grid%msftx,grid%msfty,               &
                                 grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                                 grid%khdif, grid%kvdif, grid%xkhh,               &
                                 grid%diff_6th_opt, grid%diff_6th_factor,         &
                                 config_flags%scalar_adv_opt,                     &
                                 grid%phb, grid%ph_2,                             &
                                 config_flags%scalar_mix2_off,                    &
                                 config_flags%scalar_mix6_off,                    &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

           IF( rk_step == 1 ) THEN
             IF ( config_flags%nested .OR. &
                ( config_flags%specified .AND. config_flags%have_bcs_scalar ) .OR. &
                ( ( is .EQ. P_QNWFA .OR. is .EQ. P_QNIFA) .AND. config_flags%use_aero_icbc ) ) THEN

               CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),                            &
                                       scalar(ims,kms,jms,is),  grid%mut,                      &
                                       grid%c1h, grid%c2h,                       &
                                       scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is),      &
                                       scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is),      &
                                       scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is),    &
                                       scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is),    &
                                       config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                       grid%dtbc, grid%fcx, grid%gcx,                          &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

               CALL spec_bdy_scalar  ( scalar_tend(ims,kms,jms,is),                            &
                                       scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is),      &
                                       scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is),      &
                                       scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is),    &
                                       scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is),    &
                                       config_flags%spec_bdy_width, grid%spec_zone,            &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

             ENDIF
           ENDIF ! b.c test for scalars

         ENDDO scalar_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij, tenddec )
         scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )
           tenddec = .false.
           CALL rk_update_scalar( scs=is, sce=is,                                         &
                                  scalar_1=scalar_old(ims,kms,jms,is),                    &
                                  scalar_2=scalar(ims,kms,jms,is),                        &
                                  sc_tend=scalar_tend(ims,kms,jms,is),                    &
!                                 advh_t=advh_t(ims,kms,jms,1),                           & 
!                                 advz_t=advz_t(ims,kms,jms,1),                           & 
                                  advect_tend=advect_tend,                                &
                                  h_tendency=h_tendency, z_tendency=z_tendency,           & 
                                  msftx=grid%msftx,msfty=grid%msfty,                      &
                                  c1=grid%c1h, c2=grid%c2h,                               &
                                  mu_old=grid%mu_1, mu_new=grid%mu_2, mu_base=grid%mub,   &
                                  rk_step=rk_step, dt=dt_rk, spec_zone=grid%spec_zone,    &
                                  config_flags=config_flags, tenddec=tenddec,             & 
                                  ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,   &
                                  ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,   &
                                  its=grid%i_start(ij), ite=grid%i_end(ij),               &
                                  jts=grid%j_start(ij), jte=grid%j_end(ij),               &
                                  kts=k_start    , kte=k_end                              )

           IF ( config_flags%specified ) THEN

             IF ( is .EQ. P_QNN ) THEN
               CALL flow_dep_bdy_qnn  ( scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  grid%ccn_conc,              & ! RAS
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )
             ELSE IF ( ( ( ( is .EQ. P_QNWFA ) .OR. ( is .EQ. P_QNIFA ) ) .AND. &
                         ( .NOT. config_flags%use_aero_icbc ) ) &
                         .OR. &
                       ( ( .NOT. ( ( is .EQ. P_QNWFA ) .OR. ( is .EQ. P_QNIFA ) ) ) .AND. &
                         ( .NOT. config_flags%have_bcs_scalar ) ) ) THEN

!     A = ( is .EQ. P_QNWFA ) .OR. ( is .EQ. P_QNIFA )
!     B = config_flags%use_aero_icbc
!     C = config_glags%have_bcs_scalar

! Test| A  | B  | C | ( A AND NOT B ) OR ( NOT A AND NOT C ) 
! ----+----+----+---+-----------------------------------------------
!  1  | T  | T  | T |                 F = DO NOT CALL flow_dep_bdy
!  2  | T  | T  | F |                 F = DO NOT CALL flow_dep_bdy
!  3  | T  | F  | T |                 T =        CALL flow_dep_bdy
!  4  | T  | F  | F |                 T =        CALL flow_dep_bdy
!  5  | F  | T  | T |                 F = DO NOT CALL flow_dep_bdy
!  6  | F  | T  | F |                 T =        CALL flow_dep_bdy
!  7  | F  | F  | T |                 F = DO NOT CALL flow_dep_bdy
!  8  | F  | F  | F |                 T =        CALL flow_dep_bdy
! ----+----+----+---+-----------------------------------------------

!  If this is     the special friendly fields AND are to use the aero icbc, then NO calls to flow dep: tests 1 and 2
!  If this is     the special friendly fields AND do not use the aero icbc, then    call     flow dep: tests 3 and 4
!  If this is not the special friendly fields AND: 
!           If we        have bcs for scalars, do not call flow dep: tests 5 and 7
!           If we do not have bcs for scalars,        call flow dep: tests 6 and 8

               CALL flow_dep_bdy  ( scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
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

BENCH_START(calc_p_rho_tim)

       CALL calc_p_rho_phi( moist, num_3d_m, config_flags%hypsometric_opt,                    &
                            grid%al, grid%alb, grid%mu_2, grid%muts,                          &
                            grid%c1h, grid%c2h, grid%c3h, grid%c4h, grid%c3f, grid%c4f,       &
                            grid%ph_2, grid%phb, grid%p, grid%pb, grid%t_2,                   &
                            p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,      &
                            grid%rdn, config_flags%non_hydrostatic,config_flags%use_theta_m,  &
                            ids, ide, jds, jde, kds, kde,                                     &
                            ims, ime, jms, jme, kms, kme,                                     &
                            grid%i_start(ij), grid%i_end(ij),                                 &
                            grid%j_start(ij), grid%j_end(ij),                                 &
                            k_start    , k_end                                                )

BENCH_END(calc_p_rho_tim)

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
             CALL couple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)               &
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
             CALL couple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)               &
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
                    ,flag_tracer        = im                                     &
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
             CALL couple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)           &
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
#    include "HALO_EM_D2_3.inc"
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D2_5.inc"
       ELSE 
         WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF
#  include "PERIOD_BDY_EM_D.inc"
#  include "PERIOD_BDY_EM_MOIST2.inc"
#  include "PERIOD_BDY_EM_CHEM2.inc"
#  include "PERIOD_BDY_EM_TRACER2.inc"
#  include "PERIOD_BDY_EM_SCALAR2.inc"
#  include "PERIOD_BDY_EM_TKE.inc"
#endif

BENCH_START(bc_end_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       tile_bc_loop_1: DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

         CALL rk_phys_bc_dry_2( config_flags,                     &
                                grid%u_2, grid%v_2, grid%w_2,     &
                                grid%t_2, grid%ph_2, grid%mu_2,   &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end               )

BENCH_START(diag_w_tim)
         IF (.not. config_flags%non_hydrostatic) THEN
           CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, &
                            grid%c1f, grid%c2f, dt_rk,              &
                            grid%u_2, grid%v_2, grid%ht,            &
                            grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                            ids, ide, jds, jde, kds, kde,           &
                            ims, ime, jms, jme, kms, kme,           &
                            grid%i_start(ij), grid%i_end(ij),       &
                            grid%j_start(ij), grid%j_end(ij),       &
                            k_start    , k_end                     )
         ENDIF
BENCH_END(diag_w_tim)

         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

           moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
  
             CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p', config_flags,   &
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

             CALL set_physical_bc3d( tracer(ims,kms,jms,ic), 'p', config_flags,   &
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

             CALL set_physical_bc3d( scalar(ims,kms,jms,is), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

           END DO scalar_species_bdy_loop_1

         END IF

         IF (config_flags%km_opt .eq. 2) THEN

           CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,            &
                                   ims, ime, jms, jme, kms, kme,            &
                                   ips, ipe, jps, jpe, kps, kpe,            &
                                   grid%i_start(ij), grid%i_end(ij),        &
                                   grid%j_start(ij), grid%j_end(ij),        &
                                   k_start    , k_end                      )
         END IF

       END DO tile_bc_loop_1
       !$OMP END PARALLEL DO
BENCH_END(bc_end_tim)


#ifdef DM_PARALLEL

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist, chem, scalar, tke      x


       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_5.inc"
         ELSE
#         include "HALO_EM_TKE_3.inc"
         ENDIF
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL .and. config_flags%tke_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_7.inc"
         ELSE
#         include "HALO_EM_TKE_5.inc"
         ENDIF
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF

       IF ( num_moist .GE. PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_5.inc"
           ELSE
#        include "HALO_EM_MOIST_E_3.inc"
           END IF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL .and. config_flags%moist_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_7.inc"
           ELSE
#        include "HALO_EM_MOIST_E_5.inc"
           END IF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
       IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_5.inc"
           ELSE
#        include "HALO_EM_TRACER_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL .and. config_flags%tracer_adv_opt /= WENO_SCALAR) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_7.inc"
           ELSE
#        include "HALO_EM_TRACER_E_5.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
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
!  grid%dmudt=grid%mu_2 - grid%mu_1

#if ( WRFPLUS != 1 )
   IF      ( config_flags%traj_opt .EQ. UM_TRAJECTORY ) THEN
#ifdef DM_PARALLEL
#    include "HALO_EM_F_1.inc"
#    include "HALO_EM_D.inc"
#    include "HALO_EM_INIT_4.inc"
     IF( config_flags%periodic_x ) THEN
#    include "PERIOD_EM_DA.inc"
#    include "PERIOD_EM_F.inc"
#    include "PERIOD_EM_G.inc"
     ENDIF
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

           call trajectory (grid,config_flags,                                     &
                            grid%dt,grid%itimestep,grid%ru_m, grid%rv_m, grid%ww_m,&
                            grid%muts,grid%muus,grid%muvs,                         &
                            grid%c1h, grid%c2h, grid%c1f, grid%c2f,                &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw,grid%rdzw,     &
                            grid%traj_i,grid%traj_j,grid%traj_k,                   &
                            grid%traj_long,grid%traj_lat,                          &
                            grid%xlong,grid%xlat,                                  &
                            grid%msftx,grid%msfux,grid%msfvy,                      &
                            ids, ide, jds, jde, kds, kde,                          &
                            ims, ime, jms, jme, kms, kme,                          &
                            grid%i_start(ij), grid%i_end(ij),                      &
                            grid%j_start(ij), grid%j_end(ij),                      &
                            k_start  , k_end                                       )
        ENDDO
     !$OMP END PARALLEL DO
   ENDIF
#endif
!-----------------------------------------------------------

   IF (config_flags%do_avgflx_em .EQ. 1) THEN
! Reinitialize time-averaged fluxes if history output was written after the previous time step:
      CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM ),prevringtime=temp_time)
      CALL domain_clock_get ( grid, current_time=CurrTime, &
           current_timestr=message2 )
! use overloaded -, .LT. operator to check whether to initialize avgflx:
! reinitialize after each history output (detect this here by comparing current time
! against last history time and time step - this code follows what's done in adapt_timestep_em):
      WRITE ( message , FMT = '("solve_em: old_dt =",g15.6,", dt=",g15.6," on domain ",I3)' ) &
           & old_dt,grid%dt,grid%id
      CALL wrf_debug(200,message)
      old_dt=min(old_dt,grid%dt)
      num = INT(old_dt * precision)
      den = precision
      CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)
      IF (CurrTime .lt. temp_time + dtInterval) THEN
         WRITE ( message , FMT = '("solve_em: initializing avgflx at time ",A," on domain ",I3)' ) &
              & TRIM(message2), grid%id
         CALL wrf_message(trim(message)) 
         grid%avgflx_count = 0
!tile-loop for zero_avgflx
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
            CALL wrf_debug(200,'In solve_em, before zero_avgflx call')
            CALL zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
                 &   ids, ide, jds, jde, kds, kde,           &
                 &   ims, ime, jms, jme, kms, kme,           &
                 &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
                 &   k_start    , k_end, f_flux, &
                 &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
                 &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
            CALL wrf_debug(200,'In solve_em, after zero_avgflx call')
         ENDDO
      ENDIF

! Update avgflx quantities
!tile-loop for upd_avgflx
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         CALL wrf_debug(200,'In solve_em, before upd_avgflx call')
         CALL upd_avgflx(grid%avgflx_count,grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   grid%ru_m, grid%rv_m, grid%ww_m, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
              &   k_start    , k_end, f_flux, &
              &   grid%cfu1,grid%cfd1,grid%dfu1,grid%efu1,grid%dfd1,grid%efd1,          &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
         CALL wrf_debug(200,'In solve_em, after upd_avgflx call')
         
      ENDDO
      grid%avgflx_count = grid%avgflx_count + 1
   ENDIF
!
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

BENCH_START(advance_ppt_tim)
     CALL wrf_debug ( 200 , ' call advance_ppt' )
     CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                      grid%cldfra_cup,                                         & !BSINGH -  Added for CuP scheme
                      grid%rqicuten,grid%rqscuten,           &
                      grid%rainc,grid%raincv,grid%rainsh,grid%pratec,grid%pratesh, &
                      grid%nca,grid%htop,grid%hbot,grid%cutop,grid%cubot,  &
                      grid%cuppt, grid%dt, config_flags,                   &
                      ids,ide, jds,jde, kds,kde,             &
                      ims,ime, jms,jme, kms,kme,             &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start    , k_end                    )
BENCH_END(advance_ppt_tim)

   ENDDO
  !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
        CALL wrf_debug ( 200 , ' call phy_prep_part2' )
        CALL phy_prep_part2 ( config_flags,                              &
                        grid%muts, grid%muus, grid%muvs,                 &
                        grid%c1h, grid%c2h, grid%c1f, grid%c2f,          &
                        grid%rthraten,                                   &
                        grid%rthblten, grid%rublten, grid%rvblten,       &
                        grid%rqvblten, grid%rqcblten, grid%rqiblten,     &
                        grid%rucuten,  grid%rvcuten,  grid%rthcuten,     &
                        grid%rqvcuten, grid%rqccuten, grid%rqrcuten,     &
                        grid%rqicuten, grid%rqscuten,                    &
                        grid%rushten,  grid%rvshten,  grid%rthshten,     &
                        grid%rqvshten, grid%rqcshten, grid%rqrshten,     &
                        grid%rqishten, grid%rqsshten, grid%rqgshten,     &
                        grid%rthften,  grid%rqvften,                     &
                        grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN, &
                        grid%RPHNDGDTEN,grid%RQVNDGDTEN, grid%RMUNDGDTEN,&
                        grid%t_2, th_phy, moist(ims,kms,jms,P_QV),       &
                        ids, ide, jds, jde, kds, kde,                    &
                        ims, ime, jms, jme, kms, kme,                    &
                        grid%i_start(ij), grid%i_end(ij),                &
                        grid%j_start(ij), grid%j_end(ij),                &
                        k_start, k_end                                   )
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

       CALL wrf_debug ( 200 , ' call moist_physics_prep' )
BENCH_START(moist_physics_prep_tim)
       CALL moist_physics_prep_em( grid%t_2, grid%t_1, t0, grid%rho,           &
                                   grid%al, grid%alb, grid%p, p8w, p0, grid%pb,          &
                                   grid%ph_2, grid%phb, th_phy, pi_phy , p_phy, &
                                   grid%z, grid%z_at_w, dz8w,        &
                                   dtm, grid%h_diabatic,                       &
                                   moist(ims,kms,jms,P_QV),grid%qv_diabatic,   &
                                   moist(ims,kms,jms,P_QC),grid%qc_diabatic,   &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

       IF (config_flags%dust_emis.eq.1 .AND. config_flags%mp_physics.eq.28)  then
         CALL wrf_debug ( 200 , ' call bulk_dust_emis' )
         CALL bulk_dust_emis (grid%itimestep,dtm,config_flags%num_soil_layers  &
               ,grid%u_phy,grid%v_phy,grid%rho,grid%alt                        &
               ,grid%u10,grid%v10,p8w,dz8w,grid%smois,grid%erod                &
               ,grid%ivgtyp,grid%isltyp,grid%vegfra,grid%albbck,grid%xland     &
               ,grid%dx, g, grid%qnifa2d, ids,ide, jds,jde, kds,kde            &
               ,ims,ime, jms,jme, kms,kme                                      &
               ,its,ite, jts,jte, k_start,k_end )
       ENDIF

BENCH_END(moist_physics_prep_tim)
     END DO scalar_tile_loop_1a
     !$OMP END PARALLEL DO

     CALL wrf_debug ( 200 , ' call microphysics_driver' )

     grid%sr = 0.
     specified_bdy = config_flags%specified .OR. config_flags%nested
     channel_bdy = config_flags%specified .AND. config_flags%periodic_x

BENCH_START(micro_driver_tim)

!
! WRFU_AlarmIsRinging always returned false, so using an alternate  method to find out if it is time 
! to dump history/restart files so microphysics can be told to calculate things like radar reflectivity.
!
!     diagflag = .false.
!     CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM ),prevringtime=temp_time,RingInterval=intervaltime)
!     CALL WRFU_ALARMGET(grid%alarms( RESTART_ALARM ),prevringtime=restart_time,RingInterval=restartinterval)
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
#      include "HALO_EM_SBM.inc"
#endif


     CALL microphysics_driver(                                            &
      &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy   &
      &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy              &
      &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr  &
      &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy     &
      &        ,RHO=grid%rho       ,SPEC_ZONE=grid%spec_zone              &
      &        ,SR=grid%sr              ,TH=th_phy                        &
      &        ,refl_10cm=grid%refl_10cm                                  & ! hm, 9/22/09 for refl
      &        ,vmi3d=grid%vmi3d                                          & ! for P3
      &        ,di3d=grid%di3d                                            & ! for P3
      &        ,rhopo3d=grid%rhopo3d                                      & ! for P3
      &        ,vmi3d_2=grid%vmi3d_2                                      & ! for P3
      &        ,di3d_2=grid%di3d_2                                        & ! for P3
      &        ,rhopo3d_2=grid%rhopo3d_2                                  & ! for P3
      &        ,WARM_RAIN=grid%warm_rain                                  &
      &        ,T8W=t8w                                                   &
      &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
      &        ,NSOURCE=grid%qndropsource                                 &
#if (WRF_CHEM == 1)
      &        ,QLSINK=grid%qlsink,CLDFRA_OLD=grid%cldfra_old             &
      &        ,PRECR=grid%precr, PRECI=grid%preci, PRECS=grid%precs, PRECG=grid%precg &
      &        ,CHEM_OPT=config_flags%chem_opt, PROGN=config_flags%progn  &
!======================
      ! Variables required for CAMMGMP Scheme when run with WRF_CHEM
      &        ,CHEM=chem                                                 &
      &        ,QME3D=grid%qme3d,PRAIN3D=grid%prain3d                     &
      &        ,NEVAPR3D=grid%nevapr3d                                    &
      &        ,RATE1ORD_CW2PR_ST3D=grid%rate1ord_cw2pr_st3d              &
      &        ,DGNUM4D=grid%dgnum4d,DGNUMWET4D=grid%dgnumwet4d           &
!======================
#endif
      &        ,XLAND=grid%xland,SNOWH=grid%SNOW                           &  !PMA
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
!======================
      ! Variables required for CAMMGMP Scheme
      &        ,DLF=grid%dlf,DLF2=grid%dlf2,T_PHY=grid%t_phy,P_HYD=grid%p_hyd  &
      &        ,P8W_HYD=grid%p_hyd_w,TKE_PBL=grid%tke_pbl                 &
      &        ,Z_AT_W=grid%z_at_w,QFX=grid%qfx,RLIQ=grid%rliq            &
      &        ,TURBTYPE3D=grid%turbtype3d,SMAW3D=grid%smaw3d             &
      &        ,WSEDL3D=grid%wsedl3d,CLDFRA_OLD_MP=grid%cldfra_old_mp     &
      &        ,CLDFRA_MP=grid%cldfra_mp,CLDFRA_MP_ALL=grid%cldfra_mp_ALL &
      &        ,LRADIUS=grid%LRADIUS, IRADIUS=grid%IRADIUS                & !BSINGH(01/20/2014): Added for RRTMG<->CAMMGMP
      &        ,CLDFRAI=grid%cldfrai                                      &
      &        ,CLDFRAL=grid%cldfral,CLDFRA_CONV=grid%CLDFRA_CONV         &
      &        ,ALT=grid%alt                                              &
      &        ,ACCUM_MODE=config_flags%accum_mode                        &
      &        ,AITKEN_MODE=config_flags%aitken_mode                      &
      &        ,COARSE_MODE=config_flags%coarse_mode                      &
      &        ,ICWMRSH3D=grid%icwmrsh,ICWMRDP3D=grid%icwmrdp3d           &
      &        ,SHFRC3D=grid%shfrc3d,CMFMC3D=grid%cmfmc                   &
      &        ,CMFMC2_3D=grid%cmfmc2,CONFIG_FLAGS=config_flags           &
      &        ,FNM=grid%fnm,FNP=grid%fnp,RH_OLD_MP=grid%rh_old_mp        &
      &        ,LCD_OLD_MP=grid%lcd_old_mp                                &
!======================
                 ! Optional
      &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv                 &
      &        , SNOWNC=grid%snownc, SNOWNCV=grid%snowncv                 &
      &        , GRAUPELNC=grid%graupelnc, GRAUPELNCV=grid%graupelncv     & ! for milbrandt2mom
      &        , HAILNC=grid%hailnc, HAILNCV=grid%hailncv                 &
      &        , W=grid%w_2, Z=grid%z, HT=grid%ht                         &
      &        , MP_RESTART_STATE=grid%mp_restart_state                   &
      &        , TBPVS_STATE=grid%tbpvs_state                             & ! etampnew
      &        , TBPVS0_STATE=grid%tbpvs0_state                           & ! etampnew
      &        , QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV               &
      &        , QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC               &
      &        , QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR               &
      &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI               &
      &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS               &
      &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG               &
      &        , QH_CURR=moist(ims,kms,jms,P_QH), F_QH=F_QH               & ! for milbrandt2mom
      &        , QIC_CURR=moist(ims,kms,jms,P_QIC), F_QIC=F_QIC               &
      &        , QIP_CURR=moist(ims,kms,jms,P_QIP), F_QIP=F_QIP               &
      &        , QID_CURR=moist(ims,kms,jms,P_QID), F_QID=F_QID               &
      &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
#if (WRF_CHEM == 1)
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
      &        , QNWFA_CURR=scalar(ims,kms,jms,P_QNWFA), F_QNWFA=F_QNWFA  & ! for Thompson water-friendly aerosol
      &        , QNIFA_CURR=scalar(ims,kms,jms,P_QNIFA), F_QNIFA=F_QNIFA  & ! for Thompson ice-friendly aerosol
      &        , QNH_CURR=scalar(ims,kms,jms,P_QNH), F_QNH=F_QNH          & ! for milbrandt2mom and nssl_2mom
      &        , QNIC_CURR=scalar(ims,kms,jms,P_QNIC), F_QNIC=F_QNIC          &
      &        , QNIP_CURR=scalar(ims,kms,jms,P_QNIP), F_QNIP=F_QNIP          &
      &        , QNID_CURR=scalar(ims,kms,jms,P_QNID), F_QNID=F_QNID          &
      &        , QIR_CURR=scalar(ims,kms,jms,P_QIR), F_QIR=F_QIR          & ! for P3
      &        , QIB_CURR=scalar(ims,kms,jms,P_QIB), F_QIB=F_QIB          & ! for P3
      &        , QI2_CURR=moist(ims,kms,jms,P_QI2), F_QI2=F_QI2           & ! for P3
      &        , QNI2_CURR=scalar(ims,kms,jms,P_QNI2), F_QNI2=F_QNI2      & ! for P3
      &        , QIR2_CURR=scalar(ims,kms,jms,P_QIR2), F_QIR2=F_QIR2      & ! for P3
      &        , QIB2_CURR=scalar(ims,kms,jms,P_QIB2), F_QIB2=F_QIB2      & ! for P3
!       &        , QZR_CURR=scalar(ims,kms,jms,P_QZR), F_QZR=F_QZR          & ! for milbrandt3mom
!       &        , QZI_CURR=scalar(ims,kms,jms,P_QZI), F_QZI=F_QZI          & ! "
!       &        , QZS_CURR=scalar(ims,kms,jms,P_QZS), F_QZS=F_QZS          & ! "
!       &        , QZG_CURR=scalar(ims,kms,jms,P_QZG), F_QZG=F_QZG          & ! "
!       &        , QZH_CURR=scalar(ims,kms,jms,P_QZH), F_QZH=F_QZH          & ! "
      &        , QVOLG_CURR=scalar(ims,kms,jms,P_QVOLG), F_QVOLG=F_QVOLG    & ! for nssl_2mom
      &        , QVOLH_CURR=scalar(ims,kms,jms,P_QVOLH), F_QVOLH=F_QVOLH    & ! for nssl_2mom
      &        , cu_used=config_flags%cu_used                             &
      &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &
      &        , qicuten=grid%rqicuten, qccuten=grid%rqccuten             &
      &        , HAIL=config_flags%gsfcgce_hail                           & ! for gsfcgce
      &        , ICE2=config_flags%gsfcgce_2ice                           & ! for gsfcgce
!     &        , ccntype=config_flags%milbrandt_ccntype                   & ! for milbrandt (2mom)
! YLIN
! RI_CURR INPUT
      &        , RI_CURR=grid%rimi                                          &
      &        , re_cloud=grid%re_cloud, re_ice=grid%re_ice, re_snow=grid%re_snow & ! G. Thompson
      &        , has_reqc=grid%has_reqc, has_reqi=grid%has_reqi, has_reqs=grid%has_reqs & ! G. Thompson
      &        , qnwfa2d=grid%qnwfa2d, qnifa2d=grid%qnifa2d                             & ! G. Thompson
      &        , diagflag=diag_flag, do_radar_ref=config_flags%do_radar_ref &
      &        ,u=grid%u_phy,v=grid%v_phy &
      &        ,scalar=scalar,num_scalar=num_scalar                             &
      &        ,TH_OLD=grid%th_old                                        &
      &        ,QV_OLD=grid%qv_old                                        &
      &        ,xlat=grid%xlat,xlong=grid%xlong,IVGTYP=grid%ivgtyp  &
      &        , EFFR_CURR=scalar(ims,kms,jms,P_EFFR), F_EFFR=F_EFFR          & ! for SBM
      &        , ICE_EFFR_CURR=scalar(ims,kms,jms,P_ICE_EFFR), F_ICE_EFFR=F_ICE_EFFR          & ! for SBM
      &        , TOT_EFFR_CURR=scalar(ims,kms,jms,P_TOT_EFFR), F_TOT_EFFR=F_TOT_EFFR          & ! for SBM
      &        , QIC_EFFR_CURR=scalar(ims,kms,jms,P_QIC_EFFR), F_QIC_EFFR=F_QIC_EFFR          & ! for SBM
      &        , QIP_EFFR_CURR=scalar(ims,kms,jms,P_QIP_EFFR), F_QIP_EFFR=F_QIP_EFFR          & ! for SBM
      &        , QID_EFFR_CURR=scalar(ims,kms,jms,P_QID_EFFR), F_QID_EFFR=F_QID_EFFR          & ! for SBM
      &        ,kext_ql=grid%kext_ql                                       &
      &        ,kext_qs=grid%kext_qs                                       &
      &        ,kext_qg=grid%kext_qg                                       &
      &        ,kext_qh=grid%kext_qh                                       &
      &        ,kext_qa=grid%kext_qa                                       &
      &        ,kext_qic=grid%kext_qic                                       &
      &        ,kext_qip=grid%kext_qip                                       &
      &        ,kext_qid=grid%kext_qid                                       &
      &        ,kext_ft_qic=grid%kext_ft_qic                                       &
      &        ,kext_ft_qip=grid%kext_ft_qip                                       &
      &        ,kext_ft_qid=grid%kext_ft_qid                                       &
      &        ,kext_ft_qs=grid%kext_ft_qs                                       &
      &        ,kext_ft_qg=grid%kext_ft_qg         &
      &        ,height=grid%height                                         &
      &        ,tempc=grid%tempc                                         &
      &        ,ccn_conc=grid%ccn_conc                                   & ! RAS
      &        ,aerocu=aerocu                                            &
      &        ,aercu_fct=config_flags%aercu_fct                         &
      &        ,aercu_opt=config_flags%aercu_opt                         &
      &        ,no_src_types_cu=grid%no_src_types_cu                     &
      &        ,PBL=grid%bl_pbl_physics,EFCG=grid%EFCG,EFIG=grid%EFIG,EFSG=grid%EFSG &
      &        ,WACT=grid%WACT,CCN1_GS=grid%CCN1_GS,CCN2_GS=grid%CCN2_GS,CCN3_GS=grid%CCN3_GS  &
      &        ,CCN4_GS=grid%CCN4_GS,CCN5_GS=grid%CCN5_GS,CCN6_GS=grid%CCN6_GS &
      &        ,CCN7_GS=grid%CCN7_GS,NR_CU=grid%NR_CU,QR_CU=grid%QR_CU,NS_CU=grid%NS_CU &
      &        ,QS_CU=grid%QS_CU,CU_UAF=grid%CU_UAF,mskf_refl_10cm=grid%mskf_refl_10cm)
                                                                          
BENCH_END(micro_driver_tim)

#if 0
BENCH_START(microswap_2)
! for load balancing; communication to redistribute the points
      IF ( config_flags%mp_physics .EQ. ETAMPNEW .OR. &
     &     config_flags%mp_physics .EQ. FER_MP_HIRES) THEN
#include "SWAP_ETAMP_NEW.inc"
     ELSE IF ( config_flags%mp_physics .EQ. WSM3SCHEME ) THEN
#include "SWAP_WSM3.inc"
     ENDIF
BENCH_END(microswap_2)
#endif

     CALL wrf_debug ( 200 , ' call moist_physics_finish' )
BENCH_START(moist_phys_end_tim)

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)

       CALL microphysics_zero_outb (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outb (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outb (                                    &
                      chem , num_chem , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )
       CALL microphysics_zero_outb (                                    &
                      tracer , num_tracer , config_flags ,              &
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

       CALL microphysics_zero_outa (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      chem , num_chem , config_flags ,                  &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL microphysics_zero_outa (                                    &
                      tracer , num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

       CALL moist_physics_finish_em( grid%t_2, grid%t_1, t0, grid%muts, th_phy,  &
                                     grid%h_diabatic, dtm,                       &
                                     moist(ims,kms,jms,P_QV),grid%qv_diabatic,   &
                                     moist(ims,kms,jms,P_QC),grid%qc_diabatic,   &
                                     grid%th_phy_m_t0,                           &
                                     config_flags,                               &
#if ( WRF_DFI_RADAR == 1 )
                                     grid%dfi_tten_rad,grid%dfi_stage,           &
#endif
                                     ids, ide, jds, jde, kds, kde,               &
                                     ims, ime, jms, jme, kms, kme,               &
                                     its, ite, jts, jte,                         &
                                     k_start    , k_end                          )

     END DO
     !$OMP END PARALLEL DO

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
#       include "HALO_EM_THETAM.inc"
#       include "PERIOD_EM_THETAM.inc"
#endif
        its=ips ; ite = ipe
        jts=jps ; jte = jpe
        CALL set_physical_bc3d( grid%h_diabatic, 'p', config_flags,      &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                its, ite, jts, jte,               &
                                k_start    , k_end                )
   ENDIF  ! microphysics test

!-----------------------------------------------------------
!  filter for moist variables post-microphysics and end of timestep
!-----------------------------------------------------------

   IF (config_flags%polar) THEN
     IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
       CALL wrf_debug ( 200 , ' call filter moist' )
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         IF ( config_flags%coupled_filtering ) THEN
           CALL couple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)        &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
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
           CALL uncouple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)      &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
         END IF
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

     CALL calc_p_rho_phi( moist, num_3d_m, config_flags%hypsometric_opt,                    &
                          grid%al, grid%alb, grid%mu_2, grid%muts,                          &
                          grid%c1h, grid%c2h, grid%c3h, grid%c4h, grid%c3f, grid%c4f,       &
                          grid%ph_2, grid%phb, grid%p, grid%pb, grid%t_2,                   &
                          p0, t0, grid%p_top, grid%znu, grid%znw, grid%dnw, grid%rdnw,      &
                          grid%rdn, config_flags%non_hydrostatic,config_flags%use_theta_m,  &
                          ids, ide, jds, jde, kds, kde,                                     &
                          ims, ime, jms, jme, kms, kme,                                     &
                          its, ite, jts, jte,                                               &
                          k_start    , k_end                                                )

   END DO scalar_tile_loop_1ba
   !$OMP END PARALLEL DO
BENCH_END(moist_phys_end_tim)

   IF (.not. config_flags%non_hydrostatic) THEN
#ifdef DM_PARALLEL
#    include "HALO_EM_HYDRO_UV.inc"
#    include "PERIOD_EM_HYDRO_UV.inc"
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
       CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, &
                       grid%c1f, grid%c2f, dt_rk,              &
                       grid%u_2, grid%v_2, grid%ht,            &
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
             CALL couple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)               &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
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
             CALL uncouple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)       &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
         END IF
       ENDDO chem_filter_loop
     ENDIF
     IF ( num_tracer >= PARAM_FIRST_SCALAR ) then
       tracer_filter_loop: DO im = PARAM_FIRST_SCALAR, num_tracer
         IF ( config_flags%coupled_filtering ) THEN
           CALL couple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)         &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )
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
                  ,flag_tracer        = im                                     &
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
           CALL uncouple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)       &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,C1=grid%c1h , C2=grid%c2h                                   &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
         END IF
       ENDDO tracer_filter_loop
     ENDIF

     IF ( num_3d_s >= PARAM_FIRST_SCALAR ) then
       scalar_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_s
         IF ( config_flags%coupled_filtering ) THEN
           CALL couple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)       &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,C1=grid%c1h , C2=grid%c2h                                   &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
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
           CALL uncouple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)     &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,C1=grid%c1h , C2=grid%c2h                                   &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )
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
#    include "HALO_EM_D3_3.inc"
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D3_5.inc"
   ELSE 
      WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
      CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#  include "PERIOD_BDY_EM_D3.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
#  include "PERIOD_BDY_EM_TRACER.inc"
#  include "PERIOD_BDY_EM_SCALAR.inc"
#endif

!  now set physical b.c on a patch

BENCH_START(bc_2d_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             grid%u_1, grid%u_2, grid%v_1, grid%v_2, grid%w_1, grid%w_2,           &
                             grid%t_1, grid%t_2, grid%ph_1, grid%ph_2, grid%mu_1, grid%mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL set_physical_bc3d( grid%tke_1, 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )

     CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

       CALL set_physical_bc3d( chem(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

     tracer_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_tracer

       CALL set_physical_bc3d( tracer(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO tracer_species_bdy_loop_2

     scalar_species_bdy_loop_2 : DO is = PARAM_FIRST_SCALAR , num_3d_s

       CALL set_physical_bc3d( scalar(ims,kms,jms,is) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO scalar_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO
BENCH_END(bc_2d_tim)

!  this code forces boundary values to specified values to avoid drift

   IF( config_flags%specified .or. config_flags%nested ) THEN 
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   tile_bc_loop_3: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call spec_bdy_final' )

     CALL spec_bdy_final   ( grid%u_2, grid%muus, grid%c1h, grid%c2h, grid%msfuy,     &
                                grid%u_bxs, grid%u_bxe, grid%u_bys, grid%u_bye,  &
                                grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye, &
                                'u', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )

     CALL spec_bdy_final   ( grid%v_2, grid%muvs, grid%c1h, grid%c2h, grid%msfvx,     &
                                grid%v_bxs, grid%v_bxe, grid%v_bys, grid%v_bye,  &
                                grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye, &
                                'v', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )

     IF( config_flags%nested) THEN
       CALL spec_bdy_final   ( grid%w_2, grid%muts, grid%c1f, grid%c2f, grid%msfty, &
                                grid%w_bxs, grid%w_bxe, grid%w_bys, grid%w_bye,  &
                                grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye, &
                                'w', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )
     ENDIF

     CALL spec_bdy_final   ( grid%t_2, grid%muts, grid%c1h, grid%c2h, grid%msfty,&
                                grid%t_bxs, grid%t_bxe, grid%t_bys, grid%t_bye,  &
                                grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye, &
                                't', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )

     CALL spec_bdy_final   ( grid%ph_2, grid%muts, grid%c1f, grid%c2f, grid%msfty,   &
                                grid%ph_bxs, grid%ph_bxe, grid%ph_bys, grid%ph_bye,  &
                                grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                                'h', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )

     IF( config_flags%spec_bdy_final_mu .EQ. 1 ) THEN
     CALL spec_bdy_final   ( grid%mu_2, grid%muts, grid%c1h, grid%c2h, grid%msfty,   &
                                grid%mu_bxs, grid%mu_bxe, grid%mu_bys, grid%mu_bye,  &
                                grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                                'm', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, 1,  1,    & ! domain dims
                                ims,ime, jms,jme, 1,  1,    & ! memory dims
                                ips,ipe, jps,jpe, 1,  1,    & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                1  , 1                    )
     ENDIF

     moisture_loop_bdy_3 : DO im = PARAM_FIRST_SCALAR , num_3d_m

     IF ( im .EQ. P_QV .OR. config_flags%nested .OR. &
             ( config_flags%specified .AND. config_flags%have_bcs_moist ) ) THEN
        CALL spec_bdy_final   ( moist(ims,kms,jms,im), grid%muts,                &
                                grid%c1h, grid%c2h, grid%msfty,                  &
                                moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
                                't', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )
     ENDIF

     END DO moisture_loop_bdy_3

#if (WRF_CHEM == 1)
     IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN
         chem_species_bdy_loop_3 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

     IF( ( config_flags%nested ) ) THEN
        CALL spec_bdy_final   ( chem(ims,kms,jms,ic), grid%muts,               &
                                grid%c1h, grid%c2h, grid%msfty,                &
                                chem_bxs(jms,kms,1,ic),chem_bxe(jms,kms,1,ic), &
                                chem_bys(ims,kms,1,ic),chem_bye(ims,kms,1,ic), &
                                chem_btxs(jms,kms,1,ic),chem_btxe(jms,kms,1,ic), &
                                chem_btys(ims,kms,1,ic),chem_btye(ims,kms,1,ic), &
                                't', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )
     ENDIF

         END DO chem_species_bdy_loop_3
     ENDIF
#endif

     tracer_species_bdy_loop_3 : DO im = PARAM_FIRST_SCALAR , num_tracer

     IF( ( config_flags%nested ) ) THEN
        CALL spec_bdy_final   ( tracer(ims,kms,jms,im), grid%muts,                 &
                                grid%c1h, grid%c2h, grid%msfty,                    &
                                tracer_bxs(jms,kms,1,im),tracer_bxe(jms,kms,1,im), &
                                tracer_bys(ims,kms,1,im),tracer_bye(ims,kms,1,im), &
                                tracer_btxs(jms,kms,1,im),tracer_btxe(jms,kms,1,im), &
                                tracer_btys(ims,kms,1,im),tracer_btye(ims,kms,1,im), &
                                't', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )
     ENDIF

     END DO tracer_species_bdy_loop_3

     scalar_species_bdy_loop_3 : DO is = PARAM_FIRST_SCALAR , num_3d_s

     IF( ( config_flags%nested ) ) THEN
        CALL spec_bdy_final   ( scalar(ims,kms,jms,is), grid%muts,                 &
                                grid%c1h, grid%c2h, grid%msfty,                    &
                                scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is), &
                                scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is), &
                                scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is), &
                                scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is), &
                                't', config_flags,                               &
                                config_flags%spec_bdy_width, grid%spec_zone,     &
                                grid%dtbc,                                       &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),       &
                                grid%j_start(ij), grid%j_end(ij),       &
                                k_start    , k_end                     )
     ENDIF

     END DO scalar_species_bdy_loop_3

   END DO tile_bc_loop_3
   !$OMP END PARALLEL DO

   ENDIF

! reset surface w for consistency

#ifdef DM_PARALLEL
#  include "HALO_EM_C.inc"
#  include "PERIOD_BDY_EM_E.inc"
#endif

   CALL wrf_debug ( 10 , ' call set_w_surface' )
   fill_w_flag = .false.

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      CALL set_w_surface( config_flags, grid%znw, fill_w_flag,              &
                           grid%w_2, grid%ht,  grid%u_2, grid%v_2,          &
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
#    include "HALO_EM_E_3.inc"
   ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_E_5.inc"
   ELSE
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
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
#      include "HALO_EM_MOIST_E_3.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_MOIST_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
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
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
   IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_TRACER' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_TRACER_E_3.inc"
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_TRACER_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
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
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   ENDIF
#endif

!  Max values of CFL for adaptive time step scheme

   DEALLOCATE(max_vert_cfl_tmp)
   DEALLOCATE(max_horiz_cfl_tmp)

   CALL wrf_debug ( 200 , ' call end of solve_em' )

!  Are we about to read SST input from the wrflowinput file?  That data is saved
!  for use in fractional merging of external/coupled SST and input SST. 
   IF ( coupler_on )   grid%just_read_auxinput4 = Is_alarm_tstep(grid%domain_clock, grid%alarms(AUXINPUT4_ALARM))

!  Are we about to read the lateral boundary file?  This is a domain one action only.
   IF ( grid%id .EQ. 1 ) grid%just_read_boundary = Is_alarm_tstep(grid%domain_clock, grid%alarms(BOUNDARY_ALARM))

! Finish timers if compiled with -DBENCH.
#include "bench_solve_em_end.h"

   RETURN

END SUBROUTINE solve_em

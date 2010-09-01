!WRF+/AD:MEDIATION_LAYER:SOLVER FOR AD
!Created by Ning Pan, 2010-08 

SUBROUTINE solve_em_ad ( grid , config_flags  &
! Arguments generated from Registry
#include "dummy_new_args.inc"
!
                    )
! Driver layer modules
   USE module_state_description
   USE module_domain, ONLY : &
                  domain, get_ijk_from_grid, get_ijk_from_subgrid                          &
                 ,domain_get_current_time, domain_get_start_time                           &
                 ,domain_get_sim_start_time, domain_clock_get
   USE module_domain_type, ONLY : history_alarm
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
                 ,halo_em_tracer_old_e_7_sub,period_bdy_em_a_sub                           &
                 ,period_bdy_em_b3_sub,period_bdy_em_b_sub,period_bdy_em_chem2_sub         &
                 ,period_bdy_em_chem_old_sub,period_bdy_em_chem_sub,period_bdy_em_d3_sub   &
                 ,period_bdy_em_d_sub,period_bdy_em_e_sub,period_bdy_em_moist2_sub         &
                 ,period_bdy_em_moist_old_sub,period_bdy_em_moist_sub                      &
                 ,period_bdy_em_scalar2_sub,period_bdy_em_scalar_old_sub                   &
                 ,period_bdy_em_scalar_sub,period_bdy_em_tke_old_sub                       &
                 ,period_bdy_em_tracer2_sub,period_bdy_em_tracer_old_sub                   &
                 ,period_bdy_em_tracer_sub,period_em_da_sub,period_em_hydro_uv_sub
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
   USE module_fddaobs_driver
   USE module_diagnostics
#ifdef WRF_CHEM
   USE module_input_chem_data
   USE module_input_tracer
   USE module_chem_utilities
#endif
   USE module_first_rk_step_part1
   USE module_first_rk_step_part2
   USE module_llxy, ONLY : proj_cassini
   USE module_avgflx_em, ONLY : zero_avgflx, upd_avgflx

   USE a_module_small_step_em
   USE a_module_em
   USE a_module_big_step_utilities_em
   USE a_module_bc
   USE a_module_bc_em
   USE a_module_first_rk_step_part2
   USE module_linked_list2
 

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

! storage for tendencies and decoupled state (generated from Registry)

#include <i1_decl.inc>
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
   REAL    :: curr_secs
   INTEGER :: num_sound_steps
   INTEGER :: idex, jdex
   REAL    :: max_msft
   REAL    :: spacing

   INTEGER :: ii, jj !kk is above after l,kte
   REAL    :: dclat
   INTEGER :: debug_level

! urban related variables
   INTEGER :: NUM_ROOF_LAYERS, NUM_WALL_LAYERS, NUM_ROAD_LAYERS   ! urban

   TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval
   REAL                                       :: real_time
   LOGICAL                                    :: adapt_step_flag
   LOGICAL                                    :: fill_w_flag

! variables for flux-averaging code 20091223
   CHARACTER*256                              :: message, message2
   REAL                                       :: old_dt
   TYPE(WRFU_Time)                            :: temp_time, CurrTime
   INTEGER, PARAMETER                         :: precision = 100
   INTEGER                                    :: num, den
   TYPE(WRFU_TimeInterval)                    :: dtInterval

! Define benchmarking timers if -DBENCH is compiled
#include <bench_solve_em_def.h>

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
#include <bench_solve_em_init.h>

!  Initialize linkedlist
   CALL linkedlist_initialize
 
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
   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

!  Max values of CFL for adaptive time step scheme

   ALLOCATE (max_vert_cfl_tmp(grid%num_tiles))
   ALLOCATE (max_horiz_cfl_tmp(grid%num_tiles))

  !
  ! Calculate current time in seconds since beginning of model run.
  !   Unfortunately, ESMF does not seem to have a way to return
  !   floating point seconds based on a TimeInterval.  So, we will
  !   calculate it here--but, this is not clean!!
  !
   tmpTimeInterval = domain_get_current_time ( grid ) - domain_get_sim_start_time ( grid )
   curr_secs = real_time(tmpTimeInterval)

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

   grid%itimestep = grid%itimestep - 1
   return

   IF (config_flags%polar) dclat = 90./REAL(jde-jds) !(0.5 * 180/ny)

#ifdef WRF_CHEM

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
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
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

!==[P0]===============================================================
! setting bdy tendencies to zero for DFI if constant_bc = true

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

!      IF( config_flags%specified .AND. grid%dfi_opt .NE. DFI_NODFI   &
!          .AND. config_flags%constant_bc .AND. (grid%dfi_stage .EQ. DFI_BCK .OR. grid%dfi_stage .EQ. DFI_FWD) ) THEN
       IF( config_flags%specified .AND. config_flags%constant_bc ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_bdytend{
if (config_flags % sw_call_zero_bdytend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call zero_bdytend ' )

       CALL zero_bdytend (grid%u_btxs,grid%u_btxe,grid%u_btys,grid%u_btye,     &
                          grid%v_btxs,grid%v_btxe,grid%v_btys,grid%v_btye,     &
                          grid%ph_btxs,grid%ph_btxe,grid%ph_btys,grid%ph_btye, &
                          grid%t_btxs,grid%t_btxe,grid%t_btys,grid%t_btye,     &
                          grid%w_btxs,grid%w_btxe,grid%w_btys,grid%w_btye,     &
                          grid%mu_btxs,grid%mu_btxe,grid%mu_btys,grid%mu_btye, &
                          moist_btxs,moist_btxe,                               &
                          moist_btys,moist_btye,                               &
                          grid%spec_bdy_width,num_3d_m,                &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          ips,ipe, jps,jpe, kps,kpe,                   &
                          grid%i_start(ij), grid%i_end(ij),            &
                          grid%j_start(ij), grid%j_end(ij),            &
                          k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call zero_bdytend ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_bdytend }


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

!==[P1]===============================================================
   Runge_Kutta_loop:  DO rk_step = 1, rk_order

!====[P1.1]===========================================================
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

!====[P1.2]===========================================================
!  Ensure that polar meridional velocity is zero
     IF (config_flags%polar) THEN
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_pole{
if (config_flags % sw_call_zero_pole) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call zero_pole ' )

         CALL zero_pole ( grid%v_1,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call zero_pole ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_pole }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_pole{
if (config_flags % sw_call_zero_pole) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call zero_pole ' )

         CALL zero_pole ( grid%v_2,                      &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call zero_pole ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_pole }

       END DO
       !$OMP END PARALLEL DO
     END IF
!
!  Time level t is in the *_2 variable in the first part
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
!====[P1.3]===========================================================
     CALL wrf_debug ( 200 , ' call rk_step_prep ' )

BENCH_START(step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_step_prep{
if (config_flags % sw_call_rk_step_prep) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_step_prep ' )

       CALL push4backup (grid%mu_2, "mu")
       CALL push4backup (grid%u_2,grid%v_2,grid%w_2, "u,v,w")
       CALL push4backup (moist, "moist")
       CALL rk_step_prep  ( config_flags, rk_step,            &
                            grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2, grid%mu_2,   &
                            moist,                            &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_step_prep ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_step_prep }


     END DO
     !$OMP END PARALLEL DO
BENCH_END(step_prep_tim)

!====[P1.4]===========================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_1{
if (config_flags % sw_call_rk_phys_bc_dry_1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_phys_bc_dry_1 ' )

       CALL rk_phys_bc_dry_1( config_flags, grid%ru, grid%rv, grid%rw, grid%ww,      &
                              grid%muu, grid%muv, grid%mut, grid%php, grid%alt, grid%p,        &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_phys_bc_dry_1 ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_1 }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start, k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


       IF (config_flags%polar) THEN

!-------------------------------------------------------
! lat-lon grid pole-point (v) specification (extrapolate v, rv to the pole)
!-------------------------------------------------------


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pole_point_bc{
if (config_flags % sw_call_pole_point_bc) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pole_point_bc ' )

         CALL pole_point_bc ( grid%v_1,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pole_point_bc ' )
end if
! CHECK NECESSARY SUBROUTINE: pole_point_bc }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pole_point_bc{
if (config_flags % sw_call_pole_point_bc) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pole_point_bc ' )

         CALL pole_point_bc ( grid%v_2,                      &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pole_point_bc ' )
end if
! CHECK NECESSARY SUBROUTINE: pole_point_bc }


!-------------------------------------------------------
! end lat-lon grid pole-point (v) specification
!-------------------------------------------------------

       ENDIF
     END DO
     !$OMP END PARALLEL DO
BENCH_END(set_phys_bc_tim)

!====[P1.5]===========================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: first_rk_step_part1{
if (config_flags % sw_call_first_rk_step_part1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call first_rk_step_part1 ' )

       CALL first_rk_step_part1 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0      &
                             , br , chklowq                     &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , t_phy           &
                             , u_phy , v_phy                    &
                             , dz8w , p8w , t8w , rho_phy , rho &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx, imex, jmsx, jmex, kmsx, kmex    &
                             , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
                             , imsy, imey, jmsy, jmey, kmsy, kmey    &
                             , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                            )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call first_rk_step_part1 ' )

! NPAN 05/09/10 + init_zero_tendency (module_em), which is called by first_rk_step_part1
BENCH_START(init_zero_tend_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

        DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call init_zero_tendency' )
         CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                   ph_tendf, t_tendf, tke_tend,      &
                                   mu_tendf,                         &
                                   moist_tend,chem_tend,scalar_tend, &
                                   tracer_tend,num_tracer,           &
                                   num_moist,num_chem,num_scalar,    &
                                   rk_step,                          &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                   )

       END DO
       !$OMP END PARALLEL DO
BENCH_END(init_zero_tend_tim)

! NPAN 08/30/10 + phy_prep (module_big_step_utilities_em), which is called by first_rk_step_part1
#ifdef DM_PARALLEL
#     include "HALO_EM_PHYS_A.inc"
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call phy_prep' )
        CALL phy_prep ( config_flags,                           &
                        grid%mut, grid%muu, grid%muv, grid%u_2, &
                        grid%v_2, grid%p, grid%pb, grid%alt,              &
                        grid%ph_2, grid%phb, grid%t_2, grid%tsk, moist, num_moist,   &
                        rho,th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                        p8w, t_phy, t8w, grid%z, grid%z_at_w, dz8w, &
                        grid%p_hyd, grid%p_hyd_w,                   &
                        grid%fnm, grid%fnp, grid%znw, grid%p_top,   &
                        grid%rthraten,                          &
                        grid%rthblten, grid%rublten, grid%rvblten,             &
                        grid%rqvblten, grid%rqcblten, grid%rqiblten,           &
                        grid%rthcuten, grid%rqvcuten, grid%rqccuten,           &
                        grid%rqrcuten, grid%rqicuten, grid%rqscuten,           &
                        grid%rthften,  grid%rqvften,                      &
                        grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN,       &
                        grid%RPHNDGDTEN,grid%RQVNDGDTEN, grid%RMUNDGDTEN,                 &
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        grid%i_start(ij), grid%i_end(ij),       &
                        grid%j_start(ij), grid%j_end(ij),       &
                        k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO
 
end if
! CHECK NECESSARY SUBROUTINE: first_rk_step_part1 }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: first_rk_step_part2{
if (config_flags % sw_call_first_rk_step_part2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call first_rk_step_part2 ' )

       CALL push4backup (grid%mut,grid%hfx,grid%qfx,grid%ustm, "mut,hfx,qfx,ustm") 
       CALL push4backup (grid%u_2,grid%v_2,grid%w_2,grid%t_2,grid%ph_2,grid%tke_2, &
                         "u,v,w,t,ph,tke") 
       CALL push4backup (rho,t_phy,p_phy,th_phy,T8w,p8w, "rho,t_phy,p_phy,th_phy,T8w,p8w") 
       CALL push4backup (moist, "moist") 
!!!!! USE THE STATEMENTS REMARKED WHEN chem and tracer ARE NEEDED. Ning Pan, 2010-08-20
!       CALL push4backup (chem, "chem") 
!       CALL push4backup (tracer, "tracer") 
       CALL push4backup (scalar, "scalar") 
       CALL first_rk_step_part2 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0      &
                             , br , chklowq                     &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , t_phy           &
                             , u_phy , v_phy                    &
                             , dz8w , p8w , t8w , rho_phy , rho &
                             , nba_mij, num_nba_mij             & !JDM
                             , nba_rij, num_nba_rij             & !JDM
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , k_start , k_end                  &
                            )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call first_rk_step_part2 ' )
end if
! CHECK NECESSARY SUBROUTINE: first_rk_step_part2 }


     END IF rk_step_is_one

!====[P1.6]===========================================================
BENCH_START(rk_tend_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_tendency' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_tendency{
if (config_flags % sw_call_rk_tendency) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_tendency ' )

       CALL push4backup (grid%mu_2,grid%muu,grid%muv,grid%mut, "mu,muu,muv,mut") 
       CALL push4backup (grid%ru,grid%rv,grid%rw,grid%ww, "ru,rv,rw,ww") 
       CALL push4backup (grid%u_2,grid%v_2,grid%w_2,grid%t_2,grid%ph_2, "u,v,w,t,ph") 
       CALL push4backup (grid%al,grid%alt,grid%p,grid%php,cqu,cqv,cqw, &
                         "al,alt,p,php,cqu,cqv,cqw") 
       CALL push4backup (grid%xkmh,grid%xkhh, "xkmh,xkhh") 
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
                         ,grid%al, grid%alt, grid%p, grid%pb, grid%php, cqu, cqv, cqw                          &
                         ,grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base                     &
                         ,grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv                                    &
                         ,grid%msfvy, grid%msftx,grid%msfty, grid%xlat, grid%f, grid%e, grid%sina, grid%cosa   &
                         ,grid%fnm, grid%fnp, grid%rdn, grid%rdnw                                              &
                         ,grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, grid%xkmh, grid%xkhh            &
                         ,grid%diff_6th_opt, grid%diff_6th_factor                                              &
                         ,grid%dampcoef,grid%zdamp,config_flags%damp_opt                                       &
                         ,grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m                          &
                         ,config_flags%non_hydrostatic, config_flags%top_lid                                   &
                         ,grid%u_frame, grid%v_frame                                                           &
                         ,ids, ide, jds, jde, kds, kde                                                         &
                         ,ims, ime, jms, jme, kms, kme                                                         &
                         ,grid%i_start(ij), grid%i_end(ij)                                                     &
                         ,grid%j_start(ij), grid%j_end(ij)                                                     &
                         ,k_start, k_end                                                                       &
                         ,max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij)                                         )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_tendency ' )

! NPAN 05/09/10 + zero_tend (module_big_step_utilities_em), which is called by rk_tendency
   CALL zero_tend ( grid%ru_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( grid%rv_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( rw_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( t_tend,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( ph_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( grid%u_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( grid%v_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( w_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( ph_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( grid%t_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    k_start, k_end )

   CALL zero_tend ( mu_tend,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    1, 1 )

   CALL zero_tend ( mu_save,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    grid%i_start(ij), grid%i_end(ij), &
                    grid%j_start(ij), grid%j_end(ij), &
                    1, 1 )

end if
! CHECK NECESSARY SUBROUTINE: rk_tendency }

     END DO
     !$OMP END PARALLEL DO
BENCH_END(rk_tend_tim)

!====[P1.7]===========================================================
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

!====[P1.8]===========================================================
BENCH_START(relax_bdy_dry_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

!======[P1.8.1]=======================================================
       IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_dry{
if (config_flags % sw_call_relax_bdy_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call relax_bdy_dry ' )

         CALL push4backup (grid%mut, "mut") 
         CALL push4backup (grid%ph_2,grid%t_2,grid%w_2, "ph,t,w") 
         CALL relax_bdy_dry ( config_flags,                                &
                              grid%u_save, grid%v_save, ph_save, grid%t_save,  &
                              w_save, mu_tend,                             &
                              grid%ru, grid%rv, grid%ph_2, grid%t_2,       &
                              grid%w_2, grid%mu_2, grid%mut,               &
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
                              config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                              grid%dtbc, grid%fcx, grid%gcx,               &
                              ids,ide, jds,jde, kds,kde,                   &
                              ims,ime, jms,jme, kms,kme,                   &
                              ips,ipe, jps,jpe, kps,kpe,                   &
                              grid%i_start(ij), grid%i_end(ij),            &
                              grid%j_start(ij), grid%j_end(ij),            &
                              k_start, k_end                              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call relax_bdy_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_dry }


       ENDIF


!======[P1.8.2]=======================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_addtend_dry{
if (config_flags % sw_call_rk_addtend_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_addtend_dry ' )

       CALL push4backup (grid%mut, "mut") 
       CALL push4backup (grid%h_diabatic, "h_diabatic") 
       CALL rk_addtend_dry( grid%ru_tend,  grid%rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            grid%u_save, grid%v_save, w_save, ph_save, grid%t_save, &
                            mu_tend, mu_tendf, rk_step,                      &
                            grid%h_diabatic, grid%mut, grid%msftx,           &
                            grid%msfty, grid%msfux,grid%msfuy,               &
                            grid%msfvx, grid%msfvx_inv, grid%msfvy,          &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            grid%i_start(ij), grid%i_end(ij),                &
                            grid%j_start(ij), grid%j_end(ij),                &
                            k_start, k_end                                  )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_addtend_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_addtend_dry }


!======[P1.8.3]=======================================================
       IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_dry{
if (config_flags % sw_call_spec_bdy_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdy_dry ' )

         CALL spec_bdy_dry ( config_flags,                                    &
                             grid%ru_tend, grid%rv_tend, ph_tend, t_tend,     &
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
                             config_flags%spec_bdy_width, grid%spec_zone,         &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),                &
                             grid%j_start(ij), grid%j_end(ij),                &
                             k_start, k_end                                  )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdy_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_dry }


       ENDIF

     END DO
     !$OMP END PARALLEL DO
BENCH_END(relax_bdy_dry_tim)

     ! [recalc_P5_debug_bdy] Only for debugging TL/AD of bdy-related (Added by Ning Pan)
     ! --------------------------------------------------------------------------
     DO ij = 1 , grid%num_tiles

          ! [recalc_P5.1_debug_bdy]
          ! ---------------- 
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_all{
if (config_flags % sw_call_advance_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_all ' )

          CALL save_ph_mu  ( grid%ph_1, grid%ph_2, ph_save, &
                             grid%mu_1, grid%mu_2, mu_save, &
                             rk_step,                       &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start, k_end )
 
          CALL push4backup (grid%muu,grid%muv,grid%mut, "muu,muv,mut") 
          CALL push4backup (grid%ru_tend,grid%rv_tend,rw_tend,t_tend,ph_tend, &
                            "ru_tend,rv_tend,rw_tend,t_tend,ph_tend") 
          CALL advance_all (grid%u_2, grid%ru_tend, grid%v_2, grid%rv_tend, &
                           grid%w_2,       rw_tend, grid%t_2, t_tend, &
                           grid%mu_2,      mu_tend, grid%ph_2, ph_tend, &
                           grid%muu, grid%muv, grid%mut, &
                           grid%msfuy, grid%msfvx, grid%msfty, &
                           dts_rk, &
                           config_flags,grid%spec_zone, &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start , k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_all ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_all }

  ! [recalc_P5.2_debug_bdy]
  ! ----------------
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate_all{
if (config_flags % sw_call_spec_bdyupdate_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate_all ' )

  IF( config_flags%specified .or. config_flags%nested ) THEN
     
     CALL push4backup (grid%u_2, "u") 
     CALL couple_bdy(grid%u_2,       &
                         'u'         , config_flags, &
                          grid%spec_zone,                  &
                          grid%muu, grid%msfuy,  & 
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          grid%i_start(ij), grid%i_end(ij),         &
                          grid%j_start(ij), grid%j_end(ij),         &
                          k_start    , k_end             )
           CALL spec_bdyupdate(grid%u_2, grid%ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                                grid%spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )
     CALL push4backup (grid%muu, "muu") 
     CALL push4backup (grid%u_2, "u") 
     CALL uncouple_bdy(grid%u_2,       &
                         'u'         , config_flags, &
                          grid%spec_zone,                  &
                          grid%muu, grid%msfuy,  & 
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          grid%i_start(ij), grid%i_end(ij),         &
                          grid%j_start(ij), grid%j_end(ij),         &
                          k_start    , k_end             )

     CALL push4backup (grid%v_2, "v") 
     CALL couple_bdy(grid%v_2,       &
                          'v'         , config_flags, &
                          grid%spec_zone,                  &
                          grid%muv, grid%msfvx, & 
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
     CALL push4backup (grid%muv, "muv") 
     CALL push4backup (grid%v_2, "v") 
     CALL uncouple_bdy(grid%v_2,       &
                          'v'         , config_flags, &
                          grid%spec_zone,                  &
                          grid%muv, grid%msfvx, & 
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          grid%i_start(ij), grid%i_end(ij),         &
                          grid%j_start(ij), grid%j_end(ij),         &
                          k_start    , k_end             )

     CALL push4backup (grid%t_2, "t") 
     CALL couple_bdy(grid%t_2,         &
                         't'         , config_flags,      &
                         grid%spec_zone,                  &
                         grid%mut, grid%msfty,  & 
                         ids,ide, jds,jde, kds,kde,       &
                         ims,ime, jms,jme, kms,kme,       &
                         ips,ipe, jps,jpe, kps,kpe,       &
                         grid%i_start(ij), grid%i_end(ij),&
                         grid%j_start(ij), grid%j_end(ij),&
                         k_start    , k_end              )
           CALL spec_bdyupdate(grid%t_2, t_tend, dts_rk,        &
                               't'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end              )
     CALL push4backup (grid%mut, "mut") 
     CALL push4backup (grid%t_2, "t") 
     CALL uncouple_bdy(grid%t_2,         &
                         't'         , config_flags,      &
                         grid%spec_zone,                  &
                         grid%mut, grid%msfty,  & 
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

     IF (config_flags%non_hydrostatic) THEN

       CALL push4backup (grid%mut,mu_tend, "mut,mu_tend") 
       CALL push4backup (grid%ph_2,ph_tend,ph_save, "field_ph,field_ph_tend,ph_save") 
       CALL spec_bdyupdate_ph( ph_save, grid%ph_2, ph_tend,     &
                               mu_tend, grid%mut, dts_rk,      &
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
         CALL push4backup (grid%w_2, "w") 
         CALL couple_bdy ( grid%w_2,        &
                               'w'         , config_flags,      &
                               grid%spec_zone,                  &
                               grid%mut, grid%msfty,  & 
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end               )
               CALL spec_bdyupdate ( grid%w_2, rw_tend, dts_rk,       &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )
         CALL push4backup (grid%mut, "mut") 
         CALL push4backup (grid%w_2, "w") 
         CALL uncouple_bdy ( grid%w_2,        &
                               'w'         , config_flags,      &
                               grid%spec_zone,                  &
                               grid%mut, grid%msfty,  & 
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end               )
       ENDIF
     ENDIF
   ENDIF
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate_all ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate_all }

  ! [recalc_P5.3_debug_bdy]
  ! ----------------
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_all{
if (config_flags % sw_call_advance_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_all ' )

          CALL restore_ph_mu  ( grid%ph_2, ph_save, &
                                grid%mu_2, mu_save, &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_all ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_all }

     END DO

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

!====[P1.9]===========================================================
BENCH_START(small_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first
    ! predictor because we may have changed the small timestep -> grid%dts.

!======[P1.9.1]=======================================================
       CALL wrf_debug ( 200 , ' call small_step_prep ' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: small_step_prep{
if (config_flags % sw_call_small_step_prep) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call small_step_prep ' )

       CALL push4backup (grid%muu,grid%muv,grid%mut, "muu,muv,mut") 
       IF ( rk_step == 1 ) THEN
         CALL push4backup (grid%mu_2, "mu_2")
         CALL push4backup (grid%u_2,grid%v_2,grid%t_2,grid%w_2,grid%p,grid%alt, &
                           "u_2,v_2,t_2,w_2,p,alt") 
       ELSE
         CALL push4backup (grid%mu_1, "mu_1")
         CALL push4backup (grid%u_1,grid%v_1,grid%t_1,grid%w_1, &
                           grid%u_2,grid%v_2,grid%t_2,grid%w_2,grid%p,grid%alt, &
                           "u_1,v_1,t_1,w_1,u_2,v_2,t_2,w_2,p,alt") 
       END IF 
       CALL small_step_prep( grid%u_1,grid%u_2,grid%v_1,grid%v_2,grid%w_1,grid%w_2,   &
                             grid%t_1,grid%t_2,grid%ph_1,grid%ph_2,                   &
                             grid%mub, grid%mu_1, grid%mu_2,                          &
                             grid%muu, muus, grid%muv, muvs,                          &
                             grid%mut, grid%muts, grid%mudf,                          &
                             grid%u_save, grid%v_save, w_save,                        &
                             grid%t_save, ph_save, mu_save,                           &
                             grid%ww, ww1,                                            &
                             grid%dnw, c2a, grid%pb, grid%p, grid%alt,                &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,       &
                             grid%msfvy, grid%msftx,grid%msfty,                       &
                             grid%rdx, grid%rdy, rk_step,                             &
                             ids, ide, jds, jde, kds, kde,                            &
                             ims, ime, jms, jme, kms, kme,                            &
                             grid%i_start(ij), grid%i_end(ij),                        &
                             grid%j_start(ij), grid%j_end(ij),                        &
                             k_start    , k_end                                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call small_step_prep ' )
end if
! CHECK NECESSARY SUBROUTINE: small_step_prep }


!======[P1.9.2]=======================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho{
if (config_flags % sw_call_calc_p_rho) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_p_rho ' )

       CALL push4backup (grid%mu_2,grid%muts, "mu,muts") 
       CALL push4backup (grid%alt,grid%ph_2,grid%t_2,grid%t_save,c2a, "alt,ph,t_2,t_1,c2a") 
       CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                        grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                        grid%mu_2, grid%muts, grid%znu, t0,         &
                        grid%rdnw, grid%dnw, grid%smdiv,            &
                        config_flags%non_hydrostatic, 0,            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        grid%i_start(ij), grid%i_end(ij),           &
                        grid%j_start(ij), grid%j_end(ij),           &
                        k_start    , k_end                          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_p_rho ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho }

!======[P1.9.3]=======================================================
       IF (config_flags%non_hydrostatic) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_coef_w{
if (config_flags % sw_call_calc_coef_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_coef_w ' )

         CALL push4backup (c2a,cqw, "c2a,cqw") 
         CALL push4backup (grid%mut, "mut") 
         CALL calc_coef_w( a,alpha,gamma,                    &
                           grid%mut, cqw,                    &
                           grid%rdn, grid%rdnw, c2a,         &
                           dts_rk, g, grid%epssm,            &
                           config_flags%top_lid,             &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_coef_w ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_coef_w }

       ENDIF

     ENDDO
     !$OMP END PARALLEL DO
BENCH_END(small_step_prep_tim)

!====[P1.10]==========================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%ru_tend, 'u', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%rv_tend, 'v', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,         &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%al, 'p', config_flags,           &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%p, 'p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%t_1, 'p', config_flags,          &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( grid%t_save, 't', config_flags,       &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

       CALL set_physical_bc2d( grid%mu_1, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

       CALL set_physical_bc2d( grid%mu_2, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

       CALL set_physical_bc2d( grid%mudf, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }


     END DO
     !$OMP END PARALLEL DO
BENCH_END(set_phys_bc2_tim)

!====[P1.11]==========================================================
     small_steps : DO iteration = 1 , number_of_small_timesteps

!======[P1.11.1]======================================================
       ! Boundary condition time (or communication time).
#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_B.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

BENCH_START(advance_uv_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_uv{
if (config_flags % sw_call_advance_uv) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_uv ' )

         CALL push4backup (grid%mu_2,grid%muu,grid%muv, "mu,muu,muv") 
         CALL push4backup (grid%ph_2,grid%alt,grid%p,grid%al,grid%php,cqu,cqv, "ph,alt,p,al,php,cqu,cqv") 
         CALL advance_uv ( grid%u_2, grid%ru_tend, grid%v_2, grid%rv_tend,        &
                           grid%p, grid%pb,                                       &
                           grid%ph_2, grid%php, grid%alt,  grid%al,               &
                           grid%mu_2,                                             &
                           grid%muu, cqu, grid%muv, cqv, grid%mudf,               &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_uv ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_uv }

BENCH_END(advance_uv_tim)

       END DO
       !$OMP END PARALLEL DO

!======[P1.11.2]: only needed by polar BC for global application
!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep u, v
!-----------------------------------------------------------

       IF (config_flags%polar) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
               ,positive_definite  = .FALSE.                                &
               ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
               ,fft_filter_lat = config_flags%fft_filter_lat                &
               ,dclat = dclat                                               &
               ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
               ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
               ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
               ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
               ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep u, v
!-----------------------------------------------------------

!======[P1.11.3]======================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

BENCH_START(spec_bdy_uv_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL spec_bdyupdate(grid%u_2, grid%ru_tend, dts_rk,      &
                               'u'         , config_flags,  &
                                grid%spec_zone,             &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL spec_bdyupdate(grid%v_2, grid%rv_tend, dts_rk,      &
                                'v'         , config_flags, &
                                grid%spec_zone,             &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),         &
                                grid%j_start(ij), grid%j_end(ij),         &
                                k_start    , k_end             )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }


         ENDIF
BENCH_END(spec_bdy_uv_tim)

       END DO
       !$OMP END PARALLEL DO

!======[P1.11.4]======================================================
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

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_mu_t{
if (config_flags % sw_call_advance_mu_t) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_mu_t ' )

         CALL push4backup (grid%muu,grid%muv,mu_tend, "muu,muv,mu_tend") 
         CALL push4backup (grid%ww,ww1,grid%u_2,grid%u_save,grid%v_2,grid%v_save,grid%t_save, &
                           "ww,ww_1,u,u_1,v,v_1,t_1") 
         CALL advance_mu_t( grid%ww, ww1, grid%u_2, grid%u_save, grid%v_2, grid%v_save, &
                          grid%mu_2, grid%mut, muave, grid%muts, grid%muu, grid%muv,    &
                          grid%mudf, grid%ru_m, grid%rv_m, grid%ww_m,                   &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_mu_t ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_mu_t }

BENCH_END(advance_mu_t_tim)
       ENDDO
       !$OMP END PARALLEL DO

!======[P1.11.5]: only needed by polar BC for global application
!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep mu, t
!-----------------------------------------------------------

       IF ( (config_flags%polar) ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                ,positive_definite  = .FALSE.                                &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


         grid%muts = grid%mut + grid%mu_2  ! reset muts using filtered mu_2

       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep mu, t
!-----------------------------------------------------------

!======[P1.11.6]======================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

!========[P1.11.6.1]==================================================
BENCH_START(spec_bdy_t_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL spec_bdyupdate(grid%t_2, t_tend, dts_rk,        &
                               't'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,       &
                               ims,ime, jms,jme, kms,kme,       &
                               ips,ipe, jps,jpe, kps,kpe,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               k_start    , k_end              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL spec_bdyupdate(grid%mu_2, mu_tend, dts_rk,      &
                               'm'         , config_flags,      &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,       &
                               ims,ime, jms,jme, 1  ,1  ,       &
                               ips,ipe, jps,jpe, 1  ,1  ,       &
                               grid%i_start(ij), grid%i_end(ij),&
                               grid%j_start(ij), grid%j_end(ij),&
                               1    , 1             )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL spec_bdyupdate(grid%muts, mu_tend, dts_rk,&
                              'm'         , config_flags, &
                              grid%spec_zone,             &
                              ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                              ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                              ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                              grid%i_start(ij), grid%i_end(ij),         &
                              grid%j_start(ij), grid%j_end(ij),         &
                              1    , 1             )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

         ENDIF
BENCH_END(spec_bdy_t_tim)


!======[P1.11.6.2]====================================================
         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.

BENCH_START(advance_w_tim)
         IF ( config_flags%non_hydrostatic ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_w{
if (config_flags % sw_call_advance_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_w ' )

           CALL push4backup (grid%mut,muave,grid%muts, "mut,muave,muts") 
           CALL push4backup (grid%u_2,grid%v_2,grid%w_2,rw_tend,grid%ww,w_save,t_2save,grid%t_2,grid%t_save, &
                             "u,v,w,rw_tend,ww,w_save,t_2ave,t_2,t_1")
           CALL push4backup (grid%ph_2,ph_save,ph_tend,c2a,cqw,grid%alt,a,alpha,gamma,     &
                             "ph,ph_1,ph_tend,c2a,cqw,alt,a,alpha,gamma")
           CALL advance_w( grid%w_2, rw_tend, grid%ww, w_save,         &
                           grid%u_2, grid%v_2,                         &
                           grid%mu_2, grid%mut, muave, grid%muts,      &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_w ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_w }

         ENDIF
BENCH_END(advance_w_tim)

       ENDDO
       !$OMP END PARALLEL DO

!======[P1.11.7]======================================================
!-----------------------------------------------------------
!  acoustic integration polar filter for smallstep w, geopotential
!-----------------------------------------------------------

       IF ( (config_flags%polar) .AND. (config_flags%non_hydrostatic) ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                ,positive_definite  = .FALSE.                                &
                ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                ,fft_filter_lat = config_flags%fft_filter_lat                &
                ,dclat = dclat                                               &
                ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


       END IF

!-----------------------------------------------------------
!  end acoustic integration polar filter for smallstep w, geopotential
!-----------------------------------------------------------

!======[P1.11.8]======================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

!========[P1.11.8.1]==================================================
BENCH_START(sumflux_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: sumflux{
if (config_flags % sw_call_sumflux) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call sumflux ' )

         CALL push4backup (grid%muu,grid%muv, "muu,muv") 
         CALL push4backup (grid%u_save,grid%v_save, "u_lin,v_lin") 
         CALL sumflux ( grid%u_2, grid%v_2, grid%ww,          &
                        grid%u_save, grid%v_save, ww1,        &
                        grid%muu, grid%muv,                   &
                        grid%ru_m, grid%rv_m, grid%ww_m, grid%epssm,  &
                        grid%msfux, grid% msfuy, grid%msfvx,  &
                        grid%msfvx_inv, grid%msfvy,           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call sumflux ' )
end if
! CHECK NECESSARY SUBROUTINE: sumflux }

BENCH_END(sumflux_tim)

!========[P1.11.8.2]==================================================
         IF( config_flags%specified .or. config_flags%nested ) THEN

BENCH_START(spec_bdynhyd_tim)
           IF (config_flags%non_hydrostatic)  THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate_ph{
if (config_flags % sw_call_spec_bdyupdate_ph) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate_ph ' )

             CALL push4backup (ph_save,grid%ph_2,ph_tend, "ph_save,ph,ph_tend") 
             CALL push4backup (mu_tend,grid%muts, "mu_tend,muts") 
             CALL spec_bdyupdate_ph( ph_save, grid%ph_2, ph_tend,     &
                                     mu_tend, grid%muts, dts_rk,      &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate_ph ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate_ph }

             IF( config_flags%specified ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_grad_bdy{
if (config_flags % sw_call_zero_grad_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call zero_grad_bdy ' )

               CALL zero_grad_bdy ( grid%w_2,                         &
                                    'w'         , config_flags,       &
                                    grid%spec_zone,                   &
                                    ids,ide, jds,jde, kds,kde,        &
                                    ims,ime, jms,jme, kms,kme,        &
                                    ips,ipe, jps,jpe, kps,kpe,        &
                                    grid%i_start(ij), grid%i_end(ij), &
                                    grid%j_start(ij), grid%j_end(ij), &
                                    k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call zero_grad_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_grad_bdy }

             ELSE

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

               CALL spec_bdyupdate ( grid%w_2, rw_tend, dts_rk,       &
                                     'h'         , config_flags,      &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,       &
                                     ims,ime, jms,jme, kms,kme,       &
                                     ips,ipe, jps,jpe, kps,kpe,       &
                                     grid%i_start(ij), grid%i_end(ij),&
                                     grid%j_start(ij), grid%j_end(ij),&
                                     k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

             ENDIF
           ENDIF
BENCH_END(spec_bdynhyd_tim)
         ENDIF

!========[P1.11.8.3]==================================================
 BENCH_START(cald_p_rho_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho{
if (config_flags % sw_call_calc_p_rho) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_p_rho ' )
 
          CALL push4backup (grid%mu_2,grid%muts, "mu,muts") 
          CALL push4backup (grid%alt,grid%ph_2,grid%t_2,grid%t_save,c2a, "alt,ph,t_2,t_1,c2a") 
          CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                &
                          grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                          grid%mu_2, grid%muts, grid%znu, t0,         &
                          grid%rdnw, grid%dnw, grid%smdiv,            &
                          config_flags%non_hydrostatic, iteration,    &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_p_rho ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho }

BENCH_END(cald_p_rho_tim)

       ENDDO
       !$OMP END PARALLEL DO

!======[P1.11.9]======================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,     &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%al, 'p', config_flags,       &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%p, 'p', config_flags,        &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,     &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

         CALL set_physical_bc2d( grid%mu_2, 't', config_flags,     &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

         CALL set_physical_bc2d( grid%mudf, 't', config_flags,     &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }


       END DO
       !$OMP END PARALLEL DO
BENCH_END(phys_bc_tim)

     END DO small_steps

!====[P1.12]==========================================================
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to
      ! full perturbation variables.
      ! first get updated mu at u and v points

!======[P1.12.1]======================================================
BENCH_START(calc_mu_uv_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_mu_uv_1{
if (config_flags % sw_call_calc_mu_uv_1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_mu_uv_1 ' )

       CALL calc_mu_uv_1 ( config_flags,                     &
                           grid%muts, muus, muvs,            &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_mu_uv_1 ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_mu_uv_1 }

BENCH_END(calc_mu_uv_tim)
!======[P1.12.2]======================================================
BENCH_START(small_step_finish_tim)

!======[P1.12.2.1]====================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: small_step_finish{
if (config_flags % sw_call_small_step_finish) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call small_step_finish ' )

       CALL push4backup (grid%mut,grid%muts,grid%muu,muus,grid%muv,muvs, "mut,muts,muu,muus,muv,muvs") 
       CALL push4backup (grid%u_2,grid%u_save,grid%v_2,grid%v_save,grid%w_2,w_save,grid%t_2,grid%t_save, &
                         grid%h_diabatic, "u,u_save,v,v_save,w,w_save,t,t_save,h_diabatic") 
       CALL small_step_finish( grid%u_2, grid%u_1, grid%v_2, grid%v_1, grid%w_2, grid%w_1, &
                               grid%t_2, grid%t_1, grid%ph_2, grid%ph_1, grid%ww, ww1,     &
                               grid%mu_2, grid%mu_1,                       &
                               grid%mut, grid%muts, grid%muu, muus, grid%muv, muvs,        &
                               grid%u_save, grid%v_save, w_save,           &
                               grid%t_save, ph_save, mu_save,              &
                               grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,grid%msfty, &
                               grid%h_diabatic,                  &
                               number_of_small_timesteps,dts_rk, &
                               rk_step, rk_order,                &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call small_step_finish ' )
end if
! CHECK NECESSARY SUBROUTINE: small_step_finish }

!======[P1.12.2.2]====================================================
!  call  to set ru_m, rv_m and ww_m b.c's for PD advection

       IF (rk_step == rk_order) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%ru_m, 'u', config_flags,      &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%rv_m, 'v', config_flags,      &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

         CALL set_physical_bc3d( grid%ww_m, 'w', config_flags,      &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

         CALL set_physical_bc2d( grid%mut, 't', config_flags,       &
                                 ids, ide, jds, jde,                &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc2d ' )

         CALL set_physical_bc2d( grid%muts, 't', config_flags,      &
                                 ids, ide, jds, jde,                &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }


       END IF

BENCH_END(small_step_finish_tim)

     END DO
     !$OMP END PARALLEL DO

!====[P1.13]==========================================================
!-----------------------------------------------------------
!  polar filter for full dynamics variables and time-averaged mass fluxes
!-----------------------------------------------------------

     IF (config_flags%polar) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite  = .FALSE.                                &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


     END IF

!-----------------------------------------------------------
!  end polar filter for full dynamics variables and time-averaged mass fluxes
!-----------------------------------------------------------

!====[P1.14]==========================================================
!-----------------------------------------------------------------------
!  add in physics tendency first if positive definite advection is used.
!  pd advection applies advective flux limiter on last runge-kutta step
!-----------------------------------------------------------------------
!====[P1.14]:moist====================================================
! first moisture

     IF ((config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

!======[P1.14.1]:moist================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_m

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar_pd moist' )

           CALL push4backup (grid%mu_1, "mu_1")
           CALL push4backup (moist_old(:,:,:,im),moist_tend(:,:,:,im), &
                             "moist_old,moist_tend")
           CALL rk_update_scalar_pd( im, im,                                   &
                                     moist_old(ims,kms,jms,im),                &
                                     moist_tend(ims,kms,jms,im),               &
                                     grid%mu_1, grid%mu_1, grid%mub,           &
                                     rk_step, dt_rk, grid%spec_zone,           &
                                     config_flags,                             &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar_pd moist' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
       END DO
       !$OMP END PARALLEL DO

!======[P1.14.2]:moist================================================
!---------------------- positive definite bc call
#ifdef DM_PARALLEL
       IF (config_flags%moist_adv_opt /= ORIGINAL) THEN
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

!======[P1.14.3]:moist================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
           DO im = PARAM_FIRST_SCALAR , num_3d_m

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

             CALL set_physical_bc3d( moist_old(ims,kms,jms,im), 'p', config_flags,  &
                                     ids, ide, jds, jde, kds, kde,                  &
                                     ims, ime, jms, jme, kms, kme,                  &
                                     ips, ipe, jps, jpe, kps, kpe,                  &
                                     grid%i_start(ij), grid%i_end(ij),              &
                                     grid%j_start(ij), grid%j_end(ij),              &
                                     k_start    , k_end                            )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

           END DO
         ENDIF
       END DO
       !$OMP END PARALLEL DO

     END IF  ! end if for moist_adv_opt

!====[P1.14]:scalars==================================================
! scalars

     IF ((config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

!======[P1.14.1]:scalar===============================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles
         CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
         DO im = PARAM_FIRST_SCALAR, num_3d_s

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar_pd ' )

           CALL push4backup (grid%mu_1, "mu_1")
           CALL push4backup (scalar_old(:,:,:,im),scalar_tend(:,:,:,im), &
                             "scalar_old,scalar_tend")
           CALL rk_update_scalar_pd( im, im,                                  &
                                     scalar_old(ims,kms,jms,im),              &
                                     scalar_tend(ims,kms,jms,im),             &
                                     grid%mu_1, grid%mu_1, grid%mub,          &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar_pd ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

!======[P1.14.2]:scalar===============================================
!---------------------- positive definite bc call
#ifdef DM_PARALLEL
       IF (config_flags%scalar_adv_opt /= ORIGINAL) THEN
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
!========[P1.14.3]:scalar=============================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles
           IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
             DO im = PARAM_FIRST_SCALAR , num_3d_s

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

               CALL set_physical_bc3d(  scalar_old(ims,kms,jms,im), 'p', config_flags,   &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       END IF  ! end if for scalar_adv_opt

!====[P1.14]:chem=====================================================
! chem

       IF ((config_flags%chem_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar_pd ' )

             CALL rk_update_scalar_pd( im, im,                                  &
                                       chem_old(ims,kms,jms,im),                &
                                       chem_tend(ims,kms,jms,im),               &
                                       grid%mu_1, grid%mu_1, grid%mub,          &
                                       rk_step, dt_rk, grid%spec_zone,          &
                                       config_flags,                            &
                                       ids, ide, jds, jde, kds, kde,            &
                                       ims, ime, jms, jme, kms, kme,            &
                                       grid%i_start(ij), grid%i_end(ij),        &
                                       grid%j_start(ij), grid%j_end(ij),        &
                                       k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar_pd ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

           ENDDO
         END DO
         !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%chem_adv_opt /= ORIGINAL) THEN
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

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

               CALL set_physical_bc3d(  chem_old(ims,kms,jms,im), 'p', config_flags,     &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  ! end if for chem_adv_opt

!====[P1.14]:tracer===================================================
! tracer

       IF ((config_flags%tracer_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
           DO im = PARAM_FIRST_SCALAR, num_tracer

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar_pd ' )

             CALL rk_update_scalar_pd( im, im,                                  &
                                       tracer_old(ims,kms,jms,im),              &
                                       tracer_tend(ims,kms,jms,im),             &
                                       grid%mu_1, grid%mu_1, grid%mub,          &
                                       rk_step, dt_rk, grid%spec_zone,          &
                                       config_flags,                            &
                                       ids, ide, jds, jde, kds, kde,            &
                                       ims, ime, jms, jme, kms, kme,            &
                                       grid%i_start(ij), grid%i_end(ij),        &
                                       grid%j_start(ij), grid%j_end(ij),        &
                                       k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar_pd ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

           ENDDO
         END DO
         !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%tracer_adv_opt /= ORIGINAL) THEN
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

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

               CALL set_physical_bc3d(  tracer_old(ims,kms,jms,im), 'p', config_flags,   &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

       ENDIF  ! end if for tracer_adv_opt

!====[P1.14]:tke======================================================
! tke

       IF ((config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order) &
           .and. (config_flags%km_opt .eq. 2)                ) THEN

!========[P1.14.1]:tke================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar_pd ' )

           CALL push4backup (grid%mu_1, "mu_1")
           CALL push4backup (grid%tke_1,tke_tend, &
                             "tke_1,tke_tend")
           CALL rk_update_scalar_pd( 1, 1,                                    &
                                     grid%tke_1,                              &
                                     tke_tend(ims,kms,jms),                   &
                                     grid%mu_1, grid%mu_1, grid%mub,          &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar_pd ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
         !$OMP END PARALLEL DO

!========[P1.14.2]:tke================================================
!---------------------- positive definite bc call
#ifdef DM_PARALLEL
         IF (config_flags%tke_adv_opt /= ORIGINAL) THEN
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

!========[P1.14.3]:tke================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

           CALL set_physical_bc3d(  grid%tke_1, 'p', config_flags,     &
                                    ids, ide, jds, jde, kds, kde,      &
                                    ims, ime, jms, jme, kms, kme,      &
                                    ips, ipe, jps, jpe, kps, kpe,      &
                                    grid%i_start(ij), grid%i_end(ij),  &
                                    grid%j_start(ij), grid%j_end(ij),  &
                                    k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

         END DO
         !$OMP END PARALLEL DO

!---  end of positive definite physics tendency update

       END IF  ! end if for tke_adv_opt

!====[P1.15]==========================================================
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
!    of the scalar (calling rk_scalar_update).
!</pre>
!</DESCRIPTION>


!====[P1.16]==========================================================
       moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

         moist_variable_loop: DO im = PARAM_FIRST_SCALAR, num_3d_m

! adv_moist_cond is set in module_physics_init based on mp_physics choice
!       true except for Ferrier scheme

           IF (grid%adv_moist_cond .or. im==p_qv ) THEN

!======[P1.16.1]======================================================
             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij )
             moist_tile_loop_1: DO ij = 1 , grid%num_tiles

!========[P1.16.1.1]==================================================
               CALL wrf_debug ( 200 , ' call rk_scalar_tend' )

BENCH_START(rk_scalar_tend_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_scalar_tend ' )

               IF ( rk_step == 1 ) CALL push4backup (grid%alt,grid%xkhh, "alt,xkhh")
               CALL push4backup (grid%mu_1,grid%muts, "mu_1,muts")
               CALL push4backup (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
               CALL push4backup (moist(:,:,:,im),moist_old(:,:,:,im), &
                                 "moist,moist_old")
               CALL rk_scalar_tend (  im, im, config_flags,                  &
                           rk_step, dt_rk,                                   &
                           grid%ru_m, grid%rv_m, grid%ww_m,                  &
                           grid%muts, grid%mub, grid%mu_1,                   &
                           grid%alt,                                         &
                           moist_old(ims,kms,jms,im),                        &
                           moist(ims,kms,jms,im),                            &
                           moist_tend(ims,kms,jms,im),                       &
                           advect_tend,grid%rqvften,                         &
                           grid%qv_base, .true., grid%fnm, grid%fnp,         &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,&
                           grid%msfvy, grid%msftx,grid%msfty,                &
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,                            &
                           grid%diff_6th_opt, grid%diff_6th_factor,          &
                           config_flags%moist_adv_opt,                       &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_scalar_tend ' )

   CALL zero_tend ( advect_tend(ims,kms,jms),     &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      grid%i_start(ij), grid%i_end(ij), &
                      grid%j_start(ij), grid%j_end(ij), &
                      k_start, k_end )
 end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

BENCH_END(rk_scalar_tend_tim)

!========[P1.16.1.2]==================================================
BENCH_START(rlx_bdy_scalar_tim)
               IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN
                 IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call relax_bdy_scalar ' )

                   CALL push4backup (grid%mut, "mut") 
                   CALL push4backup (moist(:,:,:,im), "moist") 
                   CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),         &
                                     moist(ims,kms,jms,im),  grid%mut,         &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call relax_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdy_scalar ' )

                   CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                     moist_bxs(jms,kms,1,im),moist_bxe(jms,kms,1,im), &
                                     moist_bys(ims,kms,1,im),moist_bye(ims,kms,1,im), &
                                     moist_btxs(jms,kms,1,im),moist_btxe(jms,kms,1,im), &
                                     moist_btys(ims,kms,1,im),moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone,       &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),          &
                                     grid%j_start(ij), grid%j_end(ij),          &
                                     k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }

                 ENDIF
               ENDIF
BENCH_END(rlx_bdy_scalar_tim)

             ENDDO moist_tile_loop_1
             !$OMP END PARALLEL DO

!======[P1.16.2]======================================================
             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij )
             moist_tile_loop_2: DO ij = 1 , grid%num_tiles

!========[P1.16.2.1]==================================================
               CALL wrf_debug ( 200 , ' call rk_update_scalar' )

BENCH_START(update_scal_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar ' )

               CALL push4backup ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
               IF ( rk_step == 1 ) THEN
                 CALL push4backup ( moist(:,:,:,im),moist_tend(:,:,:,im),advect_tend, &
                                    "moist,moist_tend,advect_tend" ) 
               ELSE
                 CALL push4backup ( moist_old(:,:,:,im),moist_tend(:,:,:,im),advect_tend, &
                                    "moist_old,moist_tend,advect_tend" ) 
               END IF 
               CALL rk_update_scalar( im, im,                     &
                               moist_old(ims,kms,jms,im),         &
                               moist(ims,kms,jms,im),             &
                               moist_tend(ims,kms,jms,im),        &
                               advect_tend, grid%msftx,grid%msfty, &
                               grid%mu_1, grid%mu_2, grid%mub,    &
                               rk_step, dt_rk, grid%spec_zone,    &
                               config_flags,                      &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }

BENCH_END(update_scal_tim)

!========[P1.16.2.2]==================================================
BENCH_START(flow_depbdy_tim)
               IF( config_flags%specified ) THEN
                 IF(im .ne. P_QV)THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call flow_dep_bdy ' )

                   CALL push4backup ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
                   CALL flow_dep_bdy  (  moist(ims,kms,jms,im),                 &
                                grid%ru_m, grid%rv_m, config_flags,             &
                                grid%spec_zone,                                 &
                                ids,ide, jds,jde, kds,kde,                      &
                                ims,ime, jms,jme, kms,kme,                      &
                                ips,ipe, jps,jpe, kps,kpe,                      &
                                grid%i_start(ij), grid%i_end(ij),               &
                                grid%j_start(ij), grid%j_end(ij),               &
                                k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call flow_dep_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

                 ENDIF
               ENDIF
BENCH_END(flow_depbdy_tim)

             ENDDO moist_tile_loop_2
             !$OMP END PARALLEL DO

           ENDIF  !-- if (grid%adv_moist_cond .or. im==p_qv ) then

         ENDDO moist_variable_loop

       ENDIF moist_scalar_advance

!====[P1.17]==========================================================
BENCH_START(tke_adv_tim)
       TKE_advance: IF (config_flags%km_opt .eq. 2) then
!======[P1.17.0]======================================================
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
!======[P1.17.1]======================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         tke_tile_loop_1: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_scalar_tend ' )

           IF ( rk_step == 1 ) CALL push4backup (grid%alt,grid%xkhh, "alt,xkhh")
           CALL push4backup (grid%mu_1,grid%muts, "mu_1,muts")
           CALL push4backup (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
           CALL push4backup (grid%tke_2,grid%tke_1, "tke_2,tke_1")
           CALL rk_scalar_tend ( 1, 1, config_flags,                               &
                            rk_step, dt_rk,                                        &
                            grid%ru_m, grid%rv_m, grid%ww_m,                       &
                            grid%muts, grid%mub, grid%mu_1,                        &
                            grid%alt,                                              &
                            grid%tke_1,                                            &
                            grid%tke_2,                                            &
                            tke_tend(ims,kms,jms),                                 &
                            advect_tend,grid%rqvften,                              &
                            grid%qv_base, .false., grid%fnm, grid%fnp,             &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,     &
                            grid%msfvy, grid%msftx,grid%msfty,                     &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif,   &
                            grid%kvdif, grid%xkhh,                                 &
                            grid%diff_6th_opt, grid%diff_6th_factor,               &
                            config_flags%tke_adv_opt,                              &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_scalar_tend ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

         ENDDO tke_tile_loop_1
         !$OMP END PARALLEL DO

!======[P1.17.2]======================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         tke_tile_loop_2: DO ij = 1 , grid%num_tiles

!========[P1.17.2.1]==================================================
           CALL wrf_debug ( 200 , ' call rk_update_scalar' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar ' )

           CALL push4backup ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
           IF ( rk_step == 1 ) THEN
             CALL push4backup ( grid%tke_2,tke_tend,advect_tend, &
                                "tke_2,tke_tend,advect_tend" ) 
           ELSE
             CALL push4backup ( grid%tke_1,tke_tend,advect_tend, &
                                "tke_1,tke_tend,advect_tend" ) 
           END IF 
           CALL rk_update_scalar( 1, 1,                             &
                             grid%tke_1,                            &
                             grid%tke_2,                            &
                             tke_tend(ims,kms,jms),                 &
                             advect_tend,grid%msftx,grid%msfty,     &
                             grid%mu_1, grid%mu_2, grid%mub,        &
                             rk_step, dt_rk, grid%spec_zone,        &
                             config_flags,                          &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }

!========[P1.17.2.2]==================================================
! bound the tke (greater than 0, less than tke_upper_bound)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: bound_tke{
if (config_flags % sw_call_bound_tke) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call bound_tke ' )

           CALL push4backup (grid%tke_2, "tke")
           CALL bound_tke( grid%tke_2, grid%tke_upper_bound,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           grid%i_start(ij), grid%i_end(ij),    &
                           grid%j_start(ij), grid%j_end(ij),    &
                           k_start    , k_end                  )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call bound_tke ' )
end if
! CHECK NECESSARY SUBROUTINE: bound_tke }

!========[P1.17.2.3]==================================================
           IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call flow_dep_bdy ' )

              CALL push4backup ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
              CALL flow_dep_bdy (  grid%tke_2,                 &
                                   grid%ru_m, grid%rv_m, config_flags,     &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij),       &
                                   grid%j_start(ij), grid%j_end(ij),       &
                                   k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call flow_dep_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

           ENDIF
         ENDDO tke_tile_loop_2
         !$OMP END PARALLEL DO

       ENDIF TKE_advance
BENCH_END(tke_adv_tim)

!====[P1.18]==========================================================
#ifdef WRF_CHEM
!  next the chemical species
BENCH_START(chem_adv_tim)
       chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

         chem_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_3d_c

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij )
           chem_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 200 , ' call rk_scalar_tend in chem_tile_loop_1' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_scalar_tend ' )

             CALL rk_scalar_tend ( ic, ic, config_flags,                         &
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%alt,                                          &
                              chem_old(ims,kms,jms,ic),                          &
                              chem(ims,kms,jms,ic),                              &
                              chem_tend(ims,kms,jms,ic),                         &
                              advect_tend,grid%rqvften,                          &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_scalar_tend ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

!
! Currently, chemistry species with specified boundaries (i.e. the mother
! domain)  are being over written by flow_dep_bdy_chem. So, relax_bdy and
! spec_bdy are only called for nests. For boundary conditions from global model or larger domain,
! chem is uncoupled, and only used for one row/column on inflow (if have_bcs_chem=.true.)
!
           IF( ( config_flags%nested ) .and. rk_step == 1 ) THEN
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_chem' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call relax_bdy_scalar ' )

             CALL relax_bdy_scalar ( chem_tend(ims,kms,jms,ic),                                    &
                                     chem(ims,kms,jms,ic),  grid%mut,                              &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call relax_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdy_scalar ' )

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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }

           ENDIF

         ENDDO chem_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         chem_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar ' )

           CALL rk_update_scalar( ic, ic,                           &
                                  chem_old(ims,kms,jms,ic),         &  ! was chem_1
                                  chem(ims,kms,jms,ic),             &
                                  chem_tend(ims,kms,jms,ic),        &
                                  advect_tend, grid%msftx, grid%msfty, &
                                  grid%mu_1, grid%mu_2, grid%mub,      &
                                  rk_step, dt_rk, grid%spec_zone,      &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }


           IF( config_flags%specified  ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy_chem{
if (config_flags % sw_call_flow_dep_bdy_chem) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call flow_dep_bdy_chem ' )

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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call flow_dep_bdy_chem ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy_chem }

           ENDIF
         ENDDO chem_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO chem_variable_loop
     ENDIF chem_scalar_advance
BENCH_END(chem_adv_tim)
#endif

!====[P1.19]==========================================================
!  next the chemical species
BENCH_START(tracer_adv_tim)
       tracer_advance: IF (num_tracer >= PARAM_FIRST_SCALAR)  THEN

         tracer_variable_loop: DO ic = PARAM_FIRST_SCALAR, num_tracer

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij )
           tracer_tile_loop_1: DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 15 , ' call rk_scalar_tend in tracer_tile_loop_1' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_scalar_tend ' )

             CALL rk_scalar_tend ( ic, ic, config_flags,                         &
                              rk_step, dt_rk,                                    &
                              grid%ru_m, grid%rv_m, grid%ww_m,                   &
                              grid%muts, grid%mub, grid%mu_1,                    &
                              grid%alt,                                          &
                              tracer_old(ims,kms,jms,ic),                          &
                              tracer(ims,kms,jms,ic),                              &
                              tracer_tend(ims,kms,jms,ic),                         &
                              advect_tend,grid%rqvften,                          &
                              grid%qv_base, .false., grid%fnm, grid%fnp,         &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx,grid%msfty,                 &
                              grid%rdx, grid%rdy, grid%rdn, grid%rdnw,           &
                              grid%khdif, grid%kvdif, grid%xkhh,                 &
                              grid%diff_6th_opt, grid%diff_6th_factor,           &
                              config_flags%tracer_adv_opt,                         &
                              ids, ide, jds, jde, kds, kde,                      &
                              ims, ime, jms, jme, kms, kme,                      &
                              grid%i_start(ij), grid%i_end(ij),                  &
                              grid%j_start(ij), grid%j_end(ij),                  &
                              k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_scalar_tend ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

!
! Currently, chemistry species with specified boundaries (i.e. the mother
! domain)  are being over written by flow_dep_bdy_chem. So, relax_bdy and
! spec_bdy are only called for nests. For boundary conditions from global model or larger domain,
! chem is uncoupled, and only used for one row/column on inflow (if have_bcs_chem=.true.)
!
           IF( ( config_flags%nested ) .and. rk_step == 1 ) THEN
             IF(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_tracer' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call relax_bdy_scalar ' )

             CALL relax_bdy_scalar ( tracer_tend(ims,kms,jms,ic),                                    &
                                     tracer(ims,kms,jms,ic),  grid%mut,                              &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call relax_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdy_scalar ' )

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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }

           ENDIF

         ENDDO tracer_tile_loop_1
         !$OMP END PARALLEL DO

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         tracer_tile_loop_2: DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call rk_update_scalar' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar ' )

           CALL rk_update_scalar( ic, ic,                           &
                                  tracer_old(ims,kms,jms,ic),       &
                                  tracer(ims,kms,jms,ic),           &
                                  tracer_tend(ims,kms,jms,ic),      &
                                  advect_tend, grid%msftx, grid%msfty, &
                                  grid%mu_1, grid%mu_2, grid%mub,      &
                                  rk_step, dt_rk, grid%spec_zone,      &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }


           IF( config_flags%specified  ) THEN
#ifdef WRF_CHEM

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy_tracer{
if (config_flags % sw_call_flow_dep_bdy_tracer) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call flow_dep_bdy_tracer ' )

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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call flow_dep_bdy_tracer ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy_tracer }

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

!====[P1.20]==========================================================
!  next the other scalar species
     other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

       scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
!========[P1.20.1]====================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

!==========[P1.20.1.1]================================================
           CALL wrf_debug ( 200 , ' call rk_scalar_tend' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_scalar_tend ' )

           IF ( rk_step == 1 ) CALL push4backup (grid%alt,grid%xkhh, "alt,xkhh")
           CALL push4backup (grid%mu_1,grid%muts, "mu_1,muts")
           CALL push4backup (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
           CALL push4backup (scalar(:,:,:,is),scalar_old(:,:,:,is), &
                             "scalar,scalar_old")
           CALL rk_scalar_tend ( is, is, config_flags,                            &
                                 rk_step, dt_rk,                                  &
                                 grid%ru_m, grid%rv_m, grid%ww_m,                 &
                                 grid%muts, grid%mub, grid%mu_1,                  &
                                 grid%alt,                                        &
                                 scalar_old(ims,kms,jms,is),                      &
                                 scalar(ims,kms,jms,is),                          &
                                 scalar_tend(ims,kms,jms,is),                     &
                                 advect_tend,grid%rqvften,                        &
                                 grid%qv_base, .false., grid%fnm, grid%fnp,       &
                                 grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                                 grid%msfvy, grid%msftx,grid%msfty,               &
                                 grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                                 grid%khdif, grid%kvdif, grid%xkhh,               &
                                 grid%diff_6th_opt, grid%diff_6th_factor,         &
                                 config_flags%scalar_adv_opt,                     &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_scalar_tend ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }


!==========[P1.20.1.2]================================================
           IF( config_flags%nested .and. (rk_step == 1) ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call relax_bdy_scalar ' )

               CALL push4backup (grid%mut, "mut") 
               CALL push4backup (scalar(:,:,:,is), "scalar") 
               CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),                            &
                                       scalar(ims,kms,jms,is),  grid%mut,                      &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call relax_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdy_scalar ' )

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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call spec_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }


           ENDIF ! b.c test for chem nested boundary condition

         ENDDO scalar_tile_loop_1
         !$OMP END PARALLEL DO

!========[P1.20.2]====================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

!==========[P1.20.2.1]================================================
           CALL wrf_debug ( 200 , ' call rk_update_scalar' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_update_scalar ' )

           CALL push4backup ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
           IF ( rk_step == 1 ) THEN
             CALL push4backup ( scalar(:,:,:,is),scalar_tend(:,:,:,is),advect_tend, &
                                "scalar,scalar_tend,advect_tend" ) 
           ELSE
             CALL push4backup ( scalar_old(:,:,:,is),scalar_tend(:,:,:,is),advect_tend, &
                                "scalar_old,scalar_tend,advect_tend" ) 
           END IF 
           CALL rk_update_scalar( is, is,                           &
                                  scalar_old(ims,kms,jms,is),       &  ! was scalar_1
                                  scalar(ims,kms,jms,is),           &
                                  scalar_tend(ims,kms,jms,is),      &
                                  advect_tend, grid%msftx, grid%msfty, &
                                  grid%mu_1, grid%mu_2, grid%mub,                  &
                                  rk_step, dt_rk, grid%spec_zone,        &
                                  config_flags,     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }



!==========[P1.20.2.2]================================================
           IF( config_flags%specified ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call flow_dep_bdy ' )

             CALL push4backup ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
             CALL flow_dep_bdy  ( scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call flow_dep_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

           ENDIF

         ENDDO scalar_tile_loop_2
         !$OMP END PARALLEL DO

       ENDDO scalar_variable_loop

     ENDIF other_scalar_advance

!====[P1.21]==========================================================
 !  update the pressure and density at the new time level

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

BENCH_START(calc_p_rho_tim)


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho_phi{
if (config_flags % sw_call_calc_p_rho_phi) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_p_rho_phi ' )

       CALL push4backup (grid%mu_2,grid%muts, "mu,muts")
       CALL push4backup (grid%ph_2,grid%t_2, "ph,t")
       CALL push4backup (moist, "moist")
       CALL calc_p_rho_phi( moist, num_3d_m,                  &
                            grid%al, grid%alb, grid%mu_2, grid%muts,  &
                            grid%ph_2, grid%p, grid%pb, grid%t_2,     &
                            p0, t0, grid%znu, grid%dnw, grid%rdnw,    &
                            grid%rdn, config_flags%non_hydrostatic,   &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_p_rho_phi ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho_phi }


BENCH_END(calc_p_rho_tim)

     ENDDO
     !$OMP END PARALLEL DO

!====[P1.22]==========================================================
!  Reset the boundary conditions if there is another corrector step.
!  (rk_step < rk_order), else we'll handle it at the end of everything
!  (after the split physics, before exiting the timestep).

     rk_step_1_check: IF ( rk_step < rk_order ) THEN

!======[P1.22.1]======================================================
!-----------------------------------------------------------
!  rk3 substep polar filter for scalars (moist,chem,scalar)
!-----------------------------------------------------------

       IF (config_flags%polar) THEN
         IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter moist ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_m

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter{
if (config_flags % sw_call_couple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call couple_scalars_for_filter ' )

             CALL couple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)              &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call couple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter{
if (config_flags % sw_call_uncouple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call uncouple_scalars_for_filter ' )

             CALL uncouple_scalars_for_filter ( FIELD=moist(ims,kms,jms,im)            &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call uncouple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter }

           END DO
         END IF

         IF ( num_3d_c >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter chem ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_c

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter{
if (config_flags % sw_call_couple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call couple_scalars_for_filter ' )

             CALL couple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)               &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call couple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter{
if (config_flags % sw_call_uncouple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call uncouple_scalars_for_filter ' )

             CALL uncouple_scalars_for_filter ( FIELD=chem(ims,kms,jms,im)             &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call uncouple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter }

           END DO
         END IF
         IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter tracer ' )
           DO im = PARAM_FIRST_SCALAR, num_tracer

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter{
if (config_flags % sw_call_couple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call couple_scalars_for_filter ' )

             CALL couple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)               &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe           )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call couple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                    ,positive_definite=.FALSE.                                   &
                    ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                    ,fft_filter_lat = config_flags%fft_filter_lat                &
                    ,dclat = dclat                                               &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                    ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                    ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter{
if (config_flags % sw_call_uncouple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call uncouple_scalars_for_filter ' )

             CALL uncouple_scalars_for_filter ( FIELD=tracer(ims,kms,jms,im)             &
                    ,MU=grid%mu_2 , MUB=grid%mub                                 &
                    ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                    ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                    ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call uncouple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter }

           END DO
         END IF

         IF ( num_3d_s >= PARAM_FIRST_SCALAR ) THEN
           CALL wrf_debug ( 200 , ' call filter scalar ' )
           DO im = PARAM_FIRST_SCALAR, num_3d_s

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter{
if (config_flags % sw_call_couple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call couple_scalars_for_filter ' )

             CALL couple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)           &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call couple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: couple_scalars_for_filter }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter{
if (config_flags % sw_call_uncouple_scalars_for_filter) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call uncouple_scalars_for_filter ' )

             CALL uncouple_scalars_for_filter ( FIELD=scalar(ims,kms,jms,im)   &
                  ,MU=grid%mu_2 , MUB=grid%mub                                 &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call uncouple_scalars_for_filter ' )
end if
! CHECK NECESSARY SUBROUTINE: uncouple_scalars_for_filter }

           END DO
         END IF
       END IF ! polar filter test

!-----------------------------------------------------------
!  END rk3 substep polar filter for scalars (moist,chem,scalar)
!-----------------------------------------------------------

!======[P1.22.2]======================================================
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
#endif

BENCH_START(bc_end_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       tile_bc_loop_1: DO ij = 1 , grid%num_tiles
!========[P1.22.2.1]==================================================
         CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_2{
if (config_flags % sw_call_rk_phys_bc_dry_2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call rk_phys_bc_dry_2 ' )

         CALL rk_phys_bc_dry_2( config_flags,                     &
                                grid%u_2, grid%v_2, grid%w_2,     &
                                grid%t_2, grid%ph_2, grid%mu_2,   &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call rk_phys_bc_dry_2 ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_2 }


!========[P1.22.2.2]==================================================
BENCH_START(diag_w_tim)
         IF (.not. config_flags%non_hydrostatic) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: diagnose_w{
if (config_flags % sw_call_diagnose_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call diagnose_w ' )

           CALL push4backup (grid%muts, "muts")
           CALL push4backup (ph_tend, "ph_tend")
           CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                            grid%u_2, grid%v_2, grid%ht,                           &
                            grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                            ids, ide, jds, jde, kds, kde,           &
                            ims, ime, jms, jme, kms, kme,           &
                            grid%i_start(ij), grid%i_end(ij),       &
                            grid%j_start(ij), grid%j_end(ij),       &
                            k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call diagnose_w ' )
end if
! CHECK NECESSARY SUBROUTINE: diagnose_w }

         ENDIF
BENCH_END(diag_w_tim)

!========[P1.22.2.3]==================================================
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

           moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

             CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     ips, ipe, jps, jpe, kps, kpe,             &
                                     grid%i_start(ij), grid%i_end(ij),                   &
                                     grid%j_start(ij), grid%j_end(ij),                   &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

           END DO moisture_loop_bdy_1

         ENDIF

!========[P1.22.2.4]==================================================
         IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN

           chem_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_3d_c


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

             CALL set_physical_bc3d( chem(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


           END DO chem_species_bdy_loop_1

         END IF

!========[P1.22.2.5]==================================================
         IF (num_tracer >= PARAM_FIRST_SCALAR) THEN

           tracer_species_bdy_loop_1 : DO ic = PARAM_FIRST_SCALAR , num_tracer


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

             CALL set_physical_bc3d( tracer(ims,kms,jms,ic), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


           END DO tracer_species_bdy_loop_1

         END IF

!========[P1.22.2.6]==================================================
         IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN

           scalar_species_bdy_loop_1 : DO is = PARAM_FIRST_SCALAR , num_3d_s


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

             CALL set_physical_bc3d( scalar(ims,kms,jms,is), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


           END DO scalar_species_bdy_loop_1

         END IF

!========[P1.22.2.7]==================================================
         IF (config_flags%km_opt .eq. 2) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

           CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,            &
                                   ims, ime, jms, jme, kms, kme,            &
                                   ips, ipe, jps, jpe, kps, kpe,            &
                                   grid%i_start(ij), grid%i_end(ij),        &
                                   grid%j_start(ij), grid%j_end(ij),        &
                                   k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

         END IF

       END DO tile_bc_loop_1
       !$OMP END PARALLEL DO
BENCH_END(bc_end_tim)


!======[P1.22.3]======================================================
#ifdef DM_PARALLEL

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist, chem, scalar, tke      x


!======[P1.22.3]:tke==================================================
       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_5.inc"
         ELSE
#         include "HALO_EM_TKE_3.inc"
         ENDIF
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_7.inc"
         ELSE
#         include "HALO_EM_TKE_5.inc"
         ENDIF
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF

!======[P1.22.3]:moist================================================
       IF ( num_moist .GE. PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_5.inc"
           ELSE
#        include "HALO_EM_MOIST_E_3.inc"
           END IF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_7.inc"
           ELSE
#        include "HALO_EM_MOIST_E_5.inc"
           END IF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
!======[P1.22.3]:chem=================================================
       IF ( num_chem >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_5.inc"
           ELSE
#        include "HALO_EM_CHEM_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%chem_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_7.inc"
           ELSE
#        include "HALO_EM_CHEM_E_5.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
!======[P1.22.3]:tracer===============================================
       IF ( num_tracer >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_5.inc"
           ELSE
#        include "HALO_EM_TRACER_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%tracer_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_TRACER_E_7.inc"
           ELSE
#        include "HALO_EM_TRACER_E_5.inc"
           ENDIF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
!======[P1.22.3]:scalar===============================================
       IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
           ELSE
#        include "HALO_EM_SCALAR_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
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

!==[P2]===============================================================
   IF (config_flags%do_avgflx_em .EQ. 1) THEN
! Reinitialize time-averaged fluxes if history output was written after the previous time step:

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: WRFU_ALARMGET{
if (config_flags % sw_call_WRFU_ALARMGET) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call WRFU_ALARMGET ' )

      CALL WRFU_ALARMGET(grid%alarms( HISTORY_ALARM ),prevringtime=temp_time)

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call WRFU_ALARMGET ' )
end if
! CHECK NECESSARY SUBROUTINE: WRFU_ALARMGET }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: domain_clock_get{
if (config_flags % sw_call_domain_clock_get) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call domain_clock_get ' )

      CALL domain_clock_get ( grid, current_time=CurrTime, &
           current_timestr=message2 )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call domain_clock_get ' )
end if
! CHECK NECESSARY SUBROUTINE: domain_clock_get }

! use overloaded -, .LT. operator to check whether to initialize avgflx:
! reinitialize after each history output (detect this here by comparing current time
! against last history time and time step - this code follows what's done in adapt_timestep_em):
      WRITE ( message , FMT = '("solve_em: old_dt =",g15.6,", dt=",g15.6," on domain ",I3)' ) &
           & old_dt,grid%dt,grid%id
      CALL wrf_debug(200,message)
      old_dt=min(old_dt,grid%dt)
      num = INT(old_dt * precision)
      den = precision

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: WRFU_TimeIntervalSet{
if (config_flags % sw_call_WRFU_TimeIntervalSet) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call WRFU_TimeIntervalSet ' )

      CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call WRFU_TimeIntervalSet ' )
end if
! CHECK NECESSARY SUBROUTINE: WRFU_TimeIntervalSet }

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

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_avgflx{
if (config_flags % sw_call_zero_avgflx) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call zero_avgflx ' )

            CALL zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
                 &   ids, ide, jds, jde, kds, kde,           &
                 &   ims, ime, jms, jme, kms, kme,           &
                 &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
                 &   k_start    , k_end, f_flux, &
                 &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
                 &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call zero_avgflx ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_avgflx }

            CALL wrf_debug(200,'In solve_em, after zero_avgflx call')
         ENDDO
      ENDIF

! Update avgflx quantities
!tile-loop for upd_avgflx
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         CALL wrf_debug(200,'In solve_em, before upd_avgflx call')

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: upd_avgflx{
if (config_flags % sw_call_upd_avgflx) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call upd_avgflx ' )

         CALL upd_avgflx(grid%avgflx_count,grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   grid%ru_m, grid%rv_m, grid%ww_m, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
              &   k_start    , k_end, f_flux, &
              &   grid%cfu1,grid%cfd1,grid%dfu1,grid%efu1,grid%dfd1,grid%efd1,          &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call upd_avgflx ' )
end if
! CHECK NECESSARY SUBROUTINE: upd_avgflx }

         CALL wrf_debug(200,'In solve_em, after upd_avgflx call')

      ENDDO
      grid%avgflx_count = grid%avgflx_count + 1
   ENDIF
!
!==[P3]===============================================================
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

BENCH_START(advance_ppt_tim)
     CALL wrf_debug ( 200 , ' call advance_ppt' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_ppt{
if (config_flags % sw_call_advance_ppt) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call advance_ppt ' )

     CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                      grid%rqicuten,grid%rqscuten,grid%rainc,grid%raincv,grid%pratec, grid%nca,    &
                      grid%htop,grid%hbot,grid%cutop,grid%cubot,                 &
                      grid%cuppt, grid%dt, config_flags,                   &
                      ids,ide, jds,jde, kds,kde,             &
                      ims,ime, jms,jme, kms,kme,             &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call advance_ppt ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_ppt }

BENCH_END(advance_ppt_tim)

   ENDDO
  !$OMP END PARALLEL DO

!==[P4]===============================================================
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

!==[P4.1]=============================================================
   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = grid%spec_zone
   ELSE
     sz = 0
   ENDIF

!==[P4.2]=============================================================
   IF (config_flags%mp_physics /= 0)  then

!====[P4.2.1]=========================================================
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )

     scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

!======[P4.2.1.0]=========================================================
       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

!======[P4.2.1.1]=========================================================
       CALL wrf_debug ( 200 , ' call moist_physics_prep' )
BENCH_START(moist_physics_prep_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: moist_physics_prep_em{
if (config_flags % sw_call_moist_physics_prep_em) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call moist_physics_prep_em ' )

       CALL moist_physics_prep_em( grid%t_2, grid%t_1, t0, rho,                &
                                   grid%al, grid%alb, grid%p, p8w, p0, grid%pb,          &
                                   grid%ph_2, grid%phb, th_phy, pi_phy, p_phy, &
                                   grid%z, grid%z_at_w, dz8w,                  &
                                   dtm, grid%h_diabatic,                  &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call moist_physics_prep_em ' )
end if
! CHECK NECESSARY SUBROUTINE: moist_physics_prep_em }

BENCH_END(moist_physics_prep_tim)
     END DO scalar_tile_loop_1a
     !$OMP END PARALLEL DO

!====[P4.2.2]=============================================================
     CALL wrf_debug ( 200 , ' call microphysics_driver' )

     grid%sr = 0.
     specified_bdy = config_flags%specified .OR. config_flags%nested
     channel_bdy = config_flags%specified .AND. config_flags%periodic_x

!====[P4.2.3]=============================================================
BENCH_START(micro_driver_tim)


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_driver{
if (config_flags % sw_call_microphysics_driver) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_driver ' )

     CALL microphysics_driver(                                            &
      &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy   &
      &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy              &
      &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr  &
      &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy     &
      &        ,RHO=rho            ,SPEC_ZONE=grid%spec_zone              &
      &        ,SR=grid%sr              ,TH=th_phy                        &
      &        ,refl_10cm=grid%refl_10cm                                  & ! hm, 9/22/09 for refl
      &        ,WARM_RAIN=grid%warm_rain                                  &
      &        ,T8W=t8w                                                   &
      &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
      &        ,NSOURCE=grid%qndropsource                                 &
#ifdef WRF_CHEM
      &        ,QLSINK=grid%qlsink,CLDFRA_OLD=grid%cldfra_old             &
      &        ,PRECR=grid%precr, PRECI=grid%preci, PRECS=grid%precs, PRECG=grid%precg &
      &        ,CHEM_OPT=config_flags%chem_opt, PROGN=config_flags%progn  &
#endif
      &        ,XLAND=grid%xland                                          &
      &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy       &
      &        ,F_RAIN_PHY=grid%f_rain_phy                                &
      &        ,F_RIMEF_PHY=grid%f_rimef_phy                              &
      &        ,MP_PHYSICS=config_flags%mp_physics                        &
      &        ,ID=grid%id                                                &
      &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde         &
      &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme         &
#ifdef RUN_ON_GPU
      &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe         &
#endif
      &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
      &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
      &        ,KTS=k_start, KTE=min(k_end,kde-1)                         &
      &        ,NUM_TILES=grid%num_tiles                                  &
      &        ,NAER=grid%naer                                            &
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
      &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
      &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT              &
      &        , QNN_CURR=scalar(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
      &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI          &
      &        , QNC_CURR=scalar(ims,kms,jms,P_QNC), F_QNC=F_QNC          &
      &        , QNR_CURR=scalar(ims,kms,jms,P_QNR), F_QNR=F_QNR          &
      &        , QNS_CURR=scalar(ims,kms,jms,P_QNS), F_QNS=F_QNS          &
      &        , QNG_CURR=scalar(ims,kms,jms,P_QNG), F_QNG=F_QNG          &
      &        , QNH_CURR=scalar(ims,kms,jms,P_QNH), F_QNH=F_QNH          & ! for milbrandt2mom
!       &        , QZR_CURR=scalar(ims,kms,jms,P_QZR), F_QZR=F_QZR          & ! for milbrandt3mom
!       &        , QZI_CURR=scalar(ims,kms,jms,P_QZI), F_QZI=F_QZI          & ! "
!       &        , QZS_CURR=scalar(ims,kms,jms,P_QZS), F_QZS=F_QZS          & ! "
!       &        , QZG_CURR=scalar(ims,kms,jms,P_QZG), F_QZG=F_QZG          & ! "
!       &        , QZH_CURR=scalar(ims,kms,jms,P_QZH), F_QZH=F_QZH          & ! "
      &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &
      &        , qicuten=grid%rqicuten,mu=grid%mut                        &
      &        , HAIL=config_flags%gsfcgce_hail                           & ! for gsfcgce
      &        , ICE2=config_flags%gsfcgce_2ice                           & ! for gsfcgce
!     &        , ccntype=config_flags%milbrandt_ccntype                   & ! for milbrandt (2mom)
                                                                          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_driver ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_driver }

BENCH_END(micro_driver_tim)

!====[P4.2.4]=============================================================
#if 0
BENCH_START(microswap_2)
! for load balancing; communication to redistribute the points
     IF ( config_flags%mp_physics .EQ. ETAMPNEW ) THEN
#include "SWAP_ETAMP_NEW.inc"
     ELSE IF ( config_flags%mp_physics .EQ. WSM3SCHEME ) THEN
#include "SWAP_WSM3.inc"
     ENDIF
BENCH_END(microswap_2)
#endif

!====[P4.2.5]=============================================================
     CALL wrf_debug ( 200 , ' call moist_physics_finish' )
BENCH_START(moist_phys_end_tim)

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

     DO ij = 1 , grid%num_tiles

!======[P4.2.5.1]=========================================================
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)

!======[P4.2.5.2]=========================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outb ' )

       CALL microphysics_zero_outb (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outb ' )

       CALL microphysics_zero_outb (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outb ' )

       CALL microphysics_zero_outb (                                    &
                      chem , num_chem , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outb ' )

       CALL microphysics_zero_outb (                                    &
                      tracer , num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }


!======[P4.2.5.3]=========================================================
       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)


!======[P4.2.5.4]=========================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outa ' )

       CALL microphysics_zero_outa (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outa ' )

       CALL microphysics_zero_outa (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outa ' )

       CALL microphysics_zero_outa (                                    &
                      chem , num_chem , config_flags ,                  &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outa ' )

       CALL microphysics_zero_outa (                                    &
                      tracer , num_tracer , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }


!======[P4.2.5.5]=========================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: moist_physics_finish_em{
if (config_flags % sw_call_moist_physics_finish_em) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call moist_physics_finish_em ' )

       CALL moist_physics_finish_em( grid%t_2, grid%t_1, t0, grid%muts, th_phy,       &
                                      grid%h_diabatic, dtm, config_flags,    &
#if ( WRF_DFI_RADAR == 1 )
                                      grid%dfi_tten_rad,grid%dfi_stage,        &
#endif
                                      ids, ide, jds, jde, kds, kde,     &
                                      ims, ime, jms, jme, kms, kme,     &
                                      its, ite, jts, jte,               &
                                      k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call moist_physics_finish_em ' )
end if
! CHECK NECESSARY SUBROUTINE: moist_physics_finish_em }


     END DO
     !$OMP END PARALLEL DO
BENCH_END(moist_phys_end_tim)

   ENDIF  ! microphysics test

!==[P4.3]=============================================================
!-----------------------------------------------------------
!  filter for moist variables post-microphysics and end of timestep
!-----------------------------------------------------------

   IF (config_flags%polar) THEN
     IF ( num_3d_m >= PARAM_FIRST_SCALAR ) THEN
       CALL wrf_debug ( 200 , ' call filter moist' )
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               moist(ii,kk,jj,im)=moist(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO
     ENDIF
   ENDIF

!-----------------------------------------------------------
!  end filter for moist variables post-microphysics and end of timestep
!-----------------------------------------------------------

!==[P4.4]=============================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho_phi{
if (config_flags % sw_call_calc_p_rho_phi) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call calc_p_rho_phi ' )

     CALL push4backup (grid%mu_2,grid%muts, "mu,muts")
     CALL push4backup (grid%ph_2,grid%t_2, "ph,t")
     CALL push4backup (moist, "moist")
     CALL calc_p_rho_phi( moist, num_3d_m,                &
                          grid%al, grid%alb, grid%mu_2, grid%muts,              &
                          grid%ph_2, grid%p, grid%pb, grid%t_2,                 &
                          p0, t0, grid%znu, grid%dnw, grid%rdnw,           &
                          grid%rdn, config_flags%non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          its, ite, jts, jte,               &
                          k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call calc_p_rho_phi ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho_phi }


   END DO scalar_tile_loop_1ba
   !$OMP END PARALLEL DO
!BENCH_END(moist_phys_end_tim)

!==[P5]===============================================================
   IF (.not. config_flags%non_hydrostatic) THEN
#ifdef DM_PARALLEL
#    include "HALO_EM_HYDRO_UV.inc"
#    include "PERIOD_EM_HYDRO_UV.inc"
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: diagnose_w{
if (config_flags % sw_call_diagnose_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call diagnose_w ' )

       CALL push4backup (grid%muts, "muts")
       CALL push4backup (ph_tend, "ph_tend")
       CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                       grid%u_2, grid%v_2, grid%ht,                           &
                       grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                       ids, ide, jds, jde, kds, kde,           &
                       ims, ime, jms, jme, kms, kme,           &
                       grid%i_start(ij), grid%i_end(ij),       &
                       grid%j_start(ij), grid%j_end(ij),       &
                       k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call diagnose_w ' )
end if
! CHECK NECESSARY SUBROUTINE: diagnose_w }


     END DO
     !$OMP END PARALLEL DO

   END IF

!==[P6]===============================================================
   CALL wrf_debug ( 200 , ' call chem polar filter ' )

!-----------------------------------------------------------
!  filter for chem and scalar variables at end of timestep
!-----------------------------------------------------------

   IF (config_flags%polar) THEN

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then
       chem_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_c
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               chem(ii,kk,jj,im)=chem(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO chem_filter_loop
     ENDIF
     IF ( num_tracer >= PARAM_FIRST_SCALAR ) then
       tracer_filter_loop: DO im = PARAM_FIRST_SCALAR, num_tracer
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               tracer(ii,kk,jj,im)=tracer(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO tracer_filter_loop
     ENDIF

     IF ( num_3d_s >= PARAM_FIRST_SCALAR ) then
       scalar_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_s
         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: pxft{
if (config_flags % sw_call_pxft) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call pxft ' )

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
                  ,positive_definite=.FALSE.                                   &
                  ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
                  ,fft_filter_lat = config_flags%fft_filter_lat                &
                  ,dclat = dclat                                               &
                  ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                  ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                  ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                  ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                  ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call pxft ' )
end if
! CHECK NECESSARY SUBROUTINE: pxft }


         DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
               scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             ENDDO
           ENDDO
         ENDDO
       ENDDO scalar_filter_loop
     ENDIF
   ENDIF

!-----------------------------------------------------------
!  end filter for chem and scalar variables at end of timestep
!-----------------------------------------------------------

!==[P7]===============================================================
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


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_phys_bc_dry_2{
if (config_flags % sw_call_set_phys_bc_dry_2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_phys_bc_dry_2 ' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             grid%u_1, grid%u_2, grid%v_1, grid%v_2, grid%w_1, grid%w_2,           &
                             grid%t_1, grid%t_2, grid%ph_1, grid%ph_2, grid%mu_1, grid%mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_phys_bc_dry_2 ' )
end if
! CHECK NECESSARY SUBROUTINE: set_phys_bc_dry_2 }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

     CALL set_physical_bc3d( grid%tke_1, 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

     CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( chem(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     END DO chem_species_bdy_loop_2

     tracer_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_tracer


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( tracer(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     END DO tracer_species_bdy_loop_2

     scalar_species_bdy_loop_2 : DO is = PARAM_FIRST_SCALAR , num_3d_s


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_physical_bc3d ' )

       CALL set_physical_bc3d( scalar(ims,kms,jms,is) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     END DO scalar_species_bdy_loop_2

   END DO tile_bc_loop_2
   !$OMP END PARALLEL DO
BENCH_END(bc_2d_tim)

!==[P8]===============================================================
! Move to the end of subroutine
!   IF( config_flags%specified .or. config_flags%nested ) THEN
!     grid%dtbc = grid%dtbc + grid%dt
!   ENDIF

!==[P9]===============================================================
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

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_w_surface{
if (config_flags % sw_call_set_w_surface) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call set_w_surface ' )

      CALL set_w_surface( config_flags, grid%znw, fill_w_flag,              &
                           grid%w_2, grid%ht,  grid%u_2, grid%v_2,          &
                           grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy,&
                           grid%msftx, grid%msfty,                          &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call set_w_surface ' )
end if
! CHECK NECESSARY SUBROUTINE: set_w_surface }

!                          its, ite, jts, jte, k_start, min(k_end,kde-1),   &

   END DO
   !$OMP END PARALLEL DO

!==[P10]==============================================================
! calculate some model diagnostics.

   CALL wrf_debug ( 200 , ' call diagnostic_driver' )


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: diagnostic_output_calc{
if (config_flags % sw_call_diagnostic_output_calc) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call diagnostic_output_calc ' )

   CALL diagnostic_output_calc(                                            &
      &              DPSDT=grid%dpsdt   ,DMUDT=grid%dmudt                  &
      &             ,P8W=p8w   ,PK1M=grid%pk1m                             &
      &             ,MU_2=grid%mu_2  ,MU_2M=grid%mu_2m                     &
      &             ,U=grid%u_2    ,V=grid%v_2                             &
      &             ,RAINCV=grid%raincv    ,RAINNCV=grid%rainncv           &
      &             ,RAINC=grid%rainc    ,RAINNC=grid%rainnc               &
      &             ,I_RAINC=grid%i_rainc    ,I_RAINNC=grid%i_rainnc       &
      &             ,HFX=grid%hfx   ,SFCEVP=grid%sfcevp    ,LH=grid%lh     &
      &             ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width      &
      &             ,XTIME=grid%xtime   ,T2=grid%t2                        &
      &        ,ACSWUPT=grid%acswupt    ,ACSWUPTC=grid%acswuptc            &
      &        ,ACSWDNT=grid%acswdnt    ,ACSWDNTC=grid%acswdntc            &
      &        ,ACSWUPB=grid%acswupb    ,ACSWUPBC=grid%acswupbc            &
      &        ,ACSWDNB=grid%acswdnb    ,ACSWDNBC=grid%acswdnbc            &
      &        ,ACLWUPT=grid%aclwupt    ,ACLWUPTC=grid%aclwuptc            &
      &        ,ACLWDNT=grid%aclwdnt    ,ACLWDNTC=grid%aclwdntc            &
      &        ,ACLWUPB=grid%aclwupb    ,ACLWUPBC=grid%aclwupbc            &
      &        ,ACLWDNB=grid%aclwdnb    ,ACLWDNBC=grid%aclwdnbc            &
      &      ,I_ACSWUPT=grid%i_acswupt  ,I_ACSWUPTC=grid%i_acswuptc        &
      &      ,I_ACSWDNT=grid%i_acswdnt  ,I_ACSWDNTC=grid%i_acswdntc        &
      &      ,I_ACSWUPB=grid%i_acswupb  ,I_ACSWUPBC=grid%i_acswupbc        &
      &      ,I_ACSWDNB=grid%i_acswdnb  ,I_ACSWDNBC=grid%i_acswdnbc        &
      &      ,I_ACLWUPT=grid%i_aclwupt  ,I_ACLWUPTC=grid%i_aclwuptc        &
      &      ,I_ACLWDNT=grid%i_aclwdnt  ,I_ACLWDNTC=grid%i_aclwdntc        &
      &      ,I_ACLWUPB=grid%i_aclwupb  ,I_ACLWUPBC=grid%i_aclwupbc        &
      &      ,I_ACLWDNB=grid%i_aclwdnb  ,I_ACLWDNBC=grid%i_aclwdnbc        &
                  ! Selection flag
      &             ,DIAG_PRINT=config_flags%diag_print                    &
      &             ,BUCKET_MM=config_flags%bucket_mm                      &
      &             ,BUCKET_J =config_flags%bucket_J                       &
      &             ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc    &
      &             ,PREC_ACC_C=grid%prec_acc_c                            &
      &             ,PREC_ACC_NC=grid%prec_acc_nc                          &
      &             ,PREC_ACC_DT=config_flags%prec_acc_dt                  &
      &             ,CURR_SECS=curr_secs                                   &
                  ! Dimension arguments
      &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
      &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
      &             ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
      &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
      &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
      &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
      &             ,NUM_TILES=grid%num_tiles                              &
      &                                                          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call diagnostic_output_calc ' )
end if
! CHECK NECESSARY SUBROUTINE: diagnostic_output_calc }


!==[P11]==============================================================
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

!==[P12]==============================================================
! Move to the end of subroutine
!!  Max values of CFL for adaptive time step scheme

!   DEALLOCATE(max_vert_cfl_tmp)
!   DEALLOCATE(max_horiz_cfl_tmp)

!==[P13]==============================================================
   CALL wrf_debug ( 200 , ' call end of solve_em' )

! Finish timers if compiled with -DBENCH.
#include <bench_solve_em_end.h>

!---------------------------------------- 
! Start adjoint computations
!---------------------------------------- 
! Reset local adjoint variables
  a_cqu = 0. 
  a_cqv = 0. 
  a_cqw = 0. 

  a_ru_tendf = 0.
  a_rv_tendf = 0. 
  a_rw_tendf = 0. 
  a_ph_tendf = 0. 
  a_t_tendf = 0. 
  a_mu_tendf = 0. 

  a_rw_tend = 0. 
  a_ph_tend = 0. 
  a_t_tend = 0. 
  a_mu_tend = 0. 
  a_moist_tend = 0. 
  a_scalar_tend = 0. 
  a_tke_tend = 0. 
  a_advect_tend = 0. 

  a_th_phy = 0. 
  a_p_phy = 0. 
  a_pi_phy = 0. 
  a_t_phy = 0. 
  a_u_phy = 0. 
  a_v_phy = 0. 
  a_rho = 0. 
  a_dz8w = 0. 
  a_p8w = 0. 
  a_t8w = 0. 

  a_w_save = 0. 
  a_ph_save = 0. 
  a_mu_save = 0. 
  a_t_2save = 0. 
  a_ww1 = 0. 
  a_moist_old = 0. 
  a_scalar_old = 0. 

  a_muus = 0. 
  a_muvs = 0. 
  a_muave = 0. 
  a_c2a = 0. 
  a_pm1 = 0. 
  a_a = 0. 
  a_alpha = 0. 
  a_gamma = 0. 

! [1] Adjoint of the part after Runge Kutta loop

!==[adj_P9]===============================================================
!  Adjoint of resetting surface w for consistency

!!!!! Need to include AD for halo (the same below). Ning Pan
#ifdef DM_PARALLEL
#  include "HALO_EM_C.inc"
#  include "PERIOD_BDY_EM_E.inc"
#endif

   CALL wrf_debug ( 10 , ' call a_set_w_surface' )
   fill_w_flag = .false.

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = grid%num_tiles,1,-1  

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_w_surface{
if (config_flags % sw_call_set_w_surface) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_w_surface ' )

      CALL a_set_w_surface( config_flags, grid%znw, fill_w_flag,            &
                           grid%w_2,grid%a_w_2, grid%ht,                    &
                           grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2,        &
                           grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy,&
                           grid%msftx, grid%msfty,                          &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_w_surface ' )
end if
! CHECK NECESSARY SUBROUTINE: set_w_surface }

   END DO
   !$OMP END PARALLEL DO

!==[adj_P7]===============================================================
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

BENCH_START(adj_bc_2d_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   adj_tile_bc_loop_2: DO ij = grid%num_tiles,1,-1

     CALL wrf_debug ( 200 , ' call a_set_phys_bc_dry_2' )


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_phys_bc_dry_2{
if (config_flags % sw_call_set_phys_bc_dry_2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_phys_bc_dry_2 ' )

     CALL a_set_phys_bc_dry_2( config_flags,                           &
                             grid%u_1,grid%a_u_1, grid%u_2,grid%a_u_2, &
                             grid%v_1,grid%a_v_1, grid%v_2,grid%a_v_2, &
                             grid%w_1,grid%a_w_1, grid%w_2,grid%a_w_2, &
                             grid%t_1,grid%a_t_1, grid%t_2,grid%a_t_2, &
                             grid%ph_1,grid%a_ph_1, grid%ph_2,grid%a_ph_2, &
                             grid%mu_1,grid%a_mu_1, grid%mu_2,grid%a_mu_2, &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_phys_bc_dry_2 ' )
end if
! CHECK NECESSARY SUBROUTINE: set_phys_bc_dry_2 }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

     CALL a_set_physical_bc3d( grid%a_tke_1, 'p', config_flags,       &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

     CALL a_set_physical_bc3d( grid%a_tke_2 , 'p', config_flags,      &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


     adj_moisture_loop_bdy_2 : DO im = num_3d_m,PARAM_FIRST_SCALAR,-1


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( a_moist(ims,kms,jms,im), 'p',         &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

     END DO adj_moisture_loop_bdy_2

     adj_scalar_species_bdy_loop_2 : DO is = num_3d_s,PARAM_FIRST_SCALAR,-1 

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( a_scalar(ims,kms,jms,is) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),        &
                               grid%j_start(ij), grid%j_end(ij),        &
                               k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

     END DO adj_scalar_species_bdy_loop_2

   END DO adj_tile_bc_loop_2
   !$OMP END PARALLEL DO
BENCH_END(adj_bc_2d_tim)

!==[adj_P5]===============================================================
   IF (.not. config_flags%non_hydrostatic) THEN
#ifdef DM_PARALLEL
#    include "HALO_EM_HYDRO_UV.inc"
#    include "PERIOD_EM_HYDRO_UV.inc"
#endif
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = grid%num_tiles,1,-1  

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: diagnose_w{
if (config_flags % sw_call_diagnose_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_diagnose_w ' )

       CALL pop2restore (ph_tend, "ph_tend")
       CALL pop2restore (grid%muts, "muts")
       CALL a_diagnose_w( ph_tend,a_ph_tend, grid%ph_2,grid%a_ph_2,  grid%ph_1,grid%a_ph_1, &
                       grid%w_2,grid%a_w_2, grid%muts,grid%a_muts, dt_rk,  &
                       grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2, grid%ht,  &
                       grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                       ids, ide, jds, jde, kds, kde,           &
                       ims, ime, jms, jme, kms, kme,           &
                       grid%i_start(ij), grid%i_end(ij),       &
                       grid%j_start(ij), grid%j_end(ij),       &
                       k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_diagnose_w ' )
end if
! CHECK NECESSARY SUBROUTINE: diagnose_w }

     END DO
     !$OMP END PARALLEL DO

   END IF

!==[adj_P4]===============================================================
!  Adjoint of time-split physics

!==[adj_P4.4]=============================================================
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )
   adj_scalar_tile_loop_1ba: DO ij = grid%num_tiles,1,-1

     IF ( config_flags%periodic_x ) THEN
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
     ELSE
       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
     ENDIF
     jts = max(grid%j_start(ij),jds+sz)
     jte = min(grid%j_end(ij),jde-1-sz)


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho_phi{
if (config_flags % sw_call_calc_p_rho_phi) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_p_rho_phi ' )

     CALL pop2restore (moist, "moist")
     CALL pop2restore (grid%ph_2,grid%t_2, "ph,t")
     CALL pop2restore (grid%mu_2,grid%muts, "mu,muts")
     CALL a_calc_p_rho_phi( moist,a_moist, num_3d_m,        &
                          grid%al,grid%a_al, grid%alb, grid%mu_2,grid%a_mu_2, grid%muts,grid%a_muts, &
                          grid%ph_2,grid%a_ph_2, grid%p,grid%a_p, grid%pb, grid%t_2,grid%a_t_2,      &
                          p0, t0, grid%znu, grid%dnw, grid%rdnw,   &
                          grid%rdn, config_flags%non_hydrostatic,  &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          its, ite, jts, jte,               &
                          k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_p_rho_phi ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho_phi }

   END DO adj_scalar_tile_loop_1ba
   !$OMP END PARALLEL DO

!==[adj_P4.2]=============================================================
   IF (config_flags%mp_physics /= 0)  then

!====[P4.2.4_adj]=========================================================
#if 0
BENCH_START(adj_microswap_2)
! for load balancing; communication to redistribute the points
     IF ( config_flags%mp_physics .EQ. ETAMPNEW ) THEN
#include "SWAP_ETAMP_NEW.inc"
     ELSE IF ( config_flags%mp_physics .EQ. WSM3SCHEME ) THEN
#include "SWAP_WSM3.inc"
     ENDIF
BENCH_END(adj_microswap_2)
#endif

!====[adj_P4.2.5]=========================================================
     CALL wrf_debug ( 200 , ' call a_moist_physics_finish' )
BENCH_START(adj_moist_phys_end_tim)

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

     DO ij = grid%num_tiles,1,-1

!======[P4.2.5.3_adj]=====================================================
       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

!======[adj_P4.2.5.5]=====================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: moist_physics_finish_em{
if (config_flags % sw_call_moist_physics_finish_em) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_moist_physics_finish_em ' )

!!!!! REPLACE WITH a_moist_physics_finish_em WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL moist_physics_finish_em( grid%t_2, grid%t_1, t0, grid%muts, th_phy,       &
                                      grid%h_diabatic, dtm, config_flags,    &
#if ( WRF_DFI_RADAR == 1 )
                                      grid%dfi_tten_rad,grid%dfi_stage,        &
#endif
                                      ids, ide, jds, jde, kds, kde,     &
                                      ims, ime, jms, jme, kms, kme,     &
                                      its, ite, jts, jte,               &
                                      k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_moist_physics_finish_em ' )
end if
! CHECK NECESSARY SUBROUTINE: moist_physics_finish_em }

!======[adj_P4.2.5.4]=====================================================
!      Remove codes on chem and tracer from [P4.2.5.4]

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_microphysics_zero_outa ' )

!!!!! REPLACE WITH a_microphysics_zero_outa WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL microphysics_zero_outa (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outa{
if (config_flags % sw_call_microphysics_zero_outa) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_microphysics_zero_outa ' )

!!!!! REPLACE WITH a_microphysics_zero_outa WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL microphysics_zero_outa (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_microphysics_zero_outa ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outa }

!======[P4.2.5.1_adj]=====================================================
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)

!======[adj_P4.2.5.2]=====================================================
!      Remove codes on chem and tracer from [P4.2.5.2]

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call microphysics_zero_outb ' )

!!!!! REPLACE WITH a_microphysics_zero_outb WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL microphysics_zero_outb (                                    &
                      scalar , num_scalar , config_flags ,              &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_zero_outb{
if (config_flags % sw_call_microphysics_zero_outb) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_microphysics_zero_outb ' )

!!!!! REPLACE WITH a_microphysics_zero_outb WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL microphysics_zero_outb (                                    &
                      moist , num_moist , config_flags ,                &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte,                               &
                      k_start    , k_end                                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_microphysics_zero_outb ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_zero_outb }

     END DO
     !$OMP END PARALLEL DO
BENCH_END(adj_moist_phys_end_tim)

!====[P4.2.2_adj]=========================================================
     CALL wrf_debug ( 200 , ' call a_microphysics_driver' )

     grid%sr = 0.

!====[adj_P4.2.3]=========================================================
BENCH_START(adj_micro_driver_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: microphysics_driver{
if (config_flags % sw_call_microphysics_driver) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_microphysics_driver ' )

!!!!! REPLACE WITH a_microphysics_driver WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
     CALL microphysics_driver(                                            &
      &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy   &
      &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy              &
      &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr  &
      &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy     &
      &        ,RHO=rho            ,SPEC_ZONE=grid%spec_zone              &
      &        ,SR=grid%sr              ,TH=th_phy                        &
      &        ,refl_10cm=grid%refl_10cm                                  & ! hm, 9/22/09 for refl
      &        ,WARM_RAIN=grid%warm_rain                                  &
      &        ,T8W=t8w                                                   &
      &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
      &        ,NSOURCE=grid%qndropsource                                 &
#ifdef WRF_CHEM
      &        ,QLSINK=grid%qlsink,CLDFRA_OLD=grid%cldfra_old             &
      &        ,PRECR=grid%precr, PRECI=grid%preci, PRECS=grid%precs, PRECG=grid%precg &
      &        ,CHEM_OPT=config_flags%chem_opt, PROGN=config_flags%progn  &
#endif
      &        ,XLAND=grid%xland                                          &
      &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy       &
      &        ,F_RAIN_PHY=grid%f_rain_phy                                &
      &        ,F_RIMEF_PHY=grid%f_rimef_phy                              &
      &        ,MP_PHYSICS=config_flags%mp_physics                        &
      &        ,ID=grid%id                                                &
      &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde         &
      &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme         &
#ifdef RUN_ON_GPU
      &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe         &
#endif
      &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
      &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
      &        ,KTS=k_start, KTE=min(k_end,kde-1)                         &
      &        ,NUM_TILES=grid%num_tiles                                  &
      &        ,NAER=grid%naer                                            &
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
      &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
      &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT              &
      &        , QNN_CURR=scalar(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
      &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI          &
      &        , QNC_CURR=scalar(ims,kms,jms,P_QNC), F_QNC=F_QNC          &
      &        , QNR_CURR=scalar(ims,kms,jms,P_QNR), F_QNR=F_QNR          &
      &        , QNS_CURR=scalar(ims,kms,jms,P_QNS), F_QNS=F_QNS          &
      &        , QNG_CURR=scalar(ims,kms,jms,P_QNG), F_QNG=F_QNG          &
      &        , QNH_CURR=scalar(ims,kms,jms,P_QNH), F_QNH=F_QNH          & ! for milbrandt2mom
!       &        , QZR_CURR=scalar(ims,kms,jms,P_QZR), F_QZR=F_QZR          & ! for milbrandt3mom
!       &        , QZI_CURR=scalar(ims,kms,jms,P_QZI), F_QZI=F_QZI          & ! "
!       &        , QZS_CURR=scalar(ims,kms,jms,P_QZS), F_QZS=F_QZS          & ! "
!       &        , QZG_CURR=scalar(ims,kms,jms,P_QZG), F_QZG=F_QZG          & ! "
!       &        , QZH_CURR=scalar(ims,kms,jms,P_QZH), F_QZH=F_QZH          & ! "
      &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &
      &        , qicuten=grid%rqicuten,mu=grid%mut                        &
      &        , HAIL=config_flags%gsfcgce_hail                           & ! for gsfcgce
      &        , ICE2=config_flags%gsfcgce_2ice                           & ! for gsfcgce
!     &        , ccntype=config_flags%milbrandt_ccntype                   & ! for milbrandt (2mom)
                                                                          )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_microphysics_driver ' )
end if
! CHECK NECESSARY SUBROUTINE: microphysics_driver }

BENCH_END(adj_micro_driver_tim)

!====[adj_P4.2.2]=========================================================
     grid%a_sr = 0.

!====[adj_P4.2.1]=========================================================
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )

     adj_scalar_tile_loop_1a: DO ij = grid%num_tiles,1,-1

!======[P4.2.1.0_adj]=====================================================
       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

!======[adj_P4.2.1.1]=====================================================
       CALL wrf_debug ( 200 , ' call a_moist_physics_prep' )
BENCH_START(adj_moist_physics_prep_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: moist_physics_prep_em{
if (config_flags % sw_call_moist_physics_prep_em) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_moist_physics_prep_em ' )

!!!!! REPLACE WITH a_moist_physics_prep_em WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL moist_physics_prep_em( grid%t_2, grid%t_1, t0, rho,                &
                                   grid%al, grid%alb, grid%p, p8w, p0, grid%pb,          &
                                   grid%ph_2, grid%phb, th_phy, pi_phy, p_phy, &
                                   grid%z, grid%z_at_w, dz8w,                  &
                                   dtm, grid%h_diabatic,                  &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_moist_physics_prep_em ' )
end if
! CHECK NECESSARY SUBROUTINE: moist_physics_prep_em }

BENCH_END(adj_moist_physics_prep_tim)
     END DO adj_scalar_tile_loop_1a
     !$OMP END PARALLEL DO

   ENDIF  ! adj microphysics test

!==[adj_P3]===============================================================
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = grid%num_tiles,1,-1

BENCH_START(adj_advance_ppt_tim)
     CALL wrf_debug ( 200 , ' call a_advance_ppt' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_ppt{
if (config_flags % sw_call_advance_ppt) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_ppt ' )

!!!!! REPLACE WITH a_advance_ppt WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
     CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                      grid%rqicuten,grid%rqscuten,grid%rainc,grid%raincv,grid%pratec, grid%nca,&
                      grid%htop,grid%hbot,grid%cutop,grid%cubot,                 &
                      grid%cuppt, grid%dt, config_flags,                   &
                      ids,ide, jds,jde, kds,kde,             &
                      ims,ime, jms,jme, kms,kme,             &
                      grid%i_start(ij), grid%i_end(ij),      &
                      grid%j_start(ij), grid%j_end(ij),      &
                      k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_ppt ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_ppt }

BENCH_END(adj_advance_ppt_tim)

   ENDDO
  !$OMP END PARALLEL DO

! [2] Adjoint of Runge Kutta loop
!==[adj_P1]===============================================================
   adj_Runge_Kutta_loop:  DO rk_step = rk_order, 1, -1

!====[P1.1_adj]===========================================================
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

!====[adj_P1.22]==========================================================
!    Adjoint of resetting the boundary conditions

     adj_rk_step_1_check: IF ( rk_step < rk_order ) THEN

!======[P1.22.3_adj]======================================================
#ifdef DM_PARALLEL

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist, chem, scalar, tke      x

!======[adj_P1.22.3]:tke==================================================
       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_5.inc"
         ELSE
#         include "HALO_EM_TKE_3.inc"
         ENDIF
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
         IF ( (config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_7.inc"
         ELSE
#         include "HALO_EM_TKE_5.inc"
         ENDIF
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF

!======[adj_P1.22.3]:moist================================================
       IF ( num_moist .GE. PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_5.inc"
           ELSE
#        include "HALO_EM_MOIST_E_3.inc"
           END IF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_7.inc"
           ELSE
#        include "HALO_EM_MOIST_E_5.inc"
           END IF
         ELSE
           WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
           CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
       ENDIF
!======[adj_P1.22.3]:scalar===============================================
       IF ( num_scalar >= PARAM_FIRST_SCALAR ) THEN
         IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
           ELSE
#        include "HALO_EM_SCALAR_E_3.inc"
           ENDIF
         ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
           IF ( (config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order-1) ) THEN
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

!======[adj_P1.22.2]======================================================
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
#endif

BENCH_START(adj_bc_end_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       adj_tile_bc_loop_1: DO ij = grid%num_tiles,1,-1

!========[adj_P1.22.2.7]==================================================
         IF (config_flags%km_opt .eq. 2) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

           CALL a_set_physical_bc3d( grid%a_tke_2 , 'p', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,            &
                                   ims, ime, jms, jme, kms, kme,            &
                                   ips, ipe, jps, jpe, kps, kpe,            &
                                   grid%i_start(ij), grid%i_end(ij),        &
                                   grid%j_start(ij), grid%j_end(ij),        &
                                   k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

         END IF

!========[adj_P1.22.2.6]==================================================
         IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN

           adj_scalar_species_bdy_loop_1 : DO is = num_3d_s,PARAM_FIRST_SCALAR,-1 


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

             CALL a_set_physical_bc3d( a_scalar(ims,kms,jms,is), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     ips, ipe, jps, jpe, kps, kpe,            &
                                     grid%i_start(ij), grid%i_end(ij),                  &
                                     grid%j_start(ij), grid%j_end(ij),                  &
                                     k_start    , k_end-1                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }


           END DO adj_scalar_species_bdy_loop_1

         END IF

!========[adj_P1.22.2.3]==================================================
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

           adj_moisture_loop_bdy_1 : DO im = num_3d_m,PARAM_FIRST_SCALAR,-1 

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

             CALL a_set_physical_bc3d( a_moist(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     ips, ipe, jps, jpe, kps, kpe,             &
                                     grid%i_start(ij), grid%i_end(ij),                   &
                                     grid%j_start(ij), grid%j_end(ij),                   &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

           END DO adj_moisture_loop_bdy_1

         ENDIF

!========[adj_P1.22.2.2]==================================================
BENCH_START(adj_diag_w_tim)
         IF (.not. config_flags%non_hydrostatic) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: diagnose_w{
if (config_flags % sw_call_diagnose_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_diagnose_w ' )

           CALL pop2restore (ph_tend, "ph_tend")
           CALL pop2restore (grid%muts, "muts")
           CALL a_diagnose_w( ph_tend,a_ph_tend, grid%ph_2,grid%a_ph_2,  grid%ph_1,grid%a_ph_1,     &
                          grid%w_2,grid%a_w_2, grid%muts,grid%a_muts, dt_rk,  &
                          grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2, grid%ht,  &
                          grid%cf1, grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          grid%i_start(ij), grid%i_end(ij),       &
                          grid%j_start(ij), grid%j_end(ij),       &
                          k_start    , k_end                     )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_diagnose_w ' )
end if
! CHECK NECESSARY SUBROUTINE: diagnose_w }

         ENDIF
BENCH_END(adj_diag_w_tim)

!========[adj_P1.22.2.1]==================================================
         CALL wrf_debug ( 200 , ' call a_rk_phys_bc_dry_2' )
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_2{
if (config_flags % sw_call_rk_phys_bc_dry_2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_phys_bc_dry_2 ' )

         CALL a_rk_phys_bc_dry_2( config_flags,                     &
                                grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2, grid%w_2,grid%a_w_2, &
                                grid%t_2,grid%a_t_2, grid%ph_2,grid%a_ph_2, grid%mu_2,grid%a_mu_2,   &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_phys_bc_dry_2 ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_2 }

       END DO adj_tile_bc_loop_1
       !$OMP END PARALLEL DO
BENCH_END(adj_bc_end_tim)

     ENDIF adj_rk_step_1_check

!====[adj_P1.21]==========================================================
!    Adjoint of updating the pressure and density at the new time level

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = grid%num_tiles,1,-1

BENCH_START(adj_calc_p_rho_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho_phi{
if (config_flags % sw_call_calc_p_rho_phi) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_p_rho_phi ' )

       CALL pop2restore (moist, "moist")
       CALL pop2restore (grid%ph_2,grid%t_2, "ph,t")
       CALL pop2restore (grid%mu_2,grid%muts, "mu,muts")
       CALL a_calc_p_rho_phi( moist,a_moist, num_3d_m,         &
                             grid%al,grid%a_al, grid%alb, grid%mu_2,grid%a_mu_2, grid%muts,grid%a_muts, &
                             grid%ph_2,grid%a_ph_2, grid%p,grid%a_p, grid%pb, grid%t_2,grid%a_t_2,      &
                             p0, t0, grid%znu, grid%dnw, grid%rdnw,   &
                             grid%rdn, config_flags%non_hydrostatic,  &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_p_rho_phi ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho_phi }

BENCH_END(adj_calc_p_rho_tim)

     ENDDO
     !$OMP END PARALLEL DO

!====[P1.15_adj]==========================================================
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

!====[adj_P1.20]==========================================================
!  next the other scalar species
     adj_other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

       adj_scalar_variable_loop: do is = num_3d_s,PARAM_FIRST_SCALAR,-1
!========[adj_P1.20.2]====================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         adj_scalar_tile_loop_2: DO ij = grid%num_tiles,1,-1

!==========[adj_P1.20.2.2]================================================
           IF( config_flags%specified ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_flow_dep_bdy ' )

             CALL pop2restore ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
             CALL a_flow_dep_bdy  ( a_scalar(ims,kms,jms,is),     &
                                  grid%ru_m, grid%rv_m, config_flags,   &
                                  grid%spec_zone,                  &
                                  ids,ide, jds,jde, kds,kde,  & ! domain dims
                                  ims,ime, jms,jme, kms,kme,  & ! memory dims
                                  ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                  grid%i_start(ij), grid%i_end(ij),  &
                                  grid%j_start(ij), grid%j_end(ij),  &
                                  k_start, k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_flow_dep_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

           ENDIF

!==========[adj_P1.20.2.1]================================================
           CALL wrf_debug ( 200 , ' call a_rk_update_scalar scalar ' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar scalar' )

           IF ( rk_step == 1 ) THEN
             CALL pop2restore ( scalar(:,:,:,is),scalar_tend(:,:,:,is),advect_tend, &
                                "scalar,scalar_tend,advect_tend" ) 
           ELSE
             CALL pop2restore ( scalar_old(:,:,:,is),scalar_tend(:,:,:,is),advect_tend,&
                                "scalar_old,scalar_tend,advect_tend" ) 
           END IF 
           CALL pop2restore ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
           CALL a_rk_update_scalar( is, is,                           &
                                  scalar_old(ims,kms,jms,is), a_scalar_old(ims,kms,jms,is),   &
                                  scalar(ims,kms,jms,is), a_scalar(ims,kms,jms,is),           &
                                  scalar_tend(ims,kms,jms,is), a_scalar_tend(ims,kms,jms,is), &
                                  advect_tend, a_advect_tend, grid%msftx,grid%msfty, &
                                  grid%mu_1, grid%a_mu_1, grid%mu_2, grid%a_mu_2, grid%mub,   &
                                  rk_step, dt_rk, grid%spec_zone,   &
                                  config_flags,                     &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }

         ENDDO adj_scalar_tile_loop_2
         !$OMP END PARALLEL DO

!========[adj_P1.20.1]====================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         adj_scalar_tile_loop_1: DO ij = grid%num_tiles,1,-1

!==========[adj_P1.20.1.2]================================================
           IF( config_flags%nested .and. (rk_step == 1) ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdy_scalar scalar' )

               CALL a_spec_bdy_scalar  ( a_scalar_tend(ims,kms,jms,is),                        &
                                       a_scalar_btxs(jms,kms,1,is),a_scalar_btxe(jms,kms,1,is),&
                                       a_scalar_btys(ims,kms,1,is),a_scalar_btye(ims,kms,1,is),&
                                       config_flags%spec_bdy_width, grid%spec_zone,            &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdy_scalar scalar' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_relax_bdy_scalar scalar' )

               CALL pop2restore (scalar(:,:,:,is), "scalar") 
               CALL pop2restore (grid%mut, "mut") 
               CALL a_relax_bdy_scalar ( a_scalar_tend(ims,kms,jms,is),                        &
                                       scalar(ims,kms,jms,is),a_scalar(ims,kms,jms,is),        &
                                       grid%mut,grid%a_mut,                                    &
                                       a_scalar_bxs(jms,kms,1,is),a_scalar_bxe(jms,kms,1,is),  &
                                       a_scalar_bys(ims,kms,1,is),a_scalar_bye(ims,kms,1,is),  &
                                       a_scalar_btxs(jms,kms,1,is),a_scalar_btxe(jms,kms,1,is),&
                                       a_scalar_btys(ims,kms,1,is),a_scalar_btye(ims,kms,1,is),&
                                       config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                       grid%dtbc, grid%fcx, grid%gcx,                          &
                                       config_flags,                                           &
                                       ids,ide, jds,jde, kds,kde,                              &
                                       ims,ime, jms,jme, kms,kme,                              &
                                       ips,ipe, jps,jpe, kps,kpe,                              &
                                       grid%i_start(ij), grid%i_end(ij),                       &
                                       grid%j_start(ij), grid%j_end(ij),                       &
                                       k_start, k_end                                          )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_relax_bdy_scalar scalar' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }

           ENDIF ! b.c test for chem nested boundary condition

!==========[adj_P1.20.1.1]================================================
           CALL wrf_debug ( 200 , ' call a_rk_scalar_tend scalar ' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_scalar_tend scalar' )

           CALL pop2restore (scalar(:,:,:,is),scalar_old(:,:,:,is), &
                             "scalar,scalar_old")
           CALL pop2restore (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
           CALL pop2restore (grid%mu_1,grid%muts, "mu_1,muts")
           IF ( rk_step == 1 ) CALL pop2restore (grid%alt,grid%xkhh, "alt,xkhh")
           CALL a_rk_scalar_tend ( is, is, config_flags, &
                       rk_step, dt_rk,         &
                       grid%ru_m, grid%a_ru_m, &
                       grid%rv_m, grid%a_rv_m, &
                       grid%ww_m, grid%a_ww_m, &
                       grid%muts, grid%a_muts, grid%mub, grid%mu_1, grid%a_mu_1,   &
                       grid%alt, grid%a_alt,                                       &
                       scalar_old(ims,kms,jms,is), a_scalar_old(ims,kms,jms,is),   &
                       scalar(ims,kms,jms,is), a_scalar(ims,kms,jms,is),           &
                       scalar_tend(ims,kms,jms,is), a_scalar_tend(ims,kms,jms,is), &
                       advect_tend, a_advect_tend, grid%rqvften, grid%a_rqvften,   &
                       grid%qv_base, .false., grid%fnm, grid%fnp,       &
                       grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                       grid%msfvy, grid%msftx,grid%msfty,               &
                       grid%rdx, grid%rdy, grid%rdn, grid%rdnw,         &
                       grid%khdif, grid%kvdif, grid%xkhh,grid%a_xkhh,   &
                       grid%diff_6th_opt, grid%diff_6th_factor,         &
                       config_flags%scalar_adv_opt,                     &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_scalar_tend scalar' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

         ENDDO adj_scalar_tile_loop_1
         !$OMP END PARALLEL DO

       ENDDO adj_scalar_variable_loop

     ENDIF adj_other_scalar_advance

!====[adj_P1.17]==========================================================
BENCH_START(adj_tke_adv_tim)
       adj_TKE_advance: IF (config_flags%km_opt .eq. 2) then
!======[P1.17.0_adj]======================================================
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
!======[adj_P1.17.2]======================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         adj_tke_tile_loop_2: DO ij = grid%num_tiles,1,-1

!========[adj_P1.17.2.3]==================================================
           IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_flow_dep_bdy tke' )

              CALL pop2restore ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
              CALL a_flow_dep_bdy (  grid%a_tke_2,                   &
                                   grid%ru_m, grid%rv_m, config_flags, &
                                   grid%spec_zone,                              &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij),       &
                                   grid%j_start(ij), grid%j_end(ij),       &
                                   k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_flow_dep_bdy tke' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

           ENDIF

!========[adj_P1.17.2.2]==================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: bound_tke{
if (config_flags % sw_call_bound_tke) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_bound_tke ' )

           CALL pop2restore (grid%tke_2, "tke")
           CALL a_bound_tke( grid%tke_2,grid%a_tke_2, grid%tke_upper_bound,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           grid%i_start(ij), grid%i_end(ij),    &
                           grid%j_start(ij), grid%j_end(ij),    &
                           k_start    , k_end                  )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_bound_tke ' )
end if
! CHECK NECESSARY SUBROUTINE: bound_tke }

!========[adj_P1.17.2.1]==================================================
           CALL wrf_debug ( 200 , ' call a_rk_update_scalar tke' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar tke' )

           IF ( rk_step == 1 ) THEN
             CALL pop2restore ( grid%tke_2,tke_tend,advect_tend, &
                                "tke_2,tke_tend,advect_tend" ) 
           ELSE
             CALL pop2restore ( grid%tke_1,tke_tend,advect_tend,&
                                "tke_1,tke_tend,advect_tend" ) 
           END IF 
           CALL pop2restore ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
           CALL a_rk_update_scalar( 1, 1,                           &
                             grid%tke_1,grid%a_tke_1,               &
                             grid%tke_2,grid%a_tke_2,               &
                             tke_tend(ims,kms,jms),a_tke_tend(ims,kms,jms),   &
                             advect_tend,a_advect_tend,grid%msftx,grid%msfty, &
                             grid%mu_1,grid%a_mu_1, grid%mu_2,grid%a_mu_2, grid%mub,  &
                             rk_step, dt_rk, grid%spec_zone,   &
                             config_flags,                     &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar tke' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }

         ENDDO adj_tke_tile_loop_2
         !$OMP END PARALLEL DO

!======[adj_P1.17.1]======================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         adj_tke_tile_loop_1: DO ij = grid%num_tiles,1,-1

           CALL wrf_debug ( 200 , ' call a_rk_scalar_tend for tke' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_scalar_tend tke' )

           CALL pop2restore (grid%tke_2,grid%tke_1, "tke_2,tke_1")
           CALL pop2restore (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
           CALL pop2restore (grid%mu_1,grid%muts, "mu_1,muts")
           IF ( rk_step == 1 ) CALL pop2restore (grid%alt,grid%xkhh, "alt,xkhh")
           CALL a_rk_scalar_tend ( 1, 1, config_flags,                             &
                            rk_step, dt_rk,                                        &
                            grid%ru_m,grid%a_ru_m, grid%rv_m,grid%a_rv_m, grid%ww_m,grid%a_ww_m, &
                            grid%muts,grid%a_muts, grid%mub, grid%mu_1,grid%a_mu_1,&
                            grid%alt,grid%a_alt,                                   &
                            grid%tke_1,grid%a_tke_1,                               &
                            grid%tke_2,grid%a_tke_2,                               &
                            tke_tend(ims,kms,jms),a_tke_tend(ims,kms,jms),         &
                            advect_tend,a_advect_tend,grid%rqvften,grid%a_rqvften, &
                            grid%qv_base, .false., grid%fnm, grid%fnp,             &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,     &
                            grid%msfvy, grid%msftx,grid%msfty,                     &
                            grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif,   &
                            grid%kvdif, grid%xkhh,grid%a_xkhh,                     &
                            grid%diff_6th_opt, grid%diff_6th_factor,               &
                            config_flags%tke_adv_opt,                              &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_scalar_tend tke' )
end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

         ENDDO adj_tke_tile_loop_1
         !$OMP END PARALLEL DO

        ENDIF adj_TKE_advance
BENCH_END(adj_tke_adv_tim)

!====[adj_P1.16]==========================================================
!    Adjoint of updating moist of grid points

     adj_moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

        adj_moist_variable_loop: DO im = num_3d_m, PARAM_FIRST_SCALAR, -1

           IF (grid%adv_moist_cond .or. im==p_qv ) THEN

!======[adj_P1.16.2]======================================================
!      Adjoint of updating moist of grid points
             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij )
             adj_moist_tile_loop_2: DO ij = grid%num_tiles,1,-1

!========[adj_P1.16.2.2]==================================================
!        Adjoint of updating moist of grid points in spec zone 
BENCH_START(adj_flow_depbdy_tim)
               IF( config_flags%specified ) THEN
                 IF(im .ne. P_QV)THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: flow_dep_bdy{
if (config_flags % sw_call_flow_dep_bdy_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_flow_dep_bdy ' )

                   CALL pop2restore ( grid%ru_m,grid%rv_m, "ru_m,rv_m" )
                   CALL a_flow_dep_bdy ( a_moist(ims,kms,jms,im),             &
                                         grid%ru_m, grid%rv_m, config_flags,  &
                                         grid%spec_zone,                      &
                                         ids,ide, jds,jde, kds,kde,           &
                                         ims,ime, jms,jme, kms,kme,           &
                                         ips,ipe, jps,jpe, kps,kpe,           &
                                         grid%i_start(ij), grid%i_end(ij),    &
                                         grid%j_start(ij), grid%j_end(ij),    &
                                         k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_flow_dep_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: flow_dep_bdy }

                 ENDIF
               ENDIF
BENCH_END(adj_flow_depbdy_tim)

!========[adj_P1.16.2.1]==================================================
!        Adjoint of updating moist of grid points except for those in spec zone 
               CALL wrf_debug ( 200 , ' call a_rk_update_scalar' )

BENCH_START(adj_update_scal_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar{
if (config_flags % sw_call_rk_update_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar ' )

               IF ( rk_step == 1 ) THEN
                 CALL pop2restore ( moist(:,:,:,im),moist_tend(:,:,:,im),advect_tend, &
                                    "moist,moist_tend,advect_tend" ) 
               ELSE
                 CALL pop2restore ( moist_old(:,:,:,im),moist_tend(:,:,:,im),advect_tend, &
                                    "moist_old,moist_tend,advect_tend" ) 
               END IF 
               CALL pop2restore ( grid%mu_1,grid%mu_2, "mu_1,mu_2" )
               CALL a_rk_update_scalar( im, im,                     &
                               moist_old(ims,kms,jms,im), a_moist_old(ims,kms,jms,im),        &
                               moist(ims,kms,jms,im), a_moist(ims,kms,jms,im),            &
                               moist_tend(ims,kms,jms,im), a_moist_tend(ims,kms,jms,im),       &
                               advect_tend, a_advect_tend, grid%msftx,grid%msfty, &
                               grid%mu_1, grid%a_mu_1, grid%mu_2, grid%a_mu_2, grid%mub,    &
                               rk_step, dt_rk, grid%spec_zone,    &
                               config_flags,                      &
                               ids, ide, jds, jde, kds, kde,     &
                               ims, ime, jms, jme, kms, kme,     &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar }

BENCH_END(adj_update_scal_tim)
             ENDDO adj_moist_tile_loop_2
             !$OMP END PARALLEL DO

!======[adj_P1.16.1]======================================================
!      Adjoint of calculating moist tendency of grid points 
             !$OMP PARALLEL DO   &
             !$OMP PRIVATE ( ij )
             adj_moist_tile_loop_1: DO ij = grid%num_tiles,1,-1

!========[adj_P1.16.1.2]==================================================
!        Adjoint of calculating moist tendency of grid points in relax zone and spec zone
BENCH_START(adj_rlx_bdy_scalar_tim)
               IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN
                 IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_scalar{
if (config_flags % sw_call_spec_bdy_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdy_scalar ' )

                   CALL a_spec_bdy_scalar ( a_moist_tend(ims,kms,jms,im),                   &
                                     a_moist_btxs(jms,kms,1,im),a_moist_btxe(jms,kms,1,im), &
                                     a_moist_btys(ims,kms,1,im),a_moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone,           &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),  &
                                     grid%j_start(ij), grid%j_end(ij),  &
                                     k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_scalar }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_scalar{
if (config_flags % sw_call_relax_bdy_scalar_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_relax_bdy_scalar ' )

                   CALL pop2restore (moist(:,:,:,im), "moist") 
                   CALL pop2restore (grid%mut, "mut") 
                   CALL a_relax_bdy_scalar ( a_moist_tend(ims,kms,jms,im),            &
                                     moist(ims,kms,jms,im), a_moist(ims,kms,jms,im),  &
                                     grid%mut, grid%a_mut,                            &
                                     a_moist_bxs(jms,kms,1,im),a_moist_bxe(jms,kms,1,im), &
                                     a_moist_bys(ims,kms,1,im),a_moist_bye(ims,kms,1,im), &
                                     a_moist_btxs(jms,kms,1,im),a_moist_btxe(jms,kms,1,im), &
                                     a_moist_btys(ims,kms,1,im),a_moist_btye(ims,kms,1,im), &
                                     config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                     grid%dtbc, grid%fcx, grid%gcx,             &
                                     config_flags,               &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),      &
                                     grid%j_start(ij), grid%j_end(ij),      &
                                     k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_relax_bdy_scalar ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_scalar }

                 ENDIF
               ENDIF
BENCH_END(adj_rlx_bdy_scalar_tim)
 
!========[adj_P1.16.1.1]==================================================
!        Adjoint of calculating moist tendency of grid points except for those in spec zone
               CALL wrf_debug ( 200 , ' call a_rk_scalar_tend' )

BENCH_START(adj_rk_scalar_tend_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_scalar_tend{
if (config_flags % sw_call_rk_scalar_tend_moist) then

   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_scalar_tend moist' )
               CALL pop2restore (moist(:,:,:,im),moist_old(:,:,:,im), &
                                 "moist,moist_old")
               CALL pop2restore (grid%ru_m,grid%rv_m,grid%ww_m, "ru_m,rv_m,ww_m")
               CALL pop2restore (grid%mu_1,grid%muts, "mu_1,muts")
               IF ( rk_step == 1 ) CALL pop2restore (grid%alt,grid%xkhh, "alt,xkhh")
               CALL a_rk_scalar_tend (  im, im, config_flags,                &
                           rk_step, dt_rk,                                   &
                           grid%ru_m, grid%a_ru_m, &
                           grid%rv_m, grid%a_rv_m, &
                           grid%ww_m, grid%a_ww_m, &
                           grid%muts, grid%a_muts, grid%mub, grid%mu_1, grid%a_mu_1, &
                           grid%alt, grid%a_alt,                                     &
                           moist_old(ims,kms,jms,im), a_moist_old(ims,kms,jms,im),   &
                           moist(ims,kms,jms,im), a_moist(ims,kms,jms,im),           &
                           moist_tend(ims,kms,jms,im), a_moist_tend(ims,kms,jms,im), &
                           advect_tend, a_advect_tend, grid%rqvften, grid%a_rqvften, &
                           grid%qv_base, .true., grid%fnm, grid%fnp,         &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,&
                           grid%msfvy, grid%msftx,grid%msfty,                &
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,grid%a_xkhh,                &
                           grid%diff_6th_opt, grid%diff_6th_factor,          &
                           config_flags%moist_adv_opt,                       &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

 else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_scalar_tend moist' )

   CALL a_zero_tend ( a_advect_tend(ims,kms,jms),   &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      grid%i_start(ij), grid%i_end(ij), &
                      grid%j_start(ij), grid%j_end(ij), &
                      k_start, k_end )

end if
! CHECK NECESSARY SUBROUTINE: rk_scalar_tend }

BENCH_END(adj_rk_scalar_tend_tim)

             ENDDO adj_moist_tile_loop_1
             !$OMP END PARALLEL DO

           ENDIF  !-- if (grid%adv_moist_cond .or. im==p_qv ) then

         ENDDO adj_moist_variable_loop

     ENDIF adj_moist_scalar_advance

!====[adj_P1.14]==========================================================

!====[adj_P1.14]:tke======================================================
       IF ((config_flags%tke_adv_opt /= ORIGINAL) .and. (rk_step == rk_order) &
           .and. (config_flags%km_opt .eq. 2)                ) THEN

!========[P1.14.2_adj]:tke================================================
#ifdef DM_PARALLEL
         IF (config_flags%tke_adv_opt /= ORIGINAL) THEN
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

!========[adj_P1.14.3]:tke================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

           CALL a_set_physical_bc3d(  grid%a_tke_1, 'p', config_flags,  &
                                    ids, ide, jds, jde, kds, kde,      &
                                    ims, ime, jms, jme, kms, kme,      &
                                    ips, ipe, jps, jpe, kps, kpe,      &
                                    grid%i_start(ij), grid%i_end(ij),  &
                                    grid%j_start(ij), grid%j_end(ij),  &
                                    k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

         END DO
         !$OMP END PARALLEL DO

!========[adj_P1.14.1]:tke================================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1
           CALL wrf_debug ( 200 , ' call a_rk_update_scalar_pd' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar_pd tke' )

           CALL pop2restore (grid%tke_1,tke_tend, &
                             "tke_1,tke_tend")
           CALL pop2restore (grid%mu_1, "mu_1")
           CALL a_rk_update_scalar_pd( 1, 1,                                    &
                                     grid%tke_1,grid%a_tke_1,                   &
                                     tke_tend(ims,kms,jms),a_tke_tend(ims,kms,jms),          &
                                     grid%mu_1,grid%a_mu_1, grid%mu_1,grid%a_mu_1, grid%mub, &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar_pd tke' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
         !$OMP END PARALLEL DO

       END IF  ! end if for tke_adv_opt

!====[ad_P1.14]:scalars===================================================
! scalars

     IF ((config_flags%scalar_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

!======[P1.14.2_adj]:scalar===============================================
!---------------------- positive definite bc call
#ifdef DM_PARALLEL
       IF (config_flags%scalar_adv_opt /= ORIGINAL) THEN
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
!========[adj_P1.14.3]:scalar=============================================
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = grid%num_tiles,1,-1
           IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
             DO im = num_3d_s,PARAM_FIRST_SCALAR,-1 

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

               CALL a_set_physical_bc3d(  a_scalar_old(ims,kms,jms,im), 'p', config_flags, &
                                        ids, ide, jds, jde, kds, kde,                    &
                                        ims, ime, jms, jme, kms, kme,                    &
                                        ips, ipe, jps, jpe, kps, kpe,                    &
                                        grid%i_start(ij), grid%i_end(ij),                &
                                        grid%j_start(ij), grid%j_end(ij),                &
                                        k_start    , k_end                              )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

             END DO
           ENDIF
         END DO
         !$OMP END PARALLEL DO

!======[adj_P1.14.1]:scalar===============================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1
         CALL wrf_debug ( 200 , ' call a_rk_update_scalar_pd' )
         DO im = num_3d_s,PARAM_FIRST_SCALAR,-1 

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar_pd ' )

           CALL pop2restore (scalar_old(:,:,:,im),scalar_tend(:,:,:,im), &
                             "scalar_old,scalar_tend")
           CALL pop2restore (grid%mu_1, "mu_1")
           CALL a_rk_update_scalar_pd( im, im,                                  &
                                     scalar_old(ims,kms,jms,im),a_scalar_old(ims,kms,jms,im), &
                                     scalar_tend(ims,kms,jms,im),a_scalar_tend(ims,kms,jms,im), &
                                     grid%mu_1,grid%a_mu_1, grid%mu_1,grid%a_mu_1, grid%mub, &
                                     rk_step, dt_rk, grid%spec_zone,          &
                                     config_flags,                            &
                                     ids, ide, jds, jde, kds, kde,            &
                                     ims, ime, jms, jme, kms, kme,            &
                                     grid%i_start(ij), grid%i_end(ij),        &
                                     grid%j_start(ij), grid%j_end(ij),        &
                                     k_start    , k_end                      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar_pd ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

       END IF  ! end if for scalar_adv_opt

!====[adj_P1.14]:moist====================================================
! first moisture

     IF ((config_flags%moist_adv_opt /= ORIGINAL) .and. (rk_step == rk_order)) THEN

!======[P1.14.2_adj]:moist================================================
#ifdef DM_PARALLEL
       IF (config_flags%moist_adv_opt /= ORIGINAL) THEN
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

!======[adj_P1.14.3]:moist================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1
         IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
           DO im = num_3d_m,PARAM_FIRST_SCALAR,-1

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

             CALL a_set_physical_bc3d( a_moist_old(ims,kms,jms,im), 'p', config_flags,   &
                                     ids, ide, jds, jde, kds, kde,                  &
                                     ims, ime, jms, jme, kms, kme,                  &
                                     ips, ipe, jps, jpe, kps, kpe,                  &
                                     grid%i_start(ij), grid%i_end(ij),              &
                                     grid%j_start(ij), grid%j_end(ij),              &
                                     k_start    , k_end                            )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

           END DO
         ENDIF
       END DO
       !$OMP END PARALLEL DO

!======[adj_P1.14.1]:moist================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1
         CALL wrf_debug ( 200 , ' call a_rk_update_scalar_pd' )
         DO im = num_3d_m,PARAM_FIRST_SCALAR,-1

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd{
if (config_flags % sw_call_rk_update_scalar_pd_moist) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_update_scalar_pd moist' )

           CALL pop2restore (moist_old(:,:,:,im),moist_tend(:,:,:,im), &
                             "moist_old,moist_tend")
           CALL pop2restore (grid%mu_1, "mu_1")
           CALL a_rk_update_scalar_pd( im, im,                                   &
                                     moist_old(ims,kms,jms,im),a_moist_old(ims,kms,jms,im),   &
                                     moist_tend(ims,kms,jms,im),a_moist_tend(ims,kms,jms,im), &
                                     grid%mu_1,grid%a_mu_1, grid%mu_1,grid%a_mu_1, grid%mub,  &
                                     rk_step, dt_rk, grid%spec_zone,           &
                                     config_flags,                             &
                                     ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end                       )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_update_scalar_pd moist' )
end if
! CHECK NECESSARY SUBROUTINE: rk_update_scalar_pd }

         ENDDO
       END DO
       !$OMP END PARALLEL DO

     END IF  ! end if for moist_adv_opt

!====[adj_P1.12]==========================================================
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = grid%num_tiles,1,-1

       CALL wrf_debug ( 200 , ' call a_rk_small_finish' )

!======[adj_P1.12.2]======================================================
BENCH_START(adj_small_step_finish_tim)

!======[adj_P1.12.2.2]====================================================
       IF (rk_step == rk_order) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_ru_m, 'u', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_rv_m, 'v', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_ww_m, 'w', config_flags,   &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 ips, ipe, jps, jpe, kps, kpe,      &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start    , k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

         CALL a_set_physical_bc2d( grid%a_mut, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

         CALL a_set_physical_bc2d( grid%a_muts, 't', config_flags,   &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,                &
                                 ips, ipe, jps, jpe,                &
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }

       END IF

!======[adj_P1.12.2.1]====================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: small_step_finish{
if (config_flags % sw_call_small_step_finish) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_small_step_finish ' )

        CALL pop2restore (grid%u_2,grid%u_save,grid%v_2,grid%v_save,grid%w_2,w_save,grid%t_2,grid%t_save, &
                          grid%h_diabatic, "u,u_save,v,v_save,w,w_save,t,t_save,h_diabatic") 
        CALL pop2restore (grid%mut,grid%muts,grid%muu,muus,grid%muv,muvs, "mut,muts,muu,muus,muv,muvs") 
        CALL a_small_step_finish( grid%u_2, grid%a_u_2, grid%u_1, grid%a_u_1, &
                                 grid%v_2, grid%a_v_2, grid%v_1, grid%a_v_1, &
                                 grid%w_2, grid%a_w_2, grid%w_1, grid%a_w_1, &
                                 grid%t_2, grid%a_t_2, grid%t_1, grid%a_t_1, &
                                 grid%ph_2, grid%a_ph_2, grid%ph_1, grid%a_ph_1, &
                                 grid%ww, grid%a_ww, ww1, a_ww1,    &
                                 grid%mu_2, grid%a_mu_2, grid%mu_1, grid%a_mu_1, &
                                 grid%mut, grid%a_mut, grid%muts, grid%a_muts, &
                                 grid%muu, grid%a_muu, muus, a_muus, &
                                 grid%muv, grid%a_muv, muvs, a_muvs, &
                                 grid%u_save, grid%a_u_save, grid%v_save, grid%a_v_save, w_save, a_w_save, &
                                 grid%t_save, grid%a_t_save, ph_save, a_ph_save, mu_save, a_mu_save,       &
                                 grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,grid%msfty, &
                                 grid%h_diabatic, grid%a_h_diabatic, &
                                 number_of_small_timesteps,dts_rk, &
                                 rk_step, rk_order,                &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_small_step_finish ' )
end if
! CHECK NECESSARY SUBROUTINE: small_step_finish }

BENCH_END(adj_small_step_finish_tim)

!======[adj_P1.12.1]======================================================
BENCH_START(adj_calc_mu_uv_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_mu_uv_1{
if (config_flags % sw_call_calc_mu_uv_1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_mu_uv_1 ' )

       CALL a_calc_mu_uv_1 ( config_flags,                     &
                             grid%muts, grid%a_muts, muus, a_muus, muvs, a_muvs, &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_mu_uv_1 ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_mu_uv_1 }

BENCH_END(adj_calc_mu_uv_tim)


     END DO
     !$OMP END PARALLEL DO
 
!====[adj_P1.11]==========================================================
     adj_small_steps : DO iteration = number_of_small_timesteps, 1 , -1

!======[adj_P1.11.9]======================================================
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

!!!!! Need to code a_set_physical_bc3d and a_set_physical_bc2d
BENCH_START(adj_phys_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1

       ! boundary condition set for next small timestep


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

         CALL a_set_physical_bc3d( grid%a_p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

         CALL a_set_physical_bc2d( grid%a_muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

         CALL a_set_physical_bc2d( grid%a_mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

         CALL a_set_physical_bc2d( grid%a_mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }


       END DO
       !$OMP END PARALLEL DO
BENCH_END(adj_phys_bc_tim)

!======[adj_P1.11.8]======================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1
!========[adj_P1.11.8.3]================================================
 BENCH_START(adj_cald_p_rho_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho{
if (config_flags % sw_call_calc_p_rho) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_p_rho ' )

         CALL pop2restore (grid%alt,grid%ph_2,grid%t_2,grid%t_save,c2a, "alt,ph,t_2,t_1,c2a") 
         CALL pop2restore (grid%mu_2,grid%muts, "mu,muts") 
         CALL a_calc_p_rho( grid%al, grid%a_al, grid%p, grid%a_p, grid%ph_2, grid%a_ph_2,           &
                            grid%alt, grid%a_alt, grid%t_2, grid%a_t_2, grid%t_save, grid%a_t_save, &
                            c2a, a_c2a, pm1, a_pm1, & 
                            grid%mu_2, grid%a_mu_2, grid%muts, grid%a_muts, grid%znu, t0,           &
                            grid%rdnw, grid%dnw, grid%smdiv,            &
                            config_flags%non_hydrostatic, iteration,    &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_p_rho ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho }

BENCH_END(adj_cald_p_rho_tim)

!========[adj_P1.11.8.2]==================================================
!        Adjoint of updating model variables of grid points in spec zone: ph,w
         IF( config_flags%specified .or. config_flags%nested ) THEN

BENCH_START(adj_spec_bdynhyd_tim)
           IF (config_flags%non_hydrostatic)  THEN

             IF( config_flags%specified ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_grad_bdy{
if (config_flags % sw_call_zero_grad_bdy) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_zero_grad_bdy ' )

                CALL a_zero_grad_bdy ( grid%a_w_2,                       &
                                       'w'         , config_flags,       &
                                       grid%spec_zone,                   &
                                       ids,ide, jds,jde, kds,kde,        &
                                       ims,ime, jms,jme, kms,kme,        &
                                       ips,ipe, jps,jpe, kps,kpe,        &
                                       grid%i_start(ij), grid%i_end(ij), &
                                       grid%j_start(ij), grid%j_end(ij), &
                                       k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_zero_grad_bdy ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_grad_bdy }

             ELSE

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate ' )

                CALL a_spec_bdyupdate ( grid%a_w_2,       &
                                        a_rw_tend, dts_rk, &
                                        'h'         , config_flags, &
                                        grid%spec_zone,             &
                                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                                        ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                        grid%i_start(ij), grid%i_end(ij), &
                                        grid%j_start(ij), grid%j_end(ij), &
                                        k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

             ENDIF

 ! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate_ph{
if (config_flags % sw_call_spec_bdyupdate_ph) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate_ph ' )

             CALL pop2restore (mu_tend,grid%muts, "mu_tend,muts") 
             CALL pop2restore (ph_save,grid%ph_2,ph_tend, "ph_save,ph,ph_tend") 
             CALL a_spec_bdyupdate_ph( ph_save, a_ph_save, grid%ph_2, grid%a_ph_2,       &
                                       ph_tend, a_ph_tend,              &
                                       mu_tend, a_mu_tend, grid%muts, grid%a_muts, dts_rk, &
                                       'h'         , config_flags,      &
                                       grid%spec_zone,                  &
                                       ids,ide, jds,jde, kds,kde,       &
                                       ims,ime, jms,jme, kms,kme,       &
                                       ips,ipe, jps,jpe, kps,kpe,       &
                                       grid%i_start(ij), grid%i_end(ij),&
                                       grid%j_start(ij), grid%j_end(ij),&
                                       k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate_ph ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate_ph }

           ENDIF

BENCH_END(adj_spec_bdynhyd_tim)
         ENDIF

!========[adj_P1.11.8.1]==================================================
BENCH_START(adj_sumflux_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: sumflux{
if (config_flags % sw_call_sumflux) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_sumflux ' )

         CALL pop2restore (grid%u_save,grid%v_save, "u_lin,v_lin") 
         CALL pop2restore (grid%muu,grid%muv, "muu,muv") 
         CALL a_sumflux ( grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2, grid%ww,grid%a_ww,            &
                          grid%u_save, grid%a_u_save, grid%v_save, grid%a_v_save, ww1, a_ww1,     &
                          grid%muu, grid%a_muu, grid%muv, grid%a_muv,                             &
                          grid%ru_m, grid%a_ru_m, grid%rv_m, grid%a_rv_m, grid%ww_m, grid%a_ww_m, &
                          grid%epssm,                          &
                          grid%msfux, grid% msfuy, grid%msfvx,  &
                          grid%msfvx_inv, grid%msfvy,           &
                          iteration, number_of_small_timesteps, &
                          ids, ide, jds, jde, kds, kde,         &
                          ims, ime, jms, jme, kms, kme,         &
                          grid%i_start(ij), grid%i_end(ij),     &
                          grid%j_start(ij), grid%j_end(ij),     &
                          k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_sumflux ' )
end if
! CHECK NECESSARY SUBROUTINE: sumflux }

BENCH_END(adj_sumflux_tim)

       ENDDO
       !$OMP END PARALLEL DO

!======[adj_P1.11.6]======================================================
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
        DO ij = grid%num_tiles,1,-1

!========[adj_P1.11.6.2]==================================================
!        Adjoint of updating model variables of grid points except for those in spec zone: w
BENCH_START(adj_advance_w_tim)
         IF ( config_flags%non_hydrostatic ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_w{
if (config_flags % sw_call_advance_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_w ' )

           CALL pop2restore (grid%ph_2,ph_save,ph_tend,c2a,cqw,grid%alt,a,alpha,gamma,     &
                             "ph,ph_1,ph_tend,c2a,cqw,alt,a,alpha,gamma")
           CALL pop2restore (grid%u_2,grid%v_2,grid%w_2,rw_tend,grid%ww,w_save,t_2save,grid%t_2,grid%t_save, &
                             "u,v,w,rw_tend,ww,w_save,t_2ave,t_2,t_1")
           CALL pop2restore (grid%mut,muave,grid%muts, "mut,muave,muts") 
           CALL a_advance_w( grid%w_2,grid%a_w_2, rw_tend,a_rw_tend, grid%ww,grid%a_ww, w_save,a_w_save, &
                             grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2,                              &
                             grid%mu_2,grid%a_mu_2, grid%mut,grid%a_mut,                            &
                             muave,a_muave, grid%muts,grid%a_muts,                                  &
                             t_2save,a_t_2save, grid%t_2,grid%a_t_2, grid%t_save,grid%a_t_save,     &
                             grid%ph_2,grid%a_ph_2, ph_save,a_ph_save, grid%phb, ph_tend,a_ph_tend, &
                             grid%ht, c2a,a_c2a, cqw,a_cqw, grid%alt,grid%a_alt, grid%alb,          &
                             a,a_a, alpha,a_alpha, gamma,a_gamma,                                   &
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
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_w ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_w }

         ENDIF
BENCH_END(adj_advance_w_tim)

!========[adj_P1.11.6.1]==================================================
!        Adjoint of updating model variables of grid points in spec zone: mu,t
BENCH_START(adj_spec_bdy_t_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call spec_bdyupdate ' )

           CALL a_spec_bdyupdate ( grid%a_muts,     &
                                   a_mu_tend, dts_rk, &
                                   'm'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, 1  ,1  ,        &
                                   ims,ime, jms,jme, 1  ,1  ,        &
                                   ips,ipe, jps,jpe, 1  ,1  ,        &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   1, 1 )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate ' )

           CALL a_spec_bdyupdate ( grid%a_mu_2,     &
                                   a_mu_tend, dts_rk, &
                                   'm'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, 1  ,1  ,        &
                                   ims,ime, jms,jme, 1  ,1  ,        &
                                   ips,ipe, jps,jpe, 1  ,1  ,        &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   1, 1 )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate ' )

           CALL a_spec_bdyupdate ( grid%a_t_2,       &
                                   a_t_tend, dts_rk,   &
                                   't'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

         ENDIF
BENCH_END(adj_spec_bdy_t_tim)

       ENDDO
       !$OMP END PARALLEL DO

!======[adj_P1.11.4]======================================================
!      Adjoint of updating model variables of grid points except for those in spec zone: mu,t
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
       DO ij = grid%num_tiles,1,-1

BENCH_START(adj_advance_mu_t_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_mu_t{
if (config_flags % sw_call_advance_mu_t) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_mu_t ' )

         CALL pop2restore (grid%ww,ww1,grid%u_2,grid%u_save,grid%v_2,grid%v_save,grid%t_save, &
                           "ww,ww_1,u,u_1,v,v_1,t_1") 
         CALL pop2restore (grid%muu,grid%muv,mu_tend, "muu,muv,mu_tend") 
         CALL a_advance_mu_t( grid%ww,grid%a_ww, ww1,a_ww1, &
                              grid%u_2,grid%a_u_2, grid%u_save,grid%a_u_save, &
                              grid%v_2,grid%a_v_2, grid%v_save,grid%a_v_save, &
                              grid%mu_2,grid%a_mu_2, grid%mut,grid%a_mut,     &
                              muave,a_muave, grid%muts,grid%a_muts,           &
                              grid%muu,grid%a_muu, grid%muv,grid%a_muv, grid%mudf,grid%a_mudf,     &
                              grid%ru_m,grid%a_ru_m, grid%rv_m,grid%a_rv_m, grid%ww_m,grid%a_ww_m, &
                              grid%t_2,grid%a_t_2, grid%t_save,grid%a_t_save, &
                              t_2save,a_t_2save, t_tend,a_t_tend,             &
                              mu_tend,a_mu_tend,                              &
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
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_mu_t ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_mu_t }

BENCH_END(adj_advance_mu_t_tim)
       ENDDO
       !$OMP END PARALLEL DO

!======[adj_P1.11.3]======================================================
!      Adjoint of updating model variables of grid points in spec zone: u,v
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1

BENCH_START(adj_spec_bdy_uv_tim)
         IF( config_flags%specified .or. config_flags%nested ) THEN
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate ' )

           CALL a_spec_bdyupdate ( grid%a_v_2,       &
                                   grid%a_rv_tend, dts_rk, &
                                   'v'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate{
if (config_flags % sw_call_spec_bdyupdate) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate ' )

           CALL a_spec_bdyupdate ( grid%a_u_2,       &
                                   grid%a_ru_tend, dts_rk, &
                                   'u'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate }

         ENDIF
BENCH_END(adj_spec_bdy_uv_tim)

       END DO
       !$OMP END PARALLEL DO

!======[adj_P1.11.1]======================================================
!      Adjoint of updating model variables of grid points except for those in spec zone: u,v
#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_B.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = grid%num_tiles,1,-1

BENCH_START(adj_advance_uv_tim)

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_uv{
if (config_flags % sw_call_advance_uv) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_uv ' )

         CALL pop2restore (grid%ph_2,grid%alt,grid%p,grid%al,grid%php,cqu,cqv, "ph,alt,p,al,php,cqu,cqv") 
         CALL pop2restore (grid%mu_2,grid%muu,grid%muv, "mu,muu,muv") 
         CALL a_advance_uv( grid%u_2,grid%a_u_2, grid%ru_tend,grid%a_ru_tend, &
                            grid%v_2,grid%a_v_2, grid%rv_tend,grid%a_rv_tend, &
                            grid%p,grid%a_p, grid%pb,                         &
                            grid%ph_2,grid%a_ph_2, grid%php,grid%a_php,       &
                            grid%alt,grid%a_alt, grid%al,grid%a_al,           &
                            grid%mu_2,grid%a_mu_2,                            &
                            grid%muu,grid%a_muu, cqu,a_cqu, grid%muv,grid%a_muv, cqv,a_cqv, &
                            grid%mudf,grid%a_mudf,                                 &
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

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_uv ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_uv }

BENCH_END(adj_advance_uv_tim)

       END DO
       !$OMP END PARALLEL DO

     END DO adj_small_steps

!====[adj_P1.10]==========================================================
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

BENCH_START(adj_set_phys_bc2_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = grid%num_tiles,1,-1


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_ru_tend, 'u', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_rv_tend, 'v', config_flags,      &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_ph_2, 'w', config_flags,         &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_al, 'p', config_flags,           &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_p, 'p', config_flags,            &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_t_1, 'p', config_flags,          &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_t_save, 't', config_flags,       &
                               ids, ide, jds, jde, kds, kde,         &
                               ims, ime, jms, jme, kms, kme,         &
                               ips, ipe, jps, jpe, kps, kpe,         &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij),     &
                               k_start    , k_end                    )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

       CALL a_set_physical_bc2d( grid%a_mu_1, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

       CALL a_set_physical_bc2d( grid%a_mu_2, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }



! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc2d{
if (config_flags % sw_call_set_physical_bc2d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc2d ' )

       CALL a_set_physical_bc2d( grid%a_mudf, 't', config_flags,         &
                               ids, ide, jds, jde,                   &
                               ims, ime, jms, jme,                   &
                               ips, ipe, jps, jpe,                   &
                               grid%i_start(ij), grid%i_end(ij),     &
                               grid%j_start(ij), grid%j_end(ij)      )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc2d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc2d }


     END DO
     !$OMP END PARALLEL DO
BENCH_END(adj_set_phys_bc2_tim)

!====[adj_P1.9]===========================================================
BENCH_START(adj_small_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = grid%num_tiles,1,-1

!======[adj_P1.9.3]=======================================================
       IF (config_flags%non_hydrostatic) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_coef_w{
if (config_flags % sw_call_calc_coef_w) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_coef_w ' )

         CALL pop2restore (grid%mut, "mut") 
         CALL pop2restore (c2a,cqw, "c2a,cqw") 
         CALL a_calc_coef_w( a,a_a,alpha,a_alpha,gamma,a_gamma, &
                             grid%mut,grid%a_mut, cqw,a_cqw,    &
                             grid%rdn, grid%rdnw, c2a,a_c2a,    &
                             dts_rk, g, grid%epssm,            &
                             config_flags%top_lid,             &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_coef_w ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_coef_w }

       ENDIF

!======[adj_P1.9.2]=======================================================
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: calc_p_rho{
if (config_flags % sw_call_calc_p_rho) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_calc_p_rho ' )

       CALL pop2restore (grid%alt,grid%ph_2,grid%t_2,grid%t_save,c2a, "alt,ph,t_2,t_1,c2a") 
       CALL pop2restore (grid%mu_2,grid%muts, "mu,muts") 
       CALL a_calc_p_rho( grid%al,grid%a_al, grid%p,grid%a_p, grid%ph_2,grid%a_ph_2, &
                          grid%alt,grid%a_alt, grid%t_2,grid%a_t_2,   &
                          grid%t_save,grid%a_t_save, c2a,a_c2a, pm1,a_pm1,  &
                          grid%mu_2,grid%a_mu_2, grid%muts,grid%a_muts, grid%znu, t0, & 
                          grid%rdnw, grid%dnw, grid%smdiv,            &
                          config_flags%non_hydrostatic, 0,            &
                          ids, ide, jds, jde, kds, kde,               &
                          ims, ime, jms, jme, kms, kme,               &
                          grid%i_start(ij), grid%i_end(ij),           &
                          grid%j_start(ij), grid%j_end(ij),           &
                          k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_calc_p_rho ' )
end if
! CHECK NECESSARY SUBROUTINE: calc_p_rho }

!======[adj_P1.9.1]=======================================================
       CALL wrf_debug ( 200 , ' call a_small_step_prep ' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: small_step_prep{
if (config_flags % sw_call_small_step_prep) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_small_step_prep ' )

       IF ( rk_step == 1 ) THEN
         CALL pop2restore (grid%u_2,grid%v_2,grid%t_2,grid%w_2,grid%p,grid%alt, &
                           "u_2,v_2,t_2,w_2,p,alt") 
         CALL pop2restore (grid%mu_2, "mu_2")
       ELSE
         CALL pop2restore (grid%u_1,grid%v_1,grid%t_1,grid%w_1, &
                           grid%u_2,grid%v_2,grid%t_2,grid%w_2,grid%p,grid%alt, &
                           "u_1,v_1,t_1,w_1,u_2,v_2,t_2,w_2,p,alt") 
         CALL pop2restore (grid%mu_1, "mu_1")
       END IF 
       CALL pop2restore (grid%muu,grid%muv,grid%mut, "muu,muv,mut") 
       CALL a_small_step_prep( grid%u_1,grid%a_u_1,grid%u_2,grid%a_u_2, &
                               grid%v_1,grid%a_v_1,grid%v_2,grid%a_v_2, &
                               grid%w_1,grid%a_w_1,grid%w_2,grid%a_w_2, &
                               grid%t_1,grid%a_t_1,grid%t_2,grid%a_t_2, &
                               grid%ph_1,grid%a_ph_1,grid%ph_2,grid%a_ph_2, &
                               grid%mub, grid%mu_1,grid%a_mu_1, grid%mu_2,grid%a_mu_2,  &
                               grid%muu,grid%a_muu, muus,a_muus, grid%muv,grid%a_muv, muvs,a_muvs, &
                               grid%mut,grid%a_mut, grid%muts,grid%a_muts, grid%mudf,grid%a_mudf,  &
                               grid%u_save,grid%a_u_save, grid%v_save,grid%a_v_save, w_save,a_w_save, &
                               grid%t_save,grid%a_t_save, ph_save,a_ph_save, mu_save,a_mu_save,       &
                               grid%ww,grid%a_ww, ww1,a_ww1,                                          &
                               grid%dnw, c2a,a_c2a, grid%pb, grid%p,grid%a_p, grid%alt,grid%a_alt,    &
                               grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,       &
                               grid%msfvy, grid%msftx,grid%msfty,                       &
                               grid%rdx, grid%rdy, rk_step,                             &
                               ids, ide, jds, jde, kds, kde,                            &
                               ims, ime, jms, jme, kms, kme,                            &
                               grid%i_start(ij), grid%i_end(ij),                        &
                               grid%j_start(ij), grid%j_end(ij),                        &
                               k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_small_step_prep ' )
end if
! CHECK NECESSARY SUBROUTINE: small_step_prep }

     ENDDO
     !$OMP END PARALLEL DO
BENCH_END(adj_small_step_prep_tim)

!    [adj_P5_debug_bdy] Adjoint of updating model variables of grid points
!    ---------------------------------------------------------------------
     adj_update_model_variables_debug_bdy: &
     DO ij = 1 , grid%num_tiles

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_all{
if (config_flags % sw_call_advance_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_all ' )

          CALL a_restore_ph_mu  ( grid%a_ph_2, a_ph_save, &
                                  grid%a_mu_2, a_mu_save, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end )
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_all ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_all }

!    [adj_P5.2_debug_bdy] Adjoint of updating model variables of grid points in spec zone 
!    ------------------------------------------------------------------------------------

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdyupdate_all{
if (config_flags % sw_call_spec_bdyupdate_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdyupdate_all ' )

     IF( config_flags%specified .or. config_flags%nested ) THEN
       IF (config_flags%non_hydrostatic) THEN
         IF( config_flags%specified ) THEN
           CALL a_zero_grad_bdy ( grid%a_w_2,                       &
                                  'w'         , config_flags,       &
                                  grid%spec_zone,                   &
                                  ids,ide, jds,jde, kds,kde,        &
                                  ims,ime, jms,jme, kms,kme,        &
                                  ips,ipe, jps,jpe, kps,kpe,        &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end )
         ELSE
           CALL pop2restore (grid%w_2, "w") 
           CALL pop2restore (grid%mut, "mut") 
           CALL a_uncouple_bdy ( grid%w_2, grid%a_w_2,       &
                                 'w'         , config_flags, &
                                 grid%spec_zone,             &
                                 grid%mut, grid%a_mut, grid%msfty,  & 
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),  &
                                 grid%j_start(ij), grid%j_end(ij),  &
                                 k_start, k_end )
           CALL a_spec_bdyupdate ( grid%a_w_2,       &
                                   a_rw_tend, dts_rk, &
                                   'h'         , config_flags, &
                                   grid%spec_zone,             &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end )
           CALL pop2restore (grid%w_2, "w") 
           CALL a_couple_bdy ( grid%w_2, grid%a_w_2,       &
                               'w'         , config_flags, &
                               grid%spec_zone,             &
                               grid%mut, grid%a_mut, grid%msfty,  & 
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),  &
                               grid%j_start(ij), grid%j_end(ij),  &
                               k_start, k_end )
         ENDIF

         CALL pop2restore (grid%ph_2,ph_tend,ph_save, "field_ph,field_ph_tend,ph_save") 
         CALL pop2restore (grid%mut,mu_tend, "mut,mu_tend") 
         CALL a_spec_bdyupdate_ph( ph_save, a_ph_save, grid%ph_2, grid%a_ph_2,       &
                                   ph_tend, a_ph_tend,              &
                                   mu_tend, a_mu_tend, grid%mut, grid%a_mut, dts_rk, &
                                   'h'         , config_flags,      &
                                   grid%spec_zone,                  &
                                   ids,ide, jds,jde, kds,kde,       &
                                   ims,ime, jms,jme, kms,kme,       &
                                   ips,ipe, jps,jpe, kps,kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),&
                                   grid%j_start(ij), grid%j_end(ij),&
                                   k_start, k_end )

       ENDIF

       CALL a_spec_bdyupdate ( grid%a_mu_2,     &
                               a_mu_tend, dts_rk, &
                               'm'         , config_flags, &
                               grid%spec_zone,             &
                               ids,ide, jds,jde, 1  ,1  ,        &
                               ims,ime, jms,jme, 1  ,1  ,        &
                               ips,ipe, jps,jpe, 1  ,1  ,        &
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               1, 1 )

       CALL pop2restore (grid%t_2, "t") 
       CALL pop2restore (grid%mut, "mut") 
       CALL a_uncouple_bdy ( grid%t_2, grid%a_t_2,       &
                             't'         , config_flags, &
                             grid%spec_zone,             &
                             grid%mut, grid%a_mut, grid%msfty,  & 
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end )

       CALL a_spec_bdyupdate ( grid%a_t_2,       &
                               a_t_tend, dts_rk,   &
                               't'         , config_flags, &
                               grid%spec_zone,             &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start, k_end )

       CALL pop2restore (grid%t_2, "t") 
       CALL a_couple_bdy ( grid%t_2, grid%a_t_2,       &
                           't'         , config_flags, &
                           grid%spec_zone,             &
                           grid%mut, grid%a_mut, grid%msfty,  & 
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start, k_end )

       CALL pop2restore (grid%v_2, "v") 
       CALL pop2restore (grid%muv, "muv") 
       CALL a_uncouple_bdy ( grid%v_2, grid%a_v_2,       &
                             'v'         , config_flags, &
                             grid%spec_zone,             &
                             grid%muv, grid%a_muv, grid%msfvx,  & 
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end )

       CALL a_spec_bdyupdate ( grid%a_v_2,       &
                               grid%a_rv_tend, dts_rk, &
                               'v'         , config_flags, &
                               grid%spec_zone,             &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start, k_end )

       CALL pop2restore (grid%v_2, "v") 
       CALL a_couple_bdy ( grid%v_2, grid%a_v_2,       &
                           'v'         , config_flags, &
                           grid%spec_zone,             &
                           grid%muv, grid%a_muv, grid%msfvx,  & 
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start, k_end )

       CALL pop2restore (grid%u_2, "u") 
       CALL pop2restore (grid%muu, "muu") 
       CALL a_uncouple_bdy ( grid%u_2, grid%a_u_2,       &
                             'u'         , config_flags, &
                             grid%spec_zone,             &
                             grid%muu, grid%a_muu, grid%msfuy,  & 
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             grid%i_start(ij), grid%i_end(ij),  &
                             grid%j_start(ij), grid%j_end(ij),  &
                             k_start, k_end )

       CALL a_spec_bdyupdate ( grid%a_u_2,       &
                               grid%a_ru_tend, dts_rk, &
                               'u'         , config_flags, &
                               grid%spec_zone,             &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij), &
                               grid%j_start(ij), grid%j_end(ij), &
                               k_start, k_end )

       CALL pop2restore (grid%u_2, "u") 
       CALL a_couple_bdy ( grid%u_2, grid%a_u_2,       &
                           'u'         , config_flags, &
                           grid%spec_zone,             &
                           grid%muu, grid%a_muu, grid%msfuy,  & 
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start, k_end )

     ENDIF
else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdyupdate_all ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdyupdate_all }

!    [adj_P5.1_debug_bdy] Adjoint of updating model variables of grid points export for those in spec zone 
!    -----------------------------------------------------------------------------------------------------
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: advance_all{
if (config_flags % sw_call_advance_all) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_advance_all ' )

     CALL pop2restore (grid%ru_tend,grid%rv_tend,rw_tend,t_tend,ph_tend, &
                       "ru_tend,rv_tend,rw_tend,t_tend,ph_tend") 
     CALL pop2restore (grid%muu,grid%muv,grid%mut, "muu,muv,mut") 
     CALL a_advance_all ( grid%a_u_2, grid%ru_tend, grid%a_ru_tend, &
                          grid%a_v_2, grid%rv_tend, grid%a_rv_tend, &
                          grid%a_w_2,      rw_tend,      a_rw_tend, &
                          grid%a_t_2,       t_tend,       a_t_tend, &
                          grid%a_mu_2,                   a_mu_tend, &
                          grid%a_ph_2,     ph_tend,      a_ph_tend, &
                          grid%muu, grid%a_muu, grid%muv, grid%a_muv, &
                          grid%mut, grid%a_mut,                       &
                          grid%msfuy, grid%msfvx, grid%msfty,         &
                          dts_rk,                       &
                          config_flags,grid%spec_zone,  &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start , k_end )

     CALL a_save_ph_mu ( grid%a_ph_1, grid%a_ph_2, a_ph_save,  &
                         grid%a_mu_1, grid%a_mu_2, a_mu_save,  &
                         rk_step,                           &
                         ids, ide, jds, jde, kds, kde,      &
                         ims, ime, jms, jme, kms, kme,      &
                         grid%i_start(ij), grid%i_end(ij),  &
                         grid%j_start(ij), grid%j_end(ij),  &
                         k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_advance_all ' )
end if
! CHECK NECESSARY SUBROUTINE: advance_all }

     END DO adj_update_model_variables_debug_bdy

!====[adj_P1.8]===========================================================
!    Adjoint of calculating tendencies of grid points
BENCH_START(adj_relax_bdy_dry_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     adj_calc_tendency: &
     DO ij = grid%num_tiles,1,-1

!======[adj_P1.8.3]=======================================================
!      Adjoint of calculating tendencies of grid points in spec zone
     IF( config_flags%specified .or. config_flags%nested ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: spec_bdy_dry{
if (config_flags % sw_call_spec_bdy_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_spec_bdy_dry ' )

       CALL a_spec_bdy_dry ( config_flags,                                     &
                   grid%a_ru_tend, grid%a_rv_tend, &
                   a_ph_tend, a_t_tend,            &
                   a_rw_tend, a_mu_tend,           &
                   grid%a_u_btxs, grid%a_u_btxe, grid%a_u_btys, grid%a_u_btye, &
                   grid%a_v_btxs, grid%a_v_btxe, grid%a_v_btys, grid%a_v_btye, &
                   grid%a_ph_btxs, grid%a_ph_btxe, grid%a_ph_btys, grid%a_ph_btye, &
                   grid%a_t_btxs, grid%a_t_btxe, grid%a_t_btys, grid%a_t_btye, &
                   grid%a_w_btxs, grid%a_w_btxe, grid%a_w_btys, grid%a_w_btye, &
                   grid%a_mu_btxs, grid%a_mu_btxe, grid%a_mu_btys, grid%a_mu_btye, &
                   config_flags%spec_bdy_width, grid%spec_zone,                &
                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                   grid%i_start(ij), grid%i_end(ij),  &
                   grid%j_start(ij), grid%j_end(ij),  &
                   k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_spec_bdy_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: spec_bdy_dry }
     ENDIF

!======[adj_P1.8.2]=======================================================
!      Adjoint of adding tendencies of grid points except for those in spec zone 
! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_addtend_dry{
if (config_flags % sw_call_rk_addtend_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_addtend_dry ' )

     CALL pop2restore (grid%h_diabatic, "h_diabatic") 
     CALL pop2restore (grid%mut, "mut") 
     CALL a_rk_addtend_dry ( grid%a_ru_tend,  &
                             grid%a_rv_tend,  &
                  a_rw_tend, a_ph_tend, a_t_tend,         &
                  a_ru_tendf, a_rv_tendf, a_rw_tendf, &
                  a_ph_tendf, a_t_tendf,                         &
                  grid%a_u_save, grid%a_v_save,           &
                  a_w_save, a_ph_save, grid%a_t_save, &
                  a_mu_tend, a_mu_tendf, rk_step,        &
                  grid%h_diabatic, grid%g_h_diabatic, grid%mut, grid%a_mut, grid%msftx,        &
                  grid%msfty, grid%msfux,grid%msfuy,               &
                  grid%msfvx, grid%msfvx_inv, grid%msfvy,          &
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  ips,ipe, jps,jpe, kps,kpe,                       &
                  grid%i_start(ij), grid%i_end(ij),                &
                  grid%j_start(ij), grid%j_end(ij),                &
                  k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_addtend_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_addtend_dry }

!======[adj_P1.8.1]=======================================================
!      Adjoint of calculating tendencies for grid points in relax zone
     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: relax_bdy_dry{
if (config_flags % sw_call_relax_bdy_dry) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_relax_bdy_dry ' )

       CALL pop2restore (grid%ph_2,grid%t_2,grid%w_2, "ph,t,w") 
       CALL pop2restore (grid%mut, "mut") 
       CALL a_relax_bdy_dry ( config_flags,                             &
                grid%a_u_save, grid%a_v_save, &
                a_ph_save, grid%a_t_save,  &
                a_w_save, a_mu_tend,            &
                grid%a_ru, grid%a_rv,          &
                grid%ph_2, grid%a_ph_2, grid%t_2, grid%a_t_2,    &
                grid%w_2, grid%a_w_2, grid%a_mu_2, grid%mut, grid%a_mut, &
                grid%a_u_bxs, grid%a_u_bxe, &
                grid%a_u_bys, grid%a_u_bye, &
                grid%a_v_bxs, grid%a_v_bxe, &
                grid%a_v_bys, grid%a_v_bye, &
                grid%a_ph_bxs,grid%a_ph_bxe, &
                grid%a_ph_bys,grid%a_ph_bye, &
                grid%a_t_bxs, grid%a_t_bxe, &
                grid%a_t_bys, grid%a_t_bye, &
                grid%a_w_bxs, grid%a_w_bxe, &
                grid%a_w_bys, grid%a_w_bye, &
                grid%a_mu_bxs,grid%a_mu_bxe, &
                grid%a_mu_bys,grid%a_mu_bye, &
                grid%a_u_btxs, grid%a_u_btxe, &
                grid%a_u_btys, grid%a_u_btye, &
                grid%a_v_btxs, grid%a_v_btxe, &
                grid%a_v_btys, grid%a_v_btye, &
                grid%a_ph_btxs,grid%a_ph_btxe, &
                grid%a_ph_btys,grid%a_ph_btye, &
                grid%a_t_btxs, grid%a_t_btxe, &
                grid%a_t_btys, grid%a_t_btye, &
                grid%a_w_btxs, grid%a_w_btxe, &
                grid%a_w_btys, grid%a_w_btye, &
                grid%a_mu_btxs,grid%a_mu_btxe, &
                grid%a_mu_btys,grid%a_mu_btye, &
                config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                grid%dtbc, grid%fcx, grid%gcx,      &
                ids,ide, jds,jde, kds,kde,          &
                ims,ime, jms,jme, kms,kme,          &
                ips,ipe, jps,jpe, kps,kpe,          &
                grid%i_start(ij), grid%i_end(ij),   &
                grid%j_start(ij), grid%j_end(ij),   &
                k_start, k_end )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_relax_bdy_dry ' )
end if
! CHECK NECESSARY SUBROUTINE: relax_bdy_dry }

     ENDIF

     END DO adj_calc_tendency
     !$OMP END PARALLEL DO
BENCH_END(adj_relax_bdy_dry_tim)
  
!====[adj_P1.6]===========================================================
!    Adjoint of calculating tendencies of grid points except for those in spec zone
!    ---------------------------------------------------------------------------------------
BENCH_START(adj_rk_tend_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     adj_zero_tendency: &
     DO ij = grid%num_tiles,1,-1

       CALL wrf_debug ( 200 , ' call a_rk_tendency' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_tendency{
if (config_flags % sw_call_rk_tendency) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_tendency ' )

       CALL pop2restore (grid%xkmh,grid%xkhh, "xkmh,xkhh") 
       CALL pop2restore (grid%al,grid%alt,grid%p,grid%php,cqu,cqv,cqw, &
                         "al,alt,p,php,cqu,cqv,cqw") 
       CALL pop2restore (grid%u_2,grid%v_2,grid%w_2,grid%t_2,grid%ph_2, "u,v,w,t,ph") 
       CALL pop2restore (grid%ru,grid%rv,grid%rw,grid%ww, "ru,rv,rw,ww") 
       CALL pop2restore (grid%mu_2,grid%muu,grid%muv,grid%mut, "mu,muu,muv,mut") 
       CALL a_rk_tendency ( config_flags, rk_step, &
                         grid%ru_tend, grid%a_ru_tend, grid%rv_tend, grid%a_rv_tend, &
                         rw_tend, a_rw_tend, ph_tend, a_ph_tend, t_tend, a_t_tend, &
                         ru_tendf, a_ru_tendf, rv_tendf, a_rv_tendf, &
                         rw_tendf, a_rw_tendf, ph_tendf, a_ph_tendf, t_tendf, a_t_tendf, &
                         mu_tend, a_mu_tend, &
                         grid%u_save, grid%a_u_save, grid%v_save, grid%a_v_save, &
                         w_save, a_w_save, ph_save, a_ph_save, &
                         grid%t_save, grid%a_t_save, mu_save, a_mu_save, &
                         grid%rthften, grid%a_rthften,    &
                         grid%ru, grid%a_ru, grid%rv, grid%a_rv, grid%rw, grid%a_rw, grid%ww, grid%a_ww, &
                         grid%u_2, grid%a_u_2, grid%v_2, grid%a_v_2, grid%w_2, grid%a_w_2, &
                         grid%t_2, grid%a_t_2, grid%ph_2, grid%a_ph_2, &
                         grid%u_1, grid%a_u_1, grid%v_1, grid%a_v_1, grid%w_1, grid%a_w_1, &
                         grid%t_1, grid%a_t_1, grid%ph_1, grid%a_ph_1, &
                         grid%h_diabatic, grid%a_h_diabatic, grid%phb, grid%t_init, &
                         grid%mu_2, grid%a_mu_2, grid%mut, grid%a_mut, grid%muu, grid%a_muu, &
                         grid%muv, grid%a_muv, grid%mub, &
                         grid%al, grid%a_al, grid%alt, grid%a_alt, grid%p, grid%a_p, grid%pb, &
                         grid%php, grid%a_php, cqu, a_cqu, cqv, a_cqv, cqw, a_cqw, &
                         grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base, &
                         grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                         grid%msfvy, grid%msftx,grid%msfty, grid%xlat, grid%f, grid%e, grid%sina, grid%cosa, &
                         grid%fnm, grid%fnp, grid%rdn, grid%rdnw,             &
                         grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, &
                         grid%xkmh,grid%a_xkmh, grid%xkhh,grid%a_xkhh,        &
                         grid%diff_6th_opt, grid%diff_6th_factor,             &
                         grid%dampcoef,grid%zdamp,config_flags%damp_opt,      &
                         grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m, &
                         config_flags%non_hydrostatic, config_flags%top_lid,  &
                         grid%u_frame, grid%v_frame,     &
                         ids, ide, jds, jde, kds, kde,   &
                         ims, ime, jms, jme, kms, kme,   &
                         grid%i_start(ij), grid%i_end(ij),  &
                         grid%j_start(ij), grid%j_end(ij),  &
                         k_start, k_end,   &
                         max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij) )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_tendency ' )

! NPAN 05/09/10 + zero_tend (module_big_step_utilities_em), which is called by rk_tendency
       CALL a_zero_tend ( a_mu_save,       &
                          ids, ide, jds, jde, 1, 1, &
                          ims, ime, jms, jme, 1, 1, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          1, 1 )

       CALL a_zero_tend ( a_mu_tend,       &
                          ids, ide, jds, jde, 1, 1, &
                          ims, ime, jms, jme, 1, 1, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          1, 1 )

       CALL a_zero_tend ( grid%a_t_save,   &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( a_ph_save,           &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( a_w_save,             &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( grid%a_v_save,   &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( grid%a_u_save,   &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( a_ph_tend,           &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( a_t_tend,             &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( a_rw_tend,           &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( grid%a_rv_tend, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

       CALL a_zero_tend ( grid%a_ru_tend, &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end )

end if
! CHECK NECESSARY SUBROUTINE: rk_tendency }

     END DO adj_zero_tendency
     !$OMP END PARALLEL DO
BENCH_END(adj_rk_tend_tim)

!====[adj_P1.5]===========================================================
!    Adjoint of initializing diffusion tendencies of grid points 
     adj_rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: first_rk_step_part2{
if (config_flags % sw_call_first_rk_step_part2) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_first_rk_step_part2 ' )

       CALL pop2restore (scalar, "scalar") 
!!!!! USE THE STATEMENTS REMARKED WHEN chem and tracer ARE NEEDED. Ning Pan, 2010-08-20
!       CALL pop2restore (tracer, "tracer") 
!       CALL pop2restore (chem, "chem") 
       CALL pop2restore (moist, "moist") 
       CALL pop2restore (rho,t_phy,p_phy,th_phy,T8w,p8w, "rho,t_phy,p_phy,th_phy,T8w,p8w") 
       CALL pop2restore (grid%u_2,grid%v_2,grid%w_2,grid%t_2,grid%ph_2,grid%tke_2, &
                         "u,v,w,t,ph,tke") 
       CALL pop2restore (grid%mut,grid%hfx,grid%qfx,grid%ustm, "mut,hfx,qfx,ustm") 
       CALL a_first_rk_step_part2 (   grid , config_flags   &
              , moist ,a_moist , moist_tend ,a_moist_tend   &
!!!!! USE THE STATEMENTS REMARKED WHEN chem and tracer ARE NEEDED. Ning Pan, 2010-08-20
!              , chem  ,a_chem  , chem_tend  ,a_chem_tend    &
!              , tracer,a_tracer, tracer_tend,a_tracer_tend  &
!!!!! REMOVE THE FOLLOWING 2 STATEMENTS WHEN chem and tracer ARE NEEDED. Ning Pan, 2010-08-20
              , chem  ,  chem  , chem_tend  ,  chem_tend    &
              , tracer,  tracer, tracer_tend,  tracer_tend  &
              , scalar,a_scalar, scalar_tend,a_scalar_tend  &
!!!!! USE THE STATEMENT REMARKED WHEN fdda3d and fdda2d ARE NEEDED. Ning Pan, 2010-08-20
!              , fdda3d,a_fdda3d, fdda2d,     a_fdda2d       &
!!!!! REMOVE THE FOLLOWING STATEMENT WHEN fdda3d and fdda2d ARE NEEDED. Ning Pan, 2010-08-20
              , fdda3d,  fdda3d, fdda2d,       fdda2d       &
              , ru_tendf,a_ru_tendf, rv_tendf,a_rv_tendf    &
              , rw_tendf,a_rw_tendf, t_tendf ,a_t_tendf     &
              , ph_tendf,a_ph_tendf, mu_tendf,a_mu_tendf    &
              , tke_tend,a_tke_tend              &
              , adapt_step_flag , curr_secs      &
!!!!! USE THE STATEMENTS REMARKED WHEN CODING AD OF PHYSICS. Ning Pan, 2010-08-20
!              , psim ,a_psim , psih ,a_psih , wspd ,a_wspd ,        &
!                gz1oz0 ,a_gz1oz0 , br ,a_br , chklowq,a_chklowq     &
!              , cu_act_flag , hol ,a_hol, th_phy,a_th_phy           &
!!!!! REMOVE THE FOLLOWING 3 STATEMENTS WHEN CODING AD OF PHYSICS. Ning Pan, 2010-08-20
              , psim ,  psim , psih ,  psih , wspd ,  wspd ,        &
                gz1oz0 ,  gz1oz0 , br ,  br , chklowq,  chklowq     &
              , cu_act_flag , hol ,  hol, th_phy,a_th_phy           &
              , pi_phy ,a_pi_phy, p_phy ,a_p_phy , t_phy ,a_t_phy , &
                u_phy ,a_u_phy , v_phy,a_v_phy                      &
              , dz8w ,a_dz8w , p8w ,a_p8w , t8w ,a_t8w,             &
                rho_phy ,a_rho_phy , rho,a_rho   &
              , nba_mij,a_nba_mij, num_nba_mij   &
              , nba_rij,a_nba_rij, num_nba_rij   &
              , ids, ide, jds, jde, kds, kde     &
              , ims, ime, jms, jme, kms, kme     &
              , ips, ipe, jps, jpe, kps, kpe     &
              , k_start , k_end                  &
             )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_first_rk_step_part2 ' )
end if
! CHECK NECESSARY SUBROUTINE: first_rk_step_part2 }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: first_rk_step_part1{
if (config_flags % sw_call_first_rk_step_part1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_first_rk_step_part1 ' )

!!!!! REPLACE WITH a_first_rk_step_part1 WHEN CONSTRUCTING AD OF PHYSICS. Ning Pan
       CALL first_rk_step_part1 (    grid, config_flags         &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0      &
                             , br , chklowq                     &
                             , cu_act_flag , hol , th_phy       &
                             , pi_phy , p_phy , t_phy           &
                             , u_phy , v_phy                    &
                             , dz8w , p8w , t8w , rho_phy , rho &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx, imex, jmsx, jmex, kmsx, kmex    &
                             , ipsx, ipex, jpsx, jpex, kpsx, kpex    &
                             , imsy, imey, jmsy, jmey, kmsy, kmey    &
                             , ipsy, ipey, jpsy, jpey, kpsy, kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                            )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_first_rk_step_part1 ' )

! NPAN 08/30/10 + phy_prep (module_big_step_utilities_em), which is called by first_rk_step_part1
#ifdef DM_PARALLEL
#     include "HALO_EM_PHYS_A.inc"
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = grid%num_tiles,1,-1

        CALL wrf_debug ( 200 , ' call a_phy_prep' )
        CALL a_phy_prep ( config_flags,                           &
                        grid%mut,grid%a_mut, grid%muu,grid%a_muu, grid%muv,grid%a_muv, &
                        grid%u_2,grid%a_u_2, grid%v_2,grid%a_v_2,             &
                        grid%p,grid%a_p, grid%pb, grid%alt,grid%a_alt,        &
                        grid%ph_2,grid%a_ph_2, grid%phb, grid%t_2,grid%a_t_2, &
                        grid%tsk,grid%tsk, moist,a_moist, num_moist,        &
                        rho,a_rho, th_phy,a_th_phy, p_phy,a_p_phy, pi_phy,a_pi_phy, &
                        u_phy,a_u_phy, v_phy,a_v_phy,                         &
                        p8w,a_p8w, t_phy,a_t_phy, t8w,a_t8w, grid%z,grid%a_z, &
                        grid%z_at_w,grid%a_z_at_w, dz8w,a_dz8w,               &
                        grid%p_hyd,grid%a_p_hyd, grid%p_hyd_w,grid%a_p_hyd_w, &
                        grid%fnm, grid%fnp, grid%znw, grid%p_top,             &
                        grid%rthraten,                                        &
                        grid%rthblten, grid%rublten, grid%rvblten,            &
                        grid%rqvblten, grid%rqcblten, grid%rqiblten,          &
                        grid%rthcuten, grid%rqvcuten, grid%rqccuten,          &
                        grid%rqrcuten, grid%rqicuten, grid%rqscuten,          &
                        grid%rthften,  grid%rqvften,                          &
                        grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN,      &
                        grid%RPHNDGDTEN,grid%RQVNDGDTEN, grid%RMUNDGDTEN,     &
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        grid%i_start(ij), grid%i_end(ij),       &
                        grid%j_start(ij), grid%j_end(ij),       &
                        k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO
 
! NPAN 05/09/10 + init_zero_tendency (module_em), which is called by first_rk_step_part1
BENCH_START(adj_init_zero_tend_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       adj_init_zero_tendency: &
       DO ij = grid%num_tiles,1,-1

         CALL wrf_debug ( 200 , ' call a_init_zero_tendency' )
         CALL a_init_zero_tendency ( a_ru_tendf, &
                                     a_rv_tendf, &
                                     a_rw_tendf, &
                                     a_ph_tendf, &
                                     a_t_tendf,  &
                                     a_tke_tend, &
                                     a_mu_tendf, &
                                     a_moist_tend,  &
!  NPan - 05/26/10 {
!  Uncomment the corresponding args when chem or tracer is needed.   
!                                     a_chem_tend,   &
                                     a_scalar_tend, &
!                                     a_tracer_tend, &
!  NPan }
                                     num_tracer,           &
                                     num_moist,num_chem,num_scalar,    &
                                     rk_step,                          &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     grid%i_start(ij), grid%i_end(ij), &
                                     grid%j_start(ij), grid%j_end(ij), &
                                     k_start, k_end                   )

       END DO adj_init_zero_tendency
       !$OMP END PARALLEL DO
BENCH_END(adj_init_zero_tend_tim)

 
end if
! CHECK NECESSARY SUBROUTINE: first_rk_step_part1 }

     END IF adj_rk_step_is_one

!====[adj_P1.4]===========================================================
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

BENCH_START(adj_set_phys_bc_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, ii, jj, kk )

     DO ij = grid%num_tiles,1,-1

       CALL wrf_debug ( 200 , ' call a_rk_phys_bc_dry_1' )

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_ph_2, 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              grid%i_start(ij), grid%i_end(ij),        &
                              grid%j_start(ij), grid%j_end(ij),        &
                              k_start, k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: set_physical_bc3d{
if (config_flags % sw_call_set_physical_bc3d) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_set_physical_bc3d ' )

       CALL a_set_physical_bc3d( grid%a_al, 'p', config_flags,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_set_physical_bc3d ' )
end if
! CHECK NECESSARY SUBROUTINE: set_physical_bc3d }

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_1{
if (config_flags % sw_call_rk_phys_bc_dry_1) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_phys_bc_dry_1 ' )

       CALL a_rk_phys_bc_dry_1( config_flags, grid%ru,grid%a_ru, grid%rv,grid%a_rv, &
                              grid%rw,grid%a_rw, grid%ww,grid%a_ww,                 &
                              grid%muu,grid%a_muu, grid%muv,grid%a_muv, grid%mut,grid%a_mut, &
                              grid%php,grid%a_php, grid%alt,grid%a_alt, grid%p,grid%a_p,     &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              ips, ipe, jps, jpe, kps, kpe,      &
                              grid%i_start(ij), grid%i_end(ij),  &
                              grid%j_start(ij), grid%j_end(ij),  &
                              k_start, k_end                )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_phys_bc_dry_1 ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_phys_bc_dry_1 }

     END DO
     !$OMP END PARALLEL DO
BENCH_END(adj_set_phys_bc_tim)

!====[adj_P1.3]===========================================================
     CALL wrf_debug ( 200 , ' call a_rk_step_prep ' )

BENCH_START(adj_step_prep_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     adj_rk_step_prep: &
     DO ij = grid%num_tiles,1,-1

! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: rk_step_prep{
if (config_flags % sw_call_rk_step_prep) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_rk_step_prep ' )

       CALL pop2restore (moist, "moist")
       CALL pop2restore (grid%u_2,grid%v_2,grid%w_2, "u,v,w")
       CALL pop2restore (grid%mu_2, "mu")
       CALL a_rk_step_prep  ( config_flags, rk_step,                        &
                              grid%u_2, grid%a_u_2, grid%v_2, grid%a_v_2,   &
                              grid%w_2, grid%a_w_2, grid%t_2, grid%a_t_2,   &
                              grid%ph_2, grid%a_ph_2, grid%mu_2, grid%a_mu_2,  &
                              moist, a_moist,                               &
                              grid%ru, grid%a_ru, grid%rv, grid%a_rv, grid%rw, grid%a_rw, &
                              grid%ww, grid%a_ww, grid%php, grid%a_php, grid%alt, grid%a_alt, &
                              grid%muu, grid%a_muu, grid%muv, grid%a_muv,   &
                              grid%mub, grid%mut, grid%a_mut,               &
                              grid%phb, grid%pb, grid%p, grid%a_p, grid%al, grid%a_al, grid%alb, &
                              cqu, a_cqu, cqv, a_cqv, cqw, a_cqw,                 &
                              grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                              grid%msfvy, grid%msftx, grid%msfty,                 &
                              grid%fnm, grid%fnp, grid%dnw, grid%rdx, grid%rdy,   &
                              num_3d_m,                         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start, k_end                   )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_rk_step_prep ' )
end if
! CHECK NECESSARY SUBROUTINE: rk_step_prep }

     END DO adj_rk_step_prep
     !$OMP END PARALLEL DO
BENCH_END(adj_step_prep_tim)

   END DO adj_Runge_Kutta_loop

!==[adj_P0]===============================================================
!  Adjoint of setting bdy tendencies to zero for DFI if constant_bc = true

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = grid%num_tiles,1,-1

       IF( config_flags%specified .AND. config_flags%constant_bc ) THEN


! NPAN 04/23/10 + CHECK NECESSARY SUBROUTINE: zero_bdytend{
if (config_flags % sw_call_zero_bdytend) then
   call wrf_debug ( -100 , ' SWITCH ON ==> call a_zero_bdytend ' )

       CALL a_zero_bdytend (grid%u_btxs,grid%a_u_btxs,grid%u_btxe,grid%a_u_btxe, &
                            grid%u_btys,grid%a_u_btys,grid%u_btye,grid%a_u_btye, &
                            grid%v_btxs,grid%a_v_btxs,grid%v_btxe,grid%a_v_btxe, &
                            grid%v_btys,grid%a_v_btys,grid%v_btye,grid%a_v_btye, &
                            grid%ph_btxs,grid%a_ph_btxs,grid%ph_btxe,grid%a_ph_btxe, &
                            grid%ph_btys,grid%a_ph_btys,grid%ph_btye,grid%a_ph_btye, &
                            grid%t_btxs,grid%a_t_btxs,grid%t_btxe,grid%a_t_btxe, &
                            grid%t_btys,grid%a_t_btys,grid%t_btye,grid%a_t_btye, &
                            grid%w_btxs,grid%a_w_btxs,grid%w_btxe,grid%a_w_btxe, &
                            grid%w_btys,grid%a_w_btys,grid%w_btye,grid%a_w_btye, &
                            grid%mu_btxs,grid%a_mu_btxs,grid%mu_btxe,grid%a_mu_btxe, &
                            grid%mu_btys,grid%a_mu_btys,grid%mu_btye,grid%a_mu_btye, &
                            moist_btxs,a_moist_btxs,moist_btxe,a_moist_btxe, &
                            moist_btys,a_moist_btys,moist_btye,a_moist_btye, &
                            grid%spec_bdy_width,num_3d_m,                &
                            ids,ide, jds,jde, kds,kde,                   &
                            ims,ime, jms,jme, kms,kme,                   &
                            ips,ipe, jps,jpe, kps,kpe,                   &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                               )

else
   call wrf_debug ( -100 , ' SWITCH OFF ==> call a_zero_bdytend ' )
end if
! CHECK NECESSARY SUBROUTINE: zero_bdytend }

       ENDIF
     ENDDO
     !$OMP END PARALLEL DO

!==[P8_adj]===============================================================
!!!!! Need to consider dtbc: whether + or - operation???  Ning Pan
   IF( config_flags%specified .or. config_flags%nested ) THEN
     grid%dtbc = grid%dtbc + grid%dt
   ENDIF

!==[P12_adj]==============================================================
!  Max values of CFL for adaptive time step scheme

   DEALLOCATE(max_vert_cfl_tmp)
   DEALLOCATE(max_horiz_cfl_tmp)

   RETURN

END SUBROUTINE solve_em_ad

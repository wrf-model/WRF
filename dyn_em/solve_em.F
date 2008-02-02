!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em ( grid , config_flags  &
! Actual arguments generated from Registry
#include "dummy_new_args.inc"
!
                    )

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
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
! Registry generated module
   USE module_state_description
   USE module_radiation_driver
   USE module_surface_driver
   USE module_cumulus_driver
   USE module_microphysics_driver
   USE module_microphysics_zero_out
   USE module_pbl_driver
   USE module_fddagd_driver
   USE module_fddaobs_driver
   USE module_diagnostics
#ifdef WRF_CHEM
   USE module_input_chem_data
   USE module_chem_utilities
#endif

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
   REAL ,DIMENSION(16)  :: max_vert_cfl_tmp, max_horiz_cfl_tmp
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk
   REAL :: curr_secs
   INTEGER :: num_sound_steps
   INTEGER :: idex, jdex
   REAL    :: max_msft, max_msftx, max_msfty
   REAL    :: spacing
   

   INTEGER :: ii, jj !kk is above after l,kte
   REAL    :: dclat

! urban related variables
   INTEGER :: NUM_ROOF_LAYERS, NUM_WALL_LAYERS, NUM_ROAD_LAYERS   ! urban

   TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval
   REAL                                       :: real_time
   LOGICAL                                    :: adapt_step_flag

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


!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  grid%itimestep = grid%itimestep + 1

IF (config_flags%polar) dclat = 90./REAL(jde-jds) !(0.5 * 180/ny)

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

#ifdef WRF_CHEM
!
!    prepare chem aerosols for advection before communication
!

   kte=min(k_end,kde-1)
# ifdef DM_PARALLEL
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see matching halo calls below for stencils
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
       if( config_flags%progn > 0 ) then
#         include "HALO_EM_SCALAR_E_3.inc"
       end if
     ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
       if( config_flags%progn > 0 ) then
#         include "HALO_EM_SCALAR_E_5.inc"
       end if
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
# endif
!--------------------------------------------------------------
#endif

 rk_order = config_flags%rk_ord


  !
  ! Calculate current time in seconds since beginning of model run.
  !   Unfortunately, ESMF does not seem to have a way to return
  !   floating point seconds based on a TimeInterval.  So, we will
  !   calculate it here--but, this is not clean!!
  !
 tmpTimeInterval = domain_get_current_time ( grid ) - domain_get_start_time ( grid )
 if (ABS(tmpTimeInterval%basetime%Sd) < 1) then
    curr_secs = tmpTimeInterval%basetime%S
 else
    curr_secs = real_time(tmpTimeInterval)
 endif

!-----------------------------------------------------------------------------
! Adaptive time step: Added by T. Hutchinson, WSI  3/5/07
!   In this call, we do the time-step adaptation.
!
 if (config_flags%use_adaptive_time_step) then
    CALL adapt_timestep(grid, config_flags)
    adapt_step_flag = .TRUE.
 else
    adapt_step_flag = .FALSE.
 endif
! End of adaptive time step modifications
!-----------------------------------------------------------------------------

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Update the specified boundary arrays.  This was moved to this routine by 
  !    T. Hutchinson, WSI, when implementing the adaptive time step.
  !

  IF (config_flags%specified) THEN
     
     ! Arrays for specified boundary conditions
     !
     DO loop = grid%spec_zone + 1, grid%spec_zone + grid%relax_zone
        grid%fcx(loop) = 0.1 / grid%dt * (grid%spec_zone + grid%relax_zone - loop) / (grid%relax_zone - 1)
        grid%gcx(loop) = 1.0 / grid%dt / 50. * (grid%spec_zone + grid%relax_zone - loop) / (grid%relax_zone - 1)
     ENDDO
     
  ELSE IF (config_flags%nested) THEN
     
     ! Arrays for specified boundary conditions
     
     DO loop = grid%spec_zone + 1, grid%spec_zone + grid%relax_zone
        grid%fcx(loop) = 0.1 / grid%dt * (grid%spec_zone + grid%relax_zone - loop) / (grid%relax_zone - 1)
        grid%gcx(loop) = 1.0 / grid%dt / 50. * (grid%spec_zone + grid%relax_zone - loop) / (grid%relax_zone - 1)
     ENDDO
     
  ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


 IF (grid%time_step_sound == 0) THEN
! auto-set option
! This function will give 4 for 6*dx and 6 for 10*dx and returns even numbers only
!
! Added MAXVAL(msft) to the denominator to better calculate num_sound_steps (T. Hutchinson, WSI)
!
    max_msftx=MAXVAL(grid%msftx)
    max_msfty=MAXVAL(grid%msfty)
#ifdef DM_PARALLEL
  call wrf_dm_maxval(max_msftx, idex, jdex)    
  call wrf_dm_maxval(max_msfty, idex, jdex)    
#endif
  max_msft=MAX(max_msftx, max_msfty)
  spacing = min(grid%dx, grid%dy)

   num_sound_steps = max ( 2 * ( INT (300. * grid%dt / (spacing / max_msft) - 0.01 ) + 1 ), 4 )
   WRITE(wrf_err_message,*)'min grid spacing, dt, time_step_sound=',spacing,grid%dt,num_sound_steps
   CALL wrf_debug ( 50 , wrf_err_message )
 ELSE
    num_sound_steps = grid%time_step_sound
 ENDIF

#ifdef DM_PARALLEL
  call wrf_dm_maxval(grid%max_vert_cfl, idex, jdex)
#endif
  
  if (config_flags%time_step < 0) then
     WRITE(wrf_err_message,*)'variable dt, max horiz cfl, max vert cfl: ',&
          grid%dt, grid%max_horiz_cfl, grid%max_vert_cfl
     CALL wrf_debug ( 50 , wrf_err_message )
  endif

 grid%max_cfl_val = 0
 grid%max_horiz_cfl = 0
 grid%max_vert_cfl = 0

 dts = grid%dt/float(num_sound_steps)

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
                           moist,                            &
                           grid%ru, grid%rv, grid%rw, grid%ww, grid%php, grid%alt, grid%muu, grid%muv,   &
                           grid%mub, grid%mut, grid%phb, grid%pb, grid%p, grid%al, grid%alb,    &
                           cqu, cqv, cqw,                    &
                           grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv, grid%msfvy, grid%msftx, grid%msfty,               &
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
!  3D variables - note staggering!  grid%ru(X), grid%rv(Y), grid%ww(grid%z), grid%php(grid%z)
!
!j grid%ru     x
!j grid%rv     x
!j grid%ww     x
!j grid%php    x
!j grid%alt    x
!j grid%ph_2   x
!j grid%phb    x
!
!  the following are 2D (xy) variables
!
!j grid%muu    x
!j grid%muv    x
!j grid%mut    x
!--------------------------------------------------------------
#    include "HALO_EM_A.inc"
#endif

! set boundary conditions on variables 
! from big_step_prep for use in big_step_proc

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A.inc"
#endif

!   CALL set_tiles ( grid , ids , ide , jds , jde , ips-1 , ipe+1 , jps-1 , jpe+1 )

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
       !TBH:  need this 2nd timestep and later
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

!         CALL zero_pole ( grid%rv,                      &
!                          ids, ide, jds, jde, kds, kde,     &
!                          ims, ime, jms, jme, kms, kme,     &
!                          grid%i_start(ij), grid%i_end(ij), &
!                          grid%j_start(ij), grid%j_end(ij), &
!                          k_start, k_end                   )

!-------------------------------------------------------
! end lat-lon grid pole-point (v) specification
!-------------------------------------------------------

   ENDIF

   END DO
   !$OMP END PARALLEL DO
BENCH_END(set_phys_bc_tim)

    rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).
 
BENCH_START(init_zero_tend_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call init_zero_tendency' )
        CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                  ph_tendf, t_tendf, tke_tend,      &
                                  mu_tendf,                         &
                                  moist_tend,chem_tend,scalar_tend, &
                                  num_3d_m,num_3d_c,num_3d_s,       &
                                  rk_step,                          &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                   )

     END DO
   !$OMP END PARALLEL DO
BENCH_END(init_zero_tend_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_PHYS_A.inc"
#endif

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
!              3D TKE and mixing.
!<pre>
!</DESCRIPTION>


BENCH_START(phy_prep_tim)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call phy_prep' )
         CALL phy_prep ( config_flags,                           &
                         grid%mut, grid%muu, grid%muv, grid%u_2, &
                         grid%v_2, grid%p, grid%pb, grid%alt,              &
                         grid%ph_2, grid%phb, grid%t_2, grid%tsk, moist, num_3d_m,   &
                         mu_3d, rho,                             &
                         th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                         p8w, t_phy, t8w, grid%z, z_at_w,             &
                         dz8w, grid%fnm, grid%fnp,                         &    
                         grid%rthraten,                               &
                         grid%rthblten, grid%rublten, grid%rvblten,             &
                         grid%rqvblten, grid%rqcblten, grid%rqiblten,           &
                         grid%rthcuten, grid%rqvcuten, grid%rqccuten,           &
                         grid%rqrcuten, grid%rqicuten, grid%rqscuten,           &
                         grid%rthften,  grid%rqvften,                      &
                         grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN,       &
                         grid%RQVNDGDTEN, grid%RMUNDGDTEN,                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO

BENCH_END(phy_prep_tim)

!  physics to implement

!      CALL set_tiles ( grid , ids , ide-1 , jds , jde-1 ips , ipe , jps , jpe )

! Open MP loops are in physics drivers
! radiation

! this driver is only needed to handle non-local shadowing effects
         CALL pre_radiation_driver ( grid, config_flags                        & 
              ,itimestep=grid%itimestep, ra_call_offset=grid%ra_call_offset    & 
              ,XLAT=grid%xlat, XLONG=grid%xlong, GMT=grid%gmt                  &
              ,julian=grid%julian, xtime=grid%xtime, RADT=grid%radt            &
              ,STEPRA=grid%stepra                                              & 
              ,ht=grid%ht,dx=grid%dx,dy=grid%dy,sina=grid%sina,cosa=grid%cosa  &
              ,shadowmask=shadowmask,slope_rad=config_flags%slope_rad          &
              ,topo_shading=config_flags%topo_shading                          & 
              ,shadlen=config_flags%shadlen,ht_shad=grid%ht_shad,ht_loc=ht_loc                              & 
              ,ht_shad_bxs=grid%ht_shad_bxs, ht_shad_bxe=grid%ht_shad_bxe      &
              ,ht_shad_bys=grid%ht_shad_bys, ht_shad_bye=grid%ht_shad_bye      &
              ,nested=config_flags%nested, min_ptchsz=grid%min_ptchsz          & 
              ,spec_bdy_width=config_flags%spec_bdy_width                      &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   )

         CALL wrf_debug ( 200 , ' call radiation_driver' )
BENCH_START(rad_driver_tim)

         CALL radiation_driver(                                           &
     &         ACFRCV=grid%acfrcv      ,ACFRST=grid%acfrst      ,ALBEDO=grid%albedo      &
     &        ,CFRACH=grid%cfrach      ,CFRACL=grid%cfracl      ,CFRACM=grid%cfracm      &
     &        ,CUPPT=grid%cuppt        ,CZMEAN=grid%czmean      ,DT=grid%dt              &
     &        ,DZ8W=dz8w          ,EMISS=grid%emiss        ,GLW=grid%glw            &
     &        ,GMT=grid%gmt            ,GSW=grid%gsw            ,HBOT=grid%hbot          &
     &        ,HTOP=grid%htop ,HBOTR=grid%hbotr, HTOPR=grid%htopr ,ICLOUD=config_flags%icloud &
     &        ,ITIMESTEP=grid%itimestep,JULDAY=grid%julday, JULIAN=grid%julian      &
     &        ,JULYR=grid%julyr        ,LW_PHYSICS=config_flags%ra_lw_physics  &
     &        ,NCFRCV=grid%ncfrcv      ,NCFRST=grid%ncfrst      ,NPHS=1             &
     &        ,P8W=p8w            ,P=p_phy            ,PI=pi_phy          &
     &        ,RADT=grid%radt     ,RA_CALL_OFFSET=grid%ra_call_offset     &
     &        ,RHO=rho            ,RLWTOA=grid%rlwtoa                          &
     &        ,RSWTOA=grid%rswtoa      ,RTHRATEN=grid%rthraten                      &
     &        ,RTHRATENLW=grid%rthratenlw                                      &
     &        ,RTHRATENSW=grid%rthratensw                  ,SNOW=grid%snow          &
     &        ,STEPRA=grid%stepra      ,SWDOWN=grid%swdown      ,SWDOWNC=grid%swdownc    &
     &        ,SW_PHYSICS=config_flags%ra_sw_physics  ,T8W=t8w            &
     &        ,T=t_phy            ,TAUCLDC=grid%taucldc    ,TAUCLDI=grid%taucldi    &
     &        ,TSK=grid%tsk            ,VEGFRA=grid%vegfra     ,WARM_RAIN=grid%warm_rain &
     &        ,XICE=grid%xice                                                  &
     &        ,XLAND=grid%xland        ,XLAT=grid%xlat          ,XLONG=grid%xlong        &
!Optional urban
     &        ,DECLIN_URB=grid%declin_urb        ,COSZ_URB2D=grid%cosz_urb2d        &
     &        ,OMG_URB2D=grid%omg_urb2d                                        &
!
     &        ,Z=grid%z                                                        &
     &        ,LEVSIZ=grid%levsiz, N_OZMIXM=num_ozmixm                    &
     &        ,N_AEROSOLC=num_aerosolc                                    &
     &        ,PAERLEV=grid%paerlev                                       &
     &        ,CAM_ABS_DIM1=grid%cam_abs_dim1, CAM_ABS_DIM2=grid%cam_abs_dim2 &
     &        ,CAM_ABS_FREQ_S=grid%cam_abs_freq_s                         &
     &        ,XTIME=grid%xtime                                                &
              ,CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag       &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   &
            ! Optional                          
     &        , CLDFRA=grid%cldfra                                        &
     &        , PB=grid%pb                                                     &
     &        , F_ICE_PHY=grid%f_ice_phy,F_RAIN_PHY=grid%f_rain_phy                 &
     &        , QV=moist(ims,kms,jms,P_QV), F_QV=F_QV                     &
     &        , QC=moist(ims,kms,jms,P_QC), F_QC=F_QC                     &
     &        , QR=moist(ims,kms,jms,P_QR), F_QR=F_QR                     &
     &        , QI=moist(ims,kms,jms,P_QI), F_QI=F_QI                     &
     &        , QS=moist(ims,kms,jms,P_QS), F_QS=F_QS                     &
     &        , QG=moist(ims,kms,jms,P_QG), F_QG=F_QG                     &
     &        , QNDROP=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP    &
#ifdef ACFLUX
     &        ,ACSWUPT=acswupt    ,ACSWUPTC=acswuptc                      &
     &        ,ACSWDNT=acswdnt    ,ACSWDNTC=acswdntc                      &
     &        ,ACSWUPB=acswupb    ,ACSWUPBC=acswupbc                      &
     &        ,ACSWDNB=acswdnb    ,ACSWDNBC=acswdnbc                      &
     &        ,ACLWUPT=aclwupt    ,ACLWUPTC=aclwuptc                      &
     &        ,ACLWDNT=aclwdnt    ,ACLWDNTC=aclwdntc                      &
     &        ,ACLWUPB=aclwupb    ,ACLWUPBC=aclwupbc                      &
     &        ,ACLWDNB=aclwdnb    ,ACLWDNBC=aclwdnbc                      &
     &        ,SWUPT=swupt    ,SWUPTC=swuptc                              &
     &        ,SWDNT=swdnt    ,SWDNTC=swdntc                              &
     &        ,SWUPB=swupb    ,SWUPBC=swupbc                              &
     &        ,SWDNB=swdnb    ,SWDNBC=swdnbc                              &
     &        ,LWUPT=lwupt    ,LWUPTC=lwuptc                              &
     &        ,LWDNT=lwdnt    ,LWDNTC=lwdntc                              &
     &        ,LWUPB=lwupb    ,LWUPBC=lwupbc                              &
     &        ,LWDNB=lwdnb    ,LWDNBC=lwdnbc                              &
#endif
     &        ,LWCF=grid%lwcf                                                  &
     &        ,SWCF=grid%swcf                                                  &
     &        ,OLR=grid%olr                                                    &
     &        ,OZMIXM=grid%ozmixm, PIN=grid%pin                                     &
     &        ,M_PS_1=grid%m_ps_1, M_PS_2=grid%m_ps_2, AEROSOLC_1=grid%aerosolc_1        &
     &        ,AEROSOLC_2=grid%aerosolc_2, M_HYBI0=grid%m_hybi                      &
     &        ,ABSTOT=grid%abstot, ABSNXT=grid%absnxt, EMSTOT=grid%emstot                &
#ifdef WRF_CHEM
     &        ,CU_RAD_FEEDBACK=config_flags%cu_rad_feedback                &
     &        ,AER_RA_FEEDBACK=config_flags%aer_ra_feedback                &
     &        ,QC_ADJUST=grid%GD_CLOUD_B , QI_ADJUST=grid%GD_CLOUD2_B         &
     &        ,PM2_5_DRY=grid%pm2_5_dry, PM2_5_WATER=grid%pm2_5_water               &
     &        ,PM2_5_DRY_EC=grid%pm2_5_dry_ec                                  &
     &        ,TAUAER300=grid%tauaer1, TAUAER400=grid%tauaer2 & ! jcb
     &        ,TAUAER600=grid%tauaer3, TAUAER999=grid%tauaer4 & ! jcb
     &        ,GAER300=grid%gaer1, GAER400=grid%gaer2, GAER600=grid%gaer3, GAER999=grid%gaer4 & ! jcb
     &        ,WAER300=grid%waer1, WAER400=grid%waer2, WAER600=grid%waer3, WAER999=grid%waer4 & ! jcb
#endif
     &         ,slope_rad=config_flags%slope_rad,topo_shading=config_flags%topo_shading     &
               ,shadowmask=shadowmask,ht=grid%ht,dx=grid%dx,dy=grid%dy,sina=grid%sina,cosa=grid%cosa    )

BENCH_END(rad_driver_tim)

!********* Surface driver
! surface

BENCH_START(surf_driver_tim)

!-----------------------------------------------------------------
! urban related variable are added to arguments of surface_driver
!-----------------------------------------------------------------
      NUM_ROOF_LAYERS = grid%num_soil_layers !urban
      NUM_WALL_LAYERS = grid%num_soil_layers !urban
      NUM_ROAD_LAYERS = grid%num_soil_layers !urban
      CALL nl_get_iswater(grid%id, iswater)

      CALL wrf_debug ( 200 , ' call surface_driver' )
      CALL surface_driver(                                                &
     &         ACSNOM=grid%acsnom      ,ACSNOW=grid%acsnow      ,AKHS=grid%akhs          &
     &        ,AKMS=grid%akms          ,ALBBCK=grid%albbck      ,ALBEDO=grid%albedo      &
     &        ,BR=br              ,CANWAT=grid%canwat      ,CHKLOWQ=chklowq    &
     &        ,CT=grid%ct              ,DT=grid%dt         ,DX=grid%dx         &
     &        ,DZ8W=dz8w          ,DZS=grid%dzs            ,FLHC=grid%flhc          &
     &        ,FLQC=grid%flqc          ,GLW=grid%glw            ,GRDFLX=grid%grdflx      &
     &        ,GSW=grid%gsw    ,SWDOWN=grid%swdown        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx              &
     &        ,HT=grid%ht              ,IFSNOW=config_flags%ifsnow      ,ISFFLX=config_flags%isfflx      &
     &        ,ISLTYP=grid%isltyp      ,ITIMESTEP=grid%itimestep                    &
     &        ,IVGTYP=grid%ivgtyp      ,LH=grid%lh              ,LOWLYR=grid%lowlyr      &
     &        ,MAVAIL=grid%mavail      ,NUM_SOIL_LAYERS=config_flags%num_soil_layers        &
     &        ,P8W=p8w            ,PBLH=grid%pblh          ,PI_PHY=pi_phy      &
     &        ,PSFC=grid%psfc          ,PSHLTR=grid%pshltr      ,PSIH=psih          &
     &        ,BLDT=grid%bldt     ,CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag  &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,Q10=grid%q10            &
     &        ,Q2=grid%q2              ,QFX=grid%qfx            ,QSFC=grid%qsfc          &
     &        ,QSHLTR=grid%qshltr      ,QZ0=grid%qz0            ,RAINCV=grid%raincv      &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics            ,RHO=rho            &
     &        ,RMOL=grid%rmol          ,SFCEVP=grid%sfcevp      ,SFCEXC=grid%sfcexc      &
     &        ,SFCRUNOFF=grid%sfcrunoff                                        &
     &        ,SF_SFCLAY_PHYSICS=config_flags%sf_sfclay_physics                        &
     &        ,SF_SURFACE_PHYSICS=config_flags%sf_surface_physics  ,SH2O=grid%sh2o          &
     &        ,SHDMAX=grid%shdmax      ,SHDMIN=grid%shdmin      ,SMOIS=grid%smois        &
     &        ,SMSTAV=grid%smstav      ,SMSTOT=grid%smstot      ,SNOALB=grid%snoalb      &
     &        ,SNOW=grid%snow          ,SNOWC=grid%snowc        ,SNOWH=grid%snowh        &
     &        ,SST=grid%sst            ,SST_UPDATE=grid%sst_update                  &
     &        ,STEPBL=grid%stepbl      ,TH10=grid%th10          ,TH2=grid%th2            &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TMN=grid%tmn            ,TSHLTR=grid%tshltr      ,TSK=grid%tsk            &
     &        ,TSLB=grid%tslb          ,T_PHY=t_phy        ,U10=grid%u10            &
     &        ,URATX=grid%uratx        ,VRATX=grid%vratx   ,TRATX=grid%tratx        &
     &        ,UDRUNOFF=grid%udrunoff  ,UST=grid%ust       ,UZ0=grid%uz0            &
     &        ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        ,V10=grid%v10            &
     &        ,VEGFRA=grid%vegfra      ,VZ0=grid%vz0       ,V_FRAME=grid%v_frame    &
     &        ,V_PHY=v_phy             ,WARM_RAIN=grid%warm_rain                    &
     &        ,WSPD=wspd               ,XICE=grid%xice     ,XLAND=grid%xland        &
     &        ,Z0=grid%z0              ,Z=grid%z        ,ZNT=grid%znt            &
     &        ,ZS=grid%zs              ,XICEM=grid%xicem   ,ISICE=grid%landuse_isice&
     &        ,USTM=grid%ustm          ,CK=grid%ck         ,CKA=grid%cka            &
     &                                 ,CD=grid%cd         ,CDA=grid%cda            &
     &        ,ISFTCFLX=config_flags%isftcflx                                       &
     &        ,OMLCALL=config_flags%omlcall ,OML_HML0=config_flags%oml_hml0 ,OML_GAMMA=config_flags%oml_gamma &
     &        ,TML=grid%tml, T0ML=grid%t0ml, HML=grid%hml, H0ML=grid%h0ml, HUML=grid%huml, HVML=grid%hvml, F=grid%f &
     &        ,ISWATER=iswater &
     &        ,DECLIN_URB=grid%declin_urb  ,COSZ_URB2D=grid%cosz_urb2d    & !I urban
     &        ,OMG_URB2D=grid%omg_urb2d    ,xlat_urb2d=grid%XLAT          & !I urban
     &        ,NUM_ROOF_LAYERS=num_roof_layers                            & !I urban
     &        ,NUM_WALL_LAYERS=num_wall_layers                            & !I urban
     &        ,NUM_ROAD_LAYERS=num_road_layers                            &
     &        ,DZR=grid%dzr ,DZB=grid%dzb ,DZG=grid%dzg                   & !I urban
     &        ,TR_URB2D=grid%tr_urb2d ,TB_URB2D=grid%tb_urb2d             &
     &        ,TG_URB2D=grid%tg_urb2d                                     & !H urban
     &        ,TC_URB2D=grid%tc_urb2d ,QC_URB2D=grid%qc_urb2d             & !H urban
     &        ,UC_URB2D=grid%uc_urb2d                                     & !H urban
     &        ,XXXR_URB2D=grid%xxxr_urb2d                                 &
     &        ,XXXB_URB2D=grid%xxxb_urb2d                                 & !H urban
     &        ,XXXG_URB2D=grid%xxxg_urb2d                                 &
     &        ,XXXC_URB2D=grid%xxxc_urb2d                                 & !H urban
     &        ,TRL_URB3D=grid%trl_urb3d   ,TBL_URB3D=grid%tbl_urb3d       & !H urban
     &        ,TGL_URB3D=grid%tgl_urb3d                                   & !H urban
     &        ,SH_URB2D=grid%sh_urb2d     ,LH_URB2D=grid%lh_urb2d         &
     &        ,G_URB2D=grid%g_urb2d                                       & !H urban
     &        ,RN_URB2D=grid%rn_urb2d     , TS_URB2D=grid%ts_urb2d        & !H urban 
     &        ,FRC_URB2D=grid%frc_urb2d                                   & !H urban
     &        ,UTYPE_URB2D=grid%utype_urb2d                               & !H urban
     &        ,ucmcall=grid%ucmcall                                       & !H urban
           ! P-X LSM Variables
     &        ,LANDUSEF=grid%landusef, SOILCTOP=grid%soilctop             &   ! P-X LSM
     &        ,SOILCBOT=grid%soilcbot                                     &   ! P-X LSM
     &        ,RA=grid%ra, RS=grid%rs, LAI=grid%lai                       &   ! P-X LSM
     &        ,NLCAT=grid%num_land_cat,  NSCAT=grid%num_soil_cat          &   ! P-X LSM
     &        ,VEGF_PX=grid%vegf_px, SNOWNCV=grid%snowncv                 &   ! P-X LSM
     &        ,ANAL_INTERVAL=config_flags%interval_seconds                &   ! P-X LSM
     &        ,PXLSM_SMOIS_INIT=config_flags%pxlsm_smois_init             &   ! P-X LSM
     &        ,PXLSM_SOIL_NUDGE=config_flags%grid_fdda                    &   ! P-X LSM
           ! Optional PX LSM nudging
     &        ,t2_ndg_old=fdda2d(ims,1,jms,P_t2_ndg_old)                  &
     &        ,q2_ndg_old=fdda2d(ims,1,jms,P_q2_ndg_old)                  &
     &        ,t2_ndg_new=fdda2d(ims,1,jms,P_t2_ndg_new)                  &
     &        ,q2_ndg_new=fdda2d(ims,1,jms,P_q2_ndg_new)                  &
     &        ,sn_ndg_old=fdda2d(ims,1,jms,P_sn_ndg_old)                  &
     &        ,sn_ndg_new=fdda2d(ims,1,jms,P_sn_ndg_new)                  &
           ! Indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        , I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
     &        , J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
     &        , KTS=k_start, KTE=min(k_end,kde-1)                         &
     &        , NUM_TILES=grid%num_tiles                                  &
           ! Optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,CAPG=grid%capg, EMISS=grid%emiss, HOL=hol,MOL=grid%mol                    &
     &        ,T2OBS=grid%t2obs, Q2OBS=grid%q2obs                         &
     &        ,RAINBL=grid%rainbl,SR=grid%sr                                              &
     &        ,RAINNCV=grid%rainncv,REGIME=regime,T2=grid%t2,THC=grid%thc                &
     &        ,QSG=grid%qsg,QVG=grid%qvg,QCG=grid%qcg,SOILT1=grid%soilt1,TSNAV=grid%tsnav          & ! ruc lsm
     &        ,SMFR3D=grid%smfr3d,KEEPFR3DFLAG=grid%keepfr3dflag                    & ! ruc lsm
     &        ,POTEVP=grid%POTEVP, SNOPCX=grid%SNOPCX, SOILTB=grid%SOILTB                & ! ruc lsm
     &                                                              )
BENCH_END(surf_driver_tim)

!*********
! pbl

      CALL wrf_debug ( 200 , ' call pbl_driver' )
BENCH_START(pbl_driver_tim)
      CALL pbl_driver(                                                    &
     &         AKHS=grid%akhs          ,AKMS=grid%akms                              &
     &        ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics                 &
     &        ,BLDT=grid%bldt, CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag &
     &        ,BR=br              ,CHKLOWQ=chklowq    ,CT=grid%ct         &
     &        ,DT=grid%dt              ,DX=grid%dx              ,DZ8W=dz8w          &
     &        ,EL_MYJ=grid%el_myj      ,EXCH_H=grid%exch_h      ,GRDFLX=grid%grdflx      &
     &        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx            ,HT=grid%ht              &
     &        ,ITIMESTEP=grid%itimestep                    ,KPBL=grid%kpbl          &
     &        ,LH=grid%lh              ,LOWLYR=grid%lowlyr      ,P8W=p8w            &
     &        ,PBLH=grid%pblh          ,PI_PHY=pi_phy      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,QFX=grid%qfx            &
     &        ,QSFC=grid%qsfc          ,QZ0=grid%qz0                                &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics                   &
     &        ,RHO=rho            ,RQCBLTEN=grid%rqcblten  ,RQIBLTEN=grid%rqiblten  &
     &        ,RQVBLTEN=grid%rqvblten  ,RTHBLTEN=grid%rthblten  ,RUBLTEN=grid%rublten    &
     &        ,RVBLTEN=grid%rvblten    ,SNOW=grid%snow          ,STEPBL=grid%stepbl      &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TSK=grid%tsk            ,T_PHY=t_phy        ,UST=grid%ust            &
     &        ,U10=grid%u10 ,UZ0=grid%uz0      ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        &
     &        ,V10=grid%v10 ,VZ0=grid%vz0      ,V_FRAME=grid%v_frame    ,V_PHY=v_phy        &
     &        ,WARM_RAIN=grid%warm_rain                    ,WSPD=wspd          &
     &        ,XICE=grid%xice          ,XLAND=grid%xland        ,Z=grid%z                &
     &        ,ZNT=grid%znt                                                    &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
          ! Variables Required by ACM PBL  - jp
     &        ,ZNU=grid%znu,ZNW=grid%znw,MUT=grid%mut                     &
          ! optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,HOL=HOL, MOL=grid%mol, REGIME=REGIME                            &
     &                                                          )

BENCH_END(pbl_driver_tim)

! cumulus para.

          CALL wrf_debug ( 200 , ' call cumulus_driver' )

BENCH_START(cu_driver_tim)
         CALL cumulus_driver(                                             &
                 ! Prognostic variables
     &              U=u_phy   ,V=v_phy   ,TH=th_phy  ,T=t_phy             &
     &             ,W=grid%w_2     ,P=p_phy   ,PI=pi_phy  ,RHO=rho             &
                 ! Other arguments
     &             ,ITIMESTEP=grid%itimestep ,DT=grid%dt      ,DX=grid%dx                &
     &             ,CUDT=grid%cudt,CURR_SECS=curr_secs,ADAPT_STEP_FLAG=adapt_step_flag &
     &             ,RAINC=grid%rainc   ,RAINCV=grid%raincv   ,PRATEC=grid%pratec         &
     &             , NCA=grid%nca                                         &
     &             ,HTOP=grid%cutop     ,HBOT=grid%cubot       ,KPBL=grid%kpbl             &
     &             ,DZ8W=dz8w     ,P8W=p8w                                &
     &             ,W0AVG=grid%w0avg   ,STEPCU=grid%stepcu                          &
     &             ,CLDEFI=grid%cldefi ,LOWLYR=grid%lowlyr ,XLAND=grid%xland             &
     &             ,APR_GR=grid%apr_gr ,APR_W=grid%apr_w   ,APR_MC=grid%apr_mc           &
     &             ,APR_ST=grid%apr_st ,APR_AS=grid%apr_as ,APR_CAPMA=grid%apr_capma     &
     &             ,APR_CAPME=grid%apr_capme          ,APR_CAPMI=grid%apr_capmi     &
     &             ,MASS_FLUX=grid%mass_flux          ,XF_ENS=grid%xf_ens           &
     &             ,PR_ENS=grid%pr_ens ,HT=grid%ht                                  &
     &             ,ENSDIM=config_flags%ensdim ,MAXIENS=config_flags%maxiens ,MAXENS=config_flags%maxens         &
     &             ,MAXENS2=config_flags%maxens2                ,MAXENS3=config_flags%maxens3       &
     &             ,CU_ACT_FLAG=cu_act_flag   ,WARM_RAIN=grid%warm_rain        &
     &             ,GSW=grid%gsw &
                   ,PERIODIC_X=(config_flags%polar .OR. config_flags%periodic_x)  &
                   ,PERIODIC_Y=config_flags%periodic_y  &
                 ! Selection flag
     &             ,CU_PHYSICS=config_flags%cu_physics                    &
                 ! Dimension arguments
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
     &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &             ,NUM_TILES=grid%num_tiles                              &
                 ! Moisture tendency arguments
     &             ,RQVCUTEN=grid%rqvcuten , RQCCUTEN=grid%rqccuten                 &
     &             ,RQSCUTEN=grid%rqscuten , RQICUTEN=grid%rqicuten                 &
     &             ,RQRCUTEN=grid%rqrcuten , RQVBLTEN=grid%rqvblten                 &
     &             ,RQVFTEN=grid%rqvften                                       &
                 ! Other tendency arguments
     &             ,RTHRATEN=grid%rthraten , RTHBLTEN=grid%rthblten                 &
     &             ,RTHCUTEN=grid%rthcuten , RTHFTEN=grid%rthften                   &
                 ! Moisture tracer arguments
     &             ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV            &
     &             ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC            &
     &             ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR            &
     &             ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI            &
     &             ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS            &
     &             ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG            &
#ifdef WRF_CHEM
     &             ,GD_CLOUD=grid%GD_CLOUD,GD_CLOUD2=grid%GD_CLOUD2                          &
#endif
     &                                                          )
BENCH_END(cu_driver_tim)

! fdda

          CALL wrf_debug ( 200 , ' call fddagd_driver' )

BENCH_START(fdda_driver_tim)
   CALL fddagd_driver(itimestep=grid%itimestep,dt=grid%dt,xtime=grid%XTIME,         &
                  id=grid%id,      &
                  RUNDGDTEN=grid%rundgdten,RVNDGDTEN=grid%rvndgdten,                &
                  RTHNDGDTEN=grid%rthndgdten,RQVNDGDTEN=grid%rqvndgdten,            &
                  RMUNDGDTEN=grid%rmundgdten,                                  &
                  u_ndg_old=fdda3d(ims,kms,jms,P_u_ndg_old),              &
                  v_ndg_old=fdda3d(ims,kms,jms,P_v_ndg_old),              &
                  t_ndg_old=fdda3d(ims,kms,jms,P_t_ndg_old),              &
                  q_ndg_old=fdda3d(ims,kms,jms,P_q_ndg_old),              &
                  mu_ndg_old=fdda2d(ims,1,jms,P_mu_ndg_old),              &
                  u_ndg_new=fdda3d(ims,kms,jms,P_u_ndg_new),              &
                  v_ndg_new=fdda3d(ims,kms,jms,P_v_ndg_new),              &
                  t_ndg_new=fdda3d(ims,kms,jms,P_t_ndg_new),              &
                  q_ndg_new=fdda3d(ims,kms,jms,P_q_ndg_new),              &
                  mu_ndg_new=fdda2d(ims,1,jms,P_mu_ndg_new),              &
                  u3d=grid%u_2,v3d=grid%v_2,th_phy=th_phy,rho=rho,moist=moist,      &
                  p_phy=p_phy,pi_phy=pi_phy,p8w=p8w,t_phy=t_phy,          &
                  dz8w=dz8w,z=grid%z,z_at_w=z_at_w,                            &
                  config_flags=config_flags,dx=grid%DX,n_moist=num_3d_m,  &
                  STEPFG=grid%STEPFG,                                          &
                  pblh=grid%pblh,ht=grid%ht,                                        &
                    IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                   ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                   ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                   ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                   ,KTS=k_start, KTE=min(k_end,kde-1)                     &
                   , num_tiles=grid%num_tiles                             )
BENCH_END(fdda_driver_tim)

! calculate_phy_tend

BENCH_START(cal_phy_tend)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
          CALL calculate_phy_tend (config_flags,grid%mut,grid%muu,grid%muv,pi_phy,            &
                     grid%rthraten,                                         &
                     grid%rublten,grid%rvblten,grid%rthblten,                         &
                     grid%rqvblten,grid%rqcblten,grid%rqiblten,                       &
                     grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten,              &
                     grid%rqicuten,grid%rqscuten,                                &
                     grid%RUNDGDTEN,grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN,        &
                     grid%RMUNDGDTEN,                                       &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO
BENCH_END(cal_phy_tend)

! tke diffusion

     IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN

BENCH_START(comp_diff_metrics_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
          CALL compute_diff_metrics ( config_flags, grid%ph_2, grid%phb, grid%z, grid%rdz, grid%rdzw, &
                                      grid%zx, grid%zy, grid%rdx, grid%rdy,                      &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(comp_diff_metrics_tim)

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A1.inc"
#endif

BENCH_START(tke_diff_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
          CALL set_physical_bc3d( grid%rdzw , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%rdz , 'w', config_flags,            &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%z , 'w', config_flags,              &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%zx , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( grid%zy , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )

       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(tke_diff_bc_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_C.inc"
#endif

BENCH_START(deform_div_tim)

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
          CALL cal_deform_and_div ( config_flags,grid%u_2,grid%v_2,grid%w_2,grid%div,  &
                                    grid%defor11,grid%defor22,grid%defor33,            &
                                    grid%defor12,grid%defor13,grid%defor23,            &
                                    grid%u_base, grid%v_base,grid%msfux,grid%msfuy,    &
                                    grid%msfvx,grid%msfvy,grid%msftx,grid%msfty,       &
                                    grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz,   &
                                    grid%rdzw,grid%fnm,grid%fnp,grid%cf1,grid%cf2,     &
                                    grid%cf3,grid%zx,grid%zy,            &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    grid%i_start(ij), grid%i_end(ij),    &
                                    grid%j_start(ij), grid%j_end(ij),    &
                                    k_start    , k_end                  )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(deform_div_tim)


#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_D.inc"
#endif


! calculate tke, kmh, and kmv

BENCH_START(calc_tke_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_km_kh' )
          CALL calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp,         &
                                config_flags%damp_opt,                                 &
                                grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv,grid%bn2,      &
                                grid%khdif,grid%kvdif,grid%div,                        &
                                grid%defor11,grid%defor22,grid%defor33,grid%defor12,   &
                                grid%defor13,grid%defor23,                             &
                                grid%tke_2,p8w,t8w,th_phy,                             &
                                t_phy,p_phy,moist,grid%dn,grid%dnw,                    &
                                grid%dx,grid%dy,grid%rdz,grid%rdzw,                    &
                                config_flags%mix_isotropic,num_3d_m,                   &
                                grid%cf1, grid%cf2, grid%cf3, grid%warm_rain,          &
                                grid%mix_upper_bound,                                  &
                                grid%msftx, grid%msfty,                                &
                                ids,ide, jds,jde, kds,kde,                             &
                                ims,ime, jms,jme, kms,kme,                             &
                                grid%i_start(ij), grid%i_end(ij),                      &
                                grid%j_start(ij), grid%j_end(ij),                      &
                                k_start    , k_end                          )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(calc_tke_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_E.inc"
#endif

     ENDIF

#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_PHY_BC.inc"
      IF ( config_flags%grid_fdda .eq. 1) THEN
#      include "PERIOD_BDY_EM_FDDA_BC.inc"
      ENDIF
#      include "PERIOD_BDY_EM_CHEM.inc"
#endif

BENCH_START(phy_bc_tim)
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call phy_bc' )
       CALL phy_bc (config_flags,grid%div,grid%defor11,grid%defor22,grid%defor33,            &
                            grid%defor12,grid%defor13,grid%defor23,                     &
                            grid%xkmh,grid%xkmv,grid%xkhh,grid%xkhv,                   &
                            grid%tke_2,                          &
                            grid%rublten, grid%rvblten,                            &
                            ids, ide, jds, jde, kds, kde,                &
                            ims, ime, jms, jme, kms, kme,                &
                            ips, ipe, jps, jpe, kps, kpe,                &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start    , k_end                           )
     ENDDO
     !$OMP END PARALLEL DO
BENCH_END(phy_bc_tim)

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
!
! MPP for some physics tendency, km, kh, deformation, and divergence
!
!               *                     *
!             * + *      * + *        +
!               *                     *
!
! (for PBL)
! grid%rublten                  x
! grid%rvblten                             x
!
! (for diff_opt >= 1)
! grid%defor11                  x
! grid%defor22                             x
! grid%defor12       x
! grid%defor13                  x
! grid%defor23                             x
! grid%div           x
! grid%xkmv          x
! grid%xkmh          x
! grid%xkhv          x
! grid%xkhh          x
! tke           x
!
!-----------------------------------------------------------------------
      IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
#      include "HALO_EM_PHYS_PBL.inc"
      ENDIF
      IF ( config_flags%grid_fdda .eq. 1) THEN
#      include "HALO_EM_FDDA.inc"
      ENDIF
      IF ( config_flags%diff_opt .ge. 1 ) THEN
#      include "HALO_EM_PHYS_DIFFUSION.inc"
      ENDIF

      IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3.inc"
      ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5.inc"
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
        CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF
#endif

BENCH_START(update_phy_ten_tim)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call update_phy_ten' )
        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
                          scalar_tend, mu_tendf,                           &
                          grid%rthraten,grid%rthblten,grid%rthcuten,       &
                          grid%rublten,grid%rvblten,                       &
                          grid%rqvblten,grid%rqcblten,grid%rqiblten,       &
                          grid%rqvcuten,grid%rqccuten,grid%rqrcuten,       &
                          grid%rqicuten,grid%rqscuten,grid%RUNDGDTEN,      &
                          grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RQVNDGDTEN,  &
                          grid%RMUNDGDTEN,                                 &
                          num_3d_m,num_3d_s,config_flags,rk_step,          &
                          grid%adv_moist_cond,                             &
                          ids, ide, jds, jde, kds, kde,                    &
                          ims, ime, jms, jme, kms, kme,                    &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                               )

      END DO
      !$OMP END PARALLEL DO
BENCH_END(update_phy_ten_tim)

#ifdef PLANET
      ! do rayleigh (and zonal-average newtonian) damping

      ! We are doing this here because we are in an "IF" statement
      ! that determines whether we are in the first RK step or not,
      ! and we only want to do the top damping once per "timestep".
      ! This is after the physics tendencies have been calculated,
      ! but before the small time steps have been started, so it seems
      ! like a good place to add in the damping tendencies.
      ! Remember we are modifying the *tendencies*, not the actual
      ! values.
      ! u_2 is u on the u-points, *uncoupled*, i.e., it has units of m/s
      ! It will get coupled with mu in the small steps (small_step_prep?)
      ! If we want tendencies, we will have to be sure to couple them
      ! appropriately, with mu.  From update_phy_ten above, RTHRATEN (e.g.)
      ! and t_tendf are now in the same units, and they are:
      ! (temperature tendency [K/s])  * (mut, surface mass)
      ! But for RU/VBLTEN, the units are:
      ! (momentum tendency (m/s^2) * (muu/v, surface mass)
      ! And it is important that these tendencies be tendf and not tend!
      ! The tend variables will be set to 0 in rk_tendency, and then the
      ! tendf variables will be added to tend variables in rk_addtend_dry
      ! The tendf's are "physics" tendencies ... see the subroutine
      ! rk_addtend_dry in module_em to see them put together.
      ! CALCULATE_PHY_TEND above takes the RUBLTEN, etc. and multiplies
      ! them by mu.

      IF ( (config_flags%damp_opt == 101) .OR. &
           (config_flags%damp_opt == 103)      ) THEN
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
            CALL damptop( grid%u_2, grid%v_2, grid%t_2, &
                          grid%mut, grid%muu, grid%muv, &
                          pi_phy,                                &
                          t_tendf, ru_tendf, rv_tendf, P2SI,     &
                          ids, ide, jds, jde, kds, kde,          &
                          ims, ime, jms, jme, kms, kme,          &
                          grid%i_start(ij), grid%i_end(ij),      &
                          grid%j_start(ij), grid%j_end(ij),      &
                          k_start, k_end                         )
         END DO
         !$OMP END PARALLEL DO
      END IF
#endif

     IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN

BENCH_START(tke_rhs_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL tke_rhs  ( tke_tend,grid%bn2,                           &
                          config_flags,grid%defor11,grid%defor22,      &
                          grid%defor33,                                &
                          grid%defor12,grid%defor13,grid%defor23,      &
                          grid%u_2,grid%v_2,grid%w_2,grid%div,         &
                          grid%tke_2,grid%mut,                         &
                          th_phy,p_phy,p8w,t8w,grid%z,grid%fnm,        & 
                          grid%fnp,grid%cf1,grid%cf2,grid%cf3,         &     
                          grid%msftx,grid%msfty,grid%xkmh,             &
                          grid%xkmv,grid%xkhv,grid%rdx,grid%rdy,       &
                          grid%dx,grid%dy,grid%dt,grid%zx,grid%zy,     &
                          grid%rdz,grid%rdzw,grid%dn,                  &
                          grid%dnw,config_flags%mix_isotropic,         &
                          grid%hfx, grid%qfx, moist(ims,kms,jms,P_QV), &
                          grid%ustm, rho,                              &
                          ids, ide, jds, jde, kds, kde,                &
                          ims, ime, jms, jme, kms, kme,                &
                          grid%i_start(ij), grid%i_end(ij),            &
                          grid%j_start(ij), grid%j_end(ij),            &
                          k_start    , k_end                           )

       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(tke_rhs_tim)

     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

     IF(config_flags%diff_opt .eq. 2) THEN


       IF (config_flags%bl_pbl_physics .eq. 0) THEN

BENCH_START(vert_diff_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
           CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,              &
                                      t_tendf, tke_tend,                         &
                                      moist_tend, num_3d_m,                      &
                                      chem_tend, num_3d_c,                       &
                                      scalar_tend, num_3d_s,                     &
                                      grid%u_2, grid%v_2,                                  &
                                      grid%t_2,grid%u_base,grid%v_base,grid%t_base,grid%qv_base,          &
                                      grid%mut,grid%tke_2,config_flags, &
                                      grid%defor13,grid%defor23,grid%defor33,                   &
                                      grid%div, moist, chem, scalar,                  &
                                      grid%xkmv, grid%xkhv, config_flags%km_opt,                        &
                                      grid%fnm, grid%fnp, grid%dn, grid%dnw, grid%rdz, grid%rdzw, &
                                      grid%hfx, grid%qfx, grid%ustm, rho,        &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start, k_end                             )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(vert_diff_tim)

       ENDIF
!
BENCH_START(hor_diff_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
         CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend,                              &
                                      moist_tend, num_3d_m,                  &
                                      chem_tend, num_3d_c,                   &
                                      scalar_tend, num_3d_s,                 &
                                      grid%t_2, th_phy,                           &
                                      grid%mut, grid%tke_2, config_flags,              &
                                      grid%defor11, grid%defor22, grid%defor12,             &
                                      grid%defor13, grid%defor23, grid%div,                 &
                                      moist, chem, scalar,                   &
                                      grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,  &
                                      grid%msfty, grid%xkmh, grid%xkhh, config_flags%km_opt,     &
                                      grid%rdx, grid%rdy, grid%rdz, grid%rdzw,                   &
                                      grid%fnm, grid%fnp, grid%cf1, grid%cf2, grid%cf3,          &
                                      grid%zx, grid%zy, grid%dn, grid%dnw,                       &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(hor_diff_tim)

     ENDIF

# ifdef DM_PARALLEL
#     include "HALO_OBS_NUDGE.inc"
#endif
!***********************************************************************
! This section for obs nudging
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

         CALL fddaobs_driver (grid%grid_id, model_config_rec%grid_id,  &
                  model_config_rec%parent_id, config_flags%restart,    &
                  grid%obs_nudge_opt,                                  &
                  grid%obs_ipf_errob,                                  &
                  grid%obs_ipf_nudob,                                  &
                  grid%fdda_start,                                     &
                  grid%fdda_end,                                       &
                  grid%obs_nudge_wind,                                 &
                  grid%obs_nudge_temp,                                 &
                  grid%obs_nudge_mois,                                 &
                  grid%obs_nudge_pstr,                                 &
                  grid%obs_coef_wind,                                  &
                  grid%obs_coef_temp,                                  &
                  grid%obs_coef_mois,                                  &
                  grid%obs_coef_pstr,                                  &             
                  grid%obs_rinxy,                                      &
                  grid%obs_rinsig,                                     &
                  grid%obs_npfi,                                       &
                  grid%obs_ionf,                                       &
                  grid%obs_nobs_prt,                                   &
                  grid%obs_idynin,                                     &
                  grid%obs_dtramp,                                     &
                  model_config_rec%cen_lat(1),                         &
                  model_config_rec%cen_lon(1),                         &
                  config_flags%truelat1,                               &
                  config_flags%truelat2,                               &
                  config_flags%map_proj,                               &
                  model_config_rec%i_parent_start,                     &
                  model_config_rec%j_parent_start,                     &
                  grid%parent_grid_ratio,                              &
                  grid%max_dom, grid%itimestep,                        &
                  grid%dt, grid%gmt, grid%julday, grid%fdob,           &
                  grid%max_obs,                                        &
                  model_config_rec%nobs_ndg_vars,                      &
                  model_config_rec%nobs_err_flds,                      &
                  grid%fdob%nstat, grid%fdob%varobs, grid%fdob%errf,   &
                  grid%dx, grid%KPBL,grid%HT,                          &
                  grid%mut, grid%muu, grid%muv,               &
                  grid%msftx, grid%msfty, grid%msfux, grid%msfuy, grid%msfvx, grid%msfvy, &
                  p_phy, t_tendf, t0,                                  &
                  grid%u_2, grid%v_2, grid%t_2,               &
                  moist(:,:,:,P_QV),                                   &
                  grid%pb, grid%p_top, grid%p,                   &
                  grid%uratx, grid%vratx, grid%tratx,                  &
                  ru_tendf, rv_tendf,                                  &
                  moist_tend(:,:,:,P_QV), grid%obs_savwt,           &
                  ids,ide, jds,jde, kds,kde,                           &
                  ims,ime, jms,jme, kms,kme,                           &
                  grid%i_start(ij), min(grid%i_end(ij),ide-1),         &
                  grid%j_start(ij), min(grid%j_end(ij),jde-1),         &
                  k_start    , min(k_end,kde-1)                     )

      ENDDO

     !$OMP END PARALLEL DO
! 
!***********************************************************************

     END IF rk_step_is_one

BENCH_START(rk_tend_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_tendency' )
      CALL rk_tendency ( config_flags, rk_step,                                                               &
                         grid%ru_tend, grid%rv_tend, rw_tend, ph_tend, t_tend,                          &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf,                                     &
                         mu_tend, grid%u_save, grid%v_save, w_save, ph_save,                            &
                         grid%t_save, mu_save, grid%rthften,                                               &
                         grid%ru, grid%rv, grid%rw, grid%ww,                                      &
                         grid%u_2, grid%v_2, grid%w_2, grid%t_2, grid%ph_2,                    &
                         grid%u_1, grid%v_1, grid%w_1, grid%t_1, grid%ph_1,                    &
                         grid%h_diabatic, grid%phb, grid%t_init,                                        &
                         grid%mu_2, grid%mut, grid%muu, grid%muv, grid%mub,                    &
                         grid%al, grid%alt, grid%p, grid%pb, grid%php, cqu, cqv, cqw,          &
                         grid%u_base, grid%v_base, grid%t_base, grid%qv_base, grid%z_base,                 &
                         grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,                                   &
                         grid%msfvy, grid%msftx,grid%msfty, grid%xlat, grid%f, grid%e, grid%sina, grid%cosa,  &
                         grid%fnm, grid%fnp, grid%rdn, grid%rdnw,                                 &
                         grid%dt, grid%rdx, grid%rdy, grid%khdif, grid%kvdif, grid%xkmh, grid%xkhh,           &
                         grid%diff_6th_opt, grid%diff_6th_factor,                                             &
                         grid%dampcoef,grid%zdamp,config_flags%damp_opt,                                      &
                         grid%cf1, grid%cf2, grid%cf3, grid%cfn, grid%cfn1, num_3d_m,                         &
                         config_flags%non_hydrostatic, config_flags%top_lid,                                  &
                         grid%u_frame, grid%v_frame,                                                          &
                         ids, ide, jds, jde, kds, kde,                                                        &
                         ims, ime, jms, jme, kms, kme,                                                        &
                         grid%i_start(ij), grid%i_end(ij),                                                    &
                         grid%j_start(ij), grid%j_end(ij),                                                    &
                         k_start, k_end,                                                                      &
                         max_vert_cfl_tmp(ij), max_horiz_cfl_tmp(ij)                                         )
   END DO
   !$OMP END PARALLEL DO
BENCH_END(rk_tend_tim)

DO ij = 1 , grid%num_tiles
   if (max_horiz_cfl_tmp(ij) .gt. grid%max_horiz_cfl) then
      grid%max_horiz_cfl = max_horiz_cfl_tmp(ij)
   endif
   if (max_vert_cfl_tmp(ij) .gt. grid%max_vert_cfl) then
      grid%max_vert_cfl = max_vert_cfl_tmp(ij)
   endif
END DO
   
if (grid%max_horiz_cfl .gt. grid%max_cfl_val) then
   grid%max_cfl_val = grid%max_horiz_cfl
endif
if (grid%max_vert_cfl .gt. grid%max_cfl_val) then
   grid%max_cfl_val = grid%max_vert_cfl
endif

BENCH_START(relax_bdy_dry_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

       CALL relax_bdy_dry ( config_flags,                                &
                            grid%u_save, grid%v_save, ph_save, grid%t_save,             &
                            w_save, mu_tend,                             & 
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
                          grid%h_diabatic, grid%mut, grid%msftx,        &
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
                            grid%t_1,grid%t_2,grid%ph_1,grid%ph_2,                         &
                            grid%mub, grid%mu_1, grid%mu_2,                                   &
                            grid%muu, muus, grid%muv, muvs,                                      &
                            grid%mut, grid%muts, grid%mudf,                                   &
                            grid%u_save, grid%v_save, w_save,                                    &
                            grid%t_save, ph_save, mu_save,                                          &
                            grid%ww, ww1,                                                           &
                            grid%dnw, c2a, grid%pb, grid%p, grid%alt,                      &
                            grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,                         &
                            grid%msfvy, grid%msftx,grid%msfty,                                         &
                            grid%rdx, grid%rdy, rk_step,                                               &
                            ids, ide, jds, jde, kds, kde,                                              &
                            ims, ime, jms, jme, kms, kme,                                              &
                            grid%i_start(ij), grid%i_end(ij),                                          &
                            grid%j_start(ij), grid%j_end(ij),                                          &
                            k_start    , k_end                                                        )

      CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                 &
                       grid%alt, grid%t_2, grid%t_save, c2a, pm1,  &
                       grid%mu_2, grid%muts, grid%znu, t0,         &
                       grid%rdnw, grid%dnw, grid%smdiv,               &
                       config_flags%non_hydrostatic, 0,                     &
                       ids, ide, jds, jde, kds, kde,                        &
                       ims, ime, jms, jme, kms, kme,                        &
                       grid%i_start(ij), grid%i_end(ij),                    &
                       grid%j_start(ij), grid%j_end(ij),                    &
                       k_start    , k_end                                  )

      IF (config_flags%non_hydrostatic) THEN
         CALL calc_coef_w( a,alpha,gamma,                    &
                           grid%mut, cqw,                 &
                           grid%rdn, grid%rdnw, c2a,   &
                           dts_rk, g, grid%epssm,            &
                           config_flags%top_lid,             &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
      END IF

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
!  3D variables - note staggering!  grid%ph_2(grid%z), grid%u_save(X), grid%v_save(Y)
!
!j grid%ph_2      x
!j grid%al        x
!j grid%p         x
!j grid%t_1       x
!j grid%t_save    x
!j grid%u_save    x
!j grid%v_save    x
!
!  the following are 2D (xy) variables
!
!j grid%mu_1      x
!j grid%mu_2      x
!j grid%mudf      x
!--------------------------------------------------------------
#      include "HALO_EM_B.inc"
#      include "PERIOD_BDY_EM_B.inc"
#endif

BENCH_START(set_phys_bc2_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

         CALL set_physical_bc3d( grid%ru_tend, 'u', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%rv_tend, 'v', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%t_1, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( grid%t_save, 't', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc2d( grid%mu_1, 't', config_flags,          &
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
                           grid%p, grid%pb,                                             &
                           grid%ph_2, grid%php, grid%alt,  grid%al,               &
                           grid%mu_2,                                                      &
                           grid%muu, cqu, grid%muv, cqv, grid%mudf,                  &
                           grid%msfux, grid%msfuy, grid%msfvx, grid%msfvx_inv, grid%msfvy,    &
                           grid%rdx, grid%rdy, dts_rk,                                        &
                           grid%cf1, grid%cf2, grid%cf3, grid%fnm, grid%fnp,            &
                           grid%emdiv,                                                        &
                           grid%rdnw, config_flags,grid%spec_zone,                         &
                           config_flags%non_hydrostatic, config_flags%top_lid,                &
                           ids, ide, jds, jde, kds, kde,                                      &
                           ims, ime, jms, jme, kms, kme,                                      &
                           grid%i_start(ij), grid%i_end(ij),                                  &
                           grid%j_start(ij), grid%j_end(ij),                                  &
                           k_start    , k_end                                                )
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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite  = .FALSE.                                &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
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
!  grid%u_2               x
!  grid%v_2                          x
!
#     include "HALO_EM_C.inc"
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        !  advance the mass in the column, theta, and calculate ww

BENCH_START(advance_mu_t_tim)
        CALL advance_mu_t( grid%ww, ww1, grid%u_2, grid%u_save, grid%v_2, grid%v_save, &
                           grid%mu_2, grid%mut, muave, grid%muts, grid%muu, grid%muv,  &
                           grid%mudf, grid%ru_m, grid%rv_m, grid%ww_m,                    &
                           grid%t_2, grid%t_save, t_2save, t_tend,                              &
                           mu_tend,                                                                   &
                           grid%rdx, grid%rdy, dts_rk, grid%epssm,                                    &
                           grid%dnw, grid%fnm, grid%fnp, grid%rdnw,                       &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,                         &
                           grid%msfvy, grid%msftx,grid%msfty,                                         &
                           iteration, config_flags,                                                   &
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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite  = .FALSE.                                &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
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

           CALL spec_bdyupdate(grid%t_2, t_tend, dts_rk,      &
                               't'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(grid%mu_2, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
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
          CALL advance_w( grid%w_2, rw_tend, grid%ww, w_save, grid%u_2, grid%v_2,       &
                          grid%mu_2, grid%mut, muave, grid%muts,           &
                          t_2save, grid%t_2, grid%t_save,             &
                          grid%ph_2, ph_save, grid%phb, ph_tend,      &
                          grid%ht, c2a, cqw, grid%alt, grid%alb,           &
                          a, alpha, gamma,                  &
                          grid%rdx, grid%rdy, dts_rk, t0, grid%epssm,      &
                          grid%dnw, grid%fnm, grid%fnp, grid%rdnw, grid%rdn,         &
                          grid%cf1, grid%cf2, grid%cf3, grid%msftx, grid%msfty, &
                          config_flags,  config_flags%top_lid,                    &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite  = .FALSE.                                &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
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
         CALL sumflux ( grid%u_2, grid%v_2, grid%ww,                  &
                        grid%u_save, grid%v_save, ww1,                   &
                        grid%muu, grid%muv,                              &
                        grid%ru_m, grid%rv_m, grid%ww_m, grid%epssm,  &
                        grid%msfux, grid% msfuy, grid%msfvx,                   &
                        grid%msfvx_inv, grid%msfvy,                            &
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
             CALL spec_bdyupdate_ph( ph_save, grid%ph_2, ph_tend, mu_tend, grid%muts, dts_rk, &
                                     'h'         , config_flags, &
                                     grid%spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end             )
             IF( config_flags%specified ) THEN
               CALL zero_grad_bdy ( grid%w_2,                        &
                                    'w'         , config_flags, &
                                    grid%spec_zone,                  &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),         &
                                    grid%j_start(ij), grid%j_end(ij),         &
                                    k_start    , k_end             )
             ELSE
               CALL spec_bdyupdate   ( grid%w_2, rw_tend, dts_rk,       &
                                       'h'         , config_flags, &
                                       grid%spec_zone,                  &
                                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                       grid%i_start(ij), grid%i_end(ij),         &
                                       grid%j_start(ij), grid%j_end(ij),         &
                                       k_start    , k_end             )
             ENDIF
          ENDIF
BENCH_END(spec_bdynhyd_tim)
        ENDIF

BENCH_START(cald_p_rho_tim)
        CALL calc_p_rho( grid%al, grid%p, grid%ph_2,                      &
                         grid%alt, grid%t_2, grid%t_save, c2a, pm1,       &
                         grid%mu_2, grid%muts, grid%znu, t0,              &
                         grid%rdnw, grid%dnw, grid%smdiv,                 &
                         config_flags%non_hydrostatic, iteration,       &
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
!  grid%ph_2   x
!  grid%al     x
!  grid%p      x
!
!  2D variables (x,y)
!
!  grid%mu_2   x
!  grid%muts   x
!  grid%mudf   x

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
                          grid%muts, muus, muvs,                 &
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
                              grid%mut, grid%muts, grid%muu, muus, grid%muv, muvs,  & 
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

         IF (rk_step == 3) THEN

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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite  = .FALSE.                                &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
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

  IF (config_flags%pd_moist .and. (rk_step == rk_order)) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_m
       CALL rk_update_scalar_pd( im, im,                                   &
                                 moist_old(ims,kms,jms,im),                &
                                 moist_tend(ims,kms,jms,im),               &
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
   if(config_flags%pd_moist) then
#ifndef RSL
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_MOIST_OLD_E_5.inc"
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_MOIST_OLD_E_7.inc"
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

   END IF  ! end if for pd_moist

! scalars

  IF (config_flags%pd_scalar .and. (rk_step == rk_order)) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_s
       CALL rk_update_scalar_pd( im, im,                                  &
                                 scalar_old(ims,kms,jms,im),              &
                                 scalar_tend(ims,kms,jms,im),             &
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
   if(config_flags%pd_scalar) then
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
      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
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

   END IF  ! end if for pd_scalar

! chem

  IF (config_flags%pd_chem .and. (rk_step == rk_order)) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       do im = PARAM_FIRST_SCALAR, num_3d_c
       CALL rk_update_scalar_pd( im, im,                                  &
                                 chem_old(ims,kms,jms,im),                &
                                 chem_tend(ims,kms,jms,im),               &
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
   if(config_flags%pd_chem) then
#ifndef RSL
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_CHEM_OLD_E_5.inc"
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_CHEM_OLD_E_7.inc"
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
#  include "PERIOD_BDY_EM_CHEM_OLD.inc"
#endif

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles
      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
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

  END IF  ! end if for pd_chem

! tke

  IF (config_flags%pd_tke .and. (rk_step == rk_order) &
      .and. (config_flags%km_opt .eq. 2)                ) THEN

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles
       CALL wrf_debug ( 200 , ' call rk_update_scalar_pd' )
       CALL rk_update_scalar_pd( 1, 1,                                    &
                                 grid%tke_1,                           &
                                 tke_tend(ims,kms,jms),                   &
                                 grid%mu_1, grid%mu_1, grid%mub, &
                                 rk_step, dt_rk, grid%spec_zone,          &
                                 config_flags,                            &
                                 ids, ide, jds, jde, kds, kde,            &
                                 ims, ime, jms, jme, kms, kme,            &
                                 grid%i_start(ij), grid%i_end(ij),        &
                                 grid%j_start(ij), grid%j_end(ij),        &
                                 k_start    , k_end                      )
   END DO
   !$OMP END PARALLEL DO

!---------------------- positive definite bc call
#ifdef DM_PARALLEL
   if(config_flags%pd_tke) then
#ifndef RSL
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
#     include "HALO_EM_TKE_OLD_E_5.inc"
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
#     include "HALO_EM_TKE_OLD_E_7.inc"
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

   END IF  ! end if for pd_tke

#ifdef DM_PARALLEL
!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!
! grid%ru_m      x
! grid%rv_m      x
! grid%ww_m      x
! grid%mut       x
!
!--------------------------------------------------------------

#  include "HALO_EM_D.inc"
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


  moist_scalar_advance: IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN

   moist_variable_loop: do im = PARAM_FIRST_SCALAR, num_3d_m

   if (grid%adv_moist_cond .or. im==p_qv ) then

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )

BENCH_START(rk_scalar_tend_tim)

       CALL rk_scalar_tend (  im, im, config_flags,                                      &
                              rk_step, dt_rk,                                            &
                              grid%ru_m, grid%rv_m, grid%ww_m,                  &
                              grid%mut, grid%mub, grid%mu_1,                    &
                              grid%alt,                                               &
                              moist_old(ims,kms,jms,im),                                 &
                              moist(ims,kms,jms,im),                                     &
                              moist_tend(ims,kms,jms,im),                                &
                              advect_tend,grid%rqvften,                                  &
                              grid%qv_base, .true., grid%fnm, grid%fnp,            &
                              grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,         &
                              grid%msfvy, grid%msftx,grid%msfty,                         & 
                              grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                              grid%kvdif, grid%xkhh,                                     &
                              grid%diff_6th_opt, grid%diff_6th_factor,                   &
                              config_flags%pd_moist,            &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

BENCH_END(rk_scalar_tend_tim)

BENCH_START(rlx_bdy_scalar_tim)
     IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN 
         IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN
            CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
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
   !$OMP PRIVATE ( ij )

   moist_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )

BENCH_START(update_scal_tim)
       CALL rk_update_scalar( im, im,                           &
                              moist_old(ims,kms,jms,im),        &
                              moist(ims,kms,jms,im),            &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend, grid%msftx,grid%msfty, &
                              grid%mu_1, grid%mu_2, grid%mub,                  &
                              rk_step, dt_rk, grid%spec_zone,        &
                              config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
BENCH_END(update_scal_tim)

BENCH_START(flow_depbdy_tim)
       IF( config_flags%specified ) THEN
         IF(im .ne. P_QV)THEN
           CALL flow_dep_bdy  (  moist(ims,kms,jms,im),                     &
                               grid%ru_m, grid%rv_m, config_flags, &
                               grid%spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),                      &
                               grid%j_start(ij), grid%j_end(ij),                      &
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
   !$OMP PRIVATE ( ij )

   tke_tile_loop_1: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
     CALL rk_scalar_tend ( 1, 1, config_flags,                                        &
                           rk_step, dt_rk,                                            &
                           grid%ru_m, grid%rv_m, grid%ww_m,                  &
                           grid%mut, grid%mub, grid%mu_1,                    &
                           grid%alt,                                               &
                           grid%tke_1,                                             &
                           grid%tke_2,                                             &
                           tke_tend(ims,kms,jms),                                     &
                           advect_tend,grid%rqvften,                                  &
                           grid%qv_base, .false., grid%fnm, grid%fnp,           &
                           grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv,         &
                           grid%msfvy, grid%msftx,grid%msfty,                         &
                           grid%rdx, grid%rdy, grid%rdn, grid%rdnw, grid%khdif, &
                           grid%kvdif, grid%xkhh,                                     &
                           grid%diff_6th_opt, grid%diff_6th_factor,                   &
                           config_flags%pd_tke,              &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

   ENDDO tke_tile_loop_1
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_update_scalar' )
     CALL rk_update_scalar( 1, 1,                             &
                            grid%tke_1,               &
                            grid%tke_2,               &
                            tke_tend(ims,kms,jms),            &
                            advect_tend,grid%msftx,grid%msfty,&
                            grid%mu_1, grid%mu_2, grid%mub,                  &
                            rk_step, dt_rk, grid%spec_zone,        &
                            config_flags,     &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               ) 

! bound the tke (greater than 0, less than tke_upper_bound)

     CALL bound_tke( grid%tke_2, grid%tke_upper_bound, &
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

   END IF TKE_advance
BENCH_END(tke_adv_tim)

#ifdef WRF_CHEM
!  next the chemical species
BENCH_START(chem_adv_tim)
  chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

   chem_variable_loop: do ic = PARAM_FIRST_SCALAR, num_3d_c

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend in chem_tile_loop_1' )
       CALL rk_scalar_tend ( ic, ic, config_flags,                              &
                             rk_step, dt_rk,                                    &
                             grid%ru_m, grid%rv_m, grid%ww_m,          &
                             grid%mut, grid%mub, grid%mu_1,            &
                             grid%alt,                                       &
                             chem_old(ims,kms,jms,ic),                          &
                             chem(ims,kms,jms,ic),                              &
                             chem_tend(ims,kms,jms,ic),                         &
                             advect_tend,grid%rqvften,                          &
                             grid%qv_base, .false., grid%fnm, grid%fnp,   &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                             grid%msfvy, grid%msftx,grid%msfty,                 &
                             grid%rdx, grid%rdy, grid%rdn, grid%rdnw,     &
                             grid%khdif, grid%kvdif, grid%xkhh,                 &
                             grid%diff_6th_opt, grid%diff_6th_factor,           &
                             config_flags%pd_chem,                              &
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
       if(ic.eq.1)CALL wrf_debug ( 10 , ' have_bcs_chem' )

         CALL relax_bdy_scalar ( chem_tend(ims,kms,jms,ic),             &
                                 chem(ims,kms,jms,ic),  grid%mut,            &
                                 chem_bxs(jms,kms,1,ic),chem_bxe(jms,kms,1,ic),chem_bys(ims,kms,1,ic),chem_bye(ims,kms,1,ic), &
                                 chem_btxs(jms,kms,1,ic),chem_btxe(jms,kms,1,ic),chem_btys(ims,kms,1,ic),chem_btye(ims,kms,1,ic), &
                                 config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                 grid%dtbc, grid%fcx, grid%gcx,             &
                                 config_flags,               &
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                         )
         CALL spec_bdy_scalar  ( chem_tend(ims,kms,jms,ic),                 &
                                 chem_bxs(jms,kms,1,ic),chem_bxe(jms,kms,1,ic),chem_bys(ims,kms,1,ic),chem_bye(ims,kms,1,ic), &
                                 chem_btxs(jms,kms,1,ic),chem_btxe(jms,kms,1,ic),chem_btys(ims,kms,1,ic),chem_btye(ims,kms,1,ic), &
                                 config_flags%spec_bdy_width, grid%spec_zone,                 &
                                 config_flags,               &
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                             )
     ENDIF

   ENDDO chem_tile_loop_1

   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
       CALL rk_update_scalar( ic, ic,                           &
                              chem_old(ims,kms,jms,ic),         &  ! was chem_1
                              chem(ims,kms,jms,ic),             &
                              chem_tend(ims,kms,jms,ic),        &
                              advect_tend, grid%msftx, grid%msfty, &
                              grid%mu_1, grid%mu_2, grid%mub,                  &
                              rk_step, dt_rk, grid%spec_zone,        &
                              config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )


       IF( config_flags%specified  ) THEN
! come back to this and figure out why two different routines are needed. JM 20041203
!#ifndef WRF_CHEM
!!$           CALL flow_dep_bdy  ( chem(ims,kms,jms,ic),       &
!!$                                grid%ru_m, grid%rv_m, config_flags,   &
!!$                                grid%spec_zone,             &
!!$                                ids,ide, jds,jde, kds,kde,  & ! domain dims
!!$                                ims,ime, jms,jme, kms,kme,  & ! memory dims
!!$                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
!!$                                grid%i_start(ij), grid%i_end(ij),  &
!!$                                grid%j_start(ij), grid%j_end(ij),  &
!!$                                k_start, k_end                    )
!#else
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
!#endif
       ENDIF


   ENDDO chem_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO chem_variable_loop

 ENDIF chem_scalar_advance
BENCH_END(chem_adv_tim)
#endif

!  next the other scalar species
  other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

   scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
       CALL rk_scalar_tend ( is, is, config_flags,                            &
                             rk_step, dt_rk,                                  &
                             grid%ru_m, grid%rv_m, grid%ww_m,        &
                             grid%mut, grid%mub, grid%mu_1,          &
                             grid%alt,                                     &
                             scalar_old(ims,kms,jms,is),                      &
                             scalar(ims,kms,jms,is),                          &
                             scalar_tend(ims,kms,jms,is),                     &
                             advect_tend,grid%rqvften,                        &
                             grid%qv_base, .false., grid%fnm, grid%fnp, &
                             grid%msfux,grid%msfuy, grid%msfvx, grid%msfvx_inv, &
                             grid%msfvy, grid%msftx,grid%msfty,               &
                             grid%rdx, grid%rdy, grid%rdn, grid%rdnw,   &
                             grid%khdif, grid%kvdif, grid%xkhh,               &
                             grid%diff_6th_opt, grid%diff_6th_factor,         &
                             config_flags%pd_scalar,           &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

     IF( config_flags%nested .and. (rk_step == 1) ) THEN

       IF (is .eq. P_QNDROP .OR. is .eq. P_QNI          & 
                 .or. is .eq. P_QNS                   &
                 .or. is .eq. P_QNR                   &
                 .or. is .eq. P_QNG) THEN             

         CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),            &
                                 scalar(ims,kms,jms,is),  grid%mut,         &
                                 scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is), &
                                 scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is), &
                                 scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is), &
                                 scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is), &
                                 config_flags%spec_bdy_width, grid%spec_zone, grid%relax_zone, &
                                 grid%dtbc, grid%fcx, grid%gcx,             &
                                 config_flags,               &
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( scalar_tend(ims,kms,jms,is),                &
                                 scalar_bxs(jms,kms,1,is),scalar_bxe(jms,kms,1,is), &
                                 scalar_bys(ims,kms,1,is),scalar_bye(ims,kms,1,is), &
                                 scalar_btxs(jms,kms,1,is),scalar_btxe(jms,kms,1,is), &
                                 scalar_btys(ims,kms,1,is),scalar_btye(ims,kms,1,is), &
                                 config_flags%spec_bdy_width, grid%spec_zone,                 &
                                 config_flags,               &
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ENDIF

     ENDIF ! b.c test for chem nested boundary condition

   ENDDO scalar_tile_loop_1
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
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


       IF( config_flags%specified ) THEN
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


   ENDDO scalar_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO scalar_variable_loop

 ENDIF other_scalar_advance

 !  update the pressure and density at the new time level

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

BENCH_START(calc_p_rho_tim)

     CALL calc_p_rho_phi( moist, num_3d_m,                &
                          grid%al, grid%alb, grid%mu_2, grid%muts,              &
                          grid%ph_2, grid%p, grid%pb, grid%t_2,                 &
                          p0, t0, grid%znu, grid%dnw, grid%rdnw,           &
                          grid%rdn, config_flags%non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

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
             CALL couple_scalars_for_filter ( FIELD=moist(:,:,:,im)           &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
             CALL pxft ( grid=grid                                            &
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
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=moist(:,:,:,im)         &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
          END DO
       END IF

       IF ( num_3d_c >= PARAM_FIRST_SCALAR ) THEN
          CALL wrf_debug ( 200 , ' call filter chem ' )
          DO im = PARAM_FIRST_SCALAR, num_3d_c
             CALL couple_scalars_for_filter ( FIELD=chem(:,:,:,im)            &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
             CALL pxft ( grid=grid                                            &
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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=chem(:,:,:,im)          &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
          END DO
       END IF

       IF ( num_3d_s >= PARAM_FIRST_SCALAR ) THEN
          CALL wrf_debug ( 200 , ' call filter scalar ' )
          DO im = PARAM_FIRST_SCALAR, num_3d_s
             CALL couple_scalars_for_filter ( FIELD=scalar(:,:,:,im)          &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
             CALL pxft ( grid=grid                                            &
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
                 ,flag_scalar        = im                                     &
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )
             CALL uncouple_scalars_for_filter ( FIELD=scalar(:,:,:,im)        &
                 ,MU=grid%mu_2 , MUB=grid%mub                           &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             )
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
!                              * * * * *
!            *        * * *    * * * * *
!          * + *      * + *    * * + * * 
!            *        * * *    * * * * *
!                              * * * * *
!
!
! grid%u_2                              x
! grid%v_2                              x
! grid%w_2                              x
! grid%t_2                              x
! grid%ph_2                             x
! grid%al         x
!
!  2D variable
! grid%mu_2       x
!
!  4D variable
! moist               x
! chem                x
!scalar               x

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
#  include "PERIOD_BDY_EM_SCALAR2.inc"
#endif

BENCH_START(bc_end_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    tile_bc_loop_1: DO ij = 1 , grid%num_tiles


      CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

      CALL rk_phys_bc_dry_2( config_flags,                         &
                             grid%u_2, grid%v_2, grid%w_2,                    &
                             grid%t_2, grid%ph_2, grid%mu_2,                  &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

BENCH_START(diag_w_tim)
     IF (.not. config_flags%non_hydrostatic) THEN
     CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                      grid%u_2, grid%v_2, grid%ht,                           &
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
        IF ( (config_flags%pd_tke) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_5.inc"
        ELSE
#         include "HALO_EM_TKE_3.inc"
        ENDIF
      ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
        IF ( (config_flags%pd_tke) .and. (rk_step == rk_order-1) ) THEN
#         include "HALO_EM_TKE_7.inc"
        ELSE
#         include "HALO_EM_TKE_5.inc"
        ENDIF
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
        CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF

#if 0
   IF (config_flags%km_opt .eq. 2) THEN
#      include  "HALO_EM_TKE_F.inc"
   ENDIF
#endif

   if ( num_moist .ge. PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_moist) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_5.inc"
       ELSE
#        include "HALO_EM_MOIST_E_3.inc"
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_moist) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_MOIST_E_7.inc"
       ELSE
#        include "HALO_EM_MOIST_E_5.inc"
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_chem) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_5.inc"
       ELSE
#        include "HALO_EM_CHEM_E_3.inc"
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_chem) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_CHEM_E_7.inc"
       ELSE
#        include "HALO_EM_CHEM_E_5.inc"
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then
     IF      ( config_flags%h_sca_adv_order <= 4 ) THEN
       IF ( (config_flags%pd_scalar) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_5.inc"
       ELSE
#        include "HALO_EM_SCALAR_E_3.inc"
       END IF
     ELSE IF ( config_flags%h_sca_adv_order <= 6 ) THEN
       IF ( (config_flags%pd_scalar) .and. (rk_step == rk_order-1) ) THEN
#        include "HALO_EM_SCALAR_E_7.inc"
       ELSE
#        include "HALO_EM_SCALAR_E_5.inc"
       END IF
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_sca_adv_order = ',config_flags%h_sca_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
#endif

   ENDIF rk_step_1_check


!**********************************************************
!
!  end of RK predictor-corrector loop
!
!**********************************************************

 END DO Runge_Kutta_loop

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

BENCH_START(advance_ppt_tim)
      CALL wrf_debug ( 200 , ' call advance_ppt' )
      CALL advance_ppt(grid%rthcuten,grid%rqvcuten,grid%rqccuten,grid%rqrcuten, &
                     grid%rqicuten,grid%rqscuten,grid%rainc,grid%raincv,grid%pratec, grid%nca,    &
                     grid%htop,grid%hbot,grid%cutop,grid%cubot,                 &
                     grid%cuppt, grid%dt, config_flags,                   &
                     ids,ide, jds,jde, kds,kde,             &
                     ims,ime, jms,jme, kms,kme,             &
                     grid%i_start(ij), grid%i_end(ij),      &
                     grid%j_start(ij), grid%j_end(ij),      &
                     k_start    , k_end                    )
BENCH_END(advance_ppt_tim)

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
       CALL moist_physics_prep_em( grid%t_2, grid%t_1, t0, rho,                &
                                   grid%al, grid%alb, grid%p, p8w, p0, grid%pb,          &
                                   grid%ph_2, grid%phb, th_phy, pi_phy, p_phy, &
                                   grid%z, z_at_w, dz8w,                  &
                                   dtm, grid%h_diabatic,                  &
                                   config_flags,grid%fnm, grid%fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )
BENCH_END(moist_physics_prep_tim)
   END DO scalar_tile_loop_1a
   !$OMP END PARALLEL DO

       CALL wrf_debug ( 200 , ' call microphysics_driver' )

       grid%sr = 0.
       specified_bdy = config_flags%specified .OR. config_flags%nested
       channel_bdy = config_flags%specified .AND. config_flags%periodic_x

#if 0
BENCH_START(microswap_1)
! for load balancing; communication to redistribute the points
   IF ( config_flags%mp_physics .EQ. ETAMPNEW ) THEN
#include "SWAP_ETAMP_NEW.inc"
   ELSE IF ( config_flags%mp_physics .EQ. WSM3SCHEME ) THEN
#include "SWAP_WSM3.inc"
   ENDIF
BENCH_END(microswap_1)
#endif

BENCH_START(micro_driver_tim)

       CALL microphysics_driver(                                          &
     &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy              &
     &        ,DZ8W=dz8w          ,F_ICE_PHY=grid%f_ice_phy                    &
     &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=grid%lowlyr      &
     &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy      &
     &        ,RHO=rho            ,SPEC_ZONE=grid%spec_zone                    &
     &        ,SR=grid%sr              ,TH=th_phy                              &
     &        ,WARM_RAIN=grid%warm_rain                                   &
     &        ,T8W=t8w                                                    &
     &        ,CLDFRA=grid%cldfra, EXCH_H=grid%exch_h &
     &        ,NSOURCE=grid%qndropsource                                  &
#ifdef WRF_CHEM
     &        ,QLSINK=grid%qlsink,CLDFRA_OLD=grid%cldfra_old             &
     &        ,PRECR=grid%precr, PRECI=grid%preci, PRECS=grid%precs, PRECG=grid%precg &
     &        ,CHEM_OPT=config_flags%chem_opt, PROGN=config_flags%progn   &
#endif
     &        ,XLAND=grid%xland                                           &
     &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy        &
     &        ,F_RAIN_PHY=grid%f_rain_phy                                      &
     &        ,F_RIMEF_PHY=grid%f_rimef_phy                                    &
     &        ,MP_PHYSICS=config_flags%mp_physics                         &
     &        ,ID=grid%id                                                 &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
#ifdef RUN_ON_GPU
     &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe          &
#endif
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
     &        ,NAER=grid%naer                                             &
                 ! Optional
     &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv                            &
     &        , SNOWNC=grid%snownc, SNOWNCV=grid%snowncv                            &
     &        , GRAUPELNC=grid%graupelnc, GRAUPELNCV=grid%graupelncv                &
     &        , W=grid%w_2, Z=grid%z, HT=grid%ht                                         &
     &        , MP_RESTART_STATE=grid%mp_restart_state                         &
     &        , TBPVS_STATE=grid%tbpvs_state                                   & ! etampnew
     &        , TBPVS0_STATE=grid%tbpvs0_state                                 & ! etampnew
     &        , QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV              &
     &        , QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC              &
     &        , QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR              &
     &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI              &
     &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS              &
     &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG              &
     &        , QNDROP_CURR=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP &
     &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI         &
     &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT             &
     &        , QNS_CURR=scalar(ims,kms,jms,P_QNS), F_QNS=F_QNS         &  
     &        , QNR_CURR=scalar(ims,kms,jms,P_QNR), F_QNR=F_QNR         &  
     &        , QNG_CURR=scalar(ims,kms,jms,P_QNG), F_QNG=F_QNG         &  
     &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten            &  
     &        , qicuten=grid%rqicuten,mu=grid%mut                    &  
     &        , HAIL=config_flags%gsfcgce_hail                          & ! for gsfcgce
     &        , ICE2=config_flags%gsfcgce_2ice                          & ! for gsfcgce
                                                                          )
BENCH_END(micro_driver_tim)

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

       CALL wrf_debug ( 200 , ' call moist_physics_finish' )
BENCH_START(moist_phys_end_tim)

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, im, ii, jj, kk )

   DO ij = 1 , grid%num_tiles

       IF ( config_flags%periodic_x ) THEN
         its = max(grid%i_start(ij),ids)
         ite = min(grid%i_end(ij),ide-1)
       ELSE
         its = max(grid%i_start(ij),ids+sz)
         ite = min(grid%i_end(ij),ide-1-sz)
       ENDIF
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL microphysics_zero_out (                                    &
                     moist , num_moist , config_flags ,                &
                     ids, ide, jds, jde, kds, kde,                     &
                     ims, ime, jms, jme, kms, kme,                     &
                     its, ite, jts, jte,                               &
                     k_start    , k_end                                )


       CALL moist_physics_finish_em( grid%t_2, grid%t_1, t0, grid%muts, th_phy,       &
                                     grid%h_diabatic, dtm, config_flags,    &
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
             DO jj = jps, MIN(jpe,jde-1)
             DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
                moist(ii,kk,jj,im)=moist(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             END DO
             END DO
             END DO

               CALL pxft ( grid=grid                                          &
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
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

             DO jj = jps, MIN(jpe,jde-1)
             DO kk = kps, MIN(kpe,kde-1)
             DO ii = ips, MIN(ipe,ide-1)
                moist(ii,kk,jj,im)=moist(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
             END DO
             END DO
             END DO
          END DO
       END IF
     END IF

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

       CALL calc_p_rho_phi( moist, num_3d_m,                &
                            grid%al, grid%alb, grid%mu_2, grid%muts,              &
                            grid%ph_2, grid%p, grid%pb, grid%t_2,                 &
                            p0, t0, grid%znu, grid%dnw, grid%rdnw,           &
                            grid%rdn, config_flags%non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            its, ite, jts, jte,               &
                            k_start    , k_end               )

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
     CALL diagnose_w( ph_tend, grid%ph_2, grid%ph_1, grid%w_2, grid%muts, dt_rk,  &
                      grid%u_2, grid%v_2, grid%ht,                           &
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
           DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
           DO ii = ips, MIN(ipe,ide-1)
              chem(ii,kk,jj,im)=chem(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
           END DO
           END DO
           END DO

             CALL pxft ( grid=grid                                            &
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
                 ,flag_scalar        = 0                                      &
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

           DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
           DO ii = ips, MIN(ipe,ide-1)
              chem(ii,kk,jj,im)=chem(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
           END DO
           END DO
           END DO
        END DO chem_filter_loop
     END IF

     IF ( num_3d_s >= PARAM_FIRST_SCALAR ) then
        scalar_filter_loop: DO im = PARAM_FIRST_SCALAR, num_3d_s
           DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
           DO ii = ips, MIN(ipe,ide-1)
              scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)*(grid%mu_2(ii,jj)+grid%mub(ii,jj))
           END DO
           END DO
           END DO

             CALL pxft ( grid=grid                                            &
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
                 ,flag_scalar        = im                                     &
                 ,positive_definite=.FALSE.                                   &
                 ,moist=moist,chem=chem,scalar=scalar                         &
                 ,fft_filter_type = config_flags%fft_filter_type              &
                 ,dclat = dclat                                               &
                 ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
                 ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
                 ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
                 ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
                 ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

           DO jj = jps, MIN(jpe,jde-1)
           DO kk = kps, MIN(kpe,kde-1)
           DO ii = ips, MIN(ipe,ide-1)
              scalar(ii,kk,jj,im)=scalar(ii,kk,jj,im)/(grid%mu_2(ii,jj)+grid%mub(ii,jj))
           END DO
           END DO
           END DO
        END DO scalar_filter_loop
     END IF

   END IF

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

   IF( config_flags%specified .or. config_flags%nested ) THEN 
     grid%dtbc = grid%dtbc + grid%dt
   ENDIF

! calculate some model diagnostics.

         CALL wrf_debug ( 200 , ' call diagnostic_driver' )
   
         CALL diagnostic_output_calc(                                     &
     &              DPSDT=grid%dpsdt   ,DMUDT=grid%dmudt                  &
     &             ,P_PHY=p_phy   ,PK1M=grid%pk1m                         &
     &             ,MU_2=grid%mu_2  ,MU_2M=grid%mu_2m                  &
     &             ,U=grid%u_2    ,V=grid%v_2                       &
     &             ,RAINCV=grid%raincv    ,RAINNCV=grid%rainncv           &
     &             ,RAINC=grid%rainc    ,RAINNC=grid%rainnc               &
     &             ,HFX=grid%hfx   ,SFCEVP=grid%sfcevp    ,LH=grid%lh     &
     &             ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width      &
     &             ,XTIME=grid%xtime                                      &
                 ! Selection flag
     &             ,DIAG_PRINT=config_flags%diag_print                    &
                 ! Dimension arguments
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
     &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
     &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &             ,NUM_TILES=grid%num_tiles                              &
     &                                                          )

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
   if ( num_moist >= PARAM_FIRST_SCALAR  ) then
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
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
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
   endif
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then
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
   endif
#endif

   CALL wrf_debug ( 200 , ' call end of solve_em' )

! Finish timers if compiled with -DBENCH.
#include <bench_solve_em_end.h>

   RETURN

END SUBROUTINE solve_em

SUBROUTINE adapt_timestep(grid, config_flags)

!--------------------------------------------------------------------------
!<DESCRIPTION>
!<pre>
!
! This routine sets the time step based on the cfl condition.  It's used to
!   dynamically adapt the timestep as the model runs.
!
! T. Hutchinson, WSI
! March 2007
!
!</pre>
!</DESCRIPTION>
!--------------------------------------------------------------------------

! Driver layer modules
  USE module_domain
  USE module_configure
  USE module_dm

  IMPLICIT NONE

  TYPE(domain) , TARGET , INTENT(INOUT)      :: grid
  TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  LOGICAL                                    :: use_last2
  REAL                                       :: curr_secs
  REAL                                       :: max_increase_factor
  REAL                                       :: time_to_output, &
                                                time_to_bc
  INTEGER                                    :: idex=0, jdex=0
  INTEGER                                    :: rc
  TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval, dtInterval
  INTEGER                                    :: num_small_steps
  integer                                    :: tile
  LOGICAL                                    :: stepping_to_bc
  INTEGER                                    :: bc_time, output_time
  double precision                           :: dt
  INTEGER, PARAMETER                         :: precision = 100
  INTEGER                                    :: dt_num, dt_den, dt_whole
  REAL                                       :: factor
  INTEGER                                    :: num, den
  TYPE(WRFU_TimeInterval)                    :: last_dtInterval
  REAL                                       :: real_time

  !
  ! If use_last2 is true, this routine will use the time step
  !   from 2 steps ago to compute the next time step.  This
  !   is used along with step_to_output and step_to_bc

  use_last2 = .FALSE.

  !
  ! For nests, we only want to change nests' time steps when the time is 
  !    conincident with the parent's time.  So, if dtbc is not
  !    zero, simply return and leave the last time step in place.
  !
  if (config_flags%nested) then
     if (abs(grid%dtbc) > 0.0001) then
        return
     endif
  endif

  last_dtInterval = grid%last_dtInterval

  !
  ! Get current time
  !

  tmpTimeInterval = domain_get_current_time ( grid ) - &
       domain_get_start_time ( grid )

  !
  ! Calculate current time in seconds since beginning of model run.
  !   Unfortunately, ESMF does not seem to have a way to return
  !   floating point seconds based on a TimeInterval.  So, we will
  !   calculate it here--but, this is not clean!!
  !
  curr_secs = real_time(tmpTimeInterval)

  !
  ! Calculate the maximum allowable increase in the time step given
  !   the user input max_step_increase_pct value and the nest ratio.
  !
  if (config_flags%nested) then
     max_increase_factor = 1. + &
          grid%parent_time_step_ratio * grid%max_step_increase_pct / 100.     
  else
     max_increase_factor = 1. + grid%max_step_increase_pct / 100.
  endif

  !
  ! If this is the first time step of the model run (indicated by current_time 
  !    eq start_time), then set the time step to the input starting_time_step.
  !
  ! Else, calculate the time step based on cfl.
  !
  if (domain_get_current_time ( grid ) .eq. domain_get_start_time ( grid )) then
     CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=1)
     curr_secs = 0
     CALL WRFU_TimeIntervalSet(last_dtInterval, Sn=0, Sd=1)

  else
     if (grid%max_cfl_val < 0.001) then 
        !
        ! If the max_cfl_val is small, then we increase dtInterval the maximum
        !    amount allowable.
        !
        num = INT(max_increase_factor * precision + 0.5)
        den = precision
        dtInterval = last_dtInterval * num / den

     else
        !
        ! If the max_cfl_val is greater than the user input target cfl, we 
        !    reduce the time step, 
        ! else, we increase it.
        !
        if (grid%max_cfl_val .gt. grid%target_cfl) then
           !
           ! If we are reducing the time step, we go below target cfl by half the
           !   difference between max and target.
           !   This tends to keep the model more stable.
           !
           
           factor = ( (grid%target_cfl - 0.5 * &
                (grid%max_cfl_val - grid%target_cfl) ) / grid%max_cfl_val )
           num = INT(factor * precision + 0.5)
           den = precision

           dtInterval = last_dtInterval * num / den

        else
           !
           ! Linearly increase dtInterval (we'll limit below)
           !
           
           factor = grid%target_cfl / grid%max_cfl_val
           num = INT(factor * precision + 0.5)
           den = precision
           dtInterval = last_dtInterval * num / den
        endif

     endif

  endif

  ! Limit the increase of dtInterval to the specified input limit

  num = NINT( max_increase_factor * precision )
  den = precision
  tmpTimeInterval = last_dtInterval * num / den
  if ( (domain_get_current_time ( grid ) .ne. domain_get_start_time ( grid )) &
       .and. (dtInterval .gt. tmpTimeInterval ) ) then
     dtInterval = tmpTimeInterval
  endif

  !
  ! Here, we round off dtInterval to nearest 1/100.  This prevents
  !    the denominator from getting too large and causing overflow.
  !
  dt = real_time(dtInterval)
  num = NINT(dt * precision)
  den = precision
  CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)

  ! Limit the maximum dtInterval based on user input

  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=1)
  if (dtInterval .gt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  ! Limit the minimum dtInterval based on user input.

  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, &
       Sd=grid%parent_time_step_ratio)
  if (dtInterval .lt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  !
  ! Now, if this is a nest, we round down the time step to the nearest 
  !   value that divides evenly into the parent time step.
  !
  if (config_flags%nested) then
     ! We'll calculate real numbers to get the number of small steps:
     
     dt = real_time(dtInterval)

     num_small_steps = CEILING( grid%parents(1)%ptr%dt / dt )

#ifdef DM_PARALLEL
     call wrf_dm_maxval(num_small_steps, idex, jdex)
#endif
     dtInterval = domain_get_time_step(grid%parents(1)%ptr) / &
          num_small_steps
  endif

  !
  ! Setup the values for several variables from the tile with the
  !   minimum dt.
  !
  dt = real_time(dtInterval)

#ifdef DM_PARALLEL
  call wrf_dm_mintile_double(dt, tile)
  dt_num = dtInterval%basetime%Sn
  dt_den = dtInterval%basetime%Sd
  dt_whole = dtInterval%basetime%S
  call wrf_dm_tile_val_int(dt_num, tile)
  call wrf_dm_tile_val_int(dt_den, tile)
  call wrf_dm_tile_val_int(dt_whole, tile)
  CALL WRFU_TimeIntervalSet(dtInterval, Sn = dt_whole*dt_den + dt_num, Sd = dt_den)

  call wrf_dm_maxtile_real(grid%max_cfl_val, tile)
  call wrf_dm_maxtile_real(grid%max_vert_cfl, tile)
  call wrf_dm_maxtile_real(grid%max_horiz_cfl, tile)
#endif

  !
  ! Assure that we fall on a BC time.  Due to a bug in WRF, the time
  !   step must fall on the boundary times.
  !

  time_to_bc = grid%interval_seconds - grid%dtbc
  num = INT(time_to_bc * precision + 0.5)
  den = precision
  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)
   
  if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
       ( tmpTimeInterval .GT. dtInterval ) ) then
     dtInterval = tmpTimeInterval / 2
     
     use_last2 = .TRUE.
     stepping_to_bc = .true.

  elseif (tmpTimeInterval .LE. dtInterval) then

     bc_time = NINT ( (curr_secs + time_to_bc) / ( grid%interval_seconds ) ) &
          * ( grid%interval_seconds )
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=bc_time)
     dtInterval = tmpTimeInterval - &
          (domain_get_current_time(grid) - domain_get_start_time(grid))

     use_last2 = .TRUE.
     stepping_to_bc = .true.
  else
     stepping_to_bc = .false.
  endif

  !
  ! If the user has requested that we step to output, then
  !   assure that we fall on an output time.  We look out two time steps to
  !   avoid having a very short time step.  Very short time steps can cause model
  !   instability.
  !

  if ((grid%step_to_output_time) .and. (.not. stepping_to_bc) .and. &
       (.not. config_flags%nested)) then

     time_to_output = grid%history_interval*60 - &
          mod(curr_secs, grid%history_interval*60.0)
     num = INT(time_to_output * precision + 0.5)
     den = precision
     call WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)

     if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
          ( tmpTimeInterval .GT. dtInterval ) ) then
        dtInterval = tmpTimeInterval / 2
        use_last2 = .TRUE.

     elseif (tmpTimeInterval .LE. dtInterval) then
        !
        ! We will do some tricks here to assure that we fall exactly on an 
        !   output time.  Without the tricks, round-off error causes problems!
        !

        !
        ! Calculate output time.  We round to nearest history time to assure 
        !    we don't have any rounding error.
        !
        output_time = NINT ( (curr_secs + time_to_output) /  &
             ( grid%history_interval * 60 ) ) * (grid%history_interval * 60)
        CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=output_time)
        dtInterval = tmpTimeInterval - &
             (domain_get_current_time(grid) - domain_get_start_time(grid))

        use_last2 = .TRUE.
     endif
  endif

  if (use_last2) then
     grid%last_dtInterval = last_dtInterval
  else
     grid%last_dtInterval = dtInterval
  endif

  grid%dt = real_time(dtInterval)

  grid%last_max_vert_cfl = grid%max_vert_cfl

  !
  ! Update the clock based on the new time step
  !
  CALL WRFU_ClockSet ( grid%domain_clock,        &
       timeStep=dtInterval, &
       rc=rc )


END SUBROUTINE adapt_timestep

FUNCTION real_time( timeinterval ) RESULT ( out_time )

  USE module_domain

  IMPLICIT NONE 

! This function returns a floating point time from an input time interval
!  
! Unfortunately, the ESMF did not provide this functionality.
!
! Be careful with the output because, due to rounding, the time is only
!   approximate.
!
! Todd Hutchinson, WSI
! 4/17/2007

! !RETURN VALUE:
      REAL :: out_time

! !ARGUMENTS:
      TYPE(WRFU_TimeInterval), intent(in) :: timeinterval

      out_time = timeinterval%basetime%S + &
           REAL(timeinterval%basetime%Sn) / REAL(timeinterval%basetime%Sd)
END FUNCTION 

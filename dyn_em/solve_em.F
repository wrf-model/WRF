!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em ( grid , config_flags , &
!
#include "em_dummy_args.inc"
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
! Registry generated module
   USE module_state_description
   USE module_radiation_driver
   USE module_surface_driver
   USE module_cumulus_driver
   USE module_microphysics_driver
   USE module_pbl_driver

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   !  WRF state bcs
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! WRF state data

!#include "../phys/physics_drive.int"

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: itmpstep
   INTEGER                         :: sz

! storage for tendencies and decoupled state (generated from Registry)
#include <em_i1_decl.inc>

integer , dimension(grid%sm31:grid%em31,grid%sm33:grid%em31) :: xxxxx , yyyyy , zzzzz , xx1 , xx2 , xx3

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

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
!    - Leapfrog time integration
!      (note: the leapfrog scheme is not correctly implemented
!      for most of the physics)
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
! (4) Scalar advance for moist and chem scalar variables (and TKE)
!     within the RK sub-steps.
!     
! (5) time-split physics (after the RK step), currently this includes
!     only microphyics
!
! A more detailed description of these sections follows.
!</pre>
!</DESCRIPTION>

!
!  set leapfrog or runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  De-reference dimension information stored in the grid data structure.

  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  k_start         = kps
  k_end           = kpe

  ijds = min(ids, jds)
  ijde = max(ide, jde)

  num_3d_m        = num_moist
  num_3d_c        = num_chem

   !  Compute these starting and stopping locations for each tile and number of tiles.

  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  itimestep = itimestep + 1

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

 rk_order = config_flags%rk_ord
 leapfrog = .false.
 dts = dt/float(time_step_sound)

 IF(rk_ord == 1) leapfrog = .true.

 Runge_Kutta_loop:  DO rk_step = 1, rk_order

   !  Set the step size and number of small timesteps for
   !  each part of the timestep

   dtm = dt
   IF ( rk_order == 1 ) THEN   ! Leapfrog

       IF (step_number /= 1) THEN
         number_of_small_timesteps = 2*time_step_sound
         dt_rk = dt
         dtm = 2*dt
       ELSE
         number_of_small_timesteps = time_step_sound
         dt_rk = dt/2.
         dtm = dt
       END IF

       dts_rk = dts

   ELSE IF ( rk_order == 2 ) THEN   ! 2nd order Runge-Kutta timestep

       IF ( rk_step == 1) THEN
             dt_rk  = 0.5*dt
             dts_rk = dts
             number_of_small_timesteps = time_step_sound/2
       ELSE
             dt_rk = dt
             dts_rk = dts
             number_of_small_timesteps = time_step_sound
       ENDIF

   ELSE IF ( rk_order == 3 ) THEN ! third order Runge-Kutta

       IF ( rk_step == 1) THEN
            dt_rk = dt/3.
            dts_rk = dt_rk
            number_of_small_timesteps = 1
       ELSE IF (rk_step == 2) THEN
            dt_rk  = 0.5*dt
            dts_rk = dts
            number_of_small_timesteps = time_step_sound/2
       ELSE
            dt_rk = dt
            dts_rk = dts
            number_of_small_timesteps = time_step_sound
       ENDIF

   ELSE

      write(wrf_err_message,*)' unknown solver, error exit for dynamics_option = ',dynamics_option
      CALL wrf_error_fatal( wrf_err_message )

   END IF

!
!  Time level t is in the *_2 variable in the first part 
!  of the step, and in the *_1 variable after the predictor.
!  the latest predicted values are stored in the *_2 variables.
!
   CALL wrf_debug ( 200 , ' call rk_step_prep ' )

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL rk_step_prep  ( config_flags, rk_step,            &
                           u_2, v_2, w_2, t_2, ph_2, mu_2,   &
                           moist_2,                          &
                           ru, rv, rw, ww, php, alt, muu, muv,   &
                           mub, mut, phb, pb, p, al, alb,    &
                           cqu, cqv, cqw,                    &
                           msfu, msfv, msft,                 &
                           fnm, fnp, dnw, rdx, rdy,          &
                           num_3d_m,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start, k_end                   )

   END DO

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
!j ru     x
!j rv     x
!j ww     x
!j php    x
!j alt    x
!j ph_2   x
!j phb    x
!
!  the following are 2D (xy) variables
!
!j muu    x
!j muv    x
!j mut    x
!--------------------------------------------------------------
#    include "HALO_EM_A.inc"
#endif

! set boundary conditions on variables 
! from big_step_prep for use in big_step_proc

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A.inc"
#endif

!   CALL set_tiles ( grid , ids , ide , jds , jde , ips-1 , ipe+1 , jps-1 , jpe+1 )

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_1' )

        CALL rk_phys_bc_dry_1( config_flags, ru, rv, rw, ww,      & 
                               muu, muv, mut, php, alt, p,        &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               ips, ipe, jps, jpe, kps, kpe,      &
                               grid%i_start(ij), grid%i_end(ij),  &
                               grid%j_start(ij), grid%j_end(ij),  &
                               k_start, k_end                )

       CALL set_physical_bc3d( ph_2, 'w', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                               grid%i_start(ij), grid%i_end(ij),        &
                               grid%j_start(ij), grid%j_end(ij),        &
                               k_start, k_end                )

   END DO

    rk_step_is_one : IF (rk_step == 1) THEN ! only need to initialize diffusion tendencies

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).
 
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call init_zero_tendency' )
        CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                  ph_tendf, t_tendf, tke_tend,      &
                                  moist_tend,chem_tend,             &
                                  num_3d_m,num_3d_c,rk_step,        &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  grid%i_start(ij), grid%i_end(ij), &
                                  grid%j_start(ij), grid%j_end(ij), &
                                  k_start, k_end                   )

     END DO

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



      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call phy_prep' )
         CALL phy_prep ( config_flags,                           &
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist_2, num_3d_m, &
                         mu_3d, rho,                             &
                         th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                         p8w, t_phy, t8w, z, z_at_w,             &
                         dz8w, fnm, fnp,                         &    
                         RTHRATEN,                               &
                         RTHBLTEN, RUBLTEN, RVBLTEN,             &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,           &
                         RTHCUTEN, RQVCUTEN, RQCCUTEN,           &
                         RQRCUTEN, RQICUTEN, RQSCUTEN,           &
                         RTHFTEN,  RQVFTEN,                      &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )
      ENDDO

!  physics to implement

!      CALL set_tiles ( grid , ids , ide-1 , jds , jde-1 ips , ipe , jps , jpe )

! Open MP loops are in physics drivers
! radiation

         CALL wrf_debug ( 200 , ' call radiation_driver' )
         CALL radiation_driver(itimestep,dt,                         &
                    RTHRATENLW,RTHRATENSW,RTHRATEN,GLW,GSW,          &
                    SWDOWN,                                          &
                    XLAT,XLONG,ALBEDO,CLDFRA,EMISS,                  &
                    rho,moist_2,num_3d_m,                            &
                    p8w,p_phy,pb,pi_phy,dz8w,t_phy,t8w,              &
                    GMT,JULDAY,config_flags,RADT,STEPRA,ICLOUD,      &
                    taucldi,taucldc,warm_rain,                       &
                    XLAND,TSK,HTOP,HBOT,CUPPT,VEGFRA,SNOW,           &
                    julyr,                                           &
                    1   ,                                            &
                    TOTSWDN,TOTLWDN,RSWTOA,RLWTOA,CZMEAN,            &
                    CFRACL,CFRACM,CFRACH,                            &
                    ACFRST,NCFRST,ACFRCV,NCFRCV,                     &
                    ids,ide, jds,jde, kds,kde,                       &
                    ims,ime, jms,jme, kms,kme,                       &
                    grid%i_start, min(grid%i_end, ide-1),            &
                    grid%j_start, min(grid%j_end, jde-1),            &
                    k_start    , min(k_end,kde-1) , grid%num_tiles   )



!********* Surface driver
! surface

      CALL wrf_debug ( 200 , ' call surface_driver' )
      CALL surface_driver(                                          &
     &           ACSNOM,ACSNOW,AKHS,AKMS,ALBEDO,BR,CANWAT,CAPG        &
     &          ,CHKLOWQ,CONFIG_FLAGS,DT,DX,DZ8W,DZS,EMISS,GLW        &
     &          ,GRDFLX,GSW,GZ1OZ0,HFX,HOL,HT,IFSNOW,ISFFLX           &
     &          ,ISLTYP,ITIMESTEP,IVGTYP,LOWLYR,MAVAIL,MOIST_2,MOL    &
     &          ,NUM_SOIL_LAYERS,NUM_3D_M,P8W,PBLH,PI_PHY,PSHLTR,PSIH &
     &          ,PSIM,P_PHY,Q10,Q2,QFX,QSFC,QSHLTR,QZ0,RAINBL         &
     &          ,RAINCV,RAINNCV,REGIME,RHO,SFCEVP,SFCEXC,SFCRUNOFF    &
     &          ,SMOIS,SMSTAV,SMSTOT,SNOALB,SNOW,SNOWC,SNOWH,STEPBL   &
     &          ,T2,TH10,TH2,THC,THZ0,TH_PHY,TMN,TSHLTR,TSK,TSLB      &
     &          ,T_PHY,U10,UDRUNOFF,UST,UZ0,U_FRAME,U_PHY,V10,VEGFRA  &
     &          ,VZ0,V_FRAME,V_PHY,WARM_RAIN,WSPD,XICE,XLAND,Z,ZNT,ZS &
     &          ,CT,TKE_MYJ                                           &
     &          ,ALBBCK,LH,SH2O,SHDMAX,SHDMIN,Z0                      &
     &          ,flqc,flhc,qsg,qvg,qcg,soilt1,tsnav                   & ! for RUC LSM
     &          ,SMFR3D,KEEPFR3DFLAG                                  &
     &          ,POTEVP,SNOPCX,SOILTB                                 & ! NMM LSM only
     &          ,PSFC                                                 &
     &          ,ids,ide, jds,jde, kds,kde                            &
     &          ,ims,ime, jms,jme, kms,kme                            &
     &          ,grid%i_start, min(grid%i_end, ide-1)                 &
     &          ,grid%j_start, min(grid%j_end, jde-1)                 &
     &          ,k_start    , min(k_end,kde-1) , grid%num_tiles       )

!*********
! pbl

      CALL wrf_debug ( 200 , ' call pbl_driver' )
      CALL pbl_driver(itimestep,dt,u_frame,v_frame                    &
     &           ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &           ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &           ,TSK,XLAND,ZNT,HT                                    &
     &           ,UST,HOL,MOL,PBLH                                    &
     &           ,HFX,QFX,REGIME,GRDFLX                               &
     &           ,u_phy,v_phy,th_phy,rho,moist_2                      &
     &           ,p_phy,pi_phy,p8w,t_phy,dz8w,z                       &
     &           ,TKE_MYJ,AKHS,AKMS                                   &
     &           ,THZ0,QZ0,UZ0,VZ0,QSFC,LOWLYR                        &
     &           ,PSIM, PSIH, GZ1OZ0, WSPD, BR, CHKLOWQ               &
     &           ,config_flags,DX,num_3d_m                            &
     &           ,STEPBL,warm_rain                                    &
     &           ,KPBL,CT,LH,SNOW,XICE                                &
     &           ,ids,ide, jds,jde, kds,kde                           &
     &           ,ims,ime, jms,jme, kms,kme                           &
     &           ,grid%i_start, min(grid%i_end,ide-1)                 &
     &           ,grid%j_start, min(grid%j_end,jde-1)                 &
     &           ,k_start    , min(k_end,kde-1) , grid%num_tiles  )

! cumulus para.

          CALL wrf_debug ( 200 , ' call cumulus_driver' )

         CALL cumulus_driver(itimestep,dt,DX,num_3d_m,                 &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,               &
                     RTHRATEN,RTHBLTEN,RQVBLTEN,                       &
                     RTHFTEN,RQVFTEN,                                  &
                     u_phy,v_phy,th_phy,t_phy,w_2,moist_2,             &
                     dz8w,p8w,p_phy,pi_phy,config_flags,               &
                     W0AVG,rho,STEPCU,                                 &
                     CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,warm_rain,        &
                     apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,      &
                     apr_capme,apr_capmi,                              &
                     HTOP,HBOT,KPBL,                                   &
                     MASS_FLUX,XF_ENS,PR_ENS,ht,                       & ! for Grell-Devenyi
                     ensdim,maxiens,maxens,maxens2,maxens3,            &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start, min(grid%i_end, ide-1),             &
                     grid%j_start, min(grid%j_end, jde-1),             &
                     k_start    , min(k_end,kde-1) , grid%num_tiles    )

! calculate_phy_tend

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_phy_tend' )
          CALL calculate_phy_tend (config_flags,mut,pi_phy,            &
                     RTHRATEN,                                         &
                     RUBLTEN,RVBLTEN,RTHBLTEN,                         &
                     RQVBLTEN,RQCBLTEN,RQIBLTEN,                       &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                     RQICUTEN,RQSCUTEN,                                &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO

! tke diffusion

     IF(diff_opt .eq. 2 .OR. diff_opt .eq. 1) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call compute_diff_metrics ' )
          CALL compute_diff_metrics ( config_flags, ph_2, phb, z, rdz, rdzw, &
                                      zx, zy, rdx, rdy,                      &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A1.inc"
#endif

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
          CALL set_physical_bc3d( rdzw , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( rdz , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( z , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zx , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zy , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )

       ENDDO

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_C.inc"
#endif

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call cal_deform_and_div' )
          CALL cal_deform_and_div ( config_flags,u_2,v_2,w_2,div,        &
                                    defor11,defor22,defor33,defor12,     &
                                    defor13,defor23,                     &
                                    u_base, v_base,msfu,msfv,msft,       &
                                    rdx, rdy, dn, dnw, rdz, rdzw,        &
                                    fnm,fnp,cf1,cf2,cf3,zx,zy,           &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    grid%i_start(ij), grid%i_end(ij),    &
                                    grid%j_start(ij), grid%j_end(ij),    &
                                    k_start    , k_end                  )
       ENDDO


#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_D.inc"
#endif


! calculate tke, kmh, and kmv

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call calculate_km_kh' )
          CALL calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,     &
                                xkmh,xkmhd,xkmv,xkhh,xkhv,BN2,               &
                                khdif,kvdif,div,                             &
                                defor11,defor22,defor33,defor12,             &
                                defor13,defor23,                             &
                                tke_2(ims,kms,jms),p8w,t8w,th_phy,           &
                                t_phy,p_phy,moist_2,dn,dnw,                  &
                                dx,dy,rdz,rdzw,mix_cr_len,num_3d_m,          &
                                cf1, cf2, cf3, warm_rain,                    &
                                kh_tke_upper_bound, kv_tke_upper_bound,      &
                                ids,ide, jds,jde, kds,kde,                   &
                                ims,ime, jms,jme, kms,kme,                   &
                                grid%i_start(ij), grid%i_end(ij),            &
                                grid%j_start(ij), grid%j_end(ij),            &
                                k_start    , k_end                          )
       ENDDO

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_E.inc"
#endif

     ENDIF

#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_PHY_BC.inc"
#      include "PERIOD_BDY_EM_CHEM.inc"
#endif

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )

     DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call phy_bc' )
       CALL phy_bc (config_flags,div,defor11,defor22,defor33,            &
                            defor12,defor13,defor23,                     &
                            xkmh,xkmhd,xkmv,xkhh,xkhv,                   &
                            tke_2(ims,kms,jms),                          &
                            RUBLTEN, RVBLTEN,                            &
                            ids, ide, jds, jde, kds, kde,                &
                            ims, ime, jms, jme, kms, kme,                &
                            ips, ipe, jps, jpe, kps, kpe,                &
                            grid%i_start(ij), grid%i_end(ij),                      &
                            grid%j_start(ij), grid%j_end(ij),                      &
                            k_start    , k_end                           )
     ENDDO

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
! RUBLTEN                  x
! RVBLTEN                             x
!
! (for diff_opt >= 1)
! defor11                  x
! defor22                             x
! defor12       x
! defor13                  x
! defor23                             x
! div           x
! xkmv          x
! xkmh          x
! xkmhd         x
! xkhv          x
! xkhh          x
! tke           x
!
!-----------------------------------------------------------------------
      IF ( bl_pbl_physics .ge. 1 ) THEN
#      include "HALO_EM_PHYS_PBL.inc"
      ENDIF
      IF ( diff_opt .ge. 1 ) THEN
#      include "HALO_EM_PHYS_DIFFUSION.inc"
      ENDIF

      IF      ( h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3.inc"
      ELSE IF ( h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5.inc"
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
        CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call update_phy_ten' )
        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
                          RTHRATEN,RTHBLTEN,RTHCUTEN,RUBLTEN,RVBLTEN,  &
                          RQVBLTEN,RQCBLTEN,RQIBLTEN,                  &
                          RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,&
                          num_3d_m,config_flags,rk_step,              &
                          ids, ide, jds, jde, kds, kde,                &
                          ims, ime, jms, jme, kms, kme,                &
                          grid%i_start(ij), grid%i_end(ij),                      &
                          grid%j_start(ij), grid%j_end(ij),                      &
                          k_start, k_end                               )

      END DO

     IF( diff_opt .eq. 2 .and. km_opt .eq. 2 ) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

          CALL tke_rhs  ( tke_tend,BN2,                               &
                          config_flags,defor11,defor22,defor33,       &
                          defor12,defor13,defor23,u_2,v_2,w_2,div,    &
                          tke_2(ims,kms,jms),mut,                     &
                          th_phy,p_phy,p8w,t8w,z,fnm,fnp,             &
                          cf1,cf2,cf3,msft,xkmh,xkmv,xkhv,rdx,rdy,    &
                          dx,dy,dt,zx,zy,rdz,rdzw,dn,dnw,mix_cr_len,  &
                          ids, ide, jds, jde, kds, kde,               &
                          ims, ime, jms, jme, kms, kme,               &
                          grid%i_start(ij), grid%i_end(ij),           &
                          grid%j_start(ij), grid%j_end(ij),           &
                          k_start    , k_end                         )

       ENDDO

     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

     IF(diff_opt .eq. 2) THEN

       IF (bl_pbl_physics .eq. 0) THEN

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call vertical_diffusion_2 ' )
           CALL vertical_diffusion_2( ru_tendf, rv_tendf, rw_tendf,              &
                                      t_tendf, tke_tend,                         &
                                      moist_tend, num_3d_m,                      &
                                      chem_tend, num_3d_c,                       &
                                      u_2, v_2,                                  &
                                      t_2,u_base,v_base,t_base,qv_base,          &
                                      mut,tke_2,config_flags,                    &
                                      defor13,defor23,defor33,                   &
                                      div, moist_2, chem_2, xkmv, xkhv, km_opt,  &
                                      fnm, fnp, dn, dnw, rdz, rdzw,              &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start    , k_end                        )

         ENDDO

       ENDIF
!
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )
       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call horizontal_diffusion_2' )
         CALL horizontal_diffusion_2( t_tendf, ru_tendf, rv_tendf, rw_tendf, &
                                      tke_tend,                              &
                                      moist_tend, num_3d_m,                  &
                                      chem_tend, num_3d_c,                   &
                                      t_2, th_phy,                           &
                                      mut, tke_2, config_flags,              &
                                      defor11, defor22, defor12,             &
                                      defor13, defor23, div,                 &
                                      moist_2, chem_2,                       &
                                      msfu, msfv, msft, xkmhd, xkhh, km_opt, &
                                      rdx, rdy, rdz, rdzw,                   &
                                      fnm, fnp, cf1, cf2, cf3,               &
                                      zx, zy, dn, dnw,                       &
                                      ids, ide, jds, jde, kds, kde,          &
                                      ims, ime, jms, jme, kms, kme,          &
                                      grid%i_start(ij), grid%i_end(ij),      &
                                      grid%j_start(ij), grid%j_end(ij),      &
                                      k_start    , k_end                    )
       ENDDO

     ENDIF

     END IF rk_step_is_one

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_tendency' )
      CALL rk_tendency ( config_flags, rk_step,                           &
                         ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, u_save, v_save, w_save, ph_save,        &
                         t_save, mu_save, RTHFTEN,                        &
                         ru, rv, rw, ww,                                  &
                         u_2, v_2, w_2, t_2, ph_2,                        &
                         u_1, v_1, w_1, t_1, ph_1,                        &
                         h_diabatic, phb, t_init,                         &
                         mu_2, mut, muu, muv, mub,                        &
                         al, alt, p, pb, php, cqu, cqv, cqw,              &
                         u_base, v_base, t_base, qv_base, z_base,         &
                         msfu, msfv, msft, f, e, sina, cosa,              &
                         fnm, fnp, rdn, rdnw,                             &
                         dt, rdx, rdy, khdif, kvdif, xkmhd,               &
                         cf1, cf2, cf3, cfn, cfn1, num_3d_m,              &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )
   END DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

       CALL relax_bdy_dry ( config_flags,                                &
                            u_save, v_save, ph_save, t_save,             &
                            w_save, mu_tend,                             & 
                            ru, rv, ph_2, t_2,                           &
                            w_2, mu_2, mut,                              &
                            u_b, v_b, ph_b, t_b, w_b,                    &
                            mu_b,                                        &
                            u_bt, v_bt, ph_bt, t_bt,                     &
                            w_bt, mu_bt,                                 &
                            spec_bdy_width, spec_zone, relax_zone,       &
                            dtbc, fcx, gcx,             &
                            ijds, ijde,                 &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            ips,ipe, jps,jpe, kps,kpe,  &
                            grid%i_start(ij), grid%i_end(ij),            &
                            grid%j_start(ij), grid%j_end(ij),            &
                            k_start, k_end                              )

     ENDIF

     CALL rk_addtend_dry( ru_tend,  rv_tend,  rw_tend,  ph_tend,  t_tend,  &
                          ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                          u_save, v_save, w_save, ph_save, t_save, rk_step,&
                          h_diabatic, mut, msft, msfu, msfv,               &
                          ids,ide, jds,jde, kds,kde,                       &
                          ims,ime, jms,jme, kms,kme,                       &
                          ips,ipe, jps,jpe, kps,kpe,                       &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN 
       CALL spec_bdy_dry ( config_flags,                                    &
                           ru_tend, rv_tend, ph_tend, t_tend,               &
                           rw_tend, mu_tend,                                &
                           u_b, v_b, ph_b, t_b,                             &
                           w_b, mu_b,                                       &
                           u_bt, v_bt, ph_bt, t_bt,                         &
                           w_bt, mu_bt,                                     &
                           spec_bdy_width, spec_zone,                       &
                           ijds, ijde,                 & ! min/max(id,jd)
                           ids,ide, jds,jde, kds,kde,  & ! domain dims
                           ims,ime, jms,jme, kms,kme,  & ! memory dims
                           ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                                  )
     
     ENDIF

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

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

    ! Calculate coefficients for the vertically implicit acoustic/gravity wave
    ! integration.  We only need calculate these for the first pass through -
    ! the predictor step.  They are reused as is for the corrector step.
    ! For third-order RK, we need to recompute these after the first 
    ! predictor because we may have changed the small timestep -> dts.

      CALL wrf_debug ( 200 , ' call calc_coef_w' )

      CALL small_step_prep( u_1,u_2,v_1,v_2,w_1,w_2,          &
                            t_1,t_2,ph_1,ph_2,                &
                            mub, mu_1, mu_2,                  &
                            muu, muus, muv, muvs,             &
                            mut, muts, mudf,                  & 
                            u_save, v_save, w_save,           & 
                            t_save, ph_save, mu_save,         &
                            ww, ww1,                          &
                            dnw, c2a, pb, p, alt,             &
                            msfu, msfv, msft,                 &
                            rk_step, leapfrog,                &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               )
      CALL calc_p_rho( al, p, ph_2,                      &
                       alt, t_2, t_save, c2a, pm1,       &
                       mu_2, muts, znu, t0,              &
                       rdnw, dnw, smdiv,                 &
                       non_hydrostatic, 0,               &
                       ids, ide, jds, jde, kds, kde,     &
                       ims, ime, jms, jme, kms, kme,     &
                       grid%i_start(ij), grid%i_end(ij), &
                       grid%j_start(ij), grid%j_end(ij), &
                       k_start    , k_end               )

      IF (non_hydrostatic)                                &
      CALL calc_coef_w( a,alpha,gamma,                    &
                        mut, cqw,                         &
                        rdn, rdnw, c2a,                   &
                        dts, g, epssm,                    &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


   ENDDO


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
!j ph_2      x
!j al        x
!j p         x
!j t_1       x
!j t_save    x
!j u_save    x
!j v_save    x
!
!  the following are 2D (xy) variables
!
!j mu_1      x
!j mu_2      x
!j mudf      x
!--------------------------------------------------------------
#      include "HALO_EM_B.inc"
#      include "PERIOD_BDY_EM_B.inc"
#endif

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

         CALL set_physical_bc3d( ru_tend, 'u', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( rv_tend, 'v', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( t_1, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc3d( t_save, 't', config_flags,             &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                           grid%i_start(ij), grid%i_end(ij),                 &
                           grid%j_start(ij), grid%j_end(ij),                 &
                           k_start    , k_end                     )

         CALL set_physical_bc2d( mu_1, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

    END DO

   small_steps : DO iteration = 1 , number_of_small_timesteps

   ! Boundary condition time (or communication time).  

#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_B.inc"
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

         CALL advance_uv ( u_2, ru_tend, v_2, rv_tend,       &
                           p, pb,                            &
                           ph_2, php, alt, al, mu_2,         &
                           muu, cqu, muv, cqv, mudf,         &
                           rdx, rdy, dts,                    &
                           cf1, cf2, cf3, fnm, fnp,          &
                           emdiv,                            &
                           rdnw, config_flags,spec_zone,     &
                           non_hydrostatic,                  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

         IF( config_flags%specified .or. config_flags%nested ) THEN
           CALL spec_bdyupdate(u_2, ru_tend, dts_rk,      &
                               'u'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(v_2, rv_tend, dts_rk,      &
                               'v'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

         ENDIF

      END DO

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

        CALL advance_mu_t( ww, ww1, u_2, u_save, v_2, v_save, &
                           mu_2, mut, muave, muts, muu, muv,  &
                           mudf, ru_m, rv_m, ww_m,            &
                           t_2, t_save, t_2save, t_tend,      &
                           mu_tend,                           &
                           rdx, rdy, dts, epssm,              &
                           dnw, fnm, fnp, rdnw,               &
                           msfu, msfv, msft,                  &
                           iteration, config_flags,           &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start    , k_end                )

         IF( config_flags%specified .or. config_flags%nested ) THEN

           CALL spec_bdyupdate(t_2, t_tend, dts_rk,      &
                               't'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               k_start    , k_end             )

           CALL spec_bdyupdate(mu_2, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )

         ENDIF

         ! sumflux accumulates the time-averged mass flux
         ! (time averaged over the acoustic steps) for use
         ! in the scalar advection (flux divergence).  Using
         ! time averaged values gives us exact scalar conservation.

         CALL sumflux ( u_2, v_2, ww,                         &
                        u_save, v_save, ww1,                  &
                        muu, muv,                             &
                        ru_m, rv_m, ww_m, epssm,              &
                        msfu, msfv,                           &
                        iteration, number_of_small_timesteps, &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        grid%i_start(ij), grid%i_end(ij),     &
                        grid%j_start(ij), grid%j_end(ij),     &
                        k_start    , k_end                   )

         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.


        IF ( non_hydrostatic ) THEN
          CALL advance_w( w_2, rw_tend, ww, u_2, v_2,       &
                          mu_2, mut, muave, muts,           &
                          t_2save, t_2, t_save,             &
                          ph_2, ph_save, phb, ph_tend,      &
                          ht, c2a, cqw, alt, alb,           &
                          a, alpha, gamma,                  &
                          rdx, rdy, dts, t0, epssm,         &
                          dnw, fnm, fnp, rdnw, rdn,         &
                          cf1, cf2, cf3, msft,              &
                          config_flags,                     &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
        ENDIF

        IF( config_flags%specified .or. config_flags%nested ) THEN

           IF (non_hydrostatic)  THEN
             CALL spec_bdyupdate_ph( ph_2, ph_tend, mut, dts_rk, &
                                     'h'         , config_flags, &
                                     spec_zone,                  &
                                     ids,ide, jds,jde, kds,kde,  & ! domain dims
                                     ims,ime, jms,jme, kms,kme,  & ! memory dims
                                     ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                     grid%i_start(ij), grid%i_end(ij),         &
                                     grid%j_start(ij), grid%j_end(ij),         &
                                     k_start    , k_end             )
             IF( config_flags%specified ) THEN
               CALL zero_grad_bdy ( w_2,                        &
                                    'w'         , config_flags, &
                                    spec_zone,                  &
                                    ids,ide, jds,jde, kds,kde,  & ! domain dims
                                    ims,ime, jms,jme, kms,kme,  & ! memory dims
                                    ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                    grid%i_start(ij), grid%i_end(ij),         &
                                    grid%j_start(ij), grid%j_end(ij),         &
                                    k_start    , k_end             )
             ELSE
               CALL spec_bdyupdate   ( w_2, rw_tend, dts_rk,       &
                                       'h'         , config_flags, &
                                       spec_zone,                  &
                                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                       grid%i_start(ij), grid%i_end(ij),         &
                                       grid%j_start(ij), grid%j_end(ij),         &
                                       k_start    , k_end             )
             ENDIF
          ENDIF
        ENDIF

        CALL calc_p_rho( al, p, ph_2,                      &
                         alt, t_2, t_save, c2a, pm1,       &
                         mu_2, muts, znu, t0,              &
                         rdnw, dnw, smdiv,                 &
                         non_hydrostatic, iteration,       &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         grid%i_start(ij), grid%i_end(ij), &
                         grid%j_start(ij), grid%j_end(ij), &
                         k_start    , k_end               )

   ENDDO

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

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        ! boundary condition set for next small timestep

         CALL set_physical_bc3d( ph_2, 'w', config_flags,          &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( al, 'p', config_flags,            &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc3d( p, 'p', config_flags,             &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 ips, ipe, jps, jpe, kps, kpe,     &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij), &
                                 k_start    , k_end               )

         CALL set_physical_bc2d( muts, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mu_2, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

         CALL set_physical_bc2d( mudf, 't', config_flags,          &
                                 ids, ide, jds, jde,               &
                                 ims, ime, jms, jme,               &
                                 ips, ipe, jps, jpe,               &
                                 grid%i_start(ij), grid%i_end(ij), &
                                 grid%j_start(ij), grid%j_end(ij) )

      END DO

   END DO small_steps

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_small_finish' )

      ! change time-perturbation variables back to 
      ! full perturbation variables.
      ! first get updated mu at u and v points

      CALL calc_mu_uv_1 ( config_flags,                     &
                          muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

      CALL small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,     &
                              t_2, t_1, ph_2, ph_1, ww, ww1,    &
                              mu_2, mu_1,                       &
                              mut, muts, muu, muus, muv, muvs,  & 
                              u_save, v_save, w_save,           &
                              t_save, ph_save, mu_save,         &
                              msfu, msfv, msft,                 &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

   END DO

#ifdef DM_PARALLEL
!
!  Stencils for patch communications  (WCS, 29 June 2001)
!
!
! ru_m      x
! rv_m      x
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

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )

       CALL rk_scalar_tend (  im, im, config_flags,             &
                              rk_step, dt_rk,                   &
                              ru_m, rv_m, ww_m,                 &
                              mut, alt,                         &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend,RQVFTEN,              &
                              qv_base, .true., fnm, fnp,        &
                              msfu, msfv, msft,                 &
                              rdx, rdy, rdn, rdnw, khdif,       &
                              kvdif, xkmhd,                     &
                              leapfrog,                         &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

     IF( (config_flags%specified .or. config_flags%nested) .and. rk_step == 1 ) THEN 

       IF(im .eq. P_QV)THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqv_b, rqv_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqv_b, rqv_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ENDIF

     ENDIF

!  ugly code for nested b.c for moist scalars other than qv

     IF( config_flags%nested .and. (rk_step == 1) ) THEN 

       IF (im .eq. P_QC) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqc_b, rqc_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqc_b, rqc_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QR) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqr_b, rqr_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqr_b, rqr_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QI) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqi_b, rqi_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqi_b, rqi_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )

       ELSE IF (im .eq. P_QS) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqs_b, rqs_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqs_b, rqs_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ELSE IF (im .eq. P_QG) THEN

         CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                 moist_2(ims,kms,jms,im),  mut,         &
                                 rqg_b, rqg_bt,                         &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( moist_tend(ims,kms,jms,im),                &
                                 rqg_b, rqg_bt,                             &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),          &
                                 grid%j_start(ij), grid%j_end(ij),          &
                                 k_start, k_end                               )
       ENDIF

     ENDIF ! b.c test for moist nested boundary condition


   ENDDO moist_tile_loop_1

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   moist_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )

       CALL rk_update_scalar( im, im,                           &
                              moist_1(ims,kms,jms,im),          &
                              moist_2(ims,kms,jms,im),          &
                              moist_tend(ims,kms,jms,im),       &
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )

       IF( config_flags%specified ) THEN
         IF(im .ne. P_QV)THEN
           CALL flow_dep_bdy  (  moist_2(ims,kms,jms,im),                     &
                               ru_m, rv_m, config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),                      &
                               grid%j_start(ij), grid%j_end(ij),                      &
                               k_start, k_end                               )
         ENDIF
       ENDIF


   ENDDO moist_tile_loop_2

   ENDDO moist_variable_loop

 ENDIF moist_scalar_advance

 TKE_advance: IF (km_opt .eq. 2) then

#ifdef DM_PARALLEL
      IF      ( h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_ADVECT_3.inc"
      ELSE IF ( h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_ADVECT_5.inc"
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
        CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF
#endif

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_1: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_scalar_tend for tke' )
     CALL rk_scalar_tend ( 1, 1, config_flags,               &
                           rk_step, dt_rk,                   &
                           ru_m, rv_m, ww_m,                 &
                           mut, alt,                         &
                           tke_1(ims,kms,jms),               &
                           tke_2(ims,kms,jms),               &
                           tke_tend(ims,kms,jms),            &
                           advect_tend,RQVFTEN,              &
                           qv_base, .false., fnm, fnp,       &
                           msfu, msfv, msft,                 &
                           rdx, rdy, rdn, rdnw, khdif,       &
                           kvdif, xkmhd,                     &
                           leapfrog,                         &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )

   ENDDO tke_tile_loop_1

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tke_tile_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call rk_update_scalar' )
     CALL rk_update_scalar( 1, 1,                             &
                            tke_1(ims,kms,jms),               &
                            tke_2(ims,kms,jms),               &
                            tke_tend(ims,kms,jms),            &
                            advect_tend,msft,                 &
                            mu_1, mu_2, mub,                  &
                            rk_step, dt_rk, spec_zone,        &
                            epsts, leapfrog,config_flags,     &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            grid%i_start(ij), grid%i_end(ij), &
                            grid%j_start(ij), grid%j_end(ij), &
                            k_start    , k_end               ) 

! bound the tke (greater than 0, less than tke_upper_bound)

     CALL bound_tke( tke_2(ims,kms,jms), tke_upper_bound, &
                     ids, ide, jds, jde, kds, kde,        &
                     ims, ime, jms, jme, kms, kme,        &
                     grid%i_start(ij), grid%i_end(ij),    &
                     grid%j_start(ij), grid%j_end(ij),    &
                     k_start    , k_end                  )

     IF( config_flags%specified .or. config_flags%nested ) THEN
         CALL flow_dep_bdy (  tke_2(ims,kms,jms),                     &
                              ru_m, rv_m, config_flags,               &
                              spec_zone,                              &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              grid%i_start(ij), grid%i_end(ij),       &
                              grid%j_start(ij), grid%j_end(ij),       &
                              k_start, k_end                               )
     ENDIF
   ENDDO tke_tile_loop_2

   END IF TKE_advance

!  next the chemical species

  chem_scalar_advance: IF (num_3d_c >= PARAM_FIRST_SCALAR)  THEN

   chem_variable_loop: do ic = PARAM_FIRST_SCALAR, num_3d_c

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
       CALL rk_scalar_tend ( ic, ic, config_flags,                  &
                             rk_step, dt_rk,                   &
                             ru_m, rv_m, ww_m,                 &
                             mut, alt,                         &
                             chem_1(ims,kms,jms,ic),           &
                             chem_2(ims,kms,jms,ic),           &
                             chem_tend(ims,kms,jms,ic),        &
                             advect_tend,RQVFTEN,              &
                             qv_base, .false., fnm, fnp,       &
                             msfu, msfv, msft,                 &
                             rdx, rdy, rdn, rdnw,              &
                             khdif, kvdif, xkmhd,              &
                             leapfrog,                         &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

   ENDDO chem_tile_loop_1


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
       CALL rk_update_scalar( ic, ic,                           &
                              chem_1(ims,kms,jms,ic),           &
                              chem_2(ims,kms,jms,ic),           &
                              chem_tend(ims,kms,jms,ic),        &
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )


       IF( config_flags%specified ) THEN
           CALL flow_dep_bdy  ( chem_2(ims,kms,jms,ic),     &
                                ru_m, rv_m, config_flags,   &
                                spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),  &
                                grid%j_start(ij), grid%j_end(ij),  &
                                k_start, k_end                    )
       ENDIF


   ENDDO chem_tile_loop_2

   ENDDO chem_variable_loop

 ENDIF chem_scalar_advance

 !  update the pressure and density at the new time level

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )
   DO ij = 1 , grid%num_tiles

     CALL calc_p_rho_phi( moist_2, num_3d_m,                &
                          al, alb, mu_2, muts,              &
                          ph_2, p, pb, t_2,                 &
                          p0, t0, znu, dnw, rdnw,           &
                          rdn, non_hydrostatic,             &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )

     IF (.not. non_hydrostatic)                               &
     CALL diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                      u_2, v_2, ht,                           &
                      cf1, cf2, cf3, rdx, rdy, msft,          &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij),       &
                      grid%j_start(ij), grid%j_end(ij),       &
                      k_start    , k_end                     )


   ENDDO

!  Reset the boundary conditions if there is another corrector step.
!  (rk_step < rk_order), else we'll handle it at the end of everything
!  (after the split physics, before exiting the timestep).

   rk_step_1_check: IF ( rk_step < rk_order ) THEN

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
! u_2                              x
! v_2                              x
! w_2                              x
! t_2                              x
! ph_2                             x
! al         x
!
!  2D variable
! mu_2       x
!
!  4D variable
! moist_2               x
! chem_2                x

#ifdef DM_PARALLEL
   IF      ( h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_D2_3.inc"
   ELSE IF ( h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D2_5.inc"
   ELSE 
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
     CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#  include "PERIOD_BDY_EM_D.inc"
#  include "PERIOD_BDY_EM_MOIST2.inc"
#  include "PERIOD_BDY_EM_CHEM2.inc"
#endif

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    tile_bc_loop_1: DO ij = 1 , grid%num_tiles

      CALL wrf_debug ( 200 , ' call rk_phys_bc_dry_2' )

      CALL rk_phys_bc_dry_2( config_flags,                         &
                             u_2, v_2, w_2,                    &
                             t_2, ph_2, mu_2,                  &
                             ids, ide, jds, jde, kds, kde,     &
                             ims, ime, jms, jme, kms, kme,     &
                             ips, ipe, jps, jpe, kps, kpe,     &
                             grid%i_start(ij), grid%i_end(ij), &
                             grid%j_start(ij), grid%j_end(ij), &
                             k_start    , k_end               )

      IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN

        moisture_loop_bdy_1 : DO im = PARAM_FIRST_SCALAR , num_3d_m
  
          CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p', config_flags,   &
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

          CALL set_physical_bc3d( chem_2(ims,kms,jms,ic), 'p', config_flags,   &
                                  ids, ide, jds, jde, kds, kde,            &
                                  ims, ime, jms, jme, kms, kme,            &
                                  ips, ipe, jps, jpe, kps, kpe,            &
                                  grid%i_start(ij), grid%i_end(ij),                  &
                                  grid%j_start(ij), grid%j_end(ij),                  &
                                  k_start    , k_end-1                    )

        END DO chem_species_bdy_loop_1

      END IF

      IF (km_opt .eq. 2) THEN

        CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                ips, ipe, jps, jpe, kps, kpe,            &
                                grid%i_start(ij), grid%i_end(ij),        &
                                grid%j_start(ij), grid%j_end(ij),        &
                                k_start    , k_end                      )
      END IF

    END DO tile_bc_loop_1


#ifdef DM_PARALLEL

      IF      ( h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3.inc"
      ELSE IF ( h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5.inc"
      ELSE
        WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
        CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF

#if 0
   IF (km_opt .eq. 2) THEN
#      include  "HALO_EM_TKE_F.inc"
   ENDIF
#endif

   if ( num_moist .ge. PARAM_FIRST_SCALAR ) then

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! moist_2                       x

     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_MOIST_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_MOIST_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! chem_2                        x

     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
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

      CALL wrf_debug ( 200 , ' call advance_ppt' )
      CALL advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN, &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,    &
                     CUPPT, config_flags,                   &
                     ids,ide, jds,jde, kds,kde,             &
                     ims,ime, jms,jme, kms,kme,             &
                     grid%i_start(ij), grid%i_end(ij),      &
                     grid%j_start(ij), grid%j_end(ij),      &
                     k_start    , k_end                    )

   ENDDO

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

  IF (config_flags%mp_physics /= 0)  then

   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1a: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL wrf_debug ( 200 , ' call moist_physics_prep' )

       CALL moist_physics_prep_em( t_2, t_1, t0, rho,                &
                                   al, alb, p, p8w, p0, pb,          &
                                   ph_2, phb, pi_phy, p_phy,         &
                                   z, z_at_w, dz8w,                  &
                                   dtm, h_diabatic,                  &
                                   config_flags,fnm, fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )
   END DO scalar_tile_loop_1a

       CALL wrf_debug ( 200 , ' call microphysics_driver' )

       CALL microphysics_driver(t_2,moist_2, moist_1, w_2,                  &
                                rho, pi_phy, p_phy, RAINNC, RAINNCV,        &
                                z, ht, dz8w, p8w, dtm, dx, dy,              &
                                config_flags, spec_zone,                    &
                                num_3d_m, warm_rain,                        &
                                XLAND,itimestep,                            & 
                                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
                                LOWLYR,                                     &
                                ids, ide, jds, jde, kds, kde,               &
                                ims, ime, jms, jme, kms, kme,               &
                                grid%i_start, min(grid%i_end, ide-1),       &
                                grid%j_start, min(grid%j_end, jde-1),       &
                                k_start    , min(k_end,kde-1) , grid%num_tiles )

       CALL wrf_debug ( 200 , ' call moist_physics_finish' )
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1b: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL moist_physics_finish_em( t_2, t_1, t0, muts,               &
                                     h_diabatic, dtm, config_flags,    &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte,               &
                                     k_start    , k_end               )


       CALL calc_p_rho_phi( moist_2, num_3d_m,                &
                            al, alb, mu_2, muts,              &
                            ph_2, p, pb, t_2,                 &
                            p0, t0, znu, dnw, rdnw,           &
                            rdn, non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            its, ite, jts, jte,               &
                            k_start    , k_end               )

       IF (.not. non_hydrostatic)                               &
       CALL diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                        u_2, v_2, ht,                           &
                        cf1, cf2, cf3, rdx, rdy, msft,          &
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        its, ite, jts, jte,                     &
                        k_start    , k_end                     )

   END DO scalar_tile_loop_1b

  ENDIF

   scalar_tile_loop_2: DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call scalar_tile_loop_2' )

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then

!  tiled chemistry here

     END IF

   END DO scalar_tile_loop_2


   IF (leapfrog ) THEN

    ! do time filter and switch for the dry variables

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    DO ij = 1 , grid%num_tiles

      call time_filter( u_1, u_2, u_save,                 &
                        v_1, v_2, v_save,                 &
                        w_1, w_2, w_save,                 &
                        t_1, t_2, t_save,                 &
                        ph_1, ph_2, ph_save,              &
                        mu_1, mu_2, mu_save,              &
                        epsts,                            &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )

    ENDDO

    END IF

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
!   u_1                            x
!   u_2                            x
!   v_1                            x
!   v_2                            x
!   w_1                            x
!   w_2                            x
!   t_1                            x
!   t_2                            x
!  ph_1                            x
!  ph_2                            x
!  tke_1                           x
!  tke_2                           x
!
!    2D variables
!  mu_1     x
!  mu_2     x
!
!    4D variables
!  moist_1                         x
!  moist_2                         x
!   chem_1                         x
!   chem_2                         x
!----------------------------------------------------------


#ifdef DM_PARALLEL
   IF      ( h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_D3_3.inc"
   ELSE IF ( h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_D3_5.inc"
   ELSE 
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
     CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#  include "PERIOD_BDY_EM_D3.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
#endif

!  now set physical b.c on a patch

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   tile_bc_loop_2: DO ij = 1 , grid%num_tiles

     CALL wrf_debug ( 200 , ' call set_phys_bc_dry_2' )

     CALL set_phys_bc_dry_2( config_flags,                           &
                             u_1, u_2, v_1, v_2, w_1, w_2,           &
                             t_1, t_2, ph_1, ph_2, mu_1, mu_2,       &
                             ids, ide, jds, jde, kds, kde,           &
                             ims, ime, jms, jme, kms, kme,           &
                             ips, ipe, jps, jpe, kps, kpe,           &
                             grid%i_start(ij), grid%i_end(ij),       &
                             grid%j_start(ij), grid%j_end(ij),       &
                             k_start    , k_end                     )

     CALL set_physical_bc3d( tke_1(ims,kms,jms), 'p', config_flags,   &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end-1                    )
     CALL set_physical_bc3d( tke_2(ims,kms,jms) , 'p', config_flags,  &
                             ids, ide, jds, jde, kds, kde,            &
                             ims, ime, jms, jme, kms, kme,            &
                             ips, ipe, jps, jpe, kps, kpe,            &
                             grid%i_start(ij), grid%i_end(ij),        &
                             grid%j_start(ij), grid%j_end(ij),        &
                             k_start    , k_end                      )

     moisture_loop_bdy_2 : DO im = PARAM_FIRST_SCALAR , num_3d_m

       CALL set_physical_bc3d( moist_1(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )
       CALL set_physical_bc3d( moist_2(ims,kms,jms,im), 'p',           &
                               config_flags,                           &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               ips, ipe, jps, jpe, kps, kpe,           &
                               grid%i_start(ij), grid%i_end(ij),       &
                               grid%j_start(ij), grid%j_end(ij),       &
                               k_start    , k_end                     )

     END DO moisture_loop_bdy_2

     chem_species_bdy_loop_2 : DO ic = PARAM_FIRST_SCALAR , num_3d_c

       CALL set_physical_bc3d( chem_1(ims,kms,jms,ic), 'p', config_flags,   &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end-1                    )
       CALL set_physical_bc3d( chem_2(ims,kms,jms,ic) , 'p', config_flags,  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               ips, ipe, jps, jpe, kps, kpe,            &
                               grid%i_start(ij), grid%i_end(ij),                  &
                               grid%j_start(ij), grid%j_end(ij),                  &
                               k_start    , k_end                      )

     END DO chem_species_bdy_loop_2

   END DO tile_bc_loop_2

   IF( config_flags%specified .or. config_flags%nested ) THEN 
     dtbc = dtbc + dt
   ENDIF

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
   CALL wrf_debug ( 200 , ' call HALO_RK_E' )
   IF      ( h_mom_adv_order <= 4 ) THEN
#    include "HALO_EM_E_3.inc"
   ELSE IF ( h_mom_adv_order <= 6 ) THEN
#    include "HALO_EM_E_5.inc"
   ELSE
     WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
     CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF
#endif

#ifdef DM_PARALLEL
   if ( num_moist >= PARAM_FIRST_SCALAR  ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_MOIST' )
     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_MOIST_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_MOIST_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_chem >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_CHEM' )
     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
#endif

   CALL wrf_debug ( 200 , ' call end of solve_em' )

#define COPY_OUT
#include <em_scalar_derefs.inc>

   RETURN

END SUBROUTINE solve_em

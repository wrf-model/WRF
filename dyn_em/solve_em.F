!WRF:MEDIATION_LAYER:SOLVER

SUBROUTINE solve_em ( grid , config_flags , &
! Actual arguments generated from Registry
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
   USE module_microphysics_zero_out
   USE module_pbl_driver
#ifdef WRF_CHEM
   USE module_input_chem_data
   USE module_chem_utilities
#endif

   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to this routine (generated from Registry).
#include <em_dummy_decl.inc>

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   ! Local data

   INTEGER                         :: k_start , k_end, its, ite, jts, jte
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c , is , num_3d_s
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: itmpstep
   INTEGER                         :: sz

   LOGICAL                         :: specified_bdy

! storage for tendencies and decoupled state (generated from Registry)
#include <em_i1_decl.inc>

! Previous time level of tracer arrays now defined as i1 variables;
! the state 4d arrays now redefined as 1-time level arrays in Registry.
! Benefit: save memory in nested runs, since only 1 domain is active at a
! time.  Potential problem on stack-limited architectures: increases
! amount of data on program stack by making these automatic arrays.

   REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)  :: moist_old
   REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)   :: chem_old
   REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar) :: scalar_old

   INTEGER :: rc 
   INTEGER :: number_of_small_timesteps, rk_step
   INTEGER :: klevel,ijm,ijp,i,j,k,size1,size2    ! for prints/plots only
   INTEGER :: idum1, idum2, dynamics_option

   INTEGER :: rk_order, iwmax, jwmax, kwmax
   REAL :: dt_rk, dts_rk, dtm, wmax
   LOGICAL :: leapfrog
   INTEGER :: l,kte,kk


! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
   INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
   INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

! Define benchmarking timers if -DBENCH is compiled
#include <bench_solve_em_def.h>

!----------------------
! Executable statements
!----------------------

! Trick problematic compilers into not performing copy-in/copy-out by adding
! indices to array arguments in the CALL statements in this routine.
! It has the effect of passing only the first element of the array, rather 
! than the entire array.  See:  
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
#include "deref_kludge.h"

! Limit the number of arguments if compiled with -DLIMIT_ARGS by copying 
! scalar (non-array) arguments out of the grid data structure into locally
! defined copies (defined in em_dummy_decl.inc, above, as they are if they
! are arguments). An equivalent include of em_scalar_derefs.inc appears
! at the end of the routine to copy back any chnaged non-array values.
! The definition of COPY_IN or COPY_OUT before the include defines the
! direction of the copy.  Em_scalar_derefs.inc is generated from Registry
#define COPY_IN
#include <em_scalar_derefs.inc>

! Needed by some comm layers, e.g. RSL. If needed, nmm_data_calls.inc is
! generated from the registry.  The definition of REGISTER_I1 allows
! I1 data to be communicated in this routine if necessary.
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

! Initialize timers if compiled with -DBENCH
#include <bench_solve_em_init.h>

!  set leapfrog or runge-kutta solver (2nd or 3rd order)

   dynamics_option = config_flags%rk_ord

!  Obtain dimension information stored in the grid data structure.
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
  num_3d_s        = num_scalar

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
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

#ifdef WRF_CHEM
!
!    prepare chem aerosols for advection before communication
!    so far only for RADM2/SORGAM choice
!

   kte=min(k_end,kde-1)
!
! change units for advection to mixing ratio
!
      if(imicrogram == 1)then

   if ( p_so4aj >= PARAM_FIRST_SCALAR ) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,its,ite,jts,jte,kte)
   aerosol_decouple_loop : DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)
      do l=p_so4aj,num_chem
      do j=jts,jte
      do k=k_start,kte+1 
      kk=min(k,kde-1)
      do i=its,ite
        chem(i,k,j,l)=chem(i,kk,j,l)*alt(i,kk,j)
      enddo
      enddo
      enddo
      enddo
    enddo aerosol_decouple_loop
    endif
    imicrogram=0
    endif
# ifdef DM_PARALLEL
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
# endif
!--------------------------------------------------------------
#endif

 rk_order = config_flags%rk_ord
 leapfrog = .false.
 IF (time_step_sound == 0) THEN
! auto-set option
! This function will give 4 for 6*dx and 6 for 10*dx and returns even numbers only
   time_step_sound = max ( 2 * ( INT (300.*dt/dx-0.01) + 1 ), 4 )
   WRITE(wrf_err_message,*)'dx, dt, time_step_sound=',dx,dt,time_step_sound
   CALL wrf_debug ( 50 , wrf_err_message )
 ENDIF

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

BENCH_START(step_prep_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   DO ij = 1 , grid%num_tiles

      CALL rk_step_prep  ( config_flags, rk_step,            &
                           u_2, v_2, w_2, t_2, ph_2, mu_2,   &
                           moist,                            &
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

BENCH_START(set_phys_bc_tim)
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
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist, num_3d_m,   &
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
      !$OMP END PARALLEL DO

BENCH_END(phy_prep_tim)

!  physics to implement

!      CALL set_tiles ( grid , ids , ide-1 , jds , jde-1 ips , ipe , jps , jpe )

! Open MP loops are in physics drivers
! radiation

         CALL wrf_debug ( 200 , ' call radiation_driver' )
BENCH_START(rad_driver_tim)
         CALL radiation_driver(                                           &
     &         ACFRCV=acfrcv      ,ACFRST=acfrst      ,AER_DRY=aer_dry    &
     &        ,AER_WATER=aer_water                    ,ALBEDO=albedo      &
     &        ,CFRACH=cfrach      ,CFRACL=cfracl      ,CFRACM=cfracm      &
     &        ,CUPPT=cuppt        ,CZMEAN=czmean      ,DT=dt              &
     &        ,DZ8W=dz8w          ,EMISS=emiss        ,GLW=glw            &
     &        ,GMT=gmt            ,GSW=gsw            ,HBOT=hbot          &
     &        ,HTOP=htop          ,ICLOUD=icloud                          &
     &        ,ITIMESTEP=itimestep                    ,JULDAY=julday      &
     &        ,JULYR=julyr        ,LW_PHYSICS=config_flags%ra_lw_physics  &
     &        ,NCFRCV=ncfrcv      ,NCFRST=ncfrst      ,NPHS=1             &
     &        ,P8W=p8w            ,P=p_phy            ,PI=pi_phy          &
     &        ,RADT=radt          ,RHO=rho            ,RLWTOA=rlwtoa      &
     &        ,RSWTOA=rswtoa      ,RTHRATEN=rthraten                      &
     &        ,RTHRATENLW=rthratenlw                                      &
     &        ,RTHRATENSW=rthratensw                  ,SNOW=snow          &
     &        ,STEPRA=stepra      ,SWDOWN=swdown                          &
     &        ,SW_PHYSICS=config_flags%ra_sw_physics  ,T8W=t8w            &
     &        ,T=t_phy            ,TAUCLDC=taucldc    ,TAUCLDI=taucldi    &
     &        ,TOTLWDN=totlwdn    ,TOTSWDN=totswdn    ,TSK=tsk            &
     &        ,VEGFRA=vegfra      ,WARM_RAIN=warm_rain                    &
     &        ,XLAND=xland        ,XLAT=xlat          ,XLONG=xlong        &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   &
            ! Optional                          
     &        , CLDFRA=CLDFRA                                             &
     &        , Pb=pb                                                     &
     &        , QV=moist(ims,kms,jms,P_QV), F_QV=F_QV                     &
     &        , QC=moist(ims,kms,jms,P_QC), F_QC=F_QC                     &
     &        , QR=moist(ims,kms,jms,P_QR), F_QR=F_QR                     &
     &        , QI=moist(ims,kms,jms,P_QI), F_QI=F_QI                     &
     &        , QS=moist(ims,kms,jms,P_QS), F_QS=F_QS                     &
     &        , QG=moist(ims,kms,jms,P_QG), F_QG=F_QG                     &
     &                                                              )

BENCH_END(rad_driver_tim)

!********* Surface driver
! surface

BENCH_START(surf_driver_tim)

      CALL wrf_debug ( 200 , ' call surface_driver' )
      CALL surface_driver(                                                &
     &         ACSNOM=acsnom      ,ACSNOW=acsnow      ,AKHS=akhs          &
     &        ,AKMS=akms          ,ALBBCK=albbck      ,ALBEDO=albedo      &
     &        ,BR=br              ,CANWAT=canwat      ,CHKLOWQ=chklowq    &
     &        ,CT=ct              ,DT=dt              ,DX=dx              &
     &        ,DZ8W=dz8w          ,DZS=dzs            ,FLHC=flhc          &
     &        ,FLQC=flqc          ,GLW=glw            ,GRDFLX=grdflx      &
     &        ,GSW=gsw            ,GZ1OZ0=gz1oz0      ,HFX=hfx            &
     &        ,HT=ht              ,IFSNOW=ifsnow      ,ISFFLX=isfflx      &
     &        ,ISLTYP=isltyp      ,ITIMESTEP=itimestep                    &
     &        ,IVGTYP=ivgtyp      ,LH=lh              ,LOWLYR=lowlyr      &
     &        ,MAVAIL=mavail      ,NUM_SOIL_LAYERS=num_soil_layers        &
     &        ,P8W=p8w            ,PBLH=pblh          ,PI_PHY=pi_phy      &
     &        ,PSFC=psfc          ,PSHLTR=pshltr      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,Q10=q10            &
     &        ,Q2=q2              ,QFX=qfx            ,QSFC=qsfc          &
     &        ,QSHLTR=qshltr      ,QZ0=qz0            ,RAINCV=raincv      &
     &        ,RA_LW_PHYSICS=ra_lw_physics            ,RHO=rho            &
     &        ,RMOL=rmol          ,SFCEVP=sfcevp      ,SFCEXC=sfcexc      &
     &        ,SFCRUNOFF=sfcrunoff                                        &
     &        ,SF_SFCLAY_PHYSICS=sf_sfclay_physics                        &
     &        ,SF_SURFACE_PHYSICS=sf_surface_physics  ,SH2O=sh2o          &
     &        ,SHDMAX=shdmax      ,SHDMIN=shdmin      ,SMOIS=smois        &
     &        ,SMSTAV=smstav      ,SMSTOT=smstot      ,SNOALB=snoalb      &
     &        ,SNOW=snow          ,SNOWC=snowc        ,SNOWH=snowh        &
     &        ,SST=sst            ,SST_UPDATE=sst_update                  &
     &        ,STEPBL=stepbl      ,TH10=th10          ,TH2=th2            &
     &        ,THZ0=thz0          ,TH_PHY=th_phy      ,TKE_MYJ=tke_myj    &
     &        ,TMN=tmn            ,TSHLTR=tshltr      ,TSK=tsk            &
     &        ,TSLB=tslb          ,T_PHY=t_phy        ,U10=u10            &
     &        ,UDRUNOFF=udrunoff  ,UST=ust            ,UZ0=uz0            &
     &        ,U_FRAME=u_frame    ,U_PHY=u_phy        ,V10=v10            &
     &        ,VEGFRA=vegfra      ,VZ0=vz0            ,V_FRAME=v_frame    &
     &        ,V_PHY=v_phy        ,WARM_RAIN=warm_rain                    &
     &        ,WSPD=wspd          ,XICE=xice          ,XLAND=xland        &
     &        ,Z0=z0              ,Z=z                ,ZNT=znt            &
     &        ,ZS=zs                                                      &
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
     &        ,CAPG=capg, EMISS=emiss, HOL=hol,MOL=mol                    &
     &        ,RAINBL=rainbl                                              &
     &        ,RAINNCV=rainncv,REGIME=regime,T2=t2,THC=thc                &
     &        ,QSG=qsg,QVG=qvg,QCG=qcg,SOILT1=soilt1,TSNAV=tsnav          & ! ruc lsm
     &        ,SMFR3D=smfr3d,KEEPFR3DFLAG=keepfr3dflag                    & ! ruc lsm
     &                                                              )
BENCH_END(surf_driver_tim)

!*********
! pbl

      CALL wrf_debug ( 200 , ' call pbl_driver' )
BENCH_START(pbl_driver_tim)
      CALL pbl_driver(                                                    &
     &         AKHS=akhs          ,AKMS=akms                              &
     &        ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics                 &
     &        ,BR=br              ,CHKLOWQ=chklowq    ,CT=ct              &
     &        ,DT=dt              ,DX=dx              ,DZ8W=dz8w          &
     &        ,EL_MYJ=el_myj      ,EXCH_H=exch_h      ,GRDFLX=grdflx      &
     &        ,GZ1OZ0=gz1oz0      ,HFX=hfx            ,HT=ht              &
     &        ,ITIMESTEP=itimestep                    ,KPBL=kpbl          &
     &        ,LH=lh              ,LOWLYR=lowlyr      ,P8W=p8w            &
     &        ,PBLH=pblh          ,PI_PHY=pi_phy      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=p_phy        ,QFX=qfx            &
     &        ,QSFC=qsfc          ,QZ0=qz0                                &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics                   &
     &        ,RHO=rho            ,RQCBLTEN=rqcblten  ,RQIBLTEN=rqiblten  &
     &        ,RQVBLTEN=rqvblten  ,RTHBLTEN=rthblten  ,RUBLTEN=rublten    &
     &        ,RVBLTEN=rvblten    ,SNOW=snow          ,STEPBL=stepbl      &
     &        ,THZ0=thz0          ,TH_PHY=th_phy      ,TKE_MYJ=tke_myj    &
     &        ,TSK=tsk            ,T_PHY=t_phy        ,UST=ust            &
     &        ,UZ0=uz0            ,U_FRAME=u_frame    ,U_PHY=u_phy        &
     &        ,VZ0=vz0            ,V_FRAME=v_frame    ,V_PHY=v_phy        &
     &        ,WARM_RAIN=warm_rain                    ,WSPD=wspd          &
     &        ,XICE=xice          ,XLAND=xland        ,Z=z                &
     &        ,ZNT=znt                                                    &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
          ! optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,HOL=HOL, MOL=MOL, REGIME=REGIME                            &
     &                                                          )

BENCH_END(pbl_driver_tim)

! cumulus para.

          CALL wrf_debug ( 200 , ' call cumulus_driver' )

BENCH_START(cu_driver_tim)
         CALL cumulus_driver(                                             &
                 ! Prognostic variables
     &              U=u_phy   ,V=v_phy   ,TH=th_phy  ,T=t_phy             &
     &             ,W=w_2     ,P=p_phy   ,PI=pi_phy  ,RHO=rho             &
                 ! Other arguments
     &             ,ITIMESTEP=itimestep ,DT=dt      ,DX=dx                &
     &             ,RAINC=rainc   ,RAINCV=raincv   ,NCA=nca               &
     &             ,HTOP=htop     ,HBOT=hbot       ,KPBL=kpbl             &
     &             ,DZ8W=dz8w     ,P8W=p8w                                &
     &             ,W0AVG=w0avg   ,STEPCU=stepcu                          &
     &             ,CLDEFI=cldefi ,LOWLYR=lowlyr ,XLAND=xland             &
     &             ,APR_GR=apr_gr ,APR_W=apr_w   ,APR_MC=apr_mc           &
     &             ,APR_ST=apr_st ,APR_AS=apr_as ,APR_CAPMA=apr_capma     &
     &             ,APR_CAPME=apr_capme          ,APR_CAPMI=apr_capmi     &
     &             ,MASS_FLUX=mass_flux          ,XF_ENS=xf_ens           &
     &             ,PR_ENS=pr_ens ,HT=ht                                  &
     &             ,ENSDIM=ensdim ,MAXIENS=maxiens ,MAXENS=maxens         &
     &             ,MAXENS2=maxens2                ,MAXENS3=maxens3       &
     &             ,CU_ACT_FLAG=cu_act_flag   ,WARM_RAIN=warm_rain        &
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
     &             ,RQVCUTEN=rqvcuten , RQCCUTEN=rqccuten                 &
     &             ,RQRCUTEN=rqrcuten , RQVBLTEN=rqvblten                 &
     &             ,RQVFTEN=rqvften                                       &
                 ! Other tendency arguments
     &             ,RTHRATEN=rthraten , RTHBLTEN=rthblten                 &
     &             ,RTHCUTEN=rthcuten , RTHFTEN=rthften                   &
                 ! Moisture tracer arguments
     &             ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV            &
     &             ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC            &
     &             ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR            &
     &             ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI            &
     &             ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS            &
     &             ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG            &
     &                                                          )
BENCH_END(cu_driver_tim)

! calculate_phy_tend

BENCH_START(cal_phy_tend)
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
      !$OMP END PARALLEL DO
BENCH_END(cal_phy_tend)

! tke diffusion

     IF(diff_opt .eq. 2 .OR. diff_opt .eq. 1) THEN

BENCH_START(comp_diff_metrics_tim)
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
       !$OMP END PARALLEL DO
BENCH_END(comp_diff_metrics_tim)

#ifdef DM_PARALLEL
#  include "PERIOD_BDY_EM_A1.inc"
#endif

BENCH_START(tke_diff_bc_tim)
       DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call bc for diffusion_metrics ' )
          CALL set_physical_bc3d( rdzw , 'w', config_flags,           &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( rdz , 'w', config_flags,            &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( z , 'w', config_flags,              &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zx , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )
          CALL set_physical_bc3d( zy , 'w', config_flags,             &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  ips, ipe, jps, jpe, kps, kpe,       &
                                  grid%i_start(ij), grid%i_end(ij),   &
                                  grid%j_start(ij), grid%j_end(ij),   &
                                  k_start    , k_end                 )

       ENDDO
BENCH_END(tke_diff_bc_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_C.inc"
#endif

BENCH_START(deform_div_tim)

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
          CALL calculate_km_kh( config_flags,dt,dampcoef,zdamp,damp_opt,     &
                                xkmh,xkmhd,xkmv,xkhh,xkhv,BN2,               &
                                khdif,kvdif,div,                             &
                                defor11,defor22,defor33,defor12,             &
                                defor13,defor23,                             &
                                tke_2(ims,kms,jms),p8w,t8w,th_phy,           &
                                t_phy,p_phy,moist,dn,dnw,                    &
                                dx,dy,rdz,rdzw,mix_cr_len,num_3d_m,          &
                                cf1, cf2, cf3, warm_rain,                    &
                                kh_tke_upper_bound, kv_tke_upper_bound,      &
                                ids,ide, jds,jde, kds,kde,                   &
                                ims,ime, jms,jme, kms,kme,                   &
                                grid%i_start(ij), grid%i_end(ij),            &
                                grid%j_start(ij), grid%j_end(ij),            &
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
#      include "PERIOD_BDY_EM_CHEM.inc"
#endif

BENCH_START(phy_bc_tim)
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

BENCH_START(update_phy_ten_tim)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call update_phy_ten' )
        CALL update_phy_ten(t_tendf, ru_tendf, rv_tendf,moist_tend,        &
                          RTHRATEN,RTHBLTEN,RTHCUTEN,RUBLTEN,RVBLTEN,      &
                          RQVBLTEN,RQCBLTEN,RQIBLTEN,                      &
                          RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN,    &
                          num_3d_m,config_flags,rk_step,                   &
                          ids, ide, jds, jde, kds, kde,                    &
                          ims, ime, jms, jme, kms, kme,                    &
                          grid%i_start(ij), grid%i_end(ij),                &
                          grid%j_start(ij), grid%j_end(ij),                &
                          k_start, k_end                               )

      END DO
      !$OMP END PARALLEL DO
BENCH_END(update_phy_ten_tim)

     IF( diff_opt .eq. 2 .and. km_opt .eq. 2 ) THEN

BENCH_START(tke_rhs_tim)
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
       !$OMP END PARALLEL DO
BENCH_END(tke_rhs_tim)

     ENDIF

! calculate vertical diffusion first and then horizontal
! (keep this order)

     IF(diff_opt .eq. 2) THEN

       IF (bl_pbl_physics .eq. 0) THEN

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
                                      u_2, v_2,                                  &
                                      t_2,u_base,v_base,t_base,qv_base,          &
                                      mut,tke_2,config_flags,                    &
                                      defor13,defor23,defor33,                   &
                                      div, moist, chem, scalar,                  &
                                      xkmv, xkhv, km_opt,                        &
                                      fnm, fnp, dn, dnw, rdz, rdzw,              &
                                      ids, ide, jds, jde, kds, kde,              &
                                      ims, ime, jms, jme, kms, kme,              &
                                      grid%i_start(ij), grid%i_end(ij),          &
                                      grid%j_start(ij), grid%j_end(ij),          &
                                      k_start    , k_end                        )

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
                                      t_2, th_phy,                           &
                                      mut, tke_2, config_flags,              &
                                      defor11, defor22, defor12,             &
                                      defor13, defor23, div,                 &
                                      moist, chem, scalar,                   &
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
       !$OMP END PARALLEL DO
BENCH_END(hor_diff_tim)

     ENDIF

     END IF rk_step_is_one

BENCH_START(rk_tend_tim)
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
                         dampcoef,zdamp,damp_opt,                         &
                         cf1, cf2, cf3, cfn, cfn1, num_3d_m,              &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         grid%i_start(ij), grid%i_end(ij),                &
                         grid%j_start(ij), grid%j_end(ij),                &
                         k_start, k_end                                  )
   END DO
   !$OMP END PARALLEL DO
BENCH_END(rk_tend_tim)

BENCH_START(relax_bdy_dry_tim)
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
                            dtbc, fcx, gcx,                              &
                            ijds, ijde,                                  &
                            ids,ide, jds,jde, kds,kde,                   &
                            ims,ime, jms,jme, kms,kme,                   &
                            ips,ipe, jps,jpe, kps,kpe,                   &
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
                        dts_rk, g, epssm,                 &
                        ids, ide, jds, jde, kds, kde,     &
                        ims, ime, jms, jme, kms, kme,     &
                        grid%i_start(ij), grid%i_end(ij), &
                        grid%j_start(ij), grid%j_end(ij), &
                        k_start    , k_end               )


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

BENCH_START(set_phys_bc2_tim)
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
         CALL advance_uv ( u_2, ru_tend, v_2, rv_tend,       &
                           p, pb,                            &
                           ph_2, php, alt, al, mu_2,         &
                           muu, cqu, muv, cqv, mudf,         &
                           rdx, rdy, dts_rk,                 &
                           cf1, cf2, cf3, fnm, fnp,          &
                           emdiv,                            &
                           rdnw, config_flags,spec_zone,     &
                           non_hydrostatic,                  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           grid%i_start(ij), grid%i_end(ij), &
                           grid%j_start(ij), grid%j_end(ij), &
                           k_start    , k_end               )
BENCH_END(advance_uv_tim)

BENCH_START(spec_bdy_uv_tim)
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
        CALL advance_mu_t( ww, ww1, u_2, u_save, v_2, v_save, &
                           mu_2, mut, muave, muts, muu, muv,  &
                           mudf, ru_m, rv_m, ww_m,            &
                           t_2, t_save, t_2save, t_tend,      &
                           mu_tend,                           &
                           rdx, rdy, dts_rk, epssm,           &
                           dnw, fnm, fnp, rdnw,               &
                           msfu, msfv, msft,                  &
                           iteration, config_flags,           &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           grid%i_start(ij), grid%i_end(ij),  &
                           grid%j_start(ij), grid%j_end(ij),  &
                           k_start    , k_end                )
BENCH_END(advance_mu_t_tim)

BENCH_START(spec_bdy_t_tim)
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

           CALL spec_bdyupdate(muts, mu_tend, dts_rk,      &
                               'm'         , config_flags, &
                               spec_zone,                  &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               grid%i_start(ij), grid%i_end(ij),         &
                               grid%j_start(ij), grid%j_end(ij),         &
                               1    , 1             )
         ENDIF
BENCH_END(spec_bdy_t_tim)

         ! sumflux accumulates the time-averged mass flux
         ! (time averaged over the acoustic steps) for use
         ! in the scalar advection (flux divergence).  Using
         ! time averaged values gives us exact scalar conservation.

BENCH_START(sumflux_tim)
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
BENCH_END(sumflux_tim)

         ! small (acoustic) step for the vertical momentum,
         ! density and coupled potential temperature.


BENCH_START(advance_w_tim)
        IF ( non_hydrostatic ) THEN
          CALL advance_w( w_2, rw_tend, ww, u_2, v_2,       &
                          mu_2, mut, muave, muts,           &
                          t_2save, t_2, t_save,             &
                          ph_2, ph_save, phb, ph_tend,      &
                          ht, c2a, cqw, alt, alb,           &
                          a, alpha, gamma,                  &
                          rdx, rdy, dts_rk, t0, epssm,      &
                          dnw, fnm, fnp, rdnw, rdn,         &
                          cf1, cf2, cf3, msft,              &
                          config_flags,                     &
                          ids,ide, jds,jde, kds,kde,        & ! domain dims
                          ims,ime, jms,jme, kms,kme,        & ! memory dims
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
        ENDIF
BENCH_END(advance_w_tim)

        IF( config_flags%specified .or. config_flags%nested ) THEN

BENCH_START(spec_bdynhyd_tim)
           IF (non_hydrostatic)  THEN
             CALL spec_bdyupdate_ph( ph_save, ph_2, ph_tend, mu_tend, muts, dts_rk, &
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
BENCH_END(spec_bdynhyd_tim)
        ENDIF

BENCH_START(cald_p_rho_tim)
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
                          muts, muus, muvs,                 &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end               )
BENCH_END(calc_mu_uv_tim)
BENCH_START(small_step_finish_tim)
      CALL small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,     &
                              t_2, t_1, ph_2, ph_1, ww, ww1,    &
                              mu_2, mu_1,                       &
                              mut, muts, muu, muus, muv, muvs,  & 
                              u_save, v_save, w_save,           &
                              t_save, ph_save, mu_save,         &
                              msfu, msfv, msft,                 &
                              h_diabatic,                       &
                              number_of_small_timesteps,dts_rk, &
                              rk_step, rk_order,                &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end               )
BENCH_END(small_step_finish_tim)

   END DO
   !$OMP END PARALLEL DO

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

BENCH_START(rk_scalar_tend_tim)
       CALL rk_scalar_tend (  im, im, config_flags,             &
                              rk_step, dt_rk,                   &
                              ru_m, rv_m, ww_m,                 &
                              mut, alt,                         &
                              moist(ims,kms,jms,im),            &
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
BENCH_END(rk_scalar_tend_tim)

BENCH_START(rlx_bdy_scalar_tim)
     IF( ( config_flags%specified .or. config_flags%nested ) .and. rk_step == 1 ) THEN 
         IF ( im .EQ. P_QV .OR. config_flags%nested ) THEN
            CALL relax_bdy_scalar ( moist_tend(ims,kms,jms,im),            & 
                                    moist(ims,kms,jms,im),  mut,         &
                                    moist_b(1,1,1,1,im),                   &
                                    moist_bt(1,1,1,1,im),                  &
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
                                    moist_b(1,1,1,1,im),                     &
                                    moist_bt(1,1,1,1,im),                    &
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
                              advect_tend, msft,                &
                              mu_1, mu_2, mub,                  &
                              rk_step, dt_rk, spec_zone,        &
                              epsts, leapfrog,config_flags,     &
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
BENCH_END(flow_depbdy_tim)

   ENDDO moist_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO moist_variable_loop

 ENDIF moist_scalar_advance

BENCH_START(tke_adv_tim)
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
   !$OMP END PARALLEL DO

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
   !$OMP END PARALLEL DO

   END IF TKE_advance
BENCH_END(tke_adv_tim)

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
                             chem(ims,kms,jms,ic),             &
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

   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   chem_tile_loop_2: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_update_scalar' )
       CALL rk_update_scalar( ic, ic,                           &
                              chem_old(ims,kms,jms,ic),         &  ! was chem_1
                              chem(ims,kms,jms,ic),             &
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
! come back to this and figure out why two different routines are needed. JM 20041203
#ifndef WRF_CHEM
           CALL flow_dep_bdy  ( chem(ims,kms,jms,ic),     &
                                ru_m, rv_m, config_flags,   &
                                spec_zone,                  &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),  &
                                grid%j_start(ij), grid%j_end(ij),  &
                                k_start, k_end                    )
#else
           CALL flow_dep_bdy_chem( chem(ims,kms,jms,ic),z,&
                                ru_m, rv_m, config_flags,alt,   &
                                t_1,pb,p,t0,p1000mb,rcp,ph_2,phb,g, &
                                spec_zone,ic,               &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                grid%i_start(ij), grid%i_end(ij),  &
                                grid%j_start(ij), grid%j_end(ij),  &
                                k_start, k_end                    )
#endif
       ENDIF


   ENDDO chem_tile_loop_2
   !$OMP END PARALLEL DO

   ENDDO chem_variable_loop

 ENDIF chem_scalar_advance

!  next the other scalar species
  other_scalar_advance: IF (num_3d_s >= PARAM_FIRST_SCALAR)  THEN

   scalar_variable_loop: do is = PARAM_FIRST_SCALAR, num_3d_s
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

   scalar_tile_loop_1: DO ij = 1 , grid%num_tiles

       CALL wrf_debug ( 200 , ' call rk_scalar_tend' )
       CALL rk_scalar_tend ( is, is, config_flags,             &
                             rk_step, dt_rk,                   &
                             ru_m, rv_m, ww_m,                 &
                             mut, alt,                         &
                             scalar(ims,kms,jms,is),           &
                             scalar_tend(ims,kms,jms,is),      &
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

     IF( config_flags%nested .and. (rk_step == 1) ) THEN

       IF (is .eq. P_QNI) THEN

         CALL relax_bdy_scalar ( scalar_tend(ims,kms,jms,is),            &
                                 scalar(ims,kms,jms,is),  mut,         &
                                 scalar_b(1,1,1,1,is),                   &
                                 scalar_bt(1,1,1,1,is),                  &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 grid%i_start(ij), grid%i_end(ij),      &
                                 grid%j_start(ij), grid%j_end(ij),      &
                                 k_start, k_end                        )

         CALL spec_bdy_scalar  ( scalar_tend(ims,kms,jms,is),                &
                                 scalar_b(1,1,1,1,is),                   &
                                 scalar_bt(1,1,1,1,is),                  &
                                 spec_bdy_width, spec_zone,                 &
                                 ijds, ijde,                 & ! min/max(id,jd)
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
           CALL flow_dep_bdy  ( scalar(ims,kms,jms,is),     &
                                ru_m, rv_m, config_flags,   &
                                spec_zone,                  &
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
                          al, alb, mu_2, muts,              &
                          ph_2, p, pb, t_2,                 &
                          p0, t0, znu, dnw, rdnw,           &
                          rdn, non_hydrostatic,             &
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
! moist               x
! chem                x
!scalar               x

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
#  include "PERIOD_BDY_EM_SCALAR2.inc"
#endif

BENCH_START(bc_end_tim)
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

BENCH_START(diag_w_tim)
     IF (.not. non_hydrostatic) THEN
     CALL diagnose_w( ph_tend, ph_2, ph_1, w_2, muts, dt_rk,  &
                      u_2, v_2, ht,                           &
                      cf1, cf2, cf3, rdx, rdy, msft,          &
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
   !$OMP END PARALLEL DO
BENCH_END(bc_end_tim)


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

! moist                       x

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

! chem                        x

     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_CHEM_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_CHEM_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then

!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *

! scalar                      x

     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_SCALAR_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_SCALAR_E_5.inc"
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

BENCH_START(advance_ppt_tim)
      CALL wrf_debug ( 200 , ' call advance_ppt' )
      CALL advance_ppt(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN, &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,    &
                     CUPPT, config_flags,                   &
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
BENCH_START(moist_physics_prep_tim)
       CALL moist_physics_prep_em( t_2, t_1, t0, rho,                &
                                   al, alb, p, p8w, p0, pb,          &
                                   ph_2, phb, th_phy, pi_phy, p_phy, &
                                   z, z_at_w, dz8w,                  &
                                   dtm, h_diabatic,                  &
                                   config_flags,fnm, fnp,            &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte,               &
                                   k_start    , k_end               )
BENCH_END(moist_physics_prep_tim)
   END DO scalar_tile_loop_1a
   !$OMP END PARALLEL DO

       CALL wrf_debug ( 200 , ' call microphysics_driver' )

BENCH_START(micro_driver_tim)
       sr = 0.
       specified_bdy = config_flags%specified .OR. config_flags%nested

       CALL microphysics_driver(                                          &
     &         DT=dtm             ,DX=dx              ,DY=dy              &
     &        ,DZ8W=dz8w          ,F_ICE_PHY=f_ice_phy                    &
     &        ,ITIMESTEP=itimestep                    ,LOWLYR=lowlyr      &
     &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy      &
     &        ,RHO=rho            ,SPEC_ZONE=spec_zone                    &
     &        ,SR=sr              ,TH=th_phy                              &
     &        ,WARM_RAIN=warm_rain                    ,XLAND=xland        &
     &        ,SPECIFIED=specified_bdy                                    &
     &        ,F_RAIN_PHY=f_rain_phy                                      &
     &        ,F_RIMEF_PHY=f_rimef_phy                                    &
     &        ,MP_PHYSICS=config_flags%mp_physics                         &
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
                 ! Optional
     &        , RAINNC=rainnc, RAINNCV=rainncv                            &
     &        , W=w_2, Z=z, HT=ht                                         &
     &        , MP_RESTART_STATE=mp_restart_state                         &
     &        , TBPVS_STATE=tbpvs_state                                   & ! etampnew
     &        , TBPVS0_STATE=tbpvs0_state                                 & ! etampnew
     &        , QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV              &
     &        , QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC              &
     &        , QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR              &
     &        , QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI              &
     &        , QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS              &
     &        , QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG              &
     &        , QNI_CURR=scalar(ims,kms,jms,P_QNI), F_QNI=F_QNI         &
                                                                          )

BENCH_END(micro_driver_tim)


       CALL wrf_debug ( 200 , ' call moist_physics_finish' )
BENCH_START(moist_phys_end_tim)
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   scalar_tile_loop_1b: DO ij = 1 , grid%num_tiles

       its = max(grid%i_start(ij),ids+sz)
       ite = min(grid%i_end(ij),ide-1-sz)
       jts = max(grid%j_start(ij),jds+sz)
       jte = min(grid%j_end(ij),jde-1-sz)

       CALL microphysics_zero_out (                                    &
                     moist , num_moist , config_flags ,              &
                     ids, ide, jds, jde, kds, kde,                     &
                     ims, ime, jms, jme, kms, kme,                     &
                     its, ite, jts, jte,                               &
                     k_start    , k_end                                )

       CALL moist_physics_finish_em( t_2, t_1, t0, muts, th_phy,       &
                                     h_diabatic, dtm, config_flags,    &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte,               &
                                     k_start    , k_end               )

       CALL calc_p_rho_phi( moist, num_3d_m,                &
                            al, alb, mu_2, muts,              &
                            ph_2, p, pb, t_2,                 &
                            p0, t0, znu, dnw, rdnw,           &
                            rdn, non_hydrostatic,             &
                            ids, ide, jds, jde, kds, kde,     &
                            ims, ime, jms, jme, kms, kme,     &
                            its, ite, jts, jte,               &
                            k_start    , k_end               )

   END DO scalar_tile_loop_1b
   !$OMP END PARALLEL DO
BENCH_END(moist_phys_end_tim)

  ENDIF

   chem_tile_loop_3: DO ij = 1 , grid%num_tiles

          CALL wrf_debug ( 200 , ' call scalar_tile_loop_2' )

     IF ( num_3d_c >= PARAM_FIRST_SCALAR ) then

!
!  tiled chemistry not here, it is called from solve_interface, and found in chem_driver
!

     END IF

   END DO chem_tile_loop_3


   IF (leapfrog ) THEN

    ! do time filter and switch for the dry variables

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij )

    DO ij = 1 , grid%num_tiles

BENCH_START(time_filt_tim)
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
BENCH_END(time_filt_tim)

    ENDDO
   !$OMP END PARALLEL DO

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
!  moist                         x
!   chem                         x
! scalar                         x
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
#  include "PERIOD_BDY_EM_SCALAR.inc"
#endif

!  now set physical b.c on a patch

BENCH_START(bc_2d_tim)
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
   if ( num_scalar >= PARAM_FIRST_SCALAR ) then
!-----------------------------------------------------------------------
! see above
!--------------------------------------------------------------
     CALL wrf_debug ( 200 , ' call HALO_RK_SCALAR' )
     IF      ( h_mom_adv_order <= 4 ) THEN
#      include "HALO_EM_SCALAR_E_3.inc"
     ELSE IF ( h_mom_adv_order <= 6 ) THEN
#      include "HALO_EM_SCALAR_E_5.inc"
     ELSE
       WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',h_mom_adv_order
       CALL wrf_error_fatal(TRIM(wrf_err_message))
     ENDIF
   endif
#endif

   CALL wrf_debug ( 200 , ' call end of solve_em' )

! Finish timers if compiled with -DBENCH.
#include <bench_solve_em_end.h>

! See comment before earlier #include of this file.
#define COPY_OUT
#include <em_scalar_derefs.inc>

   RETURN

END SUBROUTINE solve_em

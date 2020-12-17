!WRF:MODEL_LAYER:PHYSICS
MODULE module_fddaobs_driver

! This obs-nudging FDDA module (RTFDDA) is developed by the 
! NCAR/RAL/NSAP (National Security Application Programs), under the 
! sponsorship of ATEC (Army Test and Evaluation Commands). ATEC is 
! acknowledged for releasing this capability for WRF community 
! research applications.
!
! The NCAR/RAL RTFDDA module was adapted, and significantly modified 
! from the obs-nudging module in the standard MM5V3.1 which was originally 
! developed by PSU (Stauffer and Seaman, 1994). 
! 
! Yubao Liu (NCAR/RAL): lead developer of the RTFDDA module 
! Al Bourgeois (NCAR/RAL): lead engineer implementing RTFDDA into WRF-ARW
! Nov. 2006
! 
! References:
!   
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and J. Hacker, 2005: An
!     implementation of obs-nudging-based FDDA into WRF for supporting 
!     ATEC test operations. 2005 WRF user workshop. Paper 10.7.
!
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and W. Yu, 2006: An update 
!     on "obs-nudging"-based FDDA for WRF-ARW: Verification using OSSE 
!     and performance of real-time forecasts. 2006 WRF user workshop. Paper 4.7. 

!   
!   Stauffer, D.R., and N.L. Seaman, 1994: Multi-scale four-dimensional data 
!     assimilation. J. Appl. Meteor., 33, 416-434.
!
!   http://www.rap.ucar.edu/projects/armyrange/references.html
!
 
CONTAINS

!-----------------------------------------------------------------------
SUBROUTINE fddaobs_driver( inest, domid, parid, restart,         &
               config_flags,                                     &
               nudge_opt, iprt_errob, iprt_nudob,                &
               fdasta, fdaend,                                   &
               nudge_wind, nudge_temp, nudge_mois,               &
               nudge_pstr,                                       &
               coef_wind, coef_temp, coef_mois,                  &
               coef_pstr, rinxy, rinsig,                         &
               npfi, ionf,                                       &
               obs_prt_max, obs_prt_freq, idynin, dtramp,        &
               parent_grid_ratio, maxdom, itimestep,             &
               xtime,                                            &
               dt, gmt, julday,                                  &
#if ( EM_CORE == 1 ) 
               fdob,                                             &
#endif
               max_obs, nobs_ndg_vars,                           &
               nobs_err_flds, nstat, varobs, errf, dx,           &
               KPBL, HT, mut, muu, muv, c1h, c2h,                &
               msftx, msfty, msfux, msfuy, msfvx, msfvy, p_phy, t_tendf, t0,             &
               ub, vb, tb, qvb, pbase, ptop, pp, phb, ph,        &
               uratx, vratx, tratx, ru_tendf, rv_tendf,          &
               moist_tend, savwt,                                &
               regime, pblh, z_at_w,                             &
               z,                                                &
               ids,ide, jds,jde, kds,kde,                        & ! domain dims
               ims,ime, jms,jme, kms,kme,                        & ! memory dims
               its,ite, jts,jte, kts,kte                         ) ! tile   dims

!-----------------------------------------------------------------------
  USE module_domain
  ! USE module_bc  seems to not be needed
  USE module_model_constants, ONLY : g, rcp
  USE module_fddaobs_rtfdda

! This driver calls subroutines for fdda obs-nudging and
! returns computed tendencies

!
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
! taken from MM5 code - 03 Feb 2004.
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
!-- KPBL          vertical layer index for PBL top
!-- HT            terrain height (m)
!-- p_phy         pressure (Pa)
!-- t_tendf       temperature tendency

  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, intent(in)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, intent(in)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: domid(maxdom)           ! Domain IDs
  INTEGER, intent(in)  :: parid(maxdom)           ! Parent domain IDs
  LOGICAL, intent(in)  :: restart
  TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: nudge_opt
  LOGICAL, intent(in)  :: iprt_errob 
  LOGICAL, intent(in)  :: iprt_nudob 
  REAL, intent(in)     :: fdasta
  REAL, intent(in)     :: fdaend
  INTEGER, intent(in)  :: nudge_wind
  INTEGER, intent(in)  :: nudge_temp
  INTEGER, intent(in)  :: nudge_mois
  INTEGER, intent(in)  :: nudge_pstr
  REAL, intent(in) :: coef_wind
  REAL, intent(in) :: coef_temp
  REAL, intent(in) :: coef_mois
  REAL, intent(in) :: coef_pstr
  REAL, intent(inout)  :: rinxy
  REAL, intent(inout)  :: rinsig
  INTEGER, intent(in) :: npfi
  INTEGER, intent(in) :: ionf
  INTEGER, intent(in) :: obs_prt_max      ! max number of obs in printout
  INTEGER, intent(in) :: obs_prt_freq     ! frequency (in obs index) printout 
  INTEGER, intent(in) :: idynin
  REAL, intent(inout) :: dtramp
  INTEGER, intent(in) :: parent_grid_ratio
  REAL, intent(in)     :: xtime           ! forecast time in minutes
  REAL, intent(in)     :: dt
  REAL, intent(in)     :: gmt
  INTEGER, intent(in)  :: julday
  INTEGER, intent(in)  :: max_obs         ! max number of observations
  INTEGER, intent(in)  :: nobs_ndg_vars
  INTEGER, intent(in)  :: nobs_err_flds
  INTEGER, intent(in)  :: nstat
  REAL, intent(inout)  :: varobs(nobs_ndg_vars, max_obs)
  REAL, intent(inout)  :: errf(nobs_err_flds, max_obs)
  REAL, intent(in)     :: dx           ! this-domain grid cell-size (m)
  INTEGER, INTENT(IN) :: kpbl( ims:ime, jms:jme ) ! vertical layer index for PBL top
  REAL, INTENT(IN) :: ht( ims:ime, jms:jme )
  REAL, INTENT(IN) :: mut( ims:ime , jms:jme )   ! Air mass on t-grid 
  REAL, INTENT(IN) :: muu( ims:ime , jms:jme )   ! Air mass on u-grid 
  REAL, INTENT(IN) :: muv( ims:ime , jms:jme )   ! Air mass on v-grid
  REAL, INTENT(IN) :: msftx( ims:ime , jms:jme )  ! Map scale on t-grid
  REAL, INTENT(IN) :: msfty( ims:ime , jms:jme )  ! Map scale on t-grid
  REAL, INTENT(IN) :: msfux( ims:ime , jms:jme )  ! Map scale on u-grid
  REAL, INTENT(IN) :: msfuy( ims:ime , jms:jme )  ! Map scale on u-grid
  REAL, INTENT(IN) :: msfvx( ims:ime , jms:jme )  ! Map scale on v-grid
  REAL, INTENT(IN) :: msfvy( ims:ime , jms:jme )  ! Map scale on v-grid

  REAL, INTENT(IN) :: c1h( kms:kme )   ! Hybrid coordinate, weighting function
  REAL, INTENT(IN) :: c2h( kms:kme )   ! Hybrid coordinate, weighting function

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(IN) :: t0
  REAL, INTENT(INOUT) :: savwt( nobs_ndg_vars, ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: regime( ims:ime, jms:jme )
  REAL, INTENT(IN) :: pblh( ims:ime, jms:jme )
  REAL, INTENT(IN) :: z_at_w( ims:ime, kms:kme, jms:jme ) ! Model ht(m) asl, f-levs
  REAL, INTENT(IN) :: z( ims:ime, kms:kme, jms:jme )  ! Model ht(m) asl, h-levs

#if ( EM_CORE == 1 ) 
  TYPE(fdob_type), intent(inout)  :: fdob
#endif

  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme ) ! Base press. (Pa)
  REAL,   INTENT(IN) :: ptop
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme )  ! Press. pert. (Pa)
  REAL,   INTENT(IN) :: phb( ims:ime, kms:kme, jms:jme ) ! Base geopotential
  REAL,   INTENT(IN) :: ph( ims:ime, kms:kme, jms:jme )  ! Perturbation geopotential
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme )     ! On mass points
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme )     !       "
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme )     !       "
  REAL,   INTENT(INOUT) :: ru_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: rv_tendf( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(INOUT) :: moist_tend( ims:ime, kms:kme, jms:jme )

! Local variables
  logical            :: nudge_flag   ! Flag for doing nudging 
  integer            :: KTAU         ! Forecast timestep
  real               :: dtmin        ! dt in minutes
  integer            :: i, j, k      ! Loop counters.
  integer            :: idom         ! Loop counter.
  integer            :: nsta         ! Number of observation stations
  integer            :: infr         ! Frequency for obs input and error calc 
  integer            :: idarst       ! Flag for calling sub errob on restart
  real               :: dtr          ! Abs value of dtramp (for dynamic init)
  real               :: tconst       ! Reciprocal of dtr
  real    :: vih_uv(its:ite,jts:jte,2) ! Vert infl heights abv grd for LML obs (wind)
  real    :: vih_t (its:ite,jts:jte,2) ! Vert infl heights abv grd for LML obs (temp)
  real    :: vih_q (its:ite,jts:jte,2) ! Vert infl heights abv grd for LML obs (mois)
  integer :: vik_uv(its:ite,jts:jte,2) ! Vert infl k-levels for LML obs (wind)
  integer :: vik_t (its:ite,jts:jte,2) ! Vert infl k-levels for LML obs (temp)
  integer :: vik_q (its:ite,jts:jte,2) ! Vert infl k-levels for LML obs (mois)
  real    :: z_at_p( kms:kme )       ! Height at p levels
#ifdef RAL
  real    :: HTIJ(ids:ide, jds:jde) = 0.  ! Terrain ht on global grid
#endif
  character(len=200) :: msg  ! Argument to wrf_message

#if ( EM_CORE == 1 ) 
  nudge_flag = (nudge_opt  .eq. 1)

  if (.not. nudge_flag) return

!----------------------------------------------------------------------
! ***************       BEGIN FDDA SETUP SECTION        ***************

! Calculate forecast time.
  dtmin = dt/60.     
  ktau  = itimestep - 1        !ktau corresponds to xtime

! Set NSTA to zero on startup, or else retrieve value from last pass.
  IF(ktau.EQ.fdob%ktaur) THEN
     if (iprt_nudob) then
        write(msg,'(a,i2,a)') 'OBS NUDGING is requested on a total of ',   &
                              fdob%domain_tot,' domain(s).'
        call wrf_message(msg)
     endif
     nsta=0.
  ELSE
     nsta=fdob%nstat
  ENDIF
  
  infr = ionf*(parent_grid_ratio**fdob%levidn(inest))
  nsta=fdob%nstat
  idarst = 0
  IF(restart .AND. ktau.EQ.fdob%ktaur) idarst=1

  CALL wrf_debug(100,'in PSU FDDA scheme')

! Make sure regime array is set over entire grid
! (ajb: Copied code from fddagd)
    IF( config_flags%sf_sfclay_physics /= sfclayscheme &
  .AND. config_flags%sf_sfclay_physics /= mynnsfcscheme &
  .AND. config_flags%sf_sfclay_physics /= pxsfcscheme &
  .AND. config_flags%sf_sfclay_physics /= sfclayrevscheme ) THEN
      DO j = jts, jte
      DO i = its, ite
           IF( pblh(i,j) > z_at_w(i,2,j)-ht(i,j) ) THEN
             regime(i,j) = 4.0
           ELSE
             regime(i,j) = 1.0
           ENDIF
      ENDDO
      ENDDO
    ENDIF

! Compute VIF heights for each grid column (used for LML obs)
   if(fdob%sfc_scheme_vert.EQ.0) then
     if(nudge_wind.EQ.1 .AND. NSTA.GT.0)  then
       CALL compute_VIH( fdob%vif_uv, fdob%vif_max,                &
                         fdob%vif_fullmin, fdob%vif_rampmin,       &
                         regime, pblh,                             &
                         ht, z, vih_uv,                            &
                         ids,ide, jds,jde, kds,kde,                &
                         ims,ime, jms,jme, kms,kme,                &
                         its,ite, jts,jte, kts,kte )
     endif
     if(nudge_temp.EQ.1 .AND. NSTA.GT.0)  then
       CALL compute_VIH( fdob%vif_t, fdob%vif_max,                 &
                         fdob%vif_fullmin, fdob%vif_rampmin,       &
                         regime, pblh,                             &
                         ht, z, vih_t,                             &
                         ids,ide, jds,jde, kds,kde,                &
                         ims,ime, jms,jme, kms,kme,                &
                         its,ite, jts,jte, kts,kte )
     endif
     if(nudge_mois.EQ.1 .AND. NSTA.GT.0)  then
       CALL compute_VIH( fdob%vif_q, fdob%vif_max,                 &
                         fdob%vif_fullmin, fdob%vif_rampmin,       &
                         regime, pblh,                             &
                         ht, z, vih_q,                             &
                         ids,ide, jds,jde, kds,kde,                &
                         ims,ime, jms,jme, kms,kme,                &
                         its,ite, jts,jte, kts,kte )
     endif
   endif
!********************* END AJB MOVE TO SLAB ***************************

! COMPUTE ERROR BETWEEN OBSERVATIONS and MODEL
  IF( nsta.GT.0 ) THEN
    IF( MOD(ktau,infr).EQ.0 .OR. idarst.EQ.1) THEN

        CALL errob(inest, ub, vb, tb, t0, qvb, pbase, pp, rcp,       &
                   z,                                                &
                   uratx, vratx, tratx, kpbl,                        &
                   nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,    &
                   fdob%levidn, parid, fdob%nstat, fdob%nstaw,       &
                   nudge_wind, nudge_temp, nudge_mois, nudge_pstr,   &
                   fdob%timeob, fdob%rio, fdob%rjo, fdob%rko,        &
                   varobs, errf, ktau, xtime,                        &
                   parent_grid_ratio, npfi,                          &
                   obs_prt_max, obs_prt_freq, iprt_errob,            &
                   fdob%obsprt, fdob%stnidprt,                       &
                   fdob%latprt, fdob%lonprt,                         &
                   fdob%mlatprt, fdob%mlonprt,                       &
                   ids,ide, jds,jde, kds,kde,                        &
                   ims,ime, jms,jme, kms,kme,                        &
                   its,ite, jts,jte, kts,kte)
    ENDIF
  ENDIF

  fdob%tfaci=1.0
  IF(idynin.EQ.1.AND.nudge_opt.EQ.1) THEN
    dtr=ABS(dtramp)
    tconst=1./dtr
! FDAEND(IN) IS THE TIME IN MINUTES TO END THE DYNAMIC INITIALIZATION CY
    IF(xtime.LT.fdaend-dtr)THEN
      fdob%tfaci=1.
    ELSEIF(xtime.GE.fdaend-dtr.AND.xtime.LE.fdaend) THEN
      fdob%tfaci=(fdaend-xtime)*tconst
    ELSE
      fdob%tfaci=0.0
    ENDIF
    IF(ktau.EQ.fdob%ktaur.OR.MOD(ktau,10).EQ.0) THEN
      IF (iprt_nudob)                                                  &
         PRINT*,' DYNINOBS: IN,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST',   &
         ',TFACI: ',INEST,KTAU,XTIME,FDAEND,DTRAMP,DTR,TCONST,         &
         fdob%TFACI
    ENDIF
  ENDIF

#ifdef RAL
! MEIXU: collect terrain array HT into a global array HTIJ
  CALL loc2glob(HT, HTIJ, "2D", "REAL",                  &
                ids,ide, jds,jde, kds,kde,               &
                ims,ime, jms,jme, kms,kme )
! MEIXU end
#endif
!
! ***************        END FDDA SETUP SECTION         ***************
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! ***************         BEGIN NUDGING SECTION         ***************

  DO J = jts, jte
!
! IF NUDGING SURFACE WINDS IN THE BOUNDARY LAYER, IF IWINDS(INEST+2)=1
! USE A SIMILARITY CORRECTION BASED ON ROUGHNESS TO APPLY 10M
! WIND TO THE SURFACE LAYER (K=KL) AT 40M.  TO DO THIS WE MUST
! STORE ROUGHNESS AND REGIME FOR EACH J SLICE AFTER THE CALL TO
! HIRPBL FOR LATER USE IN BLNUDGD.
!
!--- OBS NUDGING FOR TEMP AND MOISTURE
!
     NSTA=NSTAT
     IF(J .GT. 2 .and. J .LT. jde-1) THEN
       IF(nudge_temp.EQ.1 .AND. NSTA.GT.0)  &
       THEN
!         write(6,*) 'calling nudob: IVAR=3, J = ',j
          CALL nudob(J, 3, t_tendf(ims,kms,j),                       &
                  inest, restart, ktau, fdob%ktaur, xtime,           &
                  mut(ims,j), c1h, c2h, msftx(ims,j), msfty(ims,j),  &
                  nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,     &
                  npfi, ionf, rinxy, fdob%window,                    &
                  fdob%nudge_t_pbl,                                  &
                  fdob%sfcfact, fdob%sfcfacr,                        &
                  fdob%levidn,                                       &
                  parid, nstat,                                      &
                  fdob%rinfmn, fdob%rinfmx, fdob%pfree,              &
                  fdob%dcon, fdob%tfaci,                             &
                  fdob%sfc_scheme_horiz, fdob%sfc_scheme_vert,       &
                  fdob%max_sndng_gap,                                &
                  fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,          &
                  parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,  &
                  fdob%rko, fdob%timeob, varobs, errf,               &
                  pbase(ims,kms,j), ptop, pp(ims,kms,j),             &
                  nudge_wind, nudge_temp, nudge_mois,                &
                  coef_wind, coef_temp, coef_mois,                   &
                  savwt(1,ims,kms,j), kpbl(ims,j), 0,                &
                  vih_t(its,j,1), vih_t(its,j,2), ht(ims,j),         &
                  z(ims,kms,j),                                      &
                  iprt_nudob,                                        &
                  ids,ide, jds,jde, kds,kde,                         & ! domain dims
                  ims,ime, jms,jme, kms,kme,                         & ! memory dims
                  its,ite, jts,jte, kts,kte,                         & ! tile   dims
                  qvb, config_flags%obs_scl_neg_qv_innov ) ! Water vapor mixing ratio / scale negative qv innovations
!         write(6,*) 'return from nudob: IVAR=3, J = ',j
       ENDIF

       IF(nudge_mois.EQ.1 .AND. NSTA.GT.0)  &
       THEN
!         write(6,*) 'calling nudob: IVAR=4, J = ',j
          CALL nudob(J, 4, moist_tend(ims,kms,j),                    &
                  inest, restart, ktau, fdob%ktaur, xtime,           &
                  mut(ims,j), c1h, c2h, msftx(ims,j), msfty(ims,j),  &
                  nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,     &
                  npfi, ionf, rinxy, fdob%window,                    &
                  fdob%nudge_q_pbl,                                  &
                  fdob%sfcfact, fdob%sfcfacr,                        &
                  fdob%levidn,                                       &
                  parid, nstat,                                      &
                  fdob%rinfmn, fdob%rinfmx, fdob%pfree,              &
                  fdob%dcon, fdob%tfaci,                             &
                  fdob%sfc_scheme_horiz, fdob%sfc_scheme_vert,       &
                  fdob%max_sndng_gap,                                &
                  fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,          &
                  parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,  &
                  fdob%rko, fdob%timeob, varobs, errf,               &
                  pbase(ims,kms,j), ptop, pp(ims,kms,j),             &
                  nudge_wind, nudge_temp, nudge_mois,                &
                  coef_wind, coef_temp, coef_mois,                   &
                  savwt(1,ims,kms,j), kpbl(ims,j), 0,                &
                  vih_q(its,j,1), vih_q(its,j,2), ht(ims,j),         &
                  z(ims,kms,j),                                      &
                  iprt_nudob,                                        &
                  ids,ide, jds,jde, kds,kde,                         & ! domain dims
                  ims,ime, jms,jme, kms,kme,                         & ! memory dims
                  its,ite, jts,jte, kts,kte,                         & ! tile   dims
                  qvb, config_flags%obs_scl_neg_qv_innov ) ! Water vapor mixing ratio / scale negative qv innovations
!         write(6,*) 'return from nudob: IVAR=4, J = ',j
       ENDIF
     ENDIF

     IF(nudge_wind.EQ.1 .AND. NSTA.GT.0)    &
     THEN
!         write(6,*) 'calling nudob: IVAR=1, J = ',j
        CALL nudob(J, 1, ru_tendf(ims,kms,j),                        &
                inest, restart, ktau, fdob%ktaur, xtime,             &
                muu(ims,j), c1h, c2h, msfux(ims,j), msfuy(ims,j),    &
                nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,       &
                npfi, ionf, rinxy, fdob%window,                      &
                fdob%nudge_uv_pbl,                                   &
                fdob%sfcfact, fdob%sfcfacr,                          &
                fdob%levidn,                                         &
                parid, nstat,                                        &
                fdob%rinfmn, fdob%rinfmx, fdob%pfree,                &
                fdob%dcon, fdob%tfaci,                               &
                fdob%sfc_scheme_horiz, fdob%sfc_scheme_vert,         &
                fdob%max_sndng_gap,                                  &
                fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,            &
                parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,    &
                fdob%rko, fdob%timeob, varobs, errf,                 &
                pbase(ims,kms,j), ptop, pp(ims,kms,j),               &
                nudge_wind, nudge_temp, nudge_mois,                  &
                coef_wind, coef_temp, coef_mois,                     &
                savwt(1,ims,kms,j), kpbl(ims,j), 0,                  &
                vih_uv(its,j,1), vih_uv(its,j,2), ht(ims,j),         &
                z(ims,kms,j),                                        &
                iprt_nudob,                                          &
                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                its,ite, jts,jte, kts,kte,                           & ! tile   dims
                qvb, config_flags%obs_scl_neg_qv_innov ) ! Water vapor mixing ratio / scale negative qv innovations
!       write(6,*) 'return from nudob: IVAR=1, J = ',j

!       write(6,*) 'calling nudob: IVAR=2, J = ',j
        CALL nudob(J, 2, rv_tendf(ims,kms,j),                        &
                inest, restart, ktau, fdob%ktaur, xtime,             &
                muv(ims,j), c1h, c2h, msfvx(ims,j), msfvy(ims,j),    &
                nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,       &
                npfi, ionf, rinxy, fdob%window,                      &
                fdob%nudge_uv_pbl,                                   &
                fdob%sfcfact, fdob%sfcfacr,                          &
                fdob%levidn,                                         &
                parid, nstat,                                        &
                fdob%rinfmn, fdob%rinfmx, fdob%pfree,                &
                fdob%dcon, fdob%tfaci,                               &
                fdob%sfc_scheme_horiz, fdob%sfc_scheme_vert,         &
                fdob%max_sndng_gap,                                  &
                fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,            &
                parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,    &
                fdob%rko, fdob%timeob, varobs, errf,                 &
                pbase(ims,kms,j), ptop, pp(ims,kms,j),               &
                nudge_wind, nudge_temp, nudge_mois,                  &
                coef_wind, coef_temp, coef_mois,                     &
                savwt(1,ims,kms,j), kpbl(ims,j), 0,                  &
                vih_uv(its,j,1), vih_uv(its,j,2), ht(ims,j),         &
                z(ims,kms,j),                                        &
                iprt_nudob,                                          &
                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                its,ite, jts,jte, kts,kte,                           & ! tile   dims
                qvb, config_flags%obs_scl_neg_qv_innov ) ! Water vapor mixing ratio / scale negative qv innovations
!       write(6,*) 'return from nudob: IVAR=2, J = ',j
     ENDIF
  ENDDO
!
! --- END OF 4DDA
!
  RETURN
#endif
  END SUBROUTINE fddaobs_driver

  SUBROUTINE compute_VIH(vif, hmax, fullmin, rampmin,       &
                         regime, pblh, terrh, z, vih,       &
                         ids,ide, jds,jde, kds,kde,         & ! domain dims
                         ims,ime, jms,jme, kms,kme,         &
                         its,ite, jts,jte, kts,kte)

  USE module_fddaobs_rtfdda

  IMPLICIT NONE
!*******************************************************************************
!*****     COMPUTE HEIGHTS FOR SURFACE OBS VERTICAL INFLUENCE FUNCTION     *****
!*******************************************************************************

  REAL,    INTENT(IN)  :: vif(6)                     ! Vert infl params for regimes
  REAL,    INTENT(IN)  :: hmax                       ! Max height to apply nudging
  REAL,    INTENT(IN)  :: fullmin                    ! Min height of full nudging
  REAL,    INTENT(IN)  :: rampmin                    ! Min height to ramp full-to-0 
  REAL,    INTENT(IN)  :: regime(ims:ime,jms:jme)    ! Stability regime
  REAL,    INTENT(IN)  :: pblh(ims:ime,jms:jme)      ! PBL height (m)
  REAL,    INTENT(IN)  :: terrh(ims:ime,jms:jme)     ! Terrain ht (m)
  REAL,    INTENT(IN)  :: z(ims:ime,kms:kme,jms:jme) ! Ht (m) above sl (half levs)
  REAL,    INTENT(OUT) :: vih(its:ite,jts:jte,2)     ! Vt infl hts abv grd for LML obs
! INTEGER, INTENT(OUT) :: vik(its:ite,jts:jte,2)     ! Vert infl k-levels for LML obs
  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

! Local variables
  real  :: fullr(its:ite)    ! Height up to full vertical weighting
  real  :: rampr(its:ite)    ! Height of ramp-down to zero weighting
  character(len=200) :: msg  ! Argument to wrf_message
  integer:: i, j             ! Loop counters

  integer k     ! ajb test

! Do J-slabs
  do j = jts, jte

!   Set fullr and rampr values according to regimes
    do i = its, ite

      if(regime(i,j).eq.1.0) then        ! REGIME 1
        fullr(i) = vif(1)
        rampr(i) = vif(2)
      elseif(regime(i,j).eq.2.0) then    ! REGIME 2
        fullr(i) = vif(3)
        rampr(i) = vif(4)
      elseif(regime(i,j).eq.3.0 .or. regime(i,j).eq.4.0) then    ! REGIME 4
        fullr(i) = vif(5)
        rampr(i) = vif(6)
      else
        write(msg,'(a,f5.1,2(a,i4))') 'Unknown regime type ', regime(i,j),    &
                                 ' at grid coordinate i = ',i,' j = ',j
        call wrf_message(msg)
        call wrf_error_fatal ( 'fddaobs_driver: compute_VIH STOP' )
        
      endif

    enddo

!   Get vert infl heights for LML obs, from fullr, rampr, and pblh
    CALL get_vif_hts_slab(fullr, rampr, pblh(ims,j),         &
                          hmax, fullmin, rampmin,            &
                          vih(its,j,1), vih(its,j,2),        &
                          ims,ime, its,ite)
  enddo
  END SUBROUTINE compute_VIH

  SUBROUTINE get_vif_hts_slab(fullr, rampr, pblh, hmax, fullmin, rampmin, &
                              ht1, ht2, ims,ime, its,ite)
! Compute VIF heights 

  IMPLICIT NONE

  REAL, INTENT(IN)    :: fullr(its:ite)   ! Height up to full vertical weighting
  REAL, INTENT(IN)    :: rampr(its:ite)   ! Height of ramp-down to zero weighting
  REAL, INTENT(IN)    :: pblh(ims:ime)    ! PBL height (m)
  REAL, INTENT(IN)    :: hmax             ! Max height to apply nudging
  REAL, INTENT(IN)    :: fullmin          ! Min height of full nudging
  REAL, INTENT(IN)    :: rampmin          ! Min height to ramp full-to-0 
  REAL, INTENT(OUT)   :: ht1(its:ite)     ! Vert infl fcn height 1
  REAL, INTENT(OUT)   :: ht2(its:ite)     ! Vert infl fcn height 2
  INTEGER, INTENT(IN) :: ims,ime          ! Memory dims.
  INTEGER, INTENT(IN) :: its,ite          ! Tile   dims.

! Local variables
  integer :: i

  do i = its, ite

! Determine lower height (below which the VIF=1 for full weighting)
    if(fullr(i).ge.0.0) then        ! fullr is height from ground
      ht1(i) = fullr(i)
    else                            ! fullr is relative to pbl (-5000 bias)
      ht1(i) = pblh(i) - (fullr(i)+5000.)
    endif

!   Height ht1 can be no smaller than fullmin
    ht1(i) = max(fullmin,ht1(i))

! Determine upper height (to which the VIF ramps down to zero weighting)
! NOTE: Height of ramp-down (ht2-ht1) can be no smaller than rampmin

    if(rampr(i).ge.0.0) then
!     rampr is height from ht1
      ht2(i) = ht1(i) + max(rampmin,rampr(i))
    else
!     rampr is relative to pbl (-5000 bias)
      ht2(i) = max( ht1(i)+rampmin, pblh(i)-(rampr(i)+5000.) )
    endif

! Apply hmax
    ht1(i) = min(ht1(i), hmax-rampmin)
    ht2(i) = min(ht2(i), hmax) 
  enddo
  END SUBROUTINE get_vif_hts_slab

  SUBROUTINE get_vik_slab( h, hlevs, ht, vik, ims,ime, kms,kme, its,ite, kts,kte )

! Compute VIK values from heights on j-slab 

  IMPLICIT NONE

  REAL,    INTENT(IN) :: h(its:ite)          ! height (m) above ground, on j-slab
  REAL,    INTENT(IN) :: hlevs(ims:ime,kms:kme) ! hgt (m) abv grd at modl levs (slab)
  REAL,    INTENT(IN) :: ht(ims:ime)         ! terrain height (m) (slab)
  INTEGER, INTENT(OUT):: vik(its:ite)        ! vert infl k levels (slab)
  INTEGER, INTENT(IN) :: ims,ime, kms,kme    ! memory dims
  INTEGER, INTENT(IN) :: its,ite, kts,kte    ! tile   dims

! Local variables
  integer :: i
  integer :: k
  real    :: ht_ag(kts:kte)

  do i = its, ite

!   Get column of height-above-ground values for this i coord
    do k = kts,kte
       ht_ag(k) = hlevs(i,k) - ht(i)
    enddo
!   Get k levels that correspond to height values
    vik(i) = ht_to_k( h(i), ht_ag, kts,kte ) 
  enddo
  END SUBROUTINE get_vik_slab

  INTEGER FUNCTION ht_to_k( h, hlevs, kts,kte )
  IMPLICIT NONE

  REAL,    INTENT(IN)  :: h                     ! height value (m)
  REAL,    INTENT(IN)  :: hlevs(kts:kte)        ! model height levels
  INTEGER, INTENT(IN)  :: kts,kte               ! tile dims

! Local variables
  INTEGER :: k               ! loop counter
  INTEGER :: klo             ! lower k bound

  KLEVS: do k = kts, kte
    klo = k-1
    if(h .le. hlevs(k)) then
      EXIT KLEVS
    endif
  enddo KLEVS
  klo = max0(1,klo)
  ht_to_k = min0(kte,klo)
  RETURN
  END FUNCTION ht_to_k

END MODULE module_fddaobs_driver

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
               nudge_opt, iprt_errob, iprt_nudob,                &
               fdasta, fdaend,                                   &
               nudge_wind, nudge_temp, nudge_mois,               &
               nudge_pstr,                                       &
               coef_wind, coef_temp, coef_mois,                  &
               coef_pstr, rinxy, rinsig,                         &
               npfi, ionf,                                       &
               obs_prt_max, obs_prt_freq, idynin, dtramp,        &
               parent_grid_ratio, maxdom, itimestep,             &
               dt, gmt, julday,                                  &
#if ( EM_CORE == 1 ) 
               fdob,                                             &
#endif
               max_obs, nobs_ndg_vars,                           &
               nobs_err_flds, nstat, varobs, errf, dx,           &
               KPBL, HT, mut, muu, muv,                          &
               msftx, msfty, msfux, msfuy, msfvx, msfvy, p_phy, t_tendf, t0,             &
               ub, vb, tb, qvb, pbase, ptop, pp, phb, ph,        &
               uratx, vratx, tratx, ru_tendf, rv_tendf,          &
               moist_tend, savwt,                                &
               ids,ide, jds,jde, kds,kde,                        & ! domain dims
               ims,ime, jms,jme, kms,kme,                        & ! memory dims
               its,ite, jts,jte, kts,kte                         ) ! tile   dims

!-----------------------------------------------------------------------
  USE module_domain
  USE module_bc
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

  REAL, INTENT(IN) :: p_phy( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(INOUT) :: t_tendf( ims:ime, kms:kme, jms:jme )
  REAL, INTENT(IN) :: t0
  REAL, INTENT(INOUT) :: savwt( nobs_ndg_vars, ims:ime, kms:kme, jms:jme )

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
  real               :: xtime        ! Forecast time in minutes
  real               :: dtmin        ! dt in minutes
  integer            :: i, j, k      ! Loop counters.
  integer            :: idom         ! Loop counter.
  integer            :: nsta         ! Number of observation stations
  integer            :: infr         ! Frequency for obs input and error calc 
  integer            :: idarst       ! Flag for calling sub errob on restart
  real               :: dtr          ! Abs value of dtramp (for dynamic init)
  real               :: tconst       ! Reciprocal of dtr
  integer :: KPBLJ(its:ite)          ! 1D temp array.
#ifdef RAL
  real    :: HTIJ(ids:ide, jds:jde) = 0.  ! Terrain ht on global grid.
#endif

#if ( EM_CORE == 1 ) 
  nudge_flag = (nudge_opt  .eq. 1)

  if (.not. nudge_flag) return

!----------------------------------------------------------------------
! ***************       BEGIN FDDA SETUP SECTION        ***************

! Calculate forecast time.
  dtmin = dt/60.     
  xtime = dtmin*(itimestep-1)
  ktau  = itimestep - 1        !ktau corresponds to xtime

! DEFINE NSTA WHEN NOT NUDGING TO IND. OBS.
! print *,'in fddaobs_driver, xtime=',xtime
  IF(ktau.EQ.fdob%ktaur) THEN
     IF (iprt_nudob) PRINT *,3333,fdob%domain_tot
!    print *,'ktau,ktaur,inest=',ktau,fdob%ktaur,inest
3333 FORMAT(1X,'IN fddaobs_driver: I4DITOT = ',I2)
     nsta=0.
  ELSE
     nsta=fdob%nstat
  ENDIF
  
  infr = ionf*(parent_grid_ratio**fdob%levidn(inest))
  nsta=fdob%nstat
  idarst = 0
  IF(restart .AND. ktau.EQ.fdob%ktaur) idarst=1

! COMPUTE ERROR BETWEEN OBSERVATIONS and MODEL
  IF( nsta.GT.0 ) THEN
    IF( MOD(ktau,infr).EQ.0 .OR. idarst.EQ.1) THEN

        CALL errob(inest, ub, vb, tb, t0, qvb, pbase, pp, rcp,       &
                   phb, ph, g,                                       &
                   uratx, vratx, tratx, kpbl,                        &
                   nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,    &
                   fdob%levidn, parid, fdob%nstat, fdob%nstaw,       &
                   nudge_wind, nudge_temp, nudge_mois, nudge_pstr,   &
                   fdob%timeob, fdob%rio, fdob%rjo, fdob%rko,        &
                   varobs, errf, ktau, xtime,                        &
                   parent_grid_ratio, npfi,                          &
                   obs_prt_max, obs_prt_freq, iprt_errob,            &
                   fdob%obsprt, fdob%latprt, fdob%lonprt,            &
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
     DO I=its,ite
       KPBLJ(I)=KPBL(I,J)
     ENDDO
!
!--- OBS NUDGING FOR TEMP AND MOISTURE
!
     NSTA=NSTAT
     IF(J .GT. 2 .and. J .LT. jde-1) THEN
       IF(nudge_temp.EQ.1 .AND. NSTA.GT.0)  &
       THEN
!         write(6,*) 'calling nudob: IVAR=3, J = ',j
          CALL NUDOB(J, 3, t_tendf(ims,kms,j),                       &
                  inest, restart, ktau, fdob%ktaur, xtime,           &
                  mut(ims,j), msftx(ims,j), msfty(ims,j),            &
                  nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,     &
                  npfi, ionf, rinxy, fdob%window,                    &
                  fdob%nudge_t_pbl,                                  &
                  fdob%sfcfact, fdob%sfcfacr,                        &
                  fdob%levidn,                                       &
                  parid, nstat,                                      &
                  fdob, fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,    &
                  parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,  &
                  fdob%rko, fdob%timeob, varobs, errf,               &
                  pbase(ims,kms,j), ptop, pp(ims,kms,j),             &
                  nudge_wind, nudge_temp, nudge_mois,                &
                  coef_wind, coef_temp, coef_mois,                   &
                  savwt(1,ims,kms,j), kpblj, 0,                      &
                  iprt_nudob,                                        &
                  ids,ide, jds,jde, kds,kde,                         & ! domain dims
                  ims,ime, jms,jme, kms,kme,                         & ! memory dims
                  its,ite, jts,jte, kts,kte         )                  ! tile   dims
!         write(6,*) 'return from nudob: IVAR=3, J = ',j
       ENDIF

       IF(nudge_mois.EQ.1 .AND. NSTA.GT.0)  &
       THEN
!         write(6,*) 'calling nudob: IVAR=4, J = ',j
          CALL NUDOB(J, 4, moist_tend(ims,kms,j),                    &
                  inest, restart, ktau, fdob%ktaur, xtime,           &
                  mut(ims,j), msftx(ims,j), msfty(ims,j),            &
                  nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,     &
                  npfi, ionf, rinxy, fdob%window,                    &
                  fdob%nudge_q_pbl,                                  &
                  fdob%sfcfact, fdob%sfcfacr,                        &
                  fdob%levidn,                                       &
                  parid, nstat,                                      &
                  fdob, fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,    &
                  parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,  &
                  fdob%rko, fdob%timeob, varobs, errf,               &
                  pbase(ims,kms,j), ptop, pp(ims,kms,j),             &
                  nudge_wind, nudge_temp, nudge_mois,                &
                  coef_wind, coef_temp, coef_mois,                   &
                  savwt(1,ims,kms,j), kpblj, 0,                      &
                  iprt_nudob,                                        &
                  ids,ide, jds,jde, kds,kde,                         & ! domain dims
                  ims,ime, jms,jme, kms,kme,                         & ! memory dims
                  its,ite, jts,jte, kts,kte         )                  ! tile   dims
!         write(6,*) 'return from nudob: IVAR=4, J = ',j
       ENDIF
     ENDIF

     IF(nudge_wind.EQ.1 .AND. NSTA.GT.0)    &
     THEN
!       write(6,*) 'calling nudob: IVAR=1, J = ',j
        CALL NUDOB(J, 1, ru_tendf(ims,kms,j),                        &
                inest, restart, ktau, fdob%ktaur, xtime,             &
                muu(ims,j), msfux(ims,j), msfuy(ims,j),              &
                nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,       &
                npfi, ionf, rinxy, fdob%window,                      &
                fdob%nudge_uv_pbl,                                   &
                fdob%sfcfact, fdob%sfcfacr,                          &
                fdob%levidn,                                         &
                parid, nstat,                                        &
                fdob, fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,      &
                parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,    &
                fdob%rko, fdob%timeob, varobs, errf,                 &
                pbase(ims,kms,j), ptop, pp(ims,kms,j),               &
                nudge_wind, nudge_temp, nudge_mois,                  &
                coef_wind, coef_temp, coef_mois,                     &
                savwt(1,ims,kms,j), kpblj, 0,                        &
                iprt_nudob,                                          &
                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                its,ite, jts,jte, kts,kte         )                    ! tile   dims
!       write(6,*) 'return from nudob: IVAR=1, J = ',j

!       write(6,*) 'calling nudob: IVAR=2, J = ',j
        CALL NUDOB(J, 2, rv_tendf(ims,kms,j),                        &
                inest, restart, ktau, fdob%ktaur, xtime,             &
                muv(ims,j), msfvx(ims,j), msfvy(ims,j),              &
                nobs_ndg_vars, nobs_err_flds, max_obs, maxdom,       &
                npfi, ionf, rinxy, fdob%window,                      &
                fdob%nudge_uv_pbl,                                   &
                fdob%sfcfact, fdob%sfcfacr,                          &
                fdob%levidn,                                         &
                parid, nstat,                                        &
                fdob, fdob%lev_in_ob, fdob%plfo, fdob%nlevs_ob,      &
                parent_grid_ratio, dx, dtmin, fdob%rio, fdob%rjo,    &
                fdob%rko, fdob%timeob, varobs, errf,                 &
                pbase(ims,kms,j), ptop, pp(ims,kms,j),               &
                nudge_wind, nudge_temp, nudge_mois,                  &
                coef_wind, coef_temp, coef_mois,                     &
                savwt(1,ims,kms,j), kpblj, 0,                        &
                iprt_nudob,                                          &
                ids,ide, jds,jde, kds,kde,                           & ! domain dims
                ims,ime, jms,jme, kms,kme,                           & ! memory dims
                its,ite, jts,jte, kts,kte         )                    ! tile   dims
!       write(6,*) 'return from nudob: IVAR=2, J = ',j
     ENDIF
  ENDDO
!
! --- END OF 4DDA
!
  RETURN
#endif
  END SUBROUTINE fddaobs_driver

END MODULE module_fddaobs_driver

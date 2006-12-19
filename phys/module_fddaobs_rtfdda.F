!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_fddaobs_rtfdda

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

!------------------------------------------------------------------------------
  SUBROUTINE fddaobs_init(obs_nudge_opt, maxdom, inest, parid,         &
                          dx_coarse, restart, obs_twindo, itimestep,   &
                          e_sn, s_sn_cg, e_sn_cg, s_we_cg, e_we_cg,    &
#if ( EM_CORE == 1 )
                          fdob,                                        &
#endif
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     
!-----------------------------------------------------------------------
!  This routine does initialization for real time fdda obs-nudging.
!
!-----------------------------------------------------------------------
  USE module_domain
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: obs_nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  REAL    ,intent(in)  :: dx_coarse    ! coarse-domain grid cell-size (km)
  LOGICAL, intent(in)  :: restart
  REAL, intent(inout)  :: obs_twindo
  INTEGER, intent(in)  :: itimestep
  INTEGER, intent(in)  :: e_sn         ! ending   south-north grid index
  INTEGER, intent(in)  :: s_sn_cg      ! starting south-north coarse-grid index
  INTEGER, intent(in)  :: e_sn_cg      ! ending   south-north coarse-grid index
  INTEGER, intent(in)  :: s_we_cg      ! starting west-east   coarse-grid index
  INTEGER, intent(in)  :: e_we_cg      ! ending   west-east   coarse-grid index
#if ( EM_CORE == 1 ) 
  TYPE(fdob_type), intent(inout)  :: fdob
#endif

! Local variables
  logical            :: nudge_flag      ! nudging flag for this nest 
  integer            :: ktau            ! current timestep
  integer            :: nest            ! loop counter
  integer            :: idom            ! domain id
  integer            :: parent          ! parent domain

#if ( EM_CORE == 1 ) 
! This routine should only be called once. This is a check to make
! certain that initialization only happens once.
  if (fdob%domain_init .ne. 1) then
!   Obs-nudging will be initialized on this call
    fdob%domain_init = 1
  else
!   Obs-nudging has already been initialized, so return
    return
  endif

! Set flag for nudging on pressure (not sigma) surfaces
  fdob%iwtsig = 0

! Set ending nudging date (used with dynamic ramp-down) to zero.
  fdob%datend = 0.

! Convert twindo from minutes to hours.
  obs_twindo = obs_twindo / 60.

! Initialize flags.

  fdob%domain_tot=0
  do nest=1,maxdom
    fdob%domain_tot = fdob%domain_tot + obs_nudge_opt(nest)
  end do

! Set parameters.

  fdob%pfree = 50.0
  fdob%rinfmn = 1.0
  fdob%rinfmx = 2.0
  fdob%dpsmx = 7.5
  fdob%dcon = 1.0/fdob%dpsmx
  fdob%xn = 0.7155668                     ! cone factor

  fdob%ds_cg = dx_coarse / 1000.          ! coarse gridsize (km)
  fdob%sn_maxcg = e_sn_cg - s_sn_cg + 1   ! coarse domain grid dimension in N-S 
  fdob%we_maxcg = e_we_cg - s_we_cg + 1   ! coarse domain grid dimension in W-E
  fdob%sn_end = e_sn - 1                  ! ending S-N grid coordinate

! Calculate the nest levels, levidn. Note that each nest
! must know the nest levels levidn(maxdom) of each domain.
  do nest=1,maxdom

! Initialize nest level for each domain.
    if (nest .eq. 1) then
      fdob%levidn(nest) = 0  ! Mother domain has nest level 0
    else
      fdob%levidn(nest) = 1  ! All other domains start with 1
    endif
    idom = nest
100 parent = parid(idom)      ! Go up the parent tree
      if (parent .gt. 1) then   ! If not up to mother domain
        fdob%levidn(nest) = fdob%levidn(nest) + 1
        idom = parid(parent)
        goto 100
      endif
  enddo

! Check to see if the nudging flag has been set. If not,
! simply RETURN.
  nudge_flag = (obs_nudge_opt(inest) .eq. 1)
  if (.not. nudge_flag) return

  ktau  = itimestep
  if(restart) then
    fdob%ktaur = ktau
  else
    fdob%ktaur = 0 
  endif

  RETURN
#endif
  END SUBROUTINE fddaobs_init

#if ( EM_CORE == 1 )
!-----------------------------------------------------------------------
SUBROUTINE errob(inest, ub, vb, tb, t0, qvb, pbase, pp, rovcp,  &
                 uratx, vratx, tratx, nndgv,                    & 
                 nerrf, niobf, maxdom, levidn, parid, nstat,    &
                 iswind,                                        &
                 istemp, ismois, ispstr, rio, rjo, rko, varobs, &
                 errf, i_parent_start, j_parent_start,          &
                 ktau, iratio, npfi, iprt,                      &
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte  )

!-----------------------------------------------------------------------
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  USE module_dm, ONLY : get_full_obs_vector
#endif

!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PURPOSE: THIS SUBROUTINE CALCULATES THE DIFFERENCE BETWEEN THE
!     OBSERVED VALUES AND THE FORECAST VALUES AT THE OBSERVATION
!     POINTS.  THE OBSERVED VALUES CLOSEST TO THE CURRENT
!     FORECAST TIME (XTIME) WERE DETERMINED IN SUBROUTINE
!     IN4DOB AND STORED IN ARRAY VAROBS.  THE DIFFERENCES
!     CALCULATED BY SUBROUTINE ERROB WILL BE STORED IN ARRAY
!     ERRF FOR THE NSTA OBSERVATION LOCATIONS.  MISSING
!     OBSERVATIONS ARE DENOTED BY THE DUMMY VALUE 99999.
!
!     HISTORY: Original author: MM5 version???
!              02/04/2004 - Creation of WRF version.           Al Bourgeois
!              08/28/2006 - Conversion from F77 to F90         Al Bourgeois
!------------------------------------------------------------------------------

! THE STORAGE ORDER IN VAROBS AND ERRF IS AS FOLLOWS:
!        IVAR                VARIABLE TYPE(TAU-1)
!        ----                --------------------
!         1                    U error
!         2                    V error
!         3                    T error
!         4                    Q error
!         5                    Surface press error at T points (not used)
!         6                    Model surface press at T-points
!         7                    Model surface press at U-points
!         8                    Model surface press at V-points
!         9                    RKO at U-points

!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: inest                        ! Domain index.
  INTEGER, INTENT(IN)  :: nndgv                        ! Number of nudge variables.
  INTEGER, INTENT(IN)  :: nerrf                        ! Number of error fields.
  INTEGER, INTENT(IN)  :: niobf                        ! Number of observations.
  INTEGER, INTENT(IN)  :: maxdom                       ! Maximum number of domains.
  INTEGER, INTENT(IN)  :: levidn(maxdom)               ! Level of nest.
  INTEGER, INTENT(IN)  :: parid(maxdom)                ! Id of parent grid.
  INTEGER, INTENT(IN)  :: i_parent_start(maxdom)       ! Start i index in parent domain.
  INTEGER, INTENT(IN)  :: j_parent_start(maxdom)       ! Start j index in parent domain.
  INTEGER, INTENT(IN)  :: ktau
  INTEGER, INTENT(IN)  :: iratio                       ! Nest to parent gridsize ratio.
  INTEGER, INTENT(IN)  :: npfi                         ! Coarse-grid diagnostics freq.
  LOGICAL, INTENT(IN)  :: iprt                         ! Print flag
  INTEGER, INTENT(IN)  :: nstat
  INTEGER, intent(in)  :: iswind
  INTEGER, intent(in)  :: istemp
  INTEGER, intent(in)  :: ismois
  INTEGER, intent(in)  :: ispstr
  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: t0
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme ) ! Press. perturbation (Pa)
  REAL,   INTENT(IN)  :: rovcp
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme ) ! U to U10 ratio on mass points.
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme ) ! V to V10 ratio on mass points.
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme ) ! T to TH2 ratio on mass points.
  REAL,   INTENT(IN) :: rio(niobf)                ! West-east coordinate.
  REAL,   INTENT(IN) :: rjo(niobf)                ! South-north coordinate.
  REAL,   INTENT(INOUT) :: rko(niobf)
  REAL,   INTENT(INOUT) :: varobs(nndgv, niobf)
  REAL,   INTENT(INOUT) :: errf(nerrf, niobf)

! Local variables
  INTEGER :: iobmg(niobf)   ! Obs i-coord on mass grid
  INTEGER :: jobmg(niobf)   ! Obs j-coord on mass grid
  INTEGER :: ia(niobf)
  INTEGER :: ib(niobf)
  INTEGER :: ic(niobf)
  REAL :: pbbo(kds:kde)    ! column base pressure (cb) at obs loc.
  REAL :: ppbo(kds:kde)    ! column pressure perturbation (cb) at obs loc.

  REAL :: ra(niobf)
  REAL :: rb(niobf)
  REAL :: rc(niobf)
  REAL :: dxobmg(niobf)     ! Interp. fraction (x dir) referenced to mass-grid
  REAL :: dyobmg(niobf)     ! Interp. fraction (y dir) referenced to mass-grid
  INTEGER MM(MAXDOM)
  INTEGER NNL
  real :: uratio( ims:ime, jms:jme )   ! U to U10 ratio on momentum points.
  real :: vratio( ims:ime, jms:jme )   ! V to V10 ratio on momentum points.
  real :: pug1,pug2,pvg1,pvg2

! Define staggers for U, V, and T grids, referenced from non-staggered grid.
  real, parameter :: gridx_t = 0.5     ! Mass-point x stagger
  real, parameter :: gridy_t = 0.5     ! Mass-point y stagger
  real, parameter :: gridx_u = 0.0     ! U-point x stagger
  real, parameter :: gridy_u = 0.5     ! U-point y stagger
  real, parameter :: gridx_v = 0.5     ! V-point x stagger
  real, parameter :: gridy_v = 0.0     ! V-point y stagger

  real :: dummy = 99999.

  real :: pbhi, pphi
  real :: press,ttemp                  !ajb scratch variables
! real model_temp,pot_temp             !ajb scratch variables

!***  DECLARATIONS FOR IMPLICIT NONE
  integer nsta,ivar,n,ityp
  integer iob,job,kob,iob_ms,job_ms
  integer k,kbot,nml,nlb,nle
  integer iobm,jobm,iobp,jobp,kobp,inpf,i,j
  integer i_start,i_end,j_start,j_end    ! loop ranges for uratio,vratio calc.
  integer k_start,k_end

  real gridx,gridy,dxob,dyob,dzob,dxob_ms,dyob_ms
  real pob
  real grfacx,grfacy,uratiob,vratiob,tratiob,tratxob,fnpf
  real stagx                       ! For x correction to mass-point stagger
  real stagy                       ! For y correction to mass-point stagger

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  LOGICAL MP_LOCAL_DUMMASK(NIOBF)  ! Mask for work to be done on this processor
  LOGICAL MP_LOCAL_UOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_VOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_COBMASK(NIOBF)  ! Cross-point mask
#endif
! LOGICAL, EXTERNAL :: TILE_MASK

  NSTA=NSTAT

! FIRST, DETERMINE THE GRID TYPE CORRECTION FOR U-momentum, V-momentum,
! AND MASS POINTS, AND WHEN INEST=2, CONVERT THE STORED COARSE MESH INDICES
! TO THE FINE MESH INDEX EQUIVALENTS

! ITYP=1 FOR U-POINTS, ITYP=2 for V-POINTS, and ITYP=3 FOR MASS POINTS

  if (iprt) then
    write(6,'(a,i5,a,i2,a,i5,a)') '++++++CALL ERROB AT KTAU = ', &
            KTAU,' AND INEST = ',INEST,':  NSTA = ',NSTA,' ++++++'
  endif

  ERRF = 0.0    ! Zero out errf array

! Set up loop bounds for this grid's boundary conditions
  i_start = max( its-1,ids )
  i_end   = min( ite+1,ide-1 )
  j_start = max( jts-1,jds )
  j_end   = min( jte+1,jde-1 )
  k_start = kts
  k_end = min( kte, kde-1 )

  DO ITYP=1,3   ! Big loop: ityp=1 for U, ityp=2 for V, ityp=3 for T,Q,SP 

!   Set grid stagger
    IF(ITYP.EQ.1) THEN        ! U-POINT CASE
       GRIDX = gridx_u
       GRIDY = gridy_u
    ELSE IF(ITYP.EQ.2) THEN   ! V-POINT CASE
       GRIDX = gridx_v
       GRIDY = gridy_v
    ELSE                      ! MASS-POINT CASE
       GRIDX = gridx_t
       GRIDY = gridy_t
    ENDIF

!   Compute URATIO and VRATIO fields on momentum (u,v) points.
    IF(ityp.eq.1)THEN
      call upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme, uratx, uratio)
    ELSE IF (ityp.eq.2) THEN
      call vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme, vratx, vratio)
    ENDIF

    IF(INEST.EQ.1) THEN       ! COARSE MESH CASE...
      DO N=1,NSTA
        RA(N)=RIO(N)-GRIDX
        RB(N)=RJO(N)-GRIDY
        IA(N)=RA(N)
        IB(N)=RB(N)
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
        DXOB=RA(N)-FLOAT(IA(N))
        DYOB=RB(N)-FLOAT(IB(N))

!       Save mass-point arrays for computing rko for all var types
        if(ityp.eq.1) then
          iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
          jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
          dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
          dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
        endif
        iob_ms = iobmg(n)
        job_ms = jobmg(n)
        dxob_ms = dxobmg(n)
        dyob_ms = dyobmg(n)


!if(n.eq.1 .and. iprt) then
!        write(6,*) 'ERROB - COARSE MESH:'
!        write(6,'(a,i1,a,i1,4(a,f5.2),2(a,i3),2(a,f6.3))') 'OBS= ',n,  &
!                   '  ityp= ',ityp,                                    &
!                   '  ra= ',ra(n),'  rb= ',rb(n),                      &
!                   '  rio= ',rio(n),'  rjo= ',rjo(n),                  &
!                   '  iob= ',iob,'  job= ',job,                        &
!                   '  dxob= ',dxob,'  dyob= ',dyob
!        write(6,'(a,i3,a,i3,a,f5.2,a,f5.2)')                           &
!                   '  iob_ms= ',iob_ms,'  job_ms= ',job_ms,            &
!                   '  dxob_ms= ',dxob_ms,'  dyob_ms= ',dyob_ms
!endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for obs to be handled by this processor
        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
  
        IF ( MP_LOCAL_DUMMASK(N) ) THEN
#endif

!         Interpolate pressure to obs location column and convert from Pa to cb.

          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )  
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )  

!     write(6,'(a,i2,2(a,f9.3)') ' k= ',k,' pbbo= ',pbbo(k),' ppbo= ',ppbo(k)
          enddo

!ajb      20040119: Note based on bugfix for dot/cross points split across processors,
!ajb                which was actually a serial code fix: The ityp=2 (v-points) and
!ajb                itype=3 (mass-points) cases should not use the ityp=1 (u-points)
!ajb                case rko! This is necessary for bit-for-bit reproducability
!ajb                with the parallel run.   (coarse mesh)


          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 199
                endif
              enddo
 199          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )

              rko(n)=max(rko(n),1.0)
            endif
          endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        ENDIF       !end IF( MP_LOCAL_DUMMASK(N) )                                 !ajb
#endif

        RC(N)=RKO(N)

      ENDDO      ! END COARSE MESH LOOP OVER NSTA 

    ELSE       ! FINE MESH CASE
            
!     CONVERT (I,J,K) OF OBSERVATIONS TO THE EQUIVALENT FINE MESH VALUES.
      DO N=1,NSTA

!       COMPUTE THE OBS LOCATION WITH RESPECT TO THIS MESH (INEST)...
        NML=INEST
        MM(LEVIDN(INEST)+1)=INEST
!       WORKING TOWARD COARSER MESHES, DETERMINE THE HIERARCHY OF MOTHER
!       MESHES WITH RESPECT TO EACH MOTHER MESH STARTING AT MESH "IN"...
!       THAT IS, DETERMINE ITS MOTHER, GRANDMOTHER, GREAT-GRANDMOTHER, ETC.
!       OUT TO THE COARSE GRID MESH (INEST=1).
!       LEVIDN HOLDS THE NEST LEVEL AND PARID HOLDS THE MOTHER MESH FOR EACH
!       GRID (E.G., FOR 3 MESHES AND 2 NEST LEVELS, IN=1 IS THE COARSE GRID
!       MESH, IN=2 HAS LEVIDN(2)=1 AND PARID(2)=1, AND IN=3 HAS LEVIDN(3)=2
!       AND PARID(3)=2...)
        DO NNL=LEVIDN(INEST),1,-1
          MM(NNL)=PARID(NML)
          NML=MM(NNL)
        ENDDO

!       NOTE: MM(1) MUST BE THE COARSE GRID MESH (INEST=0)
        IF(MM(1).NE.1) then 
            if(iprt) write(6,*) 'stopping in errob: inest = ',inest
            STOP 21
        ENDIF

        RA(N)=RIO(N)
        RB(N)=RJO(N)
        DO NNL=1,LEVIDN(INEST)
          GRFACX=0.
          GRFACY=0.
!         COMPUTE THE OBS LOCATION WITH RESPECT TO THE INNER GRID IN NON-
!         STAGGERED SPACE (GRID=0.).  WHEN WE REACH MESH INEST, THEN
!         APPLY THE APPRPRIATE STAGGER, DEPENDING ON THE VARIABLE...
          IF(NNL.EQ.LEVIDN(INEST)) THEN
            GRFACX=GRIDX
            GRFACY=GRIDY
          ENDIF
  
          RA(N)=(RA(N)-FLOAT(i_parent_start(MM(NNL+1))))*  &
                       FLOAT(IRATIO)+1.-GRFACX
          RB(N)=(RB(N)-FLOAT(j_parent_start(MM(NNL+1))))*  &
                       FLOAT(IRATIO)+1.-GRFACY

          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))

!         Save mass-point arrays for computing rko for all var types
          if(ityp.eq.1) then
            stagx = grfacx - gridx_t     !Correct x stagger to mass-point
            stagy = grfacy - gridy_t     !Correct y stagger to mass-point
            iobmg(n) = MIN0(MAX0(1,int(RA(n)+stagx)),ide-1)
            jobmg(n) = MIN0(MAX0(1,int(RB(n)+stagy)),jde-1)
            dxobmg(n) = RA(N)+stagx-FLOAT(int(RA(N)+stagx))
            dyobmg(n) = RB(N)+stagy-FLOAT(int(RB(N)+stagy))
          endif 
          iob_ms = iobmg(n)
          job_ms = jobmg(n)
          dxob_ms = dxobmg(n)
          dyob_ms = dyobmg(n)

!if(n.eq.1) then
!        write(6,*) 'ERROB - FINE MESH:'
!        write(6,*) 'RA = ',ra(n),' RB = ',rb(n)
!        write(6,'(a,i1,a,i1,4(a,f5.2),2(a,i3),2(a,f6.3))') 'OBS= ',n,  &
!                   '  ityp= ',ityp,                                    &
!                   '  ra= ',ra(n),'  rb= ',rb(n),                      &
!                   '  rio= ',rio(n),'  rjo= ',rjo(n),                  &
!                   '  iob= ',iob,'  job= ',job,                        &
!                   '  dxob= ',dxob,'  dyob= ',dyob
!        write(6,'(a,i3,a,i3,a,f5.2,a,f5.2)')                           &
!                   '  iob_ms= ',iob_ms,'  job_ms= ',job_ms,            &
!                   '  dxob_ms= ',dxob_ms,'  dyob_ms= ',dyob_ms
!endif

        ENDDO    ! end do nnl=1,levidn(inest)

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for obs to be handled by this processor
        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

        IF ( MP_LOCAL_DUMMASK(N) ) THEN
#endif

!         Interpolate pressure to obs location column and convert from Pa to cb.

          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )

!     write(6,'(a,i2,2(a,f9.3)') ' k= ',k,' pbbo= ',pbbo(k),' ppbo= ',ppbo(k)
          enddo

!ajb      20040119: Note based on bugfix for dot/cross points split across processors,
!ajb                which was actually a serial code fix: The ityp=2 (v-points) and
!ajb                itype=3 (mass-points) cases should not use the ityp=1 (u-points)
!ajb                case) rko! This is necessary for bit-for-bit reproducability
!ajb                with parallel run.   (fine mesh)

          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 198
                endif
              enddo
 198          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )
              rko(n)=max(rko(n),1.0)
            endif
          endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        ENDIF       !end IF( MP_LOCAL_DUMMASK(N) )                                 !ajb
#endif

        RC(N)=RKO(N)

      ENDDO      ! END FINE MESH LOOP OVER NSTA
    
    ENDIF      ! end if(inest.eq.1)

!   INITIALIZE THE ARRAY OF DIFFERENCES BETWEEN THE OBSERVATIONS
!   AND THE LOCAL FORECAST VALUES TO ZERO.  FOR THE FINE MESH
!   ONLY, SET THE DIFFERENCE TO A LARGE DUMMY VALUE IF THE
!   OBSERVATION IS OUTSIDE THE FINE MESH DOMAIN.

!   SET DIFFERENCE VALUE TO A DUMMY VALUE FOR OBS POINTS OUTSIDE
!   CURRENT DOMAIN
    IF(ITYP.EQ.1) THEN
      NLB=1
      NLE=1
    ELSE IF(ITYP.EQ.2) THEN
      NLB=2
      NLE=2
    ELSE
      NLB=3
      NLE=5
    ENDIF
    DO IVAR=NLB,NLE
      DO N=1,NSTA
        IF((RA(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((RB(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(ide)-2.0*GRIDX-RA(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(jde)-2.0*GRIDY-RB(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        if(rc(n).lt.1.)errf(ivar,n)=errf(ivar,n)+dummy
      ENDDO
    ENDDO

!   NOW FIND THE EXACT OFFSET OF EACH OBSERVATION FROM THE
!   GRID POINT TOWARD THE LOWER LEFT
    DO N=1,NSTA
        IA(N)=RA(N)
        IB(N)=RB(N)
        IC(N)=RC(N)
    ENDDO
    DO N=1,NSTA
        RA(N)=RA(N)-FLOAT(IA(N))
        RB(N)=RB(N)-FLOAT(IB(N))
        RC(N)=RC(N)-FLOAT(IC(N))
    ENDDO
!   PERFORM A TRILINEAR EIGHT-POINT (3-D) INTERPOLATION
!   TO FIND THE FORECAST VALUE AT THE EXACT OBSERVATION
!   POINTS FOR U, V, T, AND Q.

!   Compute local masks for dot and cross points.
    if(ityp.eq.1) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for U-momemtum points to be handled by this processor
        MP_LOCAL_UOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif
    if(ityp.eq.2) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for V-momentum points to be handled by this processor
        MP_LOCAL_VOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif
    if(ityp.eq.3) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for cross (mass) points to be handled by this processor
        MP_LOCAL_COBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif

!**********************************************************
!   PROCESS U VARIABLE (IVAR=1)
!**********************************************************
    IF(ITYP.EQ.1) THEN
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      DO N=1,NSTA
        IF(MP_LOCAL_UOBMASK(N)) THEN
          ERRF(9,N)=rko(n)       !RKO is needed by neighboring processors     !ajb
        ENDIF
      ENDDO
#endif
      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_UOBMASK(N))THEN     ! Do if obs on this processor
#endif
            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PUG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
            PUG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              uratiob=((1.-DXOB)*((1.-DYOB)*uratio(IOB,JOB)+     &
                    DYOB*uratio(IOBP,JOB)                        &
                  )+DXOB*((1.-DYOB)*uratio(IOB,JOBP)+            &
                  DYOB*uratio(IOBP,JOBP)))
            else
              uratiob=1.
            endif
!YLIU       Some PBL scheme do not define the vratio/uratio
            if(abs(uratiob).lt.1.0e-3) then
              uratiob=1.
            endif

!           INITIAL ERRF(IVAR,N) IS ZERO FOR OBSERVATIONS POINTS
!           WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!           OUTSIDE THE CURRENT DOMAIN
  
!           U COMPONENT WIND ERROR
            ERRF(1,N)=ERRF(1,N)+uratiob*VAROBS(1,N)-((1.-DZOB)*        &
                      ((1.-DyOB)*((1.-                                 &
                      DxOB)*UB(IOB,KOB,JOB)+DxOB*UB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*UB(IOB,KOB,JOB+1)+DxOB*        &
                      UB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *UB(IOB,KOBP,JOB)+DxOB*UB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*UB(IOB,KOBP,JOB+1)+DxOB*         &
                      UB(IOB+1,KOBP,JOB+1))))

!      if(n.le.10) then
!        write(6,*)
!        write(6,'(a,i3,i3,i3,a,i3,a,i2)') 'ERRF1 at ',iob,job,kob,   &
!                                     ' N = ',n,' inest = ',inest
!        write(6,*) 'VAROBS(1,N) = ',varobs(1,n)
!        write(6,*) 'VAROBS(5,N) = ',varobs(5,n)
!        write(6,*) 'UB(IOB,KOB,JOB) = ',UB(IOB,KOB,JOB)
!        write(6,*) 'UB(IOB+1,KOB,JOB) = ',UB(IOB+1,KOB,JOB)
!        write(6,*) 'UB(IOB,KOB,JOB+1) = ',UB(IOB,KOB,JOB+1)
!        write(6,*) 'UB(IOB+1,KOB,JOB+1) = ',UB(IOB+1,KOB,JOB+1)
!        write(6,*) 'UB(IOB,KOBP,JOB) = ',UB(IOB,KOBP,JOB)
!        write(6,*) 'UB(IOB+1,KOBP,JOB) = ',UB(IOB+1,KOBP,JOB)
!        write(6,*) 'UB(IOB,KOBP,JOB+1) = ',UB(IOB,KOBP,JOB+1)
!        write(6,*) 'UB(IOB+1,KOBP,JOB+1) = ',UB(IOB+1,KOBP,JOB+1)
!        write(6,*) 'uratiob = ',uratiob
!        write(6,*) 'DXOB,DYOB,DZOB = ',DXOB,DYOB,DZOB
!        write(6,*) 'ERRF(1,N) = ',errf(1,n)
!        write(6,*)
!      endif


!           Store model surface pressure (not the error!) at U point.
            ERRF(7,N)=.001*( (1.-DXOB)*PUG1 + DXOB*PUG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_UOBMASK(N) )
#endif
        ENDDO    ! END U-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=1: PROCESS U

!**********************************************************
!   PROCESS V VARIABLE (IVAR=2)
!**********************************************************
    IF(ITYP.EQ.2) THEN

      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_VOBMASK(N))THEN     ! Do if obs on this processor
#endif
            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PVG1 = .5*( pbase(IOB,1,JOBM) + pbase(IOB,1,JOB) )
            PVG2 = .5*( pbase(IOB,1,JOB) + pbase(IOB,1,JOBP) )

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              vratiob=((1.-DXOB)*((1.-DYOB)*vratio(IOB,JOB)+     &
                    DYOB*vratio(IOBP,JOB)                        &
                  )+DXOB*((1.-DYOB)*vratio(IOB,JOBP)+            &
                  DYOB*vratio(IOBP,JOBP)))
            else
              vratiob=1.
            endif
!YLIU       Some PBL scheme do not define the vratio/uratio
            if(abs(vratiob).lt.1.0e-3) then
              vratiob=1.
            endif

!           INITIAL ERRF(IVAR,N) IS ZERO FOR OBSERVATIONS POINTS
!           WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!           OUTSIDE THE CURRENT DOMAIN
  
!           V COMPONENT WIND ERROR
            ERRF(2,N)=ERRF(2,N)+vratiob*VAROBS(2,N)-((1.-DZOB)*        &
                     ((1.-DyOB)*((1.-                                  &
                      DxOB)*VB(IOB,KOB,JOB)+DxOB*VB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*VB(IOB,KOB,JOB+1)+DxOB*        &
                      VB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *VB(IOB,KOBP,JOB)+DxOB*VB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*VB(IOB,KOBP,JOB+1)+DxOB*         &
                      VB(IOB+1,KOBP,JOB+1))))
  
!           Store model surface pressure (not the error!) at V point.
            ERRF(8,N)=.001*( (1.-DYOB)*PVG1 + DYOB*PVG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_VOBMASK(N) )
#endif
        ENDDO    ! END V-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=1: PROCESS V

!**********************************************************
!   PROCESS MASS-POINT VARIABLES IVAR=3 (T) AND IVAR=4 (QV)
!**********************************************************
    IF(ITYP.EQ.3) THEN

      IF(ISTEMP.EQ.1 .OR. ISMOIS.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_COBMASK(N)) THEN     ! Do if obs on this processor
#endif
            KOB=MIN0(k_end,IC(N))
            KOBP=MIN0(KOB+1,K_END)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              tratxob=((1.-DXOB)*((1.-DYOB)*tratx(IOB,JOB)+        &
                    DYOB*tratx(IOB+1,JOB)                          &
                  )+DXOB*((1.-DYOB)*tratx(IOB,JOB+1)+              &
                  DYOB*tratx(IOB+1,JOB+1)))
            else
              tratxob=1.
            endif

!yliu
            if(abs(tratxob) .lt. 1.0E-3) tratxob=1.

!ajb        testing only
            if(iprt .and. n.eq.81)  then
              write(6,*) 'POTENTIAL TEMP FOR N=81:'
              write(6,*)
              write(6,*) '            K      THETA       TEMPERATURE', &
                         '       PBASE'
              write(6,*)
              do k=k_end,1,-1
                press = pbase(iob,k,job)+pp(iob,k,job)
                ttemp = exp ( alog(300.+TB(IOB,k,JOB)) - &
                                           .2857143*alog(100000./press) ) 
                write(6,*) k,300.+TB(IOB,k,JOB),ttemp,pbase(iob,k,job)
              enddo
            endif
!ajb        end testing only

!           TEMPERATURE ERROR
!          if(n.le.10) then
!             write(6,*) 'before: errf(3,n) = ',errf(3,n)
!          endif
            ERRF(3,N)=ERRF(3,N)+tratxob*VAROBS(3,N)-((1.-DZOB)*     &
                      ((1.-DyOB)*((1.-                              &
                      DxOB)*(TB(IOB,KOB,JOB))+DxOB*                 &
                      (TB(IOB+1,KOB,JOB)))+DyOB*((1.-DxOB)*         &
                      (TB(IOB,KOB,JOB+1))+DxOB*                     &
                      (TB(IOB+1,KOB,JOB+1))))+DZOB*((1.-            &
                      DyOB)*((1.-DxOB)*(TB(IOB,KOBP,JOB))+DxOB*     &
                      (TB(IOB+1,KOBP,JOB)))+DyOB*((1.-DxOB)*        &
                      (TB(IOB,KOBP,JOB+1))+DxOB*                    &
                      (TB(IOB+1,KOBP,JOB+1)))))

!       if(n.le.10) then
!         write(6,*)
!         write(6,'(a,i3,i3,i3,a,i3,a,i2)') 'ERRF3 at ',iob,job,kob,   &
!                                      ' N = ',n,' inest = ',inest
!         write(6,*) 'VAROBS(3,N) = ',varobs(3,n)
!         write(6,*) 'VAROBS(5,N) = ',varobs(5,n)
!         write(6,*) 'TB(IOB,KOB,JOB) = ',TB(IOB,KOB,JOB)
!         write(6,*) 'TB(IOB+1,KOB,JOB) = ',TB(IOB+1,KOB,JOB)
!         write(6,*) 'TB(IOB,KOB,JOB+1) = ',TB(IOB,KOB,JOB+1)
!         write(6,*) 'TB(IOB+1,KOB,JOB+1) = ',TB(IOB+1,KOB,JOB+1)
!         write(6,*) 'TB(IOB,KOBP,JOB) = ',TB(IOB,KOBP,JOB)
!         write(6,*) 'TB(IOB+1,KOBP,JOB) = ',TB(IOB+1,KOBP,JOB)
!         write(6,*) 'TB(IOB,KOBP,JOB+1) = ',TB(IOB,KOBP,JOB+1)
!         write(6,*) 'TB(IOB+1,KOBP,JOB+1) = ',TB(IOB+1,KOBP,JOB+1)
!         write(6,*) 'tratxob = ',tratxob
!         write(6,*) 'DXOB,DYOB,DZOB = ',DXOB,DYOB,DZOB
!         write(6,*) 'ERRF(3,N) = ',errf(3,n)
!         write(6,*)
!       endif


!           MOISTURE ERROR
            ERRF(4,N)=ERRF(4,N)+VAROBS(4,N)-((1.-DZOB)*((1.-DyOB)*((1.- &
                      DxOB)*QVB(IOB,KOB,JOB)+DxOB*                      &
                      QVB(IOB+1,KOB,JOB))+DyOB*((1.-DxOB)*              &
                      QVB(IOB,KOB,JOB+1)+DxOB*                          &
                      QVB(IOB+1,KOB,JOB+1)))+DZOB*((1.-                 &
                      DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)+DxOB           &
                      *QVB(IOB+1,KOBP,JOB))+DyOB*((1.-DxOB              &
                      )*QVB(IOB,KOBP,JOB+1)+DxOB*                       &
                      QVB(IOB+1,KOBP,JOB+1))))

!           Store model surface pressure (not the error!) at T-point
            ERRF(6,N)= .001*                                            &
                      ((1.-DyOB)*((1.-DxOB)*pbase(IOB,1,JOB)+DxOB*      &
                      pbase(IOB+1,1,JOB))+DyOB*((1.-DxOB)*              &
                      pbase(IOB,1,JOB+1)+DxOB*pbase(IOB+1,1,JOB+1) ))
 
      if(iprt .and. n.eq.81) then
       write(6,*) 'ERRF(6,81) calculation:'
       write(6,*) 'iob,job = ',iob,job
       write(6,*) 'pbase(iob,1,job) = ',pbase(iob,1,job)
       write(6,*) 'pbase(iob+1,1,job) = ',pbase(iob+1,1,job)
       write(6,*) 'pbase(iob,1,job+1) = ',pbase(iob,1,job+1)
       write(6,*) 'pbase(iob+1,1,job+1) = ',pbase(iob+1,1,job+1)
       write(6,*) 'ERRF(6,81) = ',errf(6,n)
!      call flush(6)
      endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif
        ENDDO     ! END T and QV LOOP OVER OBS

      ENDIF   !end if(istemp.eq.1 .or. ismois.eq.1)

!**********************************************************
!     PROCESS SURFACE PRESSURE CROSS-POINT FIELD, IVAR=5,
!     USING BILINEAR FOUR-POINT 2-D INTERPOLATION
!**********************************************************
      IF(ISPSTR.EQ.1) THEN 
        DO N=1,NSTA
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_COBMASK(N)) THEN    ! Do if obs on this processor
#endif
            DXOB=RA(N)
            DYOB=RB(N)
!ajb        fix this (put in correct pressure calc for IOB,JOB here)
            ERRF(5,N)=ERRF(5,N)+VAROBS(5,N)-((1.-DyOB)*((1.-DxOB)*      &
               pbase(IOB,1,JOB)+DxOB*pbase(IOB+1,1,JOB))+DyOB*          &
               ((1.-DxOB)*pbase(IOB,1,JOB+1)+DxOB*                      &
               pbase(IOB+1,1,JOB+1)))

      if(n.eq.81) then
       write(6,*) 'ERRF(5,81) calculation:'
       write(6,*) 'iob,job = ',iob,job
       write(6,*) 'pbase(iob,1,job) = ',pbase(iob,1,job)
       write(6,*) 'pbase(iob+1,1,job) = ',pbase(iob+1,1,job)
       write(6,*) 'pbase(iob,1,job+1) = ',pbase(iob,1,job+1)
       write(6,*) 'pbase(iob+1,1,job+1) = ',pbase(iob+1,1,job+1)
       write(6,*) 'errf(5,81) = ',errf(5,n)
!      call flush(6)
      endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif

        ENDDO

      ENDIF   ! end if(ispstr.eq.1)

    ENDIF   ! end if(ityp.eq.3)

  ENDDO   ! END BIG LOOP

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
! Gather the errf values calculated by the processors with the obs.
  CALL get_full_obs_vector(nsta, nerrf, niobf, mp_local_uobmask,     &
                           mp_local_vobmask, mp_local_cobmask, errf)
#endif

! DIFFERENCE BETWEEN OBS AND FCST IS COMPLETED
  IF(INEST.EQ.1)THEN
    INPF=NPFI
  ELSE
    FNPF=IRATIO**LEVIDN(INEST)
    INPF=FNPF*NPFI
  ENDIF
! Gross error check for temperature. Set all vars bad.
  do n=1,nsta
    if((abs(errf(3,n)).gt.20.).and.           &
           (errf(3,n).gt.-800000.))then

       errf(1,n)=-888888.
       errf(2,n)=-888888.
       errf(3,n)=-888888.
       errf(4,n)=-888888.
       varobs(1,n)=-888888.
       varobs(2,n)=-888888.
       varobs(3,n)=-888888.
       varobs(4,n)=-888888.
    endif
  enddo

! For printout
!  IF(MOD(KTAU,INPF).NE.0) THEN
!      RETURN
!  ENDIF

  RETURN
  END SUBROUTINE errob

  SUBROUTINE upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to U (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: ids         ! Starting i index for entire model domain
  INTEGER, INTENT(IN) :: ide         ! Ending   i index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch 
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch 
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch 
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch 
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on U points 

! Local variables
  integer :: i, j

! Do domain interior first
  do j = j_start, j_end
    do i = max(2,i_start), i_end
       arrout(i,j) = 0.5*(arrin(i,j)+arrin(i-1,j))
    enddo
  enddo

! Do west-east boundaries
  if(i_start .eq. ids) then
    do j = j_start, j_end
      arrout(i_start,j) = arrin(i_start,j)
    enddo
  endif
  if(i_end .eq. ide-1) then
    do j = j_start, j_end
      arrout(i_end+1,j) = arrin(i_end,j)
    enddo
  endif

  RETURN
  END SUBROUTINE upoint

  SUBROUTINE vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to V (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: jds         ! Starting j index for entire model domain
  INTEGER, INTENT(IN) :: jde         ! Ending   j index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch 
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch 
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch 
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch 
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on V points 

! Local variables
  integer :: i, j

! Do domain interior first
  do j = max(2,j_start), j_end
    do i = i_start, i_end
      arrout(i,j) = 0.5*(arrin(i,j)+arrin(i,j-1))
    enddo
  enddo

! Do south-north boundaries
  if(j_start .eq. jds) then
    do i = i_start, i_end
      arrout(i,j_start) = arrin(i,j_start)
    enddo
  endif
  if(j_end .eq. jde-1) then
    do i = i_start, i_end
      arrout(i,j_end+1) = arrin(i,j_end)
    enddo
  endif

  RETURN
  END SUBROUTINE vpoint

  LOGICAL FUNCTION TILE_MASK(iloc, jloc, its, ite, jts, jte)
!------------------------------------------------------------------------------
! PURPOSE: Check to see if an i, j grid coordinate is in the tile index range.
!      
! Returns: TRUE if the grid coordinate (ILOC,JLOC) is in the tile defined by
!                  tile-range indices (its,jts) and (ite,jte)
!          FALSE otherwise.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iloc
  INTEGER, INTENT(IN) :: jloc
  INTEGER, INTENT(IN) :: its
  INTEGER, INTENT(IN) :: ite
  INTEGER, INTENT(IN) :: jts
  INTEGER, INTENT(IN) :: jte

! Local variables
  LOGICAL :: retval

  TILE_MASK = (iloc .LE. ite .AND. iloc .GE. its .AND.    &
               jloc .LE. jte .AND. jloc .GE. jts )

  RETURN
  END FUNCTION TILE_MASK

!-----------------------------------------------------------------------
  SUBROUTINE nudob(j, ivar, aten, inest, ifrest, ktau, ktaur,         &
                       xtime, mu, msf, nndgv, nerrf, niobf, maxdom,   &
                       npfi, ionf, rinxy, twindo, levidn,             &
                       parid, nstat, i_parent_start, j_parent_start,  &
                       fdob, lev_in_ob, plfo, nlevs_ob,               &
                       iratio, dx, dtmin, rio, rjo, rko,              &
                       timeob, varobs, errf, pbase, ptop, pp,         &
                       iswind, istemp, ismois, giv, git, giq,         &
                       savwt, kpblt, nscan,                           &
                       iprt,                                          &
                       ids,ide, jds,jde, kds,kde,                     &  ! domain dims
                       ims,ime, jms,jme, kms,kme,                     &  ! memory dims
                       its,ite, jts,jte, kts,kte )                       ! tile   dims

!-----------------------------------------------------------------------
  USE module_model_constants
  USE module_domain
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PURPOSE: THIS SUBROUTINE GENERATES NUDGING TENDENCIES FOR THE J-TH
!     VERTICAL SLICE (I-K PLANE) FOR FOUR-DIMENSIONAL DATA
!     ASSIMILATION FROM INDIVIDUAL OBSERVATIONS.  THE NUDGING
!     TENDENCIES ARE FOUND FROM A ONE-PASS CALCULATION OF
!     WEIGHTING FACTORS SIMILAR TO THE BENJAMIN-SEAMAN OBJECTIVE
!     ANALYSIS.  THIS SUBROUTINE IS DESIGNED FOR RAPID EXECUTION
!     AND MINIMAL STORAGE REQUIREMENTS.  ALGORITHMS SHOULD BE
!     VECTORIZED WHEREVER POSSIBLE.
!
!     HISTORY: Original author: MM5 version???
!              02/04/2004 - Creation of WRF version.           Al Bourgeois
!              08/28/2006 - Conversion from F77 to F90         Al Bourgeois
!------------------------------------------------------------------------------
!
! NOTE: This routine was originally designed for MM5, which uses
!       a nonstandard (I,J) coordinate system. For WRF, I is the 
!       east-west running coordinate, and J is the south-north
!       running coordinate. So a "J-slab" here is west-east in
!       extent, not south-north as for MM5.      -ajb 06/10/2004
!
!     NET WEIGHTING (WT) OF THE DIFFERENCE BETWEEN THE OBSERVATIONS
!     AND LOCAL FORECAST VALUES IS BASED ON THE MULTIPLE OF THREE
!
!     NET WEIGHTING (WT) OF THE DIFFERENCE BETWEEN THE OBSERVATIONS
!     AND LOCAL FORECAST VALUES IS BASED ON THE MULTIPLE OF THREE
!     TYPES OF FACTORS:
!       1) TIME WEIGHTING - ONLY OBSERVATIONS WITHIN A SELECTED
!          TIME WINDOW (TWINDO) CENTERED AT THE CURRENT FORECAST
!          TIME (XTIME) ARE USED.  OBSERVATIONS CLOSEST TO
!          XTIME ARE TIME-WEIGHTED MOST HEAVILY (TIMEWT)
!       2) VERTICAL WEIGHTING - NON-ZERO WEIGHTS (WTSIG) ARE
!          CALCULATED WITHIN A VERTICAL REGION OF INFLUENCE
!          (RINSIG).
!       3) HORIZONTAL WEIGHTING - NON-ZERO WEIGHTS (WTIJ) ARE
!          CALCULATED WITHIN A RADIUS OF INFLUENCE (RINXY).  THE
!          VALUE OF RIN IS DEFINED IN KILOMETERS, AND CONVERTED
!          TO GRID LENGTHS FOR THE APPROPRIATE MESH SIZE.
!
!     THE FIVE FORECAST VARIABLES ARE PROCESSED BY CHANGING THE
!     VALUE OF IVAR AS FOLLOWS:
!             IVAR                     VARIABLE(TAU-1)
!             ----                     ---------------
!               1                             U
!               2                             V
!               3                             T
!               4                             QV
!               5                          PSB(CROSS)   REMOVED IN V3
!              (6)                           PSB(DOT)
!
!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, INTENT(IN)  :: j                          ! south-north running coordinate.
  INTEGER, INTENT(IN)  :: ivar
  INTEGER, INTENT(IN)  :: inest                      ! domain index
  LOGICAL, INTENT(IN)  :: ifrest
  INTEGER, INTENT(IN)  :: ktau
  INTEGER, INTENT(IN)  :: ktaur
  REAL, INTENT(IN)     :: xtime                      ! forecast time in minutes
  INTEGER, INTENT(IN)  :: nndgv                      ! number of nudge variables
  INTEGER, INTENT(IN)  :: nerrf                      ! number of error fields
  INTEGER, INTENT(IN)  :: niobf                      ! number of observations
  INTEGER, INTENT(IN)  :: maxdom                     ! maximum number of domains
  INTEGER, INTENT(IN)  :: npfi 
  INTEGER, INTENT(IN)  :: ionf
  REAL, INTENT(IN)     :: rinxy
  REAL, INTENT(IN)     :: twindo
  INTEGER, INTENT(IN)  :: levidn(maxdom)             ! level of nest
  INTEGER, INTENT(IN)  :: parid(maxdom)              ! parent domain id
  INTEGER, INTENT(IN)  :: nstat                      ! number of obs stations
  INTEGER, INTENT(IN)  :: i_parent_start(maxdom)     ! Start i index in parent domain.
  INTEGER, INTENT(IN)  :: j_parent_start(maxdom)     ! Start j index in parent domain.
  TYPE(fdob_type), intent(inout)  :: fdob
  REAL, INTENT(IN)     :: lev_in_ob(niobf)           ! Level in sounding-type obs.
  REAL, intent(IN)     :: plfo(niobf)
  REAL, INTENT(IN)     :: nlevs_ob(niobf)            ! Number of levels in sounding.
  INTEGER, INTENT(IN)  :: iratio                     ! Nest to parent gridsize ratio.
  REAL, INTENT(IN)     :: dx                         ! This domain grid cell-size (m)
  REAL, INTENT(IN)     :: dtmin
  REAL, INTENT(IN)     :: rio(niobf)
  REAL, INTENT(IN)     :: rjo(niobf)
  REAL, INTENT(INOUT)  :: rko(niobf)
  REAL, INTENT(IN)     :: timeob(niobf)
  REAL, INTENT(IN)     :: varobs(nndgv,niobf)
  REAL, INTENT(IN)     :: errf(nerrf, niobf)
  REAL, INTENT(IN)     :: pbase( ims:ime, kms:kme )  ! Base pressure.
  REAL, INTENT(IN)     :: ptop
  REAL, INTENT(IN)     :: pp( ims:ime, kms:kme ) ! Pressure perturbation (Pa)
  REAL, INTENT(IN)     :: mu(ims:ime)   ! Air mass on u, v, or mass-grid
  REAL, INTENT(IN)     :: msf(ims:ime)  ! Map scale (only used for vars u & v)
  INTEGER, intent(in)  :: iswind        ! Nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! Nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! Nudge flag for moisture
  REAL, intent(in)     :: giv           ! Coefficient for wind
  REAL, intent(in)     :: git           ! Coefficient for temperature
  REAL, intent(in)     :: giq           ! Coefficient for moisture
  REAL, INTENT(INOUT)  :: aten( ims:ime, kms:kme)
  REAL, INTENT(INOUT)  :: savwt( nndgv, ims:ime, kms:kme )
  INTEGER, INTENT(IN)  :: kpblt(its:ite)
  INTEGER, INTENT(IN)  :: nscan                      ! number of scans
  LOGICAL, INTENT(IN)  :: iprt                       ! print flag

! Local variables
  integer :: mm(maxdom)
  real :: ra(niobf)
  real :: rb(niobf)
  real :: psurf(niobf)
  real :: wtsig(kms:kme),wt(ims:ime,kms:kme),wt2err(ims:ime,kms:kme)
  real :: rscale(ims:ime)           ! For converting to rho-coupled units.
!      real :: tscale(ims:ime,kms:kme)   ! For converting to potential temp. units.
  real :: reserf(100)
  character*40 name
  character*3 chr_hr

!***  DECLARATIONS FOR IMPLICIT NONE
  integer :: i,k,iplo,icut,ipl,inpf,infr,jjjn
  integer :: igrid,n,nml,nnl,nsthis,nsmetar,nsspeci,nsship
  integer :: nssynop,nstemp,nspilot,nssatwnds,nssams,nsprofs
  integer :: maxi,mini,maxj,minj,nnn,nsndlev,njcsnd,kob
  integer :: komin,komax,nn,nhi,nlo,nnjc
  integer :: i_s,i_e
  integer :: istq
  real :: gfactor,rfactor,gridx,gridy,rindx,schnes,ris
  real :: grfacx,grfacy
  real :: fdtim,tw1,tw2,tconst,timewt,timewt2,ttim,dift,pob
  real :: ri,rj,rx,ry,rsq,wtij,pdfac,erfivr,dk,slope,rinfac
  real :: rinprs,pijk,pobhi,poblo,pdiffj,w2eowt,gitq

  real :: scratch

!      print *,'start nudob, nstat,j,ivar=',nstat,j,ivar
!      if(ivar.ne.4)return
!yliu start -- for multi-scans: NSCAN=0: original
!                               NSCAN=1: added a scan with a larger Ri and smaller G
!       if(NSCAN.ne.0 .and. NSCAN.ne.1)  stop
! ajb note: Will need to increase memory for SAVWT if NSCAN=1:
  if(NSCAN.ne.0) then
    IF (iprt) write(6,*) 'SAVWT must be resized for NSCAN=1'
    stop
  endif
  IPLO=0  + NSCAN*4
  GFACTOR=1. +  NSCAN*(-1. + 0.33333) 
  RFACTOR=1. +  NSCAN*(-1. + 3.0)
!yliu end
! jc

! return if too close to j boundary
  if(inest.eq.1.and.ivar.lt.3.and.(j.le.2.or.j.ge.jde-1)) then
!       write(6,*) '1 RETURN: IVAR = ',ivar,' J = ',j,
!    $             ' too close to boundary.'
    return
  endif
  if(inest.eq.1.and.ivar.ge.3.and.(j.le.2.or.j.ge.jde-2)) then
!       write(6,*) '2 RETURN: IVAR = ',ivar,' J = ',j,
!    $             ' too close to boundary.'
    return
  endif

! COMPUTE IPL WHICH REPRESENTS IVAR FOR EACH MESH IN SAVWT MODS
  ICUT=0
  IF(INEST.GT.1)ICUT=1
  i_s = max0(2+icut,its)
  i_e = min0(ide-1-icut,ite)

  IPL=IVAR    + IPLO     !yliu +IPLO

! DEFINE GRID-TYPE OFFSET FACTORS, IGRID AND GRID

  IF(INEST.EQ.1)THEN
    INPF=NPFI
    INFR=IONF
  ELSE
    IF(IRATIO.NE.3) THEN
      IF (iprt) THEN
        write(6,*) 'iratio = ',iratio
        write(6,*) 'stop 1 in nudob: iratio = ',iratio
      ENDIF
      STOP 1
    ENDIF
    INPF=(3**LEVIDN(INEST))*NPFI
    INFR=(3**LEVIDN(INEST))*IONF
  ENDIF
  GRIDX=0.0
  GRIDY=0.0
  IGRID=0
  IF(IVAR.GE.3)THEN
    GRIDX=0.5
    GRIDY=0.5
    IGRID=1
  ELSEIF(IVAR.eq.1) THEN
    GRIDY=0.5
    IGRID=1
  ELSEIF(IVAR.eq.2) THEN
    GRIDX=0.5
    IGRID=1
  ENDIF

! TRANSFORM THE HORIZONTAL RADIUS OF INFLUENCE, RINXY, FROM
! KILOMETERS TO GRID LENGTHS, RINDX

  RINDX=RINXY*1000./DX          * RFACTOR   !yliu *RFACTOR

! jc
! make horizontal radius vary per nest
!     rindx=rindx/float(inest)
! yliu test1 -- En 3, 4
!     rindx=rindx/float(3**(in-1))                                               !YLIU
! jc
! make horizontal radius vary per nest
!     schnes=1/float(inest)                                                      !JC
! yliu test1 -- En 3, 4                                                          !YLIU
  schnes=1/float(3**(inest-1))                                                   !JC
! reduce the Rinf in the nested grid proportionally
  rindx=rindx*schnes
! rinfmn =1., rinfmx=2., pfree=50 in param.F
! yliu test: for upper-air data, use larger influence radii
!            Essentially increase the slope -- the same radii
!            at 500mb and above as the coarse mesh and the
!            same small radii at sfc as that for sfc obs
  fdob%rinfmx=2. *1.0 /schnes                                                    !YLIU
!         rinfmx=1.2*1.0 /schnes                                                 !YLIU
! jc
  RIS=RINDX*RINDX
  IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 5
  IF(MOD(KTAU,INFR).NE.0)GOTO 126
5 CONTINUE
  IF (iprt) THEN
   IF(J.EQ.10) write(6,6) INEST,J,KTAU,XTIME,IVAR,IPL,rindx
  ENDIF
6 FORMAT(1X,'OBS NUDGING FOR IN,J,KTAU,XTIME,',                    &
            'IVAR,IPL: ',I2,1X,I2,1X,I5,1X,F8.2,1X,I2,1X,I2,       &
            ' rindx=',f4.1)

! SET RA AND RB
  IF(INEST.EQ.1) THEN

! SET RA AND RB FOR THE COARSE MESH...
    DO N=1,NSTAT
      RA(N)=RIO(N)-GRIDX
      RB(N)=RJO(N)-GRIDY
    ENDDO

  ELSE

! SET RA AND RB FOR THE FINE MESH CASE...
    DO N=1,NSTAT

! COMPUTE THE OBS LOCATION WITH RESPECT TO THIS MESH (INEST)...
      NML=INEST
      MM(LEVIDN(INEST)+1)=INEST
! WORKING TOWARD COARSER MESHES, DETERMINE THE HIERARCHY OF MOTHER
! MESHES WITH RESPECT TO EACH MOTHER MESH STARTING AT MESH "INEST"...
! THAT IS, DETERMINE ITS MOTHER, GRANDMOTHER, GREAT-GRANDMOTHER, ETC.
! OUT TO THE COARSE GRID MESH (INEST=1).
! LEVIDN HOLDS THE NEST LEVEL AND PARID HOLDS THE MOTHER MESH FOR EACH
! GRID (E.G., FOR 3 MESHES AND 2 NEST LEVELS, INEST=1 IS THE COARSE GRID
! MESH, INEST=2 HAS LEVIDN(2)=1 AND PARID(2)=1, AND INEST=3 HAS LEVIDN(3)=2
! AND PARID(3)=2...)
      DO NNL=LEVIDN(INEST),1,-1
        MM(NNL)=PARID(NML)
        NML=MM(NNL)
      ENDDO

! MM(1) MUST BE THE COARSE GRID MESH (INEST=0)

      IF(MM(1).NE.1) then
        IF (iprt) write(6,*) 'stop 21 in nudob: inest = ',inest
        STOP 21
      ENDIF
      RA(N)=RIO(N)
      RB(N)=RJO(N)
      DO NNL=1,LEVIDN(INEST)
        GRFACX=0.
        GRFACY=0.
! COMPUTE THE OBS LOCATION WITH RESPECT TO THE INNER GRID IN DOT-POINT
! SPACE (GRID=0.).  WHEN WE REACH MESH IN, THEN USE "GRID" TO GO TO
! CROSS OR DOT DEPENDING ON THE VARIABLE...
        IF(NNL.EQ.LEVIDN(INEST)) THEN
          GRFACX=GRIDX
          GRFACY=GRIDY
        ENDIF

        RA(N)=(RA(N)-FLOAT(i_parent_start(MM(NNL+1))))*       &
                     FLOAT(IRATIO)+1.-GRFACX
        RB(N)=(RB(N)-FLOAT(j_parent_start(MM(NNL+1))))*       &
                     FLOAT(IRATIO)+1.-GRFACY

      ENDDO

    ENDDO    ! END LOOP OVER OBS STATIONS FOR FINE MESH CASE

  ENDIF    ! END SECTION FOR SETTING RA AND RB


! OUTPUT OBS PER GRID EVERY HOUR
  if ( mod(xtime,60.).gt.56. .and. ivar.eq.3 .and. j.eq.10) then
    IF (iprt) print *,'outputting obs number on grid ',    &
                 inest,' at time=',xtime
    write(chr_hr(1:3),'(i3)')nint(xtime/60.)
    if(chr_hr(1:1).eq.' ')chr_hr(1:1)='0'
    if(chr_hr(2:2).eq.' ')chr_hr(2:2)='0'
    IF (iprt) print *,'chr_hr=',chr_hr(1:3),nint(xtime/60.)
    open(91,file=                                             &
        'obs_g'//char(inest+ichar('0'))//'_'//chr_hr(1:3),    &
        form='FORMATted',status='unknown')
    write(91,911)nstat
    write(6,911)nstat
911 FORMAT('total obs=',i8)
    nsthis=0
    nsmetar=0
    nsspeci=0
    nsship=0
    nssynop=0
    nstemp=0
    nspilot=0
    nssatwnds=0
    nssams=0
    nsprofs=0
!   print *,'ide,jde=',ide,jde
    do jjjn=1,nstat
! DETERMINE THE TIME-WEIGHT FACTOR FOR N
      FDTIM=XTIME-DTMIN
! CONVERT TWINDO AND TIMEOB FROM HOURS TO MINUTES:
      TW1=TWINDO/2.*60.
      TW2=TWINDO*60.
      TCONST=1./TW1
      TIMEWT2=0.0
      TTIM=TIMEOB(jjjn)*60.
!***********TTIM=TARGET TIME IN MINUTES
      DIFT=ABS(FDTIM-TTIM)
      IF(DIFT.LE.TW1)TIMEWT2=1.0

      IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
        IF(FDTIM.LT.TTIM)TIMEWT2=(FDTIM-(TTIM-TW2))*TCONST
        IF(FDTIM.GT.TTIM)TIMEWT2=((TTIM+TW2)-FDTIM)*TCONST
      ENDIF

!     print *,'timewt2=',timewt2,ttim,fdtim
      if (ra(jjjn).ge.1. .and. rb(jjjn).ge.1.                    &
      .and.ra(jjjn).le.real(ide) .and. rb(jjjn).le.real(jde)     &
      .and.timewt2.gt.0.) then
        if(lev_in_ob(jjjn).eq.1.)nsthis=nsthis+1
        if(plfo(jjjn).eq.1.)nsmetar=nsmetar+1
        if(plfo(jjjn).eq.2.)nsspeci=nsspeci+1
        if(plfo(jjjn).eq.3.)nsship=nsship+1
        if(plfo(jjjn).eq.4.)nssynop=nssynop+1
        if(plfo(jjjn).eq.5..and.lev_in_ob(jjjn).eq.1.) nstemp=nstemp+1
        if(plfo(jjjn).eq.6..and.lev_in_ob(jjjn).eq.1.) nspilot=nspilot+1
        if(plfo(jjjn).eq.7.)nssatwnds=nssatwnds+1
        if(plfo(jjjn).eq.8.)nssams=nssams+1
        if(plfo(jjjn).eq.9..and.lev_in_ob(jjjn).eq.1.) nsprofs=nsprofs+1
      endif
    enddo
    write(91,912)nsthis
    write(6,912)nsthis
912 FORMAT('total obs on this grid=',i8)
    write(91,921)nsmetar
    write(6,921)nsmetar
921 FORMAT('total metar obs on this grid=',i8)
    write(91,922)nsspeci
    write(6,922)nsspeci
922 FORMAT('total special obs on this grid=',i8)
    write(91,923)nsship
    write(6,923)nsship
923 FORMAT('total ship obs on this grid=',i8)
    write(91,924)nssynop
    write(6,924)nssynop
924 FORMAT('total synop obs on this grid=',i8)
    write(91,925)nstemp
    write(6,925)nstemp
925 FORMAT('total temp obs on this grid=',i8)
    write(91,926)nspilot
    write(6,926)nspilot
926 FORMAT('total pilot obs on this grid=',i8)
    write(91,927)nssatwnds
    write(6,927)nssatwnds
927 FORMAT('total sat-wind obs on this grid=',i8)
    write(91,928)nssams
    write(6,928)nssams
928 FORMAT('total sams obs on this grid=',i8)
    write(91,929)nsprofs
    write(6,929)nsprofs
929 FORMAT('total profiler obs on this grid=',i8)
    close(91)
  endif  ! END OUTPUT OBS PER GRID EVERY HOUR


! INITIALIZE WEIGHTING ARRAYS TO ZERO
  DO I=its,ite
    DO K=1,kte
      WT(I,K)=0.0
      WT2ERR(I,K)=0.0
    ENDDO
  ENDDO

! DO P* COMPUTATIONS ON DOT POINTS FOR IVAR.LT.3 (U AND V)
! AND CROSS POINTS FOR IVAR.GE.3 (T,Q,P*).
!
! COMPUTE P* AT OBS LOCATION (RA,RB).  DO THIS AS SEPARATE VECTOR LOOP H
! SO IT IS ALREADY AVAILABLE IN NSTAT LOOP 120 BELOW

! PSURF IS NOT AVAILABLE GLOBALLY, THEREFORE, THE BILINEAR INTERPOLATION
! AROUND THE OBS POINT IS DONE IN ERROB() AND STORED IN ERRF([678],N) FOR
! THE POINT (6=PRESS, 7=U-MOM, 8=V-MOM).
  DO N=1,NSTAT
    IF(IVAR.GE.3)THEN
      PSURF(N)=ERRF(6,N)
    ELSE
      IF(IVAR.EQ.1)THEN
        PSURF(N)=ERRF(7,N)        ! U-points
      ELSE
        PSURF(N)=ERRF(8,N)        ! V-points
      ENDIF
    ENDIF
  ENDDO

! DETERMINE THE LIMITS OF THE SEARCH REGION FOR THE CURRENT
! J-STRIP

  MAXJ=J+IFIX(RINDX*fdob%RINFMX+0.99)                                        !ajb
  MINJ=J-IFIX(RINDX*fdob%RINFMX+0.99)                                        !ajb

! jc comment out this? want to use obs beyond the domain?
!      MAXJ=MIN0(JL-IGRID,MAXJ)           !yliu
!      MINJ=MAX0(1,MINJ)                  !yliu

  n=1

!***********************************************************************
  DO nnn=1,NSTAT   ! BEGIN OUTER LOOP FOR THE NSTAT OBSERVATIONS
!***********************************************************************
! Soundings are consecutive obs, but they need to be treated as a single 
! entity. Thus change the looping to nnn, with n defined separately.


!yliu 
!  note for sfc data: nsndlev=1 and njcsnd=1
    nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1   

! yliu start -- set together with the other parts
! test: do the sounding levels as individual obs
!   nsndlev=1
! yliu end
    njcsnd=nsndlev
! set pob here, to be used later
    pob=varobs(5,n)
!     print *, "s-- n=,nsndlev",n,njcsnd,J, ipl
!     print *, "s--",ivar,(errf(ivar,i),i=n,n+njcsnd)
! CHECK TO SEE OF STATION N HAS DATA FOR VARIABLE IVAR
! AND IF IT IS SUFFICIENTLY CLOSE TO THE J STRIP.  THIS
! SHOULD ELIMINATE MOST STATIONS FROM FURTHER CONSIDER-
! ATION.

!yliu: Skip bad obs if it is sfc or single level sounding.
!yliu: Before this (020918), a snd will be skipped if its first 
!yliu        level has bad data- often true due to elevation

    IF( ABS(ERRF(IVAR,N)).GT.9.E4 .and. njcsnd.eq.1 ) THEN
!     print *, " bad obs skipped"

    ELSEIF( RB(N).LT.FLOAT(MINJ) .OR. RB(N).GT.FLOAT(MAXJ) ) THEN
!     print *, " skipped obs far away from this J-slice"

!----------------------------------------------------------------------
    ELSE    ! BEGIN SECTION FOR PROCESSING THE OBSERVATION
!----------------------------------------------------------------------

! DETERMINE THE TIME-WEIGHT FACTOR FOR N
      FDTIM=XTIME-DTMIN
! TWINDO IS IN MINUTES:
      TW1=TWINDO/2.*60.
      TW2=TWINDO*60.
      TCONST=1./TW1
      TIMEWT=0.0
      TTIM=TIMEOB(N)*60.
!***********TTIM=TARGET TIME IN MINUTES
      DIFT=ABS(FDTIM-TTIM)
      IF(DIFT.LE.TW1)TIMEWT=1.0
      IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
        IF(FDTIM.LT.TTIM)TIMEWT=(FDTIM-(TTIM-TW2))*TCONST
        IF(FDTIM.GT.TTIM)TIMEWT=((TTIM+TW2)-FDTIM)*TCONST
      ENDIF

! DETERMINE THE LIMITS OF APPLICATION OF THE OBS IN THE VERTICAL
! FOR THE VERTICAL WEIGHTING, WTSIG

! ASSIMILATE OBSERVATIONS ON PRESSURE LEVELS, EXCEPT FOR SURFACE
!ajb 20021210: (Bugfix) RKO is not available globally. It is computed in
!ajb ERROB() by the processor handling the obs point, and stored in ERRF(9,N).

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      rko(n) = errf(9,n)        !ajb 20021210
#endif
      KOB=nint(RKO(N))
      KOB=MIN0(kte,KOB)
      KOB=MAX0(1,KOB)

! ASSIMILATE SURFACE LAYER DATA ON SIGMA
      IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5) THEN
        DO K=1,kte
          WTSIG(K)=0.0
        ENDDO
! DEFINE WTSIG: (FOR SRP: SPREAD SURFACE DATA THROUGH LOWEST 200 M)
!       WTSIG(1)=1.0
!       WTSIG(2)=0.67
!       WTSIG(3)=0.33
!       KOMIN=3
!       KOMAX=1
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX (IN GRID LENGTHS).
! fix this because kpblt at 1 and il is 0
        MAXI=IFIX(RA(N)+0.99+RINDX)
        MAXI=MIN0(ide-1,MAXI)
        MINI=IFIX(RA(N)-RINDX-0.99)
        MINI=MAX0(2,MINI)
!yliu start
! use also obs outside of this domain  -- surface obs
!     if(RA(N).LT.0.-RINDX .or. RA(N).GT.float(IL+RINDX) .or.
!    &   RB(N).LT.0.-RINDX .or. RB(N).GT.float(JL+RINDX)) then
!        print *, " skipped obs far away from this domain"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this
!   domain. In later case, use BC column value to approximate the model value
!   at obs point -- ERRF need model field in errob.F !!
        if (  RA(N).GE.(0.-RINDX/3)                        &
        .and. RA(N).LE.float(ide)+RINDX/3                  &
        .and. RB(N).GE.(0.-RINDX/3)                        &
        .and. RB(N).LE.float(jde)+RINDX/3) then

! or use obs within this domain only
!     if(RA(N).LT.1 .or. RA(N).GT.float(IL) .or.
!    &   RB(N).LT.1 .or. RB(N).GT.float(JL)) then
!        print *, " skipped obs far outside of this domain"
!        if(j.eq.3 .and. ivar.eq.3) then
!           write(6,*) 'N = ',n,' exit 120 3'
!        endif
!yliu end
!
! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
          RJ=FLOAT(J)
          RX=RJ-RB(N)
! WEIGHTS FOR THE 3-D VARIABLES
          ERFIVR=ERRF(IVAR,N)
!
!JM I will be local, because it indexes into PDOC, WT, and others

!      if((ivar.eq.1 .or. ivar.eq.3) .and. n.le.200) then
!        write(6,'(a,i3,a,i3)')'SURF OBS NEAR: N = ',n,' nest = ',inest
!        write(6,'(a,f10.3,a,f10.3,a,i3,a,i3,a,i3,a,i2)')
!     $        ' RA =',RA(N),' RB =',RB(N),' J =',j,
!     $        ' MINI =',MINI,' MAXI =',MAXI,' NEST =',inest
!      endif

          DO I=max0(its,MINI),min0(ite,MAXI)

            RI=FLOAT(I)
            RY=RI-RA(N)
            RIS=RINDX*RINDX
            RSQ=RX*RX+RY*RY
!           DPRIM=SQRT(RSQ)
! THIS FUNCTION DECREASES WTIJ AS PSFC CHANGES WITHIN SEARCH RADIUS
!           D=DPRIM+RINDX*DCON*ABS(PSBO(N)-PDOC(I,J))
!           DSQ=D*D
!           WTIJ=(RIS-DSQ)/(RIS+DSQ)
            wtij=(ris-rsq)/(ris+rsq)
            scratch = (abs(psurf(n)-.001*pbase(i,1))*fdob%DCON)
            pdfac=1.-AMIN1(1.0,scratch)
            wtij=wtij*pdfac
            WTIJ=AMAX1(0.0,WTIJ)

! try making sfc obs weighting go thru pbl
! jc kpbl is at dot or cross only - need to interpolate?
!          wtsig(1)=1.
            komax=max0(3,kpblt(i))

! jc arbitrary check here
            IF (iprt) THEN
              if (kpblt(i).gt.25 .and. ktau.ne.0)                         &
                                     write(6,552)inest,i,j,kpblt(i)
552           FORMAT('kpblt is gt 25, inest,i,j,kpblt=',4i4)
            ENDIF

            if(kpblt(i).gt.25) komax=3
            komin=1
            dk=float(komax)

            do k=komin,komax
  
              wtsig(k)=float(komax-k+1)/dk
              WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ

              WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*WTSIG(K)    &
                          *WTSIG(K)*ERFIVR

!            if(ivar.eq.1 .and. i.eq.38 .and. j.eq.78) then
!
!             write(6,'(a,i2,a,f8.3,a,f8.3,a,f8.3,a,f8.3,a,f8.3)')
!                         'Surface obs, after: k = ',k,            &
!                         ' WT2ERR = ',wt2err(i,k),                &
!                         ' TIMEWT = ',timewt,                     &
!                         ' WTIJ = ',wtij,                         &
!                         ' WSIG = ',wtsig(k),                     &
!                         ' ERFIVR = ',erfivr
!            endif

            enddo

          ENDDO

!         print *, "  Surface "

        endif   ! end check for obs in domain
! END SURFACE-LAYER U OR V OBS NUDGING

      ELSE    
! BEGIN CALCULATIONS TO SPREAD OBS INFLUENCE ALONG PRESSURE LEVELS
!
!       print *,'in upper air section'
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX, AND RINFAC.
! RINFAC VARIES AS A LINEAR FUNCTION FROM FROM RINFMN AT P*+PTOP
! TO RINFMX AT PFREE AND "ABOVE" (LOWER PRESSURE).
!ajb          SLOPE=(RINFMN-RINFMX)/(PSBO(N)+PTOP-PFREE)

        slope = (fdob%RINFMN-fdob%RINFMX)/(psurf(n)-fdob%PFREE)

        RINFAC=SLOPE*POB+fdob%RINFMX-SLOPE*fdob%pfree
        RINFAC=AMAX1(RINFAC,fdob%RINFMN)
        RINFAC=AMIN1(RINFAC,fdob%RINFMX)
!yliu: for multilevel upper-air data, take the maximum
!      for the I loop.
        if(nsndlev.gt.1) RINFAC = fdob%RINFMX 
!yliu end

        MAXI=IFIX(RA(N)+0.99+RINDX*RINFAC)
        MAXI=MIN0(ide-IGRID,MAXI)
        MINI=IFIX(RA(N)-RINDX*RINFAC-0.99)
        MINI=MAX0(1,MINI)

! yliu start
! use also obs outside of but close to this domain  -- upr data   
!     if(   RA(N).LT.(0.-RINFAC*RINDX)
!    & .or. RA(N).GT.float(IL)+RINFAC*RINDX
!    & .or. RB(N).LT.(0.-RINFAC*RINDX)
!    & .or. RB(N).GT.float(JL)+RINFAC*RINDX)then          
!        print *, " skipped obs far away from this I-range"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this 
!   domain. In later case, use BC column value to approximate the model value 
!   at obs point -- ERRF need model field in errob.F !!

!cc         if (i.eq.39 .and. j.eq.34) then
!cc            write(6,*) 'RA(N) = ',ra(n) 
!cc            write(6,*) 'rinfac = ',rinfac,' rindx = ',rindx
!cc         endif
        if(   RA(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RA(N).LE.float(ide)+RINFAC*RINDX/3                &
        .and. RB(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RB(N).LE.float(jde)+RINFAC*RINDX/3) then
! or use obs within this domain only
!     if(RA(N).LT.1 .or. RA(N).GT.float(IL) .or.
!    &   RB(N).LT.1 .or. RB(N).GT.float(JL)) then
!        print *, " skipped obs far outside of this domain"

! yliu end
! is this 2 needed here - kpbl not used?
!          MINI=MAX0(2,MINI)

! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
          RJ=FLOAT(J)
          RX=RJ-RB(N)
! WEIGHTS FOR THE 3-D VARIABLES
!
          ERFIVR=ERRF(IVAR,N)
! jc
          nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1
! yliu start
! test: do the sounding levels as individual obs
!        nsndlev=1
! yliu end
          njcsnd=nsndlev
!
          DO I=max0(its,MINI),min0(ite,MAXI)
! jc
            RI=FLOAT(I)
            RY=RI-RA(N)
            RIS=RINDX*RINFAC*RINDX*RINFAC
            RSQ=RX*RX+RY*RY
! yliu test: for upper-air data, keep D1 influence radii
!           RIS=RIS  /schnes /schnes
            WTIJ=(RIS-RSQ)/(RIS+RSQ)
            WTIJ=AMAX1(0.0,WTIJ)
! weight ob in vertical with +- 50 mb
! yliu: 75 hba for single upper-air, 30hba for multi-level soundings
            if(nsndlev.eq.1) then
              rinprs=7.5
            else
             rinprs=3.0
            endif
! yliu end
! 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   ---  HANDLE 1-LEVEL and MULTI-LEVEL OBSERVATIONS SEPARATELY  ---
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

            if(nsndlev.eq.1)then 
!----------------------------------------------------------------------
!         ---   HANDLE 1-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------

!         if(I.eq.MINI) print *, "  Single snd "
! ERFIVR is the residual (difference) between the ob and the model
! at that point. We can analyze that residual up and down.
! First find komin for ob.
!yliu start -- in the old code, komax and komin were reversed!
              do k=kte,1,-1
                pijk = .001*(pbase(i,k)+pp(i,k))
!               print *,'k,pijk,pob,rinprs=',k,pijk,pob,rinprs
                if(pijk.ge.(pob+rinprs)) then
                  komin=k
                  go to 325
                endif
              enddo
              komin=1
 325          continue
! now find komax for ob
              do k=3,kte
                pijk = .001*(pbase(i,k)+pp(i,k))
                if(pijk.le.(pob-rinprs)) then
                  komax=k
                  go to 326
                endif
              enddo
              komax=kte   ! yliu 20050706
 326          continue

! yliu: single-level upper-air data will act either above or below the PBL top
!          komax=min0(kpblt(i), komax) 
              if(komax.gt.kpblt(i)) komin=max0(kpblt(i), komin) 
              if(komin.lt.kpblt(i)) komax=min0(kpblt(i), komax) 
! yliu end
!
!           print *,'1 level, komin,komax=',komin,komax
!           if(i.eq.MINI) then
!             print *, "yyyyyyyyyyS IPL erfivr=", IPL, ERFIVR,J,pob
!             ERFIVR=0
!           endif
              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo
!yliu end

!cc         if (i.eq.39 .and. j.eq.34) then
!cc              write(6,*) ' komin = ', komin,' komax = ',komax
!cc         endif

              do k=komin,komax
                pijk = .001*(pbase(i,k)+pp(i,k))
                reserf(k)=erfivr
                wtsig(k)=1.-abs(pijk-pob)/rinprs
                wtsig(k)=amax1(wtsig(k),0.0)
!             print *,'k,pijk,pob,rinprs,wtsig=',k,pijk,pob,rinprs,wtsig(k)
! Now calculate WT and WT2ERR for each i,j,k point                      cajb
                WT(I,K)=WT(I,K)+TIMEWT*WTIJ*wtsig(k)

                WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*        &
                            reserf(k)*wtsig(k)*wtsig(k)
              enddo

            else
!----------------------------------------------------------------------
!         ---   HANDLE MULTI-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------
!yliu start 
!           if(I.eq.MINI) print *, "  Multi-level  snd "
!             print *, "   n=,nsndlev",n,nsndlev,nlevs_ob(n),lev_in_ob(n)  &
!                  ,nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1) 
              if(nlevs_ob(n+nsndlev-1).ne.lev_in_ob(n+nsndlev-1)) then
                IF (iprt) THEN
                  print *, "n = ",n,"nsndlev = ",nsndlev 
                  print *, "nlevs_ob,lev_in_ob",                          &
                           nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1)
                  print *, "in nudobs.F: sounding level messed up, stopping"
                ENDIF
                stop
              endif       
!yliu end
! This is for a multi-level observation
! The trick here is that the sounding is "one ob". You don't
!    want multiple levels to each be treated like separate
!    and independent observations.
! At each i,j want to interpolate sounding to the model levels at that
!    particular point.
              komin=1
              komax=kte-2
! this loop goes to 1501
! do from kte-2 to 1 so don't adjust top of model. Arbitrary.
!yliu start
              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo
!yliu end

              do k=komax,komin,-1
  
                pijk = .001*(pbase(i,k)+pp(i,k))

! if sigma level pressure is .gt. than the lowest ob level, don't interpolate
                if(pijk.gt.varobs(5,n)) then
                  go to 1501
                endif

! if sigma level pressure is .lt. than the highest ob level, don't interpolate
                if(pijk.le.varobs(5,n+nsndlev-1)) then 
                  go to 1501
                endif

! now interpolate sounding to this k
! yliu start-- recalculate WTij for each k-level
!ajb      SLOPE = (fdob%RINFMN-fdob%RINFMX)/(pdoc(i,j)+PTOP-fdob%PFREE)        
                slope = (fdob%RINFMN-fdob%RINFMX)/ (.001*pbase(i,1)-fdob%PFREE)
                RINFAC=SLOPE*pijk+fdob%RINFMX-SLOPE*fdob%PFREE              
                RINFAC=AMAX1(RINFAC,fdob%RINFMN)      
                RINFAC=AMIN1(RINFAC,fdob%RINFMX)
                RIS=RINDX*RINFAC*RINDX*RINFAC  
                RSQ=RX*RX+RY*RY               

! for upper-air data, keep D1 influence radii
!         RIS=RIS  /schnes /schnes
                WTIJ=(RIS-RSQ)/(RIS+RSQ)      
                WTIJ=AMAX1(0.0,WTIJ)
! yliu end

! this loop goes to 1503
                do nn=2,nsndlev
! only set pobhi if varobs(ivar) is ok
                  pobhi=-888888.

                  if(varobs(ivar,n+nn-1).gt.-800000.                           &
                  .and. varobs(5,n+nn-1).gt.-800000.) then
                    pobhi=varobs(5,n+nn-1)
                    nhi=n+nn-1
                    if(pobhi.lt.pijk .and. abs(pobhi-pijk).lt.20.) then
                      go to 1502        ! within 200mb of obs height
                    endif
                  endif

                enddo

! did not find any ob above within 100 mb, so jump out 
                go to 1501
 1502           continue

                nlo=nhi-1
                do nnjc=nhi-1,n,-1 
                  if(varobs(ivar,nnjc).gt.-800000.                             &
                  .and. varobs(5,nnjc).gt.-800000.) then
                    poblo=varobs(5,nnjc)
                    nlo=nnjc
                    if(poblo.gt.pijk .and. abs(poblo-pijk).lt.20.) then
                      go to 1505        ! within 200mb of obs height
                    endif
                  endif
                enddo
!yliu end --

! did not find any ob below within 200 mb, so jump out 
                go to 1501
 1505           continue

! interpolate to model level
                pdiffj=alog(pijk/poblo)/alog(pobhi/poblo)
                reserf(k)=errf(ivar,nlo)+                               &
                            (errf(ivar,nhi)-errf(ivar,nlo))*pdiffj
                wtsig(k)=1.
  
 1501           continue

! now calculate WT and WT2ERR for each i,j,k point                               cajb
                WT(I,K)=WT(I,K)+TIMEWT*WTIJ*wtsig(k)
  
                WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ*WTIJ*        &
                            reserf(k)*wtsig(k)*wtsig(k)

!        if(ivar.eq.1 .and. i.eq.38 .and. j.eq.78) then
!
!            if(wt(i,k) .ne. 0.0) then
!               scratch = WT2ERR(I,K)/WT(I,K)
!            else
!               scratch = 999.
!            endif
!
!         write(6,'(a,i2,a,f8.3,a,f4.2,a,f7.4,a,f4.2,a,f5.3,a,f7.4)')
!    $                    'Multi-level obs: k = ',k,
!    $                    ' WT2ERR = ',wt2err(i,k),
!    $                    ' WTIJ = ',wtij,
!    $                    ' RSF = ',reserf(k),
!    $                    ' WSIG = ',wtsig(k),
!    $                    ' WT = ',wt(i,k),
!    $                    ' W2EOWT = ',scratch
!        endif


! end do k
              enddo   ! enddo k levels
! end multi-levels
            endif  ! end if(nsndlev.eq.1)
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   END 1-LEVEL AND MULTI-LEVEL OBSERVATIONS
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
          ENDDO ! END DO MINI,MAXI LOOP

        endif ! check for obs in domain

! END OF NUDGING TO OBS ON PRESSURE LEVELS

      ENDIF !end IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5)

!----------------------------------------------------------------------
    ENDIF  ! END SECTION FOR PROCESSING OF OBSERVATION
!----------------------------------------------------------------------

!   n=n+1
    n=n+njcsnd

!yliu  1202 continue
    if(n.gt.nstat)then
!     print *,'n,nstat=',n,nstat,ivar,j
      go to 1203
    endif
!   print *, "e-- n=,nsndlev",n,njcsnd,nlevs_ob(n),lev_in_ob(n) 

!***********************************************************************
  ENDDO  ! END OUTER LOOP FOR THE NSTAT OBSERVATIONS
!***********************************************************************

 1203 continue

! WEIGHTS AND WEIGHTED DIFFERENCES HAVE BEEN SUMMED.  NOW
! APPLY THE NUDGING FACTOR AND THE RESULTANT TENDENCY TO
! THE ATEN ARRAY
! ASSURE THAT WT(I,K) AND WTP(I,K) ARE NONZERO SINCE
! THEY ARE USED BELOW IN THE DENOMINATOR.
  DO K=kts,kte
    DO I=its,ite
      IF(WT(I,K).EQ.0)THEN
        WT2ERR(I,K)=0.0
      ENDIF
      IF(WT(I,K).EQ.0)THEN
        WT(I,K)=1.0
      ENDIF
    ENDDO
  ENDDO

126 CONTINUE

  IF(IVAR.GE.3)GOTO 170
! this is for u,v
! 3-D DOT POINT TENDENCIES
 
! Calculate scales for converting nudge factor from u (v)
! to rho_u (or rho_v) units.
  
  call calc_rcouple_scales(mu,msf,rscale,ims,ime,its,ite)
 
  DO K=1,kte

    DO I=i_s,i_e

      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)
      ENDIF

!      if(ivar .eq. 1 .and. i.eq.38 .and. j.eq.78 .and. k.eq.1) then
!           scratch = GIV*RSCALE(I)*W2EOWT*fdob%TFACI*ISWIND*GFACTOR
!           write(6,*) 'ATEN calc: k = ',k
!           write(6,*) 'U before: aten = ',aten(i,k),' scr = ',scratch
!           write(6,*) 'GIV = ',giv,' rscale = ',rscale(i),
!     $               ' W2EOWT = ',w2eowt
!           write(6,*) 'TFACI = ',fdob%tfaci,' ISWIND = ',iswind,
!     $               ' GFACTOR = ',gfactor
!       endif
!
!      if(ivar .eq. 2 .and. i.eq.39 .and. j.eq.29) then
!           scratch = GIV*RSCALE(I)*W2EOWT*fdob%TFACI*ISWIND*GFACTOR
!           write(6,*) 'ATEN calc: k = ',k
!           write(6,*) 'V before: aten = ',aten(i,k),' scr = ',scratch
!           write(6,*) 'GIV = ',giv,' rscale = ',rscale(i),
!     $                ' W2EOWT = ',w2eowt
!           write(6,*) 'TFACI = ',fdob%tfaci,' ISWIND = ',iswind,
!     $                ' GFACTOR = ',gfactor
!       endif

        ATEN(i,k)=ATEN(i,k)+GIV*RSCALE(I)                        &
                    *W2EOWT*fdob%TFACI                           &
                    *ISWIND       *GFACTOR   !yliu *GFACTOR 

!      if(ivar .eq. 1 .and. i.eq.38 .and. j.eq.78 .and. k.eq.1) then
!           write(6,*) 'U after: aten = ',aten(i,k),' scr = ',scratch
!      endif
!      if(ivar .eq. 2 .and. i.eq.39 .and. j.eq.29) then
!           write(6,*) 'V after: aten = ',aten(i,k),' scr = ',scratch
!      endif

    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)
      ENDDO
    ENDDO
  ENDIF

  RETURN

170 CONTINUE

! 3-D CROSS-POINT TENDENCIES
! this is for t (ivar=3) and q (ivsr=4)
  IF(3-IVAR.LT.0)THEN
    GITQ=GIQ
  ELSE
    GITQ=GIT
  ENDIF
  IF(3-IVAR.LT.0)THEN
    ISTQ=ISMOIS
  ELSE
    ISTQ=ISTEMP
  ENDIF

  DO K=1,kte
    DO I=i_s,i_e
      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)
      ENDIF

!        if(ivar .eq. 3 .and. i.eq.39 .and. j.eq.29) then
!            scratch = GITQ*MU(I)*W2EOWT*fdob%TFACI*ISTQ*GFACTOR
!            write(6,*) 'ATEN calc: k = ',k
!            write(6,*) 'T before: aten = ',aten(i,k),' scr = ',scratch
!            write(6,*) 'GITQ = ',gitq,' MU = ',mu(i),
!     $                 ' W2EOWT = ',w2eowt
!            write(6,*) ' TFACI = ',fdob%tfaci,' ISTQ = ',istq,
!     $                 ' GFACTOR = ',gfactor
!        endif
!
!        if(ivar .eq. 4 .and. i.eq.39 .and. j.eq.29) then
!            scratch = GITQ*MU(I)*W2EOWT*fdob%TFACI*ISTQ*GFACTOR
!            write(6,*) 'ATEN calc: k = ',k
!            write(6,*) 'Q before: aten = ',aten(i,k),' scr = ',scratch
!            write(6,*) 'GITQ = ',gitq,' MU = ',mu(i),
!     $                 ' W2EOWT = ',w2eowt
!            write(6,*) ' TFACI = ',fdob%tfaci,' ISTQ = ',istq,
!     $                 ' GFACTOR = ',gfactor
!        endif

      ATEN(i,k)=ATEN(i,k)+GITQ*MU(I)                       &
                  *W2EOWT*fdob%TFACI*ISTQ       *GFACTOR   !yliu *GFACTOR

!        if(ivar .eq. 3 .and. i.eq.39 .and. j.eq.29) then
!            write(6,*) 'T after: aten = ',aten(i,k),' scr = ',scratch
!        endif
!        if(ivar .eq. 4 .and. i.eq.39 .and. j.eq.29) then
!            write(6,*) 'Q after: aten = ',aten(i,k),' scr = ',scratch
!        endif

    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR)) THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)
      ENDDO
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE nudob

  SUBROUTINE calc_rcouple_scales(a, msf, rscale, ims,ime, its,ite)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime           ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite           ! Tile   dimensions
  REAL, INTENT(IN)     :: a( ims:ime )      ! Air mass array 
  REAL, INTENT(IN)     :: msf( ims:ime )    ! Map scale factor array
  REAL, INTENT(OUT)    :: rscale( ims:ime ) ! Scales for rho-coupling

! Local variables
  integer :: i

! Calculate scales to be used for producing rho-coupled nudging factors.
  do i = its,ite
    rscale(i) = a(i)/msf(i)
  enddo

  RETURN
  END SUBROUTINE calc_rcouple_scales

!ajb: Not used
  SUBROUTINE set_real_array(rscale, value, ims,ime, its,ite)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime           ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite           ! Tile   dimensions
  REAL, INTENT(IN)     :: value             ! Constant array value
  REAL, INTENT(OUT)    :: rscale( ims:ime ) ! Output array

! Local variables
  integer :: i

! Set array to constant value
  do i = its,ite
    rscale(i) = value 
  enddo

  RETURN
  END SUBROUTINE set_real_array

!ajb: Not used
  SUBROUTINE calc_pottemp_scales(ivar, rcp, pb, p, tscale,             &
                                       ims,ime, its,ite,               &
                                     kms,kme, kts,kte)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime, kms,kme      ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite, kts,kte      ! Tile   dimensions
  INTEGER, INTENT(IN)  :: ivar                  ! Variable identifier 
  REAL, INTENT(IN)     :: rcp                   ! Constant (2./7.)
  REAL, INTENT(IN)     :: pb(ims:ime, kms:kme)  ! Base pressure (Pa) array 
  REAL, INTENT(IN)     :: p(ims:ime, kms:kme)   ! Pressure pert. (Pa) array
  REAL, INTENT(OUT)    :: tscale(ims:ime, kms:kme) ! Scales for pot. temp.
! Local variables
  integer :: i,k

  if(ivar.eq.3) then

! Calculate scales to be used for producing potential temperature nudging factors.
    do k = kts,kte
    do i = its,ite
      tscale(i,k) = ( 1000000. / ( pb(i,k)+p(i,k)) )**rcp
    enddo
    enddo
  else
! Set to 1. for moisture scaling.
    do k = kts,kte
      do i = its,ite
        tscale(i,k) = 1.0 
      enddo
    enddo
  endif
      
  RETURN
  END SUBROUTINE calc_pottemp_scales
#endif

END MODULE module_fddaobs_rtfdda


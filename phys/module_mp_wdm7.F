#if ( RWORDSIZE == 4 )
#  define VREC vsrec
#  define VSQRT vssqrt
#else
#  define VREC vrec
#  define VSQRT vsqrt
#endif
MODULE module_mp_wdm7
!
!
   USE module_mp_radar
!
   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120. ! maximum time step for minor loops
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6         ! intercept parameter rain
   REAL, PARAMETER, PRIVATE :: n0g = 4.e6         ! intercept parameter graupel 
   REAL, PARAMETER, PRIVATE :: n0h = 4.e4         ! intercept parameter hail 
   REAL, PARAMETER, PRIVATE :: avtr = 841.9       ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8         ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5         ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: peaut = .55        ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8        ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xncr0 = 5.e7
   REAL, PARAMETER, PRIVATE :: xncr1 = 5.e8
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5    ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER, PRIVATE :: avts = 11.72       ! a constant for terminal velocity of snow
   REAL, PARAMETER, PRIVATE :: bvts = .41         ! a constant for terminal velocity of snow 
   REAL, PARAMETER, PRIVATE :: avtg = 330.        ! a constant for terminal velocity of graupel
   REAL, PARAMETER, PRIVATE :: bvtg = 0.8         ! a constant for terminal velocity of graupel 
   REAL, PARAMETER, PRIVATE :: deng = 500.        ! density of graupel 
   REAL, PARAMETER, PRIVATE :: avth = 285.        ! a constant for terminal velocity of hail
   REAL, PARAMETER, PRIVATE :: bvth = 0.8         ! a constant for terminal velocity of hail
   REAL, PARAMETER, PRIVATE :: denh = 912.        ! density of hail
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e11    ! maximum n0s (t=-90C unlimited)
   REAL, PARAMETER, PRIVATE :: lamdacmax = 5.0e5  ! limited maximum value for slope parameter of cloud water
   REAL, PARAMETER, PRIVATE :: lamdacmin = 2.0e4  ! limited minimum value for slope parameter of cloud water
   REAL, PARAMETER, PRIVATE :: lamdarmax = 5.0e4  ! limited maximum value for slope parameter of rain
   REAL, PARAMETER, PRIVATE :: lamdarmin = 2.0e3  ! limited minimum value for slope parameter of rain
   REAL, PARAMETER, PRIVATE :: lamdasmax = 1.e5   ! limited maximum value for slope parameter of snow
   REAL, PARAMETER, PRIVATE :: lamdagmax = 6.e4   ! limited maximum value for slope parameter of graupel 
   REAL, PARAMETER, PRIVATE :: lamdahmax = 2.e4   ! limited maximum value for slope parameter of hail
   REAL, PARAMETER, PRIVATE :: dicon = 11.9       ! constant for the cloud-ice diamter
   REAL, PARAMETER, PRIVATE :: dimax = 500.e-6    ! limited maximum value for the cloud-ice diamter
   REAL, PARAMETER, PRIVATE :: n0s = 2.e6         ! temperature dependent intercept parameter snow 
   REAL, PARAMETER, PRIVATE :: alpha = .12        ! .122 exponen factor for n0s
   REAL, PARAMETER, PRIVATE :: pfrz1 = 100.       ! constant in Biggs freezing
   REAL, PARAMETER, PRIVATE :: pfrz2 = 0.66       ! constant in Biggs freezing
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-9     ! minimun values for qr, qs, and qg 
   REAL, PARAMETER, PRIVATE :: ncmin = 1.e1       ! minimum value for Nc 
   REAL, PARAMETER, PRIVATE :: nrmin = 1.e-2      ! minimum value for Nr
   REAL, PARAMETER, PRIVATE :: eacrc = 1.0        ! Snow/cloud-water collection efficiency
   REAL, PARAMETER, PRIVATE :: eachs = 1.0        ! Hail/snow collection efficiency
   REAL, PARAMETER, PRIVATE :: eachg = 0.5        ! Hail/graupel collection efficiency
   REAL, PARAMETER, PRIVATE :: dens  =  100.0     ! Density of snow
   REAL, PARAMETER, PRIVATE :: qs0   =  6.e-4     ! threshold amount for aggretion to occur
!
   REAL, PARAMETER, PRIVATE :: satmax = 1.0048    ! maximum saturation value for CCN activation 
                                                  ! 1.008 for maritime /1.0048 for conti 
   REAL, PARAMETER, PRIVATE :: actk = 0.6         ! parameter for the CCN activation
   REAL, PARAMETER, PRIVATE :: actr = 1.5         ! radius of activated CCN drops
   REAL, PARAMETER, PRIVATE :: ncrk1 = 3.03e3     ! Long's collection kernel coefficient
   REAL, PARAMETER, PRIVATE :: ncrk2 = 2.59e15    ! Long's collection kernel coefficient
   REAL, PARAMETER, PRIVATE :: di100 = 1.e-4      ! parameter related with accretion and collection of cloud drops
   REAL, PARAMETER, PRIVATE :: di600 = 6.e-4      ! parameter related with accretion and collection of cloud drops
   REAL, PARAMETER, PRIVATE :: di2000 = 2000.e-6  ! parameter related with accretion and collection of cloud drops 
   REAL, PARAMETER, PRIVATE :: di82    = 82.e-6   ! dimater related with raindrops evaporation
   REAL, PARAMETER, PRIVATE :: di15    = 15.e-6   ! auto conversion takes place beyond this diameter 
   REAL, PARAMETER, PRIVATE :: t00  = 238.16
   REAL, PARAMETER, PRIVATE :: t01  = 273.16
   REAL, PARAMETER, PRIVATE :: cd = 0.6           ! drag coefficient for hailsone
!
   REAL, SAVE ::                                               &
             qc0,qc1,qck1,pidnc,bvtr1,bvtr2,bvtr3,bvtr4,bvtr5, &
             bvtr6,bvtr7, bvtr2o5,bvtr3o5,                     &
             g1pbr,g2pbr,g3pbr,g4pbr,g5pbr,g6pbr,g7pbr,        &
             g5pbro2,g7pbro2,pi,                               &
             pvtr,pvtrn,eacrr,pacrr,pidn0r,pidnr,              &
             precr1,precr2,xmmax,roqimax,bvts1,bvts2,          &
             bvts3,bvts4,g1pbs,g3pbs,g4pbs,g5pbso2,            &
             pvts,pacrs,precs1,precs2,pidn0s,xlv1,pacrc,       &
             bvtg1,bvtg2,bvtg3,bvtg4,g1pbg,g3pbg,g4pbg,        &
             g5pbgo2,g6pbgh,pvtg,pacrg,                        &
             precg1,precg2,precg3,pidn0g,                      &
             bvth2,bvth3,bvth4,                                &
             g3pbh,g4pbh,g5pbho2,pvth,pacrh,                   &
             prech1,prech2,prech3,pidn0h,                      &
             rslopecmax,rslopec2max,rslopec3max,               &
             rslopermax,rslopesmax,rslopegmax,rslopehmax,      &
             rsloperbmax,rslopesbmax,rslopegbmax,rslopehbmax,  &
             rsloper2max,rslopes2max,rslopeg2max,rslopeh2max,  &
             rsloper3max,rslopes3max,rslopeg3max,rslopeh3max
CONTAINS
!===================================================================
!
  SUBROUTINE wdm7(th, q, qc, qr, qi, qs, qg, qh,           &
                    nn, nc, nr,                            &
                    den, pii, p, delz,                     &
                    delt,g, cpd, cpv, ccn0, rd, rv, t0c,   &
                    ep1, ep2, qmin,                        &
                    XLS, XLV0, XLF0, den0, denr,           &
                    cliq,cice,psat,                        &
                    xland,                                 &
                    rain, rainncv,                         &
                    snow, snowncv,                         &
                    sr,                                    &
                    refl_10cm, diagflag, do_radar_ref,     &
                    graupel, graupelncv,                   &
                    hail, hailncv,                         &
                    itimestep,                             &
                    has_reqc, has_reqi, has_reqs,          &  ! for radiation
                    re_cloud, re_ice,   re_snow,           &  ! for radiation     
                    ids,ide, jds,jde, kds,kde,             &
                    ims,ime, jms,jme, kms,kme,             &
                    its,ite, jts,jte, kts,kte              &
                                                           )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a WRF double-moment 7-class hail phase 
!  microphyiscs scheme (WDM7). The wdm microphysics scheme predicts 
!  number concentrations for warm rain species including clouds and 
!  rain. cloud condensation nuclei (ccn) is also predicted.
!  The cold rain species including ice, snow, graupel, hail follow the 
!  WRF single-moment 7-class microphysics (WSM7, Bae et al. 2018)
!  in which theoretical background for WSM ice phase microphysics is 
!  based on Hong et al. (2004). A new mixed-phase terminal velocity
!  for precipitating ice is introduced in WSM6 (Dudhia et al. 2008). 
!  The WDM scheme is described in Lim and Hong (2010).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WDM7 cloud scheme
!
!  Coded and implemented by Soo Ya Bae (KIAPS) Fall 2015
!
!  Implemented by Soo Ya Bae (KIAPS) Winter 2018
!
!  further modifications :
!        semi-lagrangian sedimentation (JH,2010),hong, aug 2009
!        ==> higher accuracy and efficient at lower resolutions
!        reflectivity computation from greg thompson, lim, jun 2011
!        ==> only diagnostic, but with removal of too large drops
!        add hail option from afwa, aug 2014
!        ==> switch graupel or hail by changing no, den, fall vel.
!        effective radius of hydrometeors, bae from kiaps, jan 2015
!        ==> consistency in solar insolation of rrtmg radiation
!
!  References:
!        Bae, Hong and Tao (BHT, 2018) Asia-Pacific J. Atmos. Sci.
!        Lim and Hong (LH, 2010) Mon. Wea. Rev. 
!        Juang and Hong (JH, 2010) Mon. Wea. Rev.
!        Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev. 
!        Hong and Lim (HL, 2006) J. Korean Meteor. Soc. 
!        Cohard and Pinty (CP, 2000) Quart. J. Roy. Meteor. Soc.
!        Khairoutdinov and Kogan (KK, 2000) Mon. Wea. Rev.
!        Dudhia, Hong and Lim (DHL, 2008) J. Meteor. Soc. Japan
!             
!        Lin, Farley, Orville (LFO, 1983) J. Appl. Meteor.
!        Rutledge, Hobbs (RH83, 1983) J. Atmos. Sci.
!        Rutledge, Hobbs (RH84, 1984) J. Atmos. Sci.
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  real, dimension( ims:ime , jms:jme), intent(in) ::              &
                                                           xland
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                               q, &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg, &
                                                              qh, &
                                                              nn, & 
                                                              nc, &
                                                              nr
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                             pii, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                              rd, &
                                                              rv, &
                                                             t0c, &
                                                            den0, &
                                                             cpd, &
                                                             cpv, &
                                                            ccn0, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  INTEGER, INTENT(IN   ) ::                            itimestep
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr
! for radiation connecting
  INTEGER, INTENT(IN)::                                           &
                                                        has_reqc, &
                                                        has_reqi, &
                                                        has_reqs
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                     &
        INTENT(INOUT)::                                           &
                                                        re_cloud, &
                                                          re_ice, &
                                                         re_snow   

!+---+-----------------------------------------------------------------+
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT)::     &  ! GT
                                                       refl_10cm
!+---+-----------------------------------------------------------------+

  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv
  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                 graupel, &
                                                        graupelncv
  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                    hail, &
                                                         hailncv
   
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci
  REAL, DIMENSION( its:ite , kts:kte, 4 ) ::   qrs
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::   ncr
  INTEGER ::               i,j,k

!+---+-----------------------------------------------------------------+
      REAL, DIMENSION(kts:kte):: qv1d, t1d, p1d, qr1d, nr1d, qs1d, qg1d, dBZ
      LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
      INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref
!+---+-----------------------------------------------------------------+
! to calculate effective radius for radiation
  REAL, DIMENSION( kts:kte ) :: qc1d, nc1d, den1d
  REAL, DIMENSION( kts:kte ) :: qi1d
  REAL, DIMENSION( kts:kte ) :: re_qc, re_qi, re_qs

      IF (itimestep .eq. 1) THEN 
        DO j=jms,jme
           DO k=kms,kme    
           DO i=ims,ime
              nn(i,k,j) = ccn0   
           ENDDO
           ENDDO
        ENDDO
      ENDIF
!
      DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
            qrs(i,k,3) = qg(i,k,j)
            qrs(i,k,4) = qh(i,k,j)
            ncr(i,k,1) = nn(i,k,j)
            ncr(i,k,2) = nc(i,k,j)
            ncr(i,k,3) = nr(i,k,j)     
         ENDDO
         ENDDO
         !  Sending array starting locations of optional variables may cause
         !  troubles, so we explicitly change the call.
         CALL wdm72D(t, q(ims,kms,j), qci, qrs, ncr               &
                    ,den(ims,kms,j)                               &
                    ,p(ims,kms,j), delz(ims,kms,j)                &
                    ,delt,g, cpd, cpv, ccn0, rd, rv, t0c          &
                    ,ep1, ep2, qmin                               &
                    ,XLS, XLV0, XLF0, den0, denr                  &
                    ,cliq,cice,psat                               &
                    ,j                                            &
                    ,xland(ims,j)                                 &
                    ,rain(ims,j),rainncv(ims,j)                   &
                    ,sr(ims,j)                                    &
                    ,ids,ide, jds,jde, kds,kde                    &
                    ,ims,ime, jms,jme, kms,kme                    &
                    ,its,ite, jts,jte, kts,kte                    &
                    ,snow(ims,j),snowncv(ims,j)                   &
                    ,graupel(ims,j),graupelncv(ims,j)             & 
                    ,hail(ims,j),hailncv(ims,j)                   & 
                                                                   )
         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            qg(i,k,j) = qrs(i,k,3)
            qh(i,k,j) = qrs(i,k,4)
            nn(i,k,j) = ncr(i,k,1)
            nc(i,k,j) = ncr(i,k,2)
            nr(i,k,j) = ncr(i,k,3)   
         ENDDO
         ENDDO
!+---+-----------------------------------------------------------------+
         IF ( PRESENT (diagflag) ) THEN
         if (diagflag .and. do_radar_ref == 1) then
            DO I=its,ite
               DO K=kts,kte
                  t1d(k)=th(i,k,j)*pii(i,k,j)
                  p1d(k)=p(i,k,j)
                  qv1d(k)=q(i,k,j)
                  qr1d(k)=qr(i,k,j)
                  nr1d(k)=nr(i,k,j)
                  qs1d(k)=qs(i,k,j)
                  qg1d(k)=qg(i,k,j)
               ENDDO
               call refl10cm_wdm7 (qv1d, qr1d, nr1d, qs1d, qg1d,        &
                       t1d, p1d, dBZ, kts, kte, i, j)
               do k = kts, kte
                  refl_10cm(i,k,j) = MAX(-35., dBZ(k))
               enddo
            ENDDO
         endif
         ENDIF

! calculate effective radius of cloud, ice, and snow 
         IF (has_reqc.ne.0 .and. has_reqi.ne.0 .and. has_reqs.ne.0) THEN
           DO i=its,ite
             DO k=kts,kte
               re_qc(k) = 2.51E-6
               re_qi(k) = 10.01E-6
               re_qs(k) = 25.E-6

               t1d(k)  = th(i,k,j)*pii(i,k,j)
               den1d(k)= den(i,k,j)
               qc1d(k) = qc(i,k,j)
               qi1d(k) = qi(i,k,j)
               qs1d(k) = qs(i,k,j)
               nc1d(k) = nc(i,k,j)
             ENDDO
             call effectRad_wdm7(t1d, qc1d, nc1d, qi1d, qs1d, den1d,   &
                                 qmin, t0c, re_qc, re_qi, re_qs,       &
                                 kts, kte, i, j)
             DO k=kts,kte
               re_cloud(i,k,j) = max(2.51E-6,  min(re_qc(k),  50.E-6))
               re_ice(i,k,j)   = max(10.01E-6, min(re_qi(k), 125.E-6))
               re_snow(i,k,j)  = max(25.E-6,   min(re_qs(k), 999.E-6))
             ENDDO
           ENDDO
         ENDIF
           
      ENDDO
 
  END SUBROUTINE wdm7
!===================================================================
!
  SUBROUTINE wdm72D(t, q, qci, qrs, ncr, den, p, delz             &
                   ,delt,g, cpd, cpv, ccn0, rd, rv, t0c           &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,slmsk                                         &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                   ,graupel,graupelncv                            &
                   ,hail,hailncv                                  &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte , &
                                      lat
  real, dimension(ims:ime),  intent(in) ::                 slmsk
  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL, DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci
  REAL, DIMENSION( its:ite , kts:kte, 4 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qrs
  REAL, DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             ncr
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
                                                            ccn0, &
                                                             t0c, &
                                                            den0, &
                                                              rd, &
                                                              rv, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL, DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  REAL, DIMENSION( ims:ime ),     OPTIONAL,                       &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv
  REAL, DIMENSION( ims:ime ),     OPTIONAL,                       &
        INTENT(INOUT) ::                                 graupel, &
                                                      graupelncv
  REAL, DIMENSION( ims:ime ),     OPTIONAL,                       &
        INTENT(INOUT) ::                                    hail, &
                                                         hailncv
! LOCAL VAR
  real, dimension( its:ite , kts:kte ) ::                         &
        qcr 
  REAL, DIMENSION( its:ite , kts:kte , 3) ::                      &
        rh, qs
  REAL, DIMENSION( its:ite , kts:kte,  4) ::                      & 
        rslope, rslope2, rslope3, rslopeb,                        &
        falk, fall, work1, qrs_tmp
  REAL, DIMENSION( its:ite , kts:kte ) ::                         & 
        rslopec, rslopec2,rslopec3 
  REAL, DIMENSION( its:ite , kts:kte,  2) ::                      &
        avedia 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        workn,falln,falkn
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        worka,workr,workh
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        den_tmp, delz_tmp, ncr_tmp 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        lamdr_tmp
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        lamdc_tmp
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pcact, prevp, psdep, pgdep, phdep, praut, psaut, pgaut,   &
        phaut, pracw, psacw, pgacw, phacw, pgaci, pgacr, pgacs,   &
        psaci, praci, piacr, pracs, psacr, phacr, phacs, phacg,   &
        phaci, pracg, pimlt, psmlt, pgmlt, phmlt, pseml, pgeml,   &
        pheml
  REAL, DIMENSION( its:ite , kts:kte ) :: paacw
  REAL, DIMENSION( its:ite , kts:kte ) :: primh, pvapg, pvaph
  REAL, DIMENSION( its:ite , kts:kte ) :: pgwet, phwet
  REAL, DIMENSION( its:ite , kts:kte ) :: pgaci_w, phaci_w
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        nraut, nracw, ncevp, nccol, nrcol,                        &
        nsacw, ngacw, nhacw, niacr, nsacr, ngacr, nhacr, naacw,   &
        nseml, ngeml, nheml, ncact 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pigen, pidep, pcond, pgevp, psevp, phevp,                 &
        xl, cpm, work2, denfac, n0sfac, qsum,                     &
        denqrs1, denqr1, denqrs2, denqrs3, denqrs4,               &
        denncr3, denqci, xni 
  REAL, DIMENSION( its:ite ) ::                                   &
        delqrs1, delqrs2, delqrs3, delqrs4, delncr3, delqi
  REAL, DIMENSION( its:ite ) :: tstepsnow, tstepgraup, tstephail
  REAL :: gfac, sfac
! variables for optimization
  REAL, DIMENSION( its:ite )           :: tvec1
  REAL :: temp
  INTEGER, DIMENSION( its:ite ) :: mnstep, numndt
  INTEGER, DIMENSION( its:ite ) :: mstep, numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::                                                        &
            cpmcal, xlcal, lamdac,                                &
            diffus,                                               &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            ndt, qdt, holdrr, holdrs, holdrg, supcol, supcolt,    &
            pvt, coeres, supsat, dtcld, xmi, eacrs, satdt,        &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, fallsum_qg, fallsum_qh,         &
            vt2i,vt2r,vt2s,vt2g,vt2h,acrfac,egs,egi,ehi,          &
            xlwork2, factor, source, value, coecol,               &
            nfrzdtr, nfrzdtc,                                     &
            taucon, lencon, lenconcr,                             &
            xlf, pfrzdtc, pfrzdtr, supice, alpha2, delta2, delta3 
  REAL  :: vt2ave
  REAL  :: frac
  REAL  :: rs0, ghw1, ghw2, ghw3, ghw4
  REAL  :: holdc, holdci
!
  INTEGER :: i, j, k, mstepmax,                                                &
            iprt, latd, lond, loop, loops, ifsat, n, idim, kdim
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
!
!=================================================================
!   compute internal functions
!
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
!
! Optimizatin : A**B => exp(log(A)*(B)) 
      lamdac(x,y,z)= exp(log(((pidnc*z)/(x*y)))*((.33333333)))
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!
      diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y   ! 8.794e-5*x**1.81/y
      viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
      xka(x,y) = 1.414e3*viscos(x,y)*y
      diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
      venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
                     /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
      conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!
      idim = ite-its+1
      kdim = kte-kts+1
!
!----------------------------------------------------------------
!  paddint 0 for negative values generated by dynamics
!
      do k = kts, kte
        do i = its, ite
          qci(i,k,1) = max(qci(i,k,1),0.0)
          qrs(i,k,1) = max(qrs(i,k,1),0.0)
          qci(i,k,2) = max(qci(i,k,2),0.0)
          qrs(i,k,2) = max(qrs(i,k,2),0.0)
          qrs(i,k,3) = max(qrs(i,k,3),0.0)
          qrs(i,k,4) = max(qrs(i,k,4),0.0)
          ncr(i,k,1) = min(max(ncr(i,k,1),1.e8),2.e10)
          ncr(i,k,2) = max(ncr(i,k,2),0.0)
          ncr(i,k,3) = max(ncr(i,k,3),0.0) 
        enddo
      enddo
!
!----------------------------------------------------------------
!  latent heat for phase changes and heat capacity. neglect the
!  changes during microphysical process calculation
!  emanuel(1994)
!
      do k = kts, kte
        do i = its, ite
          cpm(i,k) = cpmcal(q(i,k))
          xl(i,k) = xlcal(t(i,k))
        enddo
      enddo
!
      qcr(:,:) = 0.0
      do i = its,ite
        if(slmsk(i).eq.2) then      ! water
          qcr(i,:) = qc0
        else
          qcr(i,:) = qc1
        endif
      enddo
!
      do k = kts, kte
        do i = its, ite
          delz_tmp(i,k) = delz(i,k)
          den_tmp(i,k) = den(i,k)
        enddo
      enddo
!
! initialize the surface rain, snow, graupel, hail
!
      do i = its, ite
        rainncv(i) = 0.
        if(PRESENT (snowncv) .AND. PRESENT (snow)) snowncv(i) = 0.
        if(PRESENT (graupelncv) .AND. PRESENT (graupel)) graupelncv(i) = 0.
        if(PRESENT (hailncv) .AND. PRESENT (hail)) hailncv(i) = 0.
        sr(i) = 0.
!
! new local array to catch step snow, graupel, and hail
!
        tstepsnow(i) = 0.
        tstepgraup(i) = 0.
        tstephail(i) = 0.
      enddo
!
!----------------------------------------------------------------
!     compute the minor time steps.
!
      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
      do i = its, ite
        mstep(i) = 1
        mnstep(i) = 1
        flgcld(i) = .true.
      enddo
!
      do k = kts, kte
        CALL VREC( tvec1(its), den(its,k), ite-its+1)
        do i = its, ite
          tvec1(i) = tvec1(i)*den0
        enddo
        CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1)
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
        do i = its, ite
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          endif
          qs(i,k,2) = min(qs(i,k,2),0.99*p(i,k))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
        do i = its, ite
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          pgdep(i,k) = 0.
          phdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pgaut(i,k) = 0.
          phaut(i,k) = 0.
          pracw(i,k) = 0.
          praci(i,k) = 0.
          pracs(i,k) = 0.
          pracg(i,k) = 0.
          piacr(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          psacr(i,k) = 0.
          pgacw(i,k) = 0.
          paacw(i,k) = 0.
          pgaci(i,k) = 0.
          pgacr(i,k) = 0.
          pgacs(i,k) = 0.
          phacw(i,k) = 0.
          phaci(i,k) = 0.
          phacr(i,k) = 0.
          phacs(i,k) = 0.
          phacg(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          pgmlt(i,k) = 0.
          phmlt(i,k) = 0.
          pseml(i,k) = 0.
          pgeml(i,k) = 0.
          pheml(i,k) = 0.
          psevp(i,k) = 0.
          pgevp(i,k) = 0.
          phevp(i,k) = 0.
          pcact(i,k) = 0.
          primh(i,k) = 0.
          pvapg(i,k) = 0.
          pvaph(i,k) = 0.
          pgwet(i,k) = 0.
          phwet(i,k) = 0.
          pgaci_w(i,k) = 0.
          phaci_w(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          falk(i,k,3) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fall(i,k,3) = 0.
          fall(i,k,4) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          falln(i,k) =0.
          falkn(i,k) =0.
          xni(i,k) = 1.e3
          nsacw(i,k) = 0.
          ngacw(i,k) = 0.
          nhacw(i,k) = 0.
          naacw(i,k) = 0.
          niacr(i,k) = 0.
          nsacr(i,k) = 0.
          ngacr(i,k) = 0.
          nhacr(i,k) = 0.
          nseml(i,k) = 0.
          ngeml(i,k) = 0.
          nheml(i,k) = 0.
          nracw(i,k) = 0.
          nccol(i,k) = 0.
          nrcol(i,k) = 0.
          ncact(i,k) = 0.
          nraut(i,k) = 0.
          ncevp(i,k) = 0.
          delqrs1(i) = 0.
          delqrs2(i) = 0.
          delqrs3(i) = 0.
          delqrs4(i) = 0.
        enddo
      enddo
      do k = kts, kte
        do i = its, ite
          if(qci(i,k,1).le.qmin .or. ncr(i,k,2).le.ncmin ) then
            rslopec(i,k) = rslopecmax
            rslopec2(i,k) = rslopec2max
            rslopec3(i,k) = rslopec3max
          else
            rslopec(i,k) = 1./lamdac(qci(i,k,1),den(i,k),ncr(i,k,2))
            rslopec2(i,k) = rslopec(i,k)*rslopec(i,k)
            rslopec3(i,k) = rslopec2(i,k)*rslopec(i,k)
          endif
!
! Ni: ice crystal number concentraiton   [HDC 5c]
!
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
        enddo
      enddo
!
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
          qrs_tmp(i,k,4) = qrs(i,k,4)
          ncr_tmp(i,k) = ncr(i,k,3)
        enddo
      enddo
      call slope_wdm7(qrs_tmp,ncr_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2, & 
                     rslope3,work1,workn,its,ite,kts,kte)
!
! vt update for qr and nr
!
      mstepmax = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          work1(i,k,1) = work1(i,k,1)/delz(i,k)
          workn(i,k) = workn(i,k)/delz(i,k)
          numdt(i) = max(nint(max(work1(i,k,1),workn(i,k))*dtcld+.5),1)
          if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
        enddo
      enddo
      do i = its, ite
        if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
      enddo
!
      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mstep(i)) then
            falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
            falkn(i,k) = ncr(i,k,3)*workn(i,k)/mstep(i)
            fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
            falln(i,k) = falln(i,k)+falkn(i,k)
            qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k),0.)
            ncr(i,k,3) = max(ncr(i,k,3)-falkn(i,k)*dtcld,0.)
          endif
        enddo
!
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falkn(i,k) = ncr(i,k,3)*workn(i,k)/mstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              falln(i,k) = falln(i,k)+falkn(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              ncr(i,k,3) = max(ncr(i,k,3)-(falkn(i,k)-falkn(i,k+1)*delz(i,k+1) &
                          /delz(i,k))*dtcld,0.)
            endif
          enddo
        enddo
!
        do k = kts, kte
          do i = its, ite
            qrs_tmp(i,k,1) = qrs(i,k,1)
            ncr_tmp(i,k) = ncr(i,k,3)
          enddo
        enddo
!
        call slope_rain(qrs_tmp,ncr_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2, &
                     rslope3,work1,workn,its,ite,kts,kte)
!
        do k = kte, kts, -1
          do i = its, ite
            work1(i,k,1) = work1(i,k,1)/delz(i,k)
            workn(i,k) = workn(i,k)/delz(i,k)
          enddo
        enddo
      enddo
!
! for semi
!
      do k = kte, kts, -1
        do i = its, ite
          workh(i,k) = work1(i,k,4)
          qsum(i,k) = max( (qrs(i,k,2)+qrs(i,k,3)), 1.E-15)
          if(qsum(i,k) .gt. 1.e-15 ) then
            worka(i,k) = (work1(i,k,2)*qrs(i,k,2) + work1(i,k,3)*qrs(i,k,3)) &
                      /qsum(i,k)
          else
            worka(i,k) = 0.
          endif
          denqrs2(i,k) = den(i,k)*qrs(i,k,2)
          denqrs3(i,k) = den(i,k)*qrs(i,k,3)
          denqrs4(i,k) = den(i,k)*qrs(i,k,4)
          if(qrs(i,k,4).le.0.0) workh(i,k) = 0.0
        enddo
      enddo
!
      call nislfv_rain_plm6(idim,kdim,den_tmp,denfac,t,delz_tmp,worka,         &
                           denqrs2,denqrs3,delqrs2,delqrs3,dtcld,1,1)
      call nislfv_rain_plmr(idim,kdim,den_tmp,denfac,t,delz_tmp,workh,         &
                           denqrs4,denqrs4,delqrs4,dtcld,2,1,0)
!
      do k = kts, kte
        do i = its, ite
          qrs(i,k,2) = max(denqrs2(i,k)/den(i,k),0.)
          qrs(i,k,3) = max(denqrs3(i,k)/den(i,k),0.)
          qrs(i,k,4) = max(denqrs4(i,k)/den(i,k),0.)
          fall(i,k,2) = denqrs2(i,k)*worka(i,k)/delz(i,k)
          fall(i,k,3) = denqrs3(i,k)*worka(i,k)/delz(i,k)
          fall(i,k,4) = denqrs4(i,k)*workh(i,k)/delz(i,k)
        enddo
      enddo
!
      do i = its, ite
        fall(i,1,2) = delqrs2(i)/delz(i,1)/dtcld
        fall(i,1,3) = delqrs3(i)/delz(i,1)/dtcld
        fall(i,1,4) = delqrs4(i)/delz(i,1)/dtcld
      enddo
!
      do k = kts, kte
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
          qrs_tmp(i,k,4) = qrs(i,k,4)
          ncr_tmp(i,k) = ncr(i,k,3)
        enddo
      enddo
!
      call slope_wdm7(qrs_tmp,ncr_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2, &
                     rslope3,work1,workn,its,ite,kts,kte)
!
      do k = kte, kts, -1
        do i = its, ite
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(t(i,k).gt.t0c) then
!
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: QS->QR)
!
            xlf = xlf0
            work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
            if(qrs(i,k,2).gt.0.) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2.       &
                         *n0sfac(i,k)*(precs1*rslope2(i,k,2)                 &
                         +precs2*work2(i,k)*coeres)/den(i,k)                  
              psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep(i),-qrs(i,k,2)     &
                         /mstep(i)),0.)
!
! nsmlt: melting of snow [LH A27]
!       (T>T0: ->NR)
!
              if(qrs(i,k,2).gt.qcrmin) then
                sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                ncr(i,k,3) = ncr(i,k,3) - sfac*psmlt(i,k)
              endif
              qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
              qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
              t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)
            endif
!
! pgmlt: melting of graupel [HL A23]  [LFO 47]
!       (T>T0: QG->QR)
!
            if(qrs(i,k,3).gt.0.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*(precg1     &
                           *rslope2(i,k,3) + precg2*work2(i,k)*coeres)       &
                           /den(i,k)                                          
              pgmlt(i,k) = min(max(pgmlt(i,k)*dtcld/mstep(i),                &
                          -qrs(i,k,3)/mstep(i)),0.)
!
! ngmlt: melting of graupel [LH A28]
!       (T>T0: ->NR)
!
              if(qrs(i,k,3).gt.qcrmin) then
                gfac = rslope(i,k,3)*n0g/qrs(i,k,3)
                ncr(i,k,3) = ncr(i,k,3) - gfac*pgmlt(i,k)
              endif
              qrs(i,k,3) = qrs(i,k,3) + pgmlt(i,k)
              qrs(i,k,1) = qrs(i,k,1) - pgmlt(i,k)
              t(i,k) = t(i,k) + xlf/cpm(i,k)*pgmlt(i,k)
            endif
!
! phmlt: melting of hail [BHT A22]
!       (T>T0: QH->QR)
!
            if(qrs(i,k,4).gt.0.) then
              coeres = rslope2(i,k,4)*sqrt(rslope(i,k,4)*rslopeb(i,k,4))
              phmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*(prech1     &
                           *rslope2(i,k,4) + prech2*work2(i,k)*coeres)       &
                           /den(i,k)
              phmlt(i,k) = min(max(phmlt(i,k)*dtcld/mstep(i),                &
                          -qrs(i,k,4)/mstep(i)),0.)
              qrs(i,k,4) = qrs(i,k,4) + phmlt(i,k)
              qrs(i,k,1) = qrs(i,k,1) - phmlt(i,k)
!
! nhmlt: melting of hail
!       (T>T0: ->NR)
!
              if(qrs(i,k,4).gt.qcrmin) then
                gfac = rslope(i,k,4)*n0h/qrs(i,k,4)
                ncr(i,k,3) = ncr(i,k,3) - gfac*phmlt(i,k)
              endif
              t(i,k) = t(i,k) + xlf/cpm(i,k)*phmlt(i,k)
            endif
          endif
        enddo
      enddo
!
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!
      do k = kte, kts, -1
        do i = its, ite
          if(qci(i,k,2).le.0.) then
            work1c(i,k) = 0.
          else
            xmi = den(i,k)*qci(i,k,2)/xni(i,k)
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
          endif
        enddo
      enddo
!
!  forward semi-laglangian scheme (JH), PCM (piecewise constant),  (linear)
!
      do k = kte, kts, -1
        do i = its, ite
          denqci(i,k) = den(i,k)*qci(i,k,2)
        enddo
      enddo
!
      call nislfv_rain_plmr(idim,kdim,den_tmp,denfac,t,delz_tmp,work1c,denqci,denqci,  &
                           delqi,dtcld,1,0,0)
!
      do k = kts, kte
        do i = its, ite
          qci(i,k,2) = max(denqci(i,k)/den(i,k),0.)
        enddo
      enddo
!
      do i = its, ite
        fallc(i,1) = delqi(i)/delz(i,1)/dtcld
      enddo
!
! rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,kts,1)+fall(i,kts,2)+fall(i,kts,3)+fall(i,kts,4)+fallc(i,kts)
        fallsum_qsi = fall(i,kts,2)+fallc(i,kts)
        fallsum_qg = fall(i,kts,3)
        fallsum_qh = fall(i,kts,4)
!
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,kts)/denr*dtcld*1000. + rainncv(i)
          rain(i) = fallsum*delz(i,kts)/denr*dtcld*1000. + rain(i)
        endif
!
        if(fallsum_qsi.gt.0.) then
          tstepsnow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + tstepsnow(i)
          if( PRESENT (snowncv) .AND. PRESENT (snow)) then
            snowncv(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snowncv(i)     
            snow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snow(i)
          endif
        endif
!
        if(fallsum_qg.gt.0.) then
          tstepgraup(i) = fallsum_qg*delz(i,kts)/denr*dtcld*1000.              &
                            + tstepgraup(i)
          if( PRESENT (graupelncv) .and. PRESENT (graupel)) then
            graupelncv(i) = fallsum_qg*delz(i,kts)/denr*dtcld*1000.            &  
                            + graupelncv(i)
            graupel(i) = fallsum_qg*delz(i,kts)/denr*dtcld*1000. + graupel(i)
          endif
        endif
!
        if(fallsum_qh.gt.0.) then
          tstephail(i)  = fallsum_qh*delz(i,kts)/denr*dtcld*1000.+tstephail(i)
          if ( PRESENT (hailncv) .AND. PRESENT (hail)) then
            hailncv(i) = fallsum_qh*delz(i,kts)/denr*dtcld*1000. + hailncv(i)
            hail(i) = fallsum_qh*delz(i,kts)/denr*dtcld*1000. + hail(i)
          endif
        endif
!
        if(fallsum.gt.0.) sr(i) = (tstepsnow(i) + tstepgraup(i) + tstephail(i))&
                                  /(rainncv(i)+1.e-12)
      enddo
!
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: QI->QC)
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0 .and. qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
!
! nimlt: instantaneous melting of cloud ice  [LH A18]
!        (T>T0: ->NC)
!
            ncr(i,k,2) = ncr(i,k,2) + xni(i,k)
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!
! pihmf: homogeneous  of cloud water below -40c [HL A45]
!        (T<-40C: QC->QI)
!
          if(supcol.gt.40. .and. qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
!
! nihmf: homogeneous  of cloud water below -40c [LH A17]
!        (T<-40C: NC->) 
!
            if(ncr(i,k,2).gt.0.) ncr(i,k,2) = 0. 
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!
! pihtf: heterogeneous  of cloud water [HL A44]
!        (T0>T>-40C: QC->QI)
!
          if(supcol.gt.0. .and. qci(i,k,1).gt.qmin) then
            supcolt=min(supcol,70.)
            pfrzdtc = min(pi*pi*pfrz1*(exp(pfrz2*supcolt)-1.)*denr/den(i,k)    & 
                     *ncr(i,k,2)*rslopec3(i,k)*rslopec3(i,k)/18.*dtcld         &
                     ,qci(i,k,1))
!
! nihtf: heterogeneous  of cloud water [LH A16]
!         (T0>T>-40C: NC->) 
!
            if(ncr(i,k,2).gt.ncmin) then
              nfrzdtc = min(pi*pfrz1*(exp(pfrz2*supcolt)-1.)*ncr(i,k,2)        &
                      *rslopec3(i,k)/6.*dtcld,ncr(i,k,2))
              ncr(i,k,2) = ncr(i,k,2) - nfrzdtc
            endif
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif 
!
! pgfrz:  freezing of rain water [HL A20] [LFO 45]
!        (T<T0, QR->QG)
!
          if(supcol.gt.0. .and. qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,70.)
            pfrzdtr = min(140.*(pi*pi)*pfrz1*ncr(i,k,3)*denr/den(i,k)          &
                  *(exp(pfrz2*supcolt)-1.)*rslope3(i,k,1)*rslope3(i,k,1)       & 
                  *dtcld,qrs(i,k,1))        
!
! ngfrz: freezing of rain water [LH A26]
!        (T<T0, NR-> ) 
!
            if(ncr(i,k,3).gt.nrmin) then
              nfrzdtr = min(4.*pi*pfrz1*ncr(i,k,3)*(exp(pfrz2*supcolt)-1.)     &
                       *rslope3(i,k,1)*dtcld, ncr(i,k,3)) 
              ncr(i,k,3) = ncr(i,k,3) - nfrzdtr
            endif
            qrs(i,k,3) = qrs(i,k,3) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1) - pfrzdtr
          endif
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          ncr(i,k,2) = max(ncr(i,k,2),0.0)
          ncr(i,k,3) = max(ncr(i,k,3),0.0)
        enddo
      enddo
!
!----------------------------------------------------------------
! update the slope parameters for microphysics computation
!
      do k = kts, kte
        do i = its, ite
          qrs_tmp(i,k,1) = qrs(i,k,1)
          qrs_tmp(i,k,2) = qrs(i,k,2)
          qrs_tmp(i,k,3) = qrs(i,k,3)
          qrs_tmp(i,k,4) = qrs(i,k,4)
          ncr_tmp(i,k) = ncr(i,k,3)
        enddo
      enddo
!
      call slope_wdm7(qrs_tmp,ncr_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2, &
                     rslope3,work1,workn,its,ite,kts,kte)
!
      do k = kts, kte
        do i = its, ite
!
! compute the mean-volume drop diameter                  [LH A10] 
! for raindrop distribution                          
!
          avedia(i,k,2) = rslope(i,k,1)*((24.)**(.3333333))
!
          if(qci(i,k,1).le.qmin .or. ncr(i,k,2).le.ncmin) then
            rslopec(i,k) = rslopecmax
            rslopec2(i,k) = rslopec2max
            rslopec3(i,k) = rslopec3max
          else
            rslopec(i,k) = 1./lamdac(qci(i,k,1),den(i,k),ncr(i,k,2))
            rslopec2(i,k) = rslopec(i,k)*rslopec(i,k)
            rslopec3(i,k) = rslopec2(i,k)*rslopec(i,k)
          endif
!
! compute the mean-volume drop diameter                   [LH A7]
! for cloud-droplet distribution
!
          avedia(i,k,1) = rslopec(i,k)
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k,1))
          work1(i,k,2) = diffac(xls,p(i,k),t(i,k),den(i,k),qs(i,k,2))
          work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
        enddo
      enddo
!
!===============================================================
!
! warm rain processes
!
!  - follows the double-moment processes in Lim and Hong
!
!===============================================================
!
      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!
! praut: auto conversion rate from cloud to rain  [LH 9] [CP 17]
!        (QC->QR)
!
          lencon  = 2.7e-2*den(i,k)*qci(i,k,1)*(1.e20/16.*rslopec2(i,k)        &
                   *rslopec2(i,k)-0.4)
          lenconcr = max(1.2*lencon, qcrmin)
          if(qci(i,k,1).gt.qcr(i,k)) then
            praut(i,k) = qck1*qci(i,k,1)**(7./3.)*ncr(i,k,2)**(-1./3.)
            praut(i,k) = min(praut(i,k),qci(i,k,1)/dtcld)
!
! nraut: auto conversion rate from cloud to rain [LH A6] [CP 18 & 19]
!        (NC->NR)
!
            nraut(i,k) = 3.5e9*den(i,k)*praut(i,k)
            if(qrs(i,k,1).gt.lenconcr)                                         &
            nraut(i,k) = ncr(i,k,3)/qrs(i,k,1)*praut(i,k)
            nraut(i,k) = min(nraut(i,k),ncr(i,k,2)/dtcld)
          endif
!
! pracw: accretion of cloud water by rain     [LH 10] [CP 22 & 23]
!        (QC->QR)
! nracw: accretion of cloud water by rain     [LH A9]
!        (NC->)
!
          if(qrs(i,k,1).ge.lenconcr) then
            if(avedia(i,k,2).ge.di100) then
              nracw(i,k) = min(ncrk1*ncr(i,k,2)*ncr(i,k,3)*(rslopec3(i,k)      &
                         + 24.*rslope3(i,k,1)),ncr(i,k,2)/dtcld)
              pracw(i,k) = min(pi/6.*(denr/den(i,k))*ncrk1*ncr(i,k,2)          &
                         *ncr(i,k,3)*rslopec3(i,k)*(2.*rslopec3(i,k)           &
                         + 24.*rslope3(i,k,1)),qci(i,k,1)/dtcld)   
            else
              nracw(i,k) = min(ncrk2*ncr(i,k,2)*ncr(i,k,3)*(2.*rslopec3(i,k)   &
                         *rslopec3(i,k)+5040.*rslope3(i,k,1)                   &
                         *rslope3(i,k,1)),ncr(i,k,2)/dtcld)
              pracw(i,k) = min(pi/6.*(denr/den(i,k))*ncrk2*ncr(i,k,2)          &
                         *ncr(i,k,3)*rslopec3(i,k)*(6.*rslopec3(i,k)           &     
                         *rslopec3(i,k)+5040.*rslope3(i,k,1)*rslope3(i,k,1))   & 
                         ,qci(i,k,1)/dtcld)
            endif
          endif 
!
! nccol: self collection of cloud water             [LH A8] [CP 24 & 25]     
!        (NC->)
!
          if(avedia(i,k,1).ge.di100) then
            nccol(i,k) = ncrk1*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)
          else
            nccol(i,k) = 2.*ncrk2*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)        &     
                         *rslopec3(i,k)
          endif
!
! nrcol: self collection of rain-drops and break-up [LH A21] [CP 24 & 25]
!        (NR->)
!
          if(qrs(i,k,1).ge.lenconcr) then
            if(avedia(i,k,2).lt.di100) then 
              nrcol(i,k) = 5040.*ncrk2*ncr(i,k,3)*ncr(i,k,3)*rslope3(i,k,1)    &
                          *rslope3(i,k,1)
            elseif(avedia(i,k,2).ge.di100 .and. avedia(i,k,2).lt.di600) then
              nrcol(i,k) = 24.*ncrk1*ncr(i,k,3)*ncr(i,k,3)*rslope3(i,k,1)
            elseif(avedia(i,k,2).ge.di600 .and. avedia(i,k,2).lt.di2000) then
              coecol = -2.5e3*(avedia(i,k,2)-di600) 
              nrcol(i,k) = 24.*exp(coecol)*ncrk1*ncr(i,k,3)*ncr(i,k,3)         &
                         *rslope3(i,k,1)
            else
              nrcol(i,k) = 0.
            endif
          endif
!
! prevp: evaporation/condensation rate of rain   [HL A41]  
!        (QV->QR or QR->QV)
!
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*ncr(i,k,3)*(precr1*rslope(i,k,1)       &
                         + precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
!
! Nrevp: evaporation/condensation rate of rain   [LH A14] 
!        (NR->NCCN) 
!
              if(prevp(i,k).eq.-qrs(i,k,1)/dtcld) then
                ncr(i,k,1) = ncr(i,k,1)+ncr(i,k,3)
                ncr(i,k,3) = 0.
              endif
            else
!
              prevp(i,k) = min(prevp(i,k),satdt/2)
            endif
          endif
        enddo
      enddo
!
!===============================================================
!
! cold rain processes
!
! - follows the revised ice microphysics processes in HDC
! - the processes same as in RH83 and RH84  and LFO behave
!   following ice crystal hapits defined in HDC, inclduing
!   intercept parameter for snow (n0s), ice crystal number
!   concentration (ni), ice nuclei number concentration
!   (n0i), ice diameter (d)
!
!===============================================================
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!
! Ni: ice crystal number concentraiton   [HDC 5c]
!
!         xni(i,k) = min(max(5.38e7*(den(i,k)                                  &
!                      *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
          eacrs = exp(0.07*(-supcol))
!
          xmi = den(i,k)*qci(i,k,2)/xni(i,k)
          diameter  = min(dicon * sqrt(xmi),dimax)
          vt2i = 1.49e4*diameter**1.31
          vt2r=pvtr*rslopeb(i,k,1)*denfac(i,k)
          vt2s=pvts*rslopeb(i,k,2)*denfac(i,k)
          vt2g=pvtg*rslopeb(i,k,3)*denfac(i,k)
          vt2h=pvth*rslopeb(i,k,4)*denfac(i,k)
          qsum(i,k) = max((qrs(i,k,2)+qrs(i,k,3)),1.e-15)
          if(qsum(i,k) .gt. 1.e-15) then
            vt2ave=(vt2s*qrs(i,k,2)+vt2g*qrs(i,k,3))/(qsum(i,k))          
          else    
            vt2ave=0.
          endif
          if(supcol.gt.0. .and. qci(i,k,2).gt.qmin) then
            if(qrs(i,k,1).gt.qcrmin) then
!
! praci: Accretion of cloud ice by rain [HL A15] [LFO 25]
!        (T<T0: QI->QR)
!
              acrfac = 6.*rslope2(i,k,1)+4.*diameter*rslope(i,k,1) + diameter**2
              praci(i,k) = pi*qci(i,k,2)*ncr(i,k,3)*abs(vt2r-vt2i)*acrfac/4.
              ! reduce collection efficiency (suggested by B. Wilt)
              praci(i,k) = praci(i,k)*min(max(0.0,qrs(i,k,1)/qci(i,k,2)),1.)**2
              praci(i,k) = min(praci(i,k),qci(i,k,2)/dtcld)
!
! piacr: Accretion of rain by cloud ice [HL A19] [LFO 26]
!        (T<T0: QR->QS or QR->QG)
!
              piacr(i,k) = pi*pi*avtr*ncr(i,k,3)*denr*xni(i,k)*denfac(i,k)     &
                          *g7pbr*rslope3(i,k,1)*rslope2(i,k,1)*rslopeb(i,k,1)  &
                          /24./den(i,k)
              ! reduce collection efficiency (suggested by B. Wilt)
              piacr(i,k) = piacr(i,k)*min(max(0.0,qci(i,k,2)/qrs(i,k,1)),1.)**2
              piacr(i,k) = min(piacr(i,k),qrs(i,k,1)/dtcld)
            endif
!
! niacr: Accretion of rain by cloud ice  [LH A25]
!        (T<T0: NR->) 
!
            if(ncr(i,k,3).gt.nrmin) then
              niacr(i,k) = pi*avtr*ncr(i,k,3)*xni(i,k)*denfac(i,k)*g4pbr       &
                          *rslope2(i,k,1)*rslopeb(i,k,1)/4.
              ! reduce collection efficiency (suggested by B. Wilt)
              niacr(i,k) = niacr(i,k)*min(max(0.0,qci(i,k,2)/qrs(i,k,1)),1.)**2
              niacr(i,k) = min(niacr(i,k),ncr(i,k,3)/dtcld)
            endif
!
! psaci: Accretion of cloud ice by snow [HDC 10]
!        (T<T0: QI->QS)
!
            if(qrs(i,k,2).gt.qcrmin) then
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      + diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                          *abs(vt2ave-vt2i)*acrfac/4.
              psaci(i,k) = min(psaci(i,k),qci(i,k,2)/dtcld)
            endif
!
! pgaci: Accretion of cloud ice by graupel [HL A17] [LFO 41]
!        (T<T0: QI->QG)
!
            if(qrs(i,k,3).gt.qcrmin) then
              egi = exp(0.07*(-supcol))
              acrfac = 2.*rslope3(i,k,3)+2.*diameter*rslope2(i,k,3)            &
                      + diameter**2*rslope(i,k,3)
              pgaci(i,k) = pi*egi*qci(i,k,2)*n0g*abs(vt2ave-vt2i)*acrfac/4.
              pgaci(i,k) = min(pgaci(i,k),qci(i,k,2)/dtcld)
            endif
!
! phaci: Accretion of cloud ice by hail [BHT]
!        (T<T0: QI->QH)
!
            if(qrs(i,k,4).gt.qcrmin) then
              ehi = exp(0.07*(-supcol))
              acrfac = 2.*rslope3(i,k,4)+2.*diameter*rslope2(i,k,4)            &
                      + diameter**2*rslope(i,k,4)
              phaci(i,k) = pi*ehi*qci(i,k,2)*n0h*abs(vt2h-vt2i)*acrfac/4.
              phaci(i,k) = min(phaci(i,k),qci(i,k,2)/dtcld)
            endif
          endif
!
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: QC->QS, and T>=T0: QC->QR)
!
          if(qrs(i,k,2).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)   &
              ! reduce collection efficiency (suggested by B. Wilt)
                        *min(max(0.0,qrs(i,k,2)/qci(i,k,1)),1.)**2             &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!
! nsacw: Accretion of cloud water by snow  [LH A12]
!        (NC ->) 
!
         if(qrs(i,k,2).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
           nsacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)    &
              ! reduce collection efficiency (suggested by B. Wilt)
                       *min(max(0.0,qrs(i,k,2)/qci(i,k,1)),1.)**2              &
                       *ncr(i,k,2)*denfac(i,k),ncr(i,k,2)/dtcld)
         endif
!
! pgacw: Accretion of cloud water by graupel [HL A6] [LFO 40]
!        (T<T0: QC->QG, and T>=T0: QC->QR)
!
          if(qrs(i,k,3).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            pgacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)*qci(i,k,1)    &
              ! reduce collection efficiency (suggested by B. Wilt)
                        *min(max(0.0,qrs(i,k,3)/qci(i,k,1)),1.)**2             &
                        *denfac(i,k),qci(i,k,1)/dtcld)
          endif
!
! ngacw: Accretion of cloud water by graupel [LH A13]
!        (NC-> 
!
          if(qrs(i,k,3).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
            ngacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)*ncr(i,k,2)    &
              ! reduce collection efficiency (suggested by B. Wilt)
                        *min(max(0.0,qrs(i,k,3)/qci(i,k,1)),1.)**2             &
                        *denfac(i,k),ncr(i,k,2)/dtcld)
          endif
!
! paacw: Accretion of cloud water by averaged snow/graupel 
!        (T<T0: QC->QG or QS, and T>=T0: QC->QR) 
!
          if(qsum(i,k) .gt. 1.e-15) then
            paacw(i,k) = (qrs(i,k,2)*psacw(i,k)+qrs(i,k,3)*pgacw(i,k))/(qsum(i,k))
!
! naacw: Accretion of cloud water by averaged snow/graupel
!        (Nc->)
!
            naacw(i,k) = (qrs(i,k,2)*nsacw(i,k)+qrs(i,k,3)*ngacw(i,k))/(qsum(i,k))
          endif      
!
! phacw: Accretion of cloud water by hail [BHT A08]
!        (T<T0: QC->QH, and T>=T0: QC->QR)
!
          if(qrs(i,k,4).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            phacw(i,k) = min(pacrh*rslope3(i,k,4)*rslopeb(i,k,4)*qci(i,k,1)    &
              ! reduce collection efficiency (suggested by B. Wilt)
                        *min(max(0.0,qrs(i,k,4)/qci(i,k,1)),1.)**2             &
                        *denfac(i,k),qci(i,k,1)/dtcld)
          endif
!
! nhacw: Accretion of cloud water by hail
!        (NC->) 
!
          if(qrs(i,k,4).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
            nhacw(i,k) = min(pacrh*rslope3(i,k,4)*rslopeb(i,k,4)*ncr(i,k,2)    &
              ! reduce collection efficiency (suggested by B. Wilt)
                        *min(max(0.0,qrs(i,k,4)/qci(i,k,1)),1.)**2             &
                        *denfac(i,k),ncr(i,k,2)/dtcld)
          endif
!
! pracs: Accretion of snow by rain [HL A11] [LFO 27]
!         (T<T0: QS->QG)
!
          if(qrs(i,k,2).gt.qcrmin .and. qrs(i,k,1).gt.qcrmin) then
            if(supcol.gt.0) then
              acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)                        &
                      + 4.*rslope3(i,k,2)*rslope2(i,k,2)*rslope(i,k,1)         &
                      + 1.5*rslope2(i,k,2)*rslope2(i,k,2)*rslope2(i,k,1)
              pracs(i,k) = pi*pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2r-vt2ave)   &
                          *(dens/den(i,k))*acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
              pracs(i,k) = pracs(i,k)*min(max(0.0,qrs(i,k,1)/qrs(i,k,2)),1.)**2
              pracs(i,k) = min(pracs(i,k),qrs(i,k,2)/dtcld)
            endif
!
! psacr: Accretion of rain by snow [HL A10] [LFO 28]
!         (T<T0:QR->QS or QR->QG) (T>=T0: enhance melting of snow)
!
            acrfac = 30.*rslope3(i,k,1)*rslope2(i,k,1)*rslope(i,k,2)           &
                     +10.*rslope2(i,k,1)*rslope2(i,k,1)*rslope2(i,k,2)         & 
                     + 2.*rslope3(i,k,1)*rslope3(i,k,2)
            psacr(i,k) = pi*pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2ave-vt2r)     &
                        *(denr/den(i,k))*acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            psacr(i,k) = psacr(i,k)*min(max(0.0,qrs(i,k,2)/qrs(i,k,1)),1.)**2
            psacr(i,k) = min(psacr(i,k),qrs(i,k,1)/dtcld)
          endif
          if(qrs(i,k,2).gt.qcrmin .and. ncr(i,k,3).gt.nrmin) then
!
! nsacr: Accretion of rain by snow  [LH A23] 
!       (T<T0:NR->)
!
            acrfac = 1.5*rslope2(i,k,1)*rslope(i,k,2)                          &
                    + 1.0*rslope(i,k,1)*rslope2(i,k,2)+.5*rslope3(i,k,2)        
            nsacr(i,k) = pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2ave-vt2r)        &
                        *acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            nsacr(i,k) = nsacr(i,k)*min(max(0.0,qrs(i,k,2)/qrs(i,k,1)),1.)**2
            nsacr(i,k) = min(nsacr(i,k),ncr(i,k,3)/dtcld)
          endif
!
! pracg: Accretion of graupel by rain [BHT A17]
!         (T<T0: QG->QH)
!
          if(qrs(i,k,3).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            if(supcol.gt.0) then
              acrfac = 5.*rslope3(i,k,3)*rslope3(i,k,3)                        &
                      +4.*rslope3(i,k,3)*rslope2(i,k,3)*rslope(i,k,1)          &
                      +1.5*rslope2(i,k,3)*rslope2(i,k,3)*rslope2(i,k,1)
              pracg(i,k) = pi*pi*ncr(i,k,3)*n0g*abs(vt2r-vt2ave)               &
                          *(deng/den(i,k))*acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
              pracg(i,k) = pracg(i,k)*min(max(0.0,qrs(i,k,1)/qrs(i,k,3)),1.)**2
              pracg(i,k) = min(pracg(i,k),qrs(i,k,3)/dtcld)
            endif
!
! pgacr: Accretion of rain by graupel [HL A12] [LFO 42]
!         (T<T0: QR->QG) (T>=T0: enhance melting of graupel)
!
            acrfac = 30.*rslope3(i,k,1)*rslope2(i,k,1)*rslope(i,k,3)           &
                    +10.*rslope2(i,k,1)*rslope2(i,k,1)*rslope2(i,k,3)          & 
                    + 2.*rslope3(i,k,1)*rslope3(i,k,3)
            pgacr(i,k) = pi*pi*ncr(i,k,3)*n0g*abs(vt2ave-vt2r)*(denr/den(i,k)) &
                        *acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            pgacr(i,k) = pgacr(i,k)*min(max(0.0,qrs(i,k,3)/qrs(i,k,1)),1.)**2
            pgacr(i,k) = min(pgacr(i,k),qrs(i,k,1)/dtcld)
          endif
!
! ngacr: Accretion of rain by graupel  [LH A24] 
!        (T<T0: NR->) 
!
          if(qrs(i,k,3).gt.qcrmin .and. ncr(i,k,3).gt.nrmin) then
            acrfac = 1.5*rslope2(i,k,1)*rslope(i,k,3)                          &
                    + 1.0*rslope(i,k,1)*rslope2(i,k,3) + .5*rslope3(i,k,3)   
            ngacr(i,k) = pi*ncr(i,k,3)*n0g*abs(vt2ave-vt2r)*acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            ngacr(i,k) = ngacr(i,k)*min(max(0.0,qrs(i,k,3)/qrs(i,k,1)),1.)**2
            ngacr(i,k) = min(ngacr(i,k),ncr(i,k,3)/dtcld)
          endif
!
! pgacs: Accretion of snow by graupel [HL A13] [LFO 29]
!        (QS->QG) : This process is eliminated in V3.0 with the
!        new combined snow/graupel fall speeds
!
          if(qrs(i,k,3).gt.qcrmin .and. qrs(i,k,2).gt.qcrmin) then
            pgacs(i,k) = 0. 
          endif
!
! phacr: Accretion of rain by hail [BHT A13]
!         (T<T0: QR->QH) (T>=T0: enhance melting of hail)
!
          if(qrs(i,k,4).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            acrfac = 30.*rslope3(i,k,1)*rslope2(i,k,1)*rslope(i,k,4)           &
                    +10.*rslope3(i,k,1)*rslope(i,k,1)*rslope2(i,k,4)           &
                    + 2.*rslope3(i,k,1)*rslope3(i,k,4)
            phacr(i,k) = pi*pi*ncr(i,k,3)*n0h*abs(vt2h-vt2r)*(denr/den(i,k))   &
                        *acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            phacr(i,k) = phacr(i,k)*min(max(0.0,qrs(i,k,4)/qrs(i,k,1)),1.)**2
            phacr(i,k) = min(phacr(i,k),qrs(i,k,1)/dtcld)
          endif
!
! nhacr: Accretion of rain by hail
!        (T<T0: NR->) 
!
          if(qrs(i,k,4).gt.qcrmin .and. ncr(i,k,3).gt.nrmin) then
            acrfac = 1.5*rslope2(i,k,1)*rslope(i,k,4)                          &
                    + 1.0*rslope(i,k,1)*rslope2(i,k,4) + .5*rslope3(i,k,4)
            nhacr(i,k) = pi*ncr(i,k,3)*n0h*abs(vt2h-vt2r)*acrfac
              ! reduce collection efficiency (suggested by B. Wilt)
            nhacr(i,k) = nhacr(i,k)*min(max(0.0,qrs(i,k,4)/qrs(i,k,1)),1.)**2
            nhacr(i,k) = min(nhacr(i,k),ncr(i,k,3)/dtcld)
          endif
!
! phacs: Accretion of snow by hail [BHT A14] 
!         (T<T0: QS->QH) 
!
          if(qrs(i,k,4).gt.qcrmin.and.qrs(i,k,2).gt.qcrmin) then
            acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)*rslope(i,k,4)            &
                    +2.*rslope3(i,k,2)*rslope2(i,k,2)*rslope2(i,k,4)           &
                    +.5*rslope2(i,k,2)*rslope2(i,k,2)*rslope3(i,k,4)
            phacs(i,k) = pi**2*eachs*n0s*n0sfac(i,k)*n0h*abs(vt2h-vt2ave)      &
                        *(dens/den(i,k))*acrfac
            phacs(i,k) = min(phacs(i,k),qrs(i,k,2)/dtcld)
          endif
!
! phacg: Accretion of snow by hail [BHT A15]
!         (T<T0: QG->QH) 
!
          if(qrs(i,k,4).gt.qcrmin.and.qrs(i,k,3).gt.qcrmin) then
            acrfac = 5.*rslope3(i,k,3)*rslope3(i,k,3)*rslope(i,k,4)            &
                    +2.*rslope3(i,k,3)*rslope2(i,k,3)*rslope2(i,k,4)           &
                    +.5*rslope2(i,k,3)*rslope2(i,k,3)*rslope3(i,k,4)
            phacg(i,k) = pi**2*eachg*n0g*n0h*abs(vt2h-vt2ave)                  &
                        *(deng/den(i,k))*acrfac
            phacg(i,k) = min(phacg(i,k),qrs(i,k,3)/dtcld)
          endif
!
! pgwet: wet growth of graupel [LFO 43]
!
          rs0 = psat*exp(log(ttp/t0c)*xa)*exp(xb*(1.-ttp/t0c))
          rs0 = min(rs0,0.99*p(i,k))
          rs0 = ep2*rs0/(p(i,k)-rs0)
          rs0 = max(rs0,qmin)
          ghw1 = den(i,k)*hvap*diffus(t(i,k),p(i,k))*(rs0-q(i,k))              &
               - xka(t(i,k),den(i,k))*(-supcol)
          ghw2 = den(i,k)*(xlf0+cliq*(-supcol))
          ghw3 = venfac(p(i,k),t(i,k),den(i,k))*sqrt(sqrt(g*den(i,k)/den0))
          ghw4 = den(i,k)*(xlf0-cliq*supcol+cice*supcol)
          if(qrs(i,k,3).gt.qcrmin) then
            if(pgaci(i,k).gt.0.0) then
              egi = exp(0.07*(-supcol))
              pgaci_w(i,k) = pgaci(i,k)/egi
            else
              pgaci_w(i,k) = 0.0
            endif
            pgwet(i,k) = ghw1/ghw2*(precg1*rslope2(i,k,3)                      &
                        +precg3*ghw3*rslope(i,k,4)**(2.75)                     &
                        +ghw4*(pgaci_w(i,k)+pgacs(i,k)))
            pgwet(i,k) = max(pgwet(i,k), 0.0)
          endif
!
! phwet: wet growth of hail [LFO 43] 
!
          if(qrs(i,k,4).gt.qcrmin) then
            if(phaci(i,k).gt.0.0) then
              ehi = exp(0.07*(-supcol))
              phaci_w(i,k) = phaci(i,k)/ehi
            else
              phaci_w(i,k) = 0.0
            endif
          endif
          phwet(i,k) = ghw1/ghw2*(prech1*rslope2(i,k,4)                        &
                      +prech3*ghw3*rslope(i,k,4)**(2.75)                       &
                      +ghw4*(phaci_w(i,k)+phacs(i,k)))
          phwet(i,k) = max(phwet(i,k), 0.0)
          if(supcol.le.0) then
            xlf = xlf0
!
! pseml: Enhanced melting of snow by accretion of water [HL A34]
!        (T>=T0: QS->QR)
!
            if(qrs(i,k,2).gt.0.)                                               & 
              pseml(i,k) = min(max(cliq*supcol*(paacw(i,k)+psacr(i,k))         &
                          /xlf,-qrs(i,k,2)/dtcld),0.)
!
! nseml: Enhanced melting of snow by accretion of water    [LH A29]
!        (T>=T0: ->NR)
!
              if  (qrs(i,k,2).gt.qcrmin) then
                sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                nseml(i,k) = -sfac*pseml(i,k)
              endif
!
! pgeml: Enhanced melting of graupel by accretion of water [HL A24] [RH84 A21-A22]
!        (T>=T0: QG->QR)
!
            if(qrs(i,k,3).gt.0.)                                               &
              pgeml(i,k) = min(max(cliq*supcol*(paacw(i,k)+pgacr(i,k))/xlf     &
                          ,-qrs(i,k,3)/dtcld),0.)
!
! ngeml: Enhanced melting of graupel by accretion of water [LH A30] 
!         (T>=T0: -> NR)
!
              if (qrs(i,k,3).gt.qcrmin) then
                gfac = rslope(i,k,3)*n0g/qrs(i,k,3)
                ngeml(i,k) = -gfac*pgeml(i,k)
              endif
!
! pheml: Enhanced melting of hail by accretion of water [BHT A23]
!        (T>=T0: QH->QR)
!
            if(qrs(i,k,4).gt.0.)                                               &
              pheml(i,k) = min(max(cliq*supcol*(phacw(i,k)+phacr(i,k))/xlf     &
                          ,-qrs(i,k,4)/dtcld),0.)
!
! nheml: Enhanced melting of hail by accretion of water [LH A30] 
!         (T>=T0: -> NR)
!
              if (qrs(i,k,4).gt.qcrmin) then
                gfac = rslope(i,k,4)*n0h/qrs(i,k,4)
                nheml(i,k) = -gfac*pheml(i,k)
              endif
          endif
          if(supcol.gt.0) then
!
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: QV->QI or QI->QV)
!
            if(qci(i,k,2).gt.0. .and. ifsat.ne.1) then
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
                pidep(i,k) = max(max(pidep(i,k),satdt/2),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)/dtcld)
              else
                pidep(i,k) = min(min(pidep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (T<T0: QV->QS or QS->QV)
!
            if(qrs(i,k,2).gt.0. .and. ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k,2)   &
                           + precs2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)/dtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt/2),supice)
              else
                psdep(i,k) = min(min(psdep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!
! pgdep: deposition/sublimation rate of graupel [HL A21] [LFO 46]
!        (T<T0: QV->QG or QG->QV)
!
            if(qrs(i,k,3).gt.0. .and. ifsat.ne.1) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgdep(i,k) = (rh(i,k,2)-1.)*(precg1*rslope2(i,k,3)               &
                          + precg2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              if(pgdep(i,k).lt.0.) then
                pgdep(i,k) = max(pgdep(i,k),-qrs(i,k,3)/dtcld)
                pgdep(i,k) = max(max(pgdep(i,k),satdt/2),supice)
              else
                pgdep(i,k) = min(min(pgdep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)+pgdep(i,k)).ge.          &
                abs(satdt)) ifsat = 1
            endif
!
! phdep: deposition/sublimation rate of hail  [BHT A19]
!        (T<T0: QV->QH or QH->QV)
!
            if(qrs(i,k,4).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,4)*sqrt(rslope(i,k,4)*rslopeb(i,k,4))
              phdep(i,k) = (rh(i,k,2)-1.)*(prech1*rslope2(i,k,4)               &
                              +prech2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)-pgdep(i,k)
              if(phdep(i,k).lt.0.) then
                phdep(i,k) = max(phdep(i,k),-qrs(i,k,4)/dtcld)
                phdep(i,k) = max(max(phdep(i,k),satdt/2),supice)
              else
                phdep(i,k) = min(min(phdep(i,k),satdt/2),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)+pgdep(i,k)+phdep(i,k))   &
                .ge. abs(satdt)) ifsat = 1
            endif
!
! pigen: generation(nucleation) of ice from vapor [HL 50] [HDC 7-8]
!       (T<T0: QV->QI)
!
            if(supsat.gt.0. .and. ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)-pgdep(i,k)       &
                      -phdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*xni0**1.33
              pigen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))/dtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!        (T<T0: QI->QS)
!
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
            endif
!
! pgaut: conversion(aggregation) of snow to graupel [HL A4] [LFO 37]
!        (T<T0: QS->QG)
!
            if(qrs(i,k,2).gt.0.) then
              alpha2 = 1.e-3*exp(0.09*(-supcol))
              pgaut(i,k) = min(max(0.,alpha2*(qrs(i,k,2)-qs0)),qrs(i,k,2)/dtcld)
            endif
          endif
!
! phaut: conversion(aggregation) of grauple to hail [BHT A18]
!        (T<T0: QG->QH)
!
            if(qrs(i,k,3).gt.0.) then
              alpha2 = 1.e-3*exp(0.09*(-supcol))
              phaut(i,k) = min(max(0.,alpha2*(qrs(i,k,3)-qs0)),qrs(i,k,3)/dtcld)
            endif
!
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>=T0: QS->QV)
!
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0. .and. rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psevp(i,k) = (rh(i,k,1)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k,2)   &
                           +precs2*work2(i,k)*coeres)/work1(i,k,1)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0.)
            endif
!
! pgevp: Evaporation of melting graupel [HL A25] [RH84 A19]
!       (T>=T0: QG->QV)
!
            if(qrs(i,k,3).gt.0. .and. rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgevp(i,k) = (rh(i,k,1)-1.)*(precg1*rslope2(i,k,3)               &
                         + precg2*work2(i,k)*coeres)/work1(i,k,1)
              pgevp(i,k) = min(max(pgevp(i,k),-qrs(i,k,3)/dtcld),0.)
            endif
!
! phevp: Evaporation of melting hail [BHT A20]
!       (T>=T0: QH->QV)
!
            if(qrs(i,k,4).gt.0..and.rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,4)*sqrt(rslope(i,k,4)*rslopeb(i,k,4))
              phevp(i,k) = (rh(i,k,1)-1.)*(prech1*rslope2(i,k,4)               &
                         +prech2*work2(i,k)*coeres)/work1(i,k,1)
              phevp(i,k) = min(max(phevp(i,k),-qrs(i,k,4)/dtcld),0.)
            endif
          endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
        do i = its, ite
!
          delta2=0.
          delta3=0.
          if(qrs(i,k,1).lt.1.e-4 .and. qrs(i,k,2).lt.1.e-4) delta2=1.
          if(qrs(i,k,1).lt.1.e-4) delta3=1.
          if(t(i,k).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+paacw(i,k)+paacw(i,k)+phacw(i,k))  &
                    *dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              phacw(i,k) = phacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)-pigen(i,k)-pidep(i,k)+praci(i,k)+psaci(i,k)   &
                    +pgaci(i,k)+phaci(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
              phaci(i,k) = phaci(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-prevp(i,k)-pracw(i,k)+piacr(i,k)             &
                    +psacr(i,k)+pgacr(i,k)+phacr(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              phacr(i,k) = phacr(i,k)*factor
            endif
!
!     snow
!
            value = max(qmin,qrs(i,k,2))
            source = -(psdep(i,k)+psaut(i,k)-pgaut(i,k)+paacw(i,k)             &
                     +piacr(i,k)*delta3+praci(i,k)*delta3                      &
                     +pvapg(i,k)+pvaph(i,k)                                    &
                     -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2                 &
                     +psaci(i,k)-pgacs(i,k)-phacs(i,k) )*dtcld
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              pgaut(i,k) = pgaut(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
              phacs(i,k) = phacs(i,k)*factor
              pvapg(i,k) = pvapg(i,k)*factor
              pvaph(i,k) = pvaph(i,k)*factor
            endif
!
!     graupel
!
            value = max(qmin,qrs(i,k,3))
            source = -(pgdep(i,k)+pgaut(i,k)                                   &
                     +piacr(i,k)*(1.-delta3)+praci(i,k)*(1.-delta3)            &
                     +psacr(i,k)*(1.-delta2)+pracs(i,k)*(1.-delta2)            &
                     +pgaci(i,k)+paacw(i,k)+pgacr(i,k)*delta2+pgacs(i,k)       &
                     -pracg(i,k)*(1.-delta2)-phacg(i,k)-phaut(i,k)             &
                     -pvapg(i,k)+primh(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgdep(i,k) = pgdep(i,k)*factor
              pgaut(i,k) = pgaut(i,k)*factor
              phaut(i,k) = phaut(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
              pracg(i,k) = pracg(i,k)*factor
              phacg(i,k) = phacg(i,k)*factor
              pvapg(i,k) = pvapg(i,k)*factor
              primh(i,k) = primh(i,k)*factor
            endif
!
!     hail
!
            value = max(qmin,qrs(i,k,4))
            source = -(phdep(i,k)+phaut(i,k)                                   &
                      +pgacr(i,k)*(1.-delta2)+pracg(i,k)*(1.-delta2)           &
                      +phacw(i,k)+phacr(i,k)+phaci(i,k)+phacs(i,k)             &
                      +phacg(i,k)-pvaph(i,k)-primh(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              phdep(i,k) = phdep(i,k)*factor
              phaut(i,k) = phaut(i,k)*factor
              pracg(i,k) = pracg(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              phacw(i,k) = phacw(i,k)*factor
              phaci(i,k) = phaci(i,k)*factor
              phacr(i,k) = phacr(i,k)*factor
              phacs(i,k) = phacs(i,k)*factor
              phacg(i,k) = phacg(i,k)*factor
              pvaph(i,k) = pvaph(i,k)*factor
              primh(i,k) = primh(i,k)*factor
            endif
!
!     cloud
!
            value = max(ncmin,ncr(i,k,2))
            source = (nraut(i,k)+nccol(i,k)+nracw(i,k)                         &
                    +naacw(i,k)+naacw(i,k)+nhacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              naacw(i,k) = naacw(i,k)*factor
              nhacw(i,k) = nhacw(i,k)*factor
            endif
!
!     rain
!
            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)+nrcol(i,k)+niacr(i,k)+nsacr(i,k)+ngacr(i,k)  &
                     +nhacr(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              niacr(i,k) = niacr(i,k)*factor
              nsacr(i,k) = nsacr(i,k)*factor
              ngacr(i,k) = ngacr(i,k)*factor
              nhacr(i,k) = nhacr(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pgdep(i,k)+phdep(i,k)           &
                        +pigen(i,k)+pidep(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                           +paacw(i,k)+paacw(i,k)+phacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                           +prevp(i,k)-piacr(i,k)-pgacr(i,k)                   &
                           -psacr(i,k)-phacr(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+praci(i,k)                 &
                           +psaci(i,k)+pgaci(i,k)+phaci(i,k)                   &
                           -pigen(i,k)-pidep(i,k))                             &
                           *dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)+paacw(i,k)      &
                           -pgaut(i,k)+piacr(i,k)*delta3                       &
                           +praci(i,k)*delta3+psaci(i,k)-pgacs(i,k)            &
                           -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2           &
                           +pvapg(i,k)+pvaph(i,k)-phacs(i,k))                  &
                           *dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgdep(i,k)+pgaut(i,k)                 &
                           +piacr(i,k)*(1.-delta3)                             &
                           +praci(i,k)*(1.-delta3)+psacr(i,k)*(1.-delta2)      &
                           +pracs(i,k)*(1.-delta2)+pgaci(i,k)+paacw(i,k)       &
                           +pgacr(i,k)*delta2+pgacs(i,k)+primh(i,k)            &
                           -pracg(i,k)*(1.-delta2)-phacg(i,k)-phaut(i,k)       &
                           -pvapg(i,k))*dtcld,0.)
            qrs(i,k,4) = max(qrs(i,k,4)+(phdep(i,k)+phaut(i,k)                 &
                           +pgacr(i,k)*(1.-delta2)+pracg(i,k)*(1.-delta2)      &
                           +phacw(i,k)+phacr(i,k)+phaci(i,k)+phacs(i,k)        &
                           +phacg(i,k)-pvaph(i,k)-primh(i,k))                  &
                           *dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                           -naacw(i,k)-naacw(i,k)-nhacw(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)-nrcol(i,k)-niacr(i,k)      &
                           -nsacr(i,k)-ngacr(i,k)-nhacr(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pgdep(i,k)+phdep(i,k)+pidep(i,k)        &
                      +pigen(i,k))-xl(i,k)*prevp(i,k)-xlf*(piacr(i,k)          &
                      +paacw(i,k)+paacw(i,k)+phacw(i,k)                        &
                      +pgacr(i,k)+psacr(i,k)+phacr(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source= (praut(i,k)+pracw(i,k)+paacw(i,k)+paacw(i,k)-phacw(i,k))   &
                   *dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              phacw(i,k) = phacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-prevp(i,k)-praut(i,k)+pseml(i,k)+pgeml(i,k)+pheml(i,k)  &
                     -pracw(i,k)-paacw(i,k)-paacw(i,k)-phacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              phacw(i,k) = phacw(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
              pheml(i,k) = pheml(i,k)*factor
            endif
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(pgacs(i,k)+phacs(i,k)-pseml(i,k)-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              phacs(i,k) = phacs(i,k)*factor
              psevp(i,k) = psevp(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
            endif
!
!     graupel
!
            value = max(qcrmin,qrs(i,k,3))
            source=-(pgacs(i,k)+pgevp(i,k)+pgeml(i,k)-phacg(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              pgevp(i,k) = pgevp(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
              phacg(i,k) = phacg(i,k)*factor
            endif
!
!     hail
!
            value = max(qcrmin,qrs(i,k,4))
            source=-(phacs(i,k)+phacg(i,k)+phevp(i,k)+pheml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              phacs(i,k) = phacs(i,k)*factor
              phacg(i,k) = phacg(i,k)*factor
              phevp(i,k) = phevp(i,k)*factor
              pheml(i,k) = pheml(i,k)*factor
            endif

!
!     cloud
!
            value = max(ncmin,ncr(i,k,2))
            source = (nraut(i,k)+nccol(i,k)+nracw(i,k)+naacw(i,k)             &
                     +naacw(i,k)+nhacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              naacw(i,k) = naacw(i,k)*factor
              nhacw(i,k) = nhacw(i,k)*factor
            endif
!
!     rain
!
            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)+nrcol(i,k)-nseml(i,k)-ngeml(i,k)             &
                      -nheml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              nseml(i,k) = nseml(i,k)*factor
              ngeml(i,k) = ngeml(i,k)*factor
              nheml(i,k) = nheml(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psevp(i,k)+pgevp(i,k)+phevp(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                    +paacw(i,k)+paacw(i,k)+phacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                    +prevp(i,k)+paacw(i,k)+paacw(i,k)+phacw(i,k)               &
                    -pseml(i,k)-pgeml(i,k)-pheml(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psevp(i,k)-pgacs(i,k)-phacs(i,k)      &
                    +pseml(i,k))*dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgacs(i,k)+pgevp(i,k)                 &
                    +pgeml(i,k)-phacg(i,k))*dtcld,0.)
            qrs(i,k,4) = max(qrs(i,k,4)+(phacs(i,k)+phacg(i,k)+phevp(i,k)      &
                    +pheml(i,k))*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                   -naacw(i,k)-naacw(i,k)-nhacw(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)-nrcol(i,k)+nseml(i,k)      &
                           +ngeml(i,k)+nheml(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k)+pgevp(i,k)+phevp(i,k))   &
                      -xlf*(pseml(i,k)+pgeml(i,k)+pheml(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
        do i = its, ite
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          endif
          qs(i,k,2) = min(qs(i,k,2),0.99*p(i,k))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
        enddo
      enddo
!
      call slope_wdm7(qrs_tmp,ncr_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2, &
                     rslope3,work1,workn,its,ite,kts,kte)
!
      do k = kts, kte
        do i = its, ite
!
! re-compute the mean-volume drop diameter       [LH A10]
! for raindrop distribution
!
          avedia(i,k,2) = rslope(i,k,1)*((24.)**(.3333333))
!
! Nrevp_s: evaporation/condensation rate of rain   [LH A14]
!        (NR->NC)
!
          if(avedia(i,k,2).le.di82) then
            ncr(i,k,2) = ncr(i,k,2)+ncr(i,k,3)
            ncr(i,k,3) = 0.
!
! Prevp_s: evaporation/condensation rate of rain [LH A15] [KK 23]
!        (QR->QC)
!
            qci(i,k,1) = qci(i,k,1)+qrs(i,k,1)
            qrs(i,k,1) = 0.
          endif
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
!
! rate of change of cloud drop concentration due to CCN activation
! pcact: QV -> QC                                 [LH 8]  [KK 14]
! ncact: NCCN -> NC                               [LH A2] [KK 12]
!
          if(rh(i,k,1).gt.1.) then
            ncact(i,k) = max(0.,((ncr(i,k,1)+ncr(i,k,2))                       &
                       *min(1.,(rh(i,k,1)/satmax)**actk) - ncr(i,k,2)))/dtcld
            ncact(i,k) =min(ncact(i,k),max(ncr(i,k,1),0.)/dtcld)
            pcact(i,k) = min(4.*pi*denr*(actr*1.E-6)**3*ncact(i,k)/            &
                         (3.*den(i,k)),max(q(i,k),0.)/dtcld)
            q(i,k) = max(q(i,k)-pcact(i,k)*dtcld,0.)
            qci(i,k,1) = max(qci(i,k,1)+pcact(i,k)*dtcld,0.)
            ncr(i,k,1) = max(ncr(i,k,1)-ncact(i,k)*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+ncact(i,k)*dtcld,0.)
            t(i,k) = t(i,k)+pcact(i,k)*xl(i,k)/cpm(i,k)*dtcld
          endif  
!
!  pcond:condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!  (QV->QC or QC->QV)     
!
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0. .and. work1(i,k,1).lt.0.)                        & 
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
!
! ncevp: evpration of Cloud number concentration  [LH A3] 
! (NC->NCCN) 
!
          if(pcond(i,k).eq.-qci(i,k,1)/dtcld) then
            ncr(i,k,1) = ncr(i,k,1)+ncr(i,k,2)
            ncr(i,k,2) = 0.
          endif
!
          q(i,k) = q(i,k)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
        do i = its, ite
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0
          if(qrs(i,k,1).ge.qcrmin .and. ncr(i,k,3) .ge. nrmin) then
            lamdr_tmp(i,k) = exp(log(((pidnr*ncr(i,k,3))                       & 
                         /(den(i,k)*qrs(i,k,1))))*((.33333333)))
            if(lamdr_tmp(i,k) .le. lamdarmin) then
              lamdr_tmp(i,k) = lamdarmin
              ncr(i,k,3) = den(i,k)*qrs(i,k,1)*lamdr_tmp(i,k)**3/pidnr
            elseif(lamdr_tmp(i,k) .ge. lamdarmax) then
              lamdr_tmp(i,k) = lamdarmax 
              ncr(i,k,3) = den(i,k)*qrs(i,k,1)*lamdr_tmp(i,k)**3/pidnr
            endif
          endif  
          if(qci(i,k,1).ge.qmin .and. ncr(i,k,2) .ge. ncmin ) then
            lamdc_tmp(i,k) = exp(log(((pidnc*ncr(i,k,2))                       &
                         /(den(i,k)*qci(i,k,1))))*((.33333333)))
            if(lamdc_tmp(i,k) .le. lamdacmin) then
              lamdc_tmp(i,k) = lamdacmin
              ncr(i,k,2) = den(i,k)*qci(i,k,1)*lamdc_tmp(i,k)**3/pidnc
            elseif(lamdc_tmp(i,k) .ge. lamdacmax) then
              lamdc_tmp(i,k) = lamdacmax
              ncr(i,k,2) = den(i,k)*qci(i,k,1)*lamdc_tmp(i,k)**3/pidnc
            endif
          endif
        enddo
      enddo
      enddo                  ! big loops
  END SUBROUTINE wdm72d
! ...................................................................
      REAL FUNCTION rgmma(x)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!     rgmma function:  use infinite product form
      REAL :: euler
      PARAMETER (euler=0.577215664901532)
      REAL :: x, y
      INTEGER :: i
      if(x.eq.1.)then
        rgmma=0.
      else
        rgmma=x*exp(euler*x)
        do i=1,10000
          y=float(i)
          rgmma=rgmma*(1.000+x/y)*exp(-x/y)
        enddo
        rgmma=1./rgmma
      endif
      END FUNCTION rgmma
!
!--------------------------------------------------------------------------
      REAL FUNCTION fpvs(t,ice,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c)
!--------------------------------------------------------------------------
      IMPLICIT NONE
!--------------------------------------------------------------------------
      REAL t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti,         &
           xai,xbi,ttp,tr
      INTEGER ice
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      tr=ttp/t
      if(t.lt.ttp .and. ice.eq.1) then
        fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
      else
        fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END FUNCTION fpvs
!-------------------------------------------------------------------
  SUBROUTINE wdm7init(den0,denr,dens,cl,cpv, ccn0, allowed_to_read) ! RAS
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable
   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv,ccn0
   LOGICAL, INTENT(IN) :: allowed_to_read
 
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
!
   qc0  = 4./3.*pi*denr*r0**3.*xncr0/den0
   qc1  = 4./3.*pi*denr*r0**3.*xncr1/den0
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
   pidnc = pi*denr/6.
!
   bvtr1 = 1.+bvtr
   bvtr2 = 2.+bvtr
   bvtr3 = 3.+bvtr
   bvtr4 = 4.+bvtr
   bvtr5 = 5.+bvtr
   bvtr6 = 6.+bvtr
   bvtr7 = 7.+bvtr
   bvtr2o5 = 2.5+.5*bvtr
   bvtr3o5 = 3.5+.5*bvtr
   g1pbr = rgmma(bvtr1)
   g2pbr = rgmma(bvtr2)
   g3pbr = rgmma(bvtr3)
   g4pbr = rgmma(bvtr4)            ! 17.837825
   g5pbr = rgmma(bvtr5)
   g6pbr = rgmma(bvtr6)
   g7pbr = rgmma(bvtr7)
   g5pbro2 = rgmma(bvtr2o5) 
   g7pbro2 = rgmma(bvtr3o5)
   pvtr = avtr*g5pbr/24.
   pvtrn = avtr*g2pbr
   eacrr = 1.0
   pacrr = pi*n0r*avtr*g3pbr*.25*eacrr
   precr1 = 2.*pi*1.56
   precr2 = 2.*pi*.31*avtr**.5*g7pbro2
   pidn0r =  pi*denr*n0r
   pidnr = 4.*pi*denr
!
   xmmax = (dimax/dicon)**2
   roqimax = 2.08e22*dimax**8
!
   bvts1 = 1.+bvts
   bvts2 = 2.5+.5*bvts
   bvts3 = 3.+bvts
   bvts4 = 4.+bvts
   g1pbs = rgmma(bvts1)    !.8875
   g3pbs = rgmma(bvts3)
   g4pbs = rgmma(bvts4)    ! 12.0786
   g5pbso2 = rgmma(bvts2)
   pvts = avts*g4pbs/6.
   pacrs = pi*n0s*avts*g3pbs*.25
   precs1 = 4.*n0s*.65
   precs2 = 4.*n0s*.44*avts**.5*g5pbso2
   pidn0s =  pi*dens*n0s
!
   pacrc = pi*n0s*avts*g3pbs*.25*eacrc
!
   bvtg1 = 1.+bvtg
   bvtg2 = 2.5+.5*bvtg
   bvtg3 = 3.+bvtg
   bvtg4 = 4.+bvtg
   g1pbg = rgmma(bvtg1)
   g3pbg = rgmma(bvtg3)
   g4pbg = rgmma(bvtg4)
   g5pbgo2 = rgmma(bvtg2)
   g6pbgh = rgmma(2.75)
   pacrg = pi*n0g*avtg*g3pbg*.25
   pvtg = avtg*g4pbg/6.
   precg1 = 2.*pi*n0g*.78
   precg2 = 2.*pi*n0g*.31*avtg**.5*g5pbgo2
   precg3 = 2.*pi*n0g*.31*g6pbgh*sqrt(sqrt(4.*deng/3./cd))
   pidn0g =  pi*deng*n0g
!
   bvth2 = 2.5+.5*bvth
   bvth3 = 3.+bvth
   bvth4 = 4.+bvth
   g3pbh = rgmma(bvth3)
   g4pbh = rgmma(bvth4)
   g5pbho2 = rgmma(bvth2)
   pacrh = pi*n0h*avth*g3pbh*.25
   pvth = avth*g4pbh/6.
   prech1 = 2.*pi*n0h*.78
   prech2 = 2.*pi*n0h*.31*avth**.5*g5pbho2
   prech3 = 2.*pi*n0h*.31*g6pbgh*sqrt(sqrt(4.*denh/3./cd))
   pidn0h = pi*denh*n0h
!
   rslopecmax = 1./lamdacmax
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rslopegmax = 1./lamdagmax
   rslopehmax = 1./lamdahmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rslopegbmax = rslopegmax ** bvtg
   rslopehbmax = rslopehmax ** bvth
   rslopec2max = rslopecmax * rslopecmax
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rslopeg2max = rslopegmax * rslopegmax
   rslopeh2max = rslopehmax * rslopehmax
   rslopec3max = rslopec2max * rslopecmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
   rslopeg3max = rslopeg2max * rslopegmax
   rslopeh3max = rslopeh2max * rslopehmax
!+---+-----------------------------------------------------------------+
!..Set these variables needed for computing radar reflectivity.  These
!.. get used within radar_init to create other variables used in the
!.. radar module.
   xam_r = PI*denr/6.
   xbm_r = 3.
   xmu_r = 1.
   xam_s = PI*dens/6.
   xbm_s = 3.
   xmu_s = 0.
   xam_g = PI*deng/6.
   xbm_g = 3.
   xmu_g = 0.

   call radar_init
!+---+-----------------------------------------------------------------+
!
  END SUBROUTINE wdm7init
!-------------------------------------------------------------------------------
      subroutine slope_wdm7(qrs,ncr,den,denfac,t,rslope,rslopeb,rslope2,rslope3, &
                            vt,vtn,its,ite,kts,kte)
!-------------------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte,4) ::                                     &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, & 
                                                                           vt
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          ncr, & 
                                                                          vtn, & 
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar, lamdas, lamdag, lamdah, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
!
!  Optimizatin : A**B => exp(log(A)*(B))
      lamdar(x,y,z)= exp(log(((pidnr*z)/(x*y)))*((.33333333)))
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
      lamdah(x,y)=   sqrt(sqrt(pidn0h/(x*y)))      ! (pidn0h/(x*y))**.25
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(qrs(i,k,1).le.qcrmin .or. ncr(i,k).le.nrmin ) then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = min(1./lamdar(qrs(i,k,1),den(i,k),ncr(i,k)),1.e-3)
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin) then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k),n0sfac(i,k))
            rslopeb(i,k,2) = rslope(i,k,2)**bvts
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
          if(qrs(i,k,3).le.qcrmin) then
            rslope(i,k,3) = rslopegmax
            rslopeb(i,k,3) = rslopegbmax
            rslope2(i,k,3) = rslopeg2max
            rslope3(i,k,3) = rslopeg3max
          else
            rslope(i,k,3) = 1./lamdag(qrs(i,k,3),den(i,k))
            rslopeb(i,k,3) = rslope(i,k,3)**bvtg
            rslope2(i,k,3) = rslope(i,k,3)*rslope(i,k,3)
            rslope3(i,k,3) = rslope2(i,k,3)*rslope(i,k,3)
          endif
          if(qrs(i,k,4).le.qcrmin) then
            rslope(i,k,4) = rslopehmax
            rslopeb(i,k,4) = rslopehbmax
            rslope2(i,k,4) = rslopeh2max
            rslope3(i,k,4) = rslopeh3max
          else
            rslope(i,k,4) = 1./lamdah(qrs(i,k,4),den(i,k))
            rslopeb(i,k,4) = rslope(i,k,4)**bvth
            rslope2(i,k,4) = rslope(i,k,4)*rslope(i,k,4)
            rslope3(i,k,4) = rslope2(i,k,4)*rslope(i,k,4)
          endif

          vt(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)
          vt(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)
          vt(i,k,3) = pvtg*rslopeb(i,k,3)*denfac(i,k)
          vt(i,k,4) = pvth*rslopeb(i,k,4)*denfac(i,k)
          vtn(i,k) = pvtrn*rslopeb(i,k,1)*denfac(i,k)
          if(qrs(i,k,1).le.0.0) vt(i,k,1) = 0.0 
          if(qrs(i,k,2).le.0.0) vt(i,k,2) = 0.0
          if(qrs(i,k,3).le.0.0) vt(i,k,3) = 0.0
          if(qrs(i,k,4).le.0.0) vt(i,k,4) = 0.0
          if(ncr(i,k).le.0.0) vtn(i,k) = 0.0 
        enddo
      enddo
  END subroutine slope_wdm7
!-----------------------------------------------------------------------------
      subroutine slope_rain(qrs,ncr,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,vtn,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                          ncr, & 
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          vtn, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y,z)= exp(log(((pidnr*z)/(x*y)))*((.33333333)))
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k).le.qcrmin .or. ncr(i,k).le.nrmin) then
            rslope(i,k) = rslopermax
            rslopeb(i,k) = rsloperbmax
            rslope2(i,k) = rsloper2max
            rslope3(i,k) = rsloper3max
          else
            rslope(i,k) = min(1./lamdar(qrs(i,k),den(i,k),ncr(i,k)),1.e-3)
            rslopeb(i,k) = rslope(i,k)**bvtr
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvtr*rslopeb(i,k)*denfac(i,k)
          vtn(i,k) = pvtrn*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
          if(ncr(i,k).le.0.0) vtn(i,k) = 0.0 
        enddo
      enddo
  END subroutine slope_rain
!------------------------------------------------------------------------------
      subroutine slope_snow(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdas, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopesmax
            rslopeb(i,k) = rslopesbmax
            rslope2(i,k) = rslopes2max
            rslope3(i,k) = rslopes3max
          else
            rslope(i,k) = 1./lamdas(qrs(i,k),den(i,k),n0sfac(i,k))
            rslopeb(i,k) = rslope(i,k)**bvts
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvts*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
        enddo
      enddo
  END subroutine slope_snow
!----------------------------------------------------------------------------------
      subroutine slope_graup(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdag, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
!
      do k = kts, kte
        do i = its, ite
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopegmax
            rslopeb(i,k) = rslopegbmax
            rslope2(i,k) = rslopeg2max
            rslope3(i,k) = rslopeg3max
          else
            rslope(i,k) = 1./lamdag(qrs(i,k),den(i,k))
            rslopeb(i,k) = rslope(i,k)**bvtg
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvtg*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
        enddo
      enddo
  END subroutine slope_graup
!---------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
      subroutine slope_hail(qrs,den,denfac,rslope,rslopeb,rslope2,rslope3,     &
                            vt,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          den, &
                                                                       denfac

  REAL       ::  lamdah, x, y
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdah(x,y)=   sqrt(sqrt(pidn0h/(x*y)))      ! (pidn0h/(x*y))**.25
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k).le.qcrmin)then
            rslope(i,k) = rslopehmax
            rslopeb(i,k) = rslopehbmax
            rslope2(i,k) = rslopeh2max
            rslope3(i,k) = rslopeh3max
          else
            rslope(i,k) = 1./lamdah(qrs(i,k),den(i,k))
            rslopeb(i,k) = rslope(i,k)**bvth
            rslope2(i,k) = rslope(i,k)*rslope(i,k)
            rslope3(i,k) = rslope2(i,k)*rslope(i,k)
          endif
          vt(i,k) = pvth*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
        enddo
      enddo
  END subroutine slope_hail
!------------------------------------------------------------------
!
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plmr(im,km,denl,denfacl,tkl,dzl,wwl,rql,rnl,precip,dt,id,iter,rid)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop; 2 hail
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!        rid : 1 for number 0 for mixing ratio
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  im,km,id
      real  dt
      real  dzl(im,km),wwl(im,km),rql(im,km),rnl(im,km),precip(im)
      real  denl(im,km),denfacl(im,km),tkl(im,km)
!
      integer  i,k,n,m,kk,kb,kt,iter,rid
      real  tl,tl2,qql,dql,qqd
      real  th,th2,qqh,dqh
      real  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real  allold, allnew, zz, dzamin, cflmax, decfl
      real  dz(km), ww(km), qq(km), nr(km), wd(km), wa(km), wa2(km), was(km)
      real  den(km), denfac(km), tk(km)
      real  wi(km+1), zi(km+1), za(km+1)
      real  qn(km), qr(km),tmp(km),tmp1(km),tmp2(km),tmp3(km)
      real  dza(km+1), qa(km+1), qmi(km+1), qpi(km+1)
!
      precip(:) = 0.0
!
      i_loop : do i=1,im
! -----------------------------------
      dz(:) = dzl(i,:)
      qq(:) = rql(i,:)
      nr(:) = rnl(i,:)
      if(rid .eq. 1) nr(:) = rnl(i,:)/denl(i,:)
      ww(:) = wwl(i,:)
      den(:) = denl(i,:)
      denfac(:) = denfacl(i,:)
      tk(:) = tkl(i,:)
! skip for no precipitation for all layers
      allold = 0.0
      do k=1,km
        allold = allold + qq(k)
      enddo
      if(allold.le.0.0) then
        cycle i_loop
      endif
!
! compute interface values
      zi(1)=0.0
      do k=1,km
        zi(k+1) = zi(k)+dz(k)
      enddo
!
! save departure wind
      wd(:) = ww(:)
      n=1
 100  continue
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
      wi(1) = ww(1)
      wi(km+1) = ww(km)
      do k=2,km
        wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
      enddo
! 3rd order interpolation to get wi
      fa1 = 9./16.
      fa2 = 1./16.
      wi(1) = ww(1)
      wi(2) = 0.5*(ww(2)+ww(1))
      do k=3,km-1
        wi(k) = fa1*(ww(k)+ww(k-1))-fa2*(ww(k+1)+ww(k-2))
      enddo
      wi(km) = 0.5*(ww(km)+ww(km-1))
      wi(km+1) = ww(km)
!
! terminate of top of raingroup
      do k=2,km
        if( ww(k).eq.0.0 ) wi(k)=ww(k-1)
      enddo
!
! diffusivity of wi
      con1 = 0.05
      do k=km,1,-1
        decfl = (wi(k+1)-wi(k))*dt/dz(k)
        if( decfl .gt. con1 ) then
          wi(k) = wi(k+1) - con1*dz(k)/dt
        endif
      enddo
! compute arrival point
      do k=1,km+1
        za(k) = zi(k) - wi(k)*dt
      enddo
!
      do k=1,km
        dza(k) = za(k+1)-za(k)
      enddo
      dza(km+1) = zi(km+1) - za(km+1)
!     
! computer deformation at arrival point
      do k=1,km
        qa(k) = qq(k)*dz(k)/dza(k)
        qr(k) = qa(k)/den(k)
        if(rid .eq. 1) qr(k) = qa(K)
      enddo
      qa(km+1) = 0.0
!     call maxmin(km,1,qa,' arrival points ')
!     
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        if( id.eq.1 ) then
          if(rid.eq.1) then
            call slope_rain(nr,qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,wa2,1,1,1,km)
          else
            call slope_rain(qr,nr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,wa2,1,1,1,km)
          endif
          if(rid.eq.1) wa(:) = wa2(:)
        else if(id.eq.2) then
!          print*, 'hail sedimentaion'
          call slope_hail(qr,den,denfac,tmp,tmp1,tmp2,tmp3,wa,1,1,1,km)
        endif
        if( n.ge.2 ) wa(1:km)=0.5*(wa(1:km)+was(1:km))
        do k=1,km
!#ifdef DEBUG 
!        print*,' slope_wsm3 ',qr(k)*1000.,den(k),denfac(k),tk(k),tmp(k),tmp1(k),tmp2(k),ww(k),wa(k)
!#endif
! mean wind is average of departure and new arrival winds
          ww(k) = 0.5* ( wd(k)+wa(k) )
        enddo
        was(:) = wa(:)
        n=n+1
        go to 100
      endif
!     
! estimate values at arrival cell interface with monotone
      do k=2,km
        dip=(qa(k+1)-qa(k))/(dza(k+1)+dza(k))
        dim=(qa(k)-qa(k-1))/(dza(k-1)+dza(k))
        if( dip*dim.le.0.0 ) then
          qmi(k)=qa(k)
          qpi(k)=qa(k)
        else
          qpi(k)=qa(k)+0.5*(dip+dim)*dza(k)
          qmi(k)=2.0*qa(k)-qpi(k)
          if( qpi(k).lt.0.0 .or. qmi(k).lt.0.0 ) then
            qpi(k) = qa(k)
            qmi(k) = qa(k)
          endif
        endif
      enddo
      qpi(1)=qa(1)
      qmi(1)=qa(1)
      qmi(km+1)=qa(km+1)
      qpi(km+1)=qa(km+1)
!
! interpolation to regular point
      qn = 0.0
      kb=1
      kt=1
      intp : do k=1,km
             kb=max(kb-1,1)
             kt=max(kt-1,1)
! find kb and kt
             if( zi(k).ge.za(km+1) ) then
               exit intp
             else
               find_kb : do kk=kb,km
                         if( zi(k).le.za(kk+1) ) then
                           kb = kk
                           exit find_kb
                         else
                           cycle find_kb
                         endif
               enddo find_kb
               find_kt : do kk=kt,km
                         if( zi(k+1).le.za(kk) ) then
                           kt = kk
                           exit find_kt
                         else
                           cycle find_kt
                         endif
               enddo find_kt
               kt = kt - 1
! compute q with piecewise constant method
               if( kt.eq.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 th=(zi(k+1)-za(kb))/dza(kb)
                 tl2=tl*tl
                 th2=th*th
                 qqd=0.5*(qpi(kb)-qmi(kb))
                 qqh=qqd*th2+qmi(kb)*th
                 qql=qqd*tl2+qmi(kb)*tl
                 qn(k) = (qqh-qql)/(th-tl)
               else if( kt.gt.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 tl2=tl*tl
                 qqd=0.5*(qpi(kb)-qmi(kb))
                 qql=qqd*tl2+qmi(kb)*tl
                 dql = qa(kb)-qql
                 zsum  = (1.-tl)*dza(kb)
                 qsum  = dql*dza(kb)
                 if( kt-kb.gt.1 ) then
                 do m=kb+1,kt-1
                   zsum = zsum + dza(m)
                   qsum = qsum + qa(m) * dza(m)
                 enddo
                 endif
                 th=(zi(k+1)-za(kt))/dza(kt)
                 th2=th*th
                 qqd=0.5*(qpi(kt)-qmi(kt))
                 dqh=qqd*th2+qmi(kt)*th
                 zsum  = zsum + th*dza(kt)
                 qsum  = qsum + dqh*dza(kt)
                 qn(k) = qsum/zsum
               endif
               cycle intp
             endif
!     
       enddo intp
!            
! rain out
      sum_precip: do k=1,km
                    if( za(k).lt.0.0 .and. za(k+1).lt.0.0 ) then
                      precip(i) = precip(i) + qa(k)*dza(k)
                      cycle sum_precip
                    else if ( za(k).lt.0.0 .and. za(k+1).ge.0.0 ) then
                      precip(i) = precip(i) + qa(k)*(0.0-za(k))
                      exit sum_precip
                    endif
                    exit sum_precip
      enddo sum_precip
!              
! replace the new values 
      rql(i,:) = qn(:)     
!                          
! ----------------------------------
      enddo i_loop         
!                        
  END SUBROUTINE nislfv_rain_plmr
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm6(im,km,denl,denfacl,tkl,dzl,wwl,rql,rql2, precip1, precip2,dt,id,iter)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
      implicit none
      integer  im,km,id
      real  dt
      real  dzl(im,km),wwl(im,km),rql(im,km),rql2(im,km),precip(im),precip1(im),precip2(im)
      real  denl(im,km),denfacl(im,km),tkl(im,km)
!
      integer  i,k,n,m,kk,kb,kt,iter,ist
      real  tl,tl2,qql,dql,qqd
      real  th,th2,qqh,dqh
      real  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real  allold, allnew, zz, dzamin, cflmax, decfl
      real  dz(km), ww(km), qq(km), qq2(km), wd(km), wa(km), wa2(km), was(km)
      real  den(km), denfac(km), tk(km)
      real  wi(km+1), zi(km+1), za(km+1)
      real  qn(km), qr(km),qr2(km),tmp(km),tmp1(km),tmp2(km),tmp3(km)
      real  dza(km+1), qa(km+1), qa2(km+1),qmi(km+1), qpi(km+1)
!
      precip(:) = 0.0
      precip1(:) = 0.0
      precip2(:) = 0.0
!
      i_loop : do i=1,im
! -----------------------------------
      dz(:) = dzl(i,:)
      qq(:) = rql(i,:)
      qq2(:) = rql2(i,:)
      ww(:) = wwl(i,:)
      den(:) = denl(i,:)
      denfac(:) = denfacl(i,:)
      tk(:) = tkl(i,:)
! skip for no precipitation for all layers
      allold = 0.0
      do k=1,km
        allold = allold + qq(k) + qq2(k)
      enddo
      if(allold.le.0.0) then
        cycle i_loop
      endif
!
! compute interface values
      zi(1)=0.0
      do k=1,km
        zi(k+1) = zi(k)+dz(k)
      enddo
!
! save departure wind
      wd(:) = ww(:)
      n=1
 100  continue
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
      wi(1) = ww(1)
      wi(km+1) = ww(km)
      do k=2,km
        wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
      enddo
! 3rd order interpolation to get wi
      fa1 = 9./16.
      fa2 = 1./16.
      wi(1) = ww(1)
      wi(2) = 0.5*(ww(2)+ww(1))
      do k=3,km-1
        wi(k) = fa1*(ww(k)+ww(k-1))-fa2*(ww(k+1)+ww(k-2))
      enddo
      wi(km) = 0.5*(ww(km)+ww(km-1))
      wi(km+1) = ww(km)
!
! terminate of top of raingroup
      do k=2,km
        if( ww(k).eq.0.0 ) wi(k)=ww(k-1)
      enddo
!
! diffusivity of wi
      con1 = 0.05
      do k=km,1,-1
        decfl = (wi(k+1)-wi(k))*dt/dz(k)
        if( decfl .gt. con1 ) then
          wi(k) = wi(k+1) - con1*dz(k)/dt
        endif
      enddo
! compute arrival point
      do k=1,km+1
        za(k) = zi(k) - wi(k)*dt
      enddo
!
      do k=1,km
        dza(k) = za(k+1)-za(k)
      enddo
      dza(km+1) = zi(km+1) - za(km+1)
!
! computer deformation at arrival point
      do k=1,km
        qa(k) = qq(k)*dz(k)/dza(k)
        qa2(k) = qq2(k)*dz(k)/dza(k)
        qr(k) = qa(k)/den(k)
        qr2(k) = qa2(k)/den(k)
      enddo
      qa(km+1) = 0.0
      qa2(km+1) = 0.0
!     call maxmin(km,1,qa,' arrival points ')
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_snow(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,1,1,1,km)
        call slope_graup(qr2,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa2,1,1,1,km)
        do k = 1, km
          tmp(k) = max((qr(k)+qr2(k)), 1.E-15)
          IF ( tmp(k) .gt. 1.e-15 ) THEN
            wa(k) = (wa(k)*qr(k) + wa2(k)*qr2(k))/tmp(k)
          ELSE
            wa(k) = 0.
          ENDIF
        enddo
        if( n.ge.2 ) wa(1:km)=0.5*(wa(1:km)+was(1:km))
        do k=1,km
!#ifdef DEBUG
!        print*,' slope_wsm3 ',qr(k)*1000.,den(k),denfac(k),tk(k),tmp(k),tmp1(k),tmp2(k), &
!           ww(k),wa(k)
!#endif
! mean wind is average of departure and new arrival winds
          ww(k) = 0.5* ( wd(k)+wa(k) )
        enddo
        was(:) = wa(:)
        n=n+1
        go to 100
      endif
      ist_loop : do ist = 1, 2
      if (ist.eq.2) then
       qa(:) = qa2(:)
      endif
!
      precip(i) = 0.
!
! estimate values at arrival cell interface with monotone
      do k=2,km
        dip=(qa(k+1)-qa(k))/(dza(k+1)+dza(k))
        dim=(qa(k)-qa(k-1))/(dza(k-1)+dza(k))
        if( dip*dim.le.0.0 ) then
          qmi(k)=qa(k)
          qpi(k)=qa(k)
        else
          qpi(k)=qa(k)+0.5*(dip+dim)*dza(k)
          qmi(k)=2.0*qa(k)-qpi(k)
          if( qpi(k).lt.0.0 .or. qmi(k).lt.0.0 ) then
            qpi(k) = qa(k)
            qmi(k) = qa(k)
          endif
        endif
      enddo
      qpi(1)=qa(1)
      qmi(1)=qa(1)
      qmi(km+1)=qa(km+1)
      qpi(km+1)=qa(km+1)
!
! interpolation to regular point
      qn = 0.0
      kb=1
      kt=1
      intp : do k=1,km
             kb=max(kb-1,1)
             kt=max(kt-1,1)
! find kb and kt
             if( zi(k).ge.za(km+1) ) then
               exit intp
             else
               find_kb : do kk=kb,km
                         if( zi(k).le.za(kk+1) ) then
                           kb = kk
                           exit find_kb
                         else
                           cycle find_kb
                         endif
               enddo find_kb
               find_kt : do kk=kt,km
                         if( zi(k+1).le.za(kk) ) then
                           kt = kk
                           exit find_kt
                         else
                           cycle find_kt
                         endif
               enddo find_kt
               kt = kt - 1
! compute q with piecewise constant method
               if( kt.eq.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 th=(zi(k+1)-za(kb))/dza(kb)
                 tl2=tl*tl
                 th2=th*th
                 qqd=0.5*(qpi(kb)-qmi(kb))
                 qqh=qqd*th2+qmi(kb)*th
                 qql=qqd*tl2+qmi(kb)*tl
                 qn(k) = (qqh-qql)/(th-tl)
               else if( kt.gt.kb ) then
                 tl=(zi(k)-za(kb))/dza(kb)
                 tl2=tl*tl
                 qqd=0.5*(qpi(kb)-qmi(kb))
                 qql=qqd*tl2+qmi(kb)*tl
                 dql = qa(kb)-qql
                 zsum  = (1.-tl)*dza(kb)
                 qsum  = dql*dza(kb)
                 if( kt-kb.gt.1 ) then
                 do m=kb+1,kt-1
                   zsum = zsum + dza(m)
                   qsum = qsum + qa(m) * dza(m)
                 enddo
                 endif
                 th=(zi(k+1)-za(kt))/dza(kt)
                 th2=th*th
                 qqd=0.5*(qpi(kt)-qmi(kt))
                 dqh=qqd*th2+qmi(kt)*th
                 zsum  = zsum + th*dza(kt)
                 qsum  = qsum + dqh*dza(kt)
                 qn(k) = qsum/zsum
               endif
               cycle intp
             endif
!
       enddo intp
!
! rain out
      sum_precip: do k=1,km
                    if( za(k).lt.0.0 .and. za(k+1).lt.0.0 ) then
                      precip(i) = precip(i) + qa(k)*dza(k)
                      cycle sum_precip
                    else if ( za(k).lt.0.0 .and. za(k+1).ge.0.0 ) then
                      precip(i) = precip(i) + qa(k)*(0.0-za(k))
                      exit sum_precip
                    endif
                    exit sum_precip
      enddo sum_precip
!
! replace the new values
      if(ist.eq.1) then
        rql(i,:) = qn(:)
        precip1(i) = precip(i)
      else
        rql2(i,:) = qn(:)
        precip2(i) = precip(i)
      endif
      enddo ist_loop
!
! ----------------------------------
      enddo i_loop
!
  END SUBROUTINE nislfv_rain_plm6

!+---+-----------------------------------------------------------------+
      subroutine refl10cm_wdm7 (qv1d, qr1d, nr1d, qs1d, qg1d,           &
                       t1d, p1d, dBZ, kts, kte, ii, jj)

      IMPLICIT NONE

!..Sub arguments
      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                      qv1d, qr1d, nr1d, qs1d, qg1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ

!..Local variables
      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho
      REAL, DIMENSION(kts:kte):: rr, nr, rs, rg
      REAL:: temp_C

      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilams, ilamg
      DOUBLE PRECISION, DIMENSION(kts:kte):: N0_r, N0_s, N0_g
      DOUBLE PRECISION:: lamr, lams, lamg
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg

      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel
      DOUBLE PRECISION:: fmelt_s, fmelt_g

      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti

      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL, PARAMETER:: R=287.

!+---+

      do k = kts, kte
         dBZ(k) = -35.0
      enddo

!+---+-----------------------------------------------------------------+
!..Put column of data into local arrays.
!+---+-----------------------------------------------------------------+
      do k = kts, kte
         temp(k) = t1d(k)
         temp_C = min(-0.001, temp(K)-273.15)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))

         if (qr1d(k) .gt. 1.E-9) then
            rr(k) = qr1d(k)*rho(k)
            nr(k) = nr1d(k)*rho(k)
            lamr = (xam_r*xcrg(3)*xorg2*nr(k)/rr(k))**xobmr
            ilamr(k) = 1./lamr
            N0_r(k) = nr(k)*xorg2*lamr**xcre(2)
            L_qr(k) = .true.
         else
            rr(k) = 1.E-12
            nr(k) = 1.E-12
            L_qr(k) = .false.
         endif

         if (qs1d(k) .gt. 1.E-9) then
            rs(k) = qs1d(k)*rho(k)
            N0_s(k) = min(n0smax, n0s*exp(-alpha*temp_C))
            lams = (xam_s*xcsg(3)*N0_s(k)/rs(k))**(1./xcse(1))
            ilams(k) = 1./lams
            L_qs(k) = .true.
         else
            rs(k) = 1.E-12
            L_qs(k) = .false.
         endif

         if (qg1d(k) .gt. 1.E-9) then
            rg(k) = qg1d(k)*rho(k)
            N0_g(k) = n0g
            lamg = (xam_g*xcgg(3)*N0_g(k)/rg(k))**(1./xcge(1))
            ilamg(k) = 1./lamg
            L_qg(k) = .true.
         else
            rg(k) = 1.E-12
            L_qg(k) = .false.
         endif
      enddo

!+---+-----------------------------------------------------------------+
!..Locate K-level of start of melting (k_0 is level above).
!+---+-----------------------------------------------------------------+
      melti = .false.
      k_0 = kts
      do k = kte-1, kts, -1
         if ( (temp(k).gt.273.15) .and. L_qr(k)                         &
                                  .and. (L_qs(k+1).or.L_qg(k+1)) ) then
            k_0 = MAX(k+1, k_0)
            melti=.true.
            goto 195
         endif
      enddo
 195  continue

!+---+-----------------------------------------------------------------+
!..Assume Rayleigh approximation at 10 cm wavelength. Rain (all temps)
!.. and non-water-coated snow and graupel when below freezing are
!.. simple. Integrations of m(D)*m(D)*N(D)*dD.
!+---+-----------------------------------------------------------------+

      do k = kts, kte
         ze_rain(k) = 1.e-22
         ze_snow(k) = 1.e-22
         ze_graupel(k) = 1.e-22
         if (L_qr(k)) ze_rain(k) = N0_r(k)*xcrg(4)*ilamr(k)**xcre(4)
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)     &
                                 * (xam_s/900.0)*(xam_s/900.0)          &
                                 * N0_s(k)*xcsg(4)*ilams(k)**xcse(4)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
                                    * (xam_g/900.0)*(xam_g/900.0)       &
                                    * N0_g(k)*xcgg(4)*ilamg(k)**xcge(4)
      enddo


!+---+-----------------------------------------------------------------+
!..Special case of melting ice (snow/graupel) particles.  Assume the
!.. ice is surrounded by the liquid water.  Fraction of meltwater is
!.. extremely simple based on amount found above the melting level.
!.. Uses code from Uli Blahak (rayleigh_soak_wetgraupel and supporting
!.. routines).
!+---+-----------------------------------------------------------------+

      if (melti .and. k_0.ge.kts+1) then
       do k = k_0-1, kts, -1

!..Reflectivity contributed by melting snow
          if (L_qs(k) .and. L_qs(k_0) ) then
           fmelt_s = MAX(0.005d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           eta = 0.d0
           lams = 1./ilams(k)
           do n = 1, nrbins
              x = xam_s * xxDs(n)**xbm_s
              call rayleigh_soak_wetgraupel (x,DBLE(xocms),DBLE(xobms), &
                    fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_s, matrixstring_s,          &
                    inclusionstring_s, hoststring_s,                    &
                    hostmatrixstring_s, hostinclusionstring_s)
              f_d = N0_s(k)*xxDs(n)**xmu_s * DEXP(-lams*xxDs(n))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif


!..Reflectivity contributed by melting graupel

          if (L_qg(k) .and. L_qg(k_0) ) then
           fmelt_g = MAX(0.005d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
           eta = 0.d0
           lamg = 1./ilamg(k)
           do n = 1, nrbins
              x = xam_g * xxDg(n)**xbm_g
              call rayleigh_soak_wetgraupel (x,DBLE(xocmg),DBLE(xobmg), &
                    fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
                    CBACK, mixingrulestring_g, matrixstring_g,          &
                    inclusionstring_g, hoststring_g,                    &
                    hostmatrixstring_g, hostinclusionstring_g)
              f_d = N0_g(k)*xxDg(n)**xmu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif

       enddo
      endif

      do k = kte, kts, -1
         dBZ(k) = 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.d18)
      enddo


      end subroutine refl10cm_wdm7
!+---+-----------------------------------------------------------------+

!-----------------------------------------------------------------------
     subroutine effectRad_wdm7 (t, qc, nc, qi, qs, rho, qmin, t0c,        &
                                re_qc, re_qi, re_qs, kts, kte, ii, jj)

!-----------------------------------------------------------------------
!  Compute radiation effective radii of cloud water, ice, and snow for 
!  double-moment microphysics..
!  These are entirely consistent with microphysics assumptions, not
!  constant or otherwise ad hoc as is internal to most radiation
!  schemes.  
!  Coded and implemented by Soo Ya Bae, KIAPS, January 2015.
!-----------------------------------------------------------------------

      implicit none

!..Sub arguments
      integer, intent(in) :: kts, kte, ii, jj
      real, intent(in) :: qmin
      real, intent(in) :: t0c
      real, dimension( kts:kte ), intent(in)::  t
      real, dimension( kts:kte ), intent(in)::  qc
      real, dimension( kts:kte ), intent(in)::  nc
      real, dimension( kts:kte ), intent(in)::  qi
      real, dimension( kts:kte ), intent(in)::  qs
      real, dimension( kts:kte ), intent(in)::  rho
      real, dimension( kts:kte ), intent(inout):: re_qc
      real, dimension( kts:kte ), intent(inout):: re_qi
      real, dimension( kts:kte ), intent(inout):: re_qs
!..Local variables
      integer:: i,k
      integer :: inu_c
      real, dimension( kts:kte ):: ni
      real, dimension( kts:kte ):: rqc
      real, dimension( kts:kte ):: rnc
      real, dimension( kts:kte ):: rqi
      real, dimension( kts:kte ):: rni
      real, dimension( kts:kte ):: rqs
      real :: cdm2
      real :: temp
      real :: supcol, n0sfac, lamdas
      real :: diai      ! diameter of ice in m
      double precision :: lamc
      logical :: has_qc, has_qi, has_qs
!..Minimum microphys values
      real, parameter :: R1 = 1.E-12
      real, parameter :: R2 = 1.E-6
      real, parameter :: pi = 3.1415926536
      real, parameter :: bm_r = 3.0
      real, parameter :: obmr = 1.0/bm_r
      real, parameter :: cdm  = 5./3.
!-----------------------------------------------------------------------
      has_qc = .false.
      has_qi = .false.
      has_qs = .false.

      cdm2 = rgmma(cdm)

      do k=kts,kte
        ! for cloud
        rqc(k) = max(R1, qc(k)*rho(k))
        rnc(k) = max(R2, nc(k)*rho(k))
        if (rqc(k).gt.R1 .and. rnc(k).gt.R2) has_qc = .true.
        ! for ice
        rqi(k) = max(R1, qi(k)*rho(k))
        temp = (rho(k)*max(qi(k),qmin))
        temp = sqrt(sqrt(temp*temp*temp))
        ni(k) = min(max(5.38e7*temp,1.e3),1.e6)
        rni(k)= max(R2, ni(k)*rho(k))
        if (rqi(k).gt.R1 .and. rni(k).gt.R2) has_qi = .true.
        ! for snow
        rqs(k) = max(R1, qs(k)*rho(k))
        if (rqs(k).gt.R1) has_qs = .true.
      enddo

      if (has_qc) then
        do k=kts,kte
          if (rqc(k).le.R1 .or. rnc(k).le.R2) CYCLE
          lamc = 2.*cdm2*(pidnc*nc(k)/rqc(k))**obmr
          re_qc(k) =  max(2.51e-6,min(sngl(1.0d0/lamc),50.e-6))
        enddo
      endif

      if (has_qi) then
        do k=kts,kte
          if (rqi(k).le.R1 .or. rni(k).le.R2) CYCLE
          diai = 11.9*sqrt(rqi(k)/ni(k))
          re_qi(k) = max(10.01e-6,min(0.75*0.163*diai,125.e-6))
        enddo
      endif

      if (has_qs) then
        do k=kts,kte
          if (rqs(k).le.R1) CYCLE
          supcol = t0c-t(k)
          n0sfac = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          lamdas = sqrt(sqrt(pidn0s*n0sfac/rqs(k))) 
          re_qs(k) = max(25.e-6,min(0.5*(1./lamdas),999.e-6))
        enddo
      endif

      end subroutine effectRad_wdm7
!-----------------------------------------------------------------------

END MODULE module_mp_wdm7

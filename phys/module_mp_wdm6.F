#if ( RWORDSIZE == 4 )
#  define VREC vsrec
#  define VSQRT vssqrt
#else
#  define VREC vrec
#  define VSQRT vsqrt
#endif
!
MODULE module_mp_wdm6
!
!
!
   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120. ! maximum time step for minor loops
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6         ! intercept parameter rain
   REAL, PARAMETER, PRIVATE :: n0g = 4.e6         ! intercept parameter graupel
   REAL, PARAMETER, PRIVATE :: avtr = 841.9       ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8         ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5         ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: peaut = .55        ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8        ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5    ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER, PRIVATE :: avts = 11.72       ! a constant for terminal velocity of snow
   REAL, PARAMETER, PRIVATE :: bvts = .41         ! a constant for terminal velocity of snow 
   REAL, PARAMETER, PRIVATE :: avtg = 330.        ! a constant for terminal velocity of graupel 
   REAL, PARAMETER, PRIVATE :: bvtg = 0.8         ! a constant for terminal velocity of graupel
   REAL, PARAMETER, PRIVATE :: deng = 500.        ! density of graupel
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e11    ! maximum n0s (t=-90C unlimited)
   REAL, PARAMETER, PRIVATE :: lamdacmax = 1.e10  ! limited maximum value for slope parameter of cloud water 
   REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e8   ! limited maximum value for slope parameter of rain
   REAL, PARAMETER, PRIVATE :: lamdasmax = 1.e5   ! limited maximum value for slope parameter of snow
   REAL, PARAMETER, PRIVATE :: lamdagmax = 6.e4   ! limited maximum value for slope parameter of graupel
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
!
   REAL, SAVE ::                                           &
             qc0,qck1,pidnc,bvtr1,bvtr2,bvtr3,bvtr4,bvtr5, &
             bvtr6,bvtr7, bvtr2o5,bvtr3o5,                 &
             g1pbr,g2pbr,g3pbr,g4pbr,g5pbr,g6pbr,g7pbr,    &
             g5pbro2,g7pbro2,                              &
             pvtr,pvtrn,eacrr,pacrr,pidn0r,pidnr,          &
             precr1,precr2,xmmax,roqimax,bvts1,bvts2,      &
             bvts3,bvts4,g1pbs,g3pbs,g4pbs,g5pbso2,        &
             pvts,pacrs,precs1,precs2,pidn0s,xlv1,pacrc,   &
             bvtg1,bvtg2,bvtg3,bvtg4,g1pbg,g3pbg,g4pbg,    &
             g5pbgo2,pvtg,pacrg,precg1,precg2,pidn0g,      &
             rslopecmax,rslopec2max,rslopec3max,           &
             rslopermax,rslopesmax,rslopegmax,             &
             rsloperbmax,rslopesbmax,rslopegbmax,          &
             rsloper2max,rslopes2max,rslopeg2max,          &
             rsloper3max,rslopes3max,rslopeg3max
CONTAINS
!===================================================================
!
  SUBROUTINE wdm6(th, q, qc, qr, qi, qs, qg,               &
                    nn, nc, nr,                            &
                    den, pii, p, delz,                     &
                    delt,g, cpd, cpv, ccn0, rd, rv, t0c,   &
                    ep1, ep2, qmin,                        &
                    XLS, XLV0, XLF0, den0, denr,           &
                    cliq,cice,psat,                        &
                    rain, rainncv,                         &
                    snow, snowncv,                         &
                    sr,                                    &
                    graupel, graupelncv,                   &
                    ids,ide, jds,jde, kds,kde,             &
                    ims,ime, jms,jme, kms,kme,             &
                    its,ite, jts,jte, kts,kte              &
                                                           )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a WRF double-moment 6-class GRAUPEL phase 
!  microphyiscs scheme (WDM6). The WDM microphysics scheme predicts 
!  number concentrations for warm rain species including clouds and 
!  rain. cloud condensation nuclei (CCN) is also predicted.
!  The cold rain species including ice, snow, graupel follow the 
!  WRF single-moment 6-class microphysics (WSM6, Hong and Lim 2006)
!  in which theoretical background for WSM ice phase microphysics is 
!  based on Hong et al. (2004). A new mixed-phase terminal velocity
!  for precipitating ice is introduced in WSM6 (Dudhia et al. 2008). 
!  The WDM scheme is described in Lim and Hong (2009).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WDM6 cloud scheme
!
!  Coded by Kyo-Sun Lim and Song-You Hong (Yonsei Univ.) Fall 2008
!
!  Implemented by Kyo-Sun Lim and Jimy Dudhia (NCAR) Winter 2008
!
!  Reference) Lim and Hong (LH, 2009) Manuscript in preperation
!             Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev. 
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc. 
!             Cohard and Pinty (CP, 2000) Quart. J. Roy. Meteor. Soc.
!             Khairoutdinov and Kogan (KK, 2000) Mon. Wea. Rev.
!             Dudhia, Hong and Lim (DHL, 2008) J. Meteor. Soc. Japan
!             
!             Lin, Farley, Orville (LFO, 1983) J. Appl. Meteor.
!             Rutledge, Hobbs (RH83, 1983) J. Atmos. Sci.
!             Rutledge, Hobbs (RH84, 1984) J. Atmos. Sci.
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                               q, &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg, &
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
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr 
  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv
  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                 &
        INTENT(INOUT) ::                                 graupel, &
                                                        graupelncv
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::   qrs, ncr
  INTEGER ::               i,j,k
!-------------------------------------------------------------------
      DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
            qrs(i,k,3) = qg(i,k,j)
            ncr(i,k,1) = nn(i,k,j)
            ncr(i,k,2) = nc(i,k,j)
            ncr(i,k,3) = nr(i,k,j)     
         ENDDO
         ENDDO
         !  Sending array starting locations of optional variables may cause
         !  troubles, so we explicitly change the call.

         CALL wdm62D(t, q(ims,kms,j), qci, qrs, ncr               &
                    ,den(ims,kms,j)                               &
                    ,p(ims,kms,j), delz(ims,kms,j)                &
                    ,delt,g, cpd, cpv, ccn0, rd, rv, t0c          &
                    ,ep1, ep2, qmin                               &
                    ,XLS, XLV0, XLF0, den0, denr                  &
                    ,cliq,cice,psat                               &
                    ,j                                            &
                    ,rain(ims,j),rainncv(ims,j)                   &
                    ,sr(ims,j)                                    &
                    ,ids,ide, jds,jde, kds,kde                    &
                    ,ims,ime, jms,jme, kms,kme                    &
                    ,its,ite, jts,jte, kts,kte                    &
                    ,snow(ims,j),snowncv(ims,j)                   &
                    ,graupel(ims,j),graupelncv(ims,j)             &
                                                                   )
         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            qg(i,k,j) = qrs(i,k,3)
            nn(i,k,j) = ncr(i,k,1)
            nc(i,k,j) = ncr(i,k,2)
            nr(i,k,j) = ncr(i,k,3)   
         ENDDO
         ENDDO
      ENDDO
  END SUBROUTINE wdm6
!===================================================================
!
  SUBROUTINE wdm62D(t, q, qci, qrs, ncr, den, p, delz             &
                   ,delt,g, cpd, cpv, ccn0, rd, rv, t0c           &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                   ,graupel,graupelncv                            &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte , &
                                      lat
  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL, DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci
  REAL, DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qrs, &
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
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte , 3) ::                      &
        rh, qs, rslope, rslope2, rslope3, rslopeb,                &
        falk, fall, work1
  REAL, DIMENSION( its:ite , kts:kte ) ::                         & 
        rslopec, rslopec2,rslopec3 
  REAL, DIMENSION( its:ite , kts:kte,  2) ::                      &
        avedia 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        workn,falln,falkn
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        worka
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pcact, prevp, psdep, pgdep, praut, psaut, pgaut,          &
        pracw, psacw, pgacw, pgacr, pgacs, psaci, pgmlt, praci,   &
        piacr, pracs, psacr, pgaci, pseml, pgeml, prevp_s      
  REAL, DIMENSION( its:ite , kts:kte ) :: paacw
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        nraut, nracw, nrevp, ncevp, nccol, nrcol,                 &
        nsacw, ngacw, niacr, nsacr, ngacr, naacw,                 &
        nseml, ngeml, ncact 
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pigen, pidep, pcond, xl, cpm, work2, psmlt, psevp,        &
        denfac, xni, pgevp,n0sfac, qsum
  REAL :: ifac, gfac, sfac
! variables for optimization
  REAL, DIMENSION( its:ite )           :: tvec1
  REAL :: temp
  INTEGER, DIMENSION( its:ite ) :: mnstep, numndt
  INTEGER, DIMENSION( its:ite ) :: mstep, numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdac, lamdar, lamdas, lamdag,        &
            diffus,                                               &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            ndt, qdt, holdrr, holdrs, holdrg, supcol, supcolt,    &
            pvt, coeres, supsat, dtcld, xmi, eacrs, satdt,        &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, fallsum_qg,                     &
            vt2i,vt2r,vt2s,vt2g,acrfac,egs,egi,                   &
            xlwork2, factor, source, value, coecol,               &
            nfrzdtr, nfrzdtc,                                     &
            taucon, lencon, lenconcr,                       &
            xlf, pfrzdtc, pfrzdtr, supice, alpha2, delta2, delta3 
  REAL  :: vt2ave
  REAL  :: holdc, holdci
!
  INTEGER :: i, j, k, mstepmax,                                                &
            iprt, latd, lond, loop, loops, ifsat, n
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
      lamdar(x,y,z)= exp(log(((pidnr*z)/(x*y)))*((.33333333)))    
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
      lamdag(x,y)=   sqrt(sqrt(pidn0g/(x*y)))      ! (pidn0g/(x*y))**.25
!
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
      pi = 4. * atan(1.)
!
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!
      do k = kts, kte
        do i = its, ite
          qci(i,k,1) = max(qci(i,k,1),0.0)
          qrs(i,k,1) = max(qrs(i,k,1),0.0)
          qci(i,k,2) = max(qci(i,k,2),0.0)
          qrs(i,k,2) = max(qrs(i,k,2),0.0)
          qrs(i,k,3) = max(qrs(i,k,3),0.0)
          ncr(i,k,1) = max(ncr(i,k,1),0.0)
          ncr(i,k,2) = max(ncr(i,k,2),0.0)
          ncr(i,k,3) = max(ncr(i,k,3),0.0) 
        enddo
      enddo
!
!----------------------------------------------------------------
!     latent heat for phase changes and heat capacity. neglect the
!     changes during microphysical process calculation
!     emanuel(1994)
!
      do k = kts, kte
        do i = its, ite
          cpm(i,k) = cpmcal(q(i,k))
          xl(i,k) = xlcal(t(i,k))
        enddo
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
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          endif
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
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pgaut(i,k) = 0.
          pracw(i,k) = 0.
          praci(i,k) = 0.
          piacr(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pracs(i,k) = 0.
          psacr(i,k) = 0.
          pgacw(i,k) = 0.
          paacw(i,k) = 0.
          pgaci(i,k) = 0.
          pgacr(i,k) = 0.
          pgacs(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          pgmlt(i,k) = 0.
          pseml(i,k) = 0.
          pgeml(i,k) = 0.
          psevp(i,k) = 0.
          pgevp(i,k) = 0.
          pcact(i,k) = 0.
          prevp_s(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          falk(i,k,3) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fall(i,k,3) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          falln(i,k) =0.
          falkn(i,k) =0.
          xni(i,k) = 1.e3
          nsacw(i,k) = 0.
          ngacw(i,k) = 0.
          naacw(i,k) = 0.
          niacr(i,k) = 0.
          nsacr(i,k) = 0.
          ngacr(i,k) = 0.
          nseml(i,k) = 0.
          ngeml(i,k) = 0.
          nracw(i,k) = 0.
          nccol(i,k) = 0.
          nrcol(i,k) = 0.
          ncact(i,k) = 0.
          nraut(i,k) = 0.
          nrevp(i,k) = 0.
          ncevp(i,k) = 0.
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(qrs(i,k,1).le.qcrmin .or. ncr(i,k,3).le.nrmin ) then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k),ncr(i,k,3))
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qci(i,k,1).le.qmin .or. ncr(i,k,2).le.ncmin ) then
            rslopec(i,k) = rslopecmax
            rslopec2(i,k) = rslopec2max
            rslopec3(i,k) = rslopec3max
          else
            rslopec(i,k) = 1./lamdac(qci(i,k,1),den(i,k),ncr(i,k,2))
            rslopec2(i,k) = rslopec(i,k)*rslopec(i,k)
            rslopec3(i,k) = rslopec2(i,k)*rslopec(i,k)
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
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
        enddo
      enddo
!
      mstepmax = 1
      numndt = 1
      do k = kte, kts, -1
        do i = its, ite
          workn(i,k) = pvtrn*rslopeb(i,k,1)*denfac(i,k)/delz(i,k)
          numndt(i) = max(nint(workn(i,k)*dtcld+.5),1)
          if(numndt(i).ge.mnstep(i)) mnstep(i) = numndt(i)
        enddo
      enddo
      do i = its, ite
        if(mstepmax.le.mnstep(i)) mstepmax = mnstep(i)
      enddo
!
      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mnstep(i)) then
            falkn(i,k) = den(i,k)*ncr(i,k,3)*workn(i,k)/mnstep(i)
            falln(i,k) = falln(i,k)+falkn(i,k)
            ncr(i,k,3) = max(ncr(i,k,3)-falkn(i,k)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mnstep(i)) then
              falkn(i,k) = den(i,k)*ncr(i,k,3)*workn(i,k)/mnstep(i)
              falln(i,k) = falln(i,k)+falkn(i,k)
              ncr(i,k,3) = max(ncr(i,k,3)-(falkn(i,k)-falkn(i,k+1)*delz(i,k+1) &
                          /delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
!
      mstepmax = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          work1(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)/delz(i,k)
          work1(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)/delz(i,k)
          work1(i,k,3) = pvtg*rslopeb(i,k,3)*denfac(i,k)/delz(i,k)
          qsum(i,k) = max( (qrs(i,k,2)+qrs(i,k,3)), 1.e-15)
          if (qsum(i,k) .gt. 1.e-15) then 
            worka(i,k) = (work1(i,k,2)*qrs(i,k,2) + work1(i,k,3)*qrs(i,k,3))   &       
                        /qsum(i,k)
          else
            worka(i,k) = 0.
          endif 
          numdt(i) = max(nint(max(work1(i,k,1),worka(i,k))*dtcld+.5),1)
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
            falk(i,k,2) = den(i,k)*qrs(i,k,2)*worka(i,k)/mstep(i)
            falk(i,k,3) = den(i,k)*qrs(i,k,3)*worka(i,k)/mstep(i)
            fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
            fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
            fall(i,k,3) = fall(i,k,3)+falk(i,k,3)
            qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k),0.)
            qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k),0.)
            qrs(i,k,3) = max(qrs(i,k,3)-falk(i,k,3)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*worka(i,k)/mstep(i)
              falk(i,k,3) = den(i,k)*qrs(i,k,3)*worka(i,k)/mstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              fall(i,k,3) = fall(i,k,3)+falk(i,k,3)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)           &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,3) = max(qrs(i,k,3)-(falk(i,k,3)-falk(i,k+1,3)           &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
        do k = kte, kts, -1
          do i = its, ite
            if(n.le.mstep(i) .and. t(i,k).gt.t0c) then
!---------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!---------------------------------------------------------------
              xlf = xlf0
              work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
              if(qrs(i,k,2).gt.0.) then
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
                psmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2.       &
                           *n0sfac(i,k)*(precs1*rslope2(i,k,2)                 &
                           +precs2*work2(i,k)*coeres)
                psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep(i),-qrs(i,k,2)     &
                           /mstep(i)),0.)
                qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
!-------------------------------------------------------------------
! nsmlt: melting of snow 
!       (T>T0: ->NR)
!-------------------------------------------------------------------
                if(qrs(i,k,2).gt.qcrmin) then
                  sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                  ncr(i,k,3) = ncr(i,k,3) - sfac*psmlt(i,k)
                endif
                t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)
              endif
!---------------------------------------------------------------
! pgmlt: melting of graupel [HL A23]  [LFO 47]
!       (T>T0: G->R)
!---------------------------------------------------------------
              if(qrs(i,k,3).gt.0.) then
                coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
                pgmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*(precg1     &       
                             *rslope2(i,k,3) + precg2*work2(i,k)*coeres)
                pgmlt(i,k) = min(max(pgmlt(i,k)*dtcld/mstep(i),                &
                            -qrs(i,k,3)/mstep(i)),0.)
                qrs(i,k,3) = qrs(i,k,3) + pgmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - pgmlt(i,k)
!-------------------------------------------------------------------
! ngmlt: melting of graupel
!       (T>T0: ->R) 
!-------------------------------------------------------------------
                if(qrs(i,k,3).gt.qcrmin) then  
                  gfac = rslope(i,k,3)*n0g/qrs(i,k,3)
                  ncr(i,k,3) = ncr(i,k,3) - gfac*pgmlt(i,k)
                endif
                t(i,k) = t(i,k) + xlf/cpm(i,k)*pgmlt(i,k)
              endif
            endif
          enddo
        enddo
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      mstepmax = 1
      mstep = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          if(qci(i,k,2).le.0.) then
            work2c(i,k) = 0.
          else
            xmi = den(i,k)*qci(i,k,2)/xni(i,k)
!           diameter  = min(dicon * sqrt(xmi),dimax)
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
            work1c(i,k) = 1.49e4*diameter**1.31
            work2c(i,k) = work1c(i,k)/delz(i,k)
          endif
          numdt(i) = max(nint(work2c(i,k)*dtcld+.5),1)
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
            falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k,2)
            qci(i,k,2) = max(qci(i,k,2)-falkc(i,k)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k,2)
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)*delz(i,k+1) &
                          /delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,kts,1)+fall(i,kts,2)+fall(i,kts,3)+fallc(i,kts)
        fallsum_qsi = fall(i,kts,2)+fallc(i,kts)
        fallsum_qg = fall(i,kts,3)
        rainncv(i) = 0.
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,kts)/denr*dtcld*1000.
          rain(i) = fallsum*delz(i,kts)/denr*dtcld*1000. + rain(i)
        endif
        IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN
          snowncv(i) = 0.
          if(fallsum_qsi.gt.0.) then
            snowncv(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.
            snow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snow(i)
          endif
        ENDIF
        IF ( PRESENT (graupelncv) .AND. PRESENT (graupel)) THEN
          graupelncv(i) = 0.
          if(fallsum_qg.gt.0.) then
            graupelncv(i) = fallsum_qg*delz(i,kts)/denr*dtcld*1000.
            graupel(i) = fallsum_qg*delz(i,kts)/denr*dtcld*1000. + graupel(i)
          endif
        ENDIF
        sr(i) = 0.
        if(fallsum.gt.0.)sr(i)=(fallsum_qsi*delz(i,kts)/denr*dtcld*1000. +     &
                                fallsum_qg*delz(i,kts)/denr*dtcld*1000.)       &
                                /(rainncv(i)+1.e-12)
      enddo
!
!---------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0 .and. qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
!---------------------------------------------------------------
! nimlt: instantaneous melting of cloud ice 
!        (T>T0: ->NC)
!--------------------------------------------------------------
            if(qci(i,k,2).gt.qmin) then 
              ifac = xni(i,k)/qci(i,k,2)
              ncr(i,k,2) = ncr(i,k,2) + ifac*qci(i,k,2)
            endif
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!---------------------------------------------------------------
! pihmf: homogeneous  of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40. .and. qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
!---------------------------------------------------------------
! nihmf: homogeneous  of cloud water below -40c [HL A45]
!        (T<-40C: NC->) 
!---------------------------------------------------------------
            if(ncr(i,k,2).gt.0.) ncr(i,k,2) = 0. 
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous  of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0. .and. qci(i,k,1).gt.qmin) then
            supcolt=min(supcol,70.)
            pfrzdtc = min(pi*pi*pfrz1*(exp(pfrz2*supcolt)-1.)*denr/den(i,k)    & 
                     *ncr(i,k,2)*rslopec3(i,k)*rslopec3(i,k)/18.*dtcld         &
                     ,qci(i,k,1))
!---------------------------------------------------------------
! nihtf: heterogeneous  of cloud water
!         (T0>T>-40C: NC->) 
!---------------------------------------------------------------
            if(ncr(i,k,2).gt.ncmin) then
              nfrzdtc = min(pi*pfrz1*(exp(pfrz2*supcolt)-1.)*ncr(i,k,2)        &
                      *rslopec3(i,k)/6.*dtcld,ncr(i,k,2))
              ncr(i,k,2) = ncr(i,k,2) - nfrzdtc
            endif
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif 
!---------------------------------------------------------------
! pgfrz:  freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->G)
!---------------------------------------------------------------
          if(supcol.gt.0. .and. qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,70.)
            pfrzdtr = min(140.*(pi*pi)*pfrz1*ncr(i,k,3)*denr/den(i,k)          &
                  *(exp(pfrz2*supcolt)-1.)*rslope3(i,k,1)*rslope3(i,k,1)       & 
                  *dtcld,qrs(i,k,1))        
!---------------------------------------------------------------
! ngfrz: freezing of rain water
!        (T<T0, NR-> ) 
!---------------------------------------------------------------
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
!
!----------------------------------------------------------------
!     rsloper: reverse of the slope parameter of the rain(m)
!     xka:    thermal conductivity of air(jm-1s-1k-1)
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k,1).le.qcrmin .or. ncr(i,k,3).le.nrmin ) then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k),ncr(i,k,3))
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
!
! compute the mean-volume drop diameter for raindrop distribution
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
! compute the mean-volume drop diameter for cloud-droplet distribution
          avedia(i,k,1) = rslopec(i,k)
!
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
! - follows the double-moment processes in Lim and Hong
!
!===============================================================
!
      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain  [CP 17]
!        (C->R)
!--------------------------------------------------------------
          lencon  = 2.7e-2*den(i,k)*qci(i,k,1)*(1.e20/16.*rslopec2(i,k)        &   
                   *rslopec2(i,k)-0.4)
          lenconcr = max(1.2*lencon, qcrmin)
          if(avedia(i,k,1).gt.di15) then
            taucon = 3.7/den(i,k)/qci(i,k,1)/(0.5e6*rslopec(i,k)-7.5)
            praut(i,k) = lencon/taucon
            praut(i,k) = min(max(praut(i,k),0.),qci(i,k,1)/dtcld)
!---------------------------------------------------------------
! nraut: auto conversion rate from cloud to rain [CP 18 & 19]
!        (NC->NR)
!---------------------------------------------------------------
            nraut(i,k) = 3.5e9*den(i,k)*praut(i,k)
            if(qrs(i,k,1).gt.lenconcr)                                         &
            nraut(i,k) = ncr(i,k,3)/qrs(i,k,1)*praut(i,k)
            nraut(i,k) = min(nraut(i,k),ncr(i,k,2)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain        [CP 22 & 23]
!        (C->R)
! nracw: accretion of cloud water by rain
!        (NC->)
!---------------------------------------------------------------
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
!----------------------------------------------------------------
! nccol: self collection of cloud water              [CP 24 & 25]     
!        (NC->)
!----------------------------------------------------------------
          if(avedia(i,k,1).ge.di100) then
            nccol(i,k) = ncrk1*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)
          else
            nccol(i,k) = 2.*ncrk2*ncr(i,k,2)*ncr(i,k,2)*rslopec3(i,k)        &     
                         *rslopec3(i,k)
          endif
!----------------------------------------------------------------
! nrcol: self collection of rain-drops and break-up [CP 24 & 25]
!        (NR->)
!----------------------------------------------------------------
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
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain 
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*ncr(i,k,3)*(precr1*rslope(i,k,1)       &
                         + precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
!----------------------------------------------------------------
! Nrevp: evaporation/condensation rate of rain [CP  ]
!        (NR->NC) 
!----------------------------------------------------------------
              if(avedia(i,k,2).le.di82) then
                nrevp(i,k) = ncr(i,k,3)/dtcld
!----------------------------------------------------------------
! Prevp_s: evaporation/condensation rate of rain [KK 23]
!          (R->C) 
!----------------------------------------------------------------
                prevp_s(i,k) = qrs(i,k,1)/dtcld
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
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
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
          qsum(i,k) = max((qrs(i,k,2)+qrs(i,k,3)),1.e-15)
          if(qsum(i,k) .gt. 1.e-15) then
            vt2ave=(vt2s*qrs(i,k,2)+vt2g*qrs(i,k,3))/(qsum(i,k))          
          else    
            vt2ave=0.
          endif
          if(supcol.gt.0. .and. qci(i,k,2).gt.qmin) then
            if(qrs(i,k,1).gt.qcrmin) then
!-------------------------------------------------------------
! praci: Accretion of cloud ice by rain [HL A15] [LFO 25]
!        (T<T0: I->R)
!-------------------------------------------------------------
              acrfac = 6.*rslope2(i,k,1)+4.*diameter*rslope(i,k,1) + diameter**2
              praci(i,k) = pi*qci(i,k,2)*ncr(i,k,3)*abs(vt2r-vt2i)*acrfac/4.
              praci(i,k) = min(praci(i,k),qci(i,k,2)/dtcld)
!-------------------------------------------------------------
! piacr: Accretion of rain by cloud ice [HL A19] [LFO 26]
!        (T<T0: R->S or R->G)
!-------------------------------------------------------------
              piacr(i,k) = pi*pi*avtr*ncr(i,k,3)*denr*xni(i,k)*denfac(i,k)     &
                          *g7pbr*rslope3(i,k,1)*rslope2(i,k,1)*rslopeb(i,k,1)  &
                          /24./den(i,k)
              piacr(i,k) = min(piacr(i,k),qrs(i,k,1)/dtcld)
            endif
!-------------------------------------------------------------
! niacr: Accretion of rain by cloud ice 
!        (T<T0: NR->) 
!-------------------------------------------------------------
            if(ncr(i,k,3).gt.nrmin) then
              niacr(i,k) = pi*avtr*ncr(i,k,3)*xni(i,k)*denfac(i,k)*g4pbr       &
                          *rslope2(i,k,1)*rslopeb(i,k,1)/4.
              niacr(i,k) = min(niacr(i,k),ncr(i,k,3)/dtcld)
            endif
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by snow [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.qcrmin) then
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      + diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                          *abs(vt2ave-vt2i)*acrfac/4.
              psaci(i,k) = min(psaci(i,k),qci(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! pgaci: Accretion of cloud ice by graupel [HL A17] [LFO 41]
!        (T<T0: I->G)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.qcrmin) then
              egi = exp(0.07*(-supcol))
              acrfac = 2.*rslope3(i,k,3)+2.*diameter*rslope2(i,k,3)            &
                      + diameter**2*rslope(i,k,3)
              pgaci(i,k) = pi*egi*qci(i,k,2)*n0g*abs(vt2ave-vt2i)*acrfac/4.
              pgaci(i,k) = min(pgaci(i,k),qci(i,k,2)/dtcld)
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)   &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! nsacw: Accretion of cloud water by snow 
!        (NC ->) 
!-------------------------------------------------------------
         if(qrs(i,k,2).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
           nsacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)*rslopeb(i,k,2)    &
                       *ncr(i,k,2)*denfac(i,k),ncr(i,k,2)/dtcld)
         endif
!-------------------------------------------------------------
! pgacw: Accretion of cloud water by graupel [HL A6] [LFO 40]
!        (T<T0: C->G, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin .and. qci(i,k,1).gt.qmin) then
            pgacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)*qci(i,k,1)    &
                        *denfac(i,k),qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! ngacw: Accretion of cloud water by graupel
!        (NC-> 
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin .and. ncr(i,k,2).gt.ncmin) then
            ngacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)*ncr(i,k,2)    &
                        *denfac(i,k),ncr(i,k,2)/dtcld)
          endif
!-------------------------------------------------------------
! paacw: Accretion of cloud water by averaged snow/graupel 
!        (T<T0: C->G or S, and T>=T0: C->R) 
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin .and. qrs(i,k,3).gt.qcrmin) then
            paacw(i,k) = (qrs(i,k,2)*psacw(i,k)+qrs(i,k,3)*pgacw(i,k))/(qsum(i,k))
!-------------------------------------------------------------
! naacw: Accretion of cloud water by averaged snow/graupel
!        (Nc->)
!-------------------------------------------------------------
            naacw(i,k) = (qrs(i,k,2)*nsacw(i,k)+qrs(i,k,3)*ngacw(i,k))/(qsum(i,k))
          endif      
!-------------------------------------------------------------
! pracs: Accretion of snow by rain [HL A11] [LFO 27]
!         (T<T0: S->G)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin .and. qrs(i,k,1).gt.qcrmin) then
            if(supcol.gt.0) then
              acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)                        &
                      + 4.*rslope3(i,k,2)*rslope2(i,k,2)*rslope(i,k,1)         &
                      + 1.5*rslope2(i,k,2)*rslope2(i,k,2)*rslope2(i,k,1)
              pracs(i,k) = pi*pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2r-vt2ave)   &
                          *(dens/den(i,k))*acrfac
              pracs(i,k) = min(pracs(i,k),qrs(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! psacr: Accretion of rain by snow [HL A10] [LFO 28]
!         (T<T0:R->S or R->G) (T>=T0: enhance melting of snow)
!-------------------------------------------------------------
            acrfac = 30.*rslope3(i,k,1)*rslope2(i,k,1)*rslope(i,k,2)           &
                     + 5.*rslope2(i,k,1)*rslope2(i,k,1)*rslope2(i,k,2)         &
                     + 2.*rslope3(i,k,1)*rslope3(i,k,2)
            psacr(i,k) = pi*pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2ave-vt2r)     &
                        *(denr/den(i,k))*acrfac
            psacr(i,k) = min(psacr(i,k),qrs(i,k,1)/dtcld)
          endif
          if(qrs(i,k,2).gt.qcrmin .and. ncr(i,k,3).gt.nrmin) then
!-------------------------------------------------------------
! nsacr: Accretion of rain by snow 
!       (T<T0:NR->)
!-------------------------------------------------------------
            acrfac = 1.5*rslope2(i,k,1)*rslope(i,k,2)                          &
                    + 1.0*rslope(i,k,1)*rslope2(i,k,2)+.5*rslope3(i,k,2)        
            nsacr(i,k) = pi*ncr(i,k,3)*n0s*n0sfac(i,k)*abs(vt2ave-vt2r)        &
                        *acrfac
            nsacr(i,k) = min(nsacr(i,k),ncr(i,k,3)/dtcld)
          endif
!-------------------------------------------------------------
! pgacr: Accretion of rain by graupel [HL A12] [LFO 42]
!         (T<T0: R->G) (T>=T0: enhance melting of graupel)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin .and. qrs(i,k,1).gt.qcrmin) then
            acrfac = 30.*rslope3(i,k,1)*rslope2(i,k,1)*rslope(i,k,3)           &
                    + 5.*rslope2(i,k,1)*rslope2(i,k,1)*rslope2(i,k,3)          &
                    + 2.*rslope3(i,k,1)*rslope3(i,k,3)
            pgacr(i,k) = pi*pi*ncr(i,k,3)*n0g*abs(vt2ave-vt2r)*(denr/den(i,k)) &
                        *acrfac
            pgacr(i,k) = min(pgacr(i,k),qrs(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! ngacr: Accretion of rain by graupel 
!        (T<T0: NR->) 
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin .and. ncr(i,k,3).gt.nrmin) then
            acrfac = 1.5*rslope2(i,k,1)*rslope(i,k,3)                          &
                    + 1.0*rslope(i,k,1)*rslope2(i,k,3) + .5*rslope3(i,k,3)   
            ngacr(i,k) = pi*ncr(i,k,3)*n0g*abs(vt2ave-vt2r)*acrfac
            ngacr(i,k) = min(ngacr(i,k),ncr(i,k,3)/dtcld)
          endif
!
!-------------------------------------------------------------
! pgacs: Accretion of snow by graupel [HL A13] [LFO 29]
!        (S->G) : This process is eliminated in V3.0 with the
!        new combined snow/graupel fall speeds
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin .and. qrs(i,k,2).gt.qcrmin) then
            pgacs(i,k) = 0. 
          endif
          if(supcol.le.0) then
            xlf = xlf0
!-------------------------------------------------------------
! pseml: Enhanced melting of snow by accretion of water [HL A34]
!        (T>=T0: S->R)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0.)                                               & 
              pseml(i,k) = min(max(cliq*supcol*(paacw(i,k)+psacr(i,k))         &
                          /xlf,-qrs(i,k,2)/dtcld),0.)
!--------------------------------------------------------------
! nseml: Enhanced melting of snow by accretion of water
!        (T>=T0: ->NR)
!--------------------------------------------------------------
              if  (qrs(i,k,2).gt.qcrmin) then
                sfac = rslope(i,k,2)*n0s*n0sfac(i,k)/qrs(i,k,2)
                nseml(i,k) = -sfac*pseml(i,k)
              endif
!-------------------------------------------------------------
! pgeml: Enhanced melting of graupel by accretion of water [HL A24] [RH84 A21-A22]
!        (T>=T0: G->R)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0.)                                               &
              pgeml(i,k) = min(max(cliq*supcol*(paacw(i,k)+pgacr(i,k))/xlf     &
                          ,-qrs(i,k,3)/dtcld),0.)
!--------------------------------------------------------------
! ngeml: Enhanced melting of graupel by accretion of water 
!         (T>=T0: -> NR)
!--------------------------------------------------------------
              if (qrs(i,k,3).gt.qcrmin) then
                gfac = rslope(i,k,3)*n0g/qrs(i,k,3)
                ngeml(i,k) = -gfac*pgeml(i,k)
              endif
          endif
          if(supcol.gt.0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
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
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (T<T0: V->S or S->V)
!-------------------------------------------------------------
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
!-------------------------------------------------------------
! pgdep: deposition/sublimation rate of graupel [HL A21] [LFO 46]
!        (T<T0: V->G or G->V)
!-------------------------------------------------------------
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
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL 50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0. .and. ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)-pgdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*xni0**1.33
              pigen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))/dtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
            endif
!
!-------------------------------------------------------------
! pgaut: conversion(aggregation) of snow to graupel [HL A4] [LFO 37]
!        (T<T0: S->G)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0.) then
              alpha2 = 1.e-3*exp(0.09*(-supcol))
              pgaut(i,k) = min(max(0.,alpha2*(qrs(i,k,2)-qs0)),qrs(i,k,2)/dtcld)
            endif
          endif
!
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>=T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0. .and. rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psevp(i,k) = (rh(i,k,1)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k,2)   &
                           +precs2*work2(i,k)*coeres)/work1(i,k,1)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0.)
            endif
!-------------------------------------------------------------
! pgevp: Evaporation of melting graupel [HL A25] [RH84 A19]
!       (T>=T0: G->V)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0. .and. rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgevp(i,k) = (rh(i,k,1)-1.)*(precg1*rslope2(i,k,3)               &
                         + precg2*work2(i,k)*coeres)/work1(i,k,1)
              pgevp(i,k) = min(max(pgevp(i,k),-qrs(i,k,3)/dtcld),0.)
            endif
          endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
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
            source = (praut(i,k)+pracw(i,k)+paacw(i,k)+paacw(i,k)-prevp_s(i,k))&
                    *dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor 
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)-pigen(i,k)-pidep(i,k)+praci(i,k)+psaci(i,k)   &
                    +pgaci(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-prevp(i,k)-pracw(i,k)+piacr(i,k)             &
                    +prevp_s(i,k)+psacr(i,k)+pgacr(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor 
            endif
!
!     snow
!
            value = max(qmin,qrs(i,k,2))
            source = -(psdep(i,k)+psaut(i,k)-pgaut(i,k)+paacw(i,k)             &
                     +piacr(i,k)*delta3+praci(i,k)*delta3                      &
                     -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2                 &
                     +psaci(i,k)-pgacs(i,k) )*dtcld
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
            endif
!
!     graupel
!
            value = max(qmin,qrs(i,k,3))
            source = -(pgdep(i,k)+pgaut(i,k)                                   &
                     +piacr(i,k)*(1.-delta3)+praci(i,k)*(1.-delta3)            &
                     +psacr(i,k)*(1.-delta2)+pracs(i,k)*(1.-delta2)            &
                     +pgaci(i,k)+paacw(i,k)+pgacr(i,k)+pgacs(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgdep(i,k) = pgdep(i,k)*factor
              pgaut(i,k) = pgaut(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
            endif
!
!     cloud
!
            value = max(ncmin,ncr(i,k,2))
            source = (nraut(i,k)+nccol(i,k)+nracw(i,k)                         &
                    +naacw(i,k)+naacw(i,k)-nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              naacw(i,k) = naacw(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif
!
!     rain
!
            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)+nrcol(i,k)+niacr(i,k)+nsacr(i,k)+ngacr(i,k)  &
                     +nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              niacr(i,k) = niacr(i,k)*factor
              nsacr(i,k) = nsacr(i,k)*factor
              ngacr(i,k) = ngacr(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pgdep(i,k)+pigen(i,k)+pidep(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                           +paacw(i,k)+paacw(i,k)+prevp_s(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                           +prevp(i,k)-piacr(i,k)-pgacr(i,k)                   &
                           -psacr(i,k)-prevp_s(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+praci(i,k)                 &
                           +psaci(i,k)+pgaci(i,k)-pigen(i,k)-pidep(i,k))       &
                           *dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)+paacw(i,k)      &
                           -pgaut(i,k)+piacr(i,k)*delta3                       &
                           +praci(i,k)*delta3+psaci(i,k)-pgacs(i,k)            &
                           -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2)          &
                           *dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgdep(i,k)+pgaut(i,k)                 &
                           +piacr(i,k)*(1.-delta3)                             &
                           +praci(i,k)*(1.-delta3)+psacr(i,k)*(1.-delta2)      &
                           +pracs(i,k)*(1.-delta2)+pgaci(i,k)+paacw(i,k)       &
                           +pgacr(i,k)+pgacs(i,k))*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                           -naacw(i,k)-naacw(i,k)+nrevp(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)-nrcol(i,k)-niacr(i,k)      &
                           -nsacr(i,k)-ngacr(i,k)-nrevp(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pgdep(i,k)+pidep(i,k)+pigen(i,k))       &
                      -xl(i,k)*prevp(i,k)-xlf*(piacr(i,k)+paacw(i,k)           &
                      +paacw(i,k)+pgacr(i,k)+psacr(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source= (praut(i,k)+pracw(i,k)+paacw(i,k)+paacw(i,k)-prevp_s(i,k)) &
                   *dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-paacw(i,k)-praut(i,k)+pseml(i,k)+pgeml(i,k)             &
                     +prevp_s(i,k)-pracw(i,k)-paacw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              paacw(i,k) = paacw(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
              prevp_s(i,k) = prevp_s(i,k)*factor
            endif
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(pgacs(i,k)-pseml(i,k)-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              psevp(i,k) = psevp(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
            endif
!
!     graupel
!
            value = max(qcrmin,qrs(i,k,3))
            source=-(pgacs(i,k)+pgevp(i,k)+pgeml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              pgevp(i,k) = pgevp(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
            endif
!
!     cloud
!
            value = max(ncmin,ncr(i,k,2))
            source = (+nraut(i,k)+nccol(i,k)+nracw(i,k)+naacw(i,k)             &
                     +naacw(i,k)-nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nccol(i,k) = nccol(i,k)*factor
              nracw(i,k) = nracw(i,k)*factor
              naacw(i,k) = naacw(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
            endif
!
!     rain
!
            value = max(nrmin,ncr(i,k,3))
            source = (-nraut(i,k)+nrcol(i,k)-nseml(i,k)-ngeml(i,k)             &
                      +nrevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              nraut(i,k) = nraut(i,k)*factor
              nrcol(i,k) = nrcol(i,k)*factor
              nrevp(i,k) = nrevp(i,k)*factor
              nseml(i,k) = nseml(i,k)*factor
              ngeml(i,k) = ngeml(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psevp(i,k)+pgevp(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                    +prevp_s(i,k)+paacw(i,k)+paacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                    +prevp(i,k)-prevp_s(i,k)+paacw(i,k)+paacw(i,k)-pseml(i,k)  &
                    -pgeml(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psevp(i,k)-pgacs(i,k)                 &
                    +pseml(i,k))*dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgacs(i,k)+pgevp(i,k)                 &
                    +pgeml(i,k))*dtcld,0.)
            ncr(i,k,2) = max(ncr(i,k,2)+(-nraut(i,k)-nccol(i,k)-nracw(i,k)     &
                   -naacw(i,k)-naacw(i,k)+nrevp(i,k))*dtcld,0.)
            ncr(i,k,3) = max(ncr(i,k,3)+(nraut(i,k)-nrcol(i,k)+nseml(i,k)      &
                           +ngeml(i,k)-nrevp(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k)+pgevp(i,k))              &
                      -xlf*(pseml(i,k)+pgeml(i,k))
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
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
!---------------------------------------------------------------
!  put the inital CCN number concentration
!
          if(ncr(i,k,1).eq.0.) ncr(i,k,1) = ccn0
!---------------------------------------------------------------
! rate of change of cloud drop concentration due to CCN activation
! pcact: V -> C                                            [KK 14]
! ncact: NCCN -> NC                                        [KK 12]
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
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0. .and. work1(i,k,1).lt.0.)                        & 
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
!---------------------------------------------------------------
! ncevp: evpration of Cloud number concentration  [CP  ]
!
          if(pcond(i,k).eq.-qci(i,k,1)/dtcld) then
            ncr(i,k,2) = 0.
            ncr(i,k,1) = ncr(i,k,1)+ncr(i,k,2)
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
        enddo
      enddo
      enddo                  ! big loops
  END SUBROUTINE wdm62d
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
  SUBROUTINE wdm6init(den0,denr,dens,cl,cpv, ccn0, allowed_to_read)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable
   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv,ccn0
   LOGICAL, INTENT(IN) :: allowed_to_read
   REAL :: pi
!
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
!
   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
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
   pacrg = pi*n0g*avtg*g3pbg*.25
   pvtg = avtg*g4pbg/6.
   precg1 = 2.*pi*n0g*.78
   precg2 = 2.*pi*n0g*.31*avtg**.5*g5pbgo2
   pidn0g =  pi*deng*n0g
!
   rslopecmax = 1./lamdacmax
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rslopegmax = 1./lamdagmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rslopegbmax = rslopegmax ** bvtg
   rslopec2max = rslopecmax * rslopecmax
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rslopeg2max = rslopegmax * rslopegmax
   rslopec3max = rslopec2max * rslopecmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
   rslopeg3max = rslopeg2max * rslopegmax
!
  END SUBROUTINE wdm6init
END MODULE module_mp_wdm6

MODULE module_mp_wsm6
!
!
   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120.
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6
   REAL, PARAMETER, PRIVATE :: n0g = 4.e6
   REAL, PARAMETER, PRIVATE :: avtr = 841.9
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5 ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: peaut = .55   ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8   ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5 ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER, PRIVATE :: avts = 11.72
   REAL, PARAMETER, PRIVATE :: bvts = .41
   REAL, PARAMETER, PRIVATE :: avtg = 330.
   REAL, PARAMETER, PRIVATE :: bvtg = 0.8
   REAL, PARAMETER, PRIVATE :: deng = 500.
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e11 ! t=-90C unlimited
   REAL, PARAMETER, PRIVATE :: lamdarmax = 8.e4
   REAL, PARAMETER, PRIVATE :: lamdasmax = 1.e5
   REAL, PARAMETER, PRIVATE :: lamdagmax = 6.e4
   REAL, PARAMETER, PRIVATE :: betai = .6
   REAL, PARAMETER, PRIVATE :: xn0 = 1.e-2
   REAL, PARAMETER, PRIVATE :: dicon = 11.9
   REAL, PARAMETER, PRIVATE :: di0 = 12.9e-6
   REAL, PARAMETER, PRIVATE :: dimax = 500.e-6
   REAL, PARAMETER, PRIVATE :: n0s = 2.e6             ! temperature dependent n0s
   REAL, PARAMETER, PRIVATE :: alpha = .12        ! .122 exponen factor for n0s
   REAL, PARAMETER, PRIVATE :: pfrz1 = 100.
   REAL, PARAMETER, PRIVATE :: pfrz2 = 0.66
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-9
   REAL, PARAMETER, PRIVATE :: t40c = 233.16
   REAL, PARAMETER, PRIVATE :: eacrc = 1.0
   REAL, PARAMETER, PRIVATE :: dens  =  100.0
   REAL, PARAMETER, PRIVATE :: qs0   =  6.e-4   ! pgaut
   REAL, SAVE ::                                     &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr,&
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,   &
             bvtr6,g6pbr,                            &
             precr1,precr2,xm0,xmmax,roqimax,bvts1,  &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,    &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r,&
             pidn0s,xlv1,pacrc,                      &
             bvtg1,bvtg2,bvtg3,bvtg4,g1pbg,          &
             g3pbg,g4pbg,g5pbgo2,pvtg,pacrg,         &
             precg1,precg2,pidn0g,                   &
             vt2i,vt2r,vt2s,vt2g,acrfac,egs,egi,     &
             rslopermax,rslopesmax,rslopegmax,       &
             rsloperbmax,rslopesbmax,rslopegbmax,    &
             rsloper2max,rslopes2max,rslopeg2max,    &
             rsloper3max,rslopes3max,rslopeg3max
CONTAINS
!===================================================================
!
  SUBROUTINE wsm6(th, q, qc, qr, qi, qs, qg                        &
                 ,w, den, pii, p, delz                             &
                 ,delt,g, cpd, cpv, rd, rv, t0c                    &
                 ,ep1, ep2, qmin                                   &
                 ,XLS, XLV0, XLF0, den0, denr                      &
                 ,cliq,cice,psat                                   &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
               ! Optional
                 ,rain, rainncv                                    &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a GRAUPEL phase ice microphyiscs scheme (WSM6) of the WRF
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. Related changes in ice-microphysics and description of
!  other microphysics are described in Hong et al. (2004).
!  all units are m.k.s. and source/sink terms are kgkg-1s-1.
!
! WRFSMMP cloud scheme
!
!  Coded by Song-You Hong and Jeong-Ock Lim (Yonsei Univ.)
!           Jimy Dudhia (NCAR) and Shu-Hua Chen (UC Davis)
!           Summer 2003
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Lim (2004) Master thesis, Yonsei Univ.
!             Lin, Farley, Orville (LFO, 1983) J. Appl. Meteor.
!             Rutledge, Hobbs (RH, 1983) J. Atmos. Sci.
!             Rutledge, Hobbs (RH, 1984) J. Atmos. Sci.
!
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                             th,  &
                                                              q,  &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN   ) ::                                       w, &
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
!
! Optional
!
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        OPTIONAL,                                                 &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::   qrs
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
         ENDDO
         ENDDO
         CALL wsm62D(t, q(ims,kms,j), qci, qrs                     &
                    ,w(ims,kms,j), den(ims,kms,j)                  &
                    ,p(ims,kms,j), delz(ims,kms,j)                 &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,j                                             &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                  ! optional
                    ,rain(ims,j),rainncv(ims,j)                    &
                                                                   )
         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            qg(i,k,j) = qrs(i,k,3)
         ENDDO
         ENDDO
      ENDDO
  END SUBROUTINE wsm6
!===================================================================
!
  SUBROUTINE wsm62D(t, q, qci, qrs, w, den, p, delz               &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                ! Optional
                   ,rain,rainncv                                  &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte,  &
                                      lat
  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL, DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci
  REAL, DIMENSION( its:ite , kts:kte, 3 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qrs
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                       w, &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
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
!
! Optional
!
  REAL, DIMENSION( ims:ime ),                                     &
        OPTIONAL,                                                 &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte , 3) ::                      &
        rh, qs, rslope, rslope2, rslope3, rslopeb,                &
        paut, pres, falk, fall, work1
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte) ::                          &
        pracw, psacw, pgacw, pgacr, pgacs, psaci, pgml, praci,    &
        piacr, pracs, psacr, pgaci, pseml, pgeml
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pgen, pisd, pcon, xl, cpm, work2, psml, psev, denfac,     &
        xni, pgev,n0sfac
  INTEGER, DIMENSION( its:ite ) :: mstep, numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdar, lamdas, lamdag, diffus,        &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, holdrg, supcol, pvt,             &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, xlwork2, factor, source, value,              &
            xlf, pfrzdtc, pfrzdtr, supice, alpha2, delta2, delta3
  REAL  :: holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                   &
            iprt, latd, lond, loop, loops, ifsat, n
!
!=================================================================
!   compute internal functions
!
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
!     tvcal(x,y) = x+x*ep1*max(y,qmin)
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
!
      lamdar(x,y)=(pidn0r/(x*y))**.25
      lamdas(x,y,z)=(pidn0s*z/(x*y))**.25
      lamdag(x,y)=(pidn0g/(x*y))**.25
!
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!
      diffus(x,y) = 8.794e-5*x**1.81/y
      viscos(x,y) = 1.496e-6*x**1.5/(x+120.)/y
      xka(x,y) = 1.414e3*viscos(x,y)*y
      diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
      venfac(a,b,c) = (viscos(b,c)/diffus(b,a))**(.3333333)       &
             /viscos(b,c)**(.5)*(den0/c)**0.25
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
        flgcld(i) = .true.
      enddo
!
      do k = kts, kte
        do i = its, ite
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
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
          pres(i,k,1) = 0.
          pres(i,k,2) = 0.
          pres(i,k,3) = 0.
          paut(i,k,1) = 0.
          paut(i,k,2) = 0.
          paut(i,k,3) = 0.
          pracw(i,k) = 0.
          praci(i,k) = 0.
          piacr(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pracs(i,k) = 0.
          psacr(i,k) = 0.
          pgacw(i,k) = 0.
          pgaci(i,k) = 0.
          pgacr(i,k) = 0.
          pgacs(i,k) = 0.
          pgen(i,k) = 0.
          pisd(i,k) = 0.
          pcon(i,k) = 0.
          psml(i,k) = 0.
          pgml(i,k) = 0.
          pseml(i,k) = 0.
          pgeml(i,k) = 0.
          psev(i,k) = 0.
          pgev(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          falk(i,k,3) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fall(i,k,3) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          xni(i,k) = 1.e3
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
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k))
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
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
          if(qrs(i,k,3).le.qcrmin)then
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
          xni(i,k) = min(max(5.38e7*(den(i,k)                           &
                    *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
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
          numdt(i) = max(nint(max(work1(i,k,1),work1(i,k,2),work1(i,k,3)) &
                    *dtcld+.5),1)
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
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              falk(i,k,3) = den(i,k)*qrs(i,k,3)*work1(i,k,3)/mstep(i)
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
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              falk(i,k,3) = den(i,k)*qrs(i,k,3)*work1(i,k,3)/mstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              fall(i,k,3) = fall(i,k,3)+falk(i,k,3)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)    &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)    &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,3) = max(qrs(i,k,3)-(falk(i,k,3)-falk(i,k+1,3)    &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
        do k = kte, kts, -1
          do i = its, ite
            if(n.le.mstep(i).and.t(i,k).gt.t0c) then
!---------------------------------------------------------------
! psml: melting of snow [RH83 A25]
!       (T>T0: S->R)
!---------------------------------------------------------------
              xlf = xlf0
              work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
              if(qrs(i,k,2).gt.0.) then
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
                psml(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2. &
                           *n0sfac(i,k)*(precs1*rslope2(i,k,2)          &
                           +precs2*work2(i,k)*coeres)
                psml(i,k) = min(max(psml(i,k)*dtcld/mstep(i),           &
                            -qrs(i,k,2)/mstep(i)),0.)
                qrs(i,k,2) = qrs(i,k,2) + psml(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psml(i,k)
                t(i,k) = t(i,k) + xlf/cpm(i,k)*psml(i,k)
              endif
!---------------------------------------------------------------
! pgml: melting of graupel [LFO 47]
!       (T>T0: G->R)
!---------------------------------------------------------------
              if(qrs(i,k,3).gt.0.) then
                coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
                pgml(i,k) = xka(t(i,k),den(i,k))/xlf                    &
                           *(t0c-t(i,k))*(precg1*rslope2(i,k,3)         &
                           +precg2*work2(i,k)*coeres)
                pgml(i,k) = min(max(pgml(i,k)*dtcld/mstep(i),           &
                            -qrs(i,k,3)/mstep(i)),0.)
                qrs(i,k,3) = qrs(i,k,3) + pgml(i,k)
                qrs(i,k,1) = qrs(i,k,1) - pgml(i,k)
                t(i,k) = t(i,k) + xlf/cpm(i,k)*pgml(i,k)
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
            diameter  = min(dicon * sqrt(xmi),dimax)
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
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)      &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,kts,1)+fall(i,kts,2)+fall(i,kts,3)
        if(fallsum.gt.0.) then
          IF (PRESENT(rainncv)) rainncv(i) = fallsum*delz(i,kts)/denr*dtcld*1000.
          IF (PRESENT(rain   )) rain(i) = fallsum*delz(i,kts)/denr*dtcld*1000. + rain(i)
        endif
      enddo
!
!---------------------------------------------------------------
! piml: instantaneous melting of cloud ice [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0.and.qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40..and.qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qci(i,k,1).gt.qmin) then
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                  &
               *den(i,k)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! pfrz: freezing of rain water [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qrs(i,k,1).gt.0.) then
            pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k)             &
                  *(exp(pfrz2*supcol)-1.)*rslope3(i,k,1)**2             &
                  *rslope(i,k,1)*dtcld,qrs(i,k,1))
            qrs(i,k,3) = qrs(i,k,3) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
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
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k))
            rslopeb(i,k,1) = rslope(i,k,1)**bvtr
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
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
          if(qrs(i,k,3).le.qcrmin)then
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
! - follows the processes in RH83 and LFO except for autoconcersion
!
!===============================================================
!
      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!---------------------------------------------------------------
! paut1: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          if(qci(i,k,1).gt.qc0) then
            paut(i,k,1) = qck1*qci(i,k,1)**(7./3.)
            paut(i,k,1) = min(paut(i,k,1),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pracw(i,k) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)        &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pres1: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope2(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            pres(i,k,1) = (rh(i,k,1)-1.)*(precr1*rslope2(i,k,1)         &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(pres(i,k,1).lt.0.) then
              pres(i,k,1) = max(pres(i,k,1),-qrs(i,k,1)/dtcld)
              pres(i,k,1) = max(pres(i,k,1),satdt/2)
            else
              pres(i,k,1) = min(pres(i,k,1),satdt/2)
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
          xni(i,k) = min(max(5.38e7*(den(i,k)                           &
                       *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          eacrs = exp(0.05*(-supcol))
!
          xmi = den(i,k)*qci(i,k,2)/xni(i,k)
          diameter  = min(dicon * sqrt(xmi),dimax)
          vt2r=pvtr*rslopeb(i,k,1)*denfac(i,k)
          vt2s=pvts*rslopeb(i,k,2)*denfac(i,k)
          vt2g=pvtg*rslopeb(i,k,3)*denfac(i,k)
          if(supcol.gt.0.and.qci(i,k,2).gt.qmin) then
            if(qrs(i,k,1).gt.qcrmin) then
!-------------------------------------------------------------
! praci: Accretion of cloud ice by rain [LFO 25]
!        (T<T0: I->R)
!-------------------------------------------------------------
              praci(i,k) = pacrr*rslope3(i,k,1)*rslopeb(i,k,1)          &
                          *qci(i,k,2)*denfac(i,k)
              praci(i,k) = min(praci(i,k),qci(i,k,2)/dtcld)
!-------------------------------------------------------------
! piacr: Accretion of rain by cloud ice [LFO 26]
!        (T<T0: R->S or R->G)
!-------------------------------------------------------------
              piacr(i,k) = pi**2*avtr*n0r*denr*xni(i,k)*denfac(i,k)     &
                          *g6pbr*rslope3(i,k,1)*rslope3(i,k,1)          &
                          *rslopeb(i,k,1)/24./den(i,k)
              piacr(i,k) = min(piacr(i,k),qrs(i,k,1)/dtcld)
            endif
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by snow [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.qcrmin) then
              psaci(i,k) = pacrs*n0sfac(i,k)*eacrs*rslope3(i,k,2)       &
                          *rslopeb(i,k,2)*qci(i,k,2)*denfac(i,k)
              psaci(i,k) = min(psaci(i,k),qci(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! pgaci: Accretion of cloud ice by graupel [LFO 41]
!        (T<T0: I->G)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.qcrmin) then
              egi = exp(0.07*(-supcol))
              pgaci(i,k) = pacrg*egi*rslope3(i,k,3)*rslopeb(i,k,3)      &
                          *qci(i,k,2)*denfac(i,k)
              pgaci(i,k) = min(pgaci(i,k),qci(i,k,2)/dtcld)
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [LFO 24]
!        (T<T0: C->G, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)           &
                        *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)          &
                        ,qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pgacw: Accretion of cloud water by graupel [LFO 40]
!        (T<T0: C->G, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pgacw(i,k) = min(pacrg*rslope3(i,k,3)*rslopeb(i,k,3)        &
                        *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pracs: Accretion of snow by rain [LFO 27]
!         (T<T0: S->G)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            if(supcol.gt.0) then
              acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)*rslope(i,k,1)   &
                      +2.*rslope3(i,k,2)*rslope2(i,k,2)*rslope2(i,k,1)  &
                      +.5*rslope2(i,k,2)*rslope2(i,k,2)*rslope3(i,k,1)
              pracs(i,k) = pi**2*n0r*n0s*n0sfac(i,k)*abs(vt2r-vt2s)     &
                          *(dens/den(i,k))*acrfac
              pracs(i,k) = min(pracs(i,k),qrs(i,k,2)/dtcld)
            endif
!-------------------------------------------------------------
! psacr: Accretion of rain by snow [LFO 28]
!         (T<T0:R->S or R->G) (T>=T0: enhance melting of snow)
!-------------------------------------------------------------
            acrfac = 5.*rslope3(i,k,1)*rslope3(i,k,1)*rslope(i,k,2)     &
                    +2.*rslope3(i,k,1)*rslope2(i,k,1)*rslope2(i,k,2)    &
                    +.5*rslope2(i,k,1)*rslope2(i,k,1)*rslope3(i,k,2)
            psacr(i,k) = pi**2*n0r*n0s*n0sfac(i,k)*abs(vt2s-vt2r)       &
                        *(denr/den(i,k))*acrfac
            psacr(i,k) = min(psacr(i,k),qrs(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pgacr: Accretion of rain by graupel [LFO 42]
!         (T<T0: R->G) (T>=T0: enhance melting of graupel)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qrs(i,k,1).gt.qcrmin) then
            acrfac = 5.*rslope3(i,k,1)*rslope3(i,k,1)*rslope(i,k,3)     &
                    +2.*rslope3(i,k,1)*rslope2(i,k,1)*rslope2(i,k,3)    &
                    +.5*rslope2(i,k,1)*rslope2(i,k,1)*rslope3(i,k,3)
            pgacr(i,k) = pi**2*n0r*n0g*abs(vt2g-vt2r)*(denr/den(i,k))   &
                        *acrfac
            pgacr(i,k) = min(pgacr(i,k),qrs(i,k,1)/dtcld)
          endif
!-------------------------------------------------------------
! pgacs: Accretion of snow by graupel [LFO 29]
!        (S->G)
!-------------------------------------------------------------
          if(qrs(i,k,3).gt.qcrmin.and.qrs(i,k,2).gt.qcrmin) then
            acrfac = 5.*rslope3(i,k,2)*rslope3(i,k,2)*rslope(i,k,3)     &
                    +2.*rslope3(i,k,2)*rslope2(i,k,2)*rslope2(i,k,3)    &
                    +.5*rslope2(i,k,2)*rslope2(i,k,2)*rslope3(i,k,3)
            if(supcol.gt.0) then
              egs = exp(-0.09*supcol)
            else
              egs = 1.
            endif
            pgacs(i,k) = pi**2*egs*n0s*n0sfac(i,k)*n0g*abs(vt2g-vt2s)   &
                        *(dens/den(i,k))*acrfac
            pgacs(i,k) = min(pgacs(i,k),qrs(i,k,2)/dtcld)
          endif
          if(supcol.le.0) then
            xlf = xlf0
!-------------------------------------------------------------
! pseml: Enhanced melting of snow by accretion of water
!        (T>=T0: S->R)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0.)                                        &
              pseml(i,k) = min(max(cliq*supcol*(psacw(i,k)+psacr(i,k))  &
                          /xlf,-qrs(i,k,2)/dtcld),0.)
!-------------------------------------------------------------
! pgeml: Enhanced melting of graupel by accretion of water [RH84 A21-A22]
!        (T>=T0: G->R)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0.)                                        &
              pgeml(i,k) = min(max(cliq*supcol*(pgacw(i,k)+pgacr(i,k))  &
                          /xlf,-qrs(i,k,3)/dtcld),0.)
          endif
          if(supcol.gt.0) then
!-------------------------------------------------------------
! pisd: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              pisd(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-pres(i,k,1)
              if(pisd(i,k).lt.0.) then
                pisd(i,k) = max(max(pisd(i,k),satdt/2),supice)
                pisd(i,k) = max(pisd(i,k),-qci(i,k,2)/dtcld)
              else
                pisd(i,k) = min(min(pisd(i,k),satdt/2),supice)
              endif
              if(abs(pres(i,k,1)+pisd(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pres2: deposition/sublimation rate of snow [HDC 14]
!        (T<T0: V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              pres(i,k,2) = (rh(i,k,2)-1.)*n0sfac(i,k)*(precs1          &
                           *rslope2(i,k,2)+precs2*work2(i,k)            &
                           *coeres)/work1(i,k,2)
              supice = satdt-pres(i,k,1)-pisd(i,k)
              if(pres(i,k,2).lt.0.) then
                pres(i,k,2) = max(pres(i,k,2),-qrs(i,k,2)/dtcld)
                pres(i,k,2) = max(max(pres(i,k,2),satdt/2),supice)
              else
                pres(i,k,2) = min(min(pres(i,k,2),satdt/2),supice)
              endif
              if(abs(pres(i,k,1)+pisd(i,k)+pres(i,k,2)).ge.abs(satdt))  &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pres3: deposition/sublimation rate of graupel [LFO 46]
!        (T<T0: V->G or G->V)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pres(i,k,3) = (rh(i,k,2)-1.)*(precg1*rslope2(i,k,3)       &
                              +precg2*work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-pres(i,k,1)-pisd(i,k)-pres(i,k,2)
              if(pres(i,k,3).lt.0.) then
                pres(i,k,3) = max(pres(i,k,3),-qrs(i,k,3)/dtcld)
                pres(i,k,3) = max(max(pres(i,k,3),satdt/2),supice)
              else
                pres(i,k,3) = min(min(pres(i,k,3),satdt/2),supice)
              endif
              if(abs(pres(i,k,1)+pisd(i,k)+pres(i,k,2)+pres(i,k,3)).ge. &
                abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pgen: generation(nucleation) of ice from vapor [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-pres(i,k,1)-pisd(i,k)-pres(i,k,2)-pres(i,k,3)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*xni0**1.33
              pgen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))    &
                         /dtcld)
              pgen(i,k) = min(min(pgen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! paut2: conversion(aggregation) of ice to snow [HDC 12]
!        (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
              paut(i,k,2) = max(0.,(qci(i,k,2)-qimax)/dtcld)
            endif
!
!-------------------------------------------------------------
! paut3: conversion(aggregation) of snow to graupel [LFO 37]
!        (T<T0: S->G)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0.) then
              alpha2 = 1.e-3*exp(0.09*(-supcol))
              paut(i,k,3) = min(max(0.,alpha2*(qrs(i,k,2)-qs0))         &
                           ,qrs(i,k,2)/dtcld)
            endif
          endif
!
!-------------------------------------------------------------
! psev: Evaporation of melting snow [RH83 A27]
!       (T>=T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psev(i,k) = (rh(i,k,1)-1.)*n0sfac(i,k)*(precs1            &
                           *rslope2(i,k,2)+precs2*work2(i,k)            &
                           *coeres)/work1(i,k,1)
              psev(i,k) = min(max(psev(i,k),-qrs(i,k,2)/dtcld),0.)
            endif
!-------------------------------------------------------------
! pgev: Evaporation of melting graupel [RH84 A19]
!       (T>=T0: G->V)
!-------------------------------------------------------------
            if(qrs(i,k,3).gt.0..and.rh(i,k,1).lt.1.) then
              coeres = rslope2(i,k,3)*sqrt(rslope(i,k,3)*rslopeb(i,k,3))
              pgev(i,k) = (rh(i,k,1)-1.)*(precg1*rslope2(i,k,3)         &
                         +precg2*work2(i,k)*coeres)/work1(i,k,1)
              pgev(i,k) = min(max(pgev(i,k),-qrs(i,k,3)/dtcld),0.)
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
          if(qrs(i,k,1).lt.1.e-4.and.qrs(i,k,2).lt.1.e-4) delta2=1.
          if(qrs(i,k,1).lt.1.e-4) delta3=1.
          if(t(i,k).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (paut(i,k,1)+pracw(i,k)+psacw(i,k)+pgacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              pgacw(i,k) = pgacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (paut(i,k,2)-pgen(i,k)-pisd(i,k)+praci(i,k)        &
                    +psaci(i,k)+pgaci(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,2) = paut(i,k,2)*factor
              pgen(i,k) = pgen(i,k)*factor
              pisd(i,k) = pisd(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-paut(i,k,1)-pres(i,k,1)-pracw(i,k)+piacr(i,k)    &
                    +psacr(i,k)+pgacr(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pres(i,k,1) = pres(i,k,1)*factor
              pracw(i,k) = pracw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
            endif
!
!     snow
!
            value = max(qmin,qrs(i,k,2))
            source = -(pres(i,k,2)+paut(i,k,2)-paut(i,k,3)              &
                     +piacr(i,k)*delta3+praci(i,k)*delta3               &
                     -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2          &
                     +psaci(i,k)-pgacs(i,k) )*dtcld
            if (source.gt.value) then
              factor = value/source
              pres(i,k,2) = pres(i,k,2)*factor
              paut(i,k,2) = paut(i,k,2)*factor
              paut(i,k,3) = paut(i,k,3)*factor
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
            source = -(pres(i,k,3)+paut(i,k,3)+psacw(i,k)               &
                     +piacr(i,k)*(1.-delta3)+praci(i,k)*(1.-delta3)     &
                     +psacr(i,k)*(1.-delta2)+pracs(i,k)*(1.-delta2)     &
                     +pgaci(i,k)+pgacw(i,k)+pgacr(i,k)+pgacs(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pres(i,k,3) = pres(i,k,3)*factor
              paut(i,k,3) = paut(i,k,3)*factor
              psacw(i,k) = psacw(i,k)*factor
              piacr(i,k) = piacr(i,k)*factor
              praci(i,k) = praci(i,k)*factor
              psacr(i,k) = psacr(i,k)*factor
              pracs(i,k) = pracs(i,k)*factor
              pgacw(i,k) = pgacw(i,k)*factor
              pgaci(i,k) = pgaci(i,k)*factor
              pgacr(i,k) = pgacr(i,k)*factor
              pgacs(i,k) = pgacs(i,k)*factor
            endif
!
            work2(i,k)=-(pres(i,k,1)+pres(i,k,2)+pres(i,k,3)+pgen(i,k)   &
                       +pisd(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pracw(i,k)          &
                           +psacw(i,k)+pgacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pracw(i,k)          &
                           +pres(i,k,1)-piacr(i,k)-pgacr(i,k)            &
                           -psacr(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(paut(i,k,2)+praci(i,k)          &
                           +psaci(i,k)+pgaci(i,k)-pgen(i,k)-pisd(i,k))   &
                           *dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(pres(i,k,2)+paut(i,k,2)         &
                           -paut(i,k,3)+piacr(i,k)*delta3                &
                           +praci(i,k)*delta3+psaci(i,k)-pgacs(i,k)      &
                           -pracs(i,k)*(1.-delta2)+psacr(i,k)*delta2)    &
                           *dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pres(i,k,3)+paut(i,k,3)         &
                           +psacw(i,k)+piacr(i,k)*(1.-delta3)            &
                           +praci(i,k)*(1.-delta3)+psacr(i,k)*(1.-delta2)&
                           +pracs(i,k)*(1.-delta2)+pgaci(i,k)+pgacw(i,k) &
                           +pgacr(i,k)+pgacs(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(pres(i,k,2)+pres(i,k,3)+pisd(i,k)+pgen(i,k)) &
                      -xl(i,k)*pres(i,k,1)-xlf*(piacr(i,k)+psacw(i,k)    &
                      +pgacw(i,k)+pgacr(i,k)+psacr(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source=(paut(i,k,1)+pracw(i,k)+psacw(i,k)+pgacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              pgacw(i,k) = pgacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-psacw(i,k)-paut(i,k,1)+pseml(i,k)+pgeml(i,k)     &
                     -pracw(i,k)-pgacw(i,k)-pres(i,k,1))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pres(i,k,1) = pres(i,k,1)*factor
              pgacw(i,k) = pgacw(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
            endif
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(pgacs(i,k)-pseml(i,k)-psev(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              psev(i,k) = psev(i,k)*factor
              pseml(i,k) = pseml(i,k)*factor
            endif
!
!     graupel
!
            value = max(qcrmin,qrs(i,k,3))
            source=-(pgacs(i,k)+pgev(i,k)+pgeml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              pgacs(i,k) = pgacs(i,k)*factor
              pgev(i,k) = pgev(i,k)*factor
              pgeml(i,k) = pgeml(i,k)*factor
            endif
            work2(i,k)=-(pres(i,k,1)+psev(i,k)+pgev(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pracw(i,k)         &
                    +psacw(i,k)+pgacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pracw(i,k)         &
                    +pres(i,k,1)+psacw(i,k)+pgacw(i,k)-pseml(i,k)       &
                    -pgeml(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psev(i,k)-pgacs(i,k)           &
                    +pseml(i,k))*dtcld,0.)
            qrs(i,k,3) = max(qrs(i,k,3)+(pgacs(i,k)+pgev(i,k)           &
                    +pgeml(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(pres(i,k,1)+psev(i,k)+pgev(i,k))        &
                      -xlf*(pseml(i,k)+pgeml(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!  pcon: condensational/evaporational rate of cloud water [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcon(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                   &
            pcon(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k) = q(i,k)-pcon(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcon(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcon(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo
!
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
  END SUBROUTINE wsm62d
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
      REAL t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti,  &
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
      if(t.lt.ttp.and.ice.eq.1) then
        fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
      else
        fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END FUNCTION fpvs
!-------------------------------------------------------------------
  SUBROUTINE wsm6init(den0,denr,dens,cl,cpv,allowed_to_read)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable
   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv
   LOGICAL, INTENT(IN) :: allowed_to_read
   REAL :: pi
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
   bvtr1 = 1.+bvtr
   bvtr2 = 2.5+.5*bvtr
   bvtr3 = 3.+bvtr
   bvtr4 = 4.+bvtr
   bvtr6 = 6.+bvtr
   g1pbr = rgmma(bvtr1)
   g3pbr = rgmma(bvtr3)
   g4pbr = rgmma(bvtr4)            ! 17.837825
   g6pbr = rgmma(bvtr6)
   g5pbro2 = rgmma(bvtr2)          ! 1.8273
   pvtr = avtr*g4pbr/6.
   eacrr = 1.0
   pacrr = pi*n0r*avtr*g3pbr*.25*eacrr
   precr1 = 2.*pi*n0r*.78
   precr2 = 2.*pi*n0r*.31*avtr**.5*g5pbro2
   xm0  = (di0/dicon)**2
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
   pidn0r =  pi*denr*n0r
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
   pacrg = pi*n0g*avtg*g3pbg*.25
   g5pbgo2 = rgmma(bvtg2)
   pvtg = avtg*g4pbg/6.
!  pacrg = pi*n0g*avtg*g3pbg*.25
   precg1 = 2.*pi*n0g*.78
   precg2 = 2.*pi*n0g*.31*avtg**.5*g5pbgo2
   pidn0g =  pi*deng*n0g
!
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rslopegmax = 1./lamdagmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rslopegbmax = rslopegmax ** bvtg
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rslopeg2max = rslopegmax * rslopegmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
   rslopeg3max = rslopeg2max * rslopegmax
!
  END SUBROUTINE wsm6init
END MODULE module_mp_wsm6

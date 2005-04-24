MODULE module_mp_ncloud5

   REAL, PARAMETER, PRIVATE :: dtcldcr     = 240.
   INTEGER, PARAMETER, PRIVATE :: mstepmax = 100

   REAL, PARAMETER, PRIVATE :: r0 = .8e-5 ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6
   REAL, PARAMETER, PRIVATE :: avtr = 841.9
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8
   REAL, PARAMETER, PRIVATE :: peaut = .55   ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8   ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5 ! the dynamic viscosity kgm-1s-1

   REAL, PARAMETER, PRIVATE :: avts = 16.2
   REAL, PARAMETER, PRIVATE :: bvts = .527
   REAL, PARAMETER, PRIVATE :: xncmax =  1.e8
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e9
   REAL, PARAMETER, PRIVATE :: betai = .6
   REAL, PARAMETER, PRIVATE :: xn0 = 1.e-2
   REAL, PARAMETER, PRIVATE :: dicon = 16.3
   REAL, PARAMETER, PRIVATE :: di0 = 12.9e-6*.8
   REAL, PARAMETER, PRIVATE :: dimax = 400.e-6
   REAL, PARAMETER, PRIVATE :: n0s = 2.e6             ! temperature dependent n0s
   REAL, PARAMETER, PRIVATE :: alpha = 1./8.18        ! .122 exponen factor for n0s
   REAL, PARAMETER, PRIVATE :: pfrz1 = 100.
   REAL, PARAMETER, PRIVATE :: pfrz2 = 0.66
!  REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e15
   REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e5
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-6

   REAL, PARAMETER, PRIVATE ::    t40c   =  233.16
   REAL, PARAMETER, PRIVATE ::    eacrc  =  1.0

   REAL, SAVE ::                               &
       qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr,&
       g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,   &
       precr1,precr2,xm0,xmmax,bvts1,          &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,    &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r,&
             pidn0s,xlv1,pacrc

CONTAINS

!===================================================================
!
  SUBROUTINE ncloud5(th, q, qc, qr, qi, qs                         &
                    ,w, den, pii, p, delz                          &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                ! optional
                    ,rain, rainncv                                 &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!Coded by Song-You Hong (NCEP) and implemented by Shuhua Chen (NCAR)
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
                                                              qs

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
                                                             xls, &
                                                            xlv0, &
                                                            xlf0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
!
! Optional 
!
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        OPTIONAL ,                                                &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv

! LOCAL VAR

  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci, qrs
  INTEGER ::               i,j,k

!-------------------------------------------------------------------
      DO J=jts,jte

         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
         ENDDO
         ENDDO

         CALL ncloud52D(t, q(ims,kms,j), qci, qrs                  &
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
         ENDDO
         ENDDO

      ENDDO

  END SUBROUTINE ncloud5

!===================================================================
!
  SUBROUTINE ncloud52D(t, q, qci, qrs,w, den, p, delz              &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,lat                                           &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                 ! optional
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
                                                        qci, qrs

  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q

  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                       p, &
                                                               w, &
                                                             den, &
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
                                                             xls, &
                                                            xlv0, &
                                                            xlf0, &
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

  INTEGER, PARAMETER :: iun      = 84

  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
        rh, qs, slope, slope2, slopeb,                            &
        paut, pres, falk, fall, work1    
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::                      &
        pacr
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pgen, pisd, pcon, xl, cpm, work2, q1, t1, denfac,         &
        pgens, pauts, pacrss, pisds, press, pcons, psml, psev

  INTEGER, DIMENSION( its:ite ) :: mstep
  LOGICAL, DIMENSION( its:ite ) :: flgcld

  REAL  ::  n0sfac, pi,                                         &
            cpmcal, xlcal, lamdar, lamdas, diffus,              &
            viscos, xka, venfac, conden, diffac,                &
            x, y, z, a, b, c, d, e,                             &
            qdt, holdrr, holdrs, supcol,                        &
            coeres, supsat, dtcld, xmi, eacrs, satdt, xnc,      &
            fallsum, xlwork2, factor, source, value,            &
            xlf,pfrzdtc,pfrzdtr

  INTEGER :: i,j,k,                                             &
            iprt, latd, lond, loop, loops, ifsat, n, numdt

!
!=================================================================
!   compute internal functions
!
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
!     tvcal(x,y) = x+x*ep1*max(y,qmin)
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-30 kg/kg.
!     otherwise use uniform distribution value (1.e15)
!
      lamdar(x,y)=(pidn0r/(x*y))**.25
      lamdas(x,y,z)=(pidn0s*z/(x*y))**.25
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
!=================================================================
!     set iprt = 0 for no unit fort.84 output
!
      iprt = 1
      if(iprt.eq.1) then
        qdt = delt * 1000.
        latd = (jte+jts)/2 + 1
        lond = (ite+its)/2 + 1
      else
        latd = jts
        lond = its
      endif
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
      if(lat.eq.latd) then
        i = lond
        do k = kts, kte
          press(i,k) = 0.
          pauts(i,k) = 0.
          pacrss(i,k)= 0.
          pgens(i,k) = 0.
          pisds(i,k) = 0.
          pcons(i,k) = 0.
          t1(i,k) = t(i,k)
          q1(i,k) = q(i,k)
        enddo
      endif
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
!         if(lat.eq.latd.and.i.eq.lond) write(iun,700) qci(i,k,1)*1000., &
!           qrs(i,k,1)*1000.,qci(i,k,2)*1000.,qrs(i,k,2)*1000.
!700       format(1x,'before computation', 4f10.4)
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
        do i = its, ite
          pres(i,k,1) = 0.
          pres(i,k,2) = 0.
          paut(i,k,1) = 0.
          paut(i,k,2) = 0.
          pacr(i,k,1) = 0.
          pacr(i,k,2) = 0.
          pacr(i,k,3) = 0.
          pgen(i,k) = 0.
          pisd(i,k) = 0.
          pcon(i,k) = 0.
          psml(i,k) = 0.
          psev(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
        enddo
      enddo
!
!----------------------------------------------------------------
!     sloper: the slope parameter of the rain(m-1)
!     xka:    thermal conductivity of air(jm-1s-1k-1)
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k,1).le.qcrmin)then
            slope(i,k,1) = lamdarmax
            slopeb(i,k,1) = slope(i,k,1)**bvtr
          else
            slope(i,k,1) = lamdar(qrs(i,k,1),den(i,k))
            slopeb(i,k,1) = slope(i,k,1)**bvtr
          endif
          if(qrs(i,k,2).le.qcrmin)then
            slope(i,k,2) = lamdarmax
            slopeb(i,k,2) = slope(i,k,2)**bvts
          else
            supcol = t0c-t(i,k)
            n0sfac = min(exp(alpha*supcol),n0smax)
            slope(i,k,2) = lamdas(qrs(i,k,2),den(i,k),n0sfac)
            slopeb(i,k,2) = slope(i,k,2)**bvts
          endif
          slope2(i,k,1) = slope(i,k,1)*slope(i,k,1)
          slope2(i,k,2) = slope(i,k,2)*slope(i,k,2)
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
!     instantaneous melting of cloud ice
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          xlf = xls-xl(i,k)
          if(supcol.lt.0..and.qci(i,k,2).gt.qcrmin) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
!           if(lat.eq.latd.and.i.eq.lond) write(iun,607) k,-supcol, &
!             qci(i,k,2)*1000.,xlf/cpm(i,k)*qci(i,k,2)
          endif
!607  format(1x,'k = ',i3,' t = ',f5.1,' qi melting = ',f10.6,     &
!            '  del t  = ',f10.6)
!
!     homogeneous freezing of cloud water below -40c
!
          if(supcol.gt.40..and.qci(i,k,1).gt.qcrmin) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
!           if(lat.eq.latd.and.i.eq.lond) write(iun,608) k,-supcol, &
!             qci(i,k,1)*1000.,xlf/cpm(i,k)*qci(i,k,1)
          endif
!608  format(1x,'k = ',i3,' t = ',f5.1,' qc home freezing = ',f10.6, &
!           '  del t  = ',f10.6)
!
!     heterogeneous freezing of cloud water
!
          if(supcol.gt.0..and.qci(i,k,1).gt.qcrmin) then
            pfrzdtc = min(pfrz1*exp(pfrz2*supcol-1.)                &
               *den(i,k)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
!           if(lat.eq.latd.and.i.eq.lond) write(iun,609) k,-supcol,  &
!             pfrzdtc*1000.,xlf/cpm(i,k)*pfrzdtc
          endif
!609  format(1x,'k = ',i3,' t = ',f5.1,' qc hete freezing = ',f10.6, &
!           '  del t  = ',f10.6)
!
!     freezing of rain water
!
          if(supcol.gt.0..and.qrs(i,k,1).gt.qcrmin) then
            pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k)          &
                  *exp(pfrz2*supcol-1.)*slope(i,k,1)**(-7)*dtcld,       &
                  qrs(i,k,1))
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
!           if(lat.eq.latd.and.i.eq.lond) write(iun,610) k,-supcol,  &
!             pfrzdtr*1000.,xlf/cpm(i,k)*pfrzdtr
!610  format(1x,'k = ',i3,' t = ',f5.1,' qr      freezing = ',f10.6, &
!           '  del t  = ',f10.6)
          endif
        enddo
      enddo
!
!     warm rain process
!     paut: auto conversion rate from cloud to rain (kgkg-1s-1)
!     pacr: accretion rate of rain by cloud(lin83)
!     pres: evaporation/condensation rate of rain(rh83)
!
      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
          if(qci(i,k,1).gt.qc0) then
            paut(i,k,1) = qck1*qci(i,k,1)**(7./3.)
            paut(i,k,1) = min(paut(i,k,1),qci(i,k,1)/dtcld)
          endif
          if(qrs(i,k,1).gt.qcrmin) then
            if(qci(i,k,1).gt.qcrmin) pacr(i,k,1)                           &
              = min(pacrr/slope2(i,k,1)/slope(i,k,1)/slopeb(i,k,1)       &
                *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
            coeres = slope2(i,k,1)*sqrt(slope(i,k,1)*slopeb(i,k,1))
            pres(i,k,1) = (rh(i,k,1)-1.)*(precr1/slope2(i,k,1)           &
                        +precr2*work2(i,k)/coeres)/work1(i,k,1)
            if(pres(i,k,1).lt.0.) then
              pres(i,k,1) = max(pres(i,k,1),-qrs(i,k,1)/dtcld)
              pres(i,k,1) = max(pres(i,k,1),satdt/2)
            else
              pres(i,k,1) = min(pres(i,k,1),satdt/2)
              pres(i,k,1) = min(pres(i,k,1),qrs(i,k,1)/dtcld)
            endif
          endif
        enddo
      enddo
!
!----------------------------------------------------------------
!     cold rain process
!     paut: conversion(aggregation) of ice to snow(kgkg-1s-1)(rh83)
!     pgen: generation(nucleation) of ice from vapor(kgkg-1s-1)(rh83)
!     pacr: accretion rate of snow by ice(lin83)
!     pisd: deposition/sublimation rate of ice(rh83)
!     pres: deposition/sublimation rate of snow(lin83)
!
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
          n0sfac = min(exp(alpha*supcol),n0smax)
          xnc = min(xn0 * exp(betai*supcol)/den(i,k),xncmax)
!
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qcrmin) then
            eacrs = exp(0.025*(-supcol))
            pacr(i,k,2) = min(pacrs*n0sfac*eacrs/slope2(i,k,2)/slope(i,k,2)   &
                      /slopeb(i,k,2)*qci(i,k,2)*denfac(i,k),qci(i,k,2)/dtcld)
          endif
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qcrmin) then
            pacr(i,k,3) = min(pacrc/slope2(i,k,2)/slope(i,k,2)                &
                      /slopeb(i,k,2)*qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!
          if(qci(i,k,2).gt.qcrmin) then
            xmi = qci(i,k,2)*xnc
            if(ifsat.ne.1) pisd(i,k) = 4.*dicon*sqrt(xmi)*den(i,k)         &
                                     *(rh(i,k,2)-1.)/work1(i,k,2)
            if(pisd(i,k).lt.0.) then
              pisd(i,k) = max(pisd(i,k),satdt)
              pisd(i,k) = max(pisd(i,k),-qci(i,k,2)/dtcld)
            else
              pisd(i,k) = min(pisd(i,k),satdt)
            endif
            if(abs(pisd(i,k)).gt.abs(satdt)) ifsat = 1
          endif
!
          if(qrs(i,k,2).gt.qcrmin.and.ifsat.ne.1) then
            coeres = slope2(i,k,2)*sqrt(slope(i,k,2)*slopeb(i,k,2))
            if(ifsat.ne.1) pres(i,k,2) = (rh(i,k,2)-1.)*(precs1             &
                                       /slope2(i,k,2)+precs2*work2(i,k)     &
                                       /coeres)/work1(i,k,2)
            if(pres(i,k,2).lt.0.) then
              pres(i,k,2) = max(pres(i,k,2),-qrs(i,k,2)/dtcld)
              pres(i,k,2) = max(pres(i,k,2),satdt/2)
            else
              pres(i,k,2) = min(pres(i,k,2),satdt/2)
              pres(i,k,2) = min(pres(i,k,2),qrs(i,k,2)/dtcld)
            endif
            if(abs(pisd(i,k)+pres(i,k,2)).ge.abs(satdt)) ifsat = 1
          endif
!
          if(supsat.gt.0.and.ifsat.ne.1) then
            pgen(i,k) = max(0.,(xm0*xnc-max(qci(i,k,2),0.))/dtcld)
            pgen(i,k) = min(pgen(i,k),satdt)
          endif
!
          if(qci(i,k,2).gt.qcrmin) paut(i,k,2)                           &
                 = max(0.,(qci(i,k,2)-xmmax*xnc)/dtcld)
!
          if(t(i,k).gt.t0c) then
            xlf = xls - xl(i,k)
            if(qrs(i,k,2).gt.qcrmin) then
              psml(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2.    &
                          *(precs1/slope2(i,k,2)+precs2*work2(i,k)/coeres)
              psml(i,k) = min(max(psml(i,k),-qrs(i,k,2)/dtcld),0.)
            endif
            if(qrs(i,k,2).gt.qcrmin.and.rh(i,k,1).lt.1.)                 &
              psev(i,k) = pres(i,k,2)*work1(i,k,2)/work1(i,k,1)
              psev(i,k) = min(max(psev(i,k),-qrs(i,k,2)/dtcld),0.)
          endif
        enddo
      enddo
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
        do i = its, ite
          if(t(i,k).le.t0c) then
!
!     cloud water
!
            value = max(qcrmin,qci(i,k,1))
            source = (paut(i,k,1)+pacr(i,k,1)+pacr(i,k,3))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pacr(i,k,1) = pacr(i,k,1)*factor
              pacr(i,k,3) = pacr(i,k,3)*factor
            endif
!
!     cloud ice
!
            value = max(qcrmin,qci(i,k,2))
            source = (paut(i,k,2)+pacr(i,k,2)-pgen(i,k)-pisd(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,2) = paut(i,k,2)*factor
              pacr(i,k,2) = pacr(i,k,2)*factor
              pgen(i,k) = pgen(i,k)*factor
              pisd(i,k) = pisd(i,k)*factor
            endif
            work2(i,k)=-(pres(i,k,1)+pres(i,k,2)+pgen(i,k)+pisd(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pacr(i,k,1)   &
                           +pacr(i,k,3))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pacr(i,k,1)   &
                           +pres(i,k,1))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(paut(i,k,2)+pacr(i,k,2)   &
                           -pgen(i,k)-pisd(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(pres(i,k,2)+paut(i,k,2)   &
                           +pacr(i,k,2)+pacr(i,k,3))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(pres(i,k,2)+pisd(i,k)+pgen(i,k))       &
                         -xl(i,k)*pres(i,k,1)                      &
                         -xlf*pacr(i,k,3)
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qcrmin,qci(i,k,1))
            source=(paut(i,k,1)+pacr(i,k,1)+pacr(i,k,3))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,1) = paut(i,k,1)*factor
              pacr(i,k,1) = pacr(i,k,1)*factor
              pacr(i,k,3) = pacr(i,k,3)*factor
            endif
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(-psev(i,k)-psml(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psev(i,k) = psev(i,k)*factor
              psml(i,k) = psml(i,k)*factor
            endif
            work2(i,k)=-(pres(i,k,1)+psev(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pacr(i,k,1)        &
                    +pacr(i,k,3))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pacr(i,k,1)        &
                    +pres(i,k,1) -psml(i,k)+pacr(i,k,3))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psml(i,k)+psev(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xlf*(psml(i,k))-xl(i,k)*(pres(i,k,1)+psev(i,k))
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
!     condensational/evaporational rate of cloud water if there exists
!     additional water vapor condensated/if evaporation of cloud water
!     is not enough to remove subsaturation.
!     use fall bariable for this process(pcon)
!
!     if(lat.eq.latd) write(iun,603)
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcon(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.qcrmin.and.work1(i,k,1).lt.0.and.t(i,k).gt.t0c)    &
            pcon(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k) = q(i,k)-pcon(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcon(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcon(i,k)*xl(i,k)/cpm(i,k)*dtcld
!
          if(lat.eq.latd.and.i.eq.lond) then
            pgens(i,k) = pgens(i,k)+pgen(i,k)
            pcons(i,k) = pcons(i,k)+pcon(i,k)
            pisds(i,k) = pisds(i,k)+pisd(i,k)
            pacrss(i,k) = pacrss(i,k)+pacr(i,k,1)+pacr(i,k,2)+pacr(i,k,3)
            press(i,k) = press(i,k)+pres(i,k,1)+pres(i,k,2)
            pauts(i,k) = pauts(i,k)+paut(i,k,1)+paut(i,k,2)
!           write(iun,604) k,p(i,k)/100.,                                   &
!             t(i,k)-t0c,t(i,k)-t1(i,k),q(i,k)*1000.,                       &
!             (q(i,k)-q1(i,k))*1000.,rh(i,k,2)*100.,pgens(i,k)*qdt,         &
!             pcons(i,k)*qdt,pisds(i,k)*qdt,pauts(i,k)*qdt,pacrss(i,k)*qdt, &
!             press(i,k)*qdt,qci(i,k,1)*1000.,qrs(i,k,1)*1000.,             &
!             qci(i,k,2)*1000.,qrs(i,k,2)*1000.
          endif
        enddo
      enddo
!603   format(1x,'  k','     p',                                             &
!           '    t',' delt','    q',' delq','   rh',                         &
!           ' pgen',' pcon',' pisd',' paut',' pacr',' pres',                 &
!           '   qc','   qr','   qi','   qs')
!604   format(1x,i3,f6.0,4f5.1,f5.0,10f5.2)
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k,1).le.qcrmin)then
            slope(i,k,1) = lamdarmax
            slopeb(i,k,1) = slope(i,k,1)**bvtr
          else
            slope(i,k,1) = lamdar(qrs(i,k,1),den(i,k))
            slopeb(i,k,1) = slope(i,k,1)**bvtr
          endif
          if(qrs(i,k,2).le.qcrmin)then
            slope(i,k,2) = lamdarmax
            slopeb(i,k,2) = slope(i,k,2)**bvts
          else
            supcol = t0c-t(i,k)
            n0sfac = min(exp(alpha*supcol),n0smax)
            slope(i,k,2) = lamdas(qrs(i,k,2),den(i,k),n0sfac)
            slopeb(i,k,2) = slope(i,k,2)**bvts
          endif
          slope2(i,k,1) = slope(i,k,1)*slope(i,k,1)
          slope2(i,k,2) = slope(i,k,2)*slope(i,k,2)
        enddo
      enddo
!
      do i = its, ite
        do k = kte, kts, -1
          work1(i,k,1) = pvtr/slopeb(i,k,1)*denfac(i,k)/delz(i,k)
          work1(i,k,2) = pvts/slopeb(i,k,2)*denfac(i,k)/delz(i,k)
          if(qrs(i,k,1).le.qcrmin) work1(i,k,1) = 0.
          if(qrs(i,k,2).le.qcrmin) work1(i,k,2) = 0.
          numdt = max(nint(max(work1(i,k,1),work1(i,k,2))*dtcld+.5),1)
          if(qci(i,k,2).le.qmin) then
            work2c(i,k) = 0.
          else
            work1c(i,k) = 3.29*(den(i,k)*qci(i,k,2))**0.16
            work2c(i,k) = work1c(i,k)/delz(i,k)
          endif
          numdt = max(nint(work2c(i,k)*dtcld+.5),numdt)
          if(numdt.ge.mstep(i)) mstep(i) = numdt
        enddo
        mstep(i) = min(mstep(i),mstepmax)
      enddo
!
!      if(lat.eq.latd) write(iun,605)
      do n = 1,mstepmax
          k = kte
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              holdrr = qrs(i,k,1)
              holdrs = qrs(i,k,2)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k),0.)
              falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              qci(i,k,2) = max(qci(i,k,2)-falkc(i,k)*dtcld/den(i,k),0.)
!
!              if(lat.eq.latd.and.i.eq.lond)                                        &
!                write(iun,606) k,p(i,k)/100.,                                      &
!                t(i,k)-t0c,q(i,k)*1000.,rh(i,k,2)*100.,w(i,k),work1(i,k,1)         &
!                *delz(i,k), work1(i,k,2)*delz(i,k),holdrr*1000.,holdrs*1000.,      &
!                qrs(i,k,1)*1000.,qrs(i,k,2)*1000.,n
            endif
          enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              holdrr = qrs(i,k,1)
              holdrs = qrs(i,k,2)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)                             &
                         -falk(i,k+1,1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)                             &
                         -falk(i,k+1,2)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              falkc(i,k) = den(i,k)*qci(i,k,2)*work2c(i,k)/mstep(i)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)                       &
                        -falkc(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
!
!              if(lat.eq.latd.and.i.eq.lond)                                        &
!                write(iun,606) k,p(i,k)/100.,                                      &
!                t(i,k)-t0c,q(i,k)*1000.,rh(i,k,2)*100.,w(i,k),work1(i,k,1)         &
!                *delz(i,k), work1(i,k,2)*delz(i,k),holdrr*1000.,holdrs*1000.,      &
!                qrs(i,k,1)*1000.,qrs(i,k,2)*1000.,n
            endif
          enddo
        enddo
      enddo
!605   format(1x,'  k','     p','    t','    q','   rh','     w',                   &
!            '   vtr','   vts','   qri','   qsi','   qrf','   qsf',' mstep')
!606   format(1x,i3,f6.0,2f5.1,f5.0,f6.2,6f6.2,i5)
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,1,1)+fall(i,1,2)
        if(fallsum.gt.0.) then
          IF (PRESENT(rainncv)) rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000.
          IF (PRESENT(rain   )) rain(i) = fallsum*delz(i,1)/denr*dtcld*1000.        &
                  + rain(i)
        endif
      enddo
!
!      if(lat.eq.latd.AND.PRESENT(rain)) write(iun,601) latd,lond,loop,rain(lond)
! 601  format(1x,' ncloud5 lat lon loop : rain(mm) ',3i6,f20.2)
!
      enddo                  ! big loops

  END SUBROUTINE ncloud52d
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
      end function rgmma
!
!--------------------------------------------------------------------------
      REAL FUNCTION fpvs(t,ice,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c)
!--------------------------------------------------------------------------
      IMPLICIT NONE
!--------------------------------------------------------------------------
      REAL t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti, &
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
  SUBROUTINE ncloud5init(den0,denr,dens,cl,cpv,allowed_to_read)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable

   real, intent(in) :: den0,denr,dens,cl,cpv
   logical, intent(in) :: allowed_to_read
   real :: pi

   pi = 4.*atan(1.)
   xlv1 = cl-cpv

   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu ! 7.03
   bvtr1 = 1.+bvtr
   bvtr2 = 2.5+.5*bvtr
   bvtr3 = 3.+bvtr
   bvtr4 = 4.+bvtr
   g1pbr = rgmma(bvtr1)
   g3pbr = rgmma(bvtr3)
   g4pbr = rgmma(bvtr4)            ! 17.837825
   g5pbro2 = rgmma(bvtr2)          ! 1.8273
   pvtr = avtr*g4pbr/6.
   eacrr = 1.0
   pacrr = pi*n0r*avtr*g3pbr*.25*eacrr
   precr1 = 2.*pi*n0r*.78
   precr2 = 2.*pi*n0r*.31*avtr**.5*g5pbro2
   xm0  = (di0/dicon)**2
   xmmax = (dimax/dicon)**2
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
  END SUBROUTINE ncloud5init

END MODULE module_mp_ncloud5


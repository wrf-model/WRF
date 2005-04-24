MODULE module_mp_ncloud3

   REAL, PARAMETER, PRIVATE :: dtcldcr     = 240.
   INTEGER, PARAMETER, PRIVATE :: mstepmax = 100

   REAL, PARAMETER, PRIVATE :: n0r = 8.e6
   REAL, PARAMETER, PRIVATE :: avtr = 841.9
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5 ! 8 microm  in contrast to 10 micro m
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
!  REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e15  
   REAL, PARAMETER, PRIVATE :: lamdarmax = 1.e5  
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-6  

   REAL, SAVE ::                                     &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr,&
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,   &
             precr1,precr2,xm0,xmmax,bvts1,          &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,    &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r,&
                   pidn0s,xlv1

CONTAINS

!===================================================================
!
  SUBROUTINE ncloud3(th, q, qci, qrs                               &
                   , w, den, pii, p, delz                          &
                   , delt,g, cpd, cpv, rd, rv, t0c                 &
                   , ep1, ep2, qmin                                &
                   , XLS, XLV0, XLF0, den0, denr                   &
                   , cliq,cice,psat                                &
                   , ids,ide, jds,jde, kds,kde                     &
                   , ims,ime, jms,jme, kms,kme                     &
                   , its,ite, jts,jte, kts,kte                     &
                ! optional
                   , rain, rainncv                                 &
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
                                                             qci, &
                                                             qrs

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
                                                             T0c, &
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
  INTEGER ::               i,j,k

!-------------------------------------------------------------------
      DO J=jts,jte

         DO K=kts,kte
         DO I=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
         ENDDO
         ENDDO

         CALL ncloud32D(t, q(ims,kms,j), qci(ims,kms,j)            &
                    ,qrs(ims,kms,j),w(ims,kms,j), den(ims,kms,j)   &
                    ,p(ims,kms,j), delz(ims,kms,j)                 &
                    , delt,g, cpd, cpv, rd, rv, t0c                &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,J                                             &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                 ! optional 
                    ,rain(ims,j), rainncv(ims,j)                   &
                                                                   )

         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
         ENDDO
         ENDDO

      ENDDO

  END SUBROUTINE ncloud3

!===================================================================
!
  SUBROUTINE ncloud32D(t, q, qci, qrs,w, den, p, delz              &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,lat                                           &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                  !optional
                    ,rain, rainncv                                 &
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

  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                              q,  &
                                                             qci, &
                                                             qrs

  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                       w, &
                                                             den, &
                                                               p, &
                                                            delz

  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
                                                             T0c, &
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

  INTEGER, PARAMETER :: iun      = 84

  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        rh, qs, denfac, slope, slope2, slopeb,                    &
        pgen, paut, pacr, pisd, pres, pcon, fall, falk,           &
        xl, cpm, work1, work2, q1, t1,                            &
        pgens, pauts, pacrss, pisds, press, pcons

  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc

  INTEGER, DIMENSION( its:ite ) :: mstep
  LOGICAL, DIMENSION( its:ite ) :: flgcld

  REAL  ::  n0sfac, pi,                                         &
            cpmcal, xlcal, tvcal, lamdar, lamdas, diffus,       &
            viscos, xka, venfac, conden, diffac,                &
            x, y, z, a, b, c, d, e,                             &
            qdt, pvt, qik, delq, facq, qrsci, frzmlt,           &
            snomlt, hold, holdrs, facqci, supcol, coeres,       &
            supsat, dtcld, xmi, qciik, delqci, eacrs, satdt, xnc

  INTEGER :: i,j,k,                                             &
            iprt, latd, lond, loop, loops, ifsat, kk, n, numdt

!
!=================================================================
!   compute internal functions
!
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
      tvcal(x,y) = x+x*ep1*max(y,qmin)
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
!     iprt = 0
!     if(iprt.eq.1) then
!       qdt = delt * 1000.
!       latd = jts
!       lond = its
!     else
!       latd = 0
!       lond = 0
!     endif
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!
      do k = kts, kte
        do i = its, ite
          qci(i,k) = max(qci(i,k),0.0)
          qrs(i,k) = max(qrs(i,k),0.0)
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
          work1(i,k) = tvcal(t(i,k),q(i,k))
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          qs(i,k) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs(i,k) = ep2 * qs(i,k) / (p(i,k) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          rh(i,k) = max(q(i,k) / qs(i,k),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!     if(lat.eq.latd) then
!       i = lond
!       print*,'lat',latd,lat,i

!       do k = kts, kte
!         press(i,k) = 0.
!         pauts(i,k) = 0.
!         pacrss(i,k)= 0.
!         pgens(i,k) = 0.
!         pisds(i,k) = 0.
!         pcons(i,k) = 0.
!         t1(i,k) = t(i,k)
!         q1(i,k) = q(i,k)
!       enddo
!     endif
!
      do k = kts, kte
        do i = its, ite
          pres(i,k) = 0.
          paut(i,k) = 0.
          pacr(i,k) = 0.
          pgen(i,k) = 0.
          pisd(i,k) = 0.
          pcon(i,k) = 0.
          fall(i,k) = 0.
          falk(i,k) = 0.
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
          if(qrs(i,k).le.qcrmin)then
            slope(i,k) = lamdarmax
            slopeb(i,k) = slope(i,k)**bvtr
          else
            if(t(i,k).ge.t0c) then
              slope(i,k) = lamdar(qrs(i,k),den(i,k))
              slopeb(i,k) = slope(i,k)**bvtr
            else
              supcol = t0c-t(i,k)
              n0sfac = min(exp(alpha*supcol),n0smax)
              slope(i,k) = lamdas(qrs(i,k),den(i,k),n0sfac)
              slopeb(i,k) = slope(i,k)**bvts
            endif
          endif
          slope2(i,k) = slope(i,k)*slope(i,k)
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          if(t(i,k).ge.t0c) then
            work1(i,k) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k))
          else
            work1(i,k) = diffac(xls,p(i,k),t(i,k),den(i,k),qs(i,k))
          endif
          work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          supsat = max(q(i,k),qmin)-qs(i,k)
          satdt = supsat/dtcld
          if(t(i,k).ge.t0c) then
!
!----------------------------------------------------------------
!     warm rain process
!     paut: auto conversion rate from cloud to rain (kgkg-1s-1)(kessler)
!     pacr: accretion rate of rain by cloud(lin83)
!     pres: evaporation/condensation rate of rain(rh83)
!
            if(qci(i,k).gt.qc0) then
              paut(i,k) = qck1*qci(i,k)**(7./3.)
              paut(i,k) = min(paut(i,k),qci(i,k)/dtcld)
            endif
!
            if(qrs(i,k).gt.qcrmin) then
              if(qci(i,k).gt.qcrmin)                                     &
                pacr(i,k) = min(pacrr/slope2(i,k)/slope(i,k)/slopeb(i,k) &
                     *qci(i,k)*denfac(i,k),qci(i,k)/dtcld)
              coeres = slope2(i,k)*sqrt(slope(i,k)*slopeb(i,k))
              pres(i,k) = (rh(i,k)-1.)*(precr1/slope2(i,k)               &
                       +precr2*work2(i,k)/coeres)/work1(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k)/dtcld)
                pres(i,k) = max(pres(i,k),satdt/2)
              else
                pres(i,k) = min(pres(i,k),qrs(i,k)/dtcld)
                pres(i,k) = min(pres(i,k),satdt/2)
              endif
            endif
          else
!
!----------------------------------------------------------------
!     cold rain process
!     paut: conversion(aggregation) of ice to snow(kgkg-1s-1)(rh83)
!     pgen: generation(nucleation) of ice from vapor(kgkg-1s-1)(rh83)
!     pacr: accretion rate of snow by ice(lin83)
!     pisd: deposition/sublimation rate of ice(rh83)
!     pres: deposition/sublimation rate of snow(lin83)
!
            supcol = t0c-t(i,k)
            ifsat = 0
            n0sfac = min(exp(alpha*supcol),n0smax)
            xnc = min(xn0 * exp(betai*supcol)/den(i,k),xncmax)
!
            if(qrs(i,k).gt.qcrmin.and.qci(i,k).gt.qcrmin) then
              eacrs = exp(0.025*(-supcol))
              pacr(i,k) = pacrs*n0sfac*eacrs/slope2(i,k)/slope(i,k)     &
                      /slopeb(i,k)*qci(i,k)*denfac(i,k)
            endif
!
            if(qci(i,k).gt.qcrmin) then
              xmi = qci(i,k)*xnc
              pisd(i,k) = 4.*dicon*sqrt(xmi)*den(i,k)*(rh(i,k)-1.)      &
                        /work1(i,k)
              if(pisd(i,k).lt.0.) then
                pisd(i,k) = max(pisd(i,k),satdt/2)
                pisd(i,k) = max(pisd(i,k),-qci(i,k)/dtcld)
              else
                pisd(i,k) = min(pisd(i,k),satdt/2)
              endif
              if(abs(pisd(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!
            if(qrs(i,k).gt.qcrmin.and.ifsat.ne.1) then
              coeres = slope2(i,k)*sqrt(slope(i,k)*slopeb(i,k))
              pres(i,k) = (rh(i,k)-1.)*n0sfac*(precs1/slope2(i,k)       &
                        +precs2*work2(i,k)/coeres)/work1(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k)/dtcld)
                pres(i,k) = max(pres(i,k),satdt/2)
              else
                pres(i,k) = min(pres(i,k),satdt/2)
                pres(i,k) = min(pres(i,k),qrs(i,k)/dtcld)
              endif
              if(abs(pisd(i,k)+pres(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!
            if(supsat.gt.0.and.ifsat.ne.1) then
              pgen(i,k) = max(0.,(xm0*xnc-max(qci(i,k),0.))/dtcld)
              pgen(i,k) = min(pgen(i,k),satdt)
            endif
!
            if(qci(i,k).gt.qcrmin) paut(i,k)                           &
                = max(0.,(qci(i,k)-xmmax*xnc)/dtcld)
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
          qciik = max(qcrmin,qci(i,k))
          delqci = (paut(i,k)+pacr(i,k)-pgen(i,k)-pisd(i,k))*dtcld
          if(delqci.ge.qciik) then
            facqci = qciik/delqci
            paut(i,k) = paut(i,k)*facqci
            pacr(i,k) = pacr(i,k)*facqci
            pgen(i,k) = pgen(i,k)*facqci
            pisd(i,k) = pisd(i,k)*facqci
          endif
          qik = max(qcrmin,q(i,k))
          delq = (pres(i,k)+pgen(i,k)+pisd(i,k))*dtcld
          if(delq.ge.qik) then
            facq = qik/delq
            pres(i,k) = pres(i,k)*facq
            pgen(i,k) = pgen(i,k)*facq
            pisd(i,k) = pisd(i,k)*facq
          endif
          work2(i,k) = -pres(i,k)-pgen(i,k)-pisd(i,k)
          q(i,k) = q(i,k)+work2(i,k)*dtcld
          qci(i,k) = max(qci(i,k)-(paut(i,k)+pacr(i,k)-pgen(i,k)     &
                   -pisd(i,k))*dtcld,0.)
          qrs(i,k) = max(qrs(i,k)+(paut(i,k)+pacr(i,k)               &
                   +pres(i,k))*dtcld,0.)
          if(t(i,k).lt.t0c) then
            t(i,k) = t(i,k)-xls*work2(i,k)/cpm(i,k)*dtcld
          else
            t(i,k) = t(i,k)-xl(i,k)*work2(i,k)/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          qs(i,k) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs(i,k) = ep2 * qs(i,k) / (p(i,k) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          denfac(i,k) = sqrt(den0/den(i,k))
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
          work1(i,k) = conden(t(i,k),q(i,k),qs(i,k),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k)+work1(i,k)
          pcon(i,k) = min(max(work1(i,k),0.),max(q(i,k),0.))/dtcld
          if(qci(i,k).gt.qcrmin.and.work1(i,k).lt.0.and.t(i,k).gt.t0c)   &
            pcon(i,k) = max(work1(i,k),-qci(i,k))/dtcld
          q(i,k) = q(i,k)-pcon(i,k)*dtcld
          qci(i,k) = max(qci(i,k)+pcon(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcon(i,k)*xl(i,k)/cpm(i,k)*dtcld

!
!         if(lat.eq.latd.and.i.eq.lond) then
!           pgens(i,k) = pgens(i,k)+pgen(i,k)
!           pcons(i,k) = pcons(i,k)+pcon(i,k)
!           pisds(i,k) = pisds(i,k)+pisd(i,k)
!           pacrss(i,k) = pacrss(i,k)+pacr(i,k)
!           press(i,k) = press(i,k)+pres(i,k)
!           pauts(i,k) = pauts(i,k)+paut(i,k)
!           write(iun,604) k,p(i,k)/100.,                                   &
!             t(i,k)-t0c,t(i,k)-t1(i,k),q(i,k)*1000.,                       &
!             (q(i,k)-q1(i,k))*1000.,rh(i,k)*100.,pgens(i,k)*qdt,           &
!             pcons(i,k)*qdt,pisds(i,k)*qdt,pauts(i,k)*qdt,pacrss(i,k)*qdt, &
!             press(i,k)*qdt,qci(i,k)*1000.,qrs(i,k)*1000.
!         endif

        enddo
      enddo
603   format(1x,'  k','     p',                                          &
           '    t',' delt','    q',' delq','   rh',                      &
           ' pgen',' pcon',' pisd',' paut',' pacr',' pres',              &
           '  qci','  qrs')
604   format(1x,i3,f6.0,4f5.1,f5.0,8f5.2)
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
        do i = its, ite
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k).le.qcrmin)then
            slope(i,k) = lamdarmax
            slopeb(i,k) = slope(i,k)**bvtr
          else
            if(t(i,k).ge.t0c) then
              slope(i,k) = lamdar(qrs(i,k),den(i,k))
              slopeb(i,k) = slope(i,k)**bvtr
            else
              supcol = t0c-t(i,k)
              n0sfac = min(exp(alpha*supcol),n0smax)
              slope(i,k) = lamdas(qrs(i,k),den(i,k),n0sfac)
              slopeb(i,k) = slope(i,k)**bvts
            endif
          endif
          slope2(i,k) = slope(i,k)*slope(i,k)
        enddo
      enddo
!
      do i = its, ite
        do k = kte, kts, -1
          if(t(i,k).lt.t0c) then
            pvt = pvts
          else
            pvt = pvtr
          endif
          work1(i,k) = pvt/slopeb(i,k)*denfac(i,k)
          work2(i,k) = work1(i,k)/delz(i,k)
          if(qrs(i,k).le.qcrmin) work2(i,k) = 0.
          numdt = max(nint(work2(i,k)*dtcld+.5),1)
          if(t(i,k).lt.t0c.and.qci(i,k).gt.qmin) then
            work1c(i,k) = 3.29*(den(i,k)*qci(i,k))**0.16
          else
            work1c(i,k) = 0.
          endif
          if(qci(i,k).le.qmin) then
            work2c(i,k) = 0.
          else
            work2c(i,k) = work1c(i,k)/delz(i,k)
          endif
          numdt = max(nint(work2c(i,k)*dtcld+.5),numdt)
          if(numdt.ge.mstep(i)) mstep(i) = numdt
        enddo
        mstep(i) = min(mstep(i),mstepmax)
      enddo
!
!     if(lat.eq.latd) write(iun,605)
      do n = 1,mstepmax
        k = kte
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k) = den(i,k)*qrs(i,k)*work2(i,k)/mstep(i)
              hold = falk(i,k)
              fall(i,k) = fall(i,k)+falk(i,k)
              holdrs = qrs(i,k)
              qrs(i,k) = max(qrs(i,k)-falk(i,k)*dtcld/den(i,k),0.)
              falkc(i,k) = den(i,k)*qci(i,k)*work2c(i,k)/mstep(i)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              qci(i,k) = max(qci(i,k)-falkc(i,k)*dtcld/den(i,k),0.)
            endif
          enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k) = den(i,k)*qrs(i,k)*work2(i,k)/mstep(i)
              hold = falk(i,k)
              fall(i,k) = fall(i,k)+falk(i,k)
              holdrs = qrs(i,k)
              qrs(i,k) = max(qrs(i,k)-(falk(i,k)                        &
                        -falk(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              falkc(i,k) = den(i,k)*qci(i,k)*work2c(i,k)/mstep(i)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              qci(i,k) = max(qci(i,k)-(falkc(i,k)                       &
                        -falkc(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
605   format(1x,'  k','     p','    t','    q','   rh','     w',        &
            '    vt','  falk','  falt','  qrsi','  qrsf',' mstep')
606   format(1x,i3,f6.0,2f5.1,f5.0,f6.2,5f6.2,i5)
!
!----------------------------------------------------------------
!     compute the freezing/melting term.
!     freezing occurs one layer above the melting level
!
      do i = its, ite
        mstep(i) = 0
      enddo
      do k = kts, kte
!
        do i = its, ite
          if(t(i,k).ge.t0c) then
            mstep(i) = k
          endif
        enddo
      enddo
!
      do i = its, ite
        if(mstep(i).ne.0.and.w(i,mstep(i)).gt.0.) then
          work1(i,1) = float(mstep(i) + 1)
          work1(i,2) = float(mstep(i))
        else
          work1(i,1) = float(mstep(i))
          work1(i,2) = float(mstep(i))
        endif
      enddo
!
      do i = its, ite
        k  = nint(work1(i,1))
        kk = nint(work1(i,2))
        if(k*kk.ge.1) then
          qrsci = qrs(i,k) + qci(i,k)
          if(qrsci.gt.qcrmin.or.fall(i,kk).gt.0.) then
            frzmlt = min(max(-w(i,k)*qrsci/delz(i,k),-qrsci/dtcld),    &
                     qrsci/dtcld)
            snomlt = min(max(fall(i,kk)/den(i,kk),-qrs(i,k)/dtcld),qrs(i,k)/dtcld)
            if(k.eq.kk) then
              t(i,k) = t(i,k) - xlf0/cpm(i,k)*(frzmlt+snomlt)*dtcld
            else
              t(i,k) = t(i,k) - xlf0/cpm(i,k)*frzmlt*dtcld
              t(i,kk) = t(i,kk) - xlf0/cpm(i,kk)*snomlt*dtcld
            endif
 
!           if(lat.eq.latd.and.i.eq.lond) write(iun,608) k,t(i,k)-t0c, &
!             w(i,k),frzmlt*qdt,snomlt*qdt

          endif
        endif
      enddo
 608  format(1x,'k = ',i3,' t = ',f5.1,' w = ',f6.2,' frz/mlt = ',f5.1, &
                 ' snomlt = ',f5.1)
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        if(fall(i,1).gt.0.) then
          IF ( PRESENT( rainncv ) ) rainncv(i) = fall(i,1)*delz(i,1)/denr*dtcld*1000.
          IF ( PRESENT( rain    ) ) rain(i) = fall(i,1)*delz(i,1)/denr*dtcld*1000.       &
                  + rain(i)
        endif
      enddo
!
!     if(lat.eq.latd.AND.PRESENT(rain)) write(iun,601) latd,lond,loop,rain(lond)
 601  format(1x,' ncloud3 lat lon loop : rain(mm) ',3i6,f20.2)
!
      enddo                  ! big loops

  END SUBROUTINE ncloud32D
! ...................................................................
      real function rgmma(x)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!     rgmma function:  use infinite product form
      REAL :: euler
      parameter (euler=0.577215664901532)
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
      real function fpvs(t,ice,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c)
!--------------------------------------------------------------------------
      IMPLICIT NONE
!--------------------------------------------------------------------------
      real t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti, &
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
  SUBROUTINE ncloud3init(den0,denr,dens,cl,cpv,allowed_to_read)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable

   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv
   LOGICAL,INTENT(IN) :: allowed_to_read
   REAL :: pi

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
  END SUBROUTINE ncloud3init

END MODULE module_mp_ncloud3


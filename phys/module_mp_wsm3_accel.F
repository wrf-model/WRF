#if ( RWORDSIZE == 4 )
#  define VREC vsrec
#  define VSQRT vssqrt
#else
#  define VREC vrec
#  define VSQRT vsqrt
#endif

MODULE module_mp_wsm3
!
!
   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120. ! maximum time step for minor loops
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6         ! intercept parameter rain
   REAL, PARAMETER, PRIVATE :: avtr = 841.9       ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8         ! a constant for terminal velocity of rain
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5         ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: peaut = .55        ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8        ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5    ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER, PRIVATE :: avts = 11.72       ! a constant for terminal velocity of snow
   REAL, PARAMETER, PRIVATE :: bvts = .41         ! a constant for terminal velocity of snow
   REAL, PARAMETER, PRIVATE :: n0smax =  1.e11    ! maximum n0s (t=-90C unlimited)
   REAL, PARAMETER, PRIVATE :: lamdarmax = 8.e4   ! limited maximum value for slope parameter of rain
   REAL, PARAMETER, PRIVATE :: lamdasmax = 1.e5   ! limited maximum value for slope parameter of snow
   REAL, PARAMETER, PRIVATE :: lamdagmax = 6.e4   ! limited maximum value for slope parameter of graupel
   REAL, PARAMETER, PRIVATE :: dicon = 11.9       ! constant for the cloud-ice diamter
   REAL, PARAMETER, PRIVATE :: dimax = 500.e-6    ! limited maximum value for the cloud-ice diamter
   REAL, PARAMETER, PRIVATE :: n0s = 2.e6         ! temperature dependent intercept parameter snow 
   REAL, PARAMETER, PRIVATE :: alpha = .12        ! .122 exponen factor for n0s
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-9     ! minimun values for qr, qs, and qg
   REAL, SAVE ::                                      &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr, &
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,    &
             precr1,precr2,xmmax,roqimax,bvts1,       &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,     &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r, &
             pidn0s,xlv1,                             &
             rslopermax,rslopesmax,rslopegmax,        &
             rsloperbmax,rslopesbmax,rslopegbmax,     &
             rsloper2max,rslopes2max,rslopeg2max,     &
             rsloper3max,rslopes3max,rslopeg3max
!
! Specifies code-inlining of fpvs function in WSM32D below. JM 20040507
!
CONTAINS
!===================================================================
!
  SUBROUTINE wsm3(th, q, qci, qrs                     &
                   , w, den, pii, p, delz             &
                   , delt,g, cpd, cpv, rd, rv, t0c    &
                   , ep1, ep2, qmin                   &
                   , XLS, XLV0, XLF0, den0, denr      &
                   , cliq,cice,psat                   &
                   , rain, rainncv                    &
                   , snow, snowncv                    &
                   , sr                               &
                   , ids,ide, jds,jde, kds,kde        &
                   , ims,ime, jms,jme, kms,kme        &
                   , its,ite, jts,jte, kts,kte        &
                                                      )
!-------------------------------------------------------------------
#ifdef _OPENMP
  use omp_lib
#endif
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!
!  This code is a 3-class simple ice microphyiscs scheme (WSM3) of the WRF
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. A theoretical background of the ice-microphysics and related
!  processes in the WSMMPs are described in Hong et al. (2004).
!  Production terms in the WSM6 scheme are described in Hong and Lim (2006).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WSM3 cloud scheme
!
!  Coded by Song-You Hong (Yonsei Univ.)
!             Jimy Dudhia (NCAR) and Shu-Hua Chen (UC Davis)
!             Summer 2002
!
!  Implemented by Song-You Hong (Yonsei Univ.) and Jimy Dudhia (NCAR)
!             Summer 2003
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Dudhia (D89, 1989) J. Atmos. Sci.
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc.
!
  INTEGER,      INTENT(IN   )    ::                ids,ide, jds,jde, kds,kde , &
                                                   ims,ime, jms,jme, kms,kme , &
                                                   its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                              &
        INTENT(INOUT) ::                                                       &
                                                                          th,  &
                                                                           q,  &
                                                                          qci, &
                                                                          qrs
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                              &
        INTENT(IN   ) ::                                                    w, &
                                                                          den, &
                                                                          pii, &
                                                                            p, &
                                                                         delz

  REAL, INTENT(IN   ) ::                                                 delt, &
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
  REAL, DIMENSION( ims:ime , jms:jme ),                                        &
        INTENT(INOUT) ::                                                 rain, &
                                                                      rainncv

  REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,                              &
        INTENT(INOUT) ::                                                 snow, &
                                                                      snowncv, &
                                                                           sr

! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::                                   t
  INTEGER ::                                                            i,j,k
#ifdef _ACCEL_PROF
  integer :: l
  real*8 wsm3_t(8,256), wsm5_t(8,256), t1, t2
  common /wsm_times/ wsm3_t(8,256), wsm5_t(8,256)
#endif
!-------------------------------------------------------------------
#ifdef _ACCEL_PROF
  call cpu_time(t1)
#endif

#ifdef _ACCEL

        CALL wsm32D(th, q, qci, qrs,                               &
                     w, den, pii, p, delz, rain, rainncv,          &
                     delt,g, cpd, cpv, rd, rv, t0c,                &
                     ep1, ep2, qmin,                               &
                     XLS, XLV0, XLF0, den0, denr,                  &
                     cliq,cice,psat,                               &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

#else

      DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
         ENDDO
         ENDDO
         CALL wsm32D(t, q(ims,kms,j), qci(ims,kms,j)                           &
                    ,qrs(ims,kms,j),w(ims,kms,j), den(ims,kms,j)               &
                    ,p(ims,kms,j), delz(ims,kms,j)                             &
                    ,delt,g, cpd, cpv, rd, rv, t0c                             &
                    ,ep1, ep2, qmin                                            &
                    ,XLS, XLV0, XLF0, den0, denr                               &
                    ,cliq,cice,psat                                            &
                    ,j                                                         &
                    ,rain(ims,j), rainncv(ims,j)                               &
                    ,snow(ims,j),snowncv(ims,j)                                &
                    ,sr(ims,j)                                                 &
                    ,ids,ide, jds,jde, kds,kde                                 &
                    ,ims,ime, jms,jme, kms,kme                                 &
                    ,its,ite, jts,jte, kts,kte                                 &
                                                                               )
         DO K=kts,kte
         DO I=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
         ENDDO
         ENDDO
      ENDDO
#endif


#ifdef _ACCEL_PROF
  call cpu_time(t2)
#ifdef _OPENMP
  l = omp_get_thread_num() + 1
#else
  l = 1
#endif
  wsm3_t(1,l) = wsm3_t(1,l) + (t2 - t1)
#endif

  END SUBROUTINE wsm3


#ifdef _ACCEL

!===================================================================
!{
  SUBROUTINE wsm32D(th, q, qci, qrs,                               &
                     w, den, pii, p, delz, rain, rainncv,          &
                     delt,g, cpd, cpv, rd, rv, t0c,                &
                     ep1, ep2, qmin,                               &
                     XLS, XLV0, XLF0, den0, denr,                  &
                     cliq,cice,psat,                               &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme, ims:ims ),                  &
        INTENT(INOUT) ::                                          &
                                                               th
  REAL, DIMENSION( ims:ime , kms:kme, ims:ims ),                  &
        INTENT(IN) ::                                             &
                                                               pii
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                  &
        INTENT(INOUT) ::                                          &
                                                               q, &
                                                             qci, &
                                                             qrs
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                  &
        INTENT(IN   ) ::                                       w, &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv
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
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        rh, qs, denfac, rslope, rslope2, rslope3, rslopeb,        &
        pgen, paut, pacr, pisd, pres, pcon, fall, falk,           &
        xl, cpm, work1, work2, xni, qs0, n0sfac
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte, jts:jte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc
  INTEGER :: mstep, numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdar, lamdas, diffus,                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, pvt, qik, delq, facq, qrsci, frzmlt,             &
            snomlt, hold, holdrs, facqci, supcol, coeres,         &
            supsat, dtcld, xmi, qciik, delqci, eacrs, satdt,      &
            qimax, diameter, xni0, roqi0
  REAL  :: holdc, holdci
  INTEGER :: i, k, j,                                 &
            iprt, latd, lond, loop, loops, ifsat, kk, n
!

#define INL
#ifdef INL
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
#endif



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
!----------------------------------------------------------------
!     compute the minor time steps.
!
      loops = max(int(delt/dtcldcr+0.5),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt
#ifdef INL
      cvap = cpv
      hvap=xlv0
      hsub=xls
      ttp=t0c+0.1
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
#endif
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!
!$acc region &
!$acc        local(t) &
!$acc        copyin(delz(:,:,:),p(:,:,:)) &
!$acc        copyin(den(:,:,:),w(:,:,:)) &
!$acc        copy(q(:,:,:),qci(:,:,:),qrs(:,:,:))
!$acc do &
!$acc        private(rh,qs,denfac,rslope,rslope2,rslope3,rslopeb) &
!$acc        private(pgen,paut,pacr,pisd,pres,pcon,fall,falk) &
!$acc        private(xl,cpm,work1,work2,xni,qs0,n0sfac) &
!$acc        private(falkc,work1c,work2c,fallc) &
!$acc        parallel
      do j = jts, jte
!$acc do &
!$acc        private(numdt,mstep) &
!$acc        kernel vector(96)
      do i = its, ite
      do k = kts, kte
            t(i,k,j)=th(i,k,j)*pii(i,k,j)
          qci(i,k,j) = max(qci(i,k,j),0.0)
          qrs(i,k,j) = max(qrs(i,k,j),0.0)
        enddo
!
!----------------------------------------------------------------
!     latent heat for phase changes and heat capacity. neglect the
!     changes during microphysical process calculation
!     emanuel(1994)
!
      do k = kts, kte
          cpm(i,k) = cpmcal(q(i,k,j))
          xl(i,k) = xlcal(t(i,k,j))
      enddo
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
        mstep = 1
        flgcld(i) = .true.
!
      do k = kts, kte
          denfac(i,k) = sqrt(den0/den(i,k,j))
      enddo
!
      do k = kts, kte
#ifndef INL
          qs(i,k) = fpvs(t(i,k,j),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
          qs0(i,k) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#else
          tr=ttp/t(i,k,j)
          if(t(i,k,j).lt.ttp) then
            qs(i,k) =psat*(tr**xai)*exp(xbi*(1.-tr))
          else
            qs(i,k) =psat*(tr**xa)*exp(xb*(1.-tr))
          endif
          qs0(i,k)  =psat*(tr**xa)*exp(xb*(1.-tr))
#endif
          qs0(i,k) = (qs0(i,k)-qs(i,k))/qs(i,k)
          qs(i,k) = ep2 * qs(i,k) / (p(i,k,j) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          rh(i,k) = max(q(i,k,j) / qs(i,k),qmin)
      enddo
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
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
          xni(i,k) = 1.e3
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
      do k = kts, kte
          supcol = t0c-t(i,k,j)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(t(i,k,j).ge.t0c) then
            if(qrs(i,k,j).le.qcrmin)then
              rslope(i,k) = rslopermax
              rslopeb(i,k) = rsloperbmax
              rslope2(i,k) = rsloper2max
              rslope3(i,k) = rsloper3max
            else
              rslope(i,k) = 1./lamdar(qrs(i,k,j),den(i,k,j))
              rslopeb(i,k) = rslope(i,k)**bvtr
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          else
            if(qrs(i,k,j).le.qcrmin)then
              rslope(i,k) = rslopesmax
              rslopeb(i,k) = rslopesbmax
              rslope2(i,k) = rslopes2max
              rslope3(i,k) = rslopes3max
            else
              rslope(i,k) = 1./lamdas(qrs(i,k,j),den(i,k,j),n0sfac(i,k))
              rslopeb(i,k) = rslope(i,k)**bvts
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
          xni(i,k) = min(max(5.38e7*(den(i,k,j)                           &
                    *max(qci(i,k,j),qmin))**0.75,1.e3),1.e6)
      enddo
!
      numdt = 1
      do k = kte, kts, -1
          if(t(i,k,j).lt.t0c) then
            pvt = pvts
          else
            pvt = pvtr
          endif
          work1(i,k) = pvt*rslopeb(i,k)*denfac(i,k)
          work2(i,k) = work1(i,k)/delz(i,k,j)
          numdt = max(int(work2(i,k)*dtcld+1.),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
!
      do n = 1, mstep
        k = kte
            falk(i,k) = den(i,k,j)*qrs(i,k,j)*work2(i,k)/mstep
            hold = falk(i,k)
            fall(i,k) = fall(i,k)+falk(i,k)
            holdrs = qrs(i,k,j)
            qrs(i,k,j) = max(qrs(i,k,j)-falk(i,k)*dtcld/den(i,k,j),0.)
        do k = kte-1, kts, -1
              falk(i,k) = den(i,k,j)*qrs(i,k,j)*work2(i,k)/mstep
              hold = falk(i,k)
              fall(i,k) = fall(i,k)+falk(i,k)
              holdrs = qrs(i,k,j)
              qrs(i,k,j) = max(qrs(i,k,j)-(falk(i,k)                        &
                        -falk(i,k+1)*delz(i,k+1,j)/delz(i,k,j))*dtcld/den(i,k,j),0.)
        enddo
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      mstep = 1
      numdt = 1
      do k = kte, kts, -1
          if(t(i,k,j).lt.t0c.and.qci(i,k,j).gt.0.) then
            xmi = den(i,k,j)*qci(i,k,j)/xni(i,k)
            diameter  = dicon * sqrt(xmi)
            work1c(i,k) = 1.49e4*diameter**1.31
          else
            work1c(i,k) = 0.
          endif
          if(qci(i,k,j).le.0.) then
            work2c(i,k) = 0.
          else
            work2c(i,k) = work1c(i,k)/delz(i,k,j)
          endif
          numdt = max(int(work2c(i,k)*dtcld+1.),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
!
      do n = 1, mstep
        k = kte
            falkc(i,k) = den(i,k,j)*qci(i,k,j)*work2c(i,k)/mstep
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k,j)
            qci(i,k,j) = max(qci(i,k,j)-falkc(i,k)*dtcld/den(i,k,j),0.)
        do k = kte-1, kts, -1
              falkc(i,k) = den(i,k,j)*qci(i,k,j)*work2c(i,k)/mstep
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k,j)
              qci(i,k,j) = max(qci(i,k,j)-(falkc(i,k)                       &
                        -falkc(i,k+1)*delz(i,k+1,j)/delz(i,k,j))*dtcld/den(i,k,j),0.)
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the freezing/melting term. [D89 B16-B17]
!     freezing occurs one layer above the melting level
!
        mstep = 0
!
      do k = kts, kte
          if(t(i,k,j).ge.t0c) then
            mstep = k
          endif
      enddo
!
        if(mstep.ne.0.and.w(i,mstep,j).gt.0.) then
          work1(i,1) = float(mstep + 1)
          work1(i,2) = float(mstep)
        else
          work1(i,1) = float(mstep)
          work1(i,2) = float(mstep)
        endif
!
        k  = int(work1(i,1)+0.5)
        kk = int(work1(i,2)+0.5)
        if(k*kk.ge.1) then
          qrsci = qrs(i,k,j) + qci(i,k,j)
          if(qrsci.gt.0..or.fall(i,kk).gt.0.) then
            frzmlt = min(max(-w(i,k,j)*qrsci/delz(i,k,j),-qrsci/dtcld),    &
                     qrsci/dtcld)
            snomlt = min(max(fall(i,kk)/den(i,kk,j),-qrs(i,k,j)/dtcld),    &
                     qrs(i,k,j)/dtcld)
            if(k.eq.kk) then
              t(i,k,j) = t(i,k,j) - xlf0/cpm(i,k)*(frzmlt+snomlt)*dtcld
            else
              t(i,k,j) = t(i,k,j) - xlf0/cpm(i,k)*frzmlt*dtcld
              t(i,kk,j) = t(i,kk,j) - xlf0/cpm(i,kk)*snomlt*dtcld
            endif
          endif
        endif
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
        if(fall(i,1).gt.0.) then
          rainncv(i,j) = fall(i,1)*delz(i,1,j)/denr*dtcld*1000.
          rain(i,j) = fall(i,1)*delz(i,1,j)/denr*dtcld*1000.                &
                  + rain(i,j)
        endif
!
!----------------------------------------------------------------
!     rsloper: reverse of the slope parameter of the rain(m,j)
!     xka:    thermal conductivity of air(jm-1s-1k-1)
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!
      do k = kts, kte
          if(t(i,k,j).ge.t0c) then
            if(qrs(i,k,j).le.qcrmin)then
              rslope(i,k) = rslopermax
              rslopeb(i,k) = rsloperbmax
              rslope2(i,k) = rsloper2max
              rslope3(i,k) = rsloper3max
            else
              rslope(i,k) = 1./lamdar(qrs(i,k,j),den(i,k,j))
              rslopeb(i,k) = rslope(i,k)**bvtr
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          else
            if(qrs(i,k,j).le.qcrmin)then
              rslope(i,k) = rslopesmax
              rslopeb(i,k) = rslopesbmax
              rslope2(i,k) = rslopes2max
              rslope3(i,k) = rslopes3max
            else
              rslope(i,k) = 1./lamdas(qrs(i,k,j),den(i,k,j),n0sfac(i,k))
              rslopeb(i,k) = rslope(i,k)**bvts
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          endif
      enddo
!
      do k = kts, kte
          if(t(i,k,j).ge.t0c) then
            work1(i,k) = diffac(xl(i,k),p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k))
          else
            work1(i,k) = diffac(xls,p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k))
          endif
          work2(i,k) = venfac(p(i,k,j),t(i,k,j),den(i,k,j))
      enddo
!
      do k = kts, kte
          supsat = max(q(i,k,j),qmin)-qs(i,k)
          satdt = supsat/dtcld
          if(t(i,k,j).ge.t0c) then
!
!===============================================================
!
! warm rain processes
!
! - follows the processes in RH83 and LFO except for autoconcersion
!
!===============================================================
!---------------------------------------------------------------
! paut1: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
            if(qci(i,k,j).gt.qc0) then
              paut(i,k) = qck1*qci(i,k,j)**(7./3.)
              paut(i,k) = min(paut(i,k),qci(i,k,j)/dtcld)
            endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [D89 B15]
!        (C->R)
!---------------------------------------------------------------
            if(qrs(i,k,j).gt.qcrmin.and.qci(i,k,j).gt.qmin) then
                pacr(i,k) = min(pacrr*rslope3(i,k)*rslopeb(i,k)          &
                     *qci(i,k,j)*denfac(i,k),qci(i,k,j)/dtcld)
            endif
!---------------------------------------------------------------
! pres1: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
            if(qrs(i,k,j).gt.0.) then
                coeres = rslope2(i,k)*sqrt(rslope(i,k)*rslopeb(i,k))
                pres(i,k) = (rh(i,k)-1.)*(precr1*rslope2(i,k)            &
                         +precr2*work2(i,k)*coeres)/work1(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k,j)/dtcld)
                pres(i,k) = max(pres(i,k),satdt/2)
              else
                pres(i,k) = min(pres(i,k),satdt/2)
              endif
            endif
          else
!
!===============================================================
!
! cold rain processes
!
! - follows the revised ice microphysics processes in HDC
! - the processes same as in RH83 and LFO behave
!   following ice crystal hapits defined in HDC, inclduing
!   intercept parameter for snow (n0s), ice crystal number
!   concentration (ni), ice nuclei number concentration
!   (n0i), ice diameter (d)
!
!===============================================================
!
            supcol = t0c-t(i,k,j)
            ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
            xni(i,k) = min(max(5.38e7*(den(i,k,j)                         &
                      *max(qci(i,k,j),qmin))**0.75,1.e3),1.e6)
            eacrs = exp(0.05*(-supcol))
!
            if(qrs(i,k,j).gt.qcrmin.and.qci(i,k,j).gt.qmin) then
              pacr(i,k) = min(pacrs*n0sfac(i,k)*eacrs*rslope3(i,k)       &
                       *rslopeb(i,k)*qci(i,k,j)*denfac(i,k),qci(i,k,j)/dtcld)
            endif
!-------------------------------------------------------------
! pisd: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,j).gt.0.) then
              xmi = den(i,k,j)*qci(i,k,j)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pisd(i,k) = 4.*diameter*xni(i,k)*(rh(i,k)-1.)              &
                        /work1(i,k)
              if(pisd(i,k).lt.0.) then
                pisd(i,k) = max(pisd(i,k),satdt/2)
                pisd(i,k) = max(pisd(i,k),-qci(i,k,j)/dtcld)
              else
                pisd(i,k) = min(pisd(i,k),satdt/2)
              endif
              if(abs(pisd(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pres2: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,j).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k)*sqrt(rslope(i,k)*rslopeb(i,k))
              pres(i,k) = (rh(i,k)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k)   &
                        +precs2*work2(i,k)*coeres)/work1(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k,j)/dtcld)
                pres(i,k) = max(pres(i,k),satdt/2)
              else
                pres(i,k) = min(pres(i,k),satdt/2)
              endif
              if(abs(pisd(i,k)+pres(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pgen: generation(nucleation) of ice from vapor [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*xni0**1.33
              pgen(i,k) = max(0.,(roqi0/den(i,k,j)-max(qci(i,k,j),0.))/dtcld)
              pgen(i,k) = min(pgen(i,k),satdt)
            endif
!-------------------------------------------------------------
! paut2: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,j).gt.0.) then
              qimax = roqimax/den(i,k,j)
              paut(i,k) = max(0.,(qci(i,k,j)-qimax)/dtcld)
            endif
          endif
      enddo
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
          qciik = max(qmin,qci(i,k,j))
          delqci = (paut(i,k)+pacr(i,k)-pgen(i,k)-pisd(i,k))*dtcld
          if(delqci.ge.qciik) then
            facqci = qciik/delqci
            paut(i,k) = paut(i,k)*facqci
            pacr(i,k) = pacr(i,k)*facqci
            pgen(i,k) = pgen(i,k)*facqci
            pisd(i,k) = pisd(i,k)*facqci
          endif
          qik = max(qmin,q(i,k,j))
          delq = (pres(i,k)+pgen(i,k)+pisd(i,k))*dtcld
          if(delq.ge.qik) then
            facq = qik/delq
            pres(i,k) = pres(i,k)*facq
            pgen(i,k) = pgen(i,k)*facq
            pisd(i,k) = pisd(i,k)*facq
          endif
          work2(i,k) = -pres(i,k)-pgen(i,k)-pisd(i,k)
          q(i,k,j) = q(i,k,j)+work2(i,k)*dtcld
          qci(i,k,j) = max(qci(i,k,j)-(paut(i,k)+pacr(i,k)-pgen(i,k)     &
                   -pisd(i,k))*dtcld,0.)
          qrs(i,k,j) = max(qrs(i,k,j)+(paut(i,k)+pacr(i,k)               &
                   +pres(i,k))*dtcld,0.)
          if(t(i,k,j).lt.t0c) then
            t(i,k,j) = t(i,k,j)-xls*work2(i,k)/cpm(i,k)*dtcld
          else
            t(i,k,j) = t(i,k,j)-xl(i,k)*work2(i,k)/cpm(i,k)*dtcld
          endif
      enddo
!
      do k = kts, kte
#ifndef INL
          qs(i,k) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#else
          tr=ttp/t(i,k,j)
          qs(i,k)=psat*(tr**xa)*exp(xb*(1.-tr))
#endif
          qs(i,k) = ep2 * qs(i,k) / (p(i,k,j) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          denfac(i,k) = sqrt(den0/den(i,k,j))
      enddo
!
!----------------------------------------------------------------
!  pcon: condensational/evaporational rate of cloud water [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
          work1(i,k) = conden(t(i,k,j),q(i,k,j),qs(i,k),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k,j)+work1(i,k)
          pcon(i,k) = min(max(work1(i,k),0.),max(q(i,k,j),0.))/dtcld
          if(qci(i,k,j).gt.0..and.work1(i,k).lt.0.and.t(i,k,j).gt.t0c)      &
            pcon(i,k) = max(work1(i,k),-qci(i,k,j))/dtcld
          q(i,k,j) = q(i,k,j)-pcon(i,k)*dtcld
          qci(i,k,j) = max(qci(i,k,j)+pcon(i,k)*dtcld,0.)
          t(i,k,j) = t(i,k,j)+pcon(i,k)*xl(i,k)/cpm(i,k)*dtcld
      enddo
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
          if(qci(i,k,j).le.qmin) qci(i,k,j) = 0.0
      enddo
!
      enddo                  ! big loops
         DO K=kts,kte
            th(i,k,j)=t(i,k,j)/pii(i,k,j)
         ENDDO
      enddo
      enddo
!$acc end region
  END SUBROUTINE wsm32D !}

#else


!===================================================================
!
  SUBROUTINE wsm32D(t, q, qci, qrs,w, den, p, delz                             &
                   ,delt,g, cpd, cpv, rd, rv, t0c                              &
                   ,ep1, ep2, qmin                                             &
                   ,XLS, XLV0, XLF0, den0, denr                                &
                   ,cliq,cice,psat                                             &
                   ,lat                                                        &
                   ,rain, rainncv                                              &
                   ,snow,snowncv                                               &
                   ,sr                                                         &
                   ,ids,ide, jds,jde, kds,kde                                  &
                   ,ims,ime, jms,jme, kms,kme                                  &
                   ,its,ite, jts,jte, kts,kte                                  &
                                                                               )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::                 ids,ide, jds,jde, kds,kde, &
                                                    ims,ime, jms,jme, kms,kme, &
                                                    its,ite, jts,jte, kts,kte, &
                                                    lat
  REAL, DIMENSION( its:ite , kts:kte ),                                        &
        INTENT(INOUT) ::                                                       &
                                                                            t
  REAL, DIMENSION( ims:ime , kms:kme ),                                        &
        INTENT(INOUT) ::                                                       &
                                                                            q, &
                                                                          qci, &
                                                                          qrs
  REAL, DIMENSION( ims:ime , kms:kme ),                                        &
        INTENT(IN   ) ::                                                    w, &
                                                                          den, &
                                                                            p, &
                                                                         delz
  REAL, INTENT(IN   ) ::                                                 delt, &
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
  REAL, DIMENSION( ims:ime ),                                                  &
        INTENT(INOUT) ::                                                 rain, &
                                                                      rainncv

  REAL, DIMENSION( ims:ime ),     OPTIONAL,                                    &
        INTENT(INOUT) ::                                                 snow, &
                                                                      snowncv, &
                                                                           sr
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                           rh, &
                                                                           qs, &
                                                                       denfac, &
                                                                       rslope, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                      rslopeb
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                         pgen, &
                                                                         pisd, &
                                                                         paut, &
                                                                         pacr, &
                                                                         pres, &
                                                                         pcon
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                         fall, &
                                                                         falk, &
                                                                           xl, &
                                                                          cpm, &
                                                                        work1, &
                                                                        work2, &
                                                                          xni, &
                                                                          qs0, &
                                                                       n0sfac
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                        falkc, &
                                                                       work1c, &
                                                                       work2c, &
                                                                        fallc

  INTEGER, DIMENSION( its:ite ) ::                                      kwork1,&
                                                                        kwork2

  INTEGER, DIMENSION( its:ite ) ::                                      mstep, &
                                                                        numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                                &
            cpmcal, xlcal, lamdar, lamdas, diffus,                             &
            viscos, xka, venfac, conden, diffac,                               &
            x, y, z, a, b, c, d, e,                                            &
            fallsum, fallsum_qsi, vt2i,vt2s,acrfac,                            &      
            qdt, pvt, qik, delq, facq, qrsci, frzmlt,                          &
            snomlt, hold, holdrs, facqci, supcol, coeres,                      &
            supsat, dtcld, xmi, qciik, delqci, eacrs, satdt,                   &
            qimax, diameter, xni0, roqi0, supice,holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                                &
            iprt, latd, lond, loop, loops, ifsat, kk, n
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
! variables for optimization
  REAL, DIMENSION( its:ite )    :: tvec1
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
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!
      diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
      viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
      xka(x,y) = 1.414e3*viscos(x,y)*y
      diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
!      venfac(a,b,c) = (viscos(b,c)/diffus(b,a))**(.3333333)                   &
!                      /viscos(b,c)**(.5)*(den0/c)**0.25
      venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
                     /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
      conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!
      pi = 4. * atan(1.)
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
!     do k = kts, kte
!       do i = its, ite
!         denfac(i,k) = sqrt(den0/den(i,k))
!       enddo
!     enddo
      do k = kts, kte
        CALL VREC( tvec1(its), den(its,k), ite-its+1)
        do i = its, ite
          tvec1(i) = tvec1(i)*den0
        enddo
        CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1)
      enddo
!
! Inline expansion for fpvs
!         qs(i,k) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs0(i,k) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      cvap = cpv
      hvap=xlv0
      hsub=xls
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
        do i = its, ite
!         tr=ttp/t(i,k)
!         if(t(i,k).lt.ttp) then
!           qs(i,k) =psat*(tr**xai)*exp(xbi*(1.-tr))
!         else
!           qs(i,k) =psat*(tr**xa)*exp(xb*(1.-tr))
!         endif
!         qs0(i,k)  =psat*(tr**xa)*exp(xb*(1.-tr))
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k) =psat*(exp(log(tr)*(xai)))*exp(xbi*(1.-tr))
          else
            qs(i,k) =psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          endif
          qs0(i,k)  =psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          qs0(i,k) = (qs0(i,k)-qs(i,k))/qs(i,k)
          qs(i,k) = ep2 * qs(i,k) / (p(i,k) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          rh(i,k) = max(q(i,k) / qs(i,k),qmin)
        enddo
      enddo
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
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
          xni(i,k) = 1.e3
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
      do k = kts, kte
        do i = its, ite
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          if(t(i,k).ge.t0c) then
            if(qrs(i,k).le.qcrmin)then
              rslope(i,k) = rslopermax
              rslopeb(i,k) = rsloperbmax
              rslope2(i,k) = rsloper2max
              rslope3(i,k) = rsloper3max
            else
              rslope(i,k) = 1./lamdar(qrs(i,k),den(i,k))
!             rslopeb(i,k) = rslope(i,k)**bvtr
              rslopeb(i,k) = exp(log(rslope(i,k))*(bvtr))
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          else
            if(qrs(i,k).le.qcrmin)then
              rslope(i,k) = rslopesmax
              rslopeb(i,k) = rslopesbmax
              rslope2(i,k) = rslopes2max
              rslope3(i,k) = rslopes3max
            else
              rslope(i,k) = 1./lamdas(qrs(i,k),den(i,k),n0sfac(i,k))
!             rslopeb(i,k) = rslope(i,k)**bvts
              rslopeb(i,k) = exp(log(rslope(i,k))*(bvts))
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7                                            &
!                   *(den(i,k)*max(qci(i,k),qmin))**0.75,1.e3),1.e6)
          xni(i,k) = min(max(5.38e7                                            &
                    *exp(log((den(i,k)*max(qci(i,k),qmin)))*(0.75)),1.e3),1.e6)
        enddo
      enddo
!
      mstepmax = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          if(t(i,k).lt.t0c) then
            pvt = pvts
          else
            pvt = pvtr
          endif
          work1(i,k) = pvt*rslopeb(i,k)*denfac(i,k)
          work2(i,k) = work1(i,k)/delz(i,k)
          numdt(i) = max(nint(work2(i,k)*dtcld+.5),1)
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
            falk(i,k) = den(i,k)*qrs(i,k)*work2(i,k)/mstep(i)
            hold = falk(i,k)
            fall(i,k) = fall(i,k)+falk(i,k)
            holdrs = qrs(i,k)
            qrs(i,k) = max(qrs(i,k)-falk(i,k)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k) = den(i,k)*qrs(i,k)*work2(i,k)/mstep(i)
              hold = falk(i,k)
              fall(i,k) = fall(i,k)+falk(i,k)
              holdrs = qrs(i,k)
              qrs(i,k) = max(qrs(i,k)-(falk(i,k)                               &
                        -falk(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
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
          if(t(i,k).lt.t0c.and.qci(i,k).gt.0.) then
            xmi = den(i,k)*qci(i,k)/xni(i,k)
!           diameter  = dicon * sqrt(xmi)
!           work1c(i,k) = 1.49e4*diameter**1.31
            diameter  = max(dicon * sqrt(xmi), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
          else
            work1c(i,k) = 0.
          endif
          if(qci(i,k).le.0.) then
            work2c(i,k) = 0.
          else
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
          if (n.le.mstep(i)) then
            falkc(i,k) = den(i,k)*qci(i,k)*work2c(i,k)/mstep(i)
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k)
            qci(i,k) = max(qci(i,k)-falkc(i,k)*dtcld/den(i,k),0.)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if (n.le.mstep(i)) then
              falkc(i,k) = den(i,k)*qci(i,k)*work2c(i,k)/mstep(i)
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k)
              qci(i,k) = max(qci(i,k)-(falkc(i,k)                              &
                        -falkc(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the freezing/melting term. [D89 B16-B17]
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
        kwork2(i) = mstep(i)
        kwork1(i) = mstep(i)
        if(mstep(i).ne.0) then
          if (w(i,mstep(i)).gt.0.) then
            kwork1(i) = mstep(i) + 1
          endif
        endif
      enddo
!
      do i = its, ite
        k  = kwork1(i)
        kk = kwork2(i)
        if(k*kk.ge.1) then
          qrsci = qrs(i,k) + qci(i,k)
          if(qrsci.gt.0..or.fall(i,kk).gt.0.) then
            frzmlt = min(max(-w(i,k)*qrsci/delz(i,k),-qrsci/dtcld),            &
                    qrsci/dtcld)
            snomlt = min(max(fall(i,kk)/den(i,kk),-qrs(i,k)/dtcld),            &
                    qrs(i,k)/dtcld)
            if(k.eq.kk) then
              t(i,k) = t(i,k) - xlf0/cpm(i,k)*(frzmlt+snomlt)*dtcld
            else
              t(i,k) = t(i,k) - xlf0/cpm(i,k)*frzmlt*dtcld
              t(i,kk) = t(i,kk) - xlf0/cpm(i,kk)*snomlt*dtcld
            endif
          endif
        endif
      enddo
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,1)
        fallsum_qsi = 0.
        if((t0c-t(i,1)).gt.0) then
        fallsum = fallsum+fallc(i,1)
        fallsum_qsi = fall(i,1)+fallc(i,1)
        endif
        rainncv(i) = 0.
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000.
          rain(i) = fallsum*delz(i,1)/denr*dtcld*1000. + rain(i)
        endif
        IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN
        snowncv(i) = 0.
        if(fallsum_qsi.gt.0.) then
          snowncv(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.
          snow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snow(i)
        endif
        ENDIF
        sr(i) = 0.
        if(fallsum.gt.0.) sr(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.     &
                                 /(rainncv(i)+1.e-12)
      enddo
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
          if(t(i,k).ge.t0c) then
            if(qrs(i,k).le.qcrmin)then
              rslope(i,k) = rslopermax
              rslopeb(i,k) = rsloperbmax
              rslope2(i,k) = rsloper2max
              rslope3(i,k) = rsloper3max
            else
              rslope(i,k) = 1./lamdar(qrs(i,k),den(i,k))
              rslopeb(i,k) = exp(log(rslope(i,k))*(bvtr))
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          else
            if(qrs(i,k).le.qcrmin)then
              rslope(i,k) = rslopesmax
              rslopeb(i,k) = rslopesbmax
              rslope2(i,k) = rslopes2max
              rslope3(i,k) = rslopes3max
            else
              rslope(i,k) = 1./lamdas(qrs(i,k),den(i,k),n0sfac(i,k))
              rslopeb(i,k) = exp(log(rslope(i,k))*(bvts))
              rslope2(i,k) = rslope(i,k)*rslope(i,k)
              rslope3(i,k) = rslope2(i,k)*rslope(i,k)
            endif
          endif
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
!===============================================================
!
! warm rain processes
!
! - follows the processes in RH83 and LFO except for autoconcersion
!
!===============================================================
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
            if(qci(i,k).gt.qc0) then
!             paut(i,k) = qck1*qci(i,k)**(7./3.)
              paut(i,k) = qck1*exp(log(qci(i,k))*((7./3.)))
              paut(i,k) = min(paut(i,k),qci(i,k)/dtcld)
            endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [D89 B15]
!        (C->R)
!---------------------------------------------------------------
            if(qrs(i,k).gt.qcrmin.and.qci(i,k).gt.qmin) then
                pacr(i,k) = min(pacrr*rslope3(i,k)*rslopeb(i,k)                &
                     *qci(i,k)*denfac(i,k),qci(i,k)/dtcld)
            endif
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
            if(qrs(i,k).gt.0.) then
                coeres = rslope2(i,k)*sqrt(rslope(i,k)*rslopeb(i,k))
                pres(i,k) = (rh(i,k)-1.)*(precr1*rslope2(i,k)                  &
                         +precr2*work2(i,k)*coeres)/work1(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k)/dtcld)
                pres(i,k) = max(pres(i,k),satdt/2)
              else
                pres(i,k) = min(pres(i,k),satdt/2)
              endif
            endif
          else
!
!===============================================================
!
! cold rain processes
!
! - follows the revised ice microphysics processes in HDC
! - the processes same as in RH83 and LFO behave
!   following ice crystal hapits defined in HDC, inclduing
!   intercept parameter for snow (n0s), ice crystal number
!   concentration (ni), ice nuclei number concentration
!   (n0i), ice diameter (d)
!
!===============================================================
!
            supcol = t0c-t(i,k)
            ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!           xni(i,k) = min(max(5.38e7                                          &
!                     *(den(i,k)*max(qci(i,k),qmin))**0.75,1.e3),1.e6)
            xni(i,k) = min(max(5.38e7                                          &
                      *exp(log((den(i,k)*max(qci(i,k),qmin)))*(0.75)),1.e3),1.e6)
            eacrs = exp(0.07*(-supcol))
!
            if(qrs(i,k).gt.qcrmin.and.qci(i,k).gt.qmin) then
              xmi = den(i,k)*qci(i,k)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
!             vt2i = 1.49e4*exp((log(diameter))*(1.31))
              vt2s = pvts*rslopeb(i,k)*denfac(i,k)
!-------------------------------------------------------------
! praci: Accretion of cloud ice by rain [HL A15] [LFO 25]
!        (T<T0: I->R)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k)+2.*diameter*rslope2(i,k)                &
                      +diameter**2*rslope(i,k)
              pacr(i,k) = min(pi*qci(i,k)*eacrs*n0s*n0sfac(i,k)                &
                       *abs(vt2s-vt2i)*acrfac/4.,qci(i,k)/dtcld)
            endif
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k).gt.0.) then
              xmi = den(i,k)*qci(i,k)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pisd(i,k) = 4.*diameter*xni(i,k)*(rh(i,k)-1.)/work1(i,k)
              if(pisd(i,k).lt.0.) then
                pisd(i,k) = max(pisd(i,k),satdt/2)
                pisd(i,k) = max(pisd(i,k),-qci(i,k)/dtcld)
              else
                pisd(i,k) = min(pisd(i,k),satdt/2)
              endif
              if(abs(pisd(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k)*sqrt(rslope(i,k)*rslopeb(i,k))
              pres(i,k) = (rh(i,k)-1.)*n0sfac(i,k)*(precs1*rslope2(i,k)        &
                        +precs2*work2(i,k)*coeres)/work1(i,k)
              supice = satdt-pisd(i,k)
              if(pres(i,k).lt.0.) then
                pres(i,k) = max(pres(i,k),-qrs(i,k)/dtcld)
                pres(i,k) = max(max(pres(i,k),satdt/2),supice)
              else
                pres(i,k) = min(min(pres(i,k),satdt/2),supice)
              endif
              if(abs(pisd(i,k)+pres(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-pisd(i,k)-pres(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
!             roqi0 = 4.92e-11*xni0**1.33
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pgen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k),0.))/dtcld)
              pgen(i,k) = min(min(pgen(i,k),satdt),supice)
            endif
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k).gt.0.) then
              qimax = roqimax/den(i,k)
              paut(i,k) = max(0.,(qci(i,k)-qimax)/dtcld)
            endif
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
          qciik = max(qmin,qci(i,k))
          delqci = (paut(i,k)+pacr(i,k)-pgen(i,k)-pisd(i,k))*dtcld
          if(delqci.ge.qciik) then
            facqci = qciik/delqci
            paut(i,k) = paut(i,k)*facqci
            pacr(i,k) = pacr(i,k)*facqci
            pgen(i,k) = pgen(i,k)*facqci
            pisd(i,k) = pisd(i,k)*facqci
          endif
          qik = max(qmin,q(i,k))
          delq = (pres(i,k)+pgen(i,k)+pisd(i,k))*dtcld
          if(delq.ge.qik) then
            facq = qik/delq
            pres(i,k) = pres(i,k)*facq
            pgen(i,k) = pgen(i,k)*facq
            pisd(i,k) = pisd(i,k)*facq
          endif
          work2(i,k) = -pres(i,k)-pgen(i,k)-pisd(i,k)
          q(i,k) = q(i,k)+work2(i,k)*dtcld
          qci(i,k) = max(qci(i,k)-(paut(i,k)+pacr(i,k)-pgen(i,k)-pisd(i,k))    &
                    *dtcld,0.)
          qrs(i,k) = max(qrs(i,k)+(paut(i,k)+pacr(i,k)+pres(i,k))*dtcld,0.)
          if(t(i,k).lt.t0c) then
            t(i,k) = t(i,k)-xls*work2(i,k)/cpm(i,k)*dtcld
          else
            t(i,k) = t(i,k)-xl(i,k)*work2(i,k)/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
      cvap = cpv
      hvap = xlv0
      hsub = xls
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
!         qs(i,k)=psat*(tr**xa)*exp(xb*(1.-tr))
          qs(i,k)=psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          qs(i,k) = ep2 * qs(i,k) / (p(i,k) - qs(i,k))
          qs(i,k) = max(qs(i,k),qmin)
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
!
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
        do i = its, ite
          work1(i,k) = conden(t(i,k),q(i,k),qs(i,k),xl(i,k),cpm(i,k))
          work2(i,k) = qci(i,k)+work1(i,k)
          pcon(i,k) = min(max(work1(i,k),0.),max(q(i,k),0.))/dtcld
          if(qci(i,k).gt.0..and.work1(i,k).lt.0.and.t(i,k).gt.t0c)             &
            pcon(i,k) = max(work1(i,k),-qci(i,k))/dtcld
          q(i,k) = q(i,k)-pcon(i,k)*dtcld
          qci(i,k) = max(qci(i,k)+pcon(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcon(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
        do i = its, ite
          if(qci(i,k).le.qmin) qci(i,k) = 0.0
        enddo
      enddo
!
      enddo                  ! big loops
  END SUBROUTINE wsm32D
#endif

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
      if(t.lt.ttp.and.ice.eq.1) then
        fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
      else
        fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END FUNCTION fpvs
!-------------------------------------------------------------------
  SUBROUTINE wsm3init(den0,denr,dens,cl,cpv,allowed_to_read)
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable
   REAL, INTENT(IN) :: den0,denr,dens,cl,cpv
   LOGICAL, INTENT(IN) :: allowed_to_read
   REAL :: pi
!
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
!
   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
!
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
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
!
  END SUBROUTINE wsm3init
END MODULE module_mp_wsm3

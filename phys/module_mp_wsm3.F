#ifdef _ACCEL
#  include "module_mp_wsm3_accel.F"
#else
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
             qc0, qck1, pidnc,                        &  
             bvtr1,bvtr2,bvtr3,bvtr4,g1pbr,           &
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,    &
             precr1,precr2,xmmax,roqimax,bvts1,       &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,     &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r, &
             pidn0s,xlv1,pi,                          &
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
                   , has_reqc, has_reqi, has_reqs     &  ! for radiation
                   , re_cloud, re_ice,   re_snow      &  ! for radiation 
                   , ids,ide, jds,jde, kds,kde        &
                   , ims,ime, jms,jme, kms,kme        &
                   , its,ite, jts,jte, kts,kte        &
                                                      )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
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
! for radiation connecting
  INTEGER, INTENT(IN)::                                                        &
                                                                     has_reqc, &
                                                                     has_reqi, &
                                                                     has_reqs
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                                  &
        INTENT(INOUT)::                                                        &
                                                                     re_cloud, &
                                                                       re_ice, &
                                                                      re_snow

! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::                                   t
  INTEGER ::                                                            i,j,k

! to calculate effective radius for radiation
  REAL, DIMENSION( kts:kte ) :: t1d
  REAL, DIMENSION( kts:kte ) :: den1d
  REAL, DIMENSION( kts:kte ) :: qc1d
  REAL, DIMENSION( kts:kte ) :: qi1d
  REAL, DIMENSION( kts:kte ) :: qs1d
  REAL, DIMENSION( kts:kte ) :: re_qc, re_qi, re_qs

!-------------------------------------------------------------------
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

        if (has_reqc.ne.0 .and. has_reqi.ne.0 .and. has_reqs.ne.0) then
          do i=its,ite
            do k=kts,kte
              re_qc(k) = 2.51E-6
              re_qi(k) = 10.01E-6
              re_qs(k) = 25.E-6

              t1d(k)  = th(i,k,j)*pii(i,k,j)
              den1d(k)= den(i,k,j)
              if(t(i,k).ge.t0c) then
                qc1d(k) = qci(i,k,j)
                qi1d(k) = 0.0
                qs1d(k) = 0.0
              else
                qc1d(k) = 0.0
                qi1d(k) = qci(i,k,j)
                qs1d(k) = qrs(i,k,j)
              endif
            enddo
            call effectRad_wsm3(t1d, qc1d, qi1d, qs1d, den1d,           &
                                qmin, t0c, re_qc, re_qi, re_qs,         &
                                kts, kte, i, j)
            do k=kts,kte
              re_cloud(i,k,j) = MAX(2.51E-6,  MIN(re_qc(k),  50.E-6))
              re_ice(i,k,j)   = MAX(10.01E-6, MIN(re_qi(k), 125.E-6))
              re_snow(i,k,j)  = MAX(25.E-6,   MIN(re_qs(k), 999.E-6))
            enddo
          enddo
        endif     ! has_reqc, etc...

      ENDDO
  END SUBROUTINE wsm3
!===================================================================
!
  SUBROUTINE wsm32D(t, q                                                       &
                   ,qci, qrs,w, den, p, delz                                   &
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
!
!  This code is a 3-class simple ice microphyiscs scheme (WSM3) of the 
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
!  Developed by Song-You Hong (Yonsei Univ.), Jimy Dudhia (NCAR) 
!             and Shu-Hua Chen (UC Davis) 
!             Summer 2002
!
!  Implemented by Song-You Hong (Yonsei Univ.) and Jimy Dudhia (NCAR)
!             Summer 2003
!
!  further modifications :
!        semi-lagrangian sedimentation (JH,2010),hong, aug 2009
!        ==> higher accuracy and efficient at lower resolutions
!        effective radius of hydrometeors, bae from kiaps, jan 2015
!        ==> consistency in solar insolation of rrtmg radiation
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Dudhia (D89, 1989) J. Atmos. Sci.
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc.
!             Juang and Hong (JH, 2010) Mon. Wea. Rev.
!
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
                                                                      qrs_tmp, &
                                                                      den_tmp, &
                                                                     delz_tmp, &
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
                                                                       denqci, &
                                                                       denqrs, &
                                                                       n0sfac, &
                                                                        falkc, &
                                                                       work1c, &
                                                                       work2c, &
                                                                        fallc
  REAL, DIMENSION( its:ite ) ::                                         delqrs,&
                                                                         delqi
  REAL, DIMENSION(its:ite) ::                                        tstepsnow
  INTEGER, DIMENSION( its:ite ) ::                                      kwork1,&
                                                                        kwork2
  INTEGER, DIMENSION( its:ite ) ::                                      mstep, &
                                                                        numdt
  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::                                                                     &
            cpmcal, xlcal, diffus,                                             &
            viscos, xka, venfac, conden, diffac,                               &
            x, y, z, a, b, c, d, e,                                            &
            fallsum, fallsum_qsi, vt2i,vt2s,acrfac,                            &      
            qdt, pvt, qik, delq, facq, qrsci, frzmlt,                          &
            snomlt, hold, holdrs, facqci, supcol, coeres,                      &
            supsat, dtcld, xmi, qciik, delqci, eacrs, satdt,                   &
            qimax, diameter, xni0, roqi0, supice,holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                                &
            iprt, latd, lond, loop, loops, ifsat, kk, n, idim, kdim
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
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!     Optimizatin : A**B => exp(log(A)*(B))
!
      diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
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
      do k = kts, kte
        do i = its, ite
          delz_tmp(i,k) = delz(i,k)
          den_tmp(i,k) = den(i,k)
        enddo
      enddo
!
!----------------------------------------------------------------
!    initialize the surface rain, snow
!
      do i = its, ite
        rainncv(i) = 0.
        if(PRESENT (snowncv) .AND. PRESENT (snow)) snowncv(i) = 0.
        sr(i) = 0.
! new local array to catch step snow
        tstepsnow(i) = 0.
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
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k) =psat*(exp(log(tr)*(xai)))*exp(xbi*(1.-tr))
          else
            qs(i,k) =psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          endif
          qs0(i,k)  =psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          qs0(i,k) = (qs0(i,k)-qs(i,k))/qs(i,k)
          qs(i,k) = min(qs(i,k),0.99*p(i,k))
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
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
      do k = kts, kte
        do i = its, ite
          xni(i,k) = min(max(5.38e7                                            &
                    *exp(log((den(i,k)*max(qci(i,k),qmin)))*(0.75)),1.e3),1.e6)
        enddo
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!---------------------------------------------------------------
      do k = kts, kte
        do i = its, ite
          qrs_tmp(i,k) = qrs(i,k)
        enddo
      enddo
      call slope_wsm3(qrs_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2,rslope3, &
                      work1,its,ite,kts,kte)
!
!
!  forward semi-laglangian scheme (JH), PCM (piecewise constant),  (linear)
!
      do k = kte, kts, -1
        do i = its, ite
          denqrs(i,k) = den(i,k)*qrs(i,k)
        enddo
      enddo
      call nislfv_rain_plm(idim,kdim,den_tmp,denfac,t,delz_tmp,work1,denqrs,   &
                           delqrs,dtcld,1,1)
      do k = kts, kte
        do i = its, ite
          qrs(i,k) = max(denqrs(i,k)/den(i,k),0.)
          fall(i,k) = denqrs(i,k)*work1(i,k)/delz(i,k)
        enddo
      enddo
      do i = its, ite
        fall(i,1) = delqrs(i)/delz(i,1)/dtcld
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      do k = kte, kts, -1
        do i = its, ite
          if(t(i,k).lt.t0c.and.qci(i,k).gt.0.) then
            xmi = den(i,k)*qci(i,k)/xni(i,k)
            diameter  = max(dicon * sqrt(xmi), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
          else
            work1c(i,k) = 0.
          endif
        enddo
      enddo
!
!  forward semi-laglangian scheme (JH), PCM (piecewise constant),  (linear)
!
      do k = kte, kts, -1
        do i = its, ite
          denqci(i,k) = den(i,k)*qci(i,k)
        enddo
      enddo
      call nislfv_rain_plm(idim,kdim,den_tmp,denfac,t,delz_tmp,work1c,denqci,  &
                           delqi,dtcld,1,0)
      do k = kts, kte
        do i = its, ite
          qci(i,k) = max(denqci(i,k)/den(i,k),0.)
        enddo
      enddo
      do i = its, ite
        fallc(i,1) = delqi(i)/delz(i,1)/dtcld
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
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000. + rainncv(i)
          rain(i) = fallsum*delz(i,1)/denr*dtcld*1000. + rain(i)
        endif
        if(fallsum_qsi.gt.0.) then
          tstepsnow(i)   = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.            &
                           +tstepsnow(i)
        IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN
          snowncv(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snowncv(i)
          snow(i) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snow(i)
        ENDIF
        endif
        IF ( PRESENT (snowncv) ) THEN
          if(fallsum.gt.0.) sr(i) = snowncv(i)/(rainncv(i)+1.e-12)
        ELSE
          if(fallsum.gt.0.) sr(i) = tstepsnow(i)/(rainncv(i)+1.e-12)
        ENDIF
      enddo
!
!----------------------------------------------------------------
!     update the slope parameters for microphysics computation 
!
      do k = kts, kte
        do i = its, ite
          qrs_tmp(i,k) = qrs(i,k)
        enddo
      enddo
      call slope_wsm3(qrs_tmp,den_tmp,denfac,t,rslope,rslopeb,rslope2,rslope3, &
                      work1,its,ite,kts,kte)
!
!     work1: the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!     work2: parameter associated with the ventilation effects(y93)
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
            n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
            ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
            xni(i,k) = min(max(5.38e7                                          &
                    *exp(log((den(i,k)*max(qci(i,k),qmin)))*(0.75)),1.e3),1.e6)
            eacrs = exp(0.07*(-supcol))
            if(qrs(i,k).gt.qcrmin.and.qci(i,k).gt.qmin) then
              xmi = den(i,k)*qci(i,k)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
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
          qs(i,k)=psat*(exp(log(tr)*(xa)))*exp(xb*(1.-tr))
          qs(i,k) = min(qs(i,k),0.99*p(i,k))
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
          if(qrs(i,k).le.qcrmin) qrs(i,k) = 0.0
        enddo
      enddo
!
      enddo                  ! big loops
  END SUBROUTINE wsm32D
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
!  
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
!  
   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
   pidnc = pi*denr/6.        ! syb
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
!
      subroutine slope_wsm3(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,vt,its,ite,kts,kte)
  IMPLICIT NONE
  INTEGER       ::               its,ite, jts,jte, kts,kte
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                          qrs, &
                                                                          den, &
                                                                       denfac, &
                                                                            t, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar,lamdas,x, y, z, supcol, pvt
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
!
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!
      do k = kts, kte
        do i = its, ite
          if(t(i,k).ge.t0c) then
            pvt = pvtr
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
            supcol = t0c-t(i,k)
            n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
            pvt = pvts
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
          vt(i,k) = pvt*rslopeb(i,k)*denfac(i,k)
          if(qrs(i,k).le.0.0) vt(i,k) = 0.0
        enddo
      enddo
  END subroutine slope_wsm3
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_pcm(im,km,denl,denfacl,tkl,dzl,wwl,rql,precip,dt,id,iter)
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
      real  dzl(im,km),wwl(im,km),rql(im,km),precip(im)
      real  denl(im,km),denfacl(im,km),tkl(im,km)
!
      integer  i,k,n,m,kk,kb,kt,iter
      real  tl,tl2,qql,dql,qqd
      real  th,th2,qqh,dqh
      real  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real  zsumt,qsumt,zsumb,qsumb
      real  allold, allnew, zz, dzamin, cflmax, decfl
      real  dz(km), ww(km), qq(km), wd(km), wa(km), was(km)
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
! pcm is 1st order, we should use 2nd order wi
! 2nd order interpolation to get wi
      wi(1) = ww(1)
      do k=2,km
        wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
      enddo
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
      enddo
      qa(km+1) = 0.0
!     call maxmin(km,1,qa,' arrival points ')
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_wsm3(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,1,1,1,km)
        if( n.eq.2 ) wa(1:km) = 0.5*(wa(1:km)+was(1:km))
        do k=1,km
!#ifdef DEBUG
!      print*,' slope_wsm3 ',qr(k)*1000.,den(k),denfac(k),tk(k),tmp(k),tmp1(k),tmp2(k),ww(k),wa(k)
!#endif
! mean wind is average of departure and new arrival winds
          ww(k) = 0.5* ( wd(k)+wa(k) )
        enddo
        was(:) = wa(:)
        n=n+1
        go to 100
      endif
!
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
! compute q with piecewise constant method
               if( kt-kb.eq.1 ) then
                 qn(k) = qa(kb)
               else if( kt-kb.ge.2 ) then
                 zsumb = za(kb+1)-zi(k)
                 qsumb = qa(kb) * zsumb
                 zsumt = zi(k+1)-za(kt-1)
                 qsumt = qa(kt-1) * zsumt
                 qsum = 0.0
                 zsum = 0.0
                 if( kt-kb.ge.3 ) then
                 do m=kb+1,kt-2
                   qsum = qsum + qa(m) * dza(m)
                   zsum = zsum + dza(m)
                 enddo
                 endif
                 qn(k) = (qsumb+qsum+qsumt)/(zsumb+zsum+zsumt)
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
  END SUBROUTINE nislfv_rain_pcm
!-------------------------------------------------------------------
      SUBROUTINE nislfv_rain_plm(im,km,denl,denfacl,tkl,dzl,wwl,rql,precip,dt,id,iter)
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
      real  dzl(im,km),wwl(im,km),rql(im,km),precip(im)
      real  denl(im,km),denfacl(im,km),tkl(im,km)
!
      integer  i,k,n,m,kk,kb,kt,iter
      real  tl,tl2,qql,dql,qqd
      real  th,th2,qqh,dqh
      real  zsum,qsum,dim,dip,c1,con1,fa1,fa2
      real  allold, allnew, zz, dzamin, cflmax, decfl
      real  dz(km), ww(km), qq(km), wd(km), wa(km), was(km)
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
      enddo
      qa(km+1) = 0.0
!     call maxmin(km,1,qa,' arrival points ')
!
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter ) then
        call slope_wsm3(qr,den,denfac,tk,tmp,tmp1,tmp2,tmp3,wa,1,1,1,km)
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
  END SUBROUTINE nislfv_rain_plm
!
!-----------------------------------------------------------------------
     subroutine effectRad_wsm3 (t, qc, qi, qs, rho, qmin, t0c,        &
                                re_qc, re_qi, re_qs, kts, kte, ii, jj)

!-----------------------------------------------------------------------
!  Compute radiation effective radii of cloud water, ice, and snow for 
!  single-moment microphysics.
!  These are entirely consistent with microphysics assumptions, not
!  constant or otherwise ad hoc as is internal to most radiation
!  schemes.  
!  Coded and implemented by Soo ya Bae, KIAPS, January 2015.
!-----------------------------------------------------------------------

      implicit none

!..Sub arguments
      integer, intent(in) :: kts, kte, ii, jj
      real, intent(in) :: qmin
      real, intent(in) :: t0c
      real, dimension( kts:kte ), intent(in)::  t
      real, dimension( kts:kte ), intent(in)::  qc
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
      real, dimension( kts:kte ):: rqi
      real, dimension( kts:kte ):: rni
      real, dimension( kts:kte ):: rqs
      real :: temp
      real :: lamdac
      real :: supcol, n0sfac, lamdas
      real :: diai      ! diameter of ice in m
      logical :: has_qc, has_qi, has_qs
!..Minimum microphys values
      real, parameter :: R1 = 1.E-12
      real, parameter :: R2 = 1.E-6
!..Mass power law relations:  mass = am*D**bm
      real, parameter :: bm_r = 3.0
      real, parameter :: obmr = 1.0/bm_r
      real, parameter :: nc0  = 3.E8
!-----------------------------------------------------------------------
      has_qc = .false.
      has_qi = .false.
      has_qs = .false.

      do k = kts, kte
        ! for cloud
        rqc(k) = max(R1, qc(k)*rho(k))
        if (rqc(k).gt.R1) has_qc = .true.
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
          if (rqc(k).le.R1) CYCLE
          lamdac   = (pidnc*nc0/rqc(k))**obmr
          re_qc(k) =  max(2.51E-6,min(1.5*(1.0/lamdac),50.E-6))
        enddo
      endif

     if (has_qi) then
        do k=kts,kte
          if (rqi(k).le.R1 .or. rni(k).le.R2) CYCLE
          diai = 11.9*sqrt(rqi(k)/ni(k))
          re_qi(k) = max(10.01E-6,min(0.75*0.163*diai,125.E-6))
        enddo
      endif

      if (has_qs) then
        do k=kts,kte
          if (rqs(k).le.R1) CYCLE
          supcol = t0c-t(k)
          n0sfac = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          lamdas = sqrt(sqrt(pidn0s*n0sfac/rqs(k)))
          re_qs(k) = max(25.E-6,min(0.5*(1./lamdas), 999.E-6))
        enddo
      endif

      end subroutine effectRad_wsm3
!-----------------------------------------------------------------------
END MODULE module_mp_wsm3
#endif

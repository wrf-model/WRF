#if ( RWORDSIZE == 4 )
#  define VREC vsrec
#  define VSQRT vssqrt
#else
#  define VREC vrec
#  define VSQRT vsqrt
#endif

!Including inline expansion statistical function 
MODULE module_mp_wsm5
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
   REAL, PARAMETER, PRIVATE :: pfrz1 = 100.       ! constant in Biggs freezing
   REAL, PARAMETER, PRIVATE :: pfrz2 = 0.66       ! constant in Biggs freezing
   REAL, PARAMETER, PRIVATE :: qcrmin = 1.e-9     ! minimun values for qr, qs, and qg
   REAL, PARAMETER, PRIVATE :: eacrc = 1.0        ! Snow/cloud-water collection efficiency
   REAL, SAVE ::                                      &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr, &
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,    &
             precr1,precr2,xmmax,roqimax,bvts1,       &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,     &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r, &
             pidn0s,xlv1,pacrc,                       &
             rslopermax,rslopesmax,rslopegmax,        &
             rsloperbmax,rslopesbmax,rslopegbmax,     &
             rsloper2max,rslopes2max,rslopeg2max,     &
             rsloper3max,rslopes3max,rslopeg3max
!
! Specifies code-inlining of fpvs function in WSM52D below. JM 20040507
!
CONTAINS
!===================================================================
!
  SUBROUTINE wsm5(th, q, qc, qr, qi, qs                            &
                 ,den, pii, p, delz                                &
                 ,delt,g, cpd, cpv, rd, rv, t0c                    &
                 ,ep1, ep2, qmin                                   &
                 ,XLS, XLV0, XLF0, den0, denr                      &
                 ,cliq,cice,psat                                   &
                 ,rain, rainncv                                    &
                 ,snow, snowncv                                    &
                 ,sr                                               &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
#ifdef _OPENMP
  use omp_lib
#endif
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a 5-class mixed ice microphyiscs scheme (WSM5) of the WRF
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. A theoretical background of the ice-microphysics and related
!  processes in the WSMMPs are described in Hong et al. (2004).
!  Production terms in the WSM6 scheme are described in Hong and Lim (2006).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WSM5 cloud scheme
!
!  Coded by Song-You Hong (Yonsei Univ.)
!             Jimy Dudhia (NCAR) and Shu-Hua Chen (UC Davis)
!             Summer 2002
!
!  Implemented by Song-You Hong (Yonsei Univ.) and Jimy Dudhia (NCAR)
!             Summer 2003
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Rutledge, Hobbs (RH83, 1983) J. Atmos. Sci.
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc.
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

! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci, qrs
  CHARACTER*256 :: emess
  INTEGER :: mkx_test
  INTEGER ::               i,j,k

#ifdef _ACCEL_PROF
  INTEGER :: l
  real*8 wsm3_t(8,256), wsm5_t(8,256), t1, t2
  common /wsm_times/ wsm3_t(8,256), wsm5_t(8,256)
#endif


!-------------------------------------------------------------------

#ifdef _ACCEL_PROF
call cpu_time(t1)
#endif

#ifndef RUN_ON_GPU

#ifdef _ACCEL

      !  Need to send th, pii, qc, qi, qr, qs
      !  Don't send t

      CALL wsm52D(th, pii, q, qc, qr, qi, qs                       &
                  ,den                                             &
                  ,p, delz                                         &
                  ,delt,g, cpd, cpv, rd, rv, t0c                   &
                  ,ep1, ep2, qmin                                  &
                  ,XLS, XLV0, XLF0, den0, denr                     &
                  ,cliq,cice,psat                                  &
                  ,rain,rainncv                                    &
                  ,sr                                              &
                  ,ids,ide, jds,jde, kds,kde                       &
                  ,ims,ime, jms,jme, kms,kme                       &
                  ,its,ite, jts,jte, kts,kte                       &
                  ,snow,snowncv                                    &
                                                                   )

#else

      DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
         ENDDO
         ENDDO

         !  Sending array starting locations of optional variables may cause
         !  troubles, so we explicitly change the call.

         CALL wsm52D(t, q(ims,kms,j), qci, qrs                     &
                    ,den(ims,kms,j)                                &
                    ,p(ims,kms,j), delz(ims,kms,j)                 &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,j                                             &
                    ,rain(ims,j),rainncv(ims,j)                    &
                    ,sr(ims,j)                                     &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                    ,snow,snowncv                                  &
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
#endif
#else
      CALL get_wsm5_gpu_levels ( mkx_test )
      IF ( mkx_test .LT. kte ) THEN
        WRITE(emess,*)'Number of levels compiled for GPU WSM5 too small. ',      &
                      mkx_test,' < ',kte
        CALL wrf_error_fatal(emess)
      ENDIF
      CALL wsm5_host (                                                           &
                    th(its:ite,kts:kte,jts:jte), pii(its:ite,kts:kte,jts:jte)    &
                   ,q(its:ite,kts:kte,jts:jte), qc(its:ite,kts:kte,jts:jte)      &
                   ,qi(its:ite,kts:kte,jts:jte), qr(its:ite,kts:kte,jts:jte)     &
                   ,qs(its:ite,kts:kte,jts:jte), den(its:ite,kts:kte,jts:jte)    &
                   ,p(its:ite,kts:kte,jts:jte), delz(its:ite,kts:kte,jts:jte)    &
                   ,delt                                                         &
                   ,rain(its:ite,jts:jte),rainncv(its:ite,jts:jte)               &
                   ,snow(its:ite,jts:jte),snowncv(its:ite,jts:jte)               &
                   ,sr(its:ite,jts:jte)                                          &
                   ,its, ite,  jts, jte,  kts, kte                               &
                   ,its, ite,  jts, jte,  kts, kte                               &
                   ,its, ite,  jts, jte,  kts, kte                               &
          )
#endif


#ifdef _ACCEL_PROF
  call cpu_time(t2)
#ifdef _OPENMP
  l = omp_get_thread_num() + 1
#else
  l = 1
#endif
  wsm5_t(1,l) = wsm5_t(1,l) + (t2 - t1)
#endif

  END SUBROUTINE wsm5


#ifdef _ACCEL

!===================================================================
!
  SUBROUTINE wsm52D(th, pii, q, qc, qr, qi, qqs, den, p, delz     &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                                                                  )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
  INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT) ::                                          &
                                                              th
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(IN) ::                                             &
                                                             pii
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                  &
        INTENT(INOUT) ::                                          &
                                                              qc, &
                                                              qr, &
                                                              qi, &
                                                             qqs
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                  &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                  &
        INTENT(IN   ) ::                                          &
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
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr

  REAL, DIMENSION( ims:ime, jms:jme ),     OPTIONAL,              &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv

! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
                                                              rh, &
                                                              qs, &
                                                          rslope, &
                                                         rslope2, &
                                                         rslope3, &
                                                         rslopeb, &
                                                            falk, &
                                                            fall, &
                                                           work1
  REAL, DIMENSION( its:ite , kts:kte, jts:jte ) ::                &
                                                               t
  REAL, DIMENSION( its:ite , kts:kte , 2 ) ::                     &
                                                             qci, &
                                                             qrs
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           falkc, &
                                                           fallc, &
                                                              xl, &
                                                             cpm, &
                                                          denfac, &
                                                             xni, &
                                                          n0sfac, &
                                                           work2, &
                                                          work1c, &
                                                          work2c

  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           pigen, &
                                                           pidep, &
                                                           psdep, &
                                                           praut, &
                                                           psaut, &
                                                           prevp, &
                                                           psevp, &
                                                           pracw, &
                                                           psacw, &
                                                           psaci, &
                                                           pcond, &
                                                           psmlt
  INTEGER ::                                                      &
                                                           mstep, &
                                                           numdt
  REAL ::                                                 rmstep
  REAL dtcldden, rdelz, rdtcld
  LOGICAL ::                                              flgcld

#define WSM_NO_CONDITIONAL_IN_VECTOR
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
  REAL ::                     xal, xbl
#endif

  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdar, lamdas, diffus,                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, supcol, supcolt, pvt,            &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            vt2i,vt2s,acrfac,                                     &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, xlwork2, factor, source,        &
            value, xlf, pfrzdtc, pfrzdtr, supice,  holdc, holdci
! variables for optimization
  REAL, DIMENSION( its:ite )           ::                    tvec1
  REAL ::                                                    temp 
  INTEGER :: i, j, k,                                             &
            iprt, latd, lond, loop, loops, ifsat, n
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
  REAL  :: logtr
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
!     diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
!     viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
!     xka(x,y) = 1.414e3*viscos(x,y)*y
!     diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
!     venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
!                    /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
!     conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!
!
      pi = 4. * atan(1.)
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!

!
! Moved outside of accelerator region
!
      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt
!

!....!$acc        local(t) &

      IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN

!$acc region &
!$acc        local(t) &
!$acc        copyin(delz(:,:,:),p(:,:,:),den(:,:,:),pii(:,:,:)) &
!$acc        copyout(snowncv(:,:),rainncv(:,:),sr(:,:)) &
!$acc        copy(qqs(:,:,:),qr(:,:,:),qi(:,:,:),qc(:,:,:)) &
!$acc        copy(th(:,:,:),q(:,:,:),snow(:,:),rain(:,:))
!$acc do &
!$acc        private(rh,qs,rslope,rslope2,rslope3,rslopeb,falk,fall) &
!$acc        private(work1,qci,qrs,falkc,fallc,xl,cpm,denfac,xni) &
!$acc        private(n0sfac,work2,work1c,work2c,pigen,pidep,psdep) &
!$acc        private(praut,psaut,prevp,psevp) &
!$acc        private(pracw,psacw,psaci,pcond,psmlt) &
!$acc        parallel
      do j = jts, jte
!$acc do &
!$acc        private(numdt,mstep) &
!$acc        kernel vector
        do i = its, ite
        do k = kts, kte
          t(i,k,j)=th(i,k,j)*pii(i,k,j)
          qci(i,k,1) = max(qc(i,k,j),0.0)
          qci(i,k,2) = max(qi(i,k,j),0.0)
          qrs(i,k,1) = max(qr(i,k,j),0.0)
          qrs(i,k,2) = max(qqs(i,k,j),0.0)
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
!----------------------------------------------------------------
!     compute the minor time steps.
!
!     loops = max(nint(delt/dtcldcr),1)
!     dtcld = delt/loops
!     if(delt.le.dtcldcr) dtcld = delt
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
        mstep = 1
        flgcld = .true.
!
      do k = kts, kte
          denfac(i,k) = sqrt(den0/den(i,k,j))
      enddo
!     do k = kts, kte
!       CALL VREC( tvec1(its), den(its,k,j), ite-its+1)
!       do i = its, ite
!         tvec1(i) = tvec1(i)*den0
!       enddo
!       CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1)
!     enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k,j),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
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

! this is for compilers where the conditional inhibits vectorization
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
      do k = kts, kte
          if(t(i,k,j).lt.ttp) then
            xal = xai
            xbl = xbi
          else
            xal = xa
            xbl = xb
          endif
          tr=ttp/t(i,k,j)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k,j) / qs(i,k,1),qmin)
          qs(i,k,2)=psat*exp(logtr*(xal)+xbl*(1.-tr))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k,j) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k,j) / qs(i,k,2),qmin)
      enddo
#else
      do k = kts, kte
          tr=ttp/t(i,k,j)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k,j) / qs(i,k,1),qmin)
          if(t(i,k,j).lt.ttp) then
            qs(i,k,2)=psat*exp(logtr*(xai)+xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(logtr*(xa)+xb*(1.-tr))
          endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k,j) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k,j) / qs(i,k,2),qmin)
      enddo
#endif

!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pracw(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          psevp(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          xni(i,k) = 1.e3
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
          supcol = t0c-t(i,k,j)
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
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k,j))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k,j),n0sfac(i,k))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k,j)                                  &
!                   *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k,j)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
      enddo
!
      numdt = 1
      do k = kte, kts, -1
          work1(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)/delz(i,k,j)
          work1(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)/delz(i,k,j)
          numdt = max(nint(max(work1(i,k,1),work1(i,k,2))*dtcld+.5),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
        rmstep = 1./mstep
!
      do n = 1, mstep
        k = kte
!             falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
!             falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)*rmstep
              falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)*rmstep
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
!             qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k,j),0.)
!             qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k,j),0.)
              dtcldden = dtcld/den(i,k,j)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcldden,0.)
!           endif
        do k = kte-1, kts, -1
              falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)*rmstep
              falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)*rmstep
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              dtcldden = dtcld/den(i,k,j)
              rdelz = 1./delz(i,k,j)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1,j)*rdelz)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)           &
                          *delz(i,k+1,j)*rdelz)*dtcldden,0.)
        enddo
        do k = kte, kts, -1
              if(t(i,k,j).gt.t0c.and.qrs(i,k,2).gt.0.) then
!----------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!----------------------------------------------------------------
                xlf = xlf0
!               work2(i,k)= venfac(p(i,k),t(i,k,j),den(i,k,j))
                work2(i,k)= (exp(log(((1.496e-6*((t(i,k,j))*sqrt(t(i,k,j)))        &
                            /((t(i,k,j))+120.)/(den(i,k,j)))/(8.794e-5             &
                            *exp(log(t(i,k,j))*(1.81))/p(i,k,j))))                 &
                            *((.3333333)))/sqrt((1.496e-6*((t(i,k,j))            &
                            *sqrt(t(i,k,j)))/((t(i,k,j))+120.)/(den(i,k,j))))        &
                            *sqrt(sqrt(den0/(den(i,k,j)))))
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
!               psmlt(i,k) = xka(t(i,k,j),den(i,k,j))/xlf*(t0c-t(i,k,j))*pi/2.       &
!                           *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
!                           *work2(i,k)*coeres)
                psmlt(i,k) = (1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j)))        &         
                            /((t(i,k,j))+120.)/(den(i,k,j)) )*(den(i,k,j)))          &
                            /xlf*(t0c-t(i,k,j))*pi/2.                            &
                            *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
                            *work2(i,k)*coeres)
                psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep,                &
                            -qrs(i,k,2)/mstep),0.)
                qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
                t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*psmlt(i,k)
              endif
        enddo
      enddo


!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      mstep = 1
      numdt = 1
      do k = kte, kts, -1
          if(qci(i,k,2).le.0.) then
            work2c(i,k) = 0.
          else
            xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
!           diameter  = min(dicon * sqrt(xmi),dimax)
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
            work2c(i,k) = work1c(i,k)/delz(i,k,j)
          endif
          numdt = max(nint(work2c(i,k)*dtcld+.5),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
!
      do n = 1, mstep
        k = kte
            falkc(i,k) = den(i,k,j)*qci(i,k,2)*work2c(i,k)/mstep
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k,2)
            qci(i,k,2) = max(qci(i,k,2)-falkc(i,k)*dtcld/den(i,k,j),0.)
!         endif
        do k = kte-1, kts, -1
              falkc(i,k) = den(i,k,j)*qci(i,k,2)*work2c(i,k)/mstep
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k,2)
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)             &
                          *delz(i,k+1,j)/delz(i,k,j))*dtcld/den(i,k,j),0.)
!           endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
        fallsum = fall(i,1,1)+fall(i,1,2)+fallc(i,1)
        fallsum_qsi = fall(i,1,2)+fallc(i,1)
        rainncv(i,j) = 0.
        if(fallsum.gt.0.) then
          rainncv(i,j) = fallsum*delz(i,1,j)/denr*dtcld*1000.
          rain(i,j) = fallsum*delz(i,1,j)/denr*dtcld*1000. + rain(i,j)
        endif
        snowncv(i,j) = 0.
        if(fallsum_qsi.gt.0.) then
          snowncv(i,j) = fallsum_qsi*delz(i,kts,j)/denr*dtcld*1000.
          snow(i,j) = fallsum_qsi*delz(i,kts,j)/denr*dtcld*1000. + snow(i,j)
        endif
        sr(i,j) = 0.
        if(fallsum.gt.0.)sr(i,j)=fallsum_qsi*delz(i,kts,j)/denr*dtcld*1000.        &    
        /(rainncv(i,j)+1.e-12)
!
!---------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
          supcol = t0c-t(i,k,j)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0.and.qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k,j) = t(i,k,j) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40..and.qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qci(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                         &
!              *den(i,k,j)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcolt)-1.)                        &
            *den(i,k,j)/denr/xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! psfrz: freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k,j)                    &
!                 *(exp(pfrz2*supcol)-1.)*rslope(i,k,1)**7*dtcld,              &
!                 qrs(i,k,1))
            temp = rslope(i,k,1)
            temp = temp*temp*temp*temp*temp*temp*temp
            pfrzdtr = min(20.*(pi*pi)*pfrz1*n0r*denr/den(i,k,j)                  &
                  *(exp(pfrz2*supcolt)-1.)*temp*dtcld,                         &
                  qrs(i,k,1))
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
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
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
!           rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k,j))
            rslope(i,k,1) = 1./(sqrt(sqrt(pidn0r/((qrs(i,k,1))*(den(i,k,j))))))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
!            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k,j),n0sfac(i,k))
            rslope(i,k,2) = 1./(sqrt(sqrt(pidn0s*(n0sfac(i,k))/((qrs(i,k,2))   &  
                          *(den(i,k,j))))))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
      enddo
!
      do k = kts, kte
!         work1(i,k,1) = diffac(xl(i,k),p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k,1))
          work1(i,k,1) = ((((den(i,k,j))*(xl(i,k))*(xl(i,k)))*((t(i,k,j))+120.)    &
                       *(den(i,k,j)))/(1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))&
                       *(den(i,k,j))*(rv*(t(i,k,j))*(t(i,k,j)))))                    &
                      +  p(i,k,j)/((qs(i,k,1))*(8.794e-5*exp(log(t(i,k,j))*(1.81))))
!         work1(i,k,2) = diffac(xls,p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k,2))
          work1(i,k,2) = ((((den(i,k,j))*(xls)*(xls))*((t(i,k,j))+120.)*(den(i,k,j)))&
                       /(1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))*(den(i,k,j)) &
                       *(rv*(t(i,k,j))*(t(i,k,j))))                                &
                      + p(i,k,j)/(qs(i,k,2)*(8.794e-5*exp(log(t(i,k,j))*(1.81)))))
!         work2(i,k) = venfac(p(i,k,j),t(i,k,j),den(i,k,j))
          work2(i,k) = (exp(.3333333*log(((1.496e-6 * ((t(i,k,j))*sqrt(t(i,k,j)))) &
                     *p(i,k,j))/(((t(i,k,j))+120.)*den(i,k,j)*(8.794e-5              &
                     *exp(log(t(i,k,j))*(1.81))))))*sqrt(sqrt(den0/(den(i,k,j))))) &
                      /sqrt((1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))                 &
                      /(((t(i,k,j))+120.)*den(i,k,j)))
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
          supsat = max(q(i,k,j),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          if(qci(i,k,1).gt.qc0) then
            praut(i,k) = qck1*exp(log(qci(i,k,1))*((7./3.)))
            praut(i,k) = min(praut(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pracw(i,k) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)               & 
                         *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope2(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*(precr1*rslope2(i,k,1)                 &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
            else
              prevp(i,k) = min(prevp(i,k),satdt/2)
            endif
          endif
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
      rdtcld = 1./dtcld
      do k = kts, kte
          supcol = t0c-t(i,k,j)
          supsat = max(q(i,k,j),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k,j)                                  &
!                      *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k,j)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
          eacrs = exp(0.07*(-supcol))
!
          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qmin) then
              xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
              vt2s = pvts*rslopeb(i,k,2)*denfac(i,k)
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by rain [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      +diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                           *abs(vt2s-vt2i)*acrfac/4.
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)                  &
                           *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)              &
!                          ,qci(i,k,1)/dtcld)
                           ,qci(i,k,1)*rdtcld)
          endif
          if(supcol .gt. 0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
!               pidep(i,k) = max(max(pidep(i,k),satdt/2),supice)
!               pidep(i,k) = max(pidep(i,k),-qci(i,k,2)/dtcld)
                pidep(i,k) = max(max(pidep(i,k),satdt*.5),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)*rdtcld)
              else
!               pidep(i,k) = min(min(pidep(i,k),satdt/2),supice)
                pidep(i,k) = min(min(pidep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)                          &
                           *(precs1*rslope2(i,k,2)+precs2                      &
                           *work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
!               psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)/dtcld)
!               psdep(i,k) = max(max(psdep(i,k),satdt/2),supice)
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)*rdtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt*.5),supice)
              else
!             psdep(i,k) = min(min(psdep(i,k),satdt/2),supice)
                psdep(i,k) = min(min(psdep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt))          &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL A50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pigen(i,k) = max(0.,(roqi0/den(i,k,j)-max(qci(i,k,2),0.))          &
!                        /dtcld)
                         *rdtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k,j)
!             psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
            endif
          endif
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.)                           &
              psevp(i,k) = psdep(i,k)*work1(i,k,2)/work1(i,k,1)
!              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0.)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)*rdtcld),0.)
          endif
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
          if(t(i,k,j).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)+psaci(i,k)-pigen(i,k)-pidep(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
            endif
!
!     rain
!
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
            endif
!
!    snow
!
            value = max(qmin,qrs(i,k,2))
            source = (-psdep(i,k)-psaut(i,k)-psaci(i,k)-psacw(i,k))*dtcld  
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pigen(i,k)+pidep(i,k))
!     update
            q(i,k,j) = q(i,k,j)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+psaci(i,k)                 &
                        -pigen(i,k)-pidep(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)                 &
                        +psaci(i,k)+psacw(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pidep(i,k)+pigen(i,k))                  &
                      -xl(i,k)*prevp(i,k)-xlf*psacw(i,k)
            t(i,k,j) = t(i,k,j)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source=(praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)-psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif  
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psevp(i,k) = psevp(i,k)*factor
            endif
            work2(i,k)=-(prevp(i,k)+psevp(i,k))
!     update
            q(i,k,j) = q(i,k,j)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k) +psacw(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k))
            t(i,k,j) = t(i,k,j)-xlwork2/cpm(i,k)*dtcld
          endif
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k,j),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
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
          tr=ttp/t(i,k,j)
          logtr = log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
      enddo
!
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
!         work1(i,k,1) = conden(t(i,k,j),q(i,k,j),qs(i,k,1),xl(i,k),cpm(i,k))
          work1(i,k,1) = ((max(q(i,k,j),qmin)-(qs(i,k,1)))/(1.+(xl(i,k))         &   
                        *(xl(i,k))/(rv*(cpm(i,k)))*(qs(i,k,1))                 &
                        /((t(i,k,j))*(t(i,k,j)))))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k,j),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                          &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k,j) = q(i,k,j)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k,j) = t(i,k,j)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
      enddo
!
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0
      enddo
      enddo                  ! big loops

         DO K=kts,kte
            th(i,k,j)=t(i,k,j)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qqs(i,k,j) = qrs(i,k,2)
         ENDDO



      ENDDO     !  i loop
      enddo     !  j loop
!$acc end region

    ELSE

!
! Moved outside of accelerator region
!
      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt

!$acc region &
!$acc        local(t) &
!$acc        copyin(delz(:,:,:),p(:,:,:),den(:,:,:),pii(:,:,:)) &
!$acc        copyout(rainncv(:,:),sr(:,:)) &
!$acc        copy(qqs(:,:,:),qr(:,:,:),qi(:,:,:),qc(:,:,:)) &
!$acc        copy(th(:,:,:),q(:,:,:),rain(:,:))
!$acc do &
!$acc        private(rh,qs,rslope,rslope2,rslope3,rslopeb,falk,fall) &
!$acc        private(work1,qci,qrs,falkc,fallc,xl,cpm,denfac,xni) &
!$acc        private(n0sfac,work2,work1c,work2c,pigen,pidep,psdep) &
!$acc        private(praut,psaut,prevp,psevp) &
!$acc        private(pracw,psacw,psaci,pcond,psmlt) &
!$acc        parallel
      do j = jts, jte
!$acc do &
!$acc        private(numdt,mstep) &
!$acc        kernel vector
        do i = its, ite
        do k = kts, kte
          t(i,k,j)=th(i,k,j)*pii(i,k,j)
          qci(i,k,1) = max(qc(i,k,j),0.0)
          qci(i,k,2) = max(qi(i,k,j),0.0)
          qrs(i,k,1) = max(qr(i,k,j),0.0)
          qrs(i,k,2) = max(qqs(i,k,j),0.0)
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
!----------------------------------------------------------------
!     compute the minor time steps.
!
!     loops = max(nint(delt/dtcldcr),1)
!     dtcld = delt/loops
!     if(delt.le.dtcldcr) dtcld = delt
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
        mstep = 1
        flgcld = .true.
!
      do k = kts, kte
          denfac(i,k) = sqrt(den0/den(i,k,j))
      enddo
!     do k = kts, kte
!       CALL VREC( tvec1(its), den(its,k,j), ite-its+1)
!       do i = its, ite
!         tvec1(i) = tvec1(i)*den0
!       enddo
!       CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1)
!     enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k,j),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
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

! this is for compilers where the conditional inhibits vectorization
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
      do k = kts, kte
          if(t(i,k,j).lt.ttp) then
            xal = xai
            xbl = xbi
          else
            xal = xa
            xbl = xb
          endif
          tr=ttp/t(i,k,j)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k,j) / qs(i,k,1),qmin)
          qs(i,k,2)=psat*exp(logtr*(xal)+xbl*(1.-tr))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k,j) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k,j) / qs(i,k,2),qmin)
      enddo
#else
      do k = kts, kte
          tr=ttp/t(i,k,j)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k,j) / qs(i,k,1),qmin)
          if(t(i,k,j).lt.ttp) then
            qs(i,k,2)=psat*exp(logtr*(xai)+xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(logtr*(xa)+xb*(1.-tr))
          endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k,j) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k,j) / qs(i,k,2),qmin)
      enddo
#endif
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pracw(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          psevp(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          xni(i,k) = 1.e3
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!
      do k = kts, kte
          supcol = t0c-t(i,k,j)
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
            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k,j))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k,j),n0sfac(i,k))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k,j)                                  &
!                   *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k,j)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
      enddo
!
      numdt = 1
      do k = kte, kts, -1
          work1(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)/delz(i,k,j)
          work1(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)/delz(i,k,j)
          numdt = max(nint(max(work1(i,k,1),work1(i,k,2))*dtcld+.5),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
        rmstep = 1./mstep
!
      do n = 1, mstep
        k = kte
!             falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
!             falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)*rmstep
              falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)*rmstep
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
!             qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k,j),0.)
!             qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k,j),0.)
              dtcldden = dtcld/den(i,k,j)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcldden,0.)
!           endif
        do k = kte-1, kts, -1
              falk(i,k,1) = den(i,k,j)*qrs(i,k,1)*work1(i,k,1)*rmstep
              falk(i,k,2) = den(i,k,j)*qrs(i,k,2)*work1(i,k,2)*rmstep
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              dtcldden = dtcld/den(i,k,j)
              rdelz = 1./delz(i,k,j)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1,j)*rdelz)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)           &
                          *delz(i,k+1,j)*rdelz)*dtcldden,0.)
        enddo
        do k = kte, kts, -1
              if(t(i,k,j).gt.t0c.and.qrs(i,k,2).gt.0.) then
!----------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!----------------------------------------------------------------
                xlf = xlf0
!               work2(i,k)= venfac(p(i,k),t(i,k,j),den(i,k,j))
                work2(i,k)= (exp(log(((1.496e-6*((t(i,k,j))*sqrt(t(i,k,j)))        &
                            /((t(i,k,j))+120.)/(den(i,k,j)))/(8.794e-5             &
                            *exp(log(t(i,k,j))*(1.81))/p(i,k,j))))                 &
                            *((.3333333)))/sqrt((1.496e-6*((t(i,k,j))            &
                            *sqrt(t(i,k,j)))/((t(i,k,j))+120.)/(den(i,k,j))))        &
                            *sqrt(sqrt(den0/(den(i,k,j)))))
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
!               psmlt(i,k) = xka(t(i,k,j),den(i,k,j))/xlf*(t0c-t(i,k,j))*pi/2.       &
!                           *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
!                           *work2(i,k)*coeres)
                psmlt(i,k) = (1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j)))        &         
                            /((t(i,k,j))+120.)/(den(i,k,j)) )*(den(i,k,j)))          &
                            /xlf*(t0c-t(i,k,j))*pi/2.                            &
                            *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
                            *work2(i,k)*coeres)
                psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep,                &
                            -qrs(i,k,2)/mstep),0.)
                qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
                t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*psmlt(i,k)
              endif
        enddo
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      mstep = 1
      numdt = 1
      do k = kte, kts, -1
          if(qci(i,k,2).le.0.) then
            work2c(i,k) = 0.
          else
            xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
!           diameter  = min(dicon * sqrt(xmi),dimax)
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
            work2c(i,k) = work1c(i,k)/delz(i,k,j)
          endif
          numdt = max(nint(work2c(i,k)*dtcld+.5),1)
          if(numdt.ge.mstep) mstep = numdt
      enddo
!
      do n = 1, mstep
        k = kte
            falkc(i,k) = den(i,k,j)*qci(i,k,2)*work2c(i,k)/mstep
            holdc = falkc(i,k)
            fallc(i,k) = fallc(i,k)+falkc(i,k)
            holdci = qci(i,k,2)
            qci(i,k,2) = max(qci(i,k,2)-falkc(i,k)*dtcld/den(i,k,j),0.)
        do k = kte-1, kts, -1
              falkc(i,k) = den(i,k,j)*qci(i,k,2)*work2c(i,k)/mstep
              holdc = falkc(i,k)
              fallc(i,k) = fallc(i,k)+falkc(i,k)
              holdci = qci(i,k,2)
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)             &
                          *delz(i,k+1,j)/delz(i,k,j))*dtcld/den(i,k,j),0.)
        enddo
      enddo
!
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
        fallsum = fall(i,1,1)+fall(i,1,2)+fallc(i,1)
        fallsum_qsi = fall(i,1,2)+fallc(i,1)
        rainncv(i,j) = 0.
        if(fallsum.gt.0.) then
          rainncv(i,j) = fallsum*delz(i,1,j)/denr*dtcld*1000.
          rain(i,j) = fallsum*delz(i,1,j)/denr*dtcld*1000. + rain(i,j)
        endif
        sr(i,j) = 0.
        if(fallsum.gt.0.)sr(i,j)=fallsum_qsi*delz(i,kts,j)/denr*dtcld*1000.        &    
        /(rainncv(i,j)+1.e-12)
!
!---------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
          supcol = t0c-t(i,k,j)
          xlf = xls-xl(i,k)
          if(supcol.lt.0.) xlf = xlf0
          if(supcol.lt.0.and.qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k,j) = t(i,k,j) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40..and.qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qci(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                         &
!              *den(i,k,j)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcolt)-1.)                        &
            *den(i,k,j)/denr/xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! psfrz: freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k,j)                    &
!                 *(exp(pfrz2*supcol)-1.)*rslope(i,k,1)**7*dtcld,              &
!                 qrs(i,k,1))
            temp = rslope(i,k,1)
            temp = temp*temp*temp*temp*temp*temp*temp
            pfrzdtr = min(20.*(pi*pi)*pfrz1*n0r*denr/den(i,k,j)                  &
                  *(exp(pfrz2*supcolt)-1.)*temp*dtcld,                         &
                  qrs(i,k,1))
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k,j) = t(i,k,j) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
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
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
!           rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k,j))
            rslope(i,k,1) = 1./(sqrt(sqrt(pidn0r/((qrs(i,k,1))*(den(i,k,j))))))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
!            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k,j),n0sfac(i,k))
            rslope(i,k,2) = 1./(sqrt(sqrt(pidn0s*(n0sfac(i,k))/((qrs(i,k,2))   &  
                          *(den(i,k,j))))))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
      enddo
!
      do k = kts, kte
!         work1(i,k,1) = diffac(xl(i,k),p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k,1))
          work1(i,k,1) = ((((den(i,k,j))*(xl(i,k))*(xl(i,k)))*((t(i,k,j))+120.)    &
                       *(den(i,k,j)))/(1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))&
                       *(den(i,k,j))*(rv*(t(i,k,j))*(t(i,k,j)))))                    &
                      +  p(i,k,j)/((qs(i,k,1))*(8.794e-5*exp(log(t(i,k,j))*(1.81))))
!         work1(i,k,2) = diffac(xls,p(i,k,j),t(i,k,j),den(i,k,j),qs(i,k,2))
          work1(i,k,2) = ((((den(i,k,j))*(xls)*(xls))*((t(i,k,j))+120.)*(den(i,k,j)))&
                       /(1.414e3*(1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))*(den(i,k,j)) &
                       *(rv*(t(i,k,j))*(t(i,k,j))))                                &
                      + p(i,k,j)/(qs(i,k,2)*(8.794e-5*exp(log(t(i,k,j))*(1.81)))))
!         work2(i,k) = venfac(p(i,k,j),t(i,k,j),den(i,k,j))
          work2(i,k) = (exp(.3333333*log(((1.496e-6 * ((t(i,k,j))*sqrt(t(i,k,j)))) &
                     *p(i,k,j))/(((t(i,k,j))+120.)*den(i,k,j)*(8.794e-5              &
                     *exp(log(t(i,k,j))*(1.81))))))*sqrt(sqrt(den0/(den(i,k,j))))) &
                      /sqrt((1.496e-6*((t(i,k,j))*sqrt(t(i,k,j))))                 &
                      /(((t(i,k,j))+120.)*den(i,k,j)))
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
          supsat = max(q(i,k,j),qmin)-qs(i,k,1)
          satdt = supsat/dtcld
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          if(qci(i,k,1).gt.qc0) then
            praut(i,k) = qck1*exp(log(qci(i,k,1))*((7./3.)))
            praut(i,k) = min(praut(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pracw(i,k) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)               & 
                         *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope2(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*(precr1*rslope2(i,k,1)                 &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
            else
              prevp(i,k) = min(prevp(i,k),satdt/2)
            endif
          endif
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
      rdtcld = 1./dtcld
      do k = kts, kte
          supcol = t0c-t(i,k,j)
          supsat = max(q(i,k,j),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k,j)                                  &
!                      *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k,j)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
          eacrs = exp(0.07*(-supcol))
!
          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qmin) then
              xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
              vt2s = pvts*rslopeb(i,k,2)*denfac(i,k)
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by rain [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      +diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                           *abs(vt2s-vt2i)*acrfac/4.
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)                  &
                           *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)              &
!                          ,qci(i,k,1)/dtcld)
                           ,qci(i,k,1)*rdtcld)
          endif
          if(supcol .gt. 0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              xmi = den(i,k,j)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
!               pidep(i,k) = max(max(pidep(i,k),satdt/2),supice)
!               pidep(i,k) = max(pidep(i,k),-qci(i,k,2)/dtcld)
                pidep(i,k) = max(max(pidep(i,k),satdt*.5),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)*rdtcld)
              else
!               pidep(i,k) = min(min(pidep(i,k),satdt/2),supice)
                pidep(i,k) = min(min(pidep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)                          &
                           *(precs1*rslope2(i,k,2)+precs2                      &
                           *work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
!               psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)/dtcld)
!               psdep(i,k) = max(max(psdep(i,k),satdt/2),supice)
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)*rdtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt*.5),supice)
              else
!             psdep(i,k) = min(min(psdep(i,k),satdt/2),supice)
                psdep(i,k) = min(min(psdep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt))          &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL A50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pigen(i,k) = max(0.,(roqi0/den(i,k,j)-max(qci(i,k,2),0.))          &
!                        /dtcld)
                         *rdtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k,j)
!             psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
            endif
          endif
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.)                           &
              psevp(i,k) = psdep(i,k)*work1(i,k,2)/work1(i,k,1)
!              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0.)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)*rdtcld),0.)
          endif
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
          if(t(i,k,j).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)+psaci(i,k)-pigen(i,k)-pidep(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
            endif
!
!     rain
!
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
            endif
!
!    snow
!
            value = max(qmin,qrs(i,k,2))
            source = (-psdep(i,k)-psaut(i,k)-psaci(i,k)-psacw(i,k))*dtcld  
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pigen(i,k)+pidep(i,k))
!     update
            q(i,k,j) = q(i,k,j)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+psaci(i,k)                 &
                        -pigen(i,k)-pidep(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)                 &
                        +psaci(i,k)+psacw(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pidep(i,k)+pigen(i,k))                  &
                      -xl(i,k)*prevp(i,k)-xlf*psacw(i,k)
            t(i,k,j) = t(i,k,j)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source=(praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)-psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif  
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psevp(i,k) = psevp(i,k)*factor
            endif
            work2(i,k)=-(prevp(i,k)+psevp(i,k))
!     update
            q(i,k,j) = q(i,k,j)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k) +psacw(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k))
            t(i,k,j) = t(i,k,j)-xlwork2/cpm(i,k)*dtcld
          endif
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k,j),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k,j),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
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
          tr=ttp/t(i,k,j)
          logtr = log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k,j) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
      enddo
!
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
!         work1(i,k,1) = conden(t(i,k,j),q(i,k,j),qs(i,k,1),xl(i,k),cpm(i,k))
          work1(i,k,1) = ((max(q(i,k,j),qmin)-(qs(i,k,1)))/(1.+(xl(i,k))         &   
                        *(xl(i,k))/(rv*(cpm(i,k)))*(qs(i,k,1))                 &
                        /((t(i,k,j))*(t(i,k,j)))))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k,j),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                          &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k,j) = q(i,k,j)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k,j) = t(i,k,j)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
      enddo
!
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0
      enddo
      enddo                  ! big loops

         DO K=kts,kte
            th(i,k,j)=t(i,k,j)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qqs(i,k,j) = qrs(i,k,2)
         ENDDO

      ENDDO     !  i loop
      enddo     !  j loop
!$acc end region

    ENDIF

  END SUBROUTINE wsm52d


#else

!===================================================================
!
  SUBROUTINE wsm52D(t, q, qci, qrs, den, p, delz                  &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
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
                                                             qci, &
                                                             qrs
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

  REAL, DIMENSION( ims:ime, jms:jme ),     OPTIONAL,              &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv

! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
                                                              rh, &
                                                              qs, &
                                                          rslope, &
                                                         rslope2, &
                                                         rslope3, &
                                                         rslopeb, &
                                                            falk, &
                                                            fall, &
                                                           work1

  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           falkc, &
                                                           fallc, &
                                                              xl, &
                                                             cpm, &
                                                          denfac, &
                                                             xni, &
                                                          n0sfac, &
                                                           work2, &
                                                          work1c, &
                                                          work2c

  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           pigen, &
                                                           pidep, &
                                                           psdep, &
                                                           praut, &
                                                           psaut, &
                                                           prevp, &
                                                           psevp, &
                                                           pracw, &
                                                           psacw, &
                                                           psaci, &
                                                           pcond, &
                                                           psmlt
  INTEGER, DIMENSION( its:ite ) ::                                &
                                                           mstep, &
                                                           numdt
  REAL, DIMENSION(its:ite) ::                             rmstep
  REAL dtcldden, rdelz, rdtcld
  LOGICAL, DIMENSION( its:ite ) ::                        flgcld

#define WSM_NO_CONDITIONAL_IN_VECTOR
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
  REAL, DIMENSION(its:ite) :: xal, xbl
#endif

  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdar, lamdas, diffus,                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, supcol, supcolt, pvt,            &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            vt2i,vt2s,acrfac,                                     &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, fallsum_qsi, xlwork2, factor, source,        &
            value, xlf, pfrzdtc, pfrzdtr, supice,  holdc, holdci
! variables for optimization
  REAL, DIMENSION( its:ite )           ::                    tvec1
  REAL ::                                                    temp 
  INTEGER :: i, j, k, mstepmax,                                   &
            iprt, latd, lond, loop, loops, ifsat, n
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
  REAL  :: logtr
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
!     diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
!     viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
!     xka(x,y) = 1.414e3*viscos(x,y)*y
!     diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
!     venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
!                    /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
!     conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!
!
      pi = 4. * atan(1.)
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

! this is for compilers where the conditional inhibits vectorization
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
      do k = kts, kte
        do i = its, ite
          if(t(i,k).lt.ttp) then
            xal(i) = xai
            xbl(i) = xbi
          else
            xal(i) = xa
            xbl(i) = xb
          endif
        enddo
        do i = its, ite
          tr=ttp/t(i,k)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          qs(i,k,2)=psat*exp(logtr*(xal(i))+xbl(i)*(1.-tr))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo
#else
      do k = kts, kte
        do i = its, ite
          tr=ttp/t(i,k)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(logtr*(xai)+xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(logtr*(xa)+xb*(1.-tr))
          endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo
#endif
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
        do i = its, ite
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pracw(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          psevp(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
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
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
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
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k)                                  &
!                   *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
        enddo
      enddo
!
      mstepmax = 1
      numdt = 1
      do k = kte, kts, -1
        do i = its, ite
          work1(i,k,1) = pvtr*rslopeb(i,k,1)*denfac(i,k)/delz(i,k)
          work1(i,k,2) = pvts*rslopeb(i,k,2)*denfac(i,k)/delz(i,k)
          numdt(i) = max(nint(max(work1(i,k,1),work1(i,k,2))*dtcld+.5),1)
          if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
        enddo
      enddo
      do i = its, ite
        if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
        rmstep(i) = 1./mstep(i)
      enddo
!
      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mstep(i)) then
!             falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
!             falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
!             qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k),0.)
!             qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k),0.)
              dtcldden = dtcld/den(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcldden,0.)
            endif
          enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
              dtcldden = dtcld/den(i,k)
              rdelz = 1./delz(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)           &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)           &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
            endif
          enddo
        enddo
        do k = kte, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              if(t(i,k).gt.t0c.and.qrs(i,k,2).gt.0.) then
!----------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!----------------------------------------------------------------
                xlf = xlf0
!               work2(i,k)= venfac(p(i,k),t(i,k),den(i,k))
                work2(i,k)= (exp(log(((1.496e-6*((t(i,k))*sqrt(t(i,k)))        &
                            /((t(i,k))+120.)/(den(i,k)))/(8.794e-5             &
                            *exp(log(t(i,k))*(1.81))/p(i,k))))                 &
                            *((.3333333)))/sqrt((1.496e-6*((t(i,k))            &
                            *sqrt(t(i,k)))/((t(i,k))+120.)/(den(i,k))))        &
                            *sqrt(sqrt(den0/(den(i,k)))))
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
!               psmlt(i,k) = xka(t(i,k),den(i,k))/xlf*(t0c-t(i,k))*pi/2.       &
!                           *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
!                           *work2(i,k)*coeres)
                psmlt(i,k) = (1.414e3*(1.496e-6*((t(i,k))*sqrt(t(i,k)))        &         
                            /((t(i,k))+120.)/(den(i,k)) )*(den(i,k)))          &
                            /xlf*(t0c-t(i,k))*pi/2.                            &
                            *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2         &
                            *work2(i,k)*coeres)
                psmlt(i,k) = min(max(psmlt(i,k)*dtcld/mstep(i),                &
                            -qrs(i,k,2)/mstep(i)),0.)
                qrs(i,k,2) = qrs(i,k,2) + psmlt(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psmlt(i,k)
                t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)
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
            work1c(i,k) = 1.49e4*exp(log(diameter)*(1.31))
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
              qci(i,k,2) = max(qci(i,k,2)-(falkc(i,k)-falkc(i,k+1)             &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
            endif
          enddo
        enddo
      enddo
!
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,1,1)+fall(i,1,2)+fallc(i,1)
        fallsum_qsi = fall(i,1,2)+fallc(i,1)
        rainncv(i) = 0.
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000.
          rain(i) = fallsum*delz(i,1)/denr*dtcld*1000. + rain(i)
        endif
        IF ( PRESENT (snowncv) .AND. PRESENT (snow)) THEN
        snowncv(i,lat) = 0.
        if(fallsum_qsi.gt.0.) then
          snowncv(i,lat) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000.
          snow(i,lat) = fallsum_qsi*delz(i,kts)/denr*dtcld*1000. + snow(i,lat)
        endif
        ENDIF
        sr(i) = 0.
        if(fallsum.gt.0.)sr(i)=fallsum_qsi*delz(i,kts)/denr*dtcld*1000.        &    
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
          if(supcol.lt.0.and.qci(i,k,2).gt.0.) then
            qci(i,k,1) = qci(i,k,1) + qci(i,k,2)
            t(i,k) = t(i,k) - xlf/cpm(i,k)*qci(i,k,2)
            qci(i,k,2) = 0.
          endif
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.40..and.qci(i,k,1).gt.0.) then
            qci(i,k,2) = qci(i,k,2) + qci(i,k,1)
            t(i,k) = t(i,k) + xlf/cpm(i,k)*qci(i,k,1)
            qci(i,k,1) = 0.
          endif
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qci(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                         &
!              *den(i,k)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcolt)-1.)                        &
            *den(i,k)/denr/xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! psfrz: freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qrs(i,k,1).gt.0.) then
            supcolt=min(supcol,50.)
!           pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k)                    &
!                 *(exp(pfrz2*supcol)-1.)*rslope(i,k,1)**7*dtcld,              &
!                 qrs(i,k,1))
            temp = rslope(i,k,1)
            temp = temp*temp*temp*temp*temp*temp*temp
            pfrzdtr = min(20.*(pi*pi)*pfrz1*n0r*denr/den(i,k)                  &
                  *(exp(pfrz2*supcolt)-1.)*temp*dtcld,                         &
                  qrs(i,k,1))
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
        enddo
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
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
!           rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k))
            rslope(i,k,1) = 1./(sqrt(sqrt(pidn0r/((qrs(i,k,1))*(den(i,k))))))
            rslopeb(i,k,1) = exp(log(rslope(i,k,1))*(bvtr))
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
!            rslope(i,k,2) = 1./lamdas(qrs(i,k,2),den(i,k),n0sfac(i,k))
            rslope(i,k,2) = 1./(sqrt(sqrt(pidn0s*(n0sfac(i,k))/((qrs(i,k,2))   &  
                          *(den(i,k))))))
            rslopeb(i,k,2) = exp(log(rslope(i,k,2))*(bvts))
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
        enddo
      enddo
!
      do k = kts, kte
        do i = its, ite
!         work1(i,k,1) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k,1))
          work1(i,k,1) = ((((den(i,k))*(xl(i,k))*(xl(i,k)))*((t(i,k))+120.)    &
                       *(den(i,k)))/(1.414e3*(1.496e-6*((t(i,k))*sqrt(t(i,k))))&
                       *(den(i,k))*(rv*(t(i,k))*(t(i,k)))))                    &
                      +  p(i,k)/((qs(i,k,1))*(8.794e-5*exp(log(t(i,k))*(1.81))))
!         work1(i,k,2) = diffac(xls,p(i,k),t(i,k),den(i,k),qs(i,k,2))
          work1(i,k,2) = ((((den(i,k))*(xls)*(xls))*((t(i,k))+120.)*(den(i,k)))&
                       /(1.414e3*(1.496e-6*((t(i,k))*sqrt(t(i,k))))*(den(i,k)) &
                       *(rv*(t(i,k))*(t(i,k))))                                &
                      + p(i,k)/(qs(i,k,2)*(8.794e-5*exp(log(t(i,k))*(1.81)))))
!         work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
          work2(i,k) = (exp(.3333333*log(((1.496e-6 * ((t(i,k))*sqrt(t(i,k)))) &
                     *p(i,k))/(((t(i,k))+120.)*den(i,k)*(8.794e-5              &
                     *exp(log(t(i,k))*(1.81))))))*sqrt(sqrt(den0/(den(i,k))))) &
                      /sqrt((1.496e-6*((t(i,k))*sqrt(t(i,k))))                 &
                      /(((t(i,k))+120.)*den(i,k)))
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
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          if(qci(i,k,1).gt.qc0) then
            praut(i,k) = qck1*exp(log(qci(i,k,1))*((7./3.)))
            praut(i,k) = min(praut(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pracw(i,k) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)               & 
                         *qci(i,k,1)*denfac(i,k),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.0.) then
            coeres = rslope2(i,k,1)*sqrt(rslope(i,k,1)*rslopeb(i,k,1))
            prevp(i,k) = (rh(i,k,1)-1.)*(precr1*rslope2(i,k,1)                 &
                         +precr2*work2(i,k)*coeres)/work1(i,k,1)
            if(prevp(i,k).lt.0.) then
              prevp(i,k) = max(prevp(i,k),-qrs(i,k,1)/dtcld)
              prevp(i,k) = max(prevp(i,k),satdt/2)
            else
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
      rdtcld = 1./dtcld
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
          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qmin) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
              vt2s = pvts*rslopeb(i,k,2)*denfac(i,k)
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by rain [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      +diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                           *abs(vt2s-vt2i)*acrfac/4.
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)                  &
                           *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)              &
!                          ,qci(i,k,1)/dtcld)
                           ,qci(i,k,1)*rdtcld)
          endif
          if(supcol .gt. 0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
!               pidep(i,k) = max(max(pidep(i,k),satdt/2),supice)
!               pidep(i,k) = max(pidep(i,k),-qci(i,k,2)/dtcld)
                pidep(i,k) = max(max(pidep(i,k),satdt*.5),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)*rdtcld)
              else
!               pidep(i,k) = min(min(pidep(i,k),satdt/2),supice)
                pidep(i,k) = min(min(pidep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)                          &
                           *(precs1*rslope2(i,k,2)+precs2                      &
                           *work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
!               psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)/dtcld)
!               psdep(i,k) = max(max(psdep(i,k),satdt/2),supice)
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)*rdtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt*.5),supice)
              else
!             psdep(i,k) = min(min(psdep(i,k),satdt/2),supice)
                psdep(i,k) = min(min(psdep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt))          &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL A50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pigen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))          &
!                        /dtcld)
                         *rdtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
!             psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
            endif
          endif
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.)                           &
              psevp(i,k) = psdep(i,k)*work1(i,k,2)/work1(i,k,1)
!              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)/dtcld),0.)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)*rdtcld),0.)
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
          if(t(i,k).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)+psaci(i,k)-pigen(i,k)-pidep(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
            endif
!
!     rain
!
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
            endif
!
!    snow
!
            value = max(qmin,qrs(i,k,2))
            source = (-psdep(i,k)-psaut(i,k)-psaci(i,k)-psacw(i,k))*dtcld  
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pigen(i,k)+pidep(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+psaci(i,k)                 &
                        -pigen(i,k)-pidep(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)                 &
                        +psaci(i,k)+psacw(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pidep(i,k)+pigen(i,k))                  &
                      -xl(i,k)*prevp(i,k)-xlf*psacw(i,k)
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source=(praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)-psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif  
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psevp(i,k) = psevp(i,k)*factor
            endif
            work2(i,k)=-(prevp(i,k)+psevp(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k) +psacw(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k))
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
          logtr = log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
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
!         work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
          work1(i,k,1) = ((max(q(i,k),qmin)-(qs(i,k,1)))/(1.+(xl(i,k))         &   
                        *(xl(i,k))/(rv*(cpm(i,k)))*(qs(i,k,1))                 &
                        /((t(i,k))*(t(i,k)))))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                          &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k) = q(i,k)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
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
  END SUBROUTINE wsm52d

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
        fpvs=psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
      else
        fpvs=psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END FUNCTION fpvs
!-------------------------------------------------------------------
  SUBROUTINE wsm5init(den0,denr,dens,cl,cpv,allowed_to_read)
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
   pacrc = pi*n0s*avts*g3pbs*.25*eacrc
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
  END SUBROUTINE wsm5init
END MODULE module_mp_wsm5

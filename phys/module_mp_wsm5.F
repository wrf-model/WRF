#define WSM5_OPTIM
#define INL
#define INL_STATFUN
#if (RWORDSIZE == DWORDSIZE)
#  define VREC(A,B,C) vrec(A,B,C)
#  define VLOG(A,B,C) vlog(A,B,C)
#  define VEXP(A,B,C) vexp(A,B,C)
#  define VSQRT(A,B,C) vsqrt(A,B,C)
#  define VRSQRT(A,B,C) vrsqrt(A,B,C)
#else
#  define VREC(A,B,C) vsrec(A,B,C)
#  define VLOG(A,B,C) vslog(A,B,C)
#  define VEXP(A,B,C) vsexp(A,B,C)
#  define VSQRT(A,B,C) vssqrt(A,B,C)
#  define VRSQRT(A,B,C) vsrsqrt(A,B,C)
#endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NEW OPTIMIZATIONS, 20060214, JM : 
! Comment these defines to turn off particular
! optimizations
! AAA and BBB are Section 7 optimizations
! CCC is for Section 13 optimizations
! ALGEBRAIC_11 is for Section 11 optimizations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#define AAA
#define BBB
#define CCC
#define ALGEBRAIC_11
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! uncomment to output timing of code sections
!#define WSM5_TIMERS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE module_mp_wsm5
!
!
   REAL, PARAMETER, PRIVATE :: dtcldcr     = 120.
   REAL, PARAMETER, PRIVATE :: n0r = 8.e6
   REAL, PARAMETER, PRIVATE :: avtr = 841.9
   REAL, PARAMETER, PRIVATE :: bvtr = 0.8
   REAL, PARAMETER, PRIVATE :: r0 = .8e-5 ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER, PRIVATE :: peaut = .55   ! collection efficiency
   REAL, PARAMETER, PRIVATE :: xncr = 3.e8   ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER, PRIVATE :: xmyu = 1.718e-5 ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER, PRIVATE :: avts = 11.72
   REAL, PARAMETER, PRIVATE :: bvts = .41
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
   REAL, SAVE ::                                     &
             qc0, qck1,bvtr1,bvtr2,bvtr3,bvtr4,g1pbr,&
             g3pbr,g4pbr,g5pbro2,pvtr,eacrr,pacrr,   &
             precr1,precr2,xm0,xmmax,roqimax,bvts1,  &
             bvts2,bvts3,bvts4,g1pbs,g3pbs,g4pbs,    &
             g5pbso2,pvts,pacrs,precs1,precs2,pidn0r,&
             pidn0s,xlv1,pacrc,vt2i,vt2s,acrfac,     &
             rslopermax,rslopesmax,rslopegmax,       &
             rsloperbmax,rslopesbmax,rslopegbmax,    &
             rsloper2max,rslopes2max,rslopeg2max,    &
             rsloper3max,rslopes3max,rslopeg3max

integer, parameter :: zones = 16
!
! Specifies code-inlining of fpvs function in WSM52D below. JM 20040507
!

CONTAINS
!===================================================================
!
  SUBROUTINE wsm5(th, q, qc, qr, qi, qs                            &
                 ,w, den, pii, p, delz                             &
                 ,delt,g, cpd, cpv, rd, rv, t0c                    &
                 ,ep1, ep2, qmin                                   &
                 ,XLS, XLV0, XLF0, den0, denr                      &
                 ,cliq,cice,psat                                   &
                 ,rain, rainncv                                    &
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a MIXED phase ice microphyiscs scheme (WSM5) of the WRF
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. Related changes in ice-microphysics and description of
!  other microphysics are described in Hong et al. (2004).
!  all units are m.k.s. and source/sink terms are kgkg-1s-1.
!
! WRFSMMP cloud scheme
!
!  Coded by Song-You Hong, Jeong-Ock Lim (Yonsei Univ.)
!           Jimy Dudhia (NCAR) and Shu-Hua Chen (UC Davis)
!           Summer 2003
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Rutledge, Hobbs (RH, 1983) J. Atmos. Sci.
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
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte ) ::   t
  REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci, qrs
  INTEGER ::               i,j,k

integer, external :: rsl_internal_microclock
integer time
integer timers( zones, jts:jte )
!-------------------------------------------------------------------
#ifdef WSM5_TIMERS
time = rsl_internal_microclock()
DO j=jts,jte
DO i=1,zones
timers(i,j) = 0
ENDDO
ENDDO
#endif

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
         CALL wsm52D(t, q(ims,kms,j), qci, qrs                     &
                    ,w(ims,kms,j), den(ims,kms,j)                  &
                    ,p(ims,kms,j), delz(ims,kms,j)                 &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,XLS, XLV0, XLF0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,j                                             &
                    ,rain(ims,j),rainncv(ims,j)                    &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte, timers(1,j)        &
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

#ifdef WSM5_TIMERS
write(0,*)' total: ',rsl_internal_microclock() - time
DO j=jts,jte
WRITE(0,'(i3,20i5)')j,(timers(i,j),i=1,zones)
ENDDO
#endif

  END SUBROUTINE wsm5
!===================================================================
!
  SUBROUTINE wsm52D(t, q, qci, qrs, w, den, p, delz                &
                   ,delt,g, cpd, cpv, rd, rv, t0c                  &
                   ,ep1, ep2, qmin                                 &
                   ,XLS, XLV0, XLF0, den0, denr                    &
                   ,cliq,cice,psat                                 &
                   ,lat                                            &
                   ,rain,rainncv                                   &
                   ,ids,ide, jds,jde, kds,kde                      &
                   ,ims,ime, jms,jme, kms,kme                      &
                   ,its,ite, jts,jte, kts,kte                      &
,timers &
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
  REAL, DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv
! LOCAL VAR
  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
        rh, qs, rslope, rslope2, rslope3, rslopeb,                &
        paut, pres, falk, fall, work1
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
              falkc, work1c, work2c, fallc
  REAL, DIMENSION( its:ite , kts:kte, 3 ) ::                      &
        pacr
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        pgen, pisd, pcon, xl, cpm, work2, psml, psev, denfac, xni,&
        n0sfac
  REAL, DIMENSION( its:ite )           :: tvec1,tvec2,tvec3,tvec4
  INTEGER, DIMENSION( its:ite ) :: mstep, numdt

  REAL, DIMENSION(its:ite) :: rmstep
  REAL dtcldden, rdelz, rdtcld

  LOGICAL, DIMENSION( its:ite ) :: flgcld
  REAL  ::  pi,                                                   &
            cpmcal, xlcal, lamdar, lamdas, diffus,                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, supcol, pvt,                     &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            qimax, diameter, xni0, roqi0,                         &
            fallsum, xlwork2, factor, source, value,              &
            xlf, pfrzdtc, pfrzdtr, supice, temp
  REAL  :: holdc, holdci
  INTEGER :: i, j, k, mstepmax,                                   &
            iprt, latd, lond, loop, loops, ifsat, n

integer timer, timers(zones)
integer, external :: rsl_internal_microclock
real qrs1_points, qrs2_points

#ifdef INL
! Temporaries used for inlining fpvs function
  REAL  :: dldti, xb, xai, tr, xbi, xa, hvap, cvap, hsub, dldt, ttp
#endif

#ifdef WSM5_TIMERS
# define STARTTIME  timer = rsl_internal_microclock()
# define ENDTIME(Z) timers(Z) = timers(Z) + rsl_internal_microclock() - timer 
#else
# define STARTTIME
# define ENDTIME(Z)
#endif
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
#ifdef WSM5_OPTIM
#define PWR(A,B)  exp(log(A)*(B))
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!
# ifdef INL_STATFUN
#   define DIFFUS(x,y) (8.794e-5 * PWR(x,1.81) / y        )
#   define VISCOS(x,y) (1.496e-6 * ((x)*sqrt(x)) /((x)+120.)/(y)  )
#   define XKA(x,y) (1.414e3*VISCOS(x,y)*(y))
#   define DIFFAC(a,b,c,d,e) ((d)*(a)*(a)/(XKA(c,d)*rv*(c)*(c))+1./((e)*DIFFUS(c,b)))
#   define VENFAC(a,b,c) (PWR((VISCOS(b,c)/DIFFUS(b,a)),(.3333333))/sqrt(VISCOS(b,c))*sqrt(sqrt(den0/(c))))
#   define CONDEN(a,b,c,d,e)  ((max(b,qmin)-(c))/(1.+(d)*(d)/(rv*(e))*(c)/((a)*(a))))
# else
#   define DIFFUS(x,y)       diffus(x,y)
#   define VISCOS(x,y)       viscos(x,y)
#   define XKA(x,y)          xka(x,y)
#   define DIFFAC(a,b,c,d,e) diffac(a,b,c,d,e)
#   define VENFAC(a,b,c)     venfac(a,b,c)
#   define CONDEN(a,b,c,d,e) conden(a,b,c,d,e)
      diffus(x,y)= 8.794e-5 * PWR(x,1.81) / y        ! 8.794e-5*x**1.81/y
      viscos(x,y)= 1.496e-6 * ( x *sqrt(x)) /( x +120.)/ y  ! 1.496e-6*x**1.5/(x+120.)/y
      xka(x,y)= 1.414e3*viscos(x,y)* y
      diffac(a,b,c,d,e) = d * a * a /(xka(c,d)*rv* c * c )+1./( e *diffus(c,b))
      venfac(a,b,c)= PWR((viscos(b,c)/diffus(b,a)),(.3333333))/sqrt(viscos(b,c))*sqrt(sqrt(den0/ c ))
      conden(a,b,c,d,e) = (max(b,qmin)- c )/(1.+ d * d /(rv* e )* c /( a * a ))
# endif
#else
#define PWR(A,B)  ((A)**(B))
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
#endif
!
      pi = 4. * atan(1.)
!
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!
STARTTIME
      do k = kts, kte
        do i = its, ite
          qci(i,k,1) = max(qci(i,k,1),0.0)
          qrs(i,k,1) = max(qrs(i,k,1),0.0)
          qci(i,k,2) = max(qci(i,k,2),0.0)
          qrs(i,k,2) = max(qrs(i,k,2),0.0)
!write(0,'(3i4," ",1e22.12)')i,k,lat,-qrs(i,k,2)
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
ENDTIME(1)
!
      do loop = 1,loops
STARTTIME
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
      do i = its, ite
        mstep(i) = 1
        flgcld(i) = .true.
      enddo
!
#ifdef WSM5_OPTIM
      do k = kts, kte
        CALL VREC( tvec1(its), den(its,k), ite-its+1 )
        do i = its, ite
          tvec1(i) = tvec1(i)*den0
        enddo
        CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1 )
      enddo
#else
      do k = kts, kte
        do i = its, ite
          denfac(i,k) = sqrt(den0/den(i,k))
        enddo
      enddo
#endif
ENDTIME(2)
STARTTIME
!
#ifdef INL
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
#endif
      do k = kts, kte
        do i = its, ite
#ifdef INL
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*PWR(tr,xa)*exp(xb*(1.-tr))
#else
          qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#endif
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
#ifdef INL
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*PWR(tr,xai)*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*PWR(tr,xa)*exp(xb*(1.-tr))
          endif
#else
          qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo
ENDTIME(3)
STARTTIME
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
          xni(i,k) = 1.e3
        enddo
      enddo
ENDTIME(4)
STARTTIME
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
            rslopeb(i,k,1) = PWR(rslope(i,k,1),bvtr)
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
            rslopeb(i,k,2) = PWR(rslope(i,k,2),bvts)
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
#ifdef WSM5_OPTIM
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
#else
          xni(i,k) = min(max(5.38e7*(den(i,k)                           &
                    *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
#endif
        enddo
      enddo
ENDTIME(5)
STARTTIME
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
#ifdef AAA
        rmstep(i) = 1./mstep(i)
#endif
      enddo
ENDTIME(6)
STARTTIME
!
      do n = 1, mstepmax
        k = kte
        do i = its, ite
          if(n.le.mstep(i)) then
#ifdef AAA
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
#else
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
#endif
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
#ifdef BBB
              dtcldden = dtcld/den(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcldden,0.)
#else
              qrs(i,k,1) = max(qrs(i,k,1)-falk(i,k,1)*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-falk(i,k,2)*dtcld/den(i,k),0.)
#endif
!write(0,'(3i4," ",4e22.12)')i,k,lat,falk(i,k,1),falk(i,k,2),qrs(i,k,1),qrs(i,k,2)
          endif
        enddo
        do k = kte-1, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
#ifdef AAA
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)*rmstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)*rmstep(i)
#else
              falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
              falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
#endif
              fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
              fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
#ifdef BBB
              dtcldden = dtcld/den(i,k)
              rdelz = 1./delz(i,k)
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)    &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)    &
                          *delz(i,k+1)*rdelz)*dtcldden,0.)
#else
              qrs(i,k,1) = max(qrs(i,k,1)-(falk(i,k,1)-falk(i,k+1,1)    &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
              qrs(i,k,2) = max(qrs(i,k,2)-(falk(i,k,2)-falk(i,k+1,2)    &
                          *delz(i,k+1)/delz(i,k))*dtcld/den(i,k),0.)
#endif
!write(0,'(3i4," ",4e22.12)')i,k,lat,falk(i,k,1),falk(i,k,2),qrs(i,k,1),qrs(i,k,2)
            endif
          enddo
        enddo
        do k = kte, kts, -1
          do i = its, ite
            if(n.le.mstep(i)) then
              if(t(i,k).gt.t0c.and.qrs(i,k,2).gt.0.) then
!----------------------------------------------------------------
! psml: melting of snow [RH83 A25]
!       (T>T0: S->R)
!----------------------------------------------------------------
                xlf = xlf0
#ifdef INL_STATFUN
                work2(i,k)= (exp(log(((1.496e-6*((t(i,k))*sqrt(t(i,k))) &
                            /((t(i,k))+120.)/(den(i,k)))/(8.794e-5      &
                            *exp(log(t(i,k))*(1.81))/p(i,k))))          &
                            *((.3333333)))/sqrt((1.496e-6*((t(i,k))     &
                            *sqrt(t(i,k)))/((t(i,k))+120.)/(den(i,k)))) &
                            *sqrt(sqrt(den0/(den(i,k)))))
#else
                work2(i,k) = VENFAC(p(i,k),t(i,k),den(i,k))
#endif
                coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
                psml(i,k) = XKA(t(i,k),den(i,k))                        &
                            /xlf*(t0c-t(i,k))*pi/2.                     &
                            *n0sfac(i,k)*(precs1*rslope2(i,k,2)+precs2  &
                            *work2(i,k)*coeres)
                psml(i,k) = min(max(psml(i,k)*dtcld/mstep(i),           &
                            -qrs(i,k,2)/mstep(i)),0.)
                qrs(i,k,2) = qrs(i,k,2) + psml(i,k)
                qrs(i,k,1) = qrs(i,k,1) - psml(i,k)
                t(i,k) = t(i,k) + xlf/cpm(i,k)*psml(i,k)
              endif
            endif
          enddo
        enddo
      enddo
ENDTIME(7)
STARTTIME
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
#ifdef WSM5_OPTIM
            diameter  = max(min(dicon * sqrt(xmi),dimax), 1.e-25)
#else
            diameter  = min(dicon * sqrt(xmi),dimax)
#endif
            work1c(i,k) = 1.49e4*PWR(diameter,1.31)
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
ENDTIME(8)
STARTTIME
!
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
      do i = its, ite
        fallsum = fall(i,1,1)+fall(i,1,2)
        if(fallsum.gt.0.) then
          rainncv(i) = fallsum*delz(i,1)/denr*dtcld*1000.
          rain(i) = fallsum*delz(i,1)/denr*dtcld*1000. + rain(i)
        endif
      enddo
ENDTIME(9)
STARTTIME
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
          if(supcol.gt.0..and.qci(i,k,1).gt.0.) then
#ifdef WSM5_OPTIM
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                  &
            *den(i,k)/denr/xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
#else
            pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)                  &
               *den(i,k)/denr/xncr*qci(i,k,1)**2*dtcld,qci(i,k,1))
#endif
            qci(i,k,2) = qci(i,k,2) + pfrzdtc
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtc
            qci(i,k,1) = qci(i,k,1)-pfrzdtc
          endif
!---------------------------------------------------------------
! pfrz: freezing of rain water [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          if(supcol.gt.0..and.qrs(i,k,1).gt.0.) then
#ifdef WSM5_OPTIM
            temp = rslope(i,k,1)
            temp = temp*temp*temp*temp*temp*temp*temp
            pfrzdtr = min(20.*(pi*pi)*pfrz1*n0r*denr/den(i,k)             &
                  *(exp(pfrz2*supcol)-1.)*temp*dtcld,                   &
                  qrs(i,k,1))
#else
            pfrzdtr = min(20.*pi**2*pfrz1*n0r*denr/den(i,k)             &
                  *(exp(pfrz2*supcol)-1.)*rslope(i,k,1)**7*dtcld,       &
                  qrs(i,k,1))
#endif
            qrs(i,k,2) = qrs(i,k,2) + pfrzdtr
            t(i,k) = t(i,k) + xlf/cpm(i,k)*pfrzdtr
            qrs(i,k,1) = qrs(i,k,1)-pfrzdtr
          endif
        enddo
      enddo
ENDTIME(10)
STARTTIME
!
!----------------------------------------------------------------
!     rsloper: reverse of the slope parameter of the rain(m)
!     xka:    thermal conductivity of air(jm-1s-1k-1)
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!

#if 1
#define LAMDAR(X,Y)  (sqrt(sqrt(pidn0r/((X)*(Y)))))
#define LAMDAS(X,Y,Z)  (sqrt(sqrt(pidn0s*(Z)/((X)*(Y)))))
!qrs1_points = 0
!qrs2_points = 0
      do k = kts, kte
        do i = its, ite
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
!qrs1_points = qrs1_points+1
            rslope(i,k,1) = 1./LAMDAR(qrs(i,k,1),den(i,k))
            rslopeb(i,k,1) = PWR(rslope(i,k,1),bvtr)
            rslope2(i,k,1) = rslope(i,k,1)*rslope(i,k,1)
            rslope3(i,k,1) = rslope2(i,k,1)*rslope(i,k,1)
          endif
          if(qrs(i,k,2).le.qcrmin)then
            rslope(i,k,2) = rslopesmax
            rslopeb(i,k,2) = rslopesbmax
            rslope2(i,k,2) = rslopes2max
            rslope3(i,k,2) = rslopes3max
          else
!qrs2_points = qrs2_points+1
            rslope(i,k,2) = 1./LAMDAS(qrs(i,k,2),den(i,k),n0sfac(i,k))
            rslopeb(i,k,2) = PWR(rslope(i,k,2),bvts)
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
        enddo
      enddo
!write(0,'("qrs_points ",4f10.4)')qrs1_points,qrs1_points/((kte-kts+1)*(ite-its+1)),qrs2_points,qrs2_points/((kte-kts+1)*(ite-its+1))
#else
      do k = kts, kte

        do i = its, ite
          tvec1(i) = qrs(i,k,1)*den(i,k)
        enddo
        CALL VREC( tvec2(its),tvec1(its),ite-its+1)
        do i = its, ite
          tvec2(i) = tvec1(i)*pidn0r
        enddo
        CALL VSQRT( tvec2(its),tvec1(its),ite-its+1)
        CALL VRSQRT( tvec1(its),tvec2(its),ite-its+1)
        CALL VLOG( tvec3(its),tvec1(its), ite-its+1 )
        do i = its, ite
          tvec3(i) = tvec3(i)*bvtr
        enddo
        CALL VEXP( tvec4(its),tvec3(its), ite-its+1 )

        do i = its, ite
          if(qrs(i,k,1).le.qcrmin)then
            rslope(i,k,1) = rslopermax
            rslopeb(i,k,1) = rsloperbmax
            rslope2(i,k,1) = rsloper2max
            rslope3(i,k,1) = rsloper3max
          else
!            rslope(i,k,1) = 1./lamdar(qrs(i,k,1),den(i,k))
            rslope(i,k,1) = tvec1(i)
!            rslopeb(i,k,1) = PWR(rslope(i,k,1),bvtr)
            rslopeb(i,k,1) = tvec4(i)
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
            rslopeb(i,k,2) = PWR(rslope(i,k,2),bvts)
            rslope2(i,k,2) = rslope(i,k,2)*rslope(i,k,2)
            rslope3(i,k,2) = rslope2(i,k,2)*rslope(i,k,2)
          endif
        enddo
      enddo
#endif
!

#ifndef ALGEBRAIC_11
#if 1
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = DIFFAC(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k,1))
          work1(i,k,2) = DIFFAC(xls,p(i,k),t(i,k),den(i,k),qs(i,k,2))
          work2(i,k) = VENFAC(p(i,k),t(i,k),den(i,k))
!write(0,'(3i4," ",3e22.12)')i,k,lat,work1(i,k,1),work1(i,k,2),work2(i,k)
        enddo
      enddo
#  else
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k,1))
          work1(i,k,2) = diffac(xls,p(i,k),t(i,k),den(i,k),qs(i,k,2))
          work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
!write(0,'(3i4," ",3e22.12)')i,k,lat,work1(i,k,1),work1(i,k,2),work2(i,k)
        enddo
      enddo
#  endif
#else
      do k = kts, kte
        do i = its, ite

          work1(i,k,1) =                                                     &
        ((((den(i,k))*(xl(i,k))*(xl(i,k))) * ((t(i,k))+120.) * (den(i,k)))   &
           /                                                                 &
         ( 1.414e3 * (1.496e-6 * ((t(i,k))*sqrt(t(i,k)))) * (den(i,k)) *     &
                                                   (rv*(t(i,k))*(t(i,k)))))  &
        +                                                                    &
        p(i,k) / ( (qs(i,k,1)) * ( 8.794e-5 * exp(log(t(i,k))*(1.81)) ) )

          work1(i,k,2) =                                                     &
        (                                                                    &
         (((den(i,k))*(xls)*(xls))*((t(i,k))+120.)*(den(i,k)))               &
           /                                                                 &
          (                                                                  &
         1.414e3 * (1.496e-6 * ((t(i,k))*sqrt(t(i,k)))) * (den(i,k)) *       &
                                                   (rv*(t(i,k))*(t(i,k)))    &
          )                                                                  &
          +                                                                  &
         p(i,k)                                                              &
          /                                                                  &
         ( qs(i,k,2) * (8.794e-5 * exp(log(t(i,k))*(1.81))))                 &
        )

          work2(i,k) =                                                       &
        (                                                                    &
         exp(.3333333*log(                                                   &
             ((1.496e-6 * ((t(i,k))*sqrt(t(i,k))))*p(i,k))                   &
                /                                                            &
             (((t(i,k))+120.)*den(i,k)*(8.794e-5 * exp(log(t(i,k))*(1.81)))) &
           ))                                                                &
           *                                                                 &
           sqrt(sqrt(den0/(den(i,k))))                                       &
        )                                                                    &
        /                                                                    &
        sqrt(                                                                &
           (1.496e-6 * ((t(i,k))*sqrt(t(i,k))))                              &
             /                                                               &
           (                                                                 &
            ((t(i,k))+120.) * den(i,k)                                       &
           )                                                                 &
        )

!write(0,'(3i4," ",3e22.12)')i,k,lat,work1(i,k,1),work1(i,k,2),work2(i,k)
        ENDDO
      ENDDO
#endif

ENDTIME(11)
STARTTIME
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
            paut(i,k,1) = qck1*PWR(qci(i,k,1),(7./3.))
            paut(i,k,1) = min(paut(i,k,1),qci(i,k,1)/dtcld)
          endif
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [LFO 51]
!        (C->R)
!---------------------------------------------------------------
          if(qrs(i,k,1).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            pacr(i,k,1) = min(pacrr*rslope3(i,k,1)*rslopeb(i,k,1)       &
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
ENDTIME(12)
STARTTIME
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
#ifdef WSM5_OPTIM
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
#else
          xni(i,k) = min(max(5.38e7*(den(i,k)                           &
                       *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
#endif
          eacrs = exp(0.05*(-supcol))
!
          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qmin) then
              pacr(i,k,2) = min(pacrs*n0sfac(i,k)*eacrs*rslope3(i,k,2)  &
                           *rslopeb(i,k,2)*qci(i,k,2)*denfac(i,k)       &
#ifdef CCC
                           ,qci(i,k,2)*rdtcld)
#else
                           ,qci(i,k,2)/dtcld)
#endif
            endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
              pacr(i,k,3) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)        &
                           *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)       &
#ifdef CCC
                           ,qci(i,k,1)*rdtcld)
#else
                           ,qci(i,k,1)/dtcld)
#endif
            endif
!-------------------------------------------------------------
! pisd: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pisd(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-pres(i,k,1)
              if(pisd(i,k).lt.0.) then
#ifdef CCC
                pisd(i,k) = max(max(pisd(i,k),satdt*.5),supice)
                pisd(i,k) = max(pisd(i,k),-qci(i,k,2)*rdtcld)
#else
                pisd(i,k) = max(max(pisd(i,k),satdt/2),supice)
                pisd(i,k) = max(pisd(i,k),-qci(i,k,2)/dtcld)
#endif
              else
#ifdef CCC
                pisd(i,k) = min(min(pisd(i,k),satdt*.5),supice)
#else
                pisd(i,k) = min(min(pisd(i,k),satdt/2),supice)
#endif
              endif
              if(abs(pres(i,k,1)+pisd(i,k)).ge.abs(satdt)) ifsat = 1
            endif
          endif
!-------------------------------------------------------------
! pres2: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
            coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
            pres(i,k,2) = (rh(i,k,2)-1.)*n0sfac(i,k)                    &
                         *(precs1*rslope2(i,k,2)+precs2                 &
                         *work2(i,k)*coeres)/work1(i,k,2)
            supice = satdt-pres(i,k,1)-pisd(i,k)
            if(pres(i,k,2).lt.0.) then
#ifdef CCC
              pres(i,k,2) = max(pres(i,k,2),-qrs(i,k,2)*rdtcld)
              pres(i,k,2) = max(max(pres(i,k,2),satdt*.5),supice)
#else
              pres(i,k,2) = max(pres(i,k,2),-qrs(i,k,2)/dtcld)
              pres(i,k,2) = max(max(pres(i,k,2),satdt/2),supice)
#endif
            else
#ifdef CCC
              pres(i,k,2) = min(min(pres(i,k,2),satdt*.5),supice)
#else
              pres(i,k,2) = min(min(pres(i,k,2),satdt/2),supice)
#endif
            endif
            if(abs(pres(i,k,1)+pisd(i,k)+pres(i,k,2)).ge.abs(satdt))    &
              ifsat = 1
          endif
!-------------------------------------------------------------
! pgen: generation(nucleation) of ice from vapor [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
          if(supcol.gt.0) then
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-pres(i,k,1)-pisd(i,k)-pres(i,k,2)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*PWR(xni0,1.33)
              pgen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))    &
#ifdef CCC
                         *rdtcld)
#else
                         /dtcld)
#endif
              pgen(i,k) = min(min(pgen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! paut2: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
#ifdef CCC
              paut(i,k,2) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
#else
              paut(i,k,2) = max(0.,(qci(i,k,2)-qimax)/dtcld)
#endif
            endif
          endif
!-------------------------------------------------------------
! psev: Evaporation of melting snow [RH83 A27]
!       (T>T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.)                    &
              psev(i,k) = pres(i,k,2)*work1(i,k,2)/work1(i,k,1)
#ifdef CCC
              psev(i,k) = min(max(psev(i,k),-qrs(i,k,2)*rdtcld),0.)
#else
              psev(i,k) = min(max(psev(i,k),-qrs(i,k,2)/dtcld),0.)
#endif
          endif
!write(0,'(4i4," ",7e22.12)')loop,i,k,lat,pgen(i,k),paut(i,k,2),psev(i,k),pres(i,k,2),-qrs(i,k,2),-qrs(i,k,2)*rdtcld,-qrs(i,k,2)/dtcld
        enddo
      enddo
ENDTIME(13)
STARTTIME
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
            value = max(qmin,qci(i,k,2))
            source = (paut(i,k,2)+pacr(i,k,2)-pgen(i,k)-pisd(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              paut(i,k,2) = paut(i,k,2)*factor
              pacr(i,k,2) = pacr(i,k,2)*factor
              pgen(i,k) = pgen(i,k)*factor
              pisd(i,k) = pisd(i,k)*factor
            endif
!
            work2(i,k)=-(pres(i,k,1)+pres(i,k,2)+pgen(i,k)+pisd(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pacr(i,k,1)         &
                        +pacr(i,k,3))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pacr(i,k,1)         &
                        +pres(i,k,1))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(paut(i,k,2)+pacr(i,k,2)         &
                        -pgen(i,k)-pisd(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(pres(i,k,2)+paut(i,k,2)         &
                        +pacr(i,k,2)+pacr(i,k,3))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(pres(i,k,2)+pisd(i,k)+pgen(i,k))             &
                      -xl(i,k)*pres(i,k,1)-xlf*pacr(i,k,3)
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
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
            source=(-psev(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psev(i,k) = psev(i,k)*factor
            endif
            work2(i,k)=-(pres(i,k,1)+psev(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(paut(i,k,1)+pacr(i,k,1)        &
                        +pacr(i,k,3))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(paut(i,k,1)+pacr(i,k,1)        &
                        +pres(i,k,1) +pacr(i,k,3))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psev(i,k)*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(pres(i,k,1)+psev(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo
ENDTIME(14)
STARTTIME
!
#ifdef INL
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
#endif
      do k = kts, kte
        do i = its, ite
#ifdef INL
          tr=ttp/t(i,k)
          qs(i,k,1)=psat*PWR(tr,xa)*exp(xb*(1.-tr))
#else
          qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#endif
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
#ifdef INL
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*PWR(tr,xai)*exp(xbi*(1.-tr))
          else
            qs(i,k,2)=psat*PWR(tr,xa)*exp(xb*(1.-tr))
          endif
#else
          qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
#endif
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
        enddo
      enddo
ENDTIME(15)
STARTTIME
!
!----------------------------------------------------------------
!  pcon: condensational/evaporational rate of cloud water [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
        do i = its, ite
          work1(i,k,1) = CONDEN(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))
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
ENDTIME(16)
      enddo                  ! big loops
  END SUBROUTINE wsm52d
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
      REAL t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti,   &
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
        fpvs=psat*PWR(tr,xai)*exp(xbi*(1.-tr))
      else
        fpvs=psat*PWR(tr,xa)*exp(xb*(1.-tr))
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
   pi = 4.*atan(1.)
   xlv1 = cl-cpv
   qc0  = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1 = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
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
!  rslopegmax = 1./lamdagmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
!  rslopegbmax = rslopegmax ** bvtg
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
!  rslopeg2max = rslopegmax * rslopegmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
!  rslopeg3max = rslopeg2max * rslopegmax
!
  END SUBROUTINE wsm5init
END MODULE module_mp_wsm5

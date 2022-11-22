MODULE module_mp_wsm6r
implicit none
!
! parameters
   REAL, PARAMETER :: dtcldcr   = 120.
   REAL, PARAMETER :: n0r       = 8.e6
   REAL, PARAMETER :: n0g       = 4.e6
   REAL, PARAMETER :: avtr      = 841.9
   REAL, PARAMETER :: bvtr      = 0.8
   REAL, PARAMETER :: r0        = .8e-5     ! 8 microm  in contrast to 10 micro m
   REAL, PARAMETER :: peaut     = .55       ! collection efficiency
   REAL, PARAMETER :: xncr      = 3.e8      ! maritime cloud in contrast to 3.e8 in tc80
   REAL, PARAMETER :: xmyu      = 1.718e-5  ! the dynamic viscosity kgm-1s-1
   REAL, PARAMETER :: avts      = 11.72
   REAL, PARAMETER :: bvts      = .41
   REAL, PARAMETER :: avtg      = 330.
   REAL, PARAMETER :: bvtg      = 0.8
   REAL, PARAMETER :: deng      = 500.
   REAL, PARAMETER :: n0smax    = 1.e11     ! t                                                       = -90C unlimited
!   REAL, PARAMETER :: betai     = .6
!   REAL, PARAMETER :: xn0       = 1.e-2
!   REAL, PARAMETER :: dicon     = 11.9
!   REAL, PARAMETER :: di0       = 12.9e-6
   REAL, PARAMETER :: dimax     = 500.e-6
   REAL, PARAMETER :: n0s       = 2.e6      ! temperature dependent n0s
   REAL, PARAMETER :: alpha     = .12       ! .122 exponen factor for n0s
   REAL, PARAMETER :: pfrz1     = 100.
   REAL, PARAMETER :: pfrz2     = 0.66
   REAL, PARAMETER :: t40c      = 233.16
   REAL, PARAMETER :: eacrc     = 1.0       ! Esc
   REAL, PARAMETER :: eacrr     = 1.0       ! Erc
   REAL, PARAMETER :: dens      = 100.0
   REAL, PARAMETER :: qs0       = 6.e-4     ! pgaut
   REAL, PARAMETER :: g         = 9.81      !                                                         = 9.81
   REAL, PARAMETER :: rd        = 287.      ! gas constant for dry air (J/kg/K)                       = 287
   REAL, PARAMETER :: rv        = 461.6     ! gas constant for water vapor (J/kg/K)
   REAL, PARAMETER :: t0c       = 273.15    !                                                         = 273.15
   REAL, PARAMETER :: den0      = 1.28      ! density of 0 degree air      (kg/m^3)
   REAL, PARAMETER :: cpd       = 1004.5    ! heat capacity at constant pressure for dry air (J/kg/K) = 7.*rd/2.
   REAL, PARAMETER :: cpv       = 1846.4    ! heat capacity at constant pressure for vapor  (J/kg/K)  = 4.*r_v
!   REAL, PARAMETER :: ep1       = 0.6083624 !                                                         = rv/rd-1.
   REAL, PARAMETER :: ep2       = 0.6217504 !                                                         = rd/rv
   REAL, PARAMETER :: qcrmin    = 1.e-9
   REAL, PARAMETER :: qmin      = 1.E-15    ! epsilon                                                 = 1.E-15
   REAL, PARAMETER :: xls       = 2.85E6    ! latent heat of sublimation   (J/kg)                     = 2.85E6
   REAL, PARAMETER :: xlv0      = 2.5E6     ! latent heat of vaporization  (J/kg)                     = 3.15E6
   REAL, PARAMETER :: xlf0      = 3.50E5    ! latent heat of melting       (J/kg)                     = 3.50E5
   REAL, PARAMETER :: cliq      = 4190.     !                                                         = 4190.
   REAL, PARAMETER :: cice      = 2106.     !                                                         = 2106
   REAL, PARAMETER :: psat      = 610.78    !                                                         = 610.78
   REAL, PARAMETER :: denr      = 1000.     ! water density                                           = 1000 (kg/m^3)


   REAL, SAVE :: pi , &
                 qc0     , qck1       , &
                 bvtr1   , bvtr2      , bvtr3    , bvtr4   , bvtr6   , &
                 g1pbr   , g3pbr      , g4pbr    , g5pbro2 , g6pbr   , pvtr    , &
                 bvts1   , bvts2      , bvts3    , bvts4   , &
                 g1pbs   , g3pbs      , g4pbs    , g5pbso2 , pvts    , &
                 bvtg1   , bvtg2      , bvtg3    , bvtg4   , &
                 g1pbg   , g3pbg      , g4pbg    , g5pbgo2 , pvtg    , &
                 roqimax , pidn0r     , pidn0s   , pidn0g  , xlv1    , &
                 vt2i    , vt2r       , vt2s     , vt2g    , egs     , egi     , &
                 vt2r_a  , vt2s_a     , vt2g_a   , vt2i_a  , &
                 fallr_a , falls_a    , fallg_a  , falli_a , &
                 pgfrz_a  , diffac_a   , diffac_b , pidep_a  , &
                 pgacs_a , pgacs_b    , pgacs_c  , pgacs_d , &
                 pgacr_a , pgacr_b    , pgacr_c  , pgacr_d , &
                 psacr_a , psacr_b    , psacr_c  , psacr_d , &
                 pracs_a , pracs_b    , pracs_c  , pracs_d , &
                 pgaci_a , pgaci_b    , pgaci_c  , pgaci_d , &
                 psevp_a , psevp_b    , pgevp_a  , pgevp_b , &
                 psmlt_a , psmlt_b    , pgmlt_a  , pgmlt_b , &
                 prevp_a , prevp_b    , psdep_a  , psdep_b , &
                 pgdep_a , pgdep_b    , &
                 praci_a , praci_b    , praci_c  , praci_d , &
                 psaci_a , psaci_b    , psaci_c  , psaci_d , &
                 pracw_a , piacr_a    , psacw_a  , pgacw_a

CONTAINS

!=======================================================================
!
!=======================================================================
   SUBROUTINE wsm6r(th, q, qc, qr, qi, qs, qg                        &
                  ,den, pii, p, delz                                &
                  ,delt                                             &
                  ,rain, rainncv                                    &
                  ,ids,ide, jds,jde, kds,kde                        &
                  ,ims,ime, jms,jme, kms,kme                        &
                  ,its,ite, jts,jte, kts,kte                        &
                                                                   )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
         INTENT(INOUT) ::                                          &
                                                               th, &
                                                               q , &
                                                               qc, &
                                                               qi, &
                                                               qr, &
                                                               qs, &
                                                               qg
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
         INTENT(IN   ) ::                                          &
                                                              den, &
                                                              pii, &
                                                                p, &
                                                             delz
   REAL, INTENT(IN   ) ::                                    delt
                                                             
   REAL, DIMENSION( ims:ime , jms:jme ),                           &
         INTENT(INOUT) ::                                    rain, &
                                                          rainncv
!  LOCAL VAR
   REAL, DIMENSION( its:ite , kts:kte    ) ::  t
   REAL, DIMENSION( its:ite , kts:kte, 2 ) ::  qci
   REAL, DIMENSION( its:ite , kts:kte, 3 ) ::  qrs
   REAL, DIMENSION( ims:ime , kms:kme    ) ::  q2d, den2d, p2d, delz2d
   REAL, DIMENSION( ims:ime              ) ::  r1d, rcv1d 
   REAL    ::   delt1
   INTEGER ::               i,j,k,ierr

   delt1=delt
   DO j=jts,jte
      DO i=its,ite
         r1d  (i) = rain(i,j)
         rcv1d(i) = rainncv(i,j)
         DO k=kts,kte
            t      ( i,k   ) = th   ( i,k,j ) *pii ( i,k,j )
            qci    ( i,k,1 ) = qc   ( i,k,j )
            qci    ( i,k,2 ) = qi   ( i,k,j )
            qrs    ( i,k,1 ) = qr   ( i,k,j )
            qrs    ( i,k,2 ) = qs   ( i,k,j )
            qrs    ( i,k,3 ) = qg   ( i,k,j )
            q2d    ( i,k   ) = q    ( i,k,j )
            den2d  ( i,k   ) = den  ( i,k,j )
            p2d    ( i,k   ) = p    ( i,k,j )
            delz2d ( i,k   ) = delz ( i,k,j )
         ENDDO
      ENDDO
      !  Sending array starting locations of optional variables may cause
      !  troubles, so we explicitly change the call.
      CALL wsm62D(t, q2d, qci, qrs         &
                 ,den2d                    &
                 ,p2d, delz2d              &
                 ,delt1                    &
                 ,r1d,rcv1d                &
                 ,ims,ime, kms,kme         &
                 ,its,ite, kts,kte         )
                                                                
      DO I=its,ite
         rain(i,j)   =r1d(i)  
         rainncv(i,j)=rcv1d(i)
         DO K=kts,kte
            th(i,k,j) = t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            qg(i,k,j) = qrs(i,k,3)
            q (i,k,j) = q2d(i,k) 
         ENDDO
      ENDDO
   ENDDO

   END SUBROUTINE

!=======================================================================
!
!=======================================================================
   SUBROUTINE wsm62D(t, q, qci, qrs, den, p, delz ,delt ,rain,rainncv      &
                   ,ims,ime, kms,kme ,its,ite, kts,kte )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
   integer ::   ims,ime, kms,kme,its,ite, kts,kte

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
         INTENT(IN   ) ::                                     den, &
                                                                p, &
                                                             delz
   REAL, INTENT(IN   ) ::                                    delt

   REAL, DIMENSION( ims:ime ),                                     &
         INTENT(INOUT) ::                                    rain, &
                                                          rainncv
!  LOCAL VAR
   REAL, DIMENSION( its:ite , kts:kte , 3) ::                      &
         rh, qs, rslope, rslope2, rslope3, rslopeb,                &
         falk, fall ,work1
   REAL, DIMENSION( its:ite , kts:kte) ::                          &
         pracw, psacw, pgacw, pgacr, pgacs, psaci,  praci,    &
         piacr, pracs, psacr, pgaci, pseml, pgeml, fallc, &
         praut, psaut, pgaut, prevp, psdep, pgdep
   REAL, DIMENSION( its:ite , kts:kte ) ::                         &
         pigen, pidep, pcond, xl, cpm,  psevp,             &
         xni, pgevp,n0sfac,work2
!   LOGICAL, DIMENSION( its:ite ) :: flgcld
   REAL  ::  dtcld,temp,temp0,supcol,supsat,satdt,eacrs,xmi,diameter,delta2,delta3
   INTEGER :: i,  k,  loop, loops
   real ::hsub, hvap, cvap, ttp, dldt, xa, xb, dldti, &
           xai ,xbi, tr, qs10 , qs11, qs20, qs21
   real :: fq, fqc, fqi, fqr, fqs, fqg, fallsum

!=================================================================
!
   call wsm6rinit
!----------------------------------------------------------------
!  paddint 0 for negative values generated by dynamics
!
   do k = kts, kte
      do i = its, ite
         q  (i,k  )=max(q  (i,k  ),0.)
         qci(i,k,1)=max(qci(i,k,1),0.)
         qrs(i,k,1)=max(qrs(i,k,1),0.)
         qci(i,k,2)=max(qci(i,k,2),0.)
         qrs(i,k,2)=max(qrs(i,k,2),0.)
         qrs(i,k,3)=max(qrs(i,k,3),0.)
      enddo
   enddo
!
!----------------------------------------------------------------
!  compute the minor time steps.
!
   loops = max(nint(delt/dtcldcr),1)
   dtcld = delt/loops
   if(delt.le.dtcldcr) dtcld = delt
!
 
   do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
      call inimp(prevp,psdep,pgdep,praut,psaut,pgaut,pracw,praci,piacr,psaci,                 &
                  psacw,pracs,psacr,pgacw,pgaci,pgacr,pgacs,pigen,    &
                  pidep,pcond,pseml,pgeml,psevp,pgevp,falk,fall,fallc, xni,     &
                  kts, kte,its, ite)
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops

      call fallk(cpm,t,p,q,den,qrs,     &
                 delz,dtcld,falk,fall,                       &
                 kte, kts,its, ite,kme, kms,ims, ime)

      call fallkc(qci,fallc,den,delz,dtcld,           &
                  kte,kts, its,ite, kme,kms, ims,ime)

      call rainsc(fall,fallc,xl,t,q,qci,cpm,den,qrs, &
                  delz,rain,rainncv,dtcld,                  &
                  kte, kts,its, ite,kme, kms,ims, ime)

      call warmr(t,q,qci,qrs,den,p,dtcld,xl,rh,qs,praut,pracw,&
                 prevp,ims,ime,kms,kme,its,ite,kts,kte)
!
! cold rain processes
!
!          
      call accret1(qci,den,qrs,t,q,     &
                   dtcld,praci,piacr,psaci,pgaci,psacw,pgacw,       &
                   ims,ime, kms,kme,its,ite,kts,kte)

      call accret2(qrs,t,q,den,dtcld,  &
                   psacw,pgacw,pracs,psacr,pgacr,     &
                   pgacs,pseml,pgeml,                               &
                   ims,ime, kms,kme, its,ite, kts,kte)     

      call accret3(qrs,qci,rh,t,p,den,dtcld,   &
                   q,qs,psdep,pgdep,pigen,psaut,pgaut,psevp,     &
                   pgevp,pidep,                                       &
                   ims,ime, kms,kme,its,ite,kts,kte)

      call pconadd(t,p,q,qci,qs,xl,cpm,dtcld,                       &
                      kte, kts,its, ite,kme, kms,ims, ime)

   enddo                  ! big loops
!
   END SUBROUTINE wsm62d

!=======================================================================
! 
!=======================================================================
   subroutine calcrh(t,p,q,rh,qs)

   REAL, INTENT(IN) :: t, q, p
   REAL, DIMENSION(3),  INTENT(OUT  ) :: rh, qs

   real :: tr, qs10 , qs11, qs20, qs21
   real, parameter :: hsub = xls
   real, parameter :: hvap = xlv0
   real, parameter :: cvap = cpv
   real, parameter :: ttp  = t0c+0.01
   real, parameter :: dldt = cvap-cliq
   real, parameter :: xa   = -dldt/rv
   real, parameter :: xb   = xa+hvap/(rv*ttp)
   real, parameter :: dldti= cvap-cice
   real, parameter :: xai  = -dldti/rv
   real, parameter :: xbi  = xai+hsub/(rv*ttp)
   tr=ttp/t
   qs10  = psat*exp(log(tr)*(xa))*exp(xb*(1.-tr))
   qs11  = ep2 * qs10 / (p - qs10)
   qs(1) = qs11
   rh(1) = q / max(qs(1),qmin)

   qs20 = psat*exp(log(tr)*(xai))*exp(xbi*(1.-tr))
   qs21 = ep2 * qs20 / (p - qs20)
   qs(2) = qs21
   rh(2) = q / max(qs(2),qmin)

   end subroutine
!
!=======================================================================
!
!=======================================================================
   SUBROUTINE wsm6rinit
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
!.... constants which may not be tunable
   pi          = 4.*atan(1.)
   xlv1        = cliq-cpv
   qc0         = 4./3.*pi*denr*r0**3*xncr/den0  ! 0.419e-3 -- .61e-3
   qck1        = .104*9.8*peaut/(xncr*denr)**(1./3.)/xmyu*den0**(4./3.) ! 7.03
   bvtr1       = 1.+bvtr
   bvtr2       = 2.5+.5*bvtr
   bvtr3       = 3.+bvtr
   bvtr4       = 4.+bvtr
   bvtr6       = 6.+bvtr
   g1pbr       = rgmma(bvtr1)
   g3pbr       = rgmma(bvtr3)
   g4pbr       = rgmma(bvtr4)          ! 17.837825
   g6pbr       = rgmma(bvtr6)
   g5pbro2     = rgmma(bvtr2)          ! 1.8273
   pvtr        = avtr*g4pbr/6.
   roqimax     = 2.08e22*dimax**8
!
   bvts1       = 1.+bvts
   bvts2       = 2.5+.5*bvts
   bvts3       = 3.+bvts
   bvts4       = 4.+bvts
   g1pbs       = rgmma(bvts1)    !.8875
   g3pbs       = rgmma(bvts3)
   g4pbs       = rgmma(bvts4)    ! 12.0786
   g5pbso2     = rgmma(bvts2)
   pvts        = avts*g4pbs/6.
   pidn0r      = pi*denr*n0r
   pidn0s      = pi*dens*n0s

   bvtg1       = 1.+bvtg
   bvtg2       = 2.5+.5*bvtg
   bvtg3       = 3.+bvtg
   bvtg4       = 4.+bvtg
   g1pbg       = rgmma(bvtg1)
   g3pbg       = rgmma(bvtg3)
   g4pbg       = rgmma(bvtg4)
   g5pbgo2     = rgmma(bvtg2)
   pvtg        = avtg*g4pbg/6.
   pidn0g      = pi*deng*n0g

   vt2r_a=pvtr*((pidn0r)**(-bvtr/4.))*sqrt(den0)
   vt2s_a=pvts*((pidn0s)**(-bvts/4.))*sqrt(den0)
   vt2g_a=pvtg*((pidn0g)**(-bvtg/4.))*sqrt(den0)
   vt2i_a=3.3
   fallr_a=vt2r_a
   falls_a=vt2s_a
   fallg_a=vt2g_a
   falli_a=vt2i_a

   prevp_a=1.56*pi*n0r/sqrt(pidn0r)
   prevp_b=130.37*pi*sqrt(avtr)*n0r*((pidn0r)**(-(5.+bvtr)/8.))*sqrt(sqrt(den0))*g5pbro2

   psdep_a=2.6*n0s/sqrt(pidn0s)
   psdep_b=370.08*sqrt(avts)*n0s*((pidn0s)**(-(5.+bvts)/8.))*sqrt(sqrt(den0))*g5pbso2
   psevp_a=psdep_a
   psevp_b=psdep_b

   pgdep_a=1.56*pi*n0g/sqrt(pidn0g)
   pgdep_b=130.37*pi*sqrt(avtg)*n0g*((pidn0g)**(-(5.+bvtg)/8.))*sqrt(sqrt(den0))*g5pbgo2
   pgevp_a=pgdep_a
   pgevp_b=pgdep_b

   psmlt_a=2.75e-3*pi*n0s/sqrt(pidn0s)/xlf0
   psmlt_b=0.391*pi*n0s*sqrt(sqrt(den0))*sqrt(avts)*((pidn0s)**(-(5.+bvts)/8.))*g5pbso2/xlf0

   pgmlt_a= 3.3e-3*pi*n0g/sqrt(pidn0g)/xlf0
   pgmlt_b=0.276*pi*n0g*sqrt(sqrt(den0))*sqrt(avtg)*((pidn0g)**(-(5.+bvtg)/8.))*g5pbgo2/xlf0

   praci_a=pi*n0r/4.
   praci_b=2./((pidn0r)**(3./4.))
   praci_c=3.245e-3/sqrt(pidn0r)
   praci_d=2.633e-6/sqrt(sqrt(pidn0r))

   psaci_a=pi*n0s/4.
   psaci_b=2./((pidn0s)**(3./4.))
   psaci_c=3.245e-3/sqrt(pidn0s)
   psaci_d=2.633e-6/sqrt(sqrt(pidn0s))

   pgaci_a=pi*n0g/4.
   pgaci_b=2./((pidn0g)**(3./4.))
   pgaci_c=3.245e-3/sqrt(pidn0g)
   pgaci_d=2.633e-6/sqrt(sqrt(pidn0g))

   pracs_a=pi*n0r*pidn0s
   pracs_b=5./((pidn0s)**(3./2.))/sqrt(sqrt(pidn0r))
   pracs_c=2./((pidn0s)**(5./4.))/sqrt(pidn0r)
   pracs_d=.5/(pidn0s)/((pidn0r)**(3./4.))

   psacr_a=pi*n0s*pidn0r
   psacr_b=5./((pidn0r)**(3./2.))/sqrt(sqrt(pidn0s))
   psacr_c=2./((pidn0r)**(5./4.))/sqrt(pidn0s)
   psacr_d=.5/(pidn0r)/((pidn0s)**(3./4.))

   pgacr_a=pi*n0g*pidn0r
   pgacr_b=5./((pidn0r)**(3./2.))/sqrt(sqrt(pidn0g))
   pgacr_c=2./((pidn0r)**(5./4.))/sqrt(pidn0g)
   pgacr_d=.5/(pidn0r)/((pidn0g)**(3./4.))

   pgacs_a=pi*n0g*pidn0s
   pgacs_b=5./((pidn0s)**(3./2.))/sqrt(sqrt(pidn0g))
   pgacs_c=2./((pidn0s)**(5./4.))/sqrt(pidn0g)
   pgacs_d=.5/(pidn0s)/((pidn0g)**(3./4.))

   pidep_a=3.4927e5

   diffac_a=4.7274e2
   diffac_b=1.1371e4

   pgfrz_a=20.*pi*pfrz1/((pidn0r)**(3./4.))

   piacr_a=5.38e7*pi*avtr*pidn0r*g6pbr*sqrt(den0)*((pidn0r)**(-(6.+bvtr)/4.))/24.
   pracw_a=.25*pi*avtr*n0r*g3pbr*sqrt(den0)*((pidn0r)**(-(3.+bvtr)/4.))
   psacw_a=.25*pi*avts*n0s*g3pbs*sqrt(den0)*((pidn0s)**(-(3.+bvts)/4.))
   pgacw_a=.25*pi*avtg*n0g*g3pbg*sqrt(den0)*((pidn0g)**(-(3.+bvtg)/4.))
   END SUBROUTINE wsm6rinit

!=======================================================================
!
!=======================================================================
   subroutine inimp(prevp,psdep,pgdep,praut,psaut,pgaut,pracw,praci,piacr,psaci,            &
          psacw,pracs,psacr,pgacw,pgaci,pgacr,pgacs,pigen,pidep,   &
          pcond,pseml,pgeml,psevp,pgevp,falk,fall,fallc,  &
          xni,kts, kte,its, ite)

!     initialize the variables for microphysical physics
   IMPLICIT NONE

   integer :: kts, kte,its, ite, k, i
   REAL, DIMENSION( its:ite , kts:kte , 3) :: falk, fall
   REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        xni, pgevp ,  pigen, pidep, pcond, fallc,             &
        pracw, psacw, pgacw, pgacr, pgacs, psaci, praci,    &
        piacr, pracs, psacr, pgaci, pseml, pgeml , psevp, &
        praut, psaut, pgaut, prevp, psdep, pgdep
           
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
         pgaci(i,k) = 0.
         pgacr(i,k) = 0.
         pgacs(i,k) = 0.
         pigen(i,k) = 0.
         pidep(i,k) = 0.
         pcond(i,k) = 0.
         pseml(i,k) = 0.
         pgeml(i,k) = 0.
         psevp(i,k) = 0.
         pgevp(i,k) = 0.
         falk(i,k,1) = 0.
         falk(i,k,2) = 0.
         falk(i,k,3) = 0.
         fall(i,k,1) = 0.
         fall(i,k,2) = 0.
         fall(i,k,3) = 0.
         fallc(i,k) = 0.
         xni(i,k) = 1.e3
      enddo
   enddo
   end subroutine inimp
  
!=======================================================================
!
!=======================================================================
   subroutine fallk(cpm,t,p,q,den,qrs,delz,dtcld, &
                    falk,fall,kte, kts,its, ite,kme, kms,ims, ime)
   implicit none
   integer :: kte, kts,its, ite,kme, kms,ims, ime 
   REAL, DIMENSION( its:ite , kts:kte , 3) :: qrs , falk, fall, work1
   REAL, DIMENSION( ims:ime , kms:kme )  :: delz, den ,p,q
   REAL, DIMENSION( its:ite , kts:kte )  :: psmlt,pgmlt,t,work2,cpm
   INTEGER, DIMENSION( its:ite ) :: mstep, numdt
   real ::  dtcld,coeres1,coeres2,coeresi,coeresh,xlf,psmlt0,pgmlt0,help_i,help_h,w1
   real ::  tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8
   integer :: mstepmax ,k, i,n,nw,jj
   real :: fqs, fqg, supcol, a, b, c, d

   mstep=1
   mstepmax = 1
   numdt = 1
   do k = kte, kts, -1
      do i = its, ite
         work1(i,k,1) = vt2r_a*(den(i,k)**((bvtr-2.)/4.))*(max(qcrmin,qrs(i,k,1))**(bvtr/4.))/delz(i,k)
         work1(i,k,2) = vt2s_a*(den(i,k)**((bvts-2.)/4.))*(max(qcrmin,qrs(i,k,2))**(bvts/4.))/delz(i,k) &
                       *exp(-bvts*alpha*max(0.,min(90.,(t0c-t(i,k))))/4.)
         work1(i,k,3) = vt2g_a*(den(i,k)**((bvtg-2.)/4.))*(max(qcrmin,qrs(i,k,3))**(bvtg/4.))/delz(i,k)
         if(work1(i,k,1) .ge. work1(i,k,2) .and. work1(i,k,1) .ge. work1(i,k,3)) then
            w1=work1(i,k,1) 
         elseif(work1(i,k,2) .ge. work1(i,k,1) .and. work1(i,k,2) .ge. work1(i,k,3)) then
            w1=work1(i,k,2)
         else
            w1=work1(i,k,3)
         endif
         nw=nint(w1*dtcld+.5)
         if(nw.gt.1) then
            numdt(i) = nw
         else
            numdt(i) = 1
         endif 

         if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
      enddo
   enddo
   do i = its, ite
      if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
   enddo

   do n = 1, mstepmax
      do i = its, ite
         if(n.le.mstep(i)) then
            k=kte
            falk(i,k,1) = den(i,k)*qrs(i,k,1)*work1(i,k,1)/mstep(i)
            falk(i,k,2) = den(i,k)*qrs(i,k,2)*work1(i,k,2)/mstep(i)
            falk(i,k,3) = den(i,k)*qrs(i,k,3)*work1(i,k,3)/mstep(i)

            fall(i,k,1) = fall(i,k,1)+falk(i,k,1)
            fall(i,k,2) = fall(i,k,2)+falk(i,k,2)
            fall(i,k,3) = fall(i,k,3)+falk(i,k,3)

            do jj=1,3
               tmp1=min(falk(i,k,jj)*dtcld/den(i,k),qrs(i,k,jj))
               if(abs(tmp1)<qmin)then
                  tmp1=0.
               endif
               qrs(i,k,jj)=qrs(i,k,jj)-tmp1
            enddo
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
               do jj=1,3
                  tmp2=min((falk(i,k,jj)-falk(i,k+1,jj)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),qrs(i,k,jj))
                   if(abs(tmp2)<qmin)then
                     tmp2=0.
                   endif
                   qrs(i,k,jj) = qrs(i,k,jj) - tmp2

               enddo

            endif
         enddo
      enddo


      do k = kte, kts, -1
         do i = its, ite
            if(n.le.mstep(i)) then !
   !---------------------------------------------------------------
   ! psmlt: melting of snow [RH83 A25]
   !       (T>T0: S->R) psmlt<0: min=-qrs(i,k,2), max=0
   !---------------------------------------------------------------
               !update xl, cpm
               cpm(i,k)=cpmcal(q(i,k))
               xlf = xlf0
               a=exp(alpha*max(0.,min(90.,(t0c-t(i,k))))/2.)
               b=exp(alpha*max(0.,min(90.,(t0c-t(i,k))))*(3-bvts)/8.)
               c=(t(i,k)**1.5      )*(t0c-t(i,k))/ (t(i,k)+120.)
               d=(t(i,k)**(3.88/6.))*(t0c-t(i,k))/((t(i,k)+120.)**(5./6.))
               psmlt0 = psmlt_a*a*c*sqrt(den(i,k)*max(qrs(i,k,2),qcrmin)) &
                      +psmlt_b*b*d*(p(i,k)**(1./3.))*(den(i,k)**((13.+3*bvts)/24.))*(max(qrs(i,k,2),qcrmin)**((5.+bvts)/8.))
               tmp3=psmlt0*dtcld/mstep(i)
               tmp4=-qrs(i,k,2)/mstep(i)
               if(tmp3 .gt.tmp4) then
                  tmp5=tmp3
               else
                  tmp5=tmp4
               endif
               if(tmp5 .lt. 0.) then
                  psmlt(i,k) = tmp5
               else
                  psmlt(i,k) = 0.
               endif
               if(abs(psmlt(i,k))<qmin)then
                  psmlt(i,k)=0.
               endif
               !update qs, qr, t
               qrs(i,k,2) = max(qrs(i,k,2) + psmlt(i,k),0.)
               qrs(i,k,1) = max(qrs(i,k,1) - psmlt(i,k),0.)
               t(i,k) = t(i,k) + xlf/cpm(i,k)*psmlt(i,k)

            endif
         enddo
      enddo
   !---------------------------------------------------------------
   ! pgmlt: melting of graupel [LFO 47]
   !       (T>T0: G->R) pgmlt<0: min=-qrs(i,k,3), max=0
   !---------------------------------------------------------------
      do k = kte, kts, -1
         do i = its, ite
            if(n.le.mstep(i)) then
               !update xl, cpm
               xlf = xlf0
!               cpm(i,k)=cpmcal(q(i,k)) ! not change
               c=(t(i,k)**1.5      )*(t0c-t(i,k))/ (t(i,k)+120.)
               d=(t(i,k)**(3.88/6.))*(t0c-t(i,k))/((t(i,k)+120.)**(5./6.))
               pgmlt0 = pgmlt_a*c*sqrt(den(i,k)*max(qrs(i,k,3),qcrmin)) &
                      +pgmlt_b*d*(p(i,k)**(1./3.))*(den(i,k)**((13.+3*bvtg)/24.))*(max(qrs(i,k,3),qcrmin)**((5.+bvtg)/8.))
               tmp6=pgmlt0*dtcld/mstep(i)
               tmp7=-qrs(i,k,3)/mstep(i)
               if(tmp6 .gt.tmp7) then
                  tmp8=tmp6
               else
                  tmp8=tmp7
               endif
               if(tmp8 .lt. 0.) then
                  pgmlt(i,k) = tmp8
               else
                  pgmlt(i,k) = 0.
               endif
               if(abs(pgmlt(i,k))<qmin)then
                  pgmlt(i,k)=0.
               endif
               !update qg, qr, t
               qrs(i,k,3) = max(qrs(i,k,3) + pgmlt(i,k),0.)
               qrs(i,k,1) = max(qrs(i,k,1) - pgmlt(i,k),0.)
               t(i,k) = t(i,k) + xlf/cpm(i,k)*pgmlt(i,k)
            endif
         enddo
      enddo
   enddo
 
   end subroutine fallk

!=======================================================================
!
!=======================================================================
   subroutine fallkc(qci,fallc,den,delz,dtcld, kte, kts,its, ite,kme, kms,ims, ime)
   implicit none

   integer :: kte, kts,its, ite,kme, kms,ims, ime 
   REAL, DIMENSION( its:ite , kts:kte, 2 ) ::  qci
   REAL, DIMENSION( ims:ime , kms:kme )  ::    delz, den
   REAL, DIMENSION( its:ite , kts:kte )  ::    falkc,work1c,work2c,xni,fallc

   INTEGER, DIMENSION( its:ite ) :: mstep, numdt
   real :: dtcld ,xmi,diameter,temp1,temp2,temp3,temp4,temp5, temp0
   integer :: mstepmax ,k, i, n

   mstepmax = 1
   mstep = 1
   numdt = 1
   do k = kte, kts, -1
      do i = its, ite
         work1c(i,k) = vt2i_a * ((den(i,k)*qci(i,k,2))**(1.31/8.))
         work2c(i,k) = work1c(i,k)/delz(i,k)
         numdt(i) = max(nint(work2c(i,k)*dtcld+.5),1)
         if(numdt(i).ge.mstep(i)) mstep(i) = numdt(i)
      enddo
   enddo
   do i = its, ite
      if(mstepmax.le.mstep(i)) mstepmax = mstep(i)
   enddo

   do n = 1, mstepmax
      k = kte
      do i = its, ite
         if(n.le.mstep(i)) then
            falkc(i,k) = falli_a*((den(i,k)*qci(i,k,2))**(9.31/8.))/delz(i,k)/mstep(i)
            fallc(i,k) = fallc(i,k)+falkc(i,k)

            temp3=min(falkc(i,k)*dtcld/den(i,k),qci(i,k,2))
            if(abs(temp3)<qmin)then
               temp3=0.
            endif
            qci(i,k,2)=qci(i,k,2)-temp3
         endif
      enddo
      do k = kte-1, kts, -1
         do i = its, ite
            if(n.le.mstep(i)) then
               falkc(i,k) = falli_a*((den(i,k)*qci(i,k,2))**(9.31/8.))/delz(i,k)/mstep(i)
               fallc(i,k) = fallc(i,k)+falkc(i,k)

               temp4=min((falkc(i,k)-falkc(i,k+1)*delz(i,k+1)/delz(i,k))*dtcld/den(i,k),qci(i,k,2))
               if(abs(temp4)<qmin)then
                  temp4=0.
               endif
               qci(i,k,2)=qci(i,k,2)-temp4
            endif
         enddo
      enddo
   enddo
   end subroutine fallkc

!=======================================================================
!
!=======================================================================
   subroutine rainsc(fall,fallc,xl,t,q,qci,cpm,den,qrs,delz,rain,rainncv,  &
                     dtcld,kte, kts,its, ite,kme, kms,ims, ime)
   implicit none
   integer :: kte, kts,its, ite,kme, kms,ims, ime 
   REAL, DIMENSION( its:ite , kts:kte , 3) :: qrs , fall
   REAL, DIMENSION( its:ite , kts:kte , 2) :: qci
   REAL, DIMENSION( ims:ime , kms:kme )  :: delz,den ,q
   REAL, DIMENSION( its:ite , kts:kte )  :: xl,t,cpm, fallc
   real, DIMENSION( ims:ime ) :: rain,rainncv
   integer :: k, i
   real :: dtcld ,fallsum,supcol,xlf,temp,temp0,pfrzdtr,pfrzdtc 
   real :: ft0, ft40, fsupcol, fqc, fqi, fqr, qtmp

   do i = its, ite
      fallsum = fall(i,kts,1)+fall(i,kts,2)+fall(i,kts,3)+fallc(i,kts)
      if(fallsum.gt.qmin) then
         !rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
         rainncv(i) = fallsum*delz(i,kts)/denr*dtcld*1000.
         rain   (i) = fallsum*delz(i,kts)/denr*dtcld*1000. + rain(i)
      endif
   enddo

   do k = kts, kte
      do i = its, ite
   !---------------------------------------------------------------
   ! pimlt: instantaneous melting of cloud ice [RH83 A28]
   !       (T>T0: I->C) pimlt=qci(i,k,2) t-
   !---------------------------------------------------------------
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xlf = xls-xl(i,k)
         supcol = t0c-t(i,k)
         if(supcol.lt.0.) xlf = xlf0
         call smoothif(t(i,k)    ,t0c,ft0,'t0')
         qtmp=ft0*max(qci(i,k,2),0.)
         if(abs(qtmp)<qmin)then
            qtmp=0.
         endif
         !update qc, qi, t
         qci(i,k,1) = max(qci(i,k,1) + qtmp,0.)
         qci(i,k,2) = max(qci(i,k,2) - qtmp,0.)
         t  (i,k  ) = t(i,k) - xlf/cpm(i,k)*qtmp
   !---------------------------------------------------------------
   ! pihmf: homogeneous freezing of cloud water below -40c
   !        (T<-40C: C->I) min=0,pihmf=qci(i,k,1) t+
   !---------------------------------------------------------------
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xlf = xls-xl(i,k)
         supcol = t0c-t(i,k)
         if(supcol.lt.0.) xlf = xlf0
         call smoothif(supcol   ,40.,ft40,'t0')
         qtmp=max(ft40*qci(i,k,1),0.)
         if(abs(qtmp)<qmin)then
            qtmp=0.
         endif            !update qc, qi, t
         qci(i,k,2) = max(qci(i,k,2) + qtmp,0.)
         qci(i,k,1) = max(qci(i,k,1) - qtmp,0.)
         t  (i,k  ) = t  (i,k  ) + xlf/cpm(i,k)*qtmp
   !---------------------------------------------------------------
   ! pihtf: heterogeneous freezing of cloud water
   !        (T0>T>-40C: C->I) max=qci(i,k,1),min=0. t+
   !---------------------------------------------------------------
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xlf = xls-xl(i,k)
         supcol = t0c-t(i,k)
         if(supcol.lt.0.) xlf = xlf0
         !t>-40C=t0c-40,t0c-t<40, supcol<40,-supcol>-40
         call smoothif(-supcol   ,-40.,ft40,'t0')
         ! t<0C, exp(pfrz2*supcol)-1.<0, pfrzdtc<0,
         pfrzdtc = min(pfrz1*(exp(pfrz2*supcol)-1.)*den(i,k)/denr &
                  /xncr*qci(i,k,1)*qci(i,k,1)*dtcld,qci(i,k,1))
         qtmp=max(ft40*pfrzdtc,0.)
         if(abs(qtmp)<qmin)then
            qtmp=0.
         endif            !update qc, qi, t
         qci(i,k,2) = max(qci(i,k,2) + qtmp,0.)
         qci(i,k,1) = max(qci(i,k,1) - qtmp,0.)
         t  (i,k  ) = t  (i,k  ) + xlf/cpm(i,k)*qtmp
   !---------------------------------------------------------------
   ! pgfrz: freezing of rain water [LFO 45] 
   !        (T<T0, R->G) max=qrs(i,k,1),min=0. t+
   !---------------------------------------------------------------
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
!         cpm(i,k)=cpmcal(q(i,k))!not change
         xlf = xls-xl(i,k)
         supcol = t0c-t(i,k)
         if(supcol.lt.0.) xlf = xlf0
         if(qrs(i,k,1)>0.)then
            temp=pgfrz_a*(exp(pfrz2*supcol)-1.)*(den(i,k)**(3./4.))*(qrs(i,k,1)**(7./4.))
         else
            temp=0.
         endif
         pfrzdtr = min(temp*dtcld,qrs(i,k,1))
         qtmp=max(pfrzdtr,0.)
         if(abs(qtmp)<qmin)then
            qtmp=0.
         endif
         !update qg, qr, t
         qrs(i,k,3) = max(qrs(i,k,3) + qtmp,0.)
         qrs(i,k,1) = max(qrs(i,k,1) - qtmp,0.)
         t  (i,k  ) = t  (i,k  ) + xlf/cpm(i,k)*qtmp
      enddo
   enddo
   end subroutine rainsc
!=======================================================================
!
!=======================================================================
   subroutine warmr(t, q, qci, qrs, den, p ,dtcld,xl,rh,qs,               &
                   praut,pracw,prevp,  &
                   ims,ime, kms,kme, its,ite, kts,kte)
!-------------------------------------------------------------------
   IMPLICIT NONE
   integer ::  ims,ime, kms,kme,its,ite, kts,kte
!------------------------------------------------------------------
   REAL, DIMENSION( its:ite , kts:kte, 2 ) :: qci
   REAL, DIMENSION( ims:ime , kms:kme ) :: q, den, p
   REAL, DIMENSION( its:ite , kts:kte , 3) ::                      &
         rh, qs, qrs,           &
          work1
   REAL, DIMENSION( its:ite , kts:kte) :: praut, prevp, pracw,xl,denfac,t,cpm
   REAL  ::  coeres,supsat,satdt,dtcld,praut1
   INTEGER :: i, k
   real :: fqv, fqc, fqr, fqc0, fprevp,prevp0,prevp1,temp, a, b, c, d, e
 
   do k = kts, kte
      do i = its, ite
   !---------------------------------------------------------------
   ! praut: auto conversion rate from cloud to rain [HDC 16]
   !        (C->R) praut>0 max=qci(i,k,1)/dtcld, min=0.
   !---------------------------------------------------------------
         call smoothif(qci(i,k,1),qc0,fqc0,'q0') !qc0=5.03e-4
         if(qci(i,k,1)>0.)then ! x**a need x>0
            praut1 = fqc0*qck1*exp(log(qci(i,k,1))*(7./3.)) !(qci(i,k,1)**(7./3.))
         else
            praut1=0.
         endif
         praut(i,k) = min(praut1,qci(i,k,1)/dtcld)
         if(abs(praut(i,k))<qmin/dtcld)then
            praut(i,k)=0.
         endif

         ! update qc,qr
         qci(i,k,1)=max(qci(i,k,1)-praut(i,k)*dtcld,0.)
         qrs(i,k,1)=max(qrs(i,k,1)+praut(i,k)*dtcld,0.)
         praut(i,k)=0.

   !---------------------------------------------------------------
   ! pracw: accretion of cloud water by rain [LFO 51]
   !        (C->R) max=qci(i,k,1)/dtcld, min=0.
   !---------------------------------------------------------------
         !call smoothif(qrs(i,k,1),qcrmin,fqr,'q0')
         !call smoothif(qci(i,k,1),qmin  ,fqc,'q0')
         if(qrs(i,k,1)>0.and.qci(i,k,1)>0.)then
            pracw(i,k) = pracw_a*(den(i,k)**((1.+bvtr)/4.))*(qrs(i,k,1)**((3.+bvtr)/4.))*qci(i,k,1)
         else
            pracw(i,k) = 0.
         endif
         pracw(i,k) =max(min(pracw(i,k),qci(i,k,1)/dtcld),0.)
         if(abs(pracw(i,k))<qmin/dtcld)then
            pracw(i,k)=0.
         endif
         ! update qc,qr
         qci(i,k,1)=max(qci(i,k,1)-pracw(i,k)*dtcld,0.)
         qrs(i,k,1)=max(qrs(i,k,1)+pracw(i,k)*dtcld,0.)
         pracw(i,k)=0.
   !
   !---------------------------------------------------------------
   ! prevp: evaporation/condensation rate of rain [HDC 14] 
   !        (V->R or R->V) rh(i,k,1)>1., prevp>0, V->R, min=0.,                max=satdt ;
   !                       rh(i,k,1)<1., prevp<0, R->V, min=-qrs(i,k,1)/dtcld, max=0.
   !---------------------------------------------------------------
         !update rh
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))

         supsat = q(i,k)-qs(i,k,1)
         satdt = supsat/dtcld

         a=sqrt(den(i,k)*max(qrs(i,k,1),qcrmin))
         b=((t(i,k)+120.)**(1./6.))/(t(i,k)**(5.12/6.))*(p(i,k)**(1./3.))&
            *(den(i,k)**((13.+3.*bvtr)/24.))*(max(qrs(i,k,1),qcrmin)**((5.+bvtr)/8.))
         c=diffac_a*den(i,k)*xl(i,k)*xl(i,k)*(t(i,k)+120.)/rv/(t(i,k)**3.5)
         d=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,1)
         e=(rh(i,k,1)-1.)/(c+d)
         prevp(i,k)=(prevp_a*a+prevp_b*b)*e
         if(prevp(i,k)<0.)then
            prevp(i,k) = min(max(prevp(i,k),-qrs(i,k,1)/dtcld),0.) ! R->V
         else
            prevp(i,k) = max(min(prevp(i,k),satdt),0.)             ! V->R
         endif
         if(abs(prevp(i,k))<qmin/dtcld)then
            prevp(i,k)=0.
         endif
         ! update q,qr,t
         q  (i,k  )=max(q  (i,k  )-prevp(i,k)*dtcld,0.)
         qrs(i,k,1)=max(qrs(i,k,1)+prevp(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+prevp(i,k)*dtcld*xl(i,k)/cpm(i,k)
         prevp(i,k) = 0.
      enddo
   enddo
   end subroutine warmr


!===================================================================
   subroutine accret1(qci,den,qrs,t,q,dtcld,  &
                      praci,piacr,psaci,pgaci,psacw,pgacw,                &
                      ims,ime, kms,kme,its,ite,kts,kte)
   IMPLICIT NONE
   integer ::         ims,ime, kms,kme,its,ite, kts,kte
!-------------------------------------------------------------------
   REAL, DIMENSION( its:ite , kts:kte, 2 ) ::   qci
   REAL, DIMENSION( its:ite , kts:kte, 3 ) ::   qrs
   REAL, DIMENSION( ims:ime , kms:kme ) ::  den,q
   REAL, DIMENSION( its:ite , kts:kte) :: praci,piacr,psaci,pgaci,psacw,pgacw,t,xl,cpm
   REAL  ::  supcol,dtcld,eacrs,egi,praci1,piacr1,psaci1,pgaci1,temp,temp0  
   INTEGER :: i, k
   real :: fsupcol, fqc, fqi, fqr, fqs, fqg, delta3, xlf, a, b, c, d, e

   do k = kts, kte
     do i = its, ite
   !-------------------------------------------------------------
   ! praci: Accretion of cloud ice by rain [LFO 25]
   !        (T<T0: I->S or I->G) praci: min=0,max=qci(i,k,2)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,    0.,fsupcol,'t0')

         vt2r=vt2r_a*(den(i,k)**((bvtr-2.)/4.))*(max(qrs(i,k,1),qcrmin)**(bvtr/4.))
         vt2i=vt2i_a*((den(i,k)*max(qci(i,k,2),qmin))**(1.31/8.))
         b=((den(i,k)*max(qrs(i,k,1),qcrmin))**(3./4.))*max(qci(i,k,2),qmin)
         c=(den(i,k)**(5./8.))*sqrt(max(qrs(i,k,1),qcrmin)) *(max(qci(i,k,2),qmin)**(9./8.))
         d=sqrt(den(i,k))*sqrt(sqrt(max(qrs(i,k,1),qcrmin)))*(max(qci(i,k,2),qmin)**(5./4.))
         praci1=praci_a*abs(vt2r-vt2i)*(praci_b*b+praci_c*c+praci_d*d)

         praci(i,k) = min(praci1,qci(i,k,2)/dtcld)
         praci(i,k)=fsupcol*praci(i,k)
         !update qi, qs, qg
         if(qrs(i,k,1).lt.1.e-4)then
            delta3=1.
         else
            delta3=0.
         endif
         if(abs(praci(i,k))<qmin/dtcld)then
            praci(i,k)=0.
         endif
         qci(i,k,2)=max(qci(i,k,2)-praci(i,k)           *dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+praci(i,k)    *delta3*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+praci(i,k)*(1-delta3)*dtcld,0.)
         praci(i,k)=0. 

   !-------------------------------------------------------------
   ! piacr: Accretion of rain by cloud ice [LFO 26]
   !        (T<T0: R->S or R->G) piacr: min=0,max=qrs(i,k,1)/dtcld
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) !not change
         call smoothif(supcol,    0.,fsupcol,'t0')
         !call smoothif(qci(i,k,2),qmin  ,fqi,'q0')
         !call smoothif(qrs(i,k,1),qcrmin,fqr,'q0')
         !update cpm
         cpm(i,k)=cpmcal(q(i,k))
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0

         if(qci(i,k,2)>0..and.qrs(i,k,1)>0.)then
            !piacr_a=1.75e5
            piacr1=piacr_a*(den(i,k)**((3.+bvtr)/4.))*(qci(i,k,2)**0.75) &
                      *(qrs(i,k,1)**((6.+bvtr)/4.))
         else
            piacr1=0.
         endif

         piacr(i,k) = min(piacr1,qrs(i,k,1)/dtcld)
         piacr(i,k)=fsupcol*piacr(i,k)
       ! update qr,qs,qg,t
         if(qrs(i,k,1).lt.1.e-4)then
            delta3=1.
         else
            delta3=0.
         endif
         if(abs(piacr(i,k))<qmin/dtcld)then
            piacr(i,k)=0.
         endif
         qrs(i,k,1)=max(qrs(i,k,1)-piacr(i,k)           *dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+piacr(i,k)    *delta3*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+piacr(i,k)*(1-delta3)*dtcld,0.)
         t(i,k)=t(i,k)+piacr(i,k)*dtcld*xlf/cpm(i,k)
         piacr(i,k)=0.
   !-------------------------------------------------------------
   ! psaci: Accretion of cloud ice by snow [HDC 10]
   !        (T<T0: I->S) psaci: min=0, max=qci(i,k,2)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,    0.,fsupcol,'t0')
         !call smoothif(qci(i,k,2),qmin  ,fqi,'q0')
         !call smoothif(qrs(i,k,2),qcrmin,fqs,'q0')

         eacrs = min(exp(0.07*(-supcol)),1.)

         vt2s=vt2s_a*(den(i,k)**((bvts-2.)/4.))*(max(qrs(i,k,2),qcrmin)**(bvts/4.))&
               *exp(-alpha*bvts*max(0.,min(90.,t0c-t(i,k)))/4.)
         vt2i=vt2i_a*((den(i,k)*max(qci(i,k,2),qmin))**(1.31/8.))
         a=exp(    alpha*max(0.,min(90.,t0c-t(i,k))))
         b=exp(-3.*alpha*max(0.,min(90.,t0c-t(i,k)))/4.)&
            *((den(i,k)*max(qrs(i,k,2),qcrmin))**(3./4.))*max(qci(i,k,2),qmin)
         c=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))/2.)&
            *(den(i,k)**(5./8.))*sqrt(max(qrs(i,k,2),qcrmin)) *(max(qci(i,k,2),qmin)**(9./8.))
         d=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))/4.)&
            *sqrt(den(i,k))*sqrt(sqrt(max(qrs(i,k,2),qcrmin)))*(max(qci(i,k,2),qmin)**(5./4.))
         psaci1=psaci_a*a*abs(vt2s-vt2i)*(psaci_b*b+psaci_c*c+psaci_d*d)*eacrs

         psaci(i,k) = min(psaci1,qci(i,k,2)/dtcld)
         psaci(i,k)=fsupcol*psaci(i,k)
         if(abs(psaci(i,k))<qmin/dtcld)then
            psaci(i,k)=0.
         endif
         !update qi, qs
         qci(i,k,2)=max(qci(i,k,2)-psaci(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+psaci(i,k)*dtcld,0.)
         psaci(i,k)=0.

   !-------------------------------------------------------------
   ! pgaci: Accretion of cloud ice by graupel [LFO 41]
   !        (T<T0: I->G) pgaci:min=0,max=qci(i,k,2)/dtcld
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) !not change
!         call smoothif(supcol,    0.,fsupcol,'t0')
         !call smoothif(qci(i,k,2),qmin  ,fqi,'q0')
         !call smoothif(qrs(i,k,3),qcrmin,fqg,'q0')
         egi = eacrs !min(exp(0.07*(-supcol)),1.)

         vt2g=vt2g_a*(den(i,k)**((bvtg-2.)/4.))*(max(qrs(i,k,3),qcrmin)**(bvtg/4.))
         vt2i=vt2i_a*((den(i,k)*max(qci(i,k,2),qmin))**(1.31/8.))
         b=((den(i,k)*max(qrs(i,k,3),qcrmin))**(3./4.))*max(qci(i,k,2),qmin)
         c=(den(i,k)**(5./8.))*sqrt(max(qrs(i,k,3),qcrmin) )*(max(qci(i,k,2),qmin)**(9./8.))
         d=sqrt(den(i,k))*sqrt(sqrt(max(qrs(i,k,3),qcrmin)))*(max(qci(i,k,2),qmin)**(5./4.))
         pgaci1=pgaci_a*abs(vt2g-vt2i)*(pgaci_b*b+pgaci_c*c+pgaci_d*d)*egi

         pgaci(i,k) = min(pgaci1,qci(i,k,2)/dtcld)
         pgaci(i,k)=fsupcol*pgaci(i,k)
         if(abs(pgaci(i,k))<qmin/dtcld)then
            pgaci(i,k)=0.
         endif

         ! update qi,qg
         qci(i,k,2)=max(qci(i,k,2)-pgaci(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgaci(i,k)*dtcld,0.)
         pgaci(i,k)=0.

   !-------------------------------------------------------------
   ! psacw: Accretion of cloud water by snow  [LFO 24]
   !        (T<T0: C->G, and T>=T0: C->R) psacw:min=0,max=qci(i,k,1)/dtcld
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) !not change
!         call smoothif(supcol,    0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,2),qcrmin,fqs,'q0')
         !call smoothif(qci(i,k,1),qmin  ,fqc,'q0')
         !update cpm
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0
       
         if(qrs(i,k,2)>0..and.qci(i,k,1)>0.)then
            a=exp((1.-bvts)*alpha*max(0.,min(90.,t0c-t(i,k)))/4.)
            psacw(i,k)=psacw_a*a*(den(i,k)**((1.+bvts)/4.))*(qrs(i,k,2)**((3.+bvts)/4.))*qci(i,k,1)
         else
            psacw(i,k)=0.
         endif
         psacw(i,k) =max(min(psacw(i,k),qci(i,k,1)/dtcld),0.)
         psacw(i,k)=fsupcol*psacw(i,k)
         if(abs(psacw(i,k))<qmin/dtcld)then
            psacw(i,k)=0.
         endif
         ! update qc, qr, qg, t
         qci(i,k,1)=max(qci(i,k,1)-             psacw(i,k)*dtcld,0.)
         qrs(i,k,1)=max(qrs(i,k,1)+(1.-fsupcol)*psacw(i,k)*dtcld,0.) !t>=t0
         qrs(i,k,3)=max(qrs(i,k,3)+fsupcol     *psacw(i,k)*dtcld,0.) !t<t0
         t  (i,k  )=t  (i,k  )+fsupcol*psacw(i,k)*dtcld*xlf/cpm(i,k)
         !t>=t0 pseml
         psacw(i,k)=(1-fsupcol)*psacw(i,k)
   !-------------------------------------------------------------
   ! pgacw: Accretion of cloud water by graupel [LFO 40]
   !        (T<T0: C->G, and T>=T0: C->R) pgacw:min=0.,max=qci(i,k,1)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,    0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,3),qcrmin,fqg,'q0')
         !call smoothif(qci(i,k,1),qmin  ,fqc,'q0')
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0
         if(qrs(i,k,3)>0..and.qci(i,k,1)>0.)then
            pgacw(i,k)=pgacw_a*(den(i,k)**((1.+bvtg)/4.))*(qrs(i,k,3)**((3.+bvtg)/4.))*qci(i,k,1)
         else
            pgacw(i,k)=0.
         endif
         pgacw(i,k) =max(min(pgacw(i,k),qci(i,k,1)/dtcld),0.)
         !pgacw(i,k)=fqg*fqc*pgacw(i,k)
         if(abs(pgacw(i,k))<qmin/dtcld)then
            pgacw(i,k)=0.
         endif
         ! update qc, qr, qg, t
         qci(i,k,1)=max(qci(i,k,1)-             pgacw(i,k)*dtcld,0.)
         qrs(i,k,1)=max(qrs(i,k,1)+(1.-fsupcol)*pgacw(i,k)*dtcld,0.) !t>=t0
         qrs(i,k,3)=max(qrs(i,k,3)+    fsupcol *pgacw(i,k)*dtcld,0.) !t<t0
         t  (i,k  )=t  (i,k  )+fsupcol*pgacw(i,k)*dtcld*xlf/cpm(i,k)
         ! t>=t0 pgeml
         pgacw(i,k)=(1-fsupcol)*pgacw(i,k)
      enddo
   enddo

   end	subroutine accret1

!=======================================================================
!
!=======================================================================
   subroutine accret2(qrs,t,q, den,dtcld,   &
                     psacw,pgacw,pracs,psacr,pgacr,pgacs,pseml,pgeml,  &   
                     ims,ime, kms,kme, its,ite, kts,kte)
   IMPLICIT NONE
   integer ::         ims,ime, kms,kme ,its,ite, kts,kte
!-------------------------------------------------------------------
   REAL, DIMENSION( its:ite , kts:kte, 3 ) :: qrs
   REAL, DIMENSION( ims:ime , kms:kme ) :: den,q
   REAL, DIMENSION( its:ite , kts:kte) :: psacw,pgacw,pracs,psacr,pgacr,pgacs,pseml,pgeml,  &
                                          t, xl, cpm
   REAL  ::  supcol,vt2r,vt2s,vt2g,dtcld,xlf,egs
   REAL  ::  acrfac1,acrfac2,acrfac3,acrfac4,pracs1,psacr1,pgacr1,pgacs1
   INTEGER :: i,  k
   real :: fsupcol, ft0, fqr, fqs, fqg, temp1, delta2, a, b, c, d

   do k = kts, kte
      do i = its, ite

   !-------------------------------------------------------------
   ! pracs: Accretion of snow by rain [LFO 27]
   !         (T<T0: S->G) pracs: min=0., max=qrs(i,k,2)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,1),1.e-4,fqr,'q0')
         !call smoothif(qrs(i,k,2),1.e-4,fqs,'q0')

         vt2r=vt2r_a*(den(i,k)**((bvtr-2.)/4.))*(max(qrs(i,k,1),qcrmin)**(bvtr/4.))
         vt2s=vt2s_a*(den(i,k)**((bvts-2.)/4.))*(max(qrs(i,k,2),qcrmin)**(bvts/4.))&
               *exp(-alpha*bvts*max(0.,min(90.,t0c-t(i,k)))/4.)
         a=exp(    alpha*max(0.,min(90.,t0c-t(i,k)))   )
         b=exp(-3.*alpha*max(0.,min(90.,t0c-t(i,k)))/2.)&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,2),qcrmin)**(3./2.))*sqrt(sqrt(max(qrs(i,k,1),qcrmin)))
         c=exp(-5.*alpha*max(0.,min(90.,t0c-t(i,k)))/4.)&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,2),qcrmin)**(5./4.))*sqrt     (max(qrs(i,k,1),qcrmin))
         d=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))   )&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,2),qcrmin))         *         (max(qrs(i,k,1),qcrmin)**(3./4.))
         pracs1=pracs_a*a*abs(vt2r-vt2s)*(pracs_b*b+pracs_c*c+pracs_d*d)

         pracs(i,k) = min(pracs1,qrs(i,k,2)/dtcld)
         pracs(i,k)=fsupcol*pracs(i,k)
         if(abs(pracs(i,k))<qmin/dtcld)then
            pracs(i,k)=0.
         endif
         !update qs qg
         qrs(i,k,2)=max(qrs(i,k,2)-pracs(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pracs(i,k)*dtcld,0.)
         pracs(i,k)=0.
   !-------------------------------------------------------------
   ! psacr: Accretion of rain by snow [LFO 28]
   !         (T< T0: R->S or R->G)                 min=0.,max=qrs(i,k,1)/dtcld
   !         (T>=T0: S->R enhance melting of snow) min=0.,max=qrs(i,k,2)/dtcld
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) !not change
!         call smoothif(supcol,0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,1),qcrmin,fqr,'q0')
         !call smoothif(qrs(i,k,2),qcrmin,fqs,'q0')
         !update cpm
         cpm(i,k)=cpmcal(q(i,k)) 
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0


         vt2r=vt2r_a*(den(i,k)**((bvtr-2.)/4.))*(max(qrs(i,k,1),qcrmin)**(bvtr/4.))
         vt2s=vt2s_a*(den(i,k)**((bvts-2.)/4.))*(max(qrs(i,k,2),qcrmin)**(bvts/4.))&
               *exp(-alpha*bvts*max(0.,min(90.,t0c-t(i,k)))/4.)
         a=exp(    alpha*max(0.,min(90.,t0c-t(i,k)))   )
         b=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))/4.)&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin)**(3./2.))*sqrt(sqrt(max(qrs(i,k,2),qcrmin)))
         c=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))/2.)&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin)**(5./4.))*     sqrt(max(qrs(i,k,2),qcrmin))
         d=exp(-3.*alpha*max(0.,min(90.,t0c-t(i,k)))/4.)&
            *(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin))         *         (max(qrs(i,k,2),qcrmin)**(3./4.))
         psacr1=psacr_a*a*abs(vt2r-vt2s)*(psacr_b*b+psacr_c*c+psacr_d*d)

         if(supcol>0.)then ! T<T0
            psacr(i,k) = min(psacr1,qrs(i,k,1)/dtcld)
         else ! T>=T0
            psacr(i,k) = min(psacr1,qrs(i,k,2)/dtcld)
         endif
         !psacr(i,k)=fqr*fqs*psacr(i,k)
         if(abs(psacr(i,k))<qmin/dtcld)then
            psacr(i,k)=0.
         endif
         !update qr qs qg
         if(qrs(i,k,1).lt.1.e-4.and.qrs(i,k,2).lt.1.e-4)then
            delta2=1.
         else
            delta2=0.
         endif
         qrs(i,k,1)=max(qrs(i,k,1)-fsupcol           *psacr(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+fsupcol*   delta2 *psacr(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+fsupcol*(1-delta2)*psacr(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+fsupcol*psacr(i,k)*dtcld*xlf/cpm(i,k)
         ! t>=t0 pseml 
         psacr(i,k)=(1-fsupcol)*psacr(i,k)

   !-------------------------------------------------------------
   ! pgacr: Accretion of rain by graupel [LFO 42]
   !         (T< T0: R->G)                            min=0.,max=qrs(i,k,1)/dtcld
   !         (T>=T0: G->R enhance melting of graupel) min=0.,max=qrs(i,k,3)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,3),qcrmin,fqg,'q0')
         !call smoothif(qrs(i,k,1),qcrmin,fqr,'q0')
         !update cpm
!         cpm(i,k)=cpmcal(q(i,k)) !not change
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0

         vt2r=vt2r_a*(den(i,k)**((bvtr-2.)/4.))*(max(qrs(i,k,1),qcrmin)**(bvtr/4.))
         vt2g=vt2g_a*(den(i,k)**((bvtg-2.)/4.))*(max(qrs(i,k,3),qcrmin)**(bvtg/4.))
         b=(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin)**(3./2.))*sqrt(sqrt(max(qrs(i,k,3),qcrmin)))
         c=(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin)**(5./4.))*     sqrt(max(qrs(i,k,3),qcrmin))
         d=(den(i,k)**(3./4.))*(max(qrs(i,k,1),qcrmin))         *         (max(qrs(i,k,3),qcrmin)**(3./4.))
         pgacr1=pgacr_a*abs(vt2r-vt2g)*(pgacr_b*b+pgacr_c*c+pgacr_d*d)

         if(supcol>0.)then ! T<T0
            pgacr(i,k) = min(pgacr1,qrs(i,k,1)/dtcld)
         else
            pgacr(i,k) = min(pgacr1,qrs(i,k,3)/dtcld)
         endif
         !pgacr(i,k)=fqg*fqr*pgacr(i,k)
         if(abs(pgacr(i,k))<qmin/dtcld)then
            pgacr(i,k)=0.
         endif
         ! t<t0 update qr,qg,t
         qrs(i,k,1)=max(qrs(i,k,1)-fsupcol*pgacr(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+fsupcol*pgacr(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+fsupcol*pgacr(i,k)*dtcld*xlf/cpm(i,k)
         ! t>=t0 pgeml 
         pgacr(i,k)=(1-fsupcol)*pgacr(i,k)
   !-------------------------------------------------------------
   ! pgacs: Accretion of snow by graupel [LFO 29]
   !        (S->G) min=0,max=qrs(i,k,2)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         call smoothif(supcol,0.,fsupcol,'t0')
         !call smoothif(qrs(i,k,3),qcrmin,fqg,'q0')
         !call smoothif(qrs(i,k,2),qcrmin,fqs,'q0')
         egs = min(exp(-0.09*supcol),1.)

         vt2g=vt2g_a*(den(i,k)**((bvtg-2.)/4.))*(max(qrs(i,k,3),qcrmin)**(bvtg/4.))
         vt2s=vt2s_a*(den(i,k)**((bvts-2.)/4.))*(max(qrs(i,k,2),qcrmin)**(bvts/4.))&
               *exp(-alpha*bvts*max(0.,min(90.,t0c-t(i,k)))/4.)
         a=exp(    alpha*max(0.,min(90.,t0c-t(i,k)))   )
         b=exp(-3.*alpha*max(0.,min(90.,t0c-t(i,k)))/2.)*(den(i,k)**(3./4.))&
            *(max(qrs(i,k,2),qcrmin)**(3./2.))*sqrt(sqrt(max(qrs(i,k,3),qcrmin)))
         c=exp(-5.*alpha*max(0.,min(90.,t0c-t(i,k)))/4.)*(den(i,k)**(3./4.))&
            *(max(qrs(i,k,2),qcrmin)**(5./4.))*     sqrt(max(qrs(i,k,3),qcrmin))
         d=exp(-   alpha*max(0.,min(90.,t0c-t(i,k)))   )*(den(i,k)**(3./4.))&
            *(max(qrs(i,k,2),qcrmin))         *         (max(qrs(i,k,3),qcrmin)**(3./4.))
         pgacs1=pgacs_a*a*abs(vt2g-vt2s)*(pgacs_b*b+pgacs_c*c+pgacs_d*d)*egs

         pgacs(i,k) = min(pgacs1,qrs(i,k,2)/dtcld)
         !pgacs(i,k)=fqg*fqs*pgacs(i,k)
         if(abs(pgacs(i,k))<qmin/dtcld)then
            pgacs(i,k)=0.
         endif
         !update qs, qg
         qrs(i,k,2)=max(qrs(i,k,2)-pgacs(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgacs(i,k)*dtcld,0.)
         pgacs(i,k)=0.
    
   !-------------------------------------------------------------
   ! pseml: Enhanced melting of snow by accretion of water
   !        (T>=T0: S->R) pseml<0 max=0,min=-qrs(i,k,2)/dtcld
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) ! not change
         !update cpm
!         cpm(i,k)=cpmcal(q(i,k)) ! not change
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0
         call smoothif(t(i,k),t0c,ft0,'t0')
         call smoothif(qrs(i,k,2),0.,fqs,'q+')
         pseml(i,k) = min(max(cliq*supcol*(psacw(i,k)+psacr(i,k))  &
                       /xlf,-qrs(i,k,2)/dtcld),0.)
         pseml(i,k)=ft0*fqs*pseml(i,k) 
         if(abs(pseml(i,k))<qmin/dtcld)then
            pseml(i,k)=0.
         endif
         !update qs, qr, t
         qrs(i,k,1)=max(qrs(i,k,1)-pseml(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+pseml(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pseml(i,k)*dtcld*xlf/cpm(i,k)
         pseml(i,k)=0.
         psacw(i,k)=0.
         psacr(i,k)=0.
   !-------------------------------------------------------------
   ! pgeml: Enhanced melting of graupel by accretion of water [RH84 A21-A22]
   !        (T>=T0: G->R) pgeml<0 max=0,min=-qrs(i,k,3)/dtcld
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         !update cpm
!         cpm(i,k)=cpmcal(q(i,k)) ! not change
         xl(i,k)=xlcal(t(i,k))
         xlf = xls-xl(i,k)
         if(supcol.lt.0.) xlf = xlf0
         call smoothif(t(i,k),t0c,ft0,'t0')
         call smoothif(qrs(i,k,3),0.,fqg,'q+')
         pgeml(i,k) = min(max(cliq*supcol*(pgacw(i,k)+pgacr(i,k))  &
                       /xlf,-qrs(i,k,3)/dtcld),0.)
         pgeml(i,k)=ft0*fqg*pgeml(i,k)
         if(abs(pgeml(i,k))<qmin/dtcld)then
            pgeml(i,k)=0.
         endif
         !update qg, qr, t
         qrs(i,k,1)=max(qrs(i,k,1)-pgeml(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgeml(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pgeml(i,k)*dtcld*xlf/cpm(i,k)
         pgeml(i,k)=0.
         pgacw(i,k)=0.
         pgacr(i,k)=0.
      enddo
   enddo
   end subroutine accret2

!=======================================================================
!
!=======================================================================
   subroutine accret3(qrs,qci,rh,t,p,den,dtcld,  &
                     q,qs,psdep,pgdep,pigen,psaut,pgaut,psevp,pgevp,pidep,       &
                     ims,ime, kms,kme,its,ite,kts,kte)
   IMPLICIT NONE
   integer ::   ims,ime, kms,kme,its,ite, kts,kte
!-------------------------------------------------------------------
   REAL, DIMENSION( its:ite , kts:kte, 2 ) ::       qci
   REAL, DIMENSION( ims:ime , kms:kme ) ::          den,q,p
   REAL, DIMENSION( its:ite , kts:kte , 3) :: qrs,rh,qs
   REAL, DIMENSION( its:ite , kts:kte) :: pigen,psevp,pgevp,pidep,t,xl,cpm,&
                                           psdep,pgdep,psaut,pgaut
   REAL :: supcol, dtcld,satdt,supsat,qimax, diameter,xni0,roqi0,supice1,supice2,supice3,supice4,alpha2
   real :: pidep0,pidep1,psdep0, pgdep3,pigen0,psevp0,pgevp0,coeres1,coeres2,coeres3,coeres4   
   real :: temp0,temp,xmi
   INTEGER :: i,  k
   real :: fqi, fqr, fqv, fqs, fqg, frh, ft0, fpidep, fpsdep, fpgdep, fsupcol, fsupsat, pidep2
   real :: value01, factor01, source01, vice, a, b, c, d, e, f, g

   do k = kts, kte
      do i = its, ite
   !
   !-------------------------------------------------------------
   ! pidep: Deposition/Sublimation rate of ice [HDC 9] 
   !       (T<T0: V->I or I->V) 
   !       rh(i,k,2)>1.,pidep>0: V->I, min=0,        max=satdt
   !       rh(i,k,2)<1.,pidep<0: I->V, min=-qi/dtcld,max=0,                  
   !-------------------------------------------------------------
         !update supcol
         supcol = t0c-t(i,k)
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update satdt
         supsat = q(i,k)-qs(i,k,2)
         satdt = supsat/dtcld
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))
         call smoothif(supcol,0.,fsupcol,'t+')

         if(qci(i,k,2)>0.)then
            b=diffac_a*den(i,k)*xls*xls*(t(i,k)+120.)/rv/(t(i,k)**3.5)
            c=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,2)
            a=(rh(i,k,2)-1.)/(b+c)
            pidep0 = pidep_a*a*((den(i,k)*qci(i,k,2))**(7./8.))
         else
            pidep0 = 0.
         endif

         if(pidep0<0.)then
            pidep(i,k) = min(max(pidep0,-qci(i,k,2)/dtcld),0.) !qci(i,k,2)/dtcld*dtcld /= qci(i,k,2)
         else
            pidep(i,k) = max(min(pidep0,satdt),0.) 
         endif
         pidep(i,k)=fsupcol*pidep(i,k)
         if(abs(pidep(i,k))<qmin/dtcld)then
            pidep(i,k)=0.
         endif
         !update qv, qi, t
         q  (i,k  )=max(q  (i,k  )-pidep(i,k)*dtcld,0.)
         qci(i,k,2)=max(qci(i,k,2)+pidep(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pidep(i,k)*dtcld*xls/cpm(i,k)
         pidep(i,k)=0.
   !
   !-------------------------------------------------------------
   ! psdep: deposition/sublimation rate of snow [HDC 14] 
   !        (T<T0: V->S or S->V)
   !       rh(i,k,2)>1.,psdep>0: V->S, min=0,        max=satdt
   !       rh(i,k,2)<1.,psdep<0: S->V, min=-qs/dtcld,max=0,                  
   !-------------------------------------------------------------
         !update supcol
         supcol = t0c-t(i,k)
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update satdt
         supsat = q(i,k)-qs(i,k,2)
         satdt = supsat/dtcld
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))

         call smoothif(supcol,0.,fsupcol,'t+')            
!         call smoothif(qrs(i,k,2),0.,fqs,'q+')
!         call smoothif(q  (i,k  ),0.,fqv,'q+')

         a=exp(alpha*max(0.,min(90.,t0c-t(i,k)))/2.)*sqrt(den(i,k)*max(qrs(i,k,2),qcrmin))
         b=exp((3.-bvts)*alpha*max(0.,min(90.,t0c-t(i,k)))/8.)*((t(i,k)+120.)**(1./6.))/(t(i,k)**(5.12/6.)) &
            *(p(i,k)**(1./3.))*(den(i,k)**((13.+3.*bvts)/24.))*(max(qrs(i,k,2),qcrmin)**((5.+bvts)/8.))
         c=diffac_a*den(i,k)*xls*xls*(t(i,k)+120.)/rv/(t(i,k)**3.5)
         d=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,2)
         e=(rh(i,k,2)-1.)/(c+d)
         psdep0 = e*(psdep_a*a+psdep_b*b)

         if(psdep0<0.)then
            psdep(i,k) = min(max(psdep0,-qrs(i,k,2)/dtcld),0.)      
         else
            psdep(i,k) = max(min(psdep0,satdt),0.) 
         endif
         psdep(i,k)=fsupcol*psdep(i,k)
         if(abs(psdep(i,k))<qmin/dtcld)then
            psdep(i,k)=0.
         endif
         ! update q, qs, t
         q  (i,k  )=max(q  (i,k  )-psdep(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+psdep(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+psdep(i,k)*dtcld*xls/cpm(i,k)
         psdep(i,k)=0.
   !
   !------------------------------------------------------------
   ! pgdep: deposition/sublimation rate of graupel [LFO 46] 
   !        (T<T0: V->G or G->V)
   !       rh(i,k,2)>1.,pgdep>0: V->G, min=0,        max=satdt
   !       rh(i,k,2)<1.,pgdep<0: G->V, min=-qg/dtcld,max=0,                  
   !------------------------------------------------------------
         !update supcol
         supcol = t0c-t(i,k)
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update satdt
         supsat = q(i,k)-qs(i,k,2)
         satdt = supsat/dtcld
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))

         call smoothif(supcol,0.,fsupcol,'t+')     
!         call smoothif(qrs(i,k,3),0.,fqg,'q+')
!         call smoothif(q  (i,k  ),0.,fqv,'q+')

         a=sqrt(den(i,k)*max(qrs(i,k,3),qcrmin))
         b=((t(i,k)+120.)**(1./6.))/(t(i,k)**(5.12/6.))*(p(i,k)**(1./3.)) &
            *(den(i,k)**((13.+3.*bvtg)/24.))*(max(qrs(i,k,3),qcrmin)**((5.+bvtg)/8.))
         c=diffac_a*den(i,k)*xls*xls*(t(i,k)+120.)/rv/(t(i,k)**3.5)
         d=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,2)
         e=(rh(i,k,2)-1.)/(c+d)
         pgdep3 = e*(pgdep_a*a+pgdep_b*b)

         if(pgdep3<0.)then
            pgdep(i,k) = min(max(pgdep3,-qrs(i,k,3)/dtcld),0.) 
         else
            pgdep(i,k) = max(min(pgdep3,satdt),0.) ! V->G !
         endif
         pgdep(i,k)=fsupcol*pgdep(i,k)
         if(abs(pgdep(i,k))<qmin/dtcld)then
            pgdep(i,k)=0.
         endif
         !update q, qg, t
         q  (i,k  )=max(q  (i,k  )-pgdep(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgdep(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pgdep(i,k)*dtcld*xls/cpm(i,k)
         pgdep(i,k)=0.
   !-------------------------------------------------------------
   ! pigen: generation(nucleation) of ice from vapor [HDC 7-8]
   !       (T<T0: V->I) min=0,max=min(roqi0/den(i,k)-max(qci(i,k,2),0.))/dtcld,satdt)
   !-------------------------------------------------------------
         !update supcol
         supcol = t0c-t(i,k)
         cpm(i,k)=cpmcal(q(i,k))
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update satdt
         supsat = q(i,k)-qs(i,k,2)
         satdt = supsat/dtcld
         call smoothif(supsat,0.,fsupsat,'q+')
         call smoothif(supcol,0.,fsupcol,'t+')     

         xni0 = 1.e3*exp(0.1*supcol)
         roqi0 = 4.92e-11*xni0**1.33
         pigen0 = min((roqi0/den(i,k)-max(qci(i,k,2),0.))/dtcld,satdt)
         pigen(i,k)=max(pigen0,0.) !
         pigen(i,k)=fsupcol*fsupsat*pigen(i,k)
         if(abs(pigen(i,k))<qmin/dtcld)then
            pigen(i,k)=0.
         endif
         !update q, qi, t
         q  (i,k  )=max(q  (i,k  )-pigen(i,k)*dtcld,0.)
         qci(i,k,2)=max(qci(i,k,2)+pigen(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pigen(i,k)*dtcld*xls/cpm(i,k)
         pigen(i,k )=0.
   !
   !------------------------------------------------------------
   ! psaut: conversion(aggregation) of ice to snow [HDC 12] 
   !        (T<T0: I->S) psaut>0, min=0,max=(qci(i,k,2)-qimax)/dtcld
   !-------------------------------------------------------------
         !update supcol
         supcol = t0c-t(i,k)
         call smoothif(supcol,0.,fsupcol,'t+')     
!         call smoothif(qci(i,k,2),0.,fqi,'q+')
         qimax = roqimax/den(i,k)
         psaut(i,k) = max(0.,(qci(i,k,2)-qimax)/dtcld)
         psaut(i,k) = fsupcol*psaut(i,k)
         if(abs(psaut(i,k))<qmin/dtcld)then
            psaut(i,k)=0.
         endif
         !update qi, qs
         qci(i,k,2)=max(qci(i,k,2)-psaut(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+psaut(i,k)*dtcld,0.)
         psaut(i,k)=0.
   !
   !-------------------------------------------------------------
   ! pgaut: conversion(aggregation) of snow to graupel [LFO 37] 
   !        (T<T0: S->G) pgaut>0 min=0.,max=qrs(i,k,2)/dtcld
   !-------------------------------------------------------------
         !update supcol
!         supcol = t0c-t(i,k) ! not change
!         call smoothif(supcol,0.,fsupcol,'t0')     
!         call smoothif(qrs(i,k,2),0.,fqs,'q+')
         alpha2 = 1.e-3*exp(0.09*(-supcol))
         pgaut(i,k) = min(max(0.,alpha2*(qrs(i,k,2)-qs0))         &
                      ,qrs(i,k,2)/dtcld)
         pgaut(i,k)=fsupcol*pgaut(i,k)
         if(abs(pgaut(i,k))<qmin/dtcld)then
            pgaut(i,k)=0.
         endif
         !update qs, qg
         qrs(i,k,2)=max(qrs(i,k,2)-pgaut(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgaut(i,k)*dtcld,0.)
         pgaut(i,k)=0.
   !
   !-------------------------------------------------------------
   ! psevp: Evaporation of melting snow [RH83 A27] 
   !       (T>=T0: S->V) rh<1., psevp<0, min=-qrs(i,k,2)/dtcld, max=0.
   !-------------------------------------------------------------
!         supcol = t0c-t(i,k) ! not change
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))
         call smoothif(t(i,k),    t0c,ft0,'t+')     

         a=exp(alpha*max(0.,min(90.,t0c-t(i,k)))/2.)*sqrt(den(i,k)*max(qrs(i,k,2),qcrmin))
         b=exp((3.-bvts)*alpha*max(0.,min(90.,t0c-t(i,k)))/8.)*((t(i,k)+120.)**(1./6.)) &
            /(t(i,k)**(5.12/6.))*(p(i,k)**(1./3.))*(den(i,k)**((13.+3.*bvts)/24.))*(max(qrs(i,k,2),qcrmin)**((5.+bvts)/8.))
         c=diffac_a*den(i,k)*xl(i,k)*xl(i,k)*(t(i,k)+120.)/rv/(t(i,k)**3.5)
         d=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,1)
         e=(rh(i,k,1)-1.)/(c+d)
         psevp0 = e*(psevp_a*a+psevp_b*b)

         psevp(i,k) = min(max(psevp0,-qrs(i,k,2)/dtcld),0.)
         psevp(i,k) = ft0*psevp(i,k)
         if(abs(psevp(i,k))<qmin/dtcld)then
            psevp(i,k)=0.
         endif
         !update q, qs, t
         q  (i,k  )=max(q  (i,k  )-psevp(i,k)*dtcld,0.)
         qrs(i,k,2)=max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+psevp(i,k)*dtcld*xls/cpm(i,k)
         psevp(i,k )=0.
   !
   !-------------------------------------------------------------
   ! pgevp: Evaporation of melting graupel [RH84 A19]
   !       (T>=T0: G->V) rh<1., pgevp<0, min=-qrs(i,k,3)/dtcld, max=0.
   !-------------------------------------------------------------
         supcol = t0c-t(i,k)
         !update rh qs
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))

         call smoothif(t(i,k),    t0c,ft0,'t0')     

         a=sqrt(den(i,k)*max(qrs(i,k,3),qcrmin))
         b=((t(i,k)+120.)**(1./6.))/(t(i,k)**(5.12/6.))*(p(i,k)**(1./3.)) &
            *(den(i,k)**((13.+3.*bvtg)/24.))*(max(qrs(i,k,3),qcrmin)**((5.+bvtg)/8.))
         c=diffac_a*den(i,k)*xl(i,k)*xl(i,k)*(t(i,k)+120.)/rv/(t(i,k)**3.5)
         d=diffac_b*p(i,k)/(t(i,k)**1.81)/qs(i,k,1)
         e=(rh(i,k,1)-1.)/(c+d)
         pgevp0 = e*(pgevp_a*a+pgevp_b*b)

         pgevp(i,k) = min(max(pgevp0,-qrs(i,k,3)/dtcld),0.)
         pgevp(i,k) = ft0*pgevp(i,k)
         if(abs(pgevp(i,k))<qmin/dtcld)then
            pgevp(i,k)=0.
         endif
         !update q, qg, t
         q  (i,k  )=max(q  (i,k  )-pgevp(i,k)*dtcld,0.)
         qrs(i,k,3)=max(qrs(i,k,3)+pgevp(i,k)*dtcld,0.)
         t  (i,k  )=t  (i,k  )+pgevp(i,k)*dtcld*xls/cpm(i,k)
         pgevp(i,k )=0.
      enddo
   enddo
   end subroutine accret3


!=======================================================================
!
!=======================================================================
   subroutine pconadd(t,p,q,qci,qs,xl,cpm,dtcld,kte, kts,its, ite,kme, kms,ims, ime)
   IMPLICIT NONE
   integer :: ims,ime, kms,kme ,its,ite, kts,kte
   REAL, DIMENSION( its:ite , kts:kte, 2 ) :: qci
   REAL, DIMENSION( its:ite , kts:kte ) :: t,xl,pcond,work2,cpm
   REAL, DIMENSION( its:ite , kts:kte, 3 ) :: qs,work1,rh
   REAL, DIMENSION( ims:ime , kms:kme ) ::q, p
   integer :: k,i
   real :: hsub,hvap,cvap,ttp,dldt,xa,xb,dldti,xai,xbi,tr,dtcld,qs1,qs2,qs3,qs4,w1,q1
   real :: tmp1, tmp2, f1, f2,qs0
 
   do k = kts, kte
      do i = its, ite
         !update qs 
         call calcrh(t(i,k),p(i,k),q(i,k),rh(i,k,:),qs(i,k,:))
         !update xl, cpm
         xl (i,k)=xlcal (t(i,k))
         cpm(i,k)=cpmcal(q(i,k))
   !----------------------------------------------------------------
   !  pcond: condensational/evaporational rate of cloud water [RH83 A6]
   !     if there exists additional water vapor condensated/if
   !     evaporation of cloud water is not enough to remove subsaturation
   !q>qs, work1>0, pcond>0 V->C min=0,                 max=q(i,k)/dtcld
   !q<qs, work1<0, pcond<0 C->V min=-qci(i,k,1)/dtcld, max=0,
         work1(i,k,1) = conden(t(i,k),q(i,k),qs(i,k,1),xl(i,k),cpm(i,k))

         if(work1(i,k,1)>0.)then
            pcond(i,k)=min(work1(i,k,1),max(q(i,k),0.))/dtcld
         else
            pcond(i,k)=max(work1(i,k,1),-qci(i,k,1))/dtcld
         endif

         if(abs(pcond(i,k))<qmin/dtcld)then
            pcond(i,k)=0.
         endif
         q(i,k)    = max(q(i,k)    -pcond(i,k)*dtcld,0.)
         qci(i,k,1)= max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
         t(i,k)    = t(i,k)    +pcond(i,k)*dtcld*xl(i,k)/cpm(i,k)
         pcond(i,k)=0.
      enddo
   enddo

   end subroutine pconadd

!=======================================================================
!
!=======================================================================
   subroutine smoothif(x,a,f,opt)
   implicit none
   real, intent(in)  :: x, a
   character(len=2), intent(in) :: opt
   real, intent(out) :: f

   real(kind=8) :: k1, a1, x1, c1, f1, k, b

   x1=x
   a1=a
   if(opt(1:1)=='q')then
     c1=1.E-15
   else
     c1=1.E-9
   endif
   k1=747./c1
   if(opt(2:2)=="+")then
     b=a1+710./k1
   else
     b=a1
   endif
   k=-k1*(x1-b)
   f1=1./(1.+exp(k))
   f=f1 
   end subroutine

!
!
!=======================================================================
!
!=======================================================================
   REAL FUNCTION rgmma(x)
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
!  rgmma function:  use infinite product form
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
!
!=======================================================================
!
!=======================================================================
!   compute internal functions
   function cpmcal(x)
   implicit none
   real:: cpmcal,x
   cpmcal= cpd+x*(cpv-cpd)
   end function
!
!=======================================================================
!
!=======================================================================
   function xlcal(x)
   implicit none
   real:: xlcal,x
   xlcal= xlv0-xlv1*(x-t0c)
   end function


!=======================================================================
! a:t, b:q, c:qs, d:xl, e:cpm
!=======================================================================  
   function conden(a,b,c,d,e)
   implicit none
   real :: conden,a,b,c,d,e
   real :: f
   conden = (b-c)/(1.+d*d/(rv*e)*c/(a*a))
   end function             
END MODULE module_mp_wsm6r
!

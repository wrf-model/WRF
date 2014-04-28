!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_temfsfclay

CONTAINS

!-------------------------------------------------------------------
   SUBROUTINE temfsfclay(u3d,v3d,th3d,qv3d,p3d,pi3d,rho,z,ht,         &
                     cp,g,rovcp,r,xlv,psfc,chs,chs2,cqs2,cpm,      &
                     znt,ust,mavail,xland,                         &
                     hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,            &
                     u10,v10,th2,t2,q2,                            &
                     svp1,svp2,svp3,svpt0,ep1,ep2,                 &
                     karman,fCor,te_temf,                          &
                     hd_temf,exch_temf,wm_temf,                           &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                    &
                     )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!
! This is the Total Energy - Mass Flux (TEMF) surface layer scheme.
! Initial implementation 2010 by Wayne Angevine, CIRES/NOAA ESRL.
! References:
!    Angevine et al., 2010, MWR
!    Angevine, 2005, JAM
!    Mauritsen et al., 2007, JAS
!
!-------------------------------------------------------------------
!-------------------------------------------------------------------
!-- u3d         3D u-velocity interpolated to theta points (m/s)
!-- v3d         3D v-velocity interpolated to theta points (m/s)
!-- th3d        potential temperature (K)
!-- qv3d        3D water vapor mixing ratio (Kg/Kg)
!-- p3d         3D pressure (Pa)
!-- cp          heat capacity at constant pressure for dry air (J/kg/K)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       R/CP
!-- r           gas constant for dry air (J/kg/K)
!-- xlv         latent heat of vaporization for water (J/kg)
!-- psfc        surface pressure (Pa)
!-- chs         heat/moisture exchange coefficient for LSM (m/s)
!-- chs2
!-- cqs2
!-- cpm
!-- znt         roughness length (m)
!-- ust         u* in similarity theory (m/s)
!-- mavail      surface moisture availability (between 0 and 1)
!-- xland       land mask (1 for land, 2 for water)
!-- hfx         upward heat flux at the surface (W/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- lh          net upward latent heat flux at surface (W/m^2)
!-- tsk         surface temperature (K)
!-- flhc        exchange coefficient for heat (W/m^2/K)
!-- flqc        exchange coefficient for moisture (kg/m^2/s)
!-- qgh         lowest-level saturated mixing ratio
!-- qsfc        ground saturated mixing ratio
!-- u10         diagnostic 10m u wind
!-- v10         diagnostic 10m v wind
!-- th2         diagnostic 2m theta (K)
!-- t2          diagnostic 2m temperature (K)
!-- q2          diagnostic 2m mixing ratio (kg/kg)
!-- svp1        constant for saturation vapor pressure (kPa)
!-- svp2        constant for saturation vapor pressure (dimensionless)
!-- svp3        constant for saturation vapor pressure (K)
!-- svpt0       constant for saturation vapor pressure (K)
!-- ep1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- ep2         constant for specific humidity calculation 
!               (R_d/R_v) (dimensionless)
!-- karman      Von Karman constant
!-- fCor        Coriolis parameter
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------
      INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
!                                                               
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   ) :: u3d, v3d, th3d, qv3d, p3d, pi3d, rho, z
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   ) :: mavail, xland, tsk, fCor, ht, psfc, znt
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT) :: hfx, qfx, lh, flhc, flqc
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT) :: ust, chs2, cqs2, chs, cpm, qgh, qsfc
      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(OUT  ) :: u10, v10, th2, t2, q2
      REAL,     DIMENSION( ims:ime, jms:jme )           , &
                INTENT(IN   ) :: hd_temf
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(INOUT) :: te_temf
      REAL,     DIMENSION( ims:ime, jms:jme )           , &
                INTENT(  OUT) :: exch_temf
      REAL,     DIMENSION( ims:ime, jms:jme )           , &
                INTENT(INOUT) :: wm_temf

                                        
      REAL,     INTENT(IN   ) :: cp,g,rovcp,r,xlv
      REAL,     INTENT(IN   ) :: svp1,svp2,svp3,svpt0
      REAL,     INTENT(IN   ) :: ep1,ep2,karman
!
! LOCAL VARS

      INTEGER ::  J
!

      DO J=jts,jte

        CALL temfsfclay1d(j,u1d=u3d(ims,kms,j),v1d=v3d(ims,kms,j),     &
                th1d=th3d(ims,kms,j),qv1d=qv3d(ims,kms,j),p1d=p3d(ims,kms,j), &
                pi1d=pi3d(ims,kms,j),rho=rho(ims,kms,j),z=z(ims,kms,j),&
                zsrf=ht(ims,j),      &
                cp=cp,g=g,rovcp=rovcp,r=r,xlv=xlv,psfc=psfc(ims,j),    &
                chs=chs(ims,j),chs2=chs2(ims,j),cqs2=cqs2(ims,j),      &
                cpm=cpm(ims,j),znt=znt(ims,j),ust=ust(ims,j),          &
                mavail=mavail(ims,j),xland=xland(ims,j),    &
                hfx=hfx(ims,j),qfx=qfx(ims,j),lh=lh(ims,j),tsk=tsk(ims,j), &
                flhc=flhc(ims,j),flqc=flqc(ims,j),qgh=qgh(ims,j),      &
                qsfc=qsfc(ims,j),u10=u10(ims,j),v10=v10(ims,j),        &
                th2=th2(ims,j),t2=t2(ims,j),q2=q2(ims,j),        &
                svp1=svp1,svp2=svp2,svp3=svp3,svpt0=svpt0,             &
                ep1=ep1,ep2=ep2,karman=karman,fCor=fCor(ims,j),  &
                te_temfx=te_temf(ims,kms,j),hd_temfx=hd_temf(ims,j), &
                exch_temfx=exch_temf(ims,j),wm_temfx=wm_temf(ims,j), &
                ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,     &
                ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,     &
                its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte      &
                                                                   )
      ENDDO

   END SUBROUTINE temfsfclay


!-------------------------------------------------------------------
   SUBROUTINE temfsfclay1d(j,u1d,v1d,th1d,qv1d,p1d, &
                pi1d,rho,z,zsrf,cp,g,rovcp,r,xlv,psfc,    &
                chs,chs2,cqs2,cpm,znt,ust,          &
                mavail,xland,hfx,qfx,lh,tsk, &
                flhc,flqc,qgh,qsfc,u10,v10,        &
                th2,t2,q2,svp1,svp2,svp3,svpt0,             &
                ep1,ep2,karman,fCor,  &
                te_temfx,hd_temfx,exch_temfx,wm_temfx,       &
                ids,ide, jds,jde, kds,kde,                    &
                ims,ime, jms,jme, kms,kme,                    &
                its,ite, jts,jte, kts,kte                    &
                     )
!!-------------------------------------------------------------------
      IMPLICIT NONE
!!-------------------------------------------------------------------
      INTEGER,  INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, &
                                        j
                                                               
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) ::             &
                                        u1d,v1d,qv1d,p1d,th1d,pi1d,rho,z,zsrf
      REAL,     INTENT(IN   ) ::        cp,g,rovcp,r,xlv
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: psfc,znt
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        chs,chs2,cqs2,cpm,ust
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: mavail,xland 
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        hfx,qfx,lh
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: tsk
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) ::             &
                                        flhc,flqc
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) ::             &
                                        qgh,qsfc
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) ::             &
                                        u10,v10,th2,t2,q2
      REAL,     INTENT(IN   ) ::        svp1,svp2,svp3,svpt0
      REAL,     INTENT(IN   ) ::        ep1,ep2,karman
      REAL,     DIMENSION( ims:ime ), INTENT(IN   ) :: fCor,hd_temfx
      REAL,     DIMENSION( ims:ime ), INTENT(INOUT) :: te_temfx
      REAL,     DIMENSION( ims:ime ), INTENT(  OUT) :: exch_temfx, wm_temfx
!
!! LOCAL VARS
! TE model constants
   real, parameter :: visc_temf = 1.57e-5
   real, parameter :: conduc_temf = 1.57e-5 / 0.733
   logical, parameter :: MFopt = .true.  ! Use mass flux or not
   real, parameter :: TEmin = 1e-3
   real, parameter :: ftau0 = 0.17
   real, parameter :: fth0 = 0.145
!   real, parameter :: fth0 = 0.12    ! WA 10/13/10 to make PrT0 ~= 1
   real, parameter :: Cf = 0.185
   real, parameter :: CN = 2.0
!   real, parameter :: Ceps = ftau0**1.5
   real, parameter :: Ceps = 0.070
   real, parameter :: Cgamma = Ceps
   real, parameter :: Cphi = Ceps
!   real, parameter :: PrT0 = Cphi/Ceps * ftau0**2. / 2 / fth0**2.
   real, parameter :: PrT0 = Cphi/Ceps * ftau0**2 / 2. / fth0**2
!
   integer :: i
   real :: e1
   real, dimension( its:ite)    ::  wstr, ang, wm
   real, dimension( its:ite)    ::  z0t
   real, dimension( its:ite) :: dthdz, dqtdz, dudz, dvdz
   real, dimension( its:ite) :: lepsmin
   real, dimension( its:ite) :: thetav
   real, dimension( its:ite) :: zt,zm
   real, dimension( its:ite) :: N2, S, Ri, beta, ftau, fth, ratio
   real, dimension( its:ite) :: TKE, TE2
   real, dimension( its:ite) :: ustrtilde, linv, leps
   real, dimension( its:ite) :: km, kh
   real, dimension( its:ite) :: qsfc_air
!!-------------------------------------------------------------------

!!!!!!! ******
! WA Known outages:  None

   do i = its,ite      ! Main loop 

      ! Calculate surface saturated q and q in air at surface
      e1=svp1*exp(svp2*(tsk(i)-svpt0)/(tsk(i)-svp3))                       
      qsfc(i)=ep2*e1/((psfc(i)/1000.)-e1)
      qsfc_air(i) = qsfc(i) * mavail(i)
      thetav(i) = (tsk(i)/pi1d(i)) * (1. + 0.608*qsfc_air(i))  ! WA Assumes ql(env)=0, what if it isn't?
      ! WA TEST (R5) set z0t = z0
      ! z0t(i) = znt(i) / 10.0   ! WA this is hard coded in Matlab version
      z0t(i) = znt(i)

      ! Get height and delta at turbulence levels and mass levels
      zt(i) = (z(i) - zsrf(i) - znt(i)) / 2.
      zm(i) = z(i) - zsrf(i)

      ! Gradients at first level
      dthdz(i) = (th1d(i)-(tsk(i)/pi1d(i))) / (zt(i) * log10(zm(i)/z0t(i)))
      dqtdz(i) = (qv1d(i)-qsfc_air(i)) / (zt(i) * log10(zm(i)/z0t(i)))
      dudz(i) = u1d(i) / (zt(i) * log10(zm(i)/znt(i)))
      dvdz(i) = v1d(i) / (zt(i) * log10(zm(i)/znt(i)))

      ! WA doing this because te_temf may not be initialized,
      ! would be better to do it in initialization routine but it's 
      ! not available in module_physics_init.
      if (te_temfx(i) < TEmin) te_temfx(i) = TEmin

      if ( hfx(i) > 0.) then
         wstr(i) = (g * hd_temfx(i) / thetav(i) * (hfx(i)/(rho(i)*cp))) ** (1./3.)
      else
         wstr(i) = 0.
      end if

      ! Find stability parameters and length scale
      ! WA Calculation of N should really use d(thetaV)/dz not dthdz
      ! WA 7/1/09 allow N to be negative
      ! if ( dthdz(i) >= 0.) then
      ! N(i) = csqrt(g / thetav(i) * dthdz(i))
      ! else
      !    N(i) = 0.
      ! end if
      N2(i) = g / thetav(i) * dthdz(i)
      S(i) = sqrt(dudz(i)**2. + dvdz(i)**2.)
      ! Ri(i) = N(i)**2. / S(i)**2.
      Ri(i) = N2(i) / S(i)**2.
      ! if (S(i) < 1e-15) Ri(i) = 1./1e-15
      if (S(i) < 1e-15) then
         print *,'In TEMF SFC Limiting Ri,S,N2,Ri,u,v = ',S(i),N2(i),Ri(i),u1d(i),v1d(i)
         if (N2(i) >= 0) then
            Ri(i) = 0.2
         else
            Ri(i) = -1.
         end if
      end if
      if (Ri(i) > 0.2) then  ! WA TEST to prevent runaway
         Ri(i) = 0.2
      end if
      beta(i) = g / thetav(i)
      ! WA 7/1/09 adjust ratio, ftau, fth for Ri>0
      if (Ri(i) > 0) then
         ratio(i) = Ri(i)/(Cphi**2.*ftau0**2./(2.*Ceps**2.*fth0**2.)+3.*Ri(i))
         ftau(i) = ftau0 * ((3./4.) / (1.+4.*Ri(i)) + 1./4.)
         fth(i) = fth0 / (1.+4.*Ri(i))
         ! TE2(i) = 2. * te_temfx(i) * ratio(i) * N(i)**2. / beta(i)**2.
         TE2(i) = 2. * te_temfx(i) * ratio(i) * N2(i) / beta(i)**2.
      else
         ratio(i) = Ri(i)/(Cphi**2.*ftau0**2./(-2.*Ceps**2.*fth0**2.)+2.*Ri(i))
         ftau(i) = ftau0
         fth(i) = fth0
         TE2(i) = 0.
      end if
      TKE(i) = te_temfx(i) * (1. - ratio(i))
      ustrtilde(i) = sqrt(ftau(i) * TKE(i))
      ! linv(i) = 1./karman / zt(i) + abs(fCor(i)) / (Cf*ustrtilde(i)) + N(i)/(CN*ustrtilde(i))
      if (N2(i) > 0.) then
         linv(i) = 1./karman / zt(i) + abs(fCor(i)) / (Cf*ustrtilde(i)) + sqrt(N2(i))/(CN*ustrtilde(i))
      else
         linv(i) = 1./karman / zt(i) + abs(fCor(i)) / (Cf*ustrtilde(i))
      end if
      leps(i) = 1./linv(i)
      ! WA TEST (R4) remove lower limit on leps
      ! lepsmin(i) = min(0.4*zt(i), 5.)
      lepsmin(i) = 0.
      leps(i) = max(leps(i),lepsmin(i))


      ! Find diffusion coefficients
      ! First use basic formulae for stable and neutral cases,
      ! then for convective conditions, and finally choose the larger
      km(i) = TKE(i)**1.5 * ftau(i)**2. / (-beta(i) * fth(i) * sqrt(TE2(i)) + Ceps * sqrt(TKE(i)*te_temfx(i)) / leps(i))
      kh(i) = 2. * leps(i) * fth(i)**2. * TKE(i) / sqrt(te_temfx(i)) / Cphi
      km(i) = max(km(i),visc_temf)
      kh(i) = max(kh(i),conduc_temf)

      ! Surface fluxes
      ! WA TEST 11/7/13 use w* as a component of the mean wind inside the
      ! u* calculation instead of in the velocity scale below (Felix)
      ! ust(i) = sqrt(ftau(i)/ftau0) * sqrt(u1d(i)**2. + v1d(i)**2.) * leps(i) / log(zm(i)/znt(i)) / zt(i)
      ust(i) = sqrt(ftau(i)/ftau0) * sqrt(u1d(i)**2. + v1d(i)**2. + (0.5*wstr(i))**2.) * leps(i) / log(zm(i)/znt(i)) / zt(i)
      ang(i) = atan2(v1d(i),u1d(i))

      ! WA TEST 11/7/13 back to wm = u* but with "whole" wind in u* above
      wm(i) = ust(i)
      ! Calculate mixed scaling velocity (Moeng & Sullivan 1994 JAS p.1021)
      ! wm(i) = 0.5 * (1./5. * (wstr(i)**3. + 5. * ust(i)**3.)) ** (1./3.) 

      ! WA TEST 2/22/11 average with previous value to reduce instability
      wm(i) = (wm(i) + wm_temfx(i)) / 2.0
      ! WA TEST 11/26/13 set min value
      wm_temfx(i) = max(wm(i),1e-2)

      ! Populate surface exchange coefficient variables to go back out
      ! for next time step of surface scheme
      ! Unit specifications in SLAB and sfclay are conflicting and probably
      ! incorrect.  This will give a dynamic heat flux (W/m^2) or moisture
      ! flux (kg(water)/(m^2*s)) when multiplied by a difference.
      ! These formulae are the same as what's used above to get surface
      ! flux from surface temperature and specific humidity.
      flhc(i) = rho(i) * cp * fth(i)/fth0 * wm(i) * leps(i) / PrT0 / log(zm(i)/z0t(i)) / zt(i)
      flqc(i)  = rho(i) * fth(i)/fth0 * wm(i) * leps(i) / PrT0 / log(zm(i)/z0t(i)) / zt(i) * mavail(i)
      exch_temfx(i)  = flqc(i) / mavail(i)
      chs(i) = flqc(i) / rho(i) / mavail(i) 
      ! WA Must exchange coeffs be limited to avoid runaway in some 
      ! (convective?) conditions?  Something like this is done in sfclay.
      ! Doing nothing for now.

      ! Populate surface heat and moisture fluxes
      hfx(i) = flhc(i) * (tsk(i) - th1d(i)*pi1d(i))
      ! qfx(i) = flqc(i) * (qsfc_air(i) - qv1d(i))  ! WA 2/16/11
      qfx(i) = flqc(i) * (qsfc(i) - qv1d(i))
      qfx(i) = max(qfx(i),0.)  ! WA this is done in sfclay, is it right?
      lh(i)=xlv*qfx(i)


      ! Populate 10 m winds and 2 m temp and 2 m exchange coeffs
      ! WA Note this only works if first mass level is above 10 m
      u10(i) = u1d(i) * log(10.0/znt(i)) / log(zm(i)/znt(i))
      v10(i) = v1d(i) * log(10.0/znt(i)) / log(zm(i)/znt(i))
      t2(i) = (tsk(i)/pi1d(i) + (th1d(i) - tsk(i)/pi1d(i)) * log(2.0/z0t(i)) / log(zm(i)/z0t(i))) * pi1d(i)  ! WA this should also use pi at z0
      th2(i) = t2(i) / pi1d(i)
      q2(i) = (qsfc_air(i) + (qv1d(i) - qsfc_air(i)) * log(2.0/znt(i)) / log(zm(i)/znt(i)))
      ! WA are these correct?  Difference between chs2 and cqs2 is unclear
      ! At the moment the only difference is z0t vs. znt
      chs2(i)  = fth(i)/fth0 * wm(i) * leps(i) / PrT0 / log(2.0/z0t(i)) / zt(i)
      cqs2(i)  = fth(i)/fth0 * wm(i) * leps(i) / PrT0 / log(2.0/znt(i)) / zt(i)

      ! Calculate qgh (saturated at first-level temp) and cpm
      e1=svp1*exp(svp2*((th1d(i)*pi1d(i))-svpt0)/((th1d(i)*pi1d(i))-svp3))
      qgh(i)=ep2*e1/((p1d(i)/1000.)-e1)
      cpm(i)=cp*(1.+0.8*qv1d(i))                                   

   end do  ! Main loop

   END SUBROUTINE temfsfclay1d

!====================================================================
   SUBROUTINE temfsfclayinit( restart, allowed_to_read,                &
                              wm_temf,                                 &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte                 )

   logical , intent(in)          :: restart, allowed_to_read
   REAL,     DIMENSION( ims:ime, jms:jme )           , &
                INTENT(  OUT) :: wm_temf
   integer , intent(in)          ::  ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
   
! Local variables
   integer :: i, j, itf, jtf
!
   CALL wrf_debug( 100, 'in temfsfclayinit' )
   jtf = min0(jte,jde-1)
   itf = min0(ite,ide-1)
!
   if(.not.restart)then
     do j = jts,jtf
     do i = its,itf
     ! do j = jms,jme
     ! do i = ims,ime
        wm_temf(i,j) = 0.0
     enddo
     enddo
   endif

   END SUBROUTINE temfsfclayinit

!-------------------------------------------------------------------          

END MODULE module_sf_temfsfclay

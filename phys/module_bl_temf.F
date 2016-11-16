!wrf:model_layer:physics
!
!
!
!
module module_bl_temf
contains
!
!-------------------------------------------------------------------
!
   subroutine temfpbl(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,rho,      &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qi,                          &
                  g,cp,rcp,r_d,r_v,cpv,                                   &
                  z,xlv,psfc,                                          &
                  p_top,                                               &
                  znt,ht,ust,zol,hol,hpbl,psim,psih,                         &
                  xland,hfx,qfx,tsk,qsfc,gz1oz0,wspd,br,                    &
                  dt,dtmin,kpbl2d,                                             &
                  svp1,svp2,svp3,svpt0,ep1,ep2,karman,eomeg,stbolt,            &
                  kh_temf,km_temf,                                            &
                  u10,v10,t2,                                                  &
                  te_temf,shf_temf,qf_temf,uw_temf,vw_temf,                    &
                  wupd_temf,mf_temf,thup_temf,qtup_temf,qlup_temf,            &
                  cf3d_temf,cfm_temf,                                         &
                  hd_temf,lcl_temf,hct_temf,                            &
                  flhc,flqc,exch_temf,                                &
                  fCor,                                                        &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte                                   &
                  )
!-------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------
! New variables for TEMF
!-- te_temf     Total energy from this scheme
!-- shf_temf    Sensible heat flux profile from this scheme (kinematic)
!-- qf_temf     Moisture flux profile from this scheme (kinematic)
!-- uw_temf     U momentum flux component from this scheme
!-- vw_temf     V momentum flux component from this scheme
!-- kh_temf     Exchange coefficient for heat (3D)
!-- km_temf     Exchange coefficient for momentum (3D)
!-- wupd_temf     Updraft velocity from TEMF BL scheme
!-- mf_temf       Mass flux from TEMF BL scheme
!-- thup_temf   Updraft thetal from TEMF BL scheme
!-- qtup_temf   Updraft qt from TEMF BL scheme
!-- qlup_temf   Updraft ql from TEMF BL scheme
!-- cf3d_temf   3D cloud fraction from TEMF BL scheme
!-- cfm_temf    Column cloud fraction from TEMF BL scheme
!-- exch_temf   Surface exchange coefficient (as for moisture) from TEMF surface layer scheme
!-- flhc        Surface exchange coefficient for heat (needed by surface scheme)
!-- flqc        Surface exchange coefficient for moisture (including moisture availablity)
!-- fCor        Coriolis parameter (from grid%f)
!
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d	       3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d	3d exner function (dimensionless)
!-- rho 	3d dry air density (kg/m^3)
!-- rublten     u tendency due to
!               pbl parameterization (m/s/s)
!-- rvblten     v tendency due to
!               pbl parameterization (m/s/s)
!-- rthblten    theta tendency due to
!               pbl parameterization (K/s)
!-- rqvblten    qv tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqcblten    qc tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqiblten    qi tendency due to
!               pbl parameterization (kg/kg/s)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       r/cp
!-- r_d          gas constant for dry air (j/kg/k)
!-- rovg 	r/g
!-- z		height above sea level (m)
!-- xlv         latent heat of vaporization (j/kg)
!-- r_v 		gas constant for water vapor (j/kg/k)
!-- psfc        pressure at the surface (pa)
!-- znt		roughness length (m)
!-- ht          terrain height ASL (m)
!-- ust		u* in similarity theory (m/s)
!-- zol		z/l height over monin-obukhov length
!-- hol		pbl height over monin-obukhov length
!-- hpbl	pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland	land mask (1 for land, 2 for water)
!-- hfx		upward heat flux at the surface (w/m^2)
!-- qfx		upward moisture flux at the surface (kg/m^2/s)
!-- tsk		surface temperature (k)
!-- qsfc	surface specific humidity (kg/kg)
!-- gz1oz0      log(z/z0) where z0 is roughness length
!-- wspd        wind speed at lowest model level (m/s)
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- br          bulk richardson number in surface layer
!-- dt		time step (s)
!-- dtmin	time step (minute)
!-- rvovrd      r_v divided by r_d (dimensionless)
!-- svp1        constant for saturation vapor pressure (kpa)
!-- svp2        constant for saturation vapor pressure (dimensionless)
!-- svp3        constant for saturation vapor pressure (k)
!-- svpt0       constant for saturation vapor pressure (k)
!-- ep1         constant for virtual temperature (r_v/r_d - 1) (dimensionless)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
!-- eomeg       angular velocity of earths rotation (rad/s)
!-- stbolt      stefan-boltzmann constant (w/m^2/k^4)
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
! Arguments
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
!
   real,     intent(in   )   ::      dt,dtmin,g,cp,rcp,r_d,r_v,xlv,cpv
!
   real,     intent(in )     ::      svp1,svp2,svp3,svpt0
   real,     intent(in )     ::      ep1,ep2,karman,eomeg,stbolt
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::      qv3d, qc3d, qi3d, &
                                     p3d, pi3d, th3d, t3d, &
				     z, rho
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::      te_temf
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(  out)   ::      shf_temf, qf_temf, uw_temf, vw_temf     , &
                                     wupd_temf, mf_temf, thup_temf, qtup_temf, &
                                     qlup_temf,cf3d_temf
   real,     dimension( ims:ime, jms:jme )                          , &
             intent(inout)   ::      flhc, flqc, exch_temf
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::      fCor
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(  out)   ::      hd_temf, lcl_temf, hct_temf, cfm_temf
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::      p3di
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::      rublten, rvblten, &
                                     rthblten, &
                                     rqvblten, rqcblten, rqiblten
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::      kh_temf, km_temf
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::      u10, v10, t2
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::      xland, &
                                     psim, psih, gz1oz0, br, &
                                     psfc, tsk, qsfc
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::      hfx, qfx
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::      hol, ust, hpbl, znt, wspd, zol
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::      ht
!
  real,      dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::      u3d, v3d
!
  integer,   dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::      kpbl2d
!
     logical, intent(in)        ::   flag_qi
!
!   real,     dimension( ims:ime, kms:kme, jms:jme ), &
!             optional                              , &
!             intent(inout)   ::      rqiblten
!
   real,     optional, intent(in   )   ::  p_top
!
!-------------------------------------------------------
! Local variables
   integer :: j

   do j = jts,jte
      call temf2d(J=j,ux=u3d(ims,kms,j),vx=v3d(ims,kms,j)                      &
              ,tx=t3d(ims,kms,j),thx=th3d(ims,kms,j)                           &
              ,qvx=qv3d(ims,kms,j),qcx=qc3d(ims,kms,j)                         &
              ,qix=qi3d(ims,kms,j)                                             &
              ,p2d=p3d(ims,kms,j),p2di=p3di(ims,kms,j)                         &
              ,pi2d=pi3d(ims,kms,j),rho=rho(ims,kms,j)                         &
              ,rubltenx=rublten(ims,kms,j),rvbltenx=rvblten(ims,kms,j)         &
              ,rthbltenx=rthblten(ims,kms,j),rqvbltenx=rqvblten(ims,kms,j)     &
              ,rqcbltenx=rqcblten(ims,kms,j),rqibltenx=rqiblten(ims,kms,j)     &
              ,g=g,cp=cp,rcp=rcp,r_d=r_d,r_v=r_v,cpv=cpv            &
              ,z2d=z(ims,kms,j)                         &
              ,xlv=xlv                                                   &
              ,psfcpa=psfc(ims,j),znt=znt(ims,j),zsrf=ht(ims,j),ust=ust(ims,j) &
              ,zol=zol(ims,j),hol=hol(ims,j),hpbl=hpbl(ims,j)                  &
              ,psim=psim(ims,j)                           &
              ,psih=psih(ims,j),xland=xland(ims,j)                             &
              ,hfx=hfx(ims,j),qfx=qfx(ims,j)                                   &
              ,tsk=tsk(ims,j),qsfc=qsfc(ims,j),gz1oz0=gz1oz0(ims,j)           &
              ,wspd=wspd(ims,j),br=br(ims,j)                                   &
              ,dt=dt,dtmin=dtmin,kpbl1d=kpbl2d(ims,j)                          &
              ,svp1=svp1,svp2=svp2,svp3=svp3,svpt0=svpt0                       &
              ,ep1=ep1,ep2=ep2,karman=karman,eomeg=eomeg                       &
              ,stbolt=stbolt                                                   &
              ,kh_temfx=kh_temf(ims,kms,j),km_temfx=km_temf(ims,kms,j)         &
              ,u10=u10(ims,j),v10=v10(ims,j),t2=t2(ims,j)                      &
              ,te_temfx=te_temf(ims,kms,j)                                     &
              ,shf_temfx=shf_temf(ims,kms,j),qf_temfx=qf_temf(ims,kms,j)       &
              ,uw_temfx=uw_temf(ims,kms,j),vw_temfx=vw_temf(ims,kms,j)       &
              ,wupd_temfx=wupd_temf(ims,kms,j),mf_temfx=mf_temf(ims,kms,j)   &
              ,thup_temfx=thup_temf(ims,kms,j),qtup_temfx=qtup_temf(ims,kms,j) &
              ,qlup_temfx=qlup_temf(ims,kms,j) &
              ,cf3d_temfx=cf3d_temf(ims,kms,j),cfm_temfx=cfm_temf(ims,j)       &
              ,hd_temfx=hd_temf(ims,j),lcl_temfx=lcl_temf(ims,j)       &
              ,hct_temfx=hct_temf(ims,j),exch_temfx=exch_temf(ims,j)    &
              ,flhc=flhc(ims,j),flqc=flqc(ims,j)                     &
              ,fCor=fCor(ims,j)                                              &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
   enddo
!
   end subroutine temfpbl
!
!-------------------------------------------------------------------
!
   subroutine temf2d(j,ux,vx,tx,thx,qvx,qcx,qix,p2d,p2di,pi2d,rho,            &
                  rubltenx,rvbltenx,rthbltenx,                                 &
                  rqvbltenx,rqcbltenx,rqibltenx,                               &
                  g,cp,rcp,r_d,r_v,cpv,                            &
                  z2d,                                                  &
                  xlv,psfcpa,                                    &
                  znt,zsrf,ust,zol,hol,hpbl,psim,psih,                      &
                  xland,hfx,qfx,tsk,qsfc,gz1oz0,wspd,br,                   &
                  dt,dtmin,kpbl1d,                                             &
                  svp1,svp2,svp3,svpt0,ep1,ep2,karman,eomeg,stbolt,            &
                  kh_temfx,km_temfx,                                         &
                  u10,v10,t2,                                                 &
                  te_temfx,shf_temfx,qf_temfx,uw_temfx,vw_temfx,               &
                  wupd_temfx,mf_temfx,thup_temfx,qtup_temfx,qlup_temfx,       &
                  cf3d_temfx,cfm_temfx,                                        &
                  hd_temfx,lcl_temfx,hct_temfx,exch_temfx,           &
                  flhc,flqc,                                                 &
                  fCor,                                                        &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte                                   &
                  )
!-------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------
!
! This is the Total Energy - Mass Flux (TEMF) PBL scheme.
! Initial implementation 2010 by Wayne Angevine, CIRES/NOAA ESRL.
! References:
!    Angevine et al., 2010, MWR
!    Angevine, 2005, JAM
!    Mauritsen et al., 2007, JAS
!
!-------------------------------------------------------------------
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte, j
!
   real,     intent(in   )   ::      dt,dtmin,g,cp,rcp,r_d,r_v,cpv,xlv
!
   real,     intent(in )     ::      svp1,svp2,svp3,svpt0
   real,     intent(in )     ::      ep1,ep2,karman,eomeg,stbolt
!
   real,     dimension( ims:ime, kms:kme ),                                    &
             intent(in)      ::      z2d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::      ux, vx
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::      te_temfx
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(  out)   ::      shf_temfx, qf_temfx, uw_temfx, vw_temfx , &
                                     wupd_temfx, mf_temfx,thup_temfx,          &
                                     qtup_temfx, qlup_temfx, cf3d_temfx
   real,     dimension( ims:ime )                                   , &
             intent(  out)   ::      hd_temfx, lcl_temfx, hct_temfx, cfm_temfx
   real,     dimension( ims:ime )                                   , &
             intent(in   )   ::      fCor
   real,     dimension( ims:ime )                                   , &
             intent(inout)   ::      flhc, flqc, exch_temfx
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::      tx, thx, qvx, qcx, qix, pi2d, rho
   real,     dimension( ims:ime, kms:kme )                                 , &
             intent(in   )   ::      p2di, p2d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::      rubltenx, rvbltenx, rthbltenx,            &
                                     rqvbltenx, rqcbltenx, rqibltenx
!
   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::      hol, ust, hpbl, znt
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::      xland, zsrf
   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::      hfx, qfx
!
   real,     dimension( ims:ime ), intent(inout)   ::  wspd
   real,     dimension( ims:ime ), intent(in  )    ::  br
!
   real,     dimension( ims:ime ), intent(in   )   ::  psim, psih
   real,     dimension( ims:ime ), intent(in   )   ::  gz1oz0
!
   real,     dimension( ims:ime ), intent(in   )   ::  psfcpa
   real,     dimension( ims:ime ), intent(in   )   ::  tsk, qsfc
   real,     dimension( ims:ime ), intent(inout)   ::  zol
   integer,  dimension( ims:ime ), intent(out  )   ::  kpbl1d
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::        kh_temfx, km_temfx
!
   real,     dimension( ims:ime )                                            , &
             intent(inout)    ::        u10, v10, t2
!
!
!-----------------------------------------------------------
! Local variables
!
! TE model constants
   logical, parameter :: MFopt = .true.  ! Use mass flux or not
   real, parameter :: visc_temf = 1.57e-4   ! WA TEST bigger minimum K
   real, parameter :: conduc_temf = 1.57e-4 / 0.733
   real, parameter :: Pr_temf = 0.733
   real, parameter :: TEmin = 1e-3
   real, parameter :: ftau0 = 0.17
   real, parameter :: fth0 = 0.145
   real, parameter :: critRi = 0.25
   real, parameter :: Cf = 0.185
   real, parameter :: CN = 2.0
   real, parameter :: Ceps = 0.070
   real, parameter :: Cgamma = Ceps
   real, parameter :: Cphi = Ceps
   real, parameter :: PrT0 = Cphi/Ceps * ftau0**2 / 2. / fth0**2
! EDMF constants
   real, parameter :: CM = 0.03      ! Proportionality constant for subcloud MF
   real, parameter :: Cdelt = 0.006  ! Prefactor for detrainment rate
   real, parameter :: Cw = 0.5       ! Prefactor for surface wUPD
   real, parameter :: Cc = 3.0       ! Prefactor for convective length scale
   real, parameter :: lasymp = 200.0 ! Asymptotic length scale WA 11/20/09
   real, parameter :: hmax = 4000.0  ! Max hd,hct WA 11/20/09
!
   integer :: i, k, kt   ! Loop variable
   integer, dimension( its:ite) ::  h0idx
   real, dimension( its:ite)    ::  h0
   real, dimension( its:ite)    ::  wstr, ang, wm
   real, dimension( its:ite)    ::  hd,lcl,hct,ht
   real, dimension( its:ite)    ::  convection_TKE_surface_src, sfcFTE
   real, dimension( its:ite)    ::  sfcTHVF
   real, dimension( its:ite)    ::  z0t
   integer, dimension( its:ite) ::  hdidx,lclidx,hctidx,htidx
   integer, dimension( its:ite) ::  hmax_idx
   integer, dimension( its:ite) ::  tval
   real, dimension( its:ite, kts:kte) :: thetal, qt
   real, dimension( its:ite, kts:kte) :: u_temf, v_temf
   real, dimension( its:ite, kts:kte) :: rv, rl, rt
   real, dimension( its:ite, kts:kte) :: chi_poisson, gam
   real, dimension( its:ite, kts:kte) :: dthdz, dqtdz, dudz, dvdz
   real, dimension( its:ite, kts:kte) :: lepsmin
   real, dimension( its:ite, kts:kte) :: thetav
   real, dimension( its:ite, kts:kte) :: MFCth, MFCq, MFCu, MFCv
   real, dimension( its:ite, kts:kte) :: MFCql, MFCthv, MFCTE
   real, dimension( its:ite, kts:kte) :: epsmf, deltmf, dMdz
   real, dimension( its:ite, kts:kte) :: UUPD, VUPD
   real, dimension( its:ite, kts:kte) :: thetavUPD, qlUPD, TEUPD
   real, dimension( its:ite, kts:kte) :: thetavUPDmoist, wupd_dry
   real, dimension( its:ite, kts:kte) :: B, Bmoist
   real, dimension( its:ite, kts:kte) :: zm, zt, dzm, dzt
   real, dimension( its:ite, kts:kte) :: dthUPDdz, dqtup_temfxdz, dwUPDdz
   real, dimension( its:ite, kts:kte) :: dwUPDmoistdz
   real, dimension( its:ite, kts:kte) :: dUUPDdz, dVUPDdz, dTEUPDdz
   real, dimension( its:ite, kts:kte) :: TUPD, rstUPD, rUPD, rlUPD, qstUPD
   real, dimension( its:ite, kts:kte) :: N2, S, Ri, beta, ftau, fth, ratio
   real, dimension( its:ite, kts:kte) :: TKE, TE2
   real, dimension( its:ite, kts:kte) :: ustrtilde, linv, leps
   real, dimension( its:ite, kts:kte) :: km, kh
   real, dimension( its:ite, kts:kte) :: Fz, QFK, uwk, vwk
   real, dimension( its:ite, kts:kte) :: km_conv, kh_conv, lconv
   real, dimension( its:ite, kts:kte) :: alpha2, beta2   ! For thetav flux calculation
   real, dimension( its:ite, kts:kte) :: THVF, buoy_src, srcs
   real, dimension( its:ite, kts:kte) :: u_new, v_new
   real, dimension( its:ite, kts:kte) :: thx_new, qvx_new, qcx_new
   real, dimension( its:ite, kts:kte) :: thup_new, qvup_new
   real, dimension( its:ite, kts:kte) :: beta1 ! For saturation humidity calculations
   real Cepsmf    ! Prefactor for entrainment rate
   real red_fact  ! for reducing MF components
   logical is_convective
   ! Vars for cloud fraction calculation
   real, dimension( its:ite, kts:kte) :: au, sigq, qst, satdef
   real sigq2, rst

!----------------------------------------------------------------------
! Grid staggering:  Matlab version has mass and turbulence levels.
! WRF has full levels (with w) and half levels (u,v,theta,q*).  Both
! sets of levels use the same indices (kts:kte).  See pbl_driver or
! WRF Physics doc for (a few) details.  
! So *mass levels correspond to half levels.*
! WRF full levels are ignored, we define our own turbulence levels
! in order to put the first one below the first half level.
! Another difference is that
! the Matlab version (and the Mauritsen et al. paper) consider the
! first mass level to be at z0 (effectively the surface).  WRF considers
! the first half level to be above the effective surface.  The first half
! level, at k=1, has nonzero values of u,v for example.  Here we convert
! all incoming variables to internal ones with the correct indexing
! in order to make the code consistent with the Matlab version.  We
! already had to do this for thetal and qt anyway, so the only additional
! overhead is for u and v.
! I use suffixes m for mass and t for turbulence as in Matlab for things
! like indices.
! Note that zsrf is the terrain height ASL, from Registry variable ht.
! Translations (Matlab to WRF):
!     dzt -> calculated below
!     dzm -> not supplied, calculated below
!     k -> karman
!     z0 -> znt
!     z0t -> not in WRF, calculated below
!     zt -> calculated below
!     zm -> (z2d - zsrf) but NOTE zm(1) is now z0 (znt) and zm(2) is
!                                 z2d(1) - zsrf
!
! WA I take the temperature at z0 to be
! TSK.  This isn't exactly robust.
! WA 2/16/11 removed calculation of flhc, flqc which are not needed here.
! These should be removed from the calling sequence someday.
!
! Other notes:
! - I have often used 1 instead of kts below, because the scheme demands
!   to know where the surface is.  It won't work if kts .NE. 1.


   do i = its,ite      ! Main loop 

      ! Get incoming surface theta from TSK (WA for now)
      thetal(i,1) = tsk(i) / pi2d(i,1)  ! WA really should use Exner func. at z0
      qt(i,1) = qvx(i,1)
      rv(i,1) = qt(i,1) / (1.-qt(i,1))   ! Water vapor
      rl(i,1) = 0.
      rt(i,1) = rv(i,1) + rl(i,1)        ! Total water (without ice)
      chi_poisson(i,1) = rcp * (1.+rv(i,1)/ep2) / (1.+rv(i,1)*cpv/cp)
      gam(i,1) = rv(i,1) * r_v / (cp + rv(i,1)*cpv)
      thetav(i,1) = thetal(i,1) * (1. + 0.608*qt(i,1) - qcx(i,1))  ! WA 4/6/10 allow environment liquid
      z0t(i) = znt(i)

      ! Convert incoming theta to thetal and qv,qc to qt
      ! NOTE this is where the indexing gets changed from WRF to TEMF basis
      do k = kts+1,kte
         ! Convert specific humidities to mixing ratios
         rv(i,k) = qvx(i,k-1) / (1.-qvx(i,k-1))   ! Water vapor
         rl(i,k) = qcx(i,k-1) / (1.-qcx(i,k-1))   ! Liquid water 
         rt(i,k) = rv(i,k) + rl(i,k)        ! Total water (without ice)
         chi_poisson(i,k) = rcp * (1.+rv(i,k)/ep2) / (1.+rv(i,k)*cpv/cp)
         gam(i,k) = rt(i,k) * r_v / (cp + rt(i,k)*cpv)
         thetal(i,k) = thx(i,k-1) * &
            ((ep2+rv(i,k))/(ep2+rt(i,k)))**chi_poisson(i,k) * &
            (rv(i,k)/rt(i,k))**(-gam(i,k)) * exp( -xlv*rl(i,k) / &
            ((cp + rt(i,k)*cpv) * tx(i,k)))
         qt(i,k) = qvx(i,k-1) + qcx(i,k-1)
         thetav(i,k) = thetal(i,k) * (1. + 0.608*qt(i,k) - qcx(i,k-1))  ! WA 4/6/10 allow environment liquid
      end do

      ! Convert incoming u,v to internal u_temf, v_temf
      ! NOTE this is where the indexing gets changed from WRF to TEMF basis
      u_temf(i,1) = 0.   ! zero winds at z0
      v_temf(i,1) = 0.
      do k = kts+1,kte
         u_temf(i,k) = ux(i,k-1)
         v_temf(i,k) = vx(i,k-1)
      end do

      ! Get delta height at half (mass) levels
      zm(i,1) = znt(i)
      dzt(i,1) = z2d(i,1) - zsrf(i) - zm(i,1)
      ! Get height and delta at turbulence levels
      zt(i,1) = (z2d(i,1) - zsrf(i) - znt(i)) / 2.
      do kt = kts+1,kte
         zm(i,kt) = z2d(i,kt-1) - zsrf(i) ! Convert indexing from WRF to TEMF
         zt(i,kt) = (zm(i,kt) + z2d(i,kt) - zsrf(i)) / 2.
         dzm(i,kt) = zt(i,kt) - zt(i,kt-1)
         dzt(i,kt) = z2d(i,kt+1) - z2d(i,kt)
      end do
      dzm(i,1) = dzm(i,2)  ! WA why?
      dzt(i,kte) = dzt(i,kte-1)    ! WA 12/23/09

      ! Gradients at first level
      dthdz(i,1) = (thetal(i,2)-thetal(i,1)) / (zt(i,1) * log10(zm(i,2)/z0t(i)))
      dqtdz(i,1) = (qt(i,2)-qt(i,1)) / (zt(i,1) * log10(zm(i,2)/z0t(i)))
      dudz(i,1) = (u_temf(i,2)-u_temf(i,1)) / (zt(i,1) * log10(zm(i,2)/znt(i)))
      dvdz(i,1) = (v_temf(i,2)-v_temf(i,1)) / (zt(i,1) * log10(zm(i,2)/znt(i)))

      ! Surface thetaV flux from Stull p.147
      sfcTHVF(i) = hfx(i)/(rho(i,1)*cp) * (1.+0.608*(qvx(i,1)+qcx(i,1))) + 0.608*thetav(i,1)*qf_temfx(i,1)

      ! WA use hd_temf to calculate w* instead of finding h0 here????
      ! Watch initialization!
      h0idx(i) = 1
      h0(i) = zm(i,1)

      lepsmin(i,kts) = 0.

      ! WA 2/11/13 find index just above hmax for use below
      hmax_idx(i) = kte-1

      do k = kts+1,kte-1
         lepsmin(i,k) = 0.

         ! Mean gradients
         dthdz(i,k) = (thetal(i,k+1) - thetal(i,k)) / dzt(i,k)
         dqtdz(i,k) = (qt(i,k+1) - qt(i,k)) / dzt(i,k)
         dudz(i,k) = (u_temf(i,k+1) - u_temf(i,k)) / dzt(i,k)
         dvdz(i,k) = (v_temf(i,k+1) - v_temf(i,k)) / dzt(i,k)

         ! Find h0 (should eventually be interpolated for smoothness)
         if (thetav(i,k) > thetav(i,1) .AND. h0idx(i) .EQ. 1) then
         ! WA 9/28/11 limit h0 as for hd and hct
            if (zm(i,k) < hmax) then
               h0idx(i) = k
               h0(i) = zm(i,k)
            else
               h0idx(i) = k
               h0(i) = hmax
            end if
         end if
         ! WA 2/11/13 find index just above hmax for use below
         if (zm(i,k) > hmax) then
            hmax_idx(i) = min(hmax_idx(i),k)
         end if
      end do

      ! Gradients at top level   

      dthdz(i,kte) = dthdz(i,kte-1)
      dqtdz(i,kte) = dqtdz(i,kte-1)
      dudz(i,kte) = dudz(i,kte-1)
      dvdz(i,kte) = dvdz(i,kte-1)

      if ( hfx(i) > 0.) then
         ! wstr(i) = (g * h0(i) / thetav(i,2) * shf_temfx(i,1) ) ** (1./3.)
         wstr(i) = (g * h0(i) / thetav(i,2) * hfx(i)/(rho(i,1)*cp) ) ** (1./3.)
      else
         wstr(i) = 0.
      end if
      

      ! Set flag convective or not for use below
      is_convective = wstr(i) > 0. .AND. MFopt .AND. dthdz(i,1)<0. .AND. dthdz(i,2)<0.  ! WA 12/16/09 require two levels of negative (unstable) gradient

      ! Find stability parameters and length scale (on turbulence levels)
      do kt = 1,kte-1
         N2(i,kt) = 2. * g / (thetav(i,kt) + thetav(i,kt+1))*dthdz(i,kt)
         S(i,kt) = sqrt(dudz(i,kt)**2. + dvdz(i,kt)**2.)
         Ri(i,kt) = N2(i,kt) / S(i,kt)**2.
         if (S(i,kt) < 1e-15) then
            if (N2(i,kt) >= 0) then
               Ri(i,kt) = 10.
            else
               Ri(i,kt) = -1.
            end if
         end if
         beta(i,kt) = 2. * g / (thetav(i,kt)+thetav(i,kt+1))
         if (Ri(i,kt) > 0) then
            ratio(i,kt) = Ri(i,kt)/(Cphi**2.*ftau0**2./(2.*Ceps**2.*fth0**2.)+3.*Ri(i,kt))
            ftau(i,kt) = ftau0 * ((3./4.) / (1.+4.*Ri(i,kt)) + 1./4.)
            fth(i,kt) = fth0 / (1.+4.*Ri(i,kt))
            TE2(i,kt) = 2. * te_temfx(i,kt) * ratio(i,kt) * N2(i,kt) / beta(i,kt)**2.
         else
            ratio(i,kt) = Ri(i,kt)/(Cphi**2.*ftau0**2./(-2.*Ceps**2.*fth0**2.)+2.*Ri(i,kt))
            ftau(i,kt) = ftau0
            fth(i,kt) = fth0
            TE2(i,kt) = 0.
         end if
         TKE(i,kt) = te_temfx(i,kt) * (1. - ratio(i,kt))
         ustrtilde(i,kt) = sqrt(ftau(i,kt) * TKE(i,kt))
         if (N2(i,kt) > 0.) then
            linv(i,kt) = 1./karman / zt(i,kt) + abs(fCor(i)) / &
               (Cf*ustrtilde(i,kt)) + &
               sqrt(N2(i,kt))/(CN*ustrtilde(i,kt)) + 1./lasymp
         else
            linv(i,kt) = 1./karman / zt(i,kt) + abs(fCor(i)) / &
               (Cf*ustrtilde(i,kt)) + 1./lasymp
         end if
         leps(i,kt) = 1./linv(i,kt)
         leps(i,kt) = max(leps(i,kt),lepsmin(i,kt))
      end do 
      S(i,kte) = 0.0
      N2(i,kte) = 0.0
      TKE(i,kte) = 0.0
      linv(i,kte) = linv(i,kte-1)
      leps(i,kte) = leps(i,kte-1)


      ! Find diffusion coefficients
      ! First use basic formulae for stable and neutral cases,
      ! then for convective conditions, and finally choose the larger
      ! WA 12/23/09 use convective form up to hd/2 always
      ! WA 12/28/09 after restructuring, this block is above MF block,
      ! so hd is not yet available for this timestep, must use h0,
      ! or use hd from previous timestep but be careful about initialization.
      do kt = 1,kte-1    ! WA 12/22/09
         ! WA 4/8/10 remove beta term to avoid negative and huge values
         ! of km due to very small denominator.  This is an interim fix
         ! until we find something better (more theoretically sound).
         ! km(i,kt) = TKE(i,kt)**1.5 * ftau(i,kt)**2. / (-beta(i,kt) * fth(i,kt) * sqrt(TE2(i,kt)) + Ceps * sqrt(TKE(i,kt)*te_temfx(i,kt)) / leps(i,kt))
         km(i,kt) = TKE(i,kt)**1.5 * ftau(i,kt)**2. / (Ceps * sqrt(TKE(i,kt)*te_temfx(i,kt)) / leps(i,kt))
         kh(i,kt) = 2. * leps(i,kt) * fth(i,kt)**2. * TKE(i,kt) / sqrt(te_temfx(i,kt)) / Cphi
         if ( is_convective) then
            ! WA 2/20/14 trap rare "equality" of h0 and zt (only when h0 = hmax)
            if (kt <= h0idx(i) .AND. h0(i)-zt(i,kt) > 1e-15) then
               lconv(i,kt) = 1. / (1. / (karman*zt(i,kt)) + Cc / (karman * (h0(i) - zt(i,kt))))
            else
               lconv(i,kt) = 0.
            end if
            ! WA 12/15/09 use appropriate coeffs to match kh_conv and kh at neutral
            kh_conv(i,kt) = ftau0**2. / Ceps / PrT0 * sqrt(TKE(i,kt)) * lconv(i,kt)
            if (kh_conv(i,kt) < 0.) then
               kh_conv(i,kt) = 0.
            end if
            km_conv(i,kt) = PrT0 * kh_conv(i,kt)
            if (zt(i,kt) <= h0(i)/2.) then
               km(i,kt) = km_conv(i,kt)
               kh(i,kt) = kh_conv(i,kt)
            end if
            if (zt(i,kt) > h0(i)/2. .AND. kt <= h0idx(i)) then
               km(i,kt) = max(km(i,kt),km_conv(i,kt),visc_temf)
               kh(i,kt) = max(kh(i,kt),kh_conv(i,kt),conduc_temf)
            end if
         end if  ! is_convective
         km(i,kt) = max(km(i,kt),visc_temf)
         kh(i,kt) = max(kh(i,kt),conduc_temf)
         Fz(i,kt) = -kh(i,kt) * dthdz(i,kt)  ! Diffusive heat flux
      end do
      km(i,kte) = km(i,kte-1)
      kh(i,kte) = kh(i,kte-1)
      Fz(i,kte) = 0.0


      !*** Mass flux block starts here ***

      if ( is_convective) then

         Cepsmf = 2. / max(200.,h0(i))
         Cepsmf = max(Cepsmf,0.002)
         do k = kts,kte
            ! Calculate lateral entrainment fraction for subcloud layer
            ! epsilon and delta are defined on mass grid (half levels)
            epsmf(i,k) = Cepsmf
         end do

         ! Initialize updraft
         thup_temfx(i,1) = thetal(i,1)    ! No excess
         qtup_temfx(i,1) = qt(i,1)            ! No excess
         rUPD(i,1) = qtup_temfx(i,1) / (1. - qtup_temfx(i,1))
         wupd_temfx(i,1) = Cw * wstr(i)
         wupd_dry(i,1) = Cw * wstr(i)
         UUPD(i,1) = u_temf(i,1)
         VUPD(i,1) = v_temf(i,1)
         thetavUPD(i,1) = thup_temfx(i,1) * (1. + 0.608*qtup_temfx(i,1))  ! WA Assumes no liquid
         thetavUPDmoist(i,1) = thup_temfx(i,1) * (1. + 0.608*qtup_temfx(i,1))  ! WA Assumes no liquid
         TEUPD(i,1) = te_temfx(i,1) + g / thetav(i,1) * sfcTHVF(i)
         qlUPD(i,1) = qcx(i,1)  ! WA allow environment liquid
         TUPD(i,1) = thup_temfx(i,1) * pi2d(i,1)   
         rstUPD(i,1) = rsat(p2d(i,1),TUPD(i,1),ep2)  
         rlUPD(i,1) = 0.

         ! Calculate updraft parameters counting up
         do k = 2,kte
            ! WA 2/11/13 use hmax index to prevent oddness high up
            if ( k < hmax_idx(i)) then
               dthUPDdz(i,k-1) = -epsmf(i,k) * (thup_temfx(i,k-1) - thetal(i,k-1))
               thup_temfx(i,k) = thup_temfx(i,k-1) + dthUPDdz(i,k-1) * dzm(i,k-1)
               dqtup_temfxdz(i,k-1) = -epsmf(i,k) * (qtup_temfx(i,k-1) - qt(i,k-1))
               qtup_temfx(i,k) = qtup_temfx(i,k-1) + dqtup_temfxdz(i,k-1) * dzm(i,k-1)
               thetavUPD(i,k) = thup_temfx(i,k) * (1. + 0.608*qtup_temfx(i,k))  ! WA Assumes no liquid
               B(i,k-1) = g * (thetavUPD(i,k) - thetav(i,k)) / thetav(i,k)
               if ( wupd_dry(i,k-1) < 1e-15 ) then
                  wupd_dry(i,k) = 0.
               else
                  dwUPDdz(i,k-1) = -2. *epsmf(i,k)*wupd_dry(i,k-1) + 0.33*B(i,k-1)/wupd_dry(i,k-1)
                  wupd_dry(i,k) = wupd_dry(i,k-1) + dwUPDdz(i,k-1) * dzm(i,k-1)
               end if
               dUUPDdz(i,k-1) = -epsmf(i,k) * (UUPD(i,k-1) - u_temf(i,k-1))
               UUPD(i,k) = UUPD(i,k-1) + dUUPDdz(i,k-1) * dzm(i,k-1)
               dVUPDdz(i,k-1) = -epsmf(i,k) * (VUPD(i,k-1) - v_temf(i,k-1))
               VUPD(i,k) = VUPD(i,k-1) + dVUPDdz(i,k-1) * dzm(i,k-1)
               dTEUPDdz(i,k-1) = -epsmf(i,k) * (TEUPD(i,k-1) - te_temfx(i,k-1))
               TEUPD(i,k) = TEUPD(i,k-1) + dTEUPDdz(i,k-1) * dzm(i,k-1)
               ! Alternative updraft velocity based on moist thetav
               ! Need thetavUPDmoist, qlUPD
               rUPD(i,k) = qtup_temfx(i,k) / (1. - qtup_temfx(i,k))
               ! WA Updraft temperature assuming no liquid
               TUPD(i,k) = thup_temfx(i,k) * pi2d(i,k)   
               ! Updraft saturation mixing ratio 
               rstUPD(i,k) = rsat(p2d(i,k-1),TUPD(i,k),ep2)  
               ! Correct to actual temperature (Sommeria & Deardorff 1977)
               beta1(i,k) = 0.622 * (xlv/(r_d*TUPD(i,k))) * (xlv/(cp*TUPD(i,k)))
               rstUPD(i,k) = rstUPD(i,k) * (1.0+beta1(i,k)*rUPD(i,k)) / (1.0+beta1(i,k)*rstUPD(i,k))
               qstUPD(i,k) = rstUPD(i,k) / (1. + rstUPD(i,k))
               if (rUPD(i,k) > rstUPD(i,k)) then
                  rlUPD(i,k) = rUPD(i,k) - rstUPD(i,k)
                  qlUPD(i,k) = rlUPD(i,k) / (1. + rlUPD(i,k))
                  thetavUPDmoist(i,k) = (thup_temfx(i,k) + ((xlv/cp)*qlUPD(i,k)/pi2d(i,k))) * &
                                        (1. + 0.608*qstUPD(i,k) - qlUPD(i,k))
               else
                  rlUPD(i,k) = 0.
                  qlUPD(i,k) = qcx(i,k-1)   ! WA 4/6/10 allow environment liquid
                  thetavUPDmoist(i,k) = thup_temfx(i,k) * (1. + 0.608*qtup_temfx(i,k))
               end if
               Bmoist(i,k-1) = g * (thetavUPDmoist(i,k) - thetav(i,k)) / thetav(i,k)
               if ( wupd_temfx(i,k-1) < 1e-15 ) then
                  wupd_temfx(i,k) = 0.
               else
                  dwUPDmoistdz(i,k-1) = -2. *epsmf(i,k)*wupd_temfx(i,k-1) + 0.33*Bmoist(i,k-1)/wupd_temfx(i,k-1)
                  wupd_temfx(i,k) = wupd_temfx(i,k-1) + dwUPDmoistdz(i,k-1) * dzm(i,k-1)
               end if
            else
               thup_temfx(i,k) = thetal(i,k)
               qtup_temfx(i,k) = qt(i,k)
               wupd_dry(i,k) = 0.
               UUPD(i,k) = u_temf(i,k)
               VUPD(i,k) = v_temf(i,k)
               TEUPD(i,k) = te_temfx(i,k)
               qlUPD(i,k) = qcx(i,k-1)
               wupd_temfx(i,k) = 0.
            end if
         end do

         ! Find hd based on wUPD
         if (wupd_dry(i,1) == 0.) then
            hdidx(i) = 1
         else
            hdidx(i) = kte  ! In case wUPD <= 0 not found
            do k = 2,kte
               ! if (wupd_dry(i,k) <= 0.) then
               if (wupd_dry(i,k) <= 0. .OR. zm(i,k) > hmax) then 
                  hdidx(i) = k
                  goto 100   ! FORTRAN made me do it!
               end if
            end do
         end if
100      hd(i) = zm(i,hdidx(i))
         kpbl1d(i) = hdidx(i)
         hpbl(i) = hd(i)       ! WA 5/11/10 hpbl is height.  Should still be replaced by something that works whether convective or not.

         ! Find LCL, hct, and ht
         lclidx(i) = kte   ! In case LCL not found
         do k = kts,kte
            if ( k < hmax_idx(i) .AND. rUPD(i,k) > rstUPD(i,k)) then
               lclidx(i) = k
               goto 200
            end if
         end do
200      lcl(i) = zm(i,lclidx(i))

         if (hd(i) > lcl(i)) then   ! Forced cloud (at least) occurs
            ! Find hct based on wUPDmoist
            if (wupd_temfx(i,1) == 0.) then
               hctidx(i) = 1
            else
               hctidx(i) = kte  ! In case wUPD <= 0 not found
               do k = 2,kte
                  if (wupd_temfx(i,k) <= 0. .OR. zm(i,k) > hmax) then
                     hctidx(i) = k
                     goto 300   ! FORTRAN made me do it!
                  end if
               end do
            end if
   300      hct(i) = zm(i,hctidx(i))
            if (hctidx(i) <= hdidx(i)+1) then   ! No active cloud
               hct(i) = hd(i)
               hctidx(i) = hdidx(i)
            else 
            end if
         else   ! No cloud
            hct(i) = hd(i)
            hctidx(i) = hdidx(i)
         end if
         ht(i) = max(hd(i),hct(i))
         htidx(i) = max(hdidx(i),hctidx(i))

         ! Now truncate updraft at ht with taper
         do k = 1,kte
            if (zm(i,k) < 0.9*ht(i)) then  ! Below taper region
               tval(i) = 1
            else if (zm(i,k) >= 0.9*ht(i) .AND. zm(i,k) <= 1.0*ht(i)) then
               ! Within taper region
               tval(i) = 1. - ((zm(i,k) - 0.9*ht(i)) / (1.0*ht(i) - 0.9*ht(i)))
            else  ! Above taper region
               tval(i) = 0.
            end if
            thup_temfx(i,k) = tval(i) * thup_temfx(i,k) + (1-tval(i))*thetal(i,k)
            thetavUPD(i,k) = tval(i) * thetavUPD(i,k) + (1-tval(i))*thetav(i,k)
            qtup_temfx(i,k) = tval(i) * qtup_temfx(i,k) + (1-tval(i)) * qt(i,k)
            ! WA 6/21/13 was a subscript error when k=1
            if (k > 1) then
               qlUPD(i,k) = tval(i) * qlUPD(i,k) + (1-tval(i)) * qcx(i,k-1)
            end if
            UUPD(i,k) = tval(i) * UUPD(i,k) + (1-tval(i)) * u_temf(i,k)
            VUPD(i,k) = tval(i) * VUPD(i,k) + (1-tval(i)) * v_temf(i,k)
            TEUPD(i,k) = tval(i) * TEUPD(i,k) + (1-tval(i)) * te_temfx(i,k)
            if (zm(i,k) > ht(i)) then  ! WA this is just for cleanliness
               wupd_temfx(i,k) = 0.
               dwUPDmoistdz(i,k) = 0.
               wupd_dry(i,k) = 0.
               dwUPDdz(i,k) = 0.
            end if
         end do

         ! Calculate lateral detrainment rate for cloud layer
         deltmf(i,1) = Cepsmf
         do k = 2,kte-1
            if (hctidx(i) > hdidx(i)+1) then      ! Some cloud
               deltmf(i,k) = 0.9 * Cepsmf + Cdelt * (atan((zm(i,k)-(lcl(i)+(hct(i)-lcl(i))/1.5))/ &
                                                          ((hct(i)-lcl(i))/8))+(3.1415926/2))/3.1415926
            else if (k < hdidx(i)) then   ! No cloud, below hd
               deltmf(i,k) = Cepsmf + 0.05 * 1. / (hd(i) - zm(i,k))
            else if (k >= hdidx(i)) then    ! No cloud, above hd
               deltmf(i,k) = deltmf(i,k-1)
            end if
         end do

         ! Calculate mass flux (defined on turbulence levels)
         mf_temfx(i,1) = CM * wstr(i)
         do kt = 2,kte-1
            dMdz(i,kt) = (epsmf(i,kt) - deltmf(i,kt)) * mf_temfx(i,kt-1) * dzt(i,kt)
            mf_temfx(i,kt) = mf_temfx(i,kt-1) + dMdz(i,kt)
         end do

         ! WA 12/28/09 If mass flux component > diffusive 
         ! component at second level,
         ! reduce M to prevent a stable layer
         MFCth(i,2) = mf_temfx(i,2) * (thup_temfx(i,2)-thetal(i,2) + thup_temfx(i,3)-thetal(i,3)) / 2.
         if (MFCth(i,2) > Fz(i,2)) then
            red_fact = Fz(i,2) / MFCth(i,2)
            do kt = 1,kte
               mf_temfx(i,kt) = mf_temfx(i,kt) * red_fact
            end do
         end if  ! Reduce M to prevent stable layer at second level

         ! Calculate mass flux contributions to fluxes (defined on turb levels)
         ! Use log interpolation at first level
         MFCth(i,1) = mf_temfx(i,1) * (thup_temfx(i,1)-thetal(i,1) &
            + (thup_temfx(i,2)-thetal(i,2) - &
            (thup_temfx(i,1)-thetal(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))
         MFCq(i,1) = mf_temfx(i,1) * (qtup_temfx(i,1)-qt(i,1) &
            + (qtup_temfx(i,2)-qt(i,2) - &
            (qtup_temfx(i,1)-qt(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))
         MFCu(i,1) = mf_temfx(i,1) * (UUPD(i,1)-u_temf(i,1) &
            + (UUPD(i,2)-u_temf(i,2) - &
            (UUPD(i,1)-u_temf(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))
         MFCv(i,1) = mf_temfx(i,1) * (VUPD(i,1)-v_temf(i,1) &
            + (VUPD(i,2)-v_temf(i,2) - &
            (VUPD(i,1)-v_temf(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))
         MFCql(i,1) = mf_temfx(i,1) * (qlUPD(i,1)-qcx(i,1) &
            + (qlUPD(i,2)-qcx(i,2) - &
            (qlUPD(i,1)-qcx(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))
         MFCTE(i,1) = mf_temfx(i,1) * (TEUPD(i,1)-te_temfx(i,1) &
            + (TEUPD(i,2)-te_temfx(i,2) - &
            (TEUPD(i,1)-te_temfx(i,1))) * log(zt(i,1)/znt(i))/log(zm(i,2)/znt(i)))  ! WA Check this
         do kt = 2,kte-1
            MFCth(i,kt) = mf_temfx(i,kt) * (thup_temfx(i,kt)-thetal(i,kt) + thup_temfx(i,kt+1)-thetal(i,kt+1)) / 2.
            MFCq(i,kt) = mf_temfx(i,kt) * (qtup_temfx(i,kt)-qt(i,kt) + qtup_temfx(i,kt+1)-qt(i,kt+1)) / 2.
            MFCu(i,kt) = mf_temfx(i,kt) * (UUPD(i,kt)-u_temf(i,kt) + UUPD(i,kt+1)-u_temf(i,kt+1)) / 2.
            MFCv(i,kt) = mf_temfx(i,kt) * (VUPD(i,kt)-v_temf(i,kt) + VUPD(i,kt+1)-v_temf(i,kt+1)) / 2.
            MFCql(i,kt) = mf_temfx(i,kt) * (qlUPD(i,kt)-qcx(i,kt-1) + qlUPD(i,kt+1)-qcx(i,kt)) / 2.
            MFCTE(i,kt) = mf_temfx(i,kt) * (TEUPD(i,kt)-te_temfx(i,kt)) ! TE is on turb levels
         end do
         MFCth(i,kte) = 0
         MFCq(i,kte) = 0
         MFCu(i,kte) = 0
         MFCv(i,kte) = 0
         MFCql(i,kte) = 0
         MFCTE(i,kte) = 0

         ! Calculate cloud fraction (on mass levels)
         cf3d_temfx(i,1) = 0.0
         cfm_temfx(i) = 0.0
         do k = 2,kte
            ! if (wupd_temfx(i,k-1) >= 1.0e-15 .AND. wupd_temfx(i,k) >= 1.0e-15 .AND. .NOT. isnan(wupd_temfx(i,k-1)) .AND. .NOT. isnan(wupd_temfx(i,k))) then
            if (wupd_temfx(i,k-1) >= 1.0e-15 .AND. wupd_temfx(i,k) >= 1.0e-15) then
               au(i,k) = ((mf_temfx(i,k-1)+mf_temfx(i,k))/2.0) / ((wupd_temfx(i,k-1)+wupd_temfx(i,k))/2.0)  ! WA average before divide, is that best?
            else
               au(i,k) = 0.0
            end if
            sigq2 = au(i,k) * (qtup_temfx(i,k)-qt(i,k))
            if (sigq2 > 0.0) then
               sigq(i,k) = sqrt(sigq2)
            else
               sigq(i,k) = 0.0
            end if
            ! rst = rsat(p2d(i,k),thx(i,k)*pi2d(i,k),ep2)
            rst = rsat(p2d(i,k-1),thx(i,k-1)*pi2d(i,k-1),ep2)
            qst(i,k) = rst / (1. + rst)
            satdef(i,k) = qt(i,k) - qst(i,k)
            if (satdef(i,k) <= 0.0) then
               if (sigq(i,k) > 1.0e-15) then
                  cf3d_temfx(i,k) = max(0.5 + 0.36 * atan(1.55*(satdef(i,k)/sigq(i,k))),0.0)
               else
                  cf3d_temfx(i,k) = 0.0
               end if
            else
               cf3d_temfx(i,k) = 1.0
            end if
            if (zm(i,k) < lcl(i)) then
               cf3d_temfx(i,k) = 0.0
            end if
            ! Put max value so far into cfm
            if (zt(i,k) <= hmax) then
               cfm_temfx(i) = max(cf3d_temfx(i,k),cfm_temfx(i))
            end if
         end do

      else    ! not is_convective, no MF components
         do kt = 1,kte
            MFCth(i,kt) = 0
            MFCq(i,kt) = 0
            MFCu(i,kt) = 0
            MFCv(i,kt) = 0
            MFCql(i,kt) = 0
            MFCTE(i,kt) = 0
         end do
         lcl(i) = zm(i,kte-1)
         hct(i) = zm(i,1)
         hctidx(i) = 1
         hd(i) = zm(i,1)
         hdidx(i) = 1
         ht(i) = hd(i)
         ! Cloud fraction calculations
         cf3d_temfx(i,1) = 0.0
         cfm_temfx(i) = 0.0
         do k = 2,kte
            if (qcx(i,k-1) > 1.0e-15) then
               cf3d_temfx(i,k) = 1.0
            else
               cf3d_temfx(i,k) = 0.0
            end if
            ! Put max value so far into cfm
            if (zt(i,k) <= hmax) then
               cfm_temfx(i) = max(cf3d_temfx(i,k),cfm_temfx(i))
            end if
         end do

      end if   ! MF components or not
      cf3d_temfx(i,kte) = 0.0
      ! Mass flux block ends here

      ! Flux profiles
      do kt = 2,kte
         ! Fz(i,kt) = -kh(i,kt) * dthdz(i,kt)
         shf_temfx(i,kt) = Fz(i,kt) + MFCth(i,kt)
         QFK(i,kt) = -kh(i,kt) * dqtdz(i,kt)
         qf_temfx(i,kt) = QFK(i,kt) + MFCq(i,kt)
         uwk(i,kt) = -km(i,kt) * dudz(i,kt)
         uw_temfx(i,kt) = uwk(i,kt) + MFCu(i,kt)
         vwk(i,kt) = -km(i,kt) * dvdz(i,kt)
         vw_temfx(i,kt) = vwk(i,kt) + MFCv(i,kt)
      end do

      ! Surface momentum fluxes
      ! WA TEST 11/7/13 use w* as a component of the mean wind inside the
      ! u* calculation instead of in the velocity scale below (Felix)
      ! ust(i) = sqrt(ftau(i,1)/ftau0) * sqrt(u_temf(i,2)**2. + v_temf(i,2)**2.) * leps(i,1) / log(zm(i,2)/znt(i)) / zt(i,1)
      ust(i) = sqrt(ftau(i,1)/ftau0) * sqrt(u_temf(i,2)**2. + v_temf(i,2)**2. + (0.5*wstr(i))**2.) * leps(i,1) / log(zm(i,2)/znt(i)) / zt(i,1)

      ang(i) = atan2(v_temf(i,2),u_temf(i,2))
      uw_temfx(i,1) = -cos(ang(i)) * ust(i)**2.
      vw_temfx(i,1) = -sin(ang(i)) * ust(i)**2.

      ! Calculate mixed scaling velocity (Moeng & Sullivan 1994 JAS p.1021)
      ! Replaces ust everywhere
      ! WA TEST 11/7/13 back to wm = u* but with "whole" wind in u* above
      wm(i) = ust(i)
      ! WA 7/23/10 reduce velocity scale to fix excessive fluxes
      ! wm(i) = 0.5 * (1./5. * (wstr(i)**3. + 5. * ust(i)**3.)) ** (1./3.) 

      ! Specified flux versions (flux is modified by land surface)
      ! WA 5/31/13 use whole surface flux to improve heat conservation
      shf_temfx(i,1) = hfx(i)/(rho(i,1)*cp)
      qf_temfx(i,1) = qfx(i)/rho(i,1)
      Fz(i,1) = shf_temfx(i,1) - MFCth(i,1)
      QFK(i,1) = qf_temfx(i,1) - MFCq(i,1)

      ! Calculate thetav and its flux
      ! From Lewellen 2004 eq. 3
      ! WA The constants and combinations below should probably be replaced
      ! by something more consistent with the WRF system, but for now
      ! I don't want to take the chance of breaking something.
      do kt = 2,kte-1
         alpha2(i,kt) = 0.61 * (thetal(i,kt) + thetal(i,kt+1)) / 2.
         beta2(i,kt) = (100000. / p2di(i,kt))**0.286 * 2.45e-6 / 1004.67 - 1.61 * (thetal(i,kt) + thetal(i,kt+1)) / 2.
      end do
      alpha2(i,1) = 0.61 * (thetal(i,1) + (thetal(i,2)-thetal(i,1)) * (zt(i,2) - znt(i)) / (zm(i,2) - znt(i)))
      alpha2(i,kte) = 0.61 * thetal(i,kte)
      beta2(i,1) = (100000. / p2di(i,1))**0.286 * 2.45e-6 / &
         1004.67 - 1.61 * (thetal(i,1) + (thetal(i,2) - thetal(i,1)) &
         * (zt(i,2) - znt(i)) / (zm(i,2) - znt(i)))
      beta2(i,kte) = (100000. / p2di(i,kte))**0.286 * 2.45e-6 / 1004.67 - 1.61 * thetal(i,kte)
      if ( is_convective ) then ! Activate EDMF components
         do kt = 1,kte-1
            MFCthv(i,kt) = (1. + 0.61 * (qtup_temfx(i,kt)+qtup_temfx(i,kt+1))) / 2. * MFCth(i,kt) + &
                           alpha2(i,kt) * MFCq(i,kt) + beta2(i,kt) * MFCql(i,kt)
         end do
         MFCthv(i,kte) = 0.
      else    ! No MF components
         do kt = 1,kte
            MFCthv(i,kt) = 0.
         end do
      end if

      do kt = 1,kte
         THVF(i,kt) = (1. + 0.61 * qt(i,kt)) * Fz(i,kt) + alpha2(i,kt) * QFK(i,kt) + MFCthv(i,kt)
      end do

      ! Update mean variables:
      ! This is done with implicit solver for diffusion part followed
      ! by explicit solution for MF terms.
      ! Note that Coriolis terms that were source terms for u and v
      ! in Matlab code are now handled by WRF outside this PBL context.

      u_new(i,:) = u_temf(i,:)
      call solve_implicit_temf(km(i,kts:kte-1),u_new(i,kts+1:kte), &
         uw_temfx(i,1),dzm(i,kts:kte-1),dzt(i,kts:kte-1),kts,kte-1,dt,.FALSE.)
      do k = 2,kte-1
         u_new(i,k) = u_new(i,k) + dt * (-(MFCu(i,k)-MFCu(i,k-1))) / dzm(i,k)
      end do

      v_new(i,:) = v_temf(i,:)
      call solve_implicit_temf(km(i,kts:kte-1),v_new(i,kts+1:kte), &
         vw_temfx(i,1),dzm(i,kts:kte-1),dzt(i,kts:kte-1),kts,kte-1,dt,.FALSE.)
      do k = 2,kte-1
         v_new(i,k) = v_new(i,k) + dt * (-(MFCv(i,k)-MFCv(i,k-1))) / dzm(i,k)
      end do

      call solve_implicit_temf(kh(i,kts:kte-1),thetal(i,kts+1:kte),Fz(i,1),dzm(i,kts:kte-1),&
                               dzt(i,kts:kte-1),kts,kte-1,dt,.FALSE.)
      do k = 2,kte-1
         thetal(i,k) = thetal(i,k) + dt * (-(MFCth(i,k)-MFCth(i,k-1))) / dzm(i,k)
      end do

      call solve_implicit_temf(kh(i,kts:kte-1),qt(i,kts+1:kte),QFK(i,1),dzm(i,kts:kte-1),&
                               dzt(i,kts:kte-1),kts,kte-1,dt,.FALSE.)
      do k = 2,kte-1
         qt(i,k) = qt(i,k) + dt * (-(MFCq(i,k)-MFCq(i,k-1))) / dzm(i,k)
      end do

      ! Stepping TE forward is a bit more complicated
      te_temfx(i,1) = ust(i)**2. / ftau(i,1) * (1. + ratio(i,1))
      if ( is_convective ) then
         ! WA currently disabled if MFopt=false, is that right?
         convection_TKE_surface_src(i) = 2. * beta(i,1) * shf_temfx(i,1)
      else
         convection_TKE_surface_src(i) = 0.
      end if
      te_temfx(i,1) = max(te_temfx(i,1), &
                          (leps(i,1) / Cgamma * (ust(i)**2. * S(i,1) + convection_TKE_surface_src(i)))**(2./3.))
      if (te_temfx(i,1) > 20.0) then
         te_temfx(i,1) = 20.0    ! WA 9/28/11 limit max TE
      end if
      sfcFTE(i) = -(km(i,1)+km(i,2)) / 2. * (te_temfx(i,2)-te_temfx(i,1)) / dzm(i,2)

      do kt = 1,kte
         if (THVF(i,kt) >= 0) then
            buoy_src(i,kt) = 2. * g / thetav(i,kt) * THVF(i,kt)
         else
            buoy_src(i,kt) = 0.  ! Cancel buoyancy term when locally stable
         end if
         srcs(i,kt) = -uw_temfx(i,kt) * dudz(i,kt) - vw_temfx(i,kt) * dvdz(i,kt) - &
                      Cgamma * te_temfx(i,kt)**1.5 * linv(i,kt) + buoy_src(i,kt)
      end do
      call solve_implicit_temf((km(i,kts:kte-1)+km(i,kts+1:kte))/2.0, &
         te_temfx(i,kts+1:kte),sfcFTE(i),dzt(i,kts+1:kte),dzt(i,kts:kte-1),kts,kte-1,dt,.false.)
      do kt = 2,kte-1
         te_temfx(i,kt) = te_temfx(i,kt) + dt * srcs(i,kt)
         te_temfx(i,kt) = te_temfx(i,kt) + dt * (-(MFCTE(i,kt)-MFCTE(i,kt-1))) / dzt(i,kt)
         if (te_temfx(i,kt) < TEmin) te_temfx(i,kt) = TEmin
      end do
      te_temfx(i,kte) = 0.0
      do kt = 2,kte-1
         if (te_temfx(i,kt) > 20.0) then
            te_temfx(i,kt) = 20.0    ! WA 9/29/11 reduce limit max TE from 30
         end if
      end do

      ! Done with updates, now convert internal variables back to WRF vars
      do k = kts,kte
         ! Populate kh_temfx, km_temfx from kh, km
         kh_temfx(i,k) = kh(i,k)
         km_temfx(i,k) = km(i,k)
      end do

      ! Convert thetal, qt back to theta, qv, qc
      ! See opposite conversion at top of subroutine
      ! WA this accounts for offset of indexing between
      ! WRF and TEMF, see notes at top of this routine.
      call thlqt2thqvqc(thetal(i,kts+1:kte),qt(i,kts+1:kte), &
         thx_new(i,kts:kte-1),qvx_new(i,kts:kte-1),qcx_new(i,kts:kte-1), &
         p2d(i,kts:kte-1),pi2d(i,kts:kte-1),kts,kte-1,ep2,xlv,cp)

      do k = kts,kte-1
         ! Calculate tendency terms
         ! WA this accounts for offset of indexing between
         ! WRF and TEMF, see notes at top of this routine.
         rubltenx(i,k) = (u_new(i,k+1) - u_temf(i,k+1)) / dt
         rvbltenx(i,k) = (v_new(i,k+1) - v_temf(i,k+1)) / dt
         rthbltenx(i,k) = (thx_new(i,k) - thx(i,k)) / dt
         rqvbltenx(i,k) = (qvx_new(i,k) - qvx(i,k)) / dt
         rqcbltenx(i,k) = (qcx_new(i,k) - qcx(i,k)) / dt
      end do
      rubltenx(i,kte) = 0.
      rvbltenx(i,kte) = 0.
      rthbltenx(i,kte) = 0.
      rqvbltenx(i,kte) = 0.
      rqcbltenx(i,kte) = 0.

      ! Populate surface exchange coefficient variables to go back out
      ! for next time step of surface scheme
      ! WA 2/16/11 removed, not needed in BL

      ! Populate 10 m winds and 2 m temp
      ! WA Note this only works if first mass level is above 10 m
      u10(i) = u_new(i,2) * log(10.0/znt(i)) / log(zm(i,2)/znt(i))
      v10(i) = v_new(i,2) * log(10.0/znt(i)) / log(zm(i,2)/znt(i))
      t2(i) = (tsk(i)/pi2d(i,1) + (thx_new(i,1) - tsk(i)/pi2d(i,1)) * log(2.0/z0t(i)) / log(zm(i,2)/z0t(i))) * pi2d(i,1)  ! WA this should also use pi at z0

      ! Populate diagnostic variables
      hd_temfx(i) = hd(i)
      lcl_temfx(i) = lcl(i)
      hct_temfx(i) = hct(i)

      ! Send updraft liquid water back
      if ( is_convective) then
         do k = kts,kte-1
            qlup_temfx(i,k) = qlUPD(i,k)
         end do
      else
         qlup_temfx(i,1) = qcx(i,1)
         do k = kts+1,kte-1
            qlup_temfx(i,k) = qcx(i,k-1)
         end do
      end if
      qlup_temfx(i,kte) = qcx(i,kte)

   end do  ! Main (i) loop

   end subroutine temf2d
!
!--------------------------------------------------------------------
!
   subroutine thlqt2thqvqc(thetal,qt,theta,qv,qc,p,piex,kbot,ktop,ep2,Lv,Cp)

!  Calculates theta, qv, qc from thetal, qt.
!  Originally from RAMS (subroutine satadjst) by way of Hongli Jiang.

   implicit none
   integer, intent(in   ) :: kbot, ktop
   real,    dimension( kbot:ktop ), intent(in   ) :: thetal, qt
   real,    dimension( kbot:ktop ), intent(  out) :: theta, qv, qc
   real,    dimension( kbot:ktop ), intent(in   ) :: p, piex
   real,    intent(in   ) :: ep2, Lv, Cp
!
!  Local variables
   integer :: k, iterate
   real :: T1, Tt
   real, dimension( kbot:ktop) :: rst
   real, dimension( kbot:ktop) :: Tair, rc, rt, rv
!
   do k = kbot,ktop
      T1 = thetal(k) * piex(k)   ! First guess T is just thetal converted to T
      Tair(k) = T1
      rt(k) = qt(k) / (1. - qt(k))

      do iterate = 1,20
         rst(k) = rsat(p(k),Tair(k),ep2)
         rc(k) = max(rt(k) - rst(k), 0.)
         Tt = 0.7*Tair(k) + 0.3*T1 * (1.+Lv*rc(k) / (Cp*max(Tair(k),253.)))
         if ( abs(Tt - Tair(k)) < 0.001) GOTO 100
         Tair(k) = Tt
      end do
100   continue
      rv(k) = rt(k) - rc(k)
      qv(k) = rv(k) / (1. + rv(k))
      qc(k) = rc(k) / (1. + rc(k))
      theta(k) = Tair(k) / piex(k)
   end do ! k loop
   return
   end subroutine thlqt2thqvqc
!
!--------------------------------------------------------------------
!
   subroutine findhct_te( thetavenv,thetaparin,qpar, &
                          rpar,hdidx,paridx,zm,hct,hctidx,p,piex,ep2,kbot,ktop)

! Calculates the cloud top height (limit of convection) for the
! updraft properties.  hct is the level at which a parcel lifted
! at the moist adiabatic rate where it is saturated and at the dry
! adiabatic rate otherwise, first has thetav cooler than the environment.
! Loops start at LCL (paridx).

   implicit none
   integer, intent(in) :: kbot, ktop
   integer, intent(in) :: paridx, hdidx
   real, intent(in)    :: ep2
   real, dimension( kbot:ktop), intent(in) :: thetavenv
   real, dimension( kbot:ktop), intent(in) :: thetaparin
   real, dimension( kbot:ktop), intent(in) :: qpar, rpar, zm, p, piex
   real, intent(out)    :: hct
   integer, intent(out) :: hctidx
! Local variables
   integer k
   real, dimension( kbot:ktop) :: thetapar, thetavpar, qlpar, Tpar, rsatpar
   real, dimension( kbot:ktop) :: qsatpar
   real :: gammas, TparC

   thetapar(paridx) = thetaparin(paridx)
   Tpar(paridx) = thetapar(paridx) * piex(paridx)
   hctidx = ktop   ! In case hct not found
   do k = paridx+1,ktop
      ! Find saturation mixing ratio at parcel temperature
      rsatpar(k) = rsat(p(k-1),Tpar(k-1),ep2)
      qsatpar(k) = rsatpar(k) / (1. + rsatpar(k))

      ! When parcel is unsaturated, its temperature changes 
      ! at dry adiabatic rate (in other words, theta is constant).
      if (rpar(k) <= rsatpar(k)) then
         thetapar(k) = thetapar(k-1)
         Tpar(k) = thetapar(k) * piex(k)
         thetavpar(k) = thetapar(k) * (1.+0.608*qpar(k))
      else
         ! When parcel is saturated, its temperature changes at
         ! moist adiabatic rate
         ! Calculate moist adiabatic lapse rate according to Gill A4.12
         ! Requires T in deg.C
         TparC = Tpar(k-1) - 273.15
         gammas = 6.4 - 0.12 * TparC + 2.5e-5 * TparC**3. + (-2.4 + 1.e-3 * (TparC-5.)**2.) * (1. - p(k-1)/100000.)
         Tpar(k) = Tpar(k-1) - gammas/1000. * (zm(k)-zm(k-1))
         thetapar(k) = Tpar(k) / piex(k)
         qlpar(k) = qpar(k) - qsatpar(k)  ! Liquid water in parcel
         thetavpar(k) = thetapar(k) * (1. + 0.608 * qsatpar(k) - qlpar(k))
      end if
      if (thetavenv(k) >= thetavpar(k)) then
         hctidx = k
         goto 1000
      end if
   end do
1000  hct = zm(hctidx)
   return
   end subroutine findhct_te
!
!--------------------------------------------------------------------
!
   real function rsat(p,T,ep2)

!  Calculates the saturation mixing ratio with respect to liquid water
!  Arguments are pressure (Pa) and absolute temperature (K)
!  Uses the formula from the ARM intercomparison setup.
!  Converted from Matlab by WA 7/28/08

implicit none
real p, T, ep2
real temp, x
real, parameter :: c0 = 0.6105851e+3
real, parameter :: c1 = 0.4440316e+2
real, parameter :: c2 = 0.1430341e+1
real, parameter :: c3 = 0.2641412e-1
real, parameter :: c4 = 0.2995057e-3
real, parameter :: c5 = 0.2031998e-5
real, parameter :: c6 = 0.6936113e-8
real, parameter :: c7 = 0.2564861e-11
real, parameter :: c8 = -0.3704404e-13

temp = T - 273.15

x =c0+temp*(c1+temp*(c2+temp*(c3+temp*(c4+temp*(c5+temp*(c6+temp*(c7+temp*c8)))))))
rsat = ep2*x/(p-x)

return
end function rsat
!
!--------------------------------------------------------------------
!
   subroutine solve_implicit_temf(Khlf,psi_n,srf_flux,dzm,dzt,kbot,ktop,dt,print_flag)

!  Implicit solution of vertical diffusion for conserved variable
!  psi given diffusivity Khlf on turbulence levels,
!  and surface flux srf_flux.
!  dzm is delta_z of mass levels, dzt is delta_z of turbulence levels.
!  dt is timestep (s).

   implicit none
   integer  :: kbot, ktop
   logical  :: print_flag
   real :: srf_flux, dt
   real,    dimension( kbot:ktop ), intent(in   ) :: Khlf
   real,    dimension( kbot:ktop ), intent(in   ) :: dzm, dzt
   real,    dimension( kbot:ktop ), intent(inout) :: psi_n
!
!  Local variables
   integer :: k
   real,    dimension( kbot:ktop ) :: AU, BU, CU, YU
!
   AU(kbot) = Khlf(kbot) / (dzm(kbot)*dzt(kbot))
   BU(kbot) = -1.0/dt - Khlf(kbot+1)/(dzm(kbot+1)*dzt(kbot+1))
   CU(kbot) = Khlf(kbot+1)/(dzm(kbot)*dzt(kbot+1))
   YU(kbot) = -psi_n(kbot)/dt - srf_flux/dzm(kbot)

   do k = kbot+1,ktop-1
      ! Subdiagonal (A) vector
      AU(k) = Khlf(k) / (dzm(k) * dzt(k))
      ! Main diagonal (B) vector
      BU(k) = -1.0/dt - (Khlf(k)/dzt(k) + Khlf(k+1)/dzt(k+1)) / dzm(k)
      ! Superdiagonal (C) vector
      CU(k) = Khlf(k+1) / (dzm(k)*dzt(k+1))
      ! Result vector
      YU(k) = -psi_n(k)/dt
   end do ! k loop

   AU(ktop) = 0.
   BU(ktop) = -1.0 / dt
   YU(ktop) = -psi_n(ktop) / dt

   ! Compute result with tridiagonal routine
   psi_n = trid(AU,BU,CU,YU,kbot,ktop)

   return
   end subroutine solve_implicit_temf
!
!--------------------------------------------------------------------
!
   function trid(a,b,c,r,kbot,ktop)

!  Solves tridiagonal linear system.
!  Inputs are subdiagonal vector a, main diagonal b, superdiagonal c,
!  result r, column top and bottom indices kbot and ktop.
!  Originally from Numerical Recipes section 2.4.

   implicit none
   real,    dimension( kbot:ktop ) :: trid
   integer  :: kbot, ktop
   real,    dimension( kbot:ktop ), intent(in   ) :: a, b, c, r
!
!  Local variables
   integer :: k
   real    :: bet
   real,    dimension( kbot:ktop ) :: gam, u
!
   bet = b(kbot)
   u(kbot) = r(kbot) / bet

   do k = kbot+1,ktop
      gam(k) = c(k-1) / bet
      bet = b(k) - a(k)*gam(k)
      u(k) = (r(k) - a(k)*u(k-1)) / bet
   end do

   do k = ktop-1,kbot,-1
      u(k) = u(k) - gam(k+1)*u(k+1)
   end do

   trid = u

   return
   end function trid
!
!--------------------------------------------------------------------
!
   subroutine temfinit(rublten,rvblten,rthblten,rqvblten,                      &
                      rqcblten,rqiblten,p_qi,p_first_scalar,                   &
                      restart, allowed_to_read,                                &
                      te_temf, cf3d_temf,                                     &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------
!
   logical , intent(in)          :: restart, allowed_to_read
   integer , intent(in)          ::  ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
   integer , intent(in)          ::  p_qi,p_first_scalar
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten, &
                                                                     te_temf, &
                                                                     cf3d_temf
! Local variables
   integer :: i, j, k, itf, jtf, ktf
   real, parameter :: TEmin = 1e-3
!
   jtf = min0(jte,jde-1)
   ktf = min0(kte,kde-1)
   itf = min0(ite,ide-1)
!
   if(.not.restart)then
     do j = jts,jtf
     do k = kts,ktf
     do i = its,itf
        rublten(i,k,j) = 0.
        rvblten(i,k,j) = 0.
        rthblten(i,k,j) = 0.
        rqvblten(i,k,j) = 0.
        rqcblten(i,k,j) = 0.
        te_temf(i,k,j) = TEmin
        cf3d_temf(i,k,j) = 0.
     enddo
     enddo
     enddo
   endif
!
   if (p_qi .ge. p_first_scalar .and. .not.restart) then
      do j = jts,jtf
      do k = kts,ktf
      do i = its,itf
         rqiblten(i,k,j) = 0.
      enddo
      enddo
      enddo
   endif
!
   end subroutine temfinit
!-------------------------------------------------------------------
end module module_bl_temf

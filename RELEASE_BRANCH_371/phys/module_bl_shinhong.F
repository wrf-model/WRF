!WRF:model_layer:physics
!
module module_bl_shinhong
!
   USE MODULE_MODEL_CONSTANTS
!
!-----------------------------------------------------------------------
!
      INTEGER :: ITRMX=5 ! ITERATION COUNT FOR MIXING LENGTH COMPUTATION
      REAL,PARAMETER :: PI=3.1415926,VKARMAN=0.4
!
!-----------------------------------------------------------------------
!***  QNSE MODEL CONSTANTS
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: EPSQ2L=0.01
      REAL,PARAMETER :: C0=0.55,CEPS=C0**3,BLCKDR=0.0063,CN=0.75        &
     &                 ,AM1=8.0,AM2=2.3,AM3=35.0,AH1=1.4,AH2=-0.01      &
     &                 ,AH3=1.29,AH4=2.44,AH5=19.8                      &
     &                 ,ARIMIN=0.127,BM1=2.88,BM2=16.0,BH1=3.6,BH2=16.0 &
     &                 ,BH3=720.0,EPSKM=1.E-3
      REAL,PARAMETER :: CAPA=R_D/CP
      REAL,PARAMETER :: RLIVWV=XLS/XLV,ELOCP=2.72E6/CP
      REAL,PARAMETER :: EPS1=1.E-12,EPS2=0.
      REAL,PARAMETER :: EPSL=0.32,EPSRU=1.E-7,EPSRS=1.E-7               &
     &                 ,EPSTRB=1.E-24
      REAL,PARAMETER :: EPSA=1.E-8,EPSIT=1.E-4,EPSU2=1.E-4,EPSUST=0.07
      REAL,PARAMETER :: ALPH=0.30,BETA=1./273.,EL0MAX=1000.,EL0MIN=1.   &
     &                 ,ELFC=0.23*0.5,GAM1=0.2222222222222222222        &
     &                 ,PRT=1.
      REAL,PARAMETER :: A1=0.659888514560862645                         &
     &                 ,A2X=0.6574209922667784586                       &
     &                 ,B1=11.87799326209552761                         &
     &                 ,B2=7.226971804046074028                         &
     &                 ,C1=0.000830955950095854396
      REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
      REAL,PARAMETER :: ELZ0=0.,ESQ=5.0,EXCM=0.001                      &
     &                 ,FHNEU=0.8,GLKBR=10.,GLKBS=30.                   &
     &                 ,QVISC=2.1E-5,RFC=0.191,RIC=0.505,SMALL=0.35     &
     &                 ,SQPR=0.84,SQSC=0.84,SQVISC=258.2,TVISC=2.1E-5   &
     &                 ,USTC=0.7,USTR=0.225,VISC=1.5E-5                 &
     &                 ,WOLD=0.15,WWST=1.2,ZTMAX=1.,ZTFC=1.,ZTMIN=-5.
!
      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC
!
      REAL,PARAMETER :: BTG=BETA*G,CZIV=SMALL*GLKBS                     &
     &                 ,ESQHF=0.5*5.0,GRRS=GLKBR/GLKBS                  &
     &                 ,RB1=1./B1,RTVISC=1./TVISC,RVISC=1./VISC         &
     &                 ,ZQRZT=SQSC/SQPR
!
      REAL,PARAMETER :: ADNH= 9.*A1*A2X*A2X*(12.*A1+3.*B2)*BTG*BTG      &
     &                 ,ADNM=18.*A1*A1*A2X*(B2-3.*A2X)*BTG              &
     &                 ,ANMH=-9.*A1*A2X*A2X*BTG*BTG                     &
     &                 ,ANMM=-3.*A1*A2X*(3.*A2X+3.*B2*C1+18.*A1*C1-B2)  &
     &                                *BTG                              &
     &                 ,BDNH= 3.*A2X*(7.*A1+B2)*BTG                     &
     &                 ,BDNM= 6.*A1*A1                                  &
     &                 ,BEQH= A2X*B1*BTG+3.*A2X*(7.*A1+B2)*BTG          &
     &                 ,BEQM=-A1*B1*(1.-3.*C1)+6.*A1*A1                 &
     &                 ,BNMH=-A2X*BTG                                   &
     &                 ,BNMM=A1*(1.-3.*C1)                              &
     &                 ,BSHH=9.*A1*A2X*A2X*BTG                          &
     &                 ,BSHM=18.*A1*A1*A2X*C1                           &
     &                 ,BSMH=-3.*A1*A2X*(3.*A2X+3.*B2*C1+12.*A1*C1-B2)  &
     &                                *BTG                              &
     &                 ,CESH=A2X                                        &
     &                 ,CESM=A1*(1.-3.*C1)                              &
     &                 ,CNV=EP_1*G/BTG                                  &
     &                 ,ELFCS=VKARMAN*BTG                               &
     &                 ,FZQ1=RTVISC*QVISC*ZQRZT                         &
     &                 ,FZQ2=RTVISC*QVISC*ZQRZT                         &
     &                 ,FZT1=RVISC *TVISC*SQPR                          &
     &                 ,FZT2=CZIV*GRRS*TVISC*SQPR                       &
     &                 ,FZU1=CZIV*VISC                                  &
     &                 ,PIHF=0.5*PI                                     &
     &                 ,RFAC=RIC/(FHNEU*RFC*RFC)                        &
     &                 ,RQVISC=1./QVISC                                 &
     &                 ,RRIC=1./RIC                                     &
     &                 ,USTFC=0.018/G                                   &
     &                 ,WNEW=1.-WOLD                                    &
     &                 ,WWST2=WWST*WWST
!
!-----------------------------------------------------------------------
!***  FREE TERM IN THE EQUILIBRIUM EQUATION FOR (L/Q)**2
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: AEQH=9.*A1*A2X*A2X*B1*BTG*BTG                   &
     &                      +9.*A1*A2X*A2X*(12.*A1+3.*B2)*BTG*BTG       &
     &                 ,AEQM=3.*A1*A2X*B1*(3.*A2X+3.*B2*C1+18.*A1*C1-B2)&
     &                      *BTG+18.*A1*A1*A2X*(B2-3.*A2X)*BTG
!
!-----------------------------------------------------------------------
!***  FORBIDDEN TURBULENCE AREA
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: REQU=-AEQH/AEQM                                 &
     &                 ,EPSGH=1.E-9,EPSGM=REQU*EPSGH
!
!-----------------------------------------------------------------------
!***  NEAR ISOTROPY FOR SHEAR TURBULENCE, WW/Q2 LOWER LIMIT
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: UBRYL=(18.*REQU*A1*A1*A2X*B2*C1*BTG             &
     &                         +9.*A1*A2X*A2X*B2*BTG*BTG)               &
     &                        /(REQU*ADNM+ADNH)                         &
     &                 ,UBRY=(1.+EPSRS)*UBRYL,UBRY3=3.*UBRY
!
      REAL,PARAMETER :: AUBH=27.*A1*A2X*A2X*B2*BTG*BTG-ADNH*UBRY3       &
     &                 ,AUBM=54.*A1*A1*A2X*B2*C1*BTG -ADNM*UBRY3        &
     &                 ,BUBH=(9.*A1*A2X+3.*A2X*B2)*BTG-BDNH*UBRY3       &
     &                 ,BUBM=18.*A1*A1*C1           -BDNM*UBRY3         &
     &                 ,CUBR=1.                     -     UBRY3         &
     &                 ,RCUBR=1./CUBR
!
!-----------------------------------------------------------------------
!
contains
!
!-------------------------------------------------------------------------------
!
   subroutine shinhong(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,          &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qi,                          &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znu,znw,mut,p_top,                                           &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,                                                      &
                  u10,v10,                                                     &
                  ctopo,ctopo2,                                                &
                  shinhong_tke_diag,tke_pbl,el_pbl,corf,                       &
                  dx,dy,                                                       &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  wstar,delta,                                                 &
                  regime                                           )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rr3d        3d dry air density (kg/m^3)
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
!-- rd          gas constant for dry air (j/kg/k)
!-- rovg        r/g
!-- dz8w        dz between full levels (m)
!-- xlv         latent heat of vaporization (j/kg)
!-- rv          gas constant for water vapor (j/kg/k)
!-- psfc        pressure at the surface (pa)
!-- znt         roughness length (m)
!-- ust	        u* in similarity theory (m/s)
!-- hpbl        pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx	        upward heat flux at the surface (w/m^2)
!-- qfx	        upward moisture flux at the surface (kg/m^2/s)
!-- wspd        wind speed at lowest model level (m/s)
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- br          bulk richardson number in surface layer
!-- dt          time step (s)
!-- rvovrd      r_v divided by r_d (dimensionless)
!-- ep1         constant for virtual temperature (r_v/r_d - 1)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
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
!-------------------------------------------------------------------------------
!
   integer,parameter ::  ndiff = 3
   real,parameter    ::  rcl = 1.0
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
   integer,  intent(in)      ::      shinhong_tke_diag
!
   real,     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::      ep1,ep2,karman
   real,     intent(in )     ::      dx,dy
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         th3d, &
                                                                          t3d, &
                                                                         dz8w
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          p3di
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                        exch_h
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       tke_pbl, &
                                                                       el_pbl
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                         corf, &
                                                                           br, &
                                                                         psfc
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           znt, &
                                                                          ust, &
                                                                         hpbl, &
                                                                         wspd
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
   logical,  intent(in)      ::                                       flag_qi
!
!optional
!
   real,     dimension( ims:ime, jms:jme )                                   , &
              intent(inout), optional   ::                              wstar
   real,     dimension( ims:ime, jms:jme )                                   , &
              intent(inout), optional   ::                              delta
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(inout)   ::                                        regime
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             optional                                                        , &
             intent(inout)   ::                                       rqiblten
!
   real,     dimension( kms:kme )                                            , &
             optional                                                        , &
             intent(in   )   ::                                           znu, &
                                                                          znw
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                           mut
!
   real,     optional, intent(in   )   ::                               p_top
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!local
   integer ::  i,j,k
   real,     dimension( its:ite, kts:kte*ndiff )  ::                 rqvbl2dt, &
                                                                         qv2d
   real,     dimension( its:ite, kts:kte )  ::                            pdh
   real,     dimension( its:ite, kts:kte+1 )  ::                         pdhi
   real,     dimension( its:ite )  ::                                          &
                                                                        dusfc, &
                                                                        dvsfc, &
                                                                        dtsfc, &
                                                                        dqsfc
!
   qv2d(its:ite,:) = 0.0
!
   do j = jts,jte
     if(present(mut))then
!
! For ARW we will replace p and p8w with dry hydrostatic pressure
!
       do k = kts,kte+1
         do i = its,ite
           if(k.le.kte)pdh(i,k) = mut(i,j)*znu(k) + p_top
           pdhi(i,k) = mut(i,j)*znw(k) + p_top
         enddo
       enddo
     else
       do k = kts,kte+1
         do i = its,ite
           if(k.le.kte)pdh(i,k) = p3d(i,k,j)
           pdhi(i,k) = p3di(i,k,j)
         enddo
       enddo
     endif
     do k = kts,kte
       do i = its,ite
         qv2d(i,k) = qv3d(i,k,j)
         qv2d(i,k+kte) = qc3d(i,k,j)
         if(present(rqiblten)) qv2d(i,k+kte+kte) = qi3d(i,k,j)
       enddo
     enddo
!
     call shinhong2d(J=j,ux=u3d(ims,kms,j),vx=v3d(ims,kms,j)                   &
             ,tx=t3d(ims,kms,j)                                                &
             ,qx=qv2d(its,kts)                                                 &
             ,p2d=pdh(its,kts),p2di=pdhi(its,kts)                              &
             ,pi2d=pi3d(ims,kms,j)                                             &
             ,utnp=rublten(ims,kms,j),vtnp=rvblten(ims,kms,j)                  &
             ,ttnp=rthblten(ims,kms,j),qtnp=rqvbl2dt(its,kts),ndiff=ndiff      &
             ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                            &
             ,xlv=xlv,rv=rv                                                    &
             ,ep1=ep1,ep2=ep2,karman=karman                                    &
             ,dz8w2d=dz8w(ims,kms,j)                                           &
             ,psfcpa=psfc(ims,j),znt=znt(ims,j),ust=ust(ims,j)                 &
             ,hpbl=hpbl(ims,j)                                                 &
             ,regime=regime(ims,j),psim=psim(ims,j)                            &
             ,psih=psih(ims,j),xland=xland(ims,j)                              &
             ,hfx=hfx(ims,j),qfx=qfx(ims,j)                                    &
             ,wspd=wspd(ims,j),br=br(ims,j)                                    &
             ,dusfc=dusfc,dvsfc=dvsfc,dtsfc=dtsfc,dqsfc=dqsfc                  &
             ,dt=dt,rcl=1.0,kpbl1d=kpbl2d(ims,j)                               &
             ,exch_hx=exch_h(ims,kms,j)                                        &
             ,wstar=wstar(ims,j)                                               &
             ,delta=delta(ims,j)                                               &
             ,u10=u10(ims,j),v10=v10(ims,j)                                    &
             ,ctopo=ctopo(ims,j),ctopo2=ctopo2(ims,j)                          &
             ,shinhong_tke_diag=shinhong_tke_diag                              &
             ,tke=tke_pbl(ims,kms,j),el_pbl=el_pbl(ims,kms,j)                  &
             ,corf=corf(ims,j)                                                 &
             ,dx=dx,dy=dy                                                      &
             ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde                &
             ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme                &
             ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
!
     do k = kts,kte
       do i = its,ite
         rthblten(i,k,j) = rthblten(i,k,j)/pi3d(i,k,j)
         rqvblten(i,k,j) = rqvbl2dt(i,k)
         rqcblten(i,k,j) = rqvbl2dt(i,k+kte)
         if(present(rqiblten)) rqiblten(i,k,j) = rqvbl2dt(i,k+kte+kte)
       enddo
     enddo
!
   enddo
!
   end subroutine shinhong
!
!-------------------------------------------------------------------------------
!
   subroutine shinhong2d(j,ux,vx,tx,qx,p2d,p2di,pi2d,                          &
                  utnp,vtnp,ttnp,qtnp,ndiff,                                   &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w2d,psfcpa,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dusfc,dvsfc,dtsfc,dqsfc,                                     &
                  dt,rcl,kpbl1d,                                               &
                  exch_hx,                                                     &
                  wstar,delta,                                                 &
                  shinhong_tke_diag,                                           &
                  tke,el_pbl,corf,                                             &
                  u10,v10,                                                     &
                  ctopo,ctopo2,                                                &
                  dx,dy,                                                       &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  regime )
!-------------------------------------------------------------------------------
!  use module_shinhong_tke, only: mixlen, prodq2, vdifq, epsq2l
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!     the shinhongpbl (shin and hong 2015) is based on the les study of shin
!     and hong (2013). the major ingredients of the shinhongpbl are
!       1) the prescribed nonlocal heat transport profile fit to the les and 
!       2) inclusion of explicit scale dependency functions for vertical 
!          transport in convective pbl. 
!     so, the shinhongpbl works at the gray zone resolution of convective pbl.
!     note that honnert et al. (2011) first suggested explicit scale dependency
!     function, and shin and hong (2013) further classified the function by
!     stability (u*/w*) in convective pbl and calculated the function for 
!     nonlocal and local transport separately.
!     vertical mixing in the stable boundary layer and free atmosphere follows
!     hong (2010) and hong et al. (2006), same as the ysupbl scheme.
!
!     shinhongpbl:
!     coded and implemented by hyeyum hailey shin (ncar)
!              summer 2014
!
!     ysupbl:
!     coded by song-you hong (yonsei university) and implemented by
!              song-you hong (yonsei university) and jimy dudhia (ncar)
!              summer 2002
!
!     references:
!        shin and hong (2015) mon. wea. rev.
!        shin and hong (2013) j. atmos. sci.
!        honnert, masson, and couvreux (2011) j. atmos. sci.
!        hong (2010) quart. j. roy. met. soc
!        hong, noh, and dudhia (2006), mon. wea. rev. 
!
!-------------------------------------------------------------------------------
!
   real,parameter    ::  xkzmin = 0.01,xkzmax = 1000.,rimin = -100.
   real,parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real,parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real,parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real,parameter    ::  phifac = 8.,sfcfrac = 0.1
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real,parameter    ::  ckz = 0.001,zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real,parameter    ::  tmin=1.e-2
   real,parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real,parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1
   real,parameter    ::  c_1=1.0, gamcre = 0.224
! tunable parameters
   real,parameter    ::  mltop = 1.0,sfcfracn1 = 0.075
   real,parameter    ::  nlfrac = 0.7,enlfrac = -0.4
   real,parameter    ::  a11 = 1.0,a12 = -1.15
   real,parameter    ::  ezfac = 1.5
   real,parameter    ::  cpent = -0.4,rigsmax = 100.
   real,parameter    ::  entfmin = 1.0, entfmax = 5.0
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte,                 &
                                    j,ndiff
   integer,  intent(in)      ::     shinhong_tke_diag
!
   real,     intent(in   )   ::     dt,rcl,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::     ep1,ep2,karman
!
   real,     intent(in )     ::      dx,dy
!
   real,     dimension( ims:ime, kms:kme ),                                    &
             intent(in)      ::                                        dz8w2d, &
                                                                         pi2d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            tx
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(in   )   ::                                            qx
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(inout)   ::                                          qtnp
!
   real,     dimension( its:ite, kts:kte+1 )                                 , &
             intent(in   )   ::                                          p2di
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                           p2d
!
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                           tke, &
                                                                       el_pbl
!
   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::                                           ust, &
                                                                         hpbl, &
                                                                          znt
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx
!
   real,     dimension( ims:ime ), intent(inout)   ::                    wspd
   real,     dimension( ims:ime ), intent(in  )    ::                      br
   real,     dimension( ims:ime ), intent(in  )    ::                    corf
!
   real,     dimension( ims:ime ), intent(in   )   ::                    psim, &
                                                                         psih
!
   real,     dimension( ims:ime ), intent(in   )   ::                  psfcpa
   integer,  dimension( ims:ime ), intent(out  )   ::                  kpbl1d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            ux, &
                                                                           vx

   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(inout)   ::                                        regime
   real,     dimension( its:ite )                                            , &
             intent(out  )   ::                                         wstar, &
                                                                        delta
!
! local vars
!
   real,     dimension( its:ite )            ::                           hol
   real,     dimension( its:ite, kts:kte+1 ) ::                            zq
!
   real,     dimension( its:ite )            ::                       deltaoh
   real,     dimension( its:ite, kts:kte )   ::                            mf, &
                                                                       zfacmf, &
                                                                     entfacmf
   real,     dimension( its:ite )            ::                          rigs, &
                                                                     enlfrac2, &
                                                                        cslen
!
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                     thx,thvx, &
                                                                          del, &
                                                                          dza, &
                                                                          dzq, &
                                                                         xkzo, &
                                                                           za
!
   real,    dimension( its:ite )             ::                                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                                  dusfc,dvsfc, &
                                                                  dtsfc,dqsfc, &
                                                                        prpbl, &
                                                                        wspd1
!
   real,    dimension( its:ite, kts:kte )    ::                     xkzm,xkzh, &
                                                                        f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al, &
                                                                         xkzq, &
                                                                         zfac
!
!jdf added exch_hx
!
   real,    dimension( ims:ime, kms:kme )                                    , &
            intent(inout)   ::                                        exch_hx
!
   real,    dimension( ims:ime )                                             , &
            intent(inout)    ::                                           u10, &
                                                                          v10
   real,    dimension( its:ite )    ::                                         &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro
!
   real,    dimension( its:ite, kts:kte, ndiff)  ::                     r3,f3
   integer, dimension( its:ite )             ::                          kpbl
!
   logical, dimension( its:ite )             ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable
   logical, dimension( ndiff )               ::                        ifvmix
!
   integer ::  n,i,k,l,ic,is,nwmass
   integer ::  klpbl, kqc, kqi
!
   real    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real    ::  ss,ri,qmean,tmean,alpha,chi,zk,rl2,dk,sri
   real    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real    ::  utend,vtend,ttend,qtend
   real    ::  dtstep,govrthv
   real    ::  cont, conq, conw, conwrc
   real    ::  delxy,pu1,pth1,pq1
   real    ::  zfacdx
   real    :: amf1,amf2,bmf2,amf3,bmf3,amf4,bmf4,sflux0,snlflux0
   real    :: mlfrac,ezfrac,sfcfracn
   real    :: uwst,uwstx,csfac
!
   real, dimension( its:ite, kts:kte )     ::                         wscalek
   real, dimension( its:ite, kts:kte )     ::                     xkzml,xkzhl, &
                                                               zfacent,entfac
   real, dimension( its:ite )              ::                            ust3, &
                                                                       wstar3, &
                                                                  hgamu,hgamv, &
                                                                      wm2, we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl, &
                                                                        dthvx
   real    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,                           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z
!
   integer ::  lmh,lmxl
   real    ::  dex,hgame_c
!
   real, dimension( its:ite, kts:kte )     ::                             q2x, &
                                                                      hgame2d, &
                                                                      tflux_e
!
   real, dimension( kts+1:kte )            ::                    s2,gh,rig,el, &
                                                                    akmk,akhk, &
                                                                     zfacentk
!
   real, dimension( kts:kte+1 )            ::                             zqk
!
   real, dimension( kts:kte*ndiff )        ::                             qxk
!
   real, dimension( kts:kte )              ::                         uxk,vxk, &
                                                               txk,thxk,thvxk, &
                                                                         q2xk, &
                                                                        hgame
!
   real, dimension( kts:kte )              ::            ps1d,pb1d,eps1d,pt1d, &
                                                    xkze1d,eflx_l1d,eflx_nl1d
!
   real, dimension( its:ite )              ::                          efxpbl, &
                                                                     hpbl_cbl, &
                                                                       epshol, &
                                                                           ct
!
!-------------------------------------------------------------------------------
!
   klpbl = kte
   lmh = 1
   lmxl = 1
!
   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac
!
!  k-start index for cloud and rain 
!
   kqc = 1 + kte
   kqi = 1 + kte*2
   nwmass = 3
   ifvmix(:) = .true.
!
   do k = kts,kte
     do i = its,ite
       thx(i,k) = tx(i,k)/pi2d(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       tvcon = (1.+ep1*qx(i,k))
       thvx(i,k) = thx(i,k)*tvcon
     enddo
   enddo
!
   do i = its,ite
     tvcon = (1.+ep1*qx(i,1))
     rhox(i) = psfcpa(i)/(rd*tx(i,1)*tvcon)
     govrth(i) = g/thx(i,1)
   enddo
!
!-----compute the height of full- and half-sigma levels above ground
!     level, and the layer thicknesses.
!
   do i = its,ite
     zq(i,1) = 0.
   enddo
!
   do k = kts,kte
     do i = its,ite
       zq(i,k+1) = dz8w2d(i,k)+zq(i,k)
     enddo
   enddo
! 
   do k = kts,kte
     do i = its,ite
       za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
       dzq(i,k) = zq(i,k+1)-zq(i,k)
       del(i,k) = p2di(i,k)-p2di(i,k+1)
     enddo
   enddo
!
   do i = its,ite
     dza(i,1) = za(i,1)
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       dza(i,k) = za(i,k)-za(i,k-1)
     enddo
   enddo
!
!
!-----initialize vertical tendencies and
!
   utnp(its:ite,:) = 0.
   vtnp(its:ite,:) = 0.
   ttnp(its:ite,:) = 0.
   qtnp(its:ite,:) = 0.
!
   do i = its,ite
     wspd1(i) = sqrt(ux(i,1)*ux(i,1)+vx(i,1)*vx(i,1))+1.e-9
   enddo
!
!---- compute vertical diffusion
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
   dtstep = dt
   dt2 = 2.*dtstep
   rdt = 1./dt2
!
   do i = its,ite
     bfxpbl(i) = 0.0
     hfxpbl(i) = 0.0
     qfxpbl(i) = 0.0
     ufxpbl(i) = 0.0
     vfxpbl(i) = 0.0
     hgamu(i)  = 0.0
     hgamv(i)  = 0.0
     delta(i)  = 0.0
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       wscalek(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       zfac(i,k) = 0.0
     enddo
   enddo
!
   do i = its,ite
     efxpbl(i) = 0.0
     hpbl_cbl(i) = 0.0
     epshol(i) = 0.0
     ct(i) = 0.0
   enddo
!
   do k = kts,kte
     do i = its,ite
       el_pbl(i,k)  = 0.0
       hgame2d(i,k) = 0.0
       tflux_e(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       q2x(i,k) = 2.*tke(i,k)
     enddo
   enddo
!
   do i = its,ite
     deltaoh(i) = 0.0
     rigs(i)     = 0.0
     enlfrac2(i) = 0.0
     cslen(i)    = 0.0
   enddo
!
   do k = kts,kte
     do i = its,ite
       mf(i,k) = 0.0
       zfacmf(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl-1
     do i = its,ite
       xkzo(i,k) = ckz*dza(i,k+1)
     enddo
   enddo
!
   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
     dtsfc(i) = 0.
     dqsfc(i) = 0.
   enddo
!
   do i = its,ite
     hgamt(i)  = 0.
     hgamq(i)  = 0.
     wscale(i) = 0.
     kpbl(i)   = 1
     hpbl(i)   = zq(i,1)
     hpbl_cbl(i) = zq(i,1)
     zl1(i)    = za(i,1)
     thermal(i)= thvx(i,1)
     pblflg(i) = .true.
     sfcflg(i) = .true.
     sflux(i) = hfx(i)/rhox(i)/cp + qfx(i)/rhox(i)*ep1*thx(i,1)
     if(br(i).gt.0.0) sfcflg(i) = .false.
   enddo
!
!     compute the first guess of pbl height
!
   do i = its,ite
     stable(i) = .false.
     brup(i) = br(i)
     brcr(i) = brcr_ub
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     k = kpbl(i)
     if(brdn(i).ge.brcr(i))then
       brint = 0.
     elseif(brup(i).le.brcr(i))then
       brint = 1.
     else
       brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
     endif
     hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
     if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
     if(kpbl(i).le.1) pblflg(i) = .false.
   enddo
!
   do i = its,ite
     fm = psim(i)
     fh = psih(i)
     zol1(i) = max(br(i)*fm*fm/fh,rimin)
     if(sfcflg(i))then
       zol1(i) = min(zol1(i),-zfmin)
     else
       zol1(i) = max(zol1(i),zfmin)
     endif
     hol1 = zol1(i)*hpbl(i)/zl1(i)*sfcfrac
     epshol(i) = hol1
     if(sfcflg(i))then
       phim(i) = (1.-aphi16*hol1)**(-1./4.)
       phih(i) = (1.-aphi16*hol1)**(-1./2.)
       bfx0  = max(sflux(i),0.)
       hfx0 = max(hfx(i)/rhox(i)/cp,0.)
       qfx0 = max(ep1*thx(i,1)*qfx(i)/rhox(i),0.)
       wstar3(i) = (govrth(i)*bfx0*hpbl(i))
       wstar(i) = (wstar3(i))**h1
     else
       phim(i) = (1.+aphi5*hol1)
       phih(i) = phim(i)
       wstar(i)  = 0.
       wstar3(i) = 0.
     endif
     ust3(i)   = ust(i)**3.
     wscale(i) = (ust3(i)+phifac*karman*wstar3(i)*0.5)**h1
     wscale(i) = min(wscale(i),ust(i)*aphi16)
     wscale(i) = max(wscale(i),ust(i)/aphi5)
   enddo
!
!     compute the surface variables for pbl height estimation
!     under unstable conditions
!
   do i = its,ite
     if(sfcflg(i).and.sflux(i).gt.0.0)then
       gamfac   = bfac/rhox(i)/wscale(i)
       hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
       hgamq(i) = min(gamfac*qfx(i),gamcrq)
       vpert = (hgamt(i)+ep1*thx(i,1)*hgamq(i))/bfac*afac
       thermal(i) = thermal(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       hgamt(i) = max(hgamt(i),0.0)
       hgamq(i) = max(hgamq(i),0.0)
       brint    = -15.9*ust(i)*ust(i)/wspd(i)*wstar3(i)/(wscale(i)**4.)
       hgamu(i) = brint*ux(i,1)
       hgamv(i) = brint*vx(i,1)
     else
       pblflg(i) = .false.
     endif
   enddo
!
!     enhance the pbl height by considering the thermal
!
   do i = its,ite
     if(pblflg(i))then
       kpbl(i) = 1
       hpbl(i) = zq(i,1)
     endif
   enddo
!
   do i = its,ite
     if(pblflg(i))then
       stable(i) = .false.
       brup(i) = br(i)
       brcr(i) = brcr_ub
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i).and.pblflg(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
       uwst  = abs(ust(i)/wstar(i)-0.5)
       uwstx = -80.*uwst+14.
       csfac = 0.5*(tanh(uwstx)+3.)
       cslen(i) = csfac*hpbl(i)
     endif
   enddo
!
!     stable boundary layer
!
   do i = its,ite
     hpbl_cbl(i) = hpbl(i)
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       brup(i) = br(i)
       stable(i) = .false.
     else
       stable(i) = .true.
     endif
   enddo
!
   do i = its,ite
     if((.not.stable(i)).and.((xland(i)-1.5).ge.0))then
       wspd10 = u10(i)*u10(i) + v10(i)*v10(i)
       wspd10 = sqrt(wspd10)
       ross = wspd10 / (cori*znt(i))
       brcr_sbro(i) = min(0.16*(1.e-7*ross)**(-0.18),.3)
     endif
   enddo
!
   do i = its,ite
     if(.not.stable(i))then
       if((xland(i)-1.5).ge.0)then
         brcr(i) = brcr_sbro(i)
       else
         brcr(i) = brcr_sb
       endif
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo
!
!     grid-size dependency for nonlocal momentum transport
!
   delxy=sqrt(dx*dy)
!
   do i = its,ite
     pu1=pu(delxy,cslen(i))
     pq1=pq(delxy,cslen(i))
     if(pblflg(i)) then
       hgamu(i) = hgamu(i)*pu1
       hgamv(i) = hgamv(i)*pu1
       hgamq(i) = hgamq(i)*pq1
     endif
   enddo
!
!     estimate the entrainment parameters
!
   delxy=sqrt(dx*dy)
!
   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i) - 1
       prpbl(i) = 1.0
       wm3       = wstar3(i) + 5. * ust3(i)
       wm2(i)    = wm3**h2
       bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
       dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
       dthx  = max(thx(i,k+1)-thx(i,k),tmin)
       dqx   = min(qx(i,k+1)-qx(i,k),0.0)
       we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))
       hfxpbl(i) = we(i)*dthx
       pq1=pq(delxy,cslen(i))
       qfxpbl(i) = we(i)*dqx*pq1
!
       dux = ux(i,k+1)-ux(i,k)
       dvx = vx(i,k+1)-vx(i,k)
       pu1=pu(delxy,cslen(i))
       if(dux.gt.tmin) then
         ufxpbl(i) = max(prpbl(i)*we(i)*dux*pu1,-ust(i)*ust(i))
       elseif(dux.lt.-tmin) then
         ufxpbl(i) = min(prpbl(i)*we(i)*dux*pu1,ust(i)*ust(i))
       else
         ufxpbl(i) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i) = max(prpbl(i)*we(i)*dvx*pu1,-ust(i)*ust(i))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i) = min(prpbl(i)*we(i)*dvx*pu1,ust(i)*ust(i))
       else
         vfxpbl(i) = 0.0
       endif
       delb  = govrth(i)*d3*hpbl(i)
       delta(i) = min(d1*hpbl(i) + d2*wm2(i)/delb,100.)
       delb  = govrth(i)*dthvx(i)
       deltaoh(i) = d1*hpbl(i) + d2*wm2(i)/delb
       deltaoh(i) = max(ezfac*deltaoh(i),hpbl(i)-za(i,kpbl(i)-1)-1.)
       deltaoh(i) = min(deltaoh(i)      ,hpbl(i))
       rigs(i)     = govrth(i)*dthvx(i)*deltaoh(i)/(dux**2.+dvx**2.)
       rigs(i)     = max(min(rigs(i), rigsmax),rimin)
       enlfrac2(i) = max(min(wm3/wstar3(i)/(1.+cpent/rigs(i)),entfmax), entfmin)
       enlfrac2(i) = enlfrac2(i)*enlfrac
     endif
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i))then
         entfacmf(i,k) = sqrt(((zq(i,k+1)-hpbl(i))/deltaoh(i))**2.)
       endif
       if(pblflg(i).and.k.ge.kpbl(i))then
         entfac(i,k) = ((zq(i,k+1)-hpbl(i))/deltaoh(i))**2.
       else
         entfac(i,k) = 1.e30
       endif
     enddo
   enddo
!
!     compute diffusion coefficients below pbl
!
   do k = kts,klpbl
     do i = its,ite
       if(k.lt.kpbl(i)) then
         zfac(i,k) = min(max((1.-(zq(i,k+1)-zl1(i))/(hpbl(i)-zl1(i))),zfmin),1.)
         zfacent(i,k) = (1.-zfac(i,k))**3.
         wscalek(i,k) = (ust3(i)+phifac*karman*wstar3(i)*(1.-zfac(i,k)))**h1
         if(sfcflg(i)) then 
           prfac = conpr
           prfac2 = 15.9*wstar3(i)/ust3(i)/(1.+4.*karman*wstar3(i)/ust3(i))
           prnumfac = -3.*(max(zq(i,k+1)-sfcfrac*hpbl(i),0.))**2./hpbl(i)**2.
         else
           prfac = 0.
           prfac2 = 0.
           prnumfac = 0.
           phim8z = 1.+aphi5*zol1(i)*zq(i,k+1)/zl1(i)
           wscalek(i,k) = ust(i)/phim8z
           wscalek(i,k) = max(wscalek(i,k),0.001)
         endif
         prnum0 = (phih(i)/phim(i)+prfac)
         prnum0 = max(min(prnum0,prmax),prmin)
         xkzm(i,k) = wscalek(i,k)*karman*zq(i,k+1)*zfac(i,k)**pfac
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzq(i,k) = xkzm(i,k)/prnum*zfac(i,k)**(pfac_q-pfac)
         prnum0 = prnum0/(1.+prfac2*karman*sfcfrac)
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzh(i,k) = xkzm(i,k)/prnum
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzm(i,k) = max(xkzm(i,k),xkzo(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzh(i,k) = max(xkzh(i,k),xkzo(i,k))
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
         xkzq(i,k) = max(xkzq(i,k),xkzo(i,k))
       endif
     enddo
   enddo
!
!     compute diffusion coefficients over pbl (free atmosphere)
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         ss = ((ux(i,k+1)-ux(i,k))*(ux(i,k+1)-ux(i,k))                         &
              +(vx(i,k+1)-vx(i,k))*(vx(i,k+1)-vx(i,k)))                        &
              /(dza(i,k+1)*dza(i,k+1))+1.e-9
         govrthv = g/(0.5*(thvx(i,k+1)+thvx(i,k)))
         ri = govrthv*(thvx(i,k+1)-thvx(i,k))/(ss*dza(i,k+1))
!      in cloud
         if(imvdif.eq.1.and.nwmass.ge.3)then
           if((qx(i,kqc+k-1)+qx(i,kqi+k-1)).gt.0.01e-3                         &
             .and.(qx(i,kqc+k)+qx(i,kqi+k)).gt.0.01e-3) then
             qmean = 0.5*(qx(i,k)+qx(i,k+1))
             tmean = 0.5*(tx(i,k)+tx(i,k+1))
             alpha = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alpha)*(ri-g*g/ss/tmean/cp*((chi-alpha)/(1.+chi)))
           endif
         endif
         zk = karman*zq(i,k+1)
         rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
         rlamdz = min(dza(i,k+1),rlamdz)
         rl2 = (zk*rlamdz/(rlamdz+zk))**2
         dk = rl2*sqrt(ss)
         if(ri.lt.0.)then
! unstable regime
           ri = max(ri, rimin)
           sri = sqrt(-ri)
           xkzm(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
           xkzh(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
         else
! stable regime
           xkzh(i,k) = dk/(1+5.*ri)**2
           prnum = 1.0+2.1*ri
           prnum = min(prnum,prmax)
           xkzm(i,k) = xkzh(i,k)*prnum
         endif
!
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzm(i,k) = max(xkzm(i,k),xkzo(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzh(i,k) = max(xkzh(i,k),xkzo(i,k))
         xkzml(i,k) = xkzm(i,k)
         xkzhl(i,k) = xkzh(i,k)
       endif
     enddo
   enddo
!
!     prescribe nonlocal transport below pbl
!
   do i = its,ite
     deltaoh(i) = deltaoh(i)/hpbl(i)
   enddo
!
   delxy=sqrt(dx*dy)
   do i = its,ite
!
     mlfrac      = mltop-deltaoh(i)
     ezfrac      = mltop+deltaoh(i)
     zfacmf(i,1) = min(max((zq(i,2)/hpbl(i)),zfmin),1.)
     sfcfracn    = max(sfcfracn1,zfacmf(i,1))
!
     sflux0      = (a11+a12*sfcfracn)*sflux(i)
     snlflux0    = nlfrac*sflux0
     amf1        = snlflux0/sfcfracn
     amf2        = -snlflux0/(mlfrac-sfcfracn)
     bmf2        = -mlfrac*amf2
     amf3        = snlflux0*enlfrac2(i)/deltaoh(i)
     bmf3        = -amf3*mlfrac
     hfxpbl(i)   = amf3+bmf3
     pth1=pthnl(delxy,cslen(i))
     hfxpbl(i)   = hfxpbl(i)*pth1
!
     do k = kts,klpbl
       zfacmf(i,k) = max((zq(i,k+1)/hpbl(i)),zfmin)
       if(pblflg(i).and.k.lt.kpbl(i)) then
         if(zfacmf(i,k).le.sfcfracn) then
           mf(i,k) = amf1*zfacmf(i,k)
         else if (zfacmf(i,k).le.mlfrac) then
           mf(i,k) = amf2*zfacmf(i,k)+bmf2
         endif
         mf(i,k) = mf(i,k)+hfxpbl(i)*exp(-entfacmf(i,k))
         mf(i,k) = mf(i,k)*pth1
       endif
     enddo
   enddo
!
!     compute tridiagonal matrix elements for heat
!
   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
     enddo
   enddo
!
   do i = its,ite
     ad(i,1) = 1.
     f1(i,1) = thx(i,1)-300.+hfx(i)/cont/del(i,1)*dt2
   enddo
!
   delxy=sqrt(dx*dy)
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzh(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzt = tem1*(-mf(i,k)/xkzh(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzt
         f1(i,k+1) = thx(i,k+1)-300.-dtodsu*dsdzt
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzh(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzh(i,k) = sqrt(xkzh(i,k)*xkzhl(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzh(i,k) = max(xkzh(i,k),xkzo(i,k))
         f1(i,k+1) = thx(i,k+1)-300.
       else
         f1(i,k+1) = thx(i,k+1)-300.
       endif
       tem1   = dsig*xkzh(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     grid-size dependency for local heat transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pth1=pthl(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pth1
         al(i,k) = al(i,k)*pth1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
     enddo
   enddo
!
   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)
!
!     recover tendencies of heat
!
   do k = kte,kts,-1
     do i = its,ite
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt*pi2d(i,k)
       ttnp(i,k) = ttnp(i,k)+ttend
       dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)/pi2d(i,k)
       if(k.eq.kte) then
         tflux_e(i,k) = ttend*dz8w2d(i,k)
       else
         tflux_e(i,k) = tflux_e(i,k+1) + ttend*dz8w2d(i,k)
       endif
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       if(pblflg(i).and.k.lt.kpbl(i)) then
         hgame_c=c_1*0.2*2.5*(g/thvx(i,k))*wstar(i)/(0.25*(q2x(i,k+1)+q2x(i,k)))
         hgame_c=min(hgame_c,gamcre)
         if(k.eq.kte)then
           hgame2d(i,k)=hgame_c*0.5*tflux_e(i,k)*hpbl(i)
           hgame2d(i,k)=max(hgame2d(i,k),0.0)
         else
           hgame2d(i,k)=hgame_c*0.5*(tflux_e(i,k)+tflux_e(i,k+1))*hpbl(i)
           hgame2d(i,k)=max(hgame2d(i,k),0.0)
         endif
       endif
     enddo
   enddo
!
!
!     compute tridiagonal matrix elements for moisture, clouds, and gases
!
   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
     enddo
   enddo
!
   do ic = 1,ndiff
     do i = its,ite
       do k = kts,kte
         f3(i,k,ic) = 0.
       enddo
     enddo
   enddo
!
   do i = its,ite
     ad(i,1) = 1.
     f3(i,1,1) = qx(i,1)+qfx(i)*g/del(i,1)*dt2
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do i = its,ite
         f3(i,1,ic) = qx(i,1+is)
       enddo
     enddo
   endif
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         xkzq(i,k) = xkzh(i,k)
       endif
     enddo
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzq(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzq = tem1*(-qfxpbl(i)*zfacent(i,k)/xkzq(i,k))
         f3(i,k,1) = f3(i,k,1)+dtodsd*dsdzq
         f3(i,k+1,1) = qx(i,k+1)-dtodsu*dsdzq
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzq(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzq(i,k) = sqrt(xkzq(i,k)*xkzhl(i,k))
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
         xkzq(i,k) = max(xkzq(i,k),xkzo(i,k))
         f3(i,k+1,1) = qx(i,k+1)
       else
         f3(i,k+1,1) = qx(i,k+1)
       endif
       tem1   = dsig*xkzq(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     scale dependency for local moisture transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pq1=pq(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pq1
         al(i,k) = al(i,k)*pq1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
!      exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kts,kte-1
         do i = its,ite
           f3(i,k+1,ic) = qx(i,k+1+is)
         enddo
       enddo
     enddo
   endif
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
     enddo
   enddo
!
   do ic = 1,ndiff
     do k = kts,kte
       do i = its,ite
         r3(i,k,ic) = f3(i,k,ic)
       enddo
     enddo
   enddo
!
!     solve tridiagonal problem for moisture, clouds, and gases
!
   call tridin_ysu(al,ad,cu,r3,au,f3,its,ite,kts,kte,ndiff)
!
!     recover tendencies of heat and moisture
!
   do k = kte,kts,-1
     do i = its,ite
       qtend = (f3(i,k,1)-qx(i,k))*rdt
       qtnp(i,k) = qtnp(i,k)+qtend
       dqsfc(i) = dqsfc(i)+qtend*conq*del(i,k)
     enddo
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       if(ifvmix(ic)) then
         is = (ic-1) * kte
         do k = kte,kts,-1
           do i = its,ite
             qtend = (f3(i,k,ic)-qx(i,k+is))*rdt
             qtnp(i,k+is) = qtnp(i,k+is)+qtend
           enddo
         enddo
       endif
     enddo
   endif
!
!     compute tridiagonal matrix elements for momentum
!
   do i = its,ite
     do k = kts,kte
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
       f2(i,k) = 0.
     enddo
   enddo
!
   do i = its,ite
! paj: ctopo=1 if topo_wind=0 (default)
! mchen add this line to make sure NMM can still work with YSU PBL
     ad(i,1) = 1.+ctopo(i)*ust(i)**2/wspd1(i)*rhox(i)*g/del(i,1)*dt2           &
          *(wspd1(i)/wspd(i))**2
     f1(i,1) = ux(i,1)
     f2(i,1) = vx(i,1)
   enddo
!
   delxy=sqrt(dx*dy)
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzm(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i))then
         dsdzu     = tem1*(-hgamu(i)/hpbl(i)-ufxpbl(i)*zfacent(i,k)/xkzm(i,k))
         dsdzv     = tem1*(-hgamv(i)/hpbl(i)-vfxpbl(i)*zfacent(i,k)/xkzm(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzu
         f1(i,k+1) = ux(i,k+1)-dtodsu*dsdzu
         f2(i,k)   = f2(i,k)+dtodsd*dsdzv
         f2(i,k+1) = vx(i,k+1)-dtodsu*dsdzv
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzm(i,k) = prpbl(i)*xkzh(i,k)
         xkzm(i,k) = sqrt(xkzm(i,k)*xkzml(i,k))
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzm(i,k) = max(xkzm(i,k),xkzo(i,k))
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       else
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       endif
       tem1   = dsig*xkzm(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     grid-size dependency for local momentum transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pu1=pu(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pu1
         al(i,k) = al(i,k)*pu1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
       r2(i,k) = f2(i,k)
     enddo
   enddo
!
!     solve tridiagonal problem for momentum
!
   call tridi1n(al,ad,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1)
!
!     recover tendencies of momentum
!
   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k)-ux(i,k))*rdt
       vtend = (f2(i,k)-vx(i,k))*rdt
       utnp(i,k) = utnp(i,k)+utend
       vtnp(i,k) = vtnp(i,k)+vtend
       dusfc(i) = dusfc(i) + utend*conwrc*del(i,k)
       dvsfc(i) = dvsfc(i) + vtend*conwrc*del(i,k)
     enddo
   enddo
!
! paj: ctopo2=1 if topo_wind=0 (default)
!
   do i = its,ite
     u10(i) = ctopo2(i)*u10(i)+(1-ctopo2(i))*ux(i,1)
     v10(i) = ctopo2(i)*v10(i)+(1-ctopo2(i))*vx(i,1)
   enddo
!
   if (shinhong_tke_diag.eq.1) then
!
!---- calculate tke consistent with shinhongpbl algorithm
!
   tke_calculation: do i = its,ite
!
     do k = kts+1,kte
       s2(k)   = 0.0
       gh(k)   = 0.0
       rig(k)  = 0.0
       el(k)   = 0.0
       akmk(k) = 0.0
       akhk(k) = 0.0
       zfacentk(k) = 0.0
     enddo
!
     do k = kts,kte
       uxk(k)   = 0.0
       vxk(k)   = 0.0
       txk(k)   = 0.0
       thxk(k)  = 0.0
       thvxk(k) = 0.0
       q2xk(k)  = 0.0
       hgame(k) = 0.0
       ps1d(k)  = 0.0
       pb1d(k)  = 0.0
       eps1d(k) = 0.0
       pt1d(k)  = 0.0
       xkze1d(k)    = 0.0
       eflx_l1d(k)  = 0.0
       eflx_nl1d(k) = 0.0
     enddo
!
     do k = kts,kte+1
       zqk(k)   = 0.0
     enddo
!
     do k = kts,kte*ndiff
       qxk(k) = 0.0
     enddo
!
     do k = kts,kte
       uxk(k)   = ux(i,k)
       vxk(k)   = vx(i,k)
       txk(k)   = tx(i,k)
       thxk(k)  = thx(i,k)
       thvxk(k) = thvx(i,k)
       q2xk(k)  = q2x(i,k)
       hgame(k) = hgame2d(i,k)
     enddo
!
     do k = kts,kte+1
       zqk(k) = zq(i,k)
     enddo
!
     do k = kts,kte*ndiff
       qxk(k) = qx(i,k)
     enddo
!
     do k = kts+1,kte
       akmk(k) = xkzm(i,k-1)
       akhk(k) = xkzh(i,k-1)
       zfacentk(k) = zfacent(i,k-1)
     enddo
!
     if(pblflg(i)) then
       k = kpbl(i) - 1
       dex = 0.25*(q2xk(k+2)-q2xk(k))
       efxpbl(i) = we(i)*dex
     endif
!
!---- find the mixing length
!
     call mixlen(lmh,uxk,vxk,txk,thxk,qxk(kts),qxk(kte+1)                      &
                     ,q2xk,zqk,ust(i),corf(i),epshol(i)                        &
                     ,s2,gh,rig,el                                             &
                     ,hpbl(i),kpbl(i),lmxl,ct(i)                               &
                     ,hgamu(i),hgamv(i),hgamt(i),pblflg(i)                     &
                     ,zfacentk,ufxpbl(i),vfxpbl(i),hfxpbl(i)                   &
                     ,ids,ide,jds,jde,kds,kde                                  &
                     ,ims,ime,jms,jme,kms,kme                                  &
                     ,its,ite,jts,jte,kts,kte   )
!
!---- solve for the production/dissipation of the turbulent kinetic energy
!
     call prodq2(lmh,dt,ust(i),s2,rig,q2xk,el,zqk,akmk,akhk                    &
                     ,uxk,vxk,thxk,thvxk                                       &
                     ,hgamu(i),hgamv(i),hgamt(i)                               &
                     ,hpbl(i),pblflg(i),kpbl(i)                                &
                     ,zfacentk,ufxpbl(i),vfxpbl(i),hfxpbl(i)                   &
                     ,ids,ide,jds,jde,kds,kde                                  &
                     ,ims,ime,jms,jme,kms,kme                                  &
                     ,its,ite,jts,jte,kts,kte   )
!
!
!---- carry out the vertical diffusion of turbulent kinetic energy
!
     call vdifq(lmh,dt,q2xk,el,zqk                                             &
                    ,akhk                                                      &
                    ,hgame,hpbl(i),pblflg(i),kpbl(i)                           &
                    ,efxpbl(i)                                                 &
                    ,ids,ide,jds,jde,kds,kde                                   &
                    ,ims,ime,jms,jme,kms,kme                                   &
                    ,its,ite,jts,jte,kts,kte   )
!
!---- save the new tke and mixing length.
!
     do k = kts,kte
       q2x(i,k) = amax1(q2xk(k),epsq2l)
       tke(i,k) = 0.5*q2x(i,k)
       if(k/=kts) el_pbl(i,k) = el(k) ! el is not defined at kte
     enddo
!
   enddo tke_calculation
   endif
!
!---- end of vertical diffusion
!
   do i = its,ite
     kpbl1d(i) = kpbl(i)
   enddo
!
!
   end subroutine shinhong2d
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridi1n(cl,cm,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm, &
                                                                           r1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do i = its,l
     fk = 1./cm(i,1)
     au(i,1) = fk*cu(i,1)
     f1(i,1) = fk*r1(i,1)
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do k = kts+1,n-1
     do i = its,l
       fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
       au(i,k) = fk*cu(i,k)
       f1(i,k) = fk*(r1(i,k)-cl(i,k)*f1(i,k-1))
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do i = its,l
     fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
     f1(i,n) = fk*(r1(i,n)-cl(i,n)*f1(i,n-1))
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do k = n-1,kts,-1
     do i = its,l
       f1(i,k) = f1(i,k)-au(i,k)*f1(i,k+1)
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridi1n
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridin_ysu(cl,cm,cu,r2,au,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       au(i,1) = fk*cu(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         au(i,k) = fk*cu(i,k)
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridin_ysu
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine shinhonginit(rublten,rvblten,rthblten,rqvblten,                  &
                      rqcblten,rqiblten,                                       &
                      tke_pbl,                                                 &
                      p_qi,p_first_scalar,                                     &
                      restart, allowed_to_read,                                &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   real,parameter                ::  epsq2l = 0.01
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
                                                                     rqiblten
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      tke_pbl
   integer :: i, j, k, itf, jtf, ktf
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
            tke_pbl(i,k,j) = epsq2l/2.
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
   end subroutine shinhonginit
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pu(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pu
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.0, a2 = 0.070, a3 = 1.0, a4 = 0.142, a5 = 0.071
   real,parameter :: b1 = 2.0, b2 = 0.6666667
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2
   den=a3*(doh)**b1+a4*(doh)**b2+a5
   pu=num/den
   pu=max(pu,pmin)
   pu=min(pu,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pq(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pq
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.0, a2 = -0.098, a3 = 1.0, a4 = 0.106, a5 = 0.5
   real,parameter :: b1 = 2.0
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2
   den=a3*(doh)**b1+a4
   pq=a5*num/den+(1.-a5)
   pq=max(pq,pmin)
   pq=min(pq,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pthnl(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pthnl
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.000, a2 = 0.936, a3 = -1.110,                      &
                     a4 = 1.000, a5 = 0.312, a6 = 0.329, a7 = 0.243
   real,parameter :: b1 = 2.0, b2 = 0.875
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2+a3
   den=a4*(doh)**b1+a5*(doh)**b2+a6
   pthnl=a7*num/den+(1.-a7)
   pthnl=max(pthnl,pmin)
   pthnl=min(pthnl,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pthl(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pthl
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.000, a2 = 0.870, a3 = -0.913,                      &
                     a4 = 1.000, a5 = 0.153, a6 = 0.278, a7 = 0.280
   real,parameter :: b1 = 2.0, b2 = 0.5
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2+a3
   den=a4*(doh)**b1+a5*(doh)**b2+a6
   pthl=a7*num/den+(1.-a7)
   pthl=max(pthl,pmin)
   pthl=min(pthl,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                          SUBROUTINE MIXLEN                            &
!----------------------------------------------------------------------
!   ******************************************************************
!   *                                                                *
!   *                   LEVEL 2.5 MIXING LENGTH                      *
!   *                                                                *
!   ******************************************************************
!
     &(LMH,U,V,T,THE,Q,CWM,Q2,Z,USTAR,CORF,EPSHOL                      &
     &,S2,GH,RI,EL,PBLH,LPBL,LMXL,CT                                   &
     &,HGAMU,HGAMV,HGAMT,PBLFLG                                        &
     &,ZFACENTK,UFXPBL,VFXPBL,HFXPBL                                   &
     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LMH
!
      INTEGER,INTENT(IN) :: LMXL,LPBL
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: CWM,Q,Q2,T,THE,U,V
!
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
!
      REAL,INTENT(IN) :: PBLH
!
      REAL,DIMENSION(KTS+1:KTE),INTENT(OUT) :: EL,RI,GH,S2
!
      REAL,INTENT(INOUT) :: CT
!
      REAL,INTENT(IN)    :: CORF,USTAR
      REAL,INTENT(INOUT) :: EPSHOL
!
      REAL,DIMENSION(KTS+1:KTE),INTENT(IN) :: ZFACENTK
      REAL,INTENT(IN) :: HGAMU,HGAMV,HGAMT,UFXPBL,VFXPBL,HFXPBL
      REAL :: SUK,SVK
!
      LOGICAL,INTENT(IN) :: PBLFLG
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K,LPBLM
!
      REAL :: A,ADEN,B,BDEN,AUBR,BUBR,BLMX,EL0,ELOQ2X,GHL,S2L          &
     &       ,QOL2ST,QOL2UN,QDZL,RDZ,SQ,SREL,SZQ,TEM,THM,VKRMZ,RLAMBDA &
     &       ,RLB,RLN,F
!
      REAL,DIMENSION(KTS:KTE) :: Q1,EN2
!
      REAL,DIMENSION(KTS+1:KTE) :: DTH,ELM,REL
      REAL,PARAMETER :: ELCBL=0.77
      REAL :: CKP
!----------------------------------------------------------------------
!**********************************************************************
!-----------------------------------------------------------------------
      DO K=KTS,KTE
        Q1(K)=0.
      ENDDO
!
      DO K=KTS+1,KTE
        DTH(K)=THE(K)-THE(K-1)
      ENDDO
!
      DO K=KTS+2,KTE
        IF(DTH(K)>0..AND.DTH(K-1)<=0.)THEN
          DTH(K)=DTH(K)+CT
          EXIT
        ENDIF
      ENDDO
!
      CT=0.
!----------------------------------------------------------------------
!***  COMPUTE LOCAL GRADIENT RICHARDSON NUMBER
!----------------------------------------------------------------------
      DO K=KTE,KTS+1,-1
        RDZ=2./(Z(K+1)-Z(K-1))
        S2L=((U(K)-U(K-1))**2+(V(K)-V(K-1))**2)*RDZ*RDZ   ! S**2
        IF(PBLFLG.AND.K.LE.LPBL)THEN
          SUK=(U(K)-U(K-1))*RDZ
          SVK=(V(K)-V(K-1))*RDZ
          S2L=(SUK-HGAMU/PBLH)*SUK+(SVK-HGAMV/PBLH)*SVK
        ENDIF
        S2L=MAX(S2L,EPSGM)
        S2(K)=S2L
!
        TEM=(T(K)+T(K-1))*0.5
        THM=(THE(K)+THE(K-1))*0.5
!
        A=THM*P608
        B=(ELOCP/TEM-1.-P608)*THM
!
        GHL=(DTH(K)*((Q(K)+Q(K-1)+CWM(K)+CWM(K-1))*(0.5*P608)+1.)      &
     &     +(Q(K)-Q(K-1)+CWM(K)-CWM(K-1))*A                            &
     &     +(CWM(K)-CWM(K-1))*B)*RDZ                       ! dTheta/dz
        IF(PBLFLG.AND.K.LE.LPBL)THEN
          GHL=GHL-HGAMT/PBLH
        ENDIF
!
        IF(ABS(GHL)<=EPSGH)GHL=EPSGH
!
        EN2(K)=GHL*G/THM                                   ! N**2
!
        GH(K)=GHL
        RI(K)=EN2(K)/S2L
      ENDDO
!
!----------------------------------------------------------------------
!***  FIND MAXIMUM MIXING LENGTHS AND THE LEVEL OF THE PBL TOP
!----------------------------------------------------------------------
!
      DO K=KTE,KTS+1,-1
        S2L=S2(K)
        GHL=GH(K)
!
        IF(GHL>=EPSGH)THEN
          IF(S2L/GHL<=REQU)THEN
            ELM(K)=EPSL
          ELSE
            AUBR=(AUBM*S2L+AUBH*GHL)*GHL
            BUBR= BUBM*S2L+BUBH*GHL
            QOL2ST=(-0.5*BUBR+SQRT(BUBR*BUBR*0.25-AUBR*CUBR))*RCUBR
            ELOQ2X=1./QOL2ST
            ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
          ENDIF
        ELSE
          ADEN=(ADNM*S2L+ADNH*GHL)*GHL
          BDEN= BDNM*S2L+BDNH*GHL
          QOL2UN=-0.5*BDEN+SQRT(BDEN*BDEN*0.25-ADEN)
          ELOQ2X=1./(QOL2UN+EPSRU)       ! repsr1/qol2un
          ELM(K)=MAX(SQRT(ELOQ2X*Q2(K)),EPSL)
        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
      DO K=LPBL,LMH,-1
        Q1(K)=SQRT(Q2(K))
      ENDDO
!----------------------------------------------------------------------
      SZQ=0.
      SQ =0.
!
      DO K=KTE,KTS+1,-1
        QDZL=(Q1(K)+Q1(K-1))*(Z(K)-Z(K-1))
        SZQ=(Z(K)+Z(K-1)-Z(LMH)-Z(LMH))*QDZL+SZQ
        SQ=QDZL+SQ
      ENDDO
!
!----------------------------------------------------------------------
!***  COMPUTATION OF ASYMPTOTIC L IN BLACKADAR FORMULA
!----------------------------------------------------------------------
!
      EL0=MIN(ALPH*SZQ*0.5/SQ,EL0MAX)
      EL0=MAX(EL0            ,EL0MIN)
!
!----------------------------------------------------------------------
!***  ABOVE THE PBL TOP
!----------------------------------------------------------------------
!
      LPBLM=MIN(LPBL+1,KTE)
!
      DO K=KTE,LPBLM,-1
        EL(K)=(Z(K+1)-Z(K-1))*ELFC
        REL(K)=EL(K)/ELM(K)
      ENDDO
!
!----------------------------------------------------------------------
!***  INSIDE THE PBL
!----------------------------------------------------------------------
!
      EPSHOL=MIN(EPSHOL,0.0)
      CKP=ELCBL*((1.0-8.0*EPSHOL)**(1./3.))
      IF(LPBL>LMH)THEN
        DO K=LPBL,LMH+1,-1
          VKRMZ=(Z(K)-Z(LMH))*VKARMAN
          IF(PBLFLG) THEN
            VKRMZ=CKP*(Z(K)-Z(LMH))*VKARMAN
            EL(K)=VKRMZ/(VKRMZ/EL0+1.)
          ELSE
            EL(K)=VKRMZ/(VKRMZ/EL0+1.)
          ENDIF
          REL(K)=EL(K)/ELM(K)
        ENDDO
      ENDIF
!
      DO K=LPBL-1,LMH+2,-1
        SREL=MIN(((REL(K-1)+REL(K+1))*0.5+REL(K))*0.5,REL(K))
        EL(K)=MAX(SREL*ELM(K),EPSL)
      ENDDO
!
!----------------------------------------------------------------------
!***  MIXING LENGTH FOR THE QNSE MODEL IN STABLE CASE
!----------------------------------------------------------------------
!
      F=MAX(CORF,EPS1)
      RLAMBDA=F/(BLCKDR*USTAR)
      DO K=KTE,KTS+1,-1
        IF(EN2(K)>=0.0)THEN                          ! Stable case
          VKRMZ=(Z(K)-Z(LMH))*VKARMAN
          RLB=RLAMBDA+1./VKRMZ
          RLN=SQRT(2.*EN2(K)/Q2(K))/CN
!         EL(K)=MIN(1./(RLB+RLN),ELM(K))
          EL(K)=1./(RLB+RLN)
        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
      END SUBROUTINE MIXLEN
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                          SUBROUTINE PRODQ2                            &
!----------------------------------------------------------------------
!   ******************************************************************
!   *                                                                *
!   *            LEVEL 2.5 Q2 PRODUCTION/DISSIPATION                 *
!   *                                                                *
!   ******************************************************************
!
     &(LMH,DTTURBL,USTAR,S2,RI,Q2,EL,Z,AKM,AKH                         &
     &,UXK,VXK,THXK,THVXK                                              &
     &,HGAMU,HGAMV,HGAMT                                               &
     &,HPBL,PBLFLG,KPBL                                                &
     &,ZFACENTK,UFXPBL,VFXPBL,HFXPBL                                   &
     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DTTURBL,USTAR
!
      REAL,DIMENSION(KTS+1:KTE),INTENT(IN) :: S2,RI,AKM,AKH,EL
!
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
!
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: Q2
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: UXK,VXK,THXK,THVXK
      REAL,INTENT(IN) :: HGAMU,HGAMV,HGAMT,HPBL
!
      INTEGER,INTENT(IN) :: KPBL
      LOGICAL,INTENT(IN) :: PBLFLG
!
      REAL,DIMENSION(KTS+1:KTE),INTENT(IN) :: ZFACENTK
      REAL,INTENT(IN) :: UFXPBL,VFXPBL,HFXPBL
!
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: S2L,Q2L,DELTAZ,AKML,AKHL,EN2,PR,BPR,DIS,RC02
      REAL :: SUK,SVK,GTHVK,GOVRTHVK,PRU,PRV
      REAL :: ZFACENTL
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      RC02=2.0/(C0*C0)
      main_integration: DO K=KTS+1,KTE
        DELTAZ=0.5*(Z(K+1)-Z(K-1))
        S2L=S2(K)
        Q2L=Q2(K)
        SUK=(UXK(K)-UXK(K-1))/DELTAZ
        SVK=(VXK(K)-VXK(K-1))/DELTAZ
        GTHVK=(THVXK(K)-THVXK(K-1))/DELTAZ
        GOVRTHVK=G/(0.5*(THVXK(K)+THVXK(K-1)))
        AKML=AKM(K)
        AKHL=AKH(K)
        ZFACENTL=ZFACENTK(K)
        EN2=RI(K)*S2L                                           !N**2
!
!***  TURBULENCE PRODUCTION TERM
!
        IF(PBLFLG.AND.K.LE.KPBL)THEN
          PRU=(AKML*(SUK-HGAMU/HPBL))*SUK
          PRV=(AKML*(SVK-HGAMV/HPBL))*SVK
          PRU=(AKML*(SUK-HGAMU/HPBL)-UFXPBL*ZFACENTL)*SUK
          PRV=(AKML*(SVK-HGAMV/HPBL)-VFXPBL*ZFACENTL)*SVK
        ELSE
          PRU=AKML*SUK*SUK
          PRV=AKML*SVK*SVK
        ENDIF
        PR=PRU+PRV
!
!***  BUOYANCY PRODUCTION
!
        IF(PBLFLG.AND.K.LE.KPBL)THEN
          BPR=(AKHL*(GTHVK-HGAMT/HPBL))*GOVRTHVK
          BPR=(AKHL*(GTHVK-HGAMT/HPBL)-HFXPBL*ZFACENTL)*GOVRTHVK
        ELSE
          BPR=AKHL*GTHVK*GOVRTHVK
        ENDIF
!
!***  DISSIPATION
!
        DIS=CEPS*(0.5*Q2L)**1.5/EL(K)
!
        Q2L=Q2L+2.0*(PR-BPR-DIS)*DTTURBL
        Q2(K)=AMAX1(Q2L,EPSQ2L)
!----------------------------------------------------------------------
!***  END OF PRODUCTION/DISSIPATION LOOP
!----------------------------------------------------------------------
!
      ENDDO main_integration
!
!----------------------------------------------------------------------
!***  LOWER BOUNDARY CONDITION FOR Q2
!----------------------------------------------------------------------
!
      Q2(KTS)=AMAX1(RC02*USTAR*USTAR,EPSQ2L)
!----------------------------------------------------------------------
!
      END SUBROUTINE PRODQ2
!
!----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
                           SUBROUTINE VDIFQ                            &
!   ******************************************************************
!   *                                                                *
!   *               VERTICAL DIFFUSION OF Q2 (TKE)                   *
!   *                                                                *
!   ******************************************************************
     &(LMH,DTDIF,Q2,EL,Z                                               &
     &,AKHK                                                            &
     &,HGAME,HPBL,PBLFLG,KPBL                                          &
     &,EFXPBL                                                          &
     &,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     &,IMS,IME,JMS,JME,KMS,KME                                         &
     &,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DTDIF
!
      REAL,DIMENSION(KTS+1:KTE),INTENT(IN) :: EL
      REAL,DIMENSION(KTS+1:KTE),INTENT(IN) :: AKHK
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: Z
!
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: Q2
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN)   :: HGAME
      REAL,INTENT(IN) :: HPBL
      INTEGER,INTENT(IN) :: KPBL
      LOGICAL,INTENT(IN) :: PBLFLG
!
      REAL,INTENT(IN) :: EFXPBL
!
!
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: K
!
      REAL :: ADEN,AKQS,BDEN,BESH,BESM,CDEN,CF,DTOZS,ELL,ELOQ2,ELOQ4   &
     &       ,ELQDZ,ESH,ESM,ESQHF,GHL,GML,Q1L,RDEN,RDZ
      REAL :: ZAK
!
      REAL,DIMENSION(KTS+2:KTE) :: AKQ,CM,CR,DTOZ,RSQ2
      REAL,DIMENSION(KTS+1:KTE) :: ZFACENTK
!
      REAL,PARAMETER :: C_K=1.0
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!***
!***  VERTICAL TURBULENT DIFFUSION
!***
!----------------------------------------------------------------------
      ESQHF=0.5*ESQ
      DO K=KTS+1,KTE
        ZAK=0.5*(Z(K)+Z(K-1)) !ZAK OF VDIFQ = ZA(K-1) OF SHINHONG2D
        ZFACENTK(K)=(ZAK/HPBL)**3.0
      ENDDO
!
      DO K=KTE,KTS+2,-1
        DTOZ(K)=(DTDIF+DTDIF)/(Z(K+1)-Z(K-1))
        AKQ(K)=C_K*(AKHK(K)/(Z(K+1)-Z(K-1))+AKHK(K-1)/(Z(K)-Z(K-2)))
        CR(K)=-DTOZ(K)*AKQ(K)
      ENDDO
!
        AKQS=C_K*AKHK(KTS+1)/(Z(KTS+2)-Z(KTS))
      CM(KTE)=DTOZ(KTE)*AKQ(KTE)+1.
      RSQ2(KTE)=Q2(KTE)
!
      DO K=KTE-1,KTS+2,-1
        CF=-DTOZ(K)*AKQ(K+1)/CM(K+1)
        CM(K)=-CR(K+1)*CF+(AKQ(K+1)+AKQ(K))*DTOZ(K)+1.
        RSQ2(K)=-RSQ2(K+1)*CF+Q2(K)
      IF(PBLFLG.AND.K.LT.KPBL) THEN
        RSQ2(K)=RSQ2(K)-DTOZ(K)*(2.0*HGAME(K)/HPBL)*AKQ(K+1)*(Z(K+1)-Z(K)) &
                       +DTOZ(K)*(2.0*HGAME(K-1)/HPBL)*AKQ(K)*(Z(K)-Z(K-1))
        RSQ2(K)=RSQ2(K)-DTOZ(K)*2.0*EFXPBL*ZFACENTK(K+1)                   &
                       +DTOZ(K)*2.0*EFXPBL*ZFACENTK(K)
      ENDIF
      ENDDO
!
      DTOZS=(DTDIF+DTDIF)/(Z(KTS+2)-Z(KTS))
      CF=-DTOZS*AKQ(LMH+2)/CM(LMH+2)
!
      IF(PBLFLG.AND.((LMH+1).LT.KPBL)) THEN
        Q2(LMH+1)=(DTOZS*AKQS*Q2(LMH)-RSQ2(LMH+2)*CF+Q2(LMH+1)         &
            -DTOZS*(2.0*HGAME(LMH+1)/HPBL)*AKQ(LMH+2)*(Z(LMH+2)-Z(LMH+1)) &
            +DTOZS*(2.0*HGAME(LMH)/HPBL)*AKQS*(Z(LMH+1)-Z(LMH)))
        Q2(LMH+1)=Q2(LMH+1)-DTOZS*2.0*EFXPBL*ZFACENTK(LMH+2)           &
                           +DTOZS*2.0*EFXPBL*ZFACENTK(LMH+1)
        Q2(LMH+1)=Q2(LMH+1)/((AKQ(LMH+2)+AKQS)*DTOZS-CR(LMH+2)*CF+1.)
      ELSE
        Q2(LMH+1)=(DTOZS*AKQS*Q2(LMH)-RSQ2(LMH+2)*CF+Q2(LMH+1))        &
       &        /((AKQ(LMH+2)+AKQS)*DTOZS-CR(LMH+2)*CF+1.)
      ENDIF
!
      DO K=LMH+2,KTE
        Q2(K)=(-CR(K)*Q2(K-1)+RSQ2(K))/CM(K)
      ENDDO
!----------------------------------------------------------------------
!
      END SUBROUTINE VDIFQ
!
!----------------------------------------------------------------------
end module module_bl_shinhong
!-------------------------------------------------------------------------------

!WRF:MODEL_LAYER:PHYSICS
!
! Grenier-Bretherton mixing PBL scheme (GBM PBL):
!
! REFERENCES
! 1) Grenier, H., and C. S. Bretherton, 2001: A moist PBL parameterization 
!    for large-scale models and its application to subtropical cloud-topped 
!    marine boundary layers. Mon. Wea. Rev., 129, 357-377. 
!    ("GB01" in the comments)
! 2) Galperin, B., L. H. Kantha, S. Hassid, and A. Rosati, 1988: 
!    A quasi-equilibrium turbulent energy model for geophysical flows. 
!    J. Atmos. Sci., 45, 55-62.
!    ("Gal88" in the comments)
! 3) Mellor, G., and T. Yamada, 1982: Development of a turbulence closure
!    model for geophysical fluid problems. Rev. Astrophys.Space Phys., 
!    20, 851-875.
!    ("MY82" in the comments)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Natalie Perlin, October 2012 nperlin@coas.oregonstate.edu
!! 
!! GBM PBL Code history:
!! 
!! Herve Grenier, Christopher Bretherton - original contribution
!! Jim McCaa - late 1990s, 2000-2001
!! Nicolai Thum  2004-2005(?)
!! Qingtao Song - 2007-2008
!! Natalie Perlin 2011-2012
!

MODULE module_bl_gbmpbl

   USE module_model_constants, ONLY: cp, g, rcp, r_d,            &
                                     r_v, svp1, svp2, svp3,      &
                                     svpt0, ep_1, ep_2, xlv,     &
                                     karman
public gbmpbl
public gbmpblinit
private

CONTAINS

  SUBROUTINE GBMPBL(U3D,V3D,TH3D,T3D,QV3D,QC3D,QI3D,P3D,PI3D,     &
       RUBLTEN,RVBLTEN,RTHBLTEN,                                  &
       RQVBLTEN,RQCBLTEN,RQIBLTEN,                                & 
       KZM_GBM,KTH_GBM,KETHL_GBM,EL_GBM,                          &
       dz8w,z,PSFC,TKE_PBL,RTHRATEN,                              &
       ZNT,UST,ZOL,HOL,PBL,KPBL2D,REGIME,PSIM,PSIH,               & 
       XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                          &
       DT,DTMIN,                                                  &
       ids,ide, jds,jde, kds,kde,                                 &
       ims,ime, jms,jme, kms,kme,                                 &
       its,ite, jts,jte, kts,kte                        )
    !-------------------------------------------------------------------
    IMPLICIT NONE
    !-------------------------------------------------------------------
    !-- U3D         3D u-velocity interpolated to theta points (m/s)
    !-- V3D         3D v-velocity interpolated to theta points (m/s)
    !-- TH3D	3D potential temperature (K)
    !-- T3D         temperature (K)
    !-- QV3D        3D water vapor mixing ratio (Kg/Kg)
    !-- QC3D        3D cloud mixing ratio (Kg/Kg)
    !-- QI3D        3D ice mixing ratio (Kg/Kg)
    !-- P3D         3D pressure (Pa)
    !-- PI3D	3D exner function (dimensionless)
    !-- RUBLTEN     Rho_dU tendency due to
    !               PBL parameterization (kg/m^3 . m/s)
    !-- RVBLTEN     Rho_dV tendency due to
    !               PBL parameterization (kg/m^3 . m/s)
    !-- RTHBLTEN    Rho_dTheta_m tendency due to
    !               PBL parameterization (kg/m^3 . K)
    !-- RQVBLTEN    Rho_dQv tendency due to
    !               PBL parameterization (kg/m^3 . kg/kg)
    !-- RQCBLTEN    Rho_dQc tendency due to
    !               PBL parameterization (kg/m^3 . kg/kg)
    !-- RQIBLTEN    Rho_dQi tendency due to
    !               PBL parameterization (kg/m^3 . kg/kg)
    ! 
    !-- KZM_GBM       exchange coefficient for momentum (m^2/s) 
    !-- KTH_GBM       exchange coefficient for heat (K m/s)
    !-- KETHL_GBM     exchange coeff. for TKE enhanced (m^2/s)
    ! 
    ! NB!! Vertical staggering of the GBM output variables 
    !  is as follows:
    !  KZM_GBM, KTH_GBM are on full levels, starting from the sfc 
    !  KETHL_GBM is on half-levels
    !  TKE_PBL and EL_GBM are on full levels, starting from the sfc
    !
    !-- cp          heat capacity at constant pressure for dry air (J/kg/K)
    !-- g           acceleration due to gravity (m/s^2)
    !-- rcp         R/CP
    !-- r_d         gas constant for dry air (J/kg/K)
    !-- P_QI	species index for cloud ice
    !-- dz8w	dz between full levels (m)
    !-- z		height above sea level (m)
    !-- XLV         latent heat of vaporization (J/kg)
    !-- r_v		gas constant for water vapor (J/kg/K)
    !-- PSFC        pressure at the surface (Pa)
    !-- ZNT		roughness length (m)
    !-- UST		u* in similarity theory (m/s)
    !-- ZOL		z/L height over Monin-Obukhov length
    !-- HOL		PBL height over Monin-Obukhov length
    !-- PBL		PBL height (m)
    !-- KPBL2D     Layer index containing PBL top within or at the base interface
    !-- REGIME	flag indicating PBL regime (stable, unstable, etc.)
    !-- PSIM        similarity stability function for momentum
    !-- PSIH        similarity stability function for heat
    !-- XLAND	land mask (1 for land, 2 for water)
    !-- HFX		upward heat flux at the surface (W/m^2)
    !-- QFX		upward moisture flux at the surface (kg/m^2/s)
    !-- TSK		surface temperature (K)
    !-- GZ1OZ0      log(z/z0) where z0 is roughness length
    !-- WSPD        wind speed at lowest model level (m/s)
    !-- BR          bulk Richardson number in surface layer
    !-- DT		time step (s)
    !-- DTMIN	time step (minute)
    !-- svp1        constant for saturation vapor pressure (kPa)
    !-- svp2        constant for saturation vapor pressure (dimensionless)
    !-- svp3        constant for saturation vapor pressure (K)
    !-- svpt0       constant for saturation vapor pressure (K)
    !-- ep_1        constant for virtual temperature (r_v/r_d - 1) (dimensionless)
    !-- ep_2        constant for specific humidity calculation
    !-- karman      Von Karman constant
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

    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte

    REAL,     INTENT(IN   )   ::      DT,DTMIN

    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(IN   )   ::                           QV3D, &
         QC3D, &
         QI3D, &
         P3D, &
         PI3D, &
         TH3D, &
         T3D, &
         dz8w, &
         z   

    !
    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(INOUT)   ::                          RUBLTEN, &
         RVBLTEN, &
         RTHBLTEN, &
         RQVBLTEN, &
         RQCBLTEN, &
         RQIBLTEN

    REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
         INTENT(OUT)   ::   KZM_GBM,KTH_GBM,KETHL_GBM, EL_GBM

    REAL,     DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(IN   )   ::                          XLAND, &
         HFX, &
         QFX, &
         REGIME

    REAL,     DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(INOUT)   ::                            HOL, &
         UST, &
         PBL, &
         ZNT
    INTEGER,  DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(INOUT)   ::                            KPBL2D

    !
!m The following 5 variables are changed to memory size from tile size--
!
     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::    &
                                                             PSIM, &
                                                             PSIH

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)   ::   &
                                                             WSPD

     REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::   &
                                                           GZ1OZ0, &
                                                               BR

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )               ::               PSFC

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(IN   )   ::                            TSK

      REAL,     DIMENSION( ims:ime, jms:jme )                    , &
                INTENT(INOUT)   ::                            ZOL
                                                                      
      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
                INTENT(IN   )   ::                            U3D, &
                                                              V3D

    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
         ,INTENT(INOUT) ::                    TKE_PBL
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                          &
         ,INTENT(IN) ::                    RTHRATEN

    ! LOCAL VARIABLES

    REAL,       DIMENSION( its:ite, kts:kte )          ::   dz8w2d, & 
         z2d,t2dten_ra

    INTEGER ::  I,J,K,NK,pass
    CHARACTER(LEN=200)        :: string
    REAL,DIMENSION(IMS:IME,KMS:KME)   :: TKE2d_1,TKE2d_2 ,          &
                                         u2dblten_1,u2dblten_2,     &
                                         v2dblten_1,v2dblten_2

    tke2d_1 = 0.0 
    tke2d_2 = 0.0 
    u2dblten_1 = 0.0 
    u2dblten_2 = 0.0 
    v2dblten_1 = 0.0 
    v2dblten_2 = 0.0 
    t2dten_ra  = 0.0 
    
    DO J=jts,jte
       DO k=kts,kte
          NK=kme-k
          DO i=its,ite
             dz8w2d(I,K) = dz8w(i,K,j)
             z2d(I,K) = z(i,NK,j)
             t2dten_ra(i,k) = RTHRATEN(i,k,j)*PI3D(I,K,J)
             !t2dten is flipped inside the code
! TKE_PBL = tke2d = 0.5 * (q**2) == e
! tke_2d_2 in gbmpbl is equivalent to "e" in GB01
             tke2d_2(i,k)= TKE_PBL(i,k,j) 
          ENDDO
       ENDDO

    do pass=1,2
       CALL GBM2D(J,U3D(ims,kms,j),V3D(ims,kms,j),T3D(ims,kms,j),&
            QV3D(ims,kms,j),QC3D(ims,kms,j),QI3D(ims,kms,j),     &
            P3D(ims,kms,j),u2dblten_2(ims,kms),v2dblten_2(ims,kms),&
            RTHBLTEN(ims,kms,j),RQVBLTEN(ims,kms,j),             &
            RQCBLTEN(ims,kms,j),RQIBLTEN(ims,kms,j),             &
            KZM_GBM(ims,kms,j),KTH_GBM(ims,kms,j),               &
            KETHL_GBM(ims,kms,j),EL_GBM(ims,kms,j),              &
            TKE2d_2(ims,kms),t2dten_ra,                          &
            dz8w2d,z2d,                                          &
            PSFC(ims,j),ZNT(ims,j),UST(ims,j),ZOL(ims,j),        &
            HOL(ims,j),PBL(ims,j),KPBL2D(ims,j),PSIM(ims,j),     &
            PSIH(ims,j),XLAND(ims,j),HFX(ims,j),QFX(ims,j),      &
            TSK(ims,j),GZ1OZ0(ims,j),WSPD(ims,j),BR(ims,j),      &
            DT,DTMIN,                                            &
            ids,ide, jds,jde, kds,kde,                           &
            ims,ime, jms,jme, kms,kme,                           &
            its,ite, jts,jte, kts,kte                            )
      if(pass==1)then
        TKE2d_1=tke2d_2 !tke at n+1
        u2dblten_1=u2dblten_2
        v2dblten_1=v2dblten_2
      end if
      if(pass==2)then
! tke_2d_2 in gbmpbl is equivalent to e == 2*(q**2)
        TKE_PBL(:,:,j)=tke2d_1 !otherwise tke is advanced to n+2
        rublten(:,:,j)=0.5*(u2dblten_2+u2dblten_1)
        rvblten(:,:,j)=0.5*(v2dblten_2+v2dblten_1)
      end if
    end do


       DO k=kts,kte
          DO i=its,ite
             RTHBLTEN(I,K,J)=RTHBLTEN(I,K,J)/PI3D(I,K,J)
          ENDDO
       ENDDO
    ENDDO

  END SUBROUTINE GBMPBL

  SUBROUTINE GBM2D(J,U2D,V2D,T2D,QV2D,QC2D,QI2D,P2D,    &
       U2DTEN,V2DTEN,T2DTEN,                            &
       QV2DTEN,QC2DTEN,QI2DTEN,                         & 
       KZM2D,KTH2D,KETHL2D,EL2D,                        &
       TKE2D,T2DTEN_RA,                                 &
       dz8w2d,z2d,PSFCPA,                               &
       ZNT,UST,ZOL,HOL,PBL,kpbl1d,PSIM,PSIH,            &
       XLAND,HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                &
       DT,DTMIN,                                        &
       ids,ide, jds,jde, kds,kde,                       &
       ims,ime, jms,jme, kms,kme,                       &
       its,ite, jts,jte, kts,kte                        )
    !-------------------------------------------------------------------
    implicit none
    !
    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte,J
    !
    REAL,     INTENT(IN   )   ::      DT,DTMIN
    REAL                      ::     SVP1PA

    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(IN   )   ::                           QV2D, &
         QC2D, &
         QI2D, &
         P2D, &
         T2D

    !
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(INOUT)   ::                         U2DTEN, &
         V2DTEN, &
         T2DTEN, &
         QV2DTEN, &
         QC2DTEN, &
         QI2DTEN, &
         tke2d
    !
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(OUT)   ::    KZM2d,KTH2d,KETHL2d,EL2D

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            HOL, &
         UST, &
         PBL, &
         ZNT

    INTEGER,  DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            kpbl1d

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )   ::                          XLAND, &
         HFX, &
         QFX
    !
    REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::      PSIM, &
         PSIH

    REAL,     DIMENSION( ims:ime ), INTENT(INOUT)   ::      WSPD

    REAL,     DIMENSION( ims:ime ), INTENT(IN   )   ::    GZ1OZ0, &
         BR

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )               ::             PSFCPA

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(IN   )   ::                            TSK

    REAL,     DIMENSION( ims:ime )                             , &
         INTENT(INOUT)   ::                            ZOL

    !
    REAL,     DIMENSION( its:ite, kts:kte ) ,                    &
         INTENT(IN)      ::                         dz8w2d, &
         z2d,t2dten_ra
    !                                                                                
    REAL,     DIMENSION( ims:ime, kms:kme )                    , &
         INTENT(IN   )   ::                            U2D, &
         V2D
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    !     PBL SCHEME                                                      C
    !     Questions? Contact mccaa@atmos.washington.edu                   C
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    !     
    !     Order of events:
    !     0.  Preliminaries:  Some things we only need to do once
    !     1.  First, save initial values and calculate some derivatives.
    !     4.  Calculate the buoyancy jump at flux levels
    !     5.  Calculate the pbl height and length scale
    !     6.  Compute diffusivity profiles
    !     7.  Implicit diffusion of thetal, total water
    !     8.  Implicit diffusion of momentum
    !     9.  Implicit diffusion of cloud ice
    !     10. Prediction of TKE
    !     11. Calculation of tendencies for output to model
    !     
    !     Local variables on full levels
    real zqx(kts:kte+2)
    REAL KTH(KTS:KTE+1),KZM(KTS:KTE+1),RHOXFL(KTS:KTE+1),tke(kts:kte+1),tkes(kts:kte+1), &
         rrhoxfl(kts:kte+1),BBLS(KTS:KTE+1),NSQUAR(KTS:KTE+1),BOUYAN(KTS:KTE+1), &
         DQWDZ(kts:kte+1),rdza(kts:kte+1),dza(kts:kte+1),SVS(KTS:KTE+1),presfl(kts:kte+1), &
         exnerfl(kts:kte+1),SHEAR(KTS:KTE+1),rexnerfl(kts:kte+1),rcldb(kts:kte+1), &
         epop(kts:kte+1),DTLDZ(KTS:KTE+1)
    !     Local variables on half levels
    REAL UX(KTS:KTE),VX(KTS:KTE),THX(KTS:KTE),QX(KTS:KTE),THVX(KTS:KTE),zax(kts:kte),qix(kts:kte), &
         KETHL(KTS:KTE),THLX(KTS:KTE),THXS(KTS:KTE),tx(kts:kte),tvx(kts:kte),rttenx(kts:kte), &
         PRESHL(KTS:KTE),QCX(KTS:KTE),QWX(KTS:KTE),dzq(kts:kte),rRHOXHL(KTS:KTE),UXS(KTS:KTE), &
         QXS(KTS:KTE),RHOXHL(KTS:KTE),exnerhl(kts:kte),rexnerhl(kts:kte),rdzq(kts:kte), &
         VXS(KTS:KTE),qixs(kts:kte),qcxs(kts:kte)
    REAL,     DIMENSION( its:ite ) :: wspd1
    REAL UFLXP,VFLXP,RHOXSF,Q0S, &
         RDT,dt2, &
         aone,atwo,czero,tskx, &
         tvcon,fracz,dudz,dvdz,rvls,thv0,dthv,xfr, &
         cpoxlv,r1cp,rczero, &
         templ,temps,gamcrt,gamcrq,cell, &
         rwspd,cfac, &
         thgb,pblx,gothv,capcd,capch,capcq, &
         ustx,ustxm,qfxx,hfxx,rlwp,tkeavg,wstar,xlvocp,wso,phih, &
         rstbl,vk,tbbls,pref
    integer i,k,l,iconv,ilay, &
         ktop(kts:kte),kpbl2dx,kmix2dx, &
         iteration,pass,kr,&
	 ktop_save(kts:kte) !NT new to store original height in case layers get merged
    !     Arrays for tridiagonal matrix solver
    real aimp(kts:kte),bimp(kts:kte),cimp(kts:kte)
    real uimp1(kts:kte),rimp1(kts:kte)
    real uimp2(kts:kte),rimp2(kts:kte)
    !NT some temporary variables to recompute n2
    real  THX_t,THVX_t,DTHV_t
    !     
    parameter(XFR=0.1)        !Fraction of turb layer to be considered in bbls
!    parameter(aone=1.9*xfr,atwo=15.,cfac=7.8)  orig, why is atwo 15!???? atn aone 1.9
    parameter(aone=1.9*xfr,atwo=10.,cfac=7.8) 
    parameter(czero=5.869)    ! b1/2**(3/2)
    parameter(rstbl=1.5)      ! empirical constant for l at stable interfaces
    PARAMETER (GAMCRT=3.,GAMCRQ=2.E-3)
    parameter(vk=0.4)
    parameter(pref = 100000.)
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    !     
    !     ---0---  Preliminaries:  Some things we only need to do once
    !     
    RDT=1./DT 
    dt2=dt 
    SVP1PA=svp1*10. ! NT actually this is svp1 in mb
    cpoxlv = cp/xlv
    xlvocp = xlv/cp
    r1cp = 1./cp
    rczero = 1./czero

    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    !     This is the beginning of big I loop
    !     
    !     ---1---   First, save initial values and calculate some derivatives.
    !     
    DO I=its,ite
       !     Copy in j-dependent variables
       tskx = tsk(i)
       pblx = pbl(i)
       qfxx = qfx(i)
       hfxx = hfx(i)
       zqx(kte+1)=0.0
       zqx(kte+1+1)=0.0
       tke(kte+1) = 0.0
       DO K = kte,kts,-1
          KR = kte+1-K
          zqx(k)= zqx(k+1) + dz8w2d(i,kr)
          zax(k)=0.5*(zqx(k)+zqx(k+1))
          tke(k) = tke2d(i,kr)
          !     Wind components
          UX(K)=U2D(I,KR)  !
          VX(K)=V2D(I,KR)  !
          TX(K)=T2D(I,KR)
          QX(K)=QV2D(I,KR)
          QCX(K)=QC2D(I,KR)
          qix(k)=qi2D(i,kr)
          rttenx(k)=t2dten_ra(i,kr)
       ENDDO
       !     Done copying j-dependent variables
       tkes(kte+1) = tke(kte+1)
       do k = kts,kte
          kr = kte+1-k
          !     Pressure at half levels
          PRESHL(K)=p2d(i,kr)
          DZQ(K)=ZQX(K)-ZQX(K+1)
          rdzq(k)=1./dzq(k)
          exnerhl(k)=(preshl(k)/pref)**rcp
          rexnerhl(k)=1./exnerhl(k)
          THX(K)=TX(K)*rexnerhl(k)
          QWX(K) = QX(K) + QCX(K)
          TVCON=(1.+ep_1*QX(K)-qcx(k))
          TVX(K)=TX(K)*TVCON  ! virtual temperature
          THVX(K)=THX(K)*TVCON
          THLX(K) = THX(K) - XLVOCP * rexnerhl(K) * QCX(K)
          !     SAVE INITIAL VALUES of winds, thx, and qx
          tkes(k) = tke(k)
          UXS(K)  = UX(K)
          VXS(K)  = VX(K)
          THXS(K) = THX(K)
          QXS(K) = QX(K)
          qixs(k) = qix(k)
          qcxs(k) = qcx(k)
          !     Density at half levels
          RHOXHL(K)=PRESHL(K)/(r_d*TVX(K))
          rrhoxhl(k)=1./rhoxhl(k)
       enddo
       !     Density and vertical derivatives at the full sigma levels.
       presfl(1)=0.
       DO K = 2, kte
          kr = kte+1-k
          !     Pressure at full levels
          PRESFL(K)=exp(0.5*(log(p2d(i,kr+1))+log(p2d(i,kr))))
          epop(k)=ep_2/presfl(k)
          DZA(K)=ZAX(K-1)-ZAX(K)
          rdza(k)=1./dza(k)
          exnerfl(k)=(presfl(k)/pref)**rcp
          rexnerfl(k)=1./exnerfl(k)
          FRACZ=(ZQX(K)-ZAX(K))*RDZA(K)
          RHOXFL(K)=RHOXHL(K)+(RHOXHL(K-1)-RHOXHL(K))*FRACZ
          RRHOXFL(K)=1./RHOXFL(K)
          DUDZ =  (UX(K-1)  - UX(K))  *RDZA(K)
          DVDZ =  (VX(K-1)  - VX(K))  *RDZA(K)
          SVS(K)= DUDZ*DUDZ + DVDZ*DVDZ
          DQWDZ(K) =  (QWX(K-1)  - QWX(K))  *RDZA(K)
          DTLDZ(K) =  (THLX(K-1)  - THLX(K))  *RDZA(K)
       ENDDO
!write(*,*)'dzq', dzq
!write(*,*)'dza', dza
       !     Pressure at the surface (in centibars)
       PRESFL(KTE+1)=psfcpa(i)
       rexnerfl(kte+1)=(pref/presfl(kte+1))**rcp
       exnerfl(kte+1)=1./rexnerfl(kte+1)
       !     Saturation Mixing Ratio at Surface
       q0s=ep_2/(PRESFL(KTE+1)/(100.*SVP1PA*EXP(svp2*(tskx-svpt0)/(tskx-svp3)))-1.)
       !     COMPUTE SOIL POTENTIAL TEMPERATURE
       THGB = TSKX * rexnerfl(kte+1)
       !     DENSITY AT THE SURFACE
       RHOXSF=(PRESFL(KTE+1))/(r_d*TVX(kte))
       !     More surface variables
       WSPD1(i)=SQRT(UX(KTE)*UX(KTE)+VX(KTE)*VX(KTE))+1.e-4
       THV0=THGB*(1.+ep_1*Q0S)
       DTHV=(THVX(KTE)-THV0)
       gothv = g/thvx(kte)
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       ustx = wspd1(i)*vk/(gz1oz0(i)-psim(i))
       ustxm = amax1(ustx,0.1)
       if ((xland(i)-1.5).lt.0)  ustx = ustxm   
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     
       !     ---4---   Calculate the buoyancy jump at flux levels
       !     
       call n2(thlx,exnerfl,epop,qwx,cpoxlv,rdza,            &
            rexnerfl,kts,kte,nsquar,rcldb,xlvocp,svp1pa)
       nsquar(kte+1)=  g/thvx(kte) * dthv / zax(kte)
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---5--- Calculate the pbl height and length scale
       !     
       call pblhgt( &
            ! input variables &
       zqx,kts,kte, &
            nsquar,tke,presfl,rhoxfl,rcldb,exnerfl, &
            rttenx,thvx,thlx,thx,qwx,rexnerhl,qcx, &
            xfr,xlvocp,aone,atwo,rstbl,            &
            ! output variables &
       bbls,pblx, &
            ktop,iconv,kmix2dx,kpbl2dx)
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---6---   Compute diffusivity profiles
       !     
       do pass = 1,2
          call my( &
               ! Input variables &
          bbls,nsquar,tke,iconv,ktop,thlx,thx,qwx,     &
               xlvocp,rcldb,rexnerhl,aone,atwo,kts,kte, &
               ! Output variables &
          kzm,kth,kethl )
          !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          !     
          !     ---7---    Implicit diffusion of thetal, total water
          !     
          !     First find the coefficients that apply to all scalars at half-levels
          do k = 1, kte
             if(k.eq.1)then
                aimp(k)=0.
             else
                aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                     kth(k) * dt2 *rdzq(k)*rdza(k)
             endif
             if(k.eq.kte)then
                cimp(k)=0.
             else
                cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                     kth(k+1) * dt2 *rdzq(k)*rdza(k+1)
             endif
             bimp(k) = 1 - aimp(k) - cimp(k)
             !     Now find right side for various scalars:
             !     No flux out top, so no (k.eq.1)
             if(k.eq.kte)then  ! at surface
                !     Include surface latent heat flux
                rimp2(k) = qwx(k) + dt2 * qfxx*rrhoxhl(k)*rdzq(kte)
                !     Include surface sensible heat flux
                rimp1(k) = thlx(k) + &
                     dt2 * hfxx*rrhoxhl(k)*r1cp*rdzq(kte)*rexnerhl(kte)
             else
                rimp2(k) = qwx(k)
                rimp1(k) = thlx(k)
             endif
          enddo
          call tridag(aimp,bimp,cimp,rimp1,uimp1,kte)
          call tridag(aimp,bimp,cimp,rimp2,uimp2,kte)
          !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          !     
          !     Recompute the buoyancy jump at flux levels
          !     
          call n2(uimp1,exnerfl,epop,uimp2,cpoxlv,rdza,       &
	       rexnerfl, kts,kte,nsquar,rcldb,xlvocp,svp1pa)
	       !NT new recompute also n2 at sfc!!
               ! THLX(K) = THX(K) - XLVOCP * rexnerhl(K) * QCX(K)
               THX_t = uimp1(KTE) + XLVOCP * rexnerhl(KTE) * QCX(KTE)
               THVX_t=THX_t*TVCON
               DTHV_t=(THVX_t-THV0)
               nsquar(kte+1)=  g/thvx_t * dthv_t / zax(kte)
	       !NT end
          !     Update only nsquar, then go back
       !TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
       !
       !       Recompute pbl-height? since n2 changes; use uimp1 instead thlx
       !       TEST!!!
       !
!NT       call pblhgt( &
!NT            ! input variables &
!NT       zqx,kts,kte, &
!NT            nsquar,tke,presfl,rhoxfl,rcldb,exnerfl, &
!NT            rttenx,thvx,uimp1,thx,qwx,rexnerhl,qcx, &
!NT            xfr,xlvocp,aone,atwo,rstbl, &
!NT            ! output variables &
!NT       bbls,pblx, &
!NT            ktop,iconv,kmix2dx,kpbl2dx)
       !TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
       enddo
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     Update the scalars (only after second pass)
       !     
       !     Calculate new values of thx, qx and qcx using new values of thlx and qcx.
       do  k = 1, kte
          thlx(k) = uimp1(k)
          qwx(k) = uimp2(k)
          if(k.ge.2) DQWDZ(K) = (QWX(K-1) - QWX(K)) *RDZA(K)
          if(k.ge.2) DTLDZ(K) = (THLX(K-1) - THLX(K)) *RDZA(K)
          templ = thlx(k)*exnerhl(k)
          temps = templ
          rvls = ep_2/ &
               (preshl(k)/(100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3)))-1.)
          do iteration = 1, 3
             temps = temps  + ((templ-temps)*cp/xlv + qwx(k)-rvls)/ &
                  (cp/xlv+ep_2*xlv*rvls/r_d/temps/temps)
             rvls = ep_2/ &
                  (preshl(k)/(100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3)))-1.)
          enddo
          qcx(k)=max(qwx(k)-rvls,0.)
          qx(k) = qwx(k) - qcx(k)
          thx(k) = (templ+qcx(k)*xlv/cp) / exnerhl(k) ! theta_l
          THVX(K)=THX(K)*(1.+ep_1*QX(K)-QCX(K)) !NT is this GB01:A13 then it should
!NT       qx=q_v ; qwx=q_t; qcx=q_l
!nt thvx is theta virtual
       ENDDO
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---8--- Implicit diffusion of momentum
       !     
       !     First find the coefficients that apply to winds at half-levels
       do k = 1, kte
          if(k.eq.1)then
             aimp(k)=0.
          else
             aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                  kzm(k) * dt2 *rdzq(k)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k)=0.
          else
             cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                  kzm(k+1) * dt2 *rdzq(k)*rdza(k+1)
          endif
          bimp(k) = 1 - aimp(k) - cimp(k)
          !     Now find right side 
          !     No flux out top, so no (k.eq.1)
          if(k.eq.kte)then
             !     At surface
             UFLXP=-USTX*USTX*UX(KTE)/WSPD1(i)
             VFLXP=-USTX*USTX*VX(KTE)/WSPD1(i)
             !     Include surface momentum fluxes
             rimp1(k) = ux(k) + dt2 *  &
                  uflxp * (rhoxsf*rrhoxhl(k)) *rdzq(kte)
             rimp2(k) = vx(k) + dt2 *  &
                  vflxp * (rhoxsf*rrhoxhl(k)) *rdzq(kte)
          else
             rimp1(k) = ux(k)
             rimp2(k) = vx(k)
          endif
       enddo
       call tridag(aimp,bimp,cimp,rimp1,uimp1,kte)
       call tridag(aimp,bimp,cimp,rimp2,uimp2,kte)
       !     Update the winds
       do  k = 1, kte
          ux(k) = uimp1(k)
          vx(k) = uimp2(k)
          if(k.ge.2)then
             DUDZ =  (UX(K-1)  - UX(K))  *RDZA(K)
             DVDZ =  (VX(K-1)  - VX(K))  *RDZA(K)
             SVS(K)= DUDZ*DUDZ + DVDZ*DVDZ
          endif
       enddo
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---9---    Implicit diffusion of cloud ice
       !     
       !     First find the coefficients that apply to all scalars at half-levels
       do k = 1, kte
          if(k.eq.1)then
             aimp(k)=0.
          else
             aimp(k)=-(rhoxfl(k)*rrhoxhl(k))* &
                  kth(k) * dt2 *rdzq(k)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k)=0.
          else
             cimp(k)=-(rhoxfl(k+1)*rrhoxhl(k))* &
                  kth(k+1) * dt2 *rdzq(k)*rdza(k+1)
          endif
          bimp(k) = 1 - aimp(k) - cimp(k)
          !     Now find right side for various scalars:
          !     No flux out top, so no (k.eq.1)
          !     No flux in bottom, so no (k.eq.kte)
          rimp1(k) = qix(k)
          !            rimp2(k) = qncx(k)
       enddo
       call tridag(aimp,bimp,cimp,rimp1,qix,kte)
       !         call tridag(aimp,bimp,cimp,rimp2,qncx,kte)
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---10---   Prediction of TKE
       !     
       !     Features:
       !     a. Explicit calculation of buoyancy and shear terms using
       !     time=t+1 values of thetal, qw, and winds
       !     b. Surface TKE is diagnosed
       !     c. Semi-implicit calculation of dissipation term and
       !     implicit calculation of turbulent transfer term 
       !     First, buoyancy and shear terms
       DO K=2,KTE
          !     Compute buoyancy term with new values of thetal
          BOUYAN(K)=-KTH(K)*nsquar(K)
          !     Compute shear term with new values of svs
          SHEAR(K)  = KZM(K) * SVS(K)
       ENDDO
       do ilay = 1,iconv
          k = ktop(ilay)
          if(qcx(k).gt.1e-8.and.k.gt.1) bouyan(k) = bouyan(k) - & !NT GB01:A14
               rttenx(k)*(presfl(k+1)-presfl(k)) * rrhoxfl(k) &
               * rexnerfl(k) / thvx(k) 
       enddo
       !     TKE at top is fixed
       tke(1)=0.
       bbls(1)= 0.0
       !     Diagnose TKE at surface, following MY 82, B1 ** (2/3) / 2 = 3.25
       tke(kte+1)=3.25 * ustx * ustx ! normal
       !     tke(kte+1)=.33*wstar**2 ! dryrun
       !     Now the implicit calculations
       !     First find the coefficients that apply for full levels
       do k = 2, kte
          if(k.eq.2)then
             aimp(k-1)=0.
          else
             aimp(k-1)=-(rhoxhl(k-1)*rrhoxfl(k))* &
                  kethl(k-1)*dt2*rdzq(k-1)*rdza(k)
          endif
          if(k.eq.kte)then
             cimp(k-1)=0.
             !     Account for implicit part of flux between level kte and surface
             if(bbls(k).gt.0)then
                bimp(k-1) = 1 - aimp(k-1) - cimp(k-1) + &
                     dt2 * ( sqrt(tke(k))*rczero/bbls(k) + &
                     rhoxhl(k)*rrhoxfl(k)*kethl(k)*rdzq(k)*rdza(k) )
             else
                bimp(k-1) = 1 - aimp(k-1) - cimp(k-1) + &
                     dt2 * rhoxhl(k)*rrhoxfl(k)* kethl(k)*rdzq(k)*rdza(k)
             endif

          else
             cimp(k-1)=-(rhoxhl(k)*rrhoxfl(k))* &
                  kethl(k)*dt2*rdzq(k)*rdza(k)
             tbbls = max(bbls(k),bbls(k+1))
             if(tbbls.gt.0)then
                bimp(k-1)= 1 - aimp(k-1) - cimp(k-1) + dt2 * sqrt(tke(k))*rczero/tbbls
             else
                bimp(k-1)= 1 - aimp(k-1) - cimp(k-1)
             endif
          endif
          !     Now find right side 
          if(k.eq.kte)then
             !     Account for fixed part of flux between level kte and surface
             rimp1(k-1) = tke(k) + dt2 * ( SHEAR(K)+BOUYAN(K) + &
                  tke(kte+1)*(rhoxhl(k)*rrhoxfl(k))*kethl(k)*rdzq(k)*rdza(k) )
          else
             rimp1(k-1) = tke(k) + dt2 * (SHEAR(K)+BOUYAN(K))
          endif
       enddo

       call tridag(aimp,bimp,cimp,rimp1,uimp1,kte-1)
       !     Update the tke
       do  k = 2, kte
          tke(k) = max(uimp1(k-1),0.001) ! background tke .001
       enddo
       !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       !     
       !     ---11---   Calculation of tendencies for output to model
       !     

       DO K=KTS,KTE
          kr = kte+1-k
          U2DTEN(I,KR)= (UX(K)-UXS(K))*RDT
          V2DTEN(I,KR)=(VX(K)-VXS(K))*RDT
          T2DTEN(I,KR)=(THX(K)-THXS(K))*EXNERHL(K)*RDT
          QV2DTEN(I,KR) = (QX(K)-QXS(K))*RDT
          qi2Dten(i,kr) = (qix(k)-qixs(k))*rdt
          qc2Dten(i,kr) = (qcx(k)-qcxs(k))*rdt
          tke2d(i,kr)=tke(k)
          kzm2d(i,kr)=kzm(k)
          kth2d(i,kr)=kth(k)
          kethl2d(i,kr)=kethl(k)
          EL2D(i,kr) = bbls(k)
       ENDDO
       !     Copy out j-dependent variables
       pbl(i)=pblx
       ust(i)=ustx
       kpbl1d(i)=kte+1-kpbl2dx
    end do
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    RETURN
  END SUBROUTINE GBM2D
  !CCCCCCCCCCCCCCCCCCCCC END OF PBL SUBROUTINE CCCCCCCCCCCCCCCCCCCCCCCCC
  !
  subroutine tridag(a,b,c,r,u,n)
    !     Solves tridiagonal matrix
    !     See "Numerical Recipes in Fortran 77", p. 42
    implicit none
    integer n,nmax
    real a(n),b(n),c(n),r(n),u(n)
    parameter (nmax=100)
    integer j
    real rbet,gam(nmax)
    rbet=1./b(1)
    u(1)=r(1)*rbet
    do j=2,n
       gam(j)=c(j-1)*rbet
       rbet=1./(b(j)-a(j)*gam(j))
       u(j)=(r(j)-a(j)*u(j-1))*rbet
    end do
    do  j=n-1,1,-1
       u(j)=u(j)-gam(j+1)*u(j+1)
    end do
    return
  end subroutine tridag
      
  subroutine pblhgt( &
       ! input variables
       zqx,kts,kte, &
       nsquar,tke,presfl,rhoxfl,rcldb,exnerfl, &
       rttenx,thvx,thlx,thx,qwx,rexnerhl,qcx, &
       xfr,xlvocp,aone,atwo,rstbl,            &
       ! output variables
       bbls,pblx, &
       ktop,iconv,kmix2dx,kpbl2dx &
       )
    
    implicit none
    !     Input variables
    real zqx(kts:kte+2)
    real nsquar(kts:kte+1),tke(kts:kte+1),presfl(kts:kte+1),rhoxfl(kts:kte+1), &
         rcldb(kts:kte+1),exnerfl(kts:kte+1)
    real rttenx(kts:kte),thvx(kts:kte),thlx(kts:kte),thx(kts:kte),qwx(kts:kte), &
         rexnerhl(kts:kte),qcx(kts:kte)
    real xfr,xlvocp,aone,atwo,rstbl
    !     Output variables
    real bbls(kts:kte+1), pblx
    integer ktop(kts:kte+1), iconv,kmix2dx,kpbl2dx, &
            ktop_save(kts:kte+1)

    !     Local variables
    integer kbot(kts:kte+1),kts,kte,kstart
    integer istabl,ibeg,ilay,nlev,k,itemp
    real blinf,rnnll,tkeavg,trnnll,radnnll,delthvl,elambda, &
         bige,biga,entnnll,tbbls

    !     Find noncontiguous convectively unstable layers
    iconv = 0
    istabl = 1
    do k=2,kte+1   !nt nscquar is defined at kte+1 after the call to n2
       if(nsquar(k).le.0)then   
          if(istabl.eq.1)then
             iconv = iconv + 1
             ktop(iconv)=k
          endif
          istabl=0
          kbot(iconv)=k
       else
          istabl=1
          BBLS(K) = MIN(rstbl*SQRT(TKE(K)/NSQUAR(K)),karman*ZQX(K))
       endif
    enddo
    !NT new
!     if(ktop(iconv).eq.kte+1 .and. kbot(iconv).eq.kte+1)then
!	     iconv=iconv-1 !NT don't let the bottome layer be a convective top.
!     end if
    !NT end
	      
    !     Now see if they have sufficient buoyant convection to connect
!NT save ktops to be able to reset top of a merged layers
   ktop_save=ktop
!NT put this if-statement in to make sure;
  IF(iconv.ge.1)THEN
!NT
    ibeg = 1
2745 do ilay = ibeg, iconv
       blinf = xfr*(zqx(ktop(ilay)-1) - zqx(kbot(ilay)+1))
       !     Find average n*n*l*l for layer
       rnnll = 0.
       tkeavg = 0.
       nlev = kbot(ilay)-ktop(ilay)+1
       do k = ktop(ilay), kbot(ilay)
          bbls(k) = min(blinf,karman*ZQX(K))
          rnnll = rnnll + nsquar(k)*bbls(k)*bbls(k)   !NT is not averaged, but correct
          tkeavg = tkeavg + tke(k) / nlev
       enddo
       !     First extend up
       kstart=ktop(ilay)-1
!NT orig       do k = ktop(ilay)-1,2,-1
       do k = kstart,2,-1
          ktop(ilay)=k        ! We always go up at least one, for the entrainment interface
          bbls(k) = min(karman * ZQX(K),blinf)
          trnnll = nsquar(k)*bbls(k)*bbls(k)
          if(trnnll*nlev.ge.-0.5*rnnll) goto 2746 ! Is it the top?
!NT test          if(trnnll*nlev.ge.-0.7*rnnll) goto 2746 ! Requires stronger jump to be top
!NT doesn't workkpbl           if(trnnll*nlev.ge.-0.5*rnnll.and. &
!NT n2 gets recomputed      nsquar(k).ge.1.e-6) goto 2746 ! Avoids weak layers with even weaker tops
!NT after pblhgt                           ! Imagine n2 small negative over several layers
                                           ! then average rnnll is very small and a very weak 
                                           ! pos n2 is enough to be 'inversion' top. This makes
                                           ! sure that we go up at least one more.
          if(ilay.gt.1) then
             if(ktop(ilay).eq.kbot(ilay-1))then ! did we merge with layer above?
                ibeg = ilay - 1
   !NT orig     ktop(ibeg)=ktop(ibeg)+1 !NT not correct if one up was not inversion, the new thicker
                                        !NT layer might have different average properties, should
                                        !NT reset to original ktop
                ktop(ibeg)=ktop_save(ibeg) !NT new
                kbot(ibeg)=kbot(ibeg+1)
                iconv = iconv - 1
                do itemp = ibeg+1,iconv !NT if there's a layer below decrease layer index
                   ktop(itemp)=ktop(itemp+1)
                   kbot(itemp)=kbot(itemp+1)
                   ktop_save(itemp)=ktop_save(itemp+1) !NT new
                enddo
                goto 2745        ! recompute for the new, deeper layer
             endif
          endif
          rnnll = rnnll + trnnll
          nlev = nlev + 1 !NT moved up
       enddo
       !     Add radiative/entrainment contribution to total
2746   k = ktop(ilay) 
       radnnll = 0.
       if(qcx(k).gt.1e-8) radnnll =  &
            rttenx(k)*(presfl(k+1)-presfl(k))/ &
            (rhoxfl(k)*thvx(k)*exnerfl(k))
       entnnll = 0.
       if(k.ge.3)then
          delthvl = (thlx(k-2)+thx(k-2)*ep_1*qwx(k-2)) &
               - (thlx(k) + thx(k)*ep_1*qwx(k))
          elambda = xlvocp*rcldb(k)*rexnerhl(k)/max(delthvl,0.1)
          bige = 0.8 * elambda
          biga = aone * (1 + atwo * bige)
          entnnll = biga * sqrt(tkeavg**3) / bbls(k)
       endif
       if(tkeavg.gt.0.) rnnll = rnnll + min(0.,bbls(k)/sqrt(tkeavg) * (radnnll + entnnll) )
       !     Now extend down
       do k = kbot(ilay)+1,kte+1
          tbbls = min(karman * ZQX(K),blinf)
          trnnll = nsquar(k)*tbbls*tbbls
          if(trnnll*nlev.ge.-0.5*rnnll)goto 2747 !  Is it the bottom?
          kbot(ilay)=k
          if(ilay.lt.iconv.and.kbot(ilay).eq.ktop(ilay+1))then ! did we merge with layer below?
!NT orig             ktop(ilay)=ktop(ilay)+1
             ktop(ilay)=ktop_save(ilay)
             kbot(ilay)=kbot(ilay+1)
             iconv = iconv - 1
             do itemp = ilay+1,iconv
                ktop(itemp)=ktop(itemp+1)
                kbot(itemp)=kbot(itemp+1)
                ktop_save(itemp)=ktop_save(itemp+1)
             enddo
             goto 2745        ! recompute for the new, deeper layer
          endif
          rnnll = rnnll + trnnll
          bbls(k)=tbbls
          nlev = nlev + 1
       enddo
2747   continue
    enddo !NT all sorting is done, go through layers and calk l
    do ilay = 1, iconv
       blinf = xfr*(zqx(ktop(ilay)-1) - zqx(kbot(ilay)+1))
       do k = ktop(ilay),kbot(ilay)
          bbls(k) = min(karman * ZQX(K),blinf)
       enddo
    enddo
!NT added to check if iconv .ge. 1
 ENDIF  
!NT end
    !     We should now have tops and bottoms for iconv layers
    ! NT not clear how kpbl2dx should work, but doesn't matter since
    ! NT only kmix2dx is used no matter what kpbl2dx is.
    ! NT Looks like it could be used to choose between mixing pbl and
    ! NT convective pbl height if there are more than 1 unstable layers
			
    if(iconv.gt.0)then
       if(kbot(iconv).eq.kte+1)then
          kmix2dx = ktop(iconv)
          if(kpbl2dx.ge.0)then
             if(iconv.gt.1)then
                kpbl2dx = ktop(iconv-1)
             else
                kpbl2dx = kmix2dx
             endif
          else
             kpbl2dx=-kpbl2dx
          endif
       else
          kmix2dx = kte
          if(kpbl2dx.ge.0)then
             kpbl2dx = ktop(iconv)
          else
             kpbl2dx = -kpbl2dx
          endif
       endif
    else
       kmix2dx = kte
       if(kpbl2dx.ge.0)then
          kpbl2dx = kmix2dx
       else
          kpbl2dx = -kpbl2dx
       endif
    endif
    pblx=ZQX(KMIX2DX)
    return
  end subroutine pblhgt


  subroutine roots(a,b,c,r1,r2)
    implicit none
    real a,b,c,r1,r2,q
    if(a.eq.0)then            ! form b*x + c = 0
       if(b.eq.0)then         ! failure: c = 0
          r1 = -9.99e33
       else                   ! b*x + c = 0
          r1 = -c / b
       endif
       r2 = r1
    else
       if(b.eq.0.)then        ! form a*x**2 + c = 0
          if(a*c.gt.0.)then   ! failure: x**2 = -c/a < 0
             r1 =  -9.99e33
          else                ! x**2 = -c/a 
             r1 = sqrt(-c/a)
          endif
          r2 = -r1
       else                   ! form a*x**2 + b*x + c = 0
          if((b**2 - 4*a*c).lt.0.)then ! failure, no real roots
             r1 =  -9.99e33
             r2 = -r1
          else
             q = - 0.5 * ( b + sign(1.0,b) * &
                  sqrt(b**2 - 4*a*c) )
             r1 = q/a
             r2 = c/q
          endif
       endif
    endif
    return
  end subroutine roots

  subroutine n2(thlx,exnerfl,epop,qwx,cpoxlv,rdza,        &
       rexnerfl, kts,kte, nsquar,rcldb,xlvocp, svp1pa)
    implicit none
    !     Input/output variables
    real thlx(kts:kte),exnerfl(kts:kte+1),epop(kts:kte+1),qwx(kts:kte), &
         rexnerfl(kts:kte+1),rdza(kts:kte+1),nsquar(kts:kte+1),rcldb(kts:kte+1)
    real cpoxlv,xlvocp,svp1pa
    !     Local variables
    real templ,rvls,temps,tempv,tvbl,rcld,tvab,thvxfl,dtvdz
    integer k,kts,kte

    DO K = 2, KTE
       !     Buoyancy is jump in thetav across flux level/dza
       !     First, layer below, go up and see if anything condenses.
       templ = thlx(k)*exnerfl(k)
       rvls  = 100.*svp1pa*EXP(svp2*(templ-svpt0)/(templ-svp3))*epop(k)
       temps=templ + (qwx(k)-rvls)/(cpoxlv + &
            ep_2*xlv*rvls/(r_d*templ**2))
       rvls  = 100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3))*epop(k)
       rcldb(k)=max(qwx(k)-rvls,0.)
       tempv = (templ + xlvocp*rcldb(k)) * &
            (1 + ep_1*(qwx(k)-rcldb(k)) - rcldb(k))
       tvbl = tempv*rexnerfl(k) 
       !     Now do layer above; go down to see how much evaporates
       templ = thlx(k-1)*exnerfl(k)
       rvls  = 100.*svp1pa*EXP(svp2*(templ-svpt0)/(templ-svp3))*epop(k)
       temps=templ+(qwx(k-1)-rvls)/(cpoxlv+ &
            ep_2*xlv*rvls/(r_d*templ**2))
       rvls  = 100.*svp1pa*EXP(svp2*(temps-svpt0)/(temps-svp3))*epop(k)
       rcld=max(qwx(k-1)-rvls,0.)
       tempv = (templ + xlvocp*rcld) * &
            (1 + ep_1*(qwx(k-1)-rcld) - rcld)
       tvab = tempv*rexnerfl(k) 
       !     
       thvxfl= .5 * (tvab+tvbl)
       dtvdz = (tvab - tvbl) *rdza(k)
       nsquar(k) = g/thvxfl * dtvdz
    ENDDO
    nsquar(1)=nsquar(2)
    return
  end subroutine n2

  subroutine my( &
       ! Input variables &
    bbls,nsquar,tke,iconv,ktop,thlx,thx,qwx,     &
         xlvocp,rcldb,rexnerhl,aone,atwo,kts,kte, &
         ! Output variables &
         kzm,kth,kethl)
    implicit none
    real bbls(kts:kte+1),nsquar(kts:kte+1),tke(kts:kte+1),thlx(kts:kte),thx(kts:kte), &
         qwx(kts:kte),kzm(kts:kte+1),kth(kts:kte+1),kethl(kts:kte)
    real xlvocp,rcldb(kts:kte+1),rexnerhl(kts:kte),aone,atwo
    integer iconv,ktop(kts:kte)
    !     Local variables
    real gh,a1,b1,c1,a2,b2,a1ob1,nuk,delthvl,elambda,bige,biga,kmax,n2min
    real sm(kts:kte+1),sh(kts:kte+1)
    integer k,ilay,kts,kte
    parameter(A1=0.92,B1=16.6,C1=0.08,A2=0.74,B2=10.1)
    parameter(nuk=5.0)        !multiplier for kethl, GB01 value

    parameter(kmax=1000.)      ! max for kethl 
    parameter(n2min=1.e-7)     ! min for n2
    a1ob1 = a1/b1

    !     Calculate the diffusivities for momentum, thetal, qw and tke
    !     KTH and KZM are at full levels.
    !     KETHL is at half levels. NT: BMG03:6/7
    kzm(kte+1)=0.
    kth(kte+1)=0.
    kzm(1)=0.
    kth(1)=0.
    sm(kte+1)=1.
    sh(kte+1)=1.
    sm(1) = 1. 
    sh(1) = 1. 
    DO K = KTE, 2, -1
! According on how the TKE equiation is computed, the "tke" variable in gbmpbl !NP
! is equivalent to "e" in GB01 paper, or e==(q^2 / 2)  in MY82 or Gal88.       !NP
! Calculations of gh (MY82, Eq.33(b) and Gal88, Eq.26) contain q^2;            !NP
! therefore, gh in the code below should contain (2*tke)                       !NP
       gh =-bbls(k)*bbls(k)*nsquar(k)/(2*tke(k)+1e-9) ! MY82:33b/ Gal88:26
       gh = min(gh,0.0233)
       gh = max(gh,-0.28)  ! according to Gal88:30;  -0.28 < gh < 0.0233
       sm(k) = a1 * (1. - 3.*c1 - 6.*a1ob1 - 3.*a2*gh* &
            ((b2-3.*a2)*(1.-6.*a1ob1) - 3.*c1*(b2+6.*a1))) / &
            ((1. - 3.*a2*gh*(6.*a1 + b2)) * (1. - 9.*a1*a2*gh))
       sh(k) = a2 * (1. - 6.*a1ob1) / (1. - 3.*a2*gh*(6.*a1+b2))
! See the previous comments regarding the convention of "e", "q^2/2" and "tke".!NP
! In order to use "q" in the calculations of kzm and kzh, apply "sqrt(2*tke)"  !NP
       kzm(k) = min(bbls(k)*sqrt(2*tke(k))*sm(k),kmax) 
       kth(k) = min(bbls(k)*sqrt(2*tke(k))*sh(k),kmax)
! 
       kethl(k)=nuk*sqrt(kzm(k)*kzm(k+1)) ! GB01:A7
       kethl(k)=min(kethl(k),kmax) ! keep in bounds
    ENDDO
    !     Special case for tops of convective layers
    ! NT GB01 calls this local entrainment closure : 28
     do ilay = 1,iconv
      k = ktop(ilay)
      IF(nsquar(k).ge.n2min)THEN !NTnew
      kethl(k  )=nuk*kzm(k+1)
      if(k.ge.3)then !NT only in combination with previous n2 test.
         kethl(k-1)=0.0   !NT no transport through inversion
         delthvl = (thlx(k-2)+thx(k-2)*ep_1*qwx(k-2)) - &   !NT GB01:18,B12
              (thlx(k) + thx(k) * ep_1 * qwx(k))
         elambda = xlvocp*rcldb(k)*rexnerhl(k)/max(delthvl,0.1)
         bige = 0.8 * elambda
         biga = aone * (1 + atwo * bige)
         kth(k) = min(kth(k), biga*sqrt(TKE(k)**3)/NSQUAR(k)/ &
              max(bbls(k),bbls(k+1)))
         kzm(k) = min(kzm(k), kth(k) / sh(k+1) * sm(k+1)) ! prandtl number from layer below
        endif
    ENDIF !NTnew end
   enddo
     !     Need KETHL at the top (top-down indexing)
     KETHL(1) = kethl(2)
     !     Replace KETHL at the surface with something non-zero (top-down indexing) 
     kethl(kte) = nuk*0.5*kzm(kte)
    return
  end subroutine my

  SUBROUTINE gbmpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,        &
       RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
       TKE_PBL,KTH_GBM,                             &
       restart,allowed_to_read,                     &
       ids, ide, jds, jde, kds, kde,                &
       ims, ime, jms, jme, kms, kme,                &
       its, ite, jts, jte, kts, kte                 )
    !-------------------------------------------------------------------          
    IMPLICIT NONE
    !-------------------------------------------------------------------          
    LOGICAL , INTENT(IN)          :: restart,allowed_to_read
    INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
         ims, ime, jms, jme, kms, kme, &
         its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

    REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
         RUBLTEN, &
         RVBLTEN, &
         RTHBLTEN, &
         RQVBLTEN, &
         RQCBLTEN, & 
         RQIBLTEN, &
         TKE_PBL,  &
         KTH_GBM
    INTEGER :: i, j, k, itf, jtf, ktf

    jtf=min0(jte,jde-1)
    ktf=min0(kte,kde-1)
    itf=min0(ite,ide-1)

    IF(.not.restart)THEN
       DO j=jts,jtf
          DO k=kts,ktf
             DO i=its,itf
                RUBLTEN(i,k,j)=0.
                RVBLTEN(i,k,j)=0.
                RTHBLTEN(i,k,j)=0.
                RQVBLTEN(i,k,j)=0.
                RQCBLTEN(i,k,j)=0.
                TKE_PBL(i,k,j)=0.001 ! use array for TKE
                KTH_GBM(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
       DO j=jts,jtf
          DO k=kts,ktf
             DO i=its,itf
                RQIBLTEN(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

  END SUBROUTINE gbmpblinit
!-------------------------------------------------------------------          

END MODULE module_bl_gbmpbl


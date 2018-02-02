!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_acm

!  USE module_model_constants

  REAL, PARAMETER      :: RIC    = 0.25                ! critical Richardson number
  REAL, PARAMETER      :: CRANKP = 0.5                 ! CRANK-NIC PARAMETER

CONTAINS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   SUBROUTINE ACMPBL(XTIME,    DTPBL,    U3D,   V3D,                  &
                     PP3D,     DZ8W,     TH3D,  T3D,                  &
                     QV3D,     QC3D,     QI3D,  RR3D,                 &
#if (WRF_CHEM == 1)
                     CHEM3D,   VD3D,     NCHEM,                       &  ! For WRF-Chem
                     KDVEL, NDVEL, NUM_VERT_MIX,                      &  ! For WRF-Chem
#endif
                     UST,      HFX,      QFX,   TSK,                  &
                     PSFC,     EP1,      G,                           &
                     ROVCP,    RD,       CPD,                         &
                     PBLH,     KPBL2D,   EXCH_H, REGIME,              &
                     GZ1OZ0,   WSPD,     PSIM, MUT, RMOL,             &
                     RUBLTEN,  RVBLTEN,  RTHBLTEN,                    &
                     RQVBLTEN, RQCBLTEN, RQIBLTEN,                    &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     its,ite, jts,jte, kts,kte)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!   THIS MODULE COMPUTES VERTICAL MIXING IN AND ABOVE THE PBL ACCORDING TO 
!   THE ASYMMETRICAL CONVECTIVE MODEL, VERSION 2  (ACM2), WHICH IS A COMBINED 
!   LOCAL NON-LOCAL CLOSURE SCHEME BASED ON THE ORIGINAL ACM (PLEIM AND CHANG 1992)
!
!   REFERENCES: 
!   Pleim (2007) A combined local and non-local closure model for the atmospheric
!                boundary layer.  Part1: Model description and testing.  
!                JAMC, 46, 1383-1395
!   Pleim (2007) A combined local and non-local closure model for the atmospheric
!                boundary layer.  Part2: Application and evaluation in a mesoscale
!                meteorology model.  JAMC, 46, 1396-1409
!
!  REVISION HISTORY:
!     AX        3/2005   - developed WRF version based on the MM5 PX LSM
!     RG and JP 7/2006   - Finished WRF adaptation
!     JP 12/2011 12/2011 - ACM2 modified so it's not dependent on first layer thickness. 
!     JP        3/2013   - WRFChem version. Mixing of chemical species are added
!     HF        5/2016   - MPAS version  
!     JP        8/2017   - Z-coord version from MPAS version for hybrid coords  
!
!**********************************************************************
!   ARGUMENT LIST:
!
!... Inputs:
!-- XTIME           Time since simulation start (min)
!-- DTPBL           PBL time step
!-- U3D             3D u-velocity interpolated to theta points (m/s)
!-- V3D             3D v-velocity interpolated to theta points (m/s)
!-- PP3D            Pressure at half levels (Pa)
!-- DZ8W            dz between full levels (m)
!-- TH3D            Potential Temperature (K)
!-- T3D             Temperature (K)
!-- QV3D            3D water vapor mixing ratio (Kg/Kg)
!-- QC3D            3D cloud mixing ratio (Kg/Kg)
!-- QI3D            3D ice mixing ratio (Kg/Kg)
!-- RR3D            3D dry air density (kg/m^3)
!-- CHEM3D          Chemical species mixing ratios (ppm)  Optional for WRFChem   
!-- VD3D            Dry deposition velocity (m/s)         Optional for WRFChem
!-- NCHEM           Number of chemical species            Optional for WRFChem
!-- UST             Friction Velocity (m/s)
!-- HFX		    Upward heat flux at the surface (w/m^2)
!-- QFX		    Upward moisture flux at the surface (Kg/m^2/s)
!-- TSK             Surface temperature (K)
!-- PSFC            Pressure at the surface (Pa)
!-- EP1             Constant for virtual temperature (r_v/r_d-1) (dimensionless)
!-- G               Gravity (m/s^2)
!-- ROVCP           r/cp
!-- RD              gas constant for dry air (j/kg/k)
!-- CPD             heat capacity at constant pressure for dry air (j/kg/k)
!-- GZ1OZ0          log(z/z0) where z0 is roughness length
!-- WSPD            wind speed at lowest model level (m/s)
!-- PSIM            similarity stability function for momentum
!-- MUT             Total Mu : Psfc - Ptop
 
!-- ids             start index for i in domain
!-- ide             end index for i in domain
!-- jds             start index for j in domain
!-- jde             end index for j in domain
!-- kds             start index for k in domain
!-- kde             end index for k in domain
!-- ims             start index for i in memory
!-- ime             end index for i in memory
!-- jms             start index for j in memory
!-- jme             end index for j in memory
!-- kms             start index for k in memory
!-- kme             end index for k in memory
!-- jts             start index for j in tile
!-- jte             end index for j in tile
!-- kts             start index for k in tile
!-- kte             end index for k in tile
!
!... Outputs: 
!-- PBLH            PBL height (m)
!-- KPBL2D          K index for PBL layer
!-- REGIME          Flag indicating PBL regime (stable, unstable, etc.)
!-- RUBLTEN         U tendency due to PBL parameterization (m/s^2)
!-- RVBLTEN         V tendency due to PBL parameterization (m/s^2)
!-- RTHBLTEN        Theta tendency due to PBL parameterization (K/s)
!-- RQVBLTEN        Qv tendency due to PBL parameterization (kg/kg/s)
!-- RQCBLTEN        Qc tendency due to PBL parameterization (kg/kg/s)
!-- RQIBLTEN        Qi tendency due to PBL parameterization (kg/kg/s)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
     IMPLICIT NONE

!.......Arguments
! DECLARATIONS - INTEGER
    INTEGER,  INTENT(IN   )   ::      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte, XTIME

! DECLARATIONS - REAL
    REAL,                                INTENT(IN)  ::  DTPBL, EP1,   &
                                                        G, ROVCP, RD, CPD

    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),                         &
             INTENT(IN) ::                              U3D, V3D,            &
                                                        PP3D, DZ8W, T3D,     &
                                                        QV3D, QC3D, QI3D,    &
                                                        RR3D, TH3D

    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: PSIM, GZ1OZ0,     &
                                                          HFX, QFX, TSK,    &
                                                          PSFC, WSPD, MUT

    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::  PBLH, REGIME, & 
                                                              UST, RMOL

    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),                         &
             INTENT(INOUT)   ::                         RUBLTEN, RVBLTEN,    &
                                                        RTHBLTEN, RQVBLTEN,  &
                                                        RQCBLTEN, RQIBLTEN

   real,     dimension( ims:ime, kms:kme, jms:jme ),                         &
             intent(inout)   ::                         exch_h

    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(OUT  ) ::  KPBL2D
 
#if (WRF_CHEM == 1)
!... Chem
    INTEGER, INTENT(IN   )   ::   nchem, kdvel, ndvel, num_vert_mix
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme, nchem ), INTENT(INOUT) :: CHEM3D
    REAL,    DIMENSION( ims:ime, kdvel, jms:jme, ndvel ), INTENT(IN) :: VD3D
#endif
!... Local Variables

!... Integer
      INTEGER :: I, J, K, L
!... Real
      REAL  RDT
      REAL, PARAMETER :: KARMAN = 0.4

#if (WRF_CHEM == 1)
!... Chem
    REAL,    DIMENSION( ims:ime, kms:kme, nchem ) :: CHEM2D
    REAL,    DIMENSION( ims:ime, kdvel, ndvel ) :: VD2D
#endif
   


!**************************************
! dry air density - Hosein, June 2015
!     do j = jts,jte
!     do k = kts,kte
!     do i = its,ite
!       RR3DA(i,k,j) = RR3D(i,k,j) * (1.0 + 1.609*QV3D(i,k,j)) / (1.0 + QV3D(i,k,j))
!     enddo
!     enddo
!     enddo
!***************************************
   RDT = 1.0 / DTPBL

!==================================================

   DO j = jts,jte   
#if (WRF_CHEM == 1)
      DO L = 1, nchem
      DO K = kms,kme
      DO I = ims, ime
        CHEM2D(i,k,l) = chem3d(i,k,j,l)
      ENDDO
      ENDDO
      ENDDO

      DO L = 1, ndvel
      DO K = 1, kdvel
      DO I = ims, ime
        VD2D(i,k,l) = VD3D(i,k,j,l)
      ENDDO
      ENDDO
      ENDDO
#endif
      CALL ACM2D(j=j,xtime=xtime,dtpbl=dtpbl                       &
              ,us=u3d(ims,kms,j),vs=v3d(ims,kms,j)                 &
              ,theta=th3d(ims,kms,j),tt=t3d(ims,kms,j)             &
              ,qvs=qv3d(ims,kms,j),qcs=qc3d(ims,kms,j)             &
              ,qis=qi3d(ims,kms,j)                                 &
#if (WRF_CHEM == 1)
              ,chem=chem2d                                         &
              ,vd=vd2d                                             &
              ,nchem=nchem,kdvel=kdvel,ndvel=ndvel                 &
              ,num_vert_mix=num_vert_mix                           &
#endif
              ,dzf=DZ8W(ims,kms,j)                                 &
              ,densx=RR3D(ims,kms,j)                              &
              ,utnp=rublten(ims,kms,j),vtnp=rvblten(ims,kms,j)     &
              ,ttnp=rthblten(ims,kms,j),qvtnp=rqvblten(ims,kms,j)  &
              ,qctnp=rqcblten(ims,kms,j),qitnp=rqiblten(ims,kms,j) &
              ,cpd=cpd,g=g,rovcp=rovcp,rd=rd,rdt=rdt               &
              ,psfcpa=psfc(ims,j),ust=ust(ims,j)                   &
              ,pbl=pblh(ims,j)                                     &
              ,exch_hx=exch_h(ims,kms,j)                           &
              ,regime=regime(ims,j),psim=psim(ims,j)               &
              ,hfx=hfx(ims,j),qfx=qfx(ims,j)                       &
              ,tg=tsk(ims,j),gz1oz0=gz1oz0(ims,j)                  &
              ,wspd=wspd(ims,j) ,klpbl=kpbl2d(ims,j)               &
              ,mut=mut(ims,j), rmol=rmol(ims,j)                    &
              ,ep1=ep1,karman=karman                               &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde   &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme   &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
#if (WRF_CHEM == 1)
      DO L = 1, nchem
      DO I = ims, ime
        chem3d(i,kms:kme,j,l) = CHEM2D(i,kms:kme,l)
      ENDDO
      ENDDO
#endif
   ENDDO


!   write(46,*) pblh (7221,1)
!   write(56,*) 'rublten'  , rublten (1000,6,1)
!   write(56,*) 'rvblten'  , rvblten (1000,6,1)
!   write(56,*) 'rthblten' , rthblten(1000,6,1)
!   write(56,*) '==========='


   END SUBROUTINE ACMPBL
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   SUBROUTINE ACM2D(j,xtime,dtpbl                           &
              ,us,vs,theta,tt,qvs,qcs,qis                   &
#if (WRF_CHEM == 1)
              ,chem,  vd, nchem, kdvel, ndvel               &
              ,num_vert_mix                                 &
#endif
              ,dzf,densx,utnp,vtnp,ttnp,qvtnp,qctnp,qitnp   &
              ,cpd,g,rovcp,rd,rdt,psfcpa,ust                &
              ,pbl,exch_hx,regime,psim                      &
              ,hfx,qfx,tg,gz1oz0,wspd ,klpbl                &
              ,mut, rmol                                    &
              ,ep1,karman                                   &
              ,ids,ide, jds,jde, kds,kde   &
              ,ims,ime, jms,jme, kms,kme   &
              ,its,ite, jts,jte, kts,kte   )

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      IMPLICIT NONE

!.......Arguments

!... Real
      REAL ,                                INTENT(IN)  :: DTPBL, G, RD,ep1,karman,CPD,ROVCP,RDT
      REAL , DIMENSION( ims:ime ),          INTENT(INOUT)  :: PBL, UST
      
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS, THETA, TT,   &
                                                           QVS, QCS, QIS, DENSX
      REAL,  DIMENSION( ims:ime, kms:kme ), intent(in)  :: DZF
      REAL,  DIMENSION( ims:ime, kms:kme ), intent(inout)   ::  utnp,  &
                                                                vtnp,  &
                                                                ttnp,  &
                                                                qvtnp, &
                                                                qctnp, &
                                                                qitnp
      real,     dimension( ims:ime ), intent(in   )   ::   psfcpa
      real,     dimension( ims:ime ), intent(in   )   ::   tg
      real,     dimension( ims:ime ), intent(inout)   ::   regime, rmol
      real,     dimension( ims:ime ), intent(in)      ::   wspd, psim, gz1oz0
      real,     dimension( ims:ime ), intent(in)      ::   hfx, qfx
      real,     dimension( ims:ime ), intent(in)      ::   mut
      real,     dimension( ims:ime, kms:kme ),                    &
                intent(inout)                         ::   exch_hx
!... Integer
      INTEGER, DIMENSION( ims:ime ),       INTENT(OUT):: KLPBL
      INTEGER,  INTENT(IN)      ::      XTIME
      integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, j
#if (WRF_CHEM == 1)
!....Chem
      INTEGER,  INTENT(IN)      ::      NCHEM, KDVEL,NDVEL,NUM_VERT_MIX
      REAL , DIMENSION( ims:ime, kms:kme, NCHEM ), INTENT(INOUT)  :: CHEM
      REAL , DIMENSION( ims:ime, KDVEL, NDVEL ), INTENT(IN)  :: VD
#endif
!--------------------------------------------------------------------
!--Local 
      INTEGER I, K     
      INTEGER :: KPBLHT
      INTEGER, DIMENSION( its:ite ) :: KPBLH, NOCONV

!... Real
      REAL    ::  TVCON, WSS, TCONV, TH1, TOG, DTMP, WSSQ
      REAL    ::  ZH1,UH1,VH1                                   ! NEW FOR V3.7
      REAL    ::  psix, THV1
      REAL, DIMENSION( its:ite )          :: FINT, PSTAR, CPAIR
      REAL, DIMENSION( its:ite, kts:kte ) :: THETAV, RIB,                        &
                                             EDDYZ, EDDYZM, UX, VX, THETAX,      &
                                             QVX, QCX, QIX, ZA, DZHI, DZFI, DZH
      REAL, DIMENSION( its:ite, 0:kte )   :: ZF
      REAL,    DIMENSION( its:ite)        :: WST, TST, QST, USTM, TSTV
      REAL,    DIMENSION( its:ite )       :: MOL
      REAL    ::  FINTT, ZMIX, UMIX, VMIX
      REAL    ::  TMPFX, TMPVTCON, TMPP, TMPTHS, TMPTH1, TMPVCONV, WS1, DTH
      REAL    ::  A,TST12,RL,ZFUNC,DENSF

!... Integer
      INTEGER :: KL, jtf, ktf, itf, KMIX, KSRC
      character*512 :: message
!-----initialize vertical tendencies and

      DO i = its,ite
        DO k = kts,kte
          utnp(i,k) = 0.
          vtnp(i,k) = 0.
          ttnp(i,k) = 0.
        ENDDO
      ENDDO

      DO k = kts,kte
        DO i = its,ite
          qvtnp(i,k) = 0.
        ENDDO
      ENDDO

      DO k = kts,kte
        DO i = its,ite
          qctnp(i,k) = 0.
          qitnp(i,k) = 0.
        ENDDO
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Compute Micromet Scaling variables, not availiable in WRF for ACM
     DO I = its,ite
           CPAIR(I)  = CPD * (1.0 + 0.84 * QVS(I,1))                    ! J/(K KG)
           TMPFX     = HFX(I)  / (cpair(i) * DENSX(I,1))
           TMPVTCON  = 1.0 + EP1 * QVS(I,1)                             ! COnversion factor for virtual temperature
           WS1       = SQRT(US(I,1)**2 + VS(I,1)**2)                    ! Level 1 wind speed
           TST(I)    = -TMPFX / UST(I)
           QST(I)    = -QFX(I) / (UST(I)*DENSX(I,1))
           USTM(I)   = UST(I) * WS1 / wspd(i)
           THV1      = TMPVTCON * THETA(I,1) 
           TSTV(I)   = TST(I)*TMPVTCON + THV1*EP1*QST(I)
           IF(ABS(TSTV(I)).LT.1.0E-6) THEN
             TSTV(I) = SIGN(1.0E-6,TSTV(I))
           ENDIF
           MOL(I)    = THV1 * UST(i)**2/(KARMAN*G*TSTV(I))
           RMOL(I)   = 1.0/MOL(I)
           WST(I)    = UST(I) * (PBL(I)/(KARMAN*ABS(MOL(I)))) ** 0.333333       
           PSTAR(I)  =  MUT(I)/1000.                                     ! P* in cb 
     ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!... Compute PBL height

!... compute the height of full- and half-sigma level above ground level
     DO I = its,ite
       ZF(I,0)    = 0.0
       KLPBL(I)   = 1
     ENDDO

!     write(0,*) 'HHHHH2'
     DO K = kts, kte
       DO I = its,ite
         ZF(I,K) = DZF(I,K) + ZF(I,K-1)
         ZA(I,K) = 0.5 * (ZF(I,K) + ZF(I,K-1))
!         write(0,*) i , k , zf, za 
         DZH(I,K) = ZF(I,K) - ZF(I,K-1)
         DZHI(I,K)= 1./DZH(I,K)
       ENDDO
     ENDDO

     DO K = kts, kte-1
       DO I = its,ite
         DZFI(I,K) = 1./(ZA(I,K+1)-ZA(I,K))
       ENDDO
     ENDDO
     DO I = its,ite
       DZFI(I,KTE) = DZFI(I,KTE-1)
     ENDDO

     DO K = kts, kte
       DO I = its,ite
         TVCON       = 1.0 + EP1 * QVS(I,K)
         THETAV(I,K) = THETA(I,K) * TVCON
       ENDDO
     ENDDO


!...  COMPUTE PBL WHERE RICHARDSON NUMBER = RIC (0.25) HOLTSLAG ET AL 1990  
     DO 100 I = its,ite
       DO K = 1,kte
         KSRC = K
         IF (ZF(I,K).gt.30.0) GO TO 69
       ENDDO
69     CONTINUE
       TH1 = 0.0
       ZH1 = 0.0
       UH1 = 0.0
       VH1 = 0.0
       DO K = 1,KSRC
         TH1 = TH1 + THETAV(I,K)  
         ZH1 = ZH1 + ZA(I,K)
         UH1 = UH1 + US(I,K)
         VH1 = VH1 + VS(I,K)
       ENDDO  
       TH1 = TH1/KSRC
       ZH1 = ZH1/KSRC
       UH1 = UH1/KSRC
       VH1 = VH1/KSRC
       IF(MOL(I).LT.0.0 .AND. XTIME.GT.1) then
         WSS   = (UST(I) ** 3 + 0.6 * WST(I) ** 3) ** 0.33333
         TCONV = -8.5 * UST(I) * TSTV(I) / WSS
         TH1   = TH1 + TCONV
       ENDIF

99     KMIX = KSRC
       DO K = KSRC,kte
         DTMP   = THETAV(I,K) - TH1
         IF (DTMP.LT.0.0) KMIX = K
       ENDDO
       IF(KMIX.GT.KSRC) THEN
         FINTT = (TH1 - THETAV(I,KMIX)) / (THETAV(I,KMIX+1)               &
               - THETAV(I,KMIX))
         ZMIX = FINTT * (ZA(I,KMIX+1)-ZA(I,KMIX)) + ZA(I,KMIX)
         UMIX = FINTT * (US(I,KMIX+1)-US(I,KMIX)) + US(I,KMIX)
         VMIX = FINTT * (VS(I,KMIX+1)-VS(I,KMIX)) + VS(I,KMIX)
       ELSE
         ZMIX = ZH1
         UMIX = UH1
         VMIX = VH1
       ENDIF
       DO K = KMIX,kte
         DTMP   = THETAV(I,K) - TH1
         TOG = 0.5 * (THETAV(I,K) + TH1) / G
         WSSQ = (US(I,K)-UMIX)**2                                     &
              + (VS(I,K)-VMIX)**2
         IF (KMIX == KSRC) WSSQ = WSSQ + 100.*UST(I)*UST(I) 
         WSSQ = MAX( WSSQ, 0.1 )
         RIB(I,K) = ABS(ZA(I,K)-ZMIX) * DTMP / (TOG * WSSQ)
         IF (RIB(I,K) .GE. RIC) GO TO 201
       ENDDO


       write (message,*)' RIBX never exceeds RIC, RIB(i,kte) = ',rib(i,5),        &
               ' THETAV(i,1) = ',thetav(i,1),' MOL=',mol(i),            &
               ' TCONV = ',TCONV,' WST = ',WST(I),                      &
               ' KMIX = ',kmix,' UST = ',UST(I),                       &
               ' TST = ',TST(I),' U,V = ',US(I,1),VS(I,1),              &
               ' I,J=',I,J
       CALL wrf_error_fatal ( message )

201    CONTINUE

       KPBLH(I) = K

100  CONTINUE

     DO I = its,ite
       IF (KPBLH(I) .GT. KSRC) THEN
!---------INTERPOLATE BETWEEN LEVELS -- jp 7/93
         FINT(I) = (RIC - RIB(I,KPBLH(I)-1)) / (RIB(I,KPBLH(I)) -       &
                    RIB(I,KPBLH(I)-1))
         IF (FINT(I) .GT. 0.5) THEN
           KPBLHT  = KPBLH(I)
           FINT(I) = FINT(I) - 0.5
         ELSE
           KPBLHT  = KPBLH(I) - 1
           FINT(I) = FINT(I) + 0.5
         ENDIF
         PBL(I)  = FINT(I) * (ZF(I,KPBLHT) - ZF(I,KPBLHT-1)) +          &
                     ZF(I,KPBLHT-1)
         KLPBL(I) = KPBLHT
       ELSE
         KLPBL(I) = KSRC
         PBL(I)    = ZA(I,KSRC)                                                  
       ENDIF

     ENDDO

     DO I = its,ite       
       NOCONV(I) = 0
       
! Check for CBL and identify conv. vs. non-conv cells
       IF (PBL(I) / MOL(I) .LT. -0.02 .AND. KLPBL(I) .GT. 3        &
           .AND. THETAV(I,1) .GT. THETAV(I,2) .AND. XTIME .GT. 1) THEN
          NOCONV(I)   = 1
          REGIME(I) = 4.0                     ! FREE CONVECTIVE - ACM
       ENDIF
     ENDDO

!... Calculate Kz
     CALL EDDYX(DTPBL, ZF,  ZA,     MOL, PBL,  UST,                 &
                US,    VS,  TT,  THETAV, DENSX, PSTAR,              &
                QVS,   QCS, QIS, G, RD, CPAIR,                      &
                EDDYZ, EDDYZM, its,ite, kts,kte,ims,ime, kms,kme)

     CALL ACM(DTPBL, PSTAR,  NOCONV, ZF, DZH, DZHI, J,              &
                 KLPBL, PBL,   DZFI, MOL,  UST,                     &
                 TST, QST,  USTM,   EDDYZ,  DENSX,                  &
                 THETA,  QVS,    QCS,    QIS,                       &
                 THETAX, QVX,    QCX,    QIX,                       &
#if (WRF_CHEM == 1)
                 CHEM,  VD,  NCHEM, KDVEL, NDVEL,NUM_VERT_MIX,      &
#endif
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte)
     CALL ACMM(DTPBL, PSTAR,  NOCONV, ZF, DZH, DZHI, J,             &
                 KLPBL, PBL,   DZFI, MOL,  UST,                     &
                 TST, QST,  USTM,  EDDYZM, DENSX,                   &
                 US,    VS,         &
                 UX,    VX,         &
                 ids,ide, jds,jde, kds,kde,                         &
                 ims,ime, jms,jme, kms,kme,                         &
                 its,ite, jts,jte, kts,kte)

!.. Load exch_h for use in CCN activation

     DO K = kts, kte-1
       DO I = its, ite
         exch_hx(I,K) = EDDYZ(I,K)
       ENDDO
     ENDDO

!... Calculate tendency due to PBL parameterization

     DO K = kts, kte
       DO I = its, ite
         UTNP(I,K)  = UTNP(I,K) + (UX(I,K) - US(I,K)) * RDT
         VTNP(I,K)  = VTNP(I,K) + (VX(I,K) - VS(I,K)) * RDT
         TTNP(I,K)  = TTNP(I,K) + (THETAX(I,K) - THETA(I,K)) * RDT
         QVTNP(I,K) = QVTNP(I,K) + (QVX(I,K) - QVS(I,K)) * RDT
         QCTNP(I,K) = QCTNP(I,K) + (QCX(I,K) - QCS(I,K)) * RDT
         QITNP(I,K) = QITNP(I,K) + (QIX(I,K) - QIS(I,K)) * RDT
       ENDDO
     ENDDO

   END SUBROUTINE ACM2D
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   SUBROUTINE ACMINIT(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,           &
                      RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
                      restart, allowed_to_read ,                   &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-----------------------------------------------------------------------
!
!    This subroutine is for preparing ACM PBL variables. 
!    Called from module_physics_init.F
!
!  REVISION HISTORY:
!     AX     3/2005 - Originally developed
!-----------------------------------------------------------------------
!   ARGUMENT LIST:
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
     IMPLICIT NONE
!
   LOGICAL , INTENT(IN)          :: restart , allowed_to_read

   INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

!   REAL , DIMENSION( kms:kme ), INTENT(IN)  :: SHALF
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
                                                         RUBLTEN, &
                                                         RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN, &
                                                         RQCBLTEN, & 
                                                         RQIBLTEN

!... Local Variables
   INTEGER :: i, j, k, itf, jtf, ktf

!
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


   END SUBROUTINE acminit
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-------------------------------------------------------------------          
   SUBROUTINE EDDYX(DTPBL, ZF,  ZA,     MOL, PBL,  UST,               &
                    US,    VS,  TT,  THETAV, DENSX, PSTAR,            &
                    QVS,   QCS, QIS, G, RD, CPAIR,                    &
                    EDDYZ, EDDYZM, its,ite, kts,kte,ims,ime,kms,kme )


!**********************************************************************
!   Two methods for computing Kz:
!   1.  Boundary scaling similar to Holtslag and Boville (1993)
!   2.  Local Kz computed as function of local Richardson # and vertical 
!       wind shear, similar to LIU & CARROLL (1996)
!
!**********************************************************************
!
!-- DTPBL           time step of the minor loop for the land-surface/pbl model
!-- ZF              height of full level
!-- ZA              height of half level
!-- MOL             Monin-Obukhov length in 1D form
!-- PBL             PBL height in 1D form
!-- UST             friction velocity U* in 1D form (m/s)
!-- US              U wind 
!-- VS              V wind
!-- TT              temperature
!-- THETAV          potential virtual temperature
!-- DENSX           dry air density (kg/m^3)
!-- PSTAR           P*=Psfc-Ptop
!-- QVS             water vapor mixing ratio (Kg/Kg)
!-- QCS             cloud mixing ratio (Kg/Kg)
!-- QIS             ice mixing ratio (Kg/Kg)
!-- G               gravity
!-- RD              gas constant for dry air (j/kg/k)
!-- CPAIR           specific heat of moist air (M^2 S^-2 K^-1)
!-- EDDYZ           eddy diffusivity for heat KZ
!-- EDDYZM          eddy diffusivity for momentum KM
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IMPLICIT NONE

!.......Arguments
  
!... Integer
      INTEGER,  INTENT(IN)   ::    its,ite, kts,kte,ims,ime,kms,kme
!... Real
      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL, G, RD
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: MOL, PSTAR, CPAIR

      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS, TT,   &
                                                           QVS, QCS, QIS, DENSX
      REAL, DIMENSION( its:ite, kts:kte ), INTENT(IN) :: ZA, THETAV
      REAL, DIMENSION( its:ite, 0:kte )  , INTENT(IN) :: ZF
      
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: EDDYZ,EDDYZM

!.......Local variables

!... Integer
      INTEGER  :: ILX, KL, KLM, K, I

!... Real
      REAL     :: ZOVL, PHIH, WT, ZSOL, ZFUNC, DZF, SS, GOTH, EDYZ
      REAL     :: RI, QMEAN, TMEAN, XLV, ALPH, CHI, ZK, SQL, DENSF, KZO
      REAL     :: FH, FM
      REAL     :: WM, EDYZM, PHIM
!... Parameters
      REAL, PARAMETER :: RV     = 461.5
      REAL, PARAMETER :: RC     = 0.25
      REAL, PARAMETER :: RLAM   = 80.0
      REAL, PARAMETER :: GAMH   = 16.0 !Dyer74 !15.0  !  Holtslag and Boville (1993)
      REAL, PARAMETER :: GAMM   = 16.0 !Dyer74
      REAL, PARAMETER :: BETAH  = 5.0   !  Holtslag and Boville (1993)  BETAM = BETAH
      REAL, PARAMETER :: KARMAN = 0.4
      REAL, PARAMETER :: P      = 2.0   ! ZFUNC exponent
      REAL, PARAMETER :: EDYZ0  = 0.01  ! New Min Kz
      REAL, PARAMETER :: PR     = 0.8   ! Prandtl #
!      REAL, PARAMETER :: EDYZ0  = 0.1
!--   IMVDIF      imvdif=1 for moist adiabat vertical diffusion
      INTEGER, PARAMETER :: imvdif = 1
!
      ILX = ite 
      KL  = kte
      KLM = kte - 1
      
      DO K = kts,KLM
        DO I = its,ILX
          EDYZ = 0.0
          ZOVL = 0.0
          DZF  = ZA(I,K+1) - ZA(I,K)
          KZO = EDYZ0
!--------------------------------------------------------------------------
          IF (ZF(I,K) .LT. PBL(I)) THEN
            ZOVL = ZF(I,K) / MOL(I)
            IF (ZOVL .LT. 0.0) THEN
              IF (ZF(I,K) .LT. 0.1 * PBL(I)) THEN
                PHIH = 1.0 / SQRT(1.0 - GAMH * ZOVL)
                PHIM = (1.0 - GAMM * ZOVL)**(-0.25)
                WT   = UST(I) / PHIH
                WM   = UST(I) / PHIM
              ELSE
                ZSOL = 0.1 * PBL(I) / MOL(I)
                PHIH = 1.0 / SQRT(1.0 - GAMH * ZSOL)
                PHIM = (1.0 - GAMM * ZSOL)**(-0.25)
                WT   = UST(I) / PHIH
                WM   = UST(I) / PHIM
              ENDIF
            ELSE IF (ZOVL .LT. 1.0) THEN
              PHIH = 1.0 + BETAH * ZOVL
              WT   = UST(I) / PHIH
              WM   = WT
            ELSE
              PHIH = BETAH + ZOVL
              WT   = UST(I) / PHIH
              WM   = WT
            ENDIF
            ZFUNC      = ZF(I,K) * (1.0 - ZF(I,K) / PBL(I)) ** P
            EDYZ = KARMAN * WT * ZFUNC
            EDYZM = KARMAN * WM * ZFUNC
          ENDIF
!--------------------------------------------------------------------------!--------------------------------------------------------------------------
          SS   = ((US(I,K+1) - US(I,K)) ** 2 + (VS(I,K+1) - VS(I,K)) ** 2)   &
                  / (DZF * DZF) + 1.0E-9
          GOTH = 2.0 * G / (THETAV(I,K+1) + THETAV(I,K))
          RI   = GOTH * (THETAV(I,K+1) - THETAV(I,K)) / (DZF * SS)
!--------------------------------------------------------------------------
!         Adjustment to vert diff in Moist air
          IF(imvdif.eq.1)then
            IF ((QCS(I,K)+QIS(I,K)) .GT. 0.01E-3 .OR. (QCS(I,K+1)+             &
                 QIS(I,K+1)) .GT. 0.01E-3) THEN
              QMEAN = 0.5 * (QVS(I,K) + QVS(I,K+1))
              TMEAN = 0.5 * (TT(I,K) + TT(I,K+1))
              XLV   = (2.501 - 0.00237 * (TMEAN - 273.15)) * 1.E6
              ALPH  =  XLV * QMEAN / RD / TMEAN
              CHI   =  XLV * XLV * QMEAN / CPAIR(I) / RV / TMEAN / TMEAN
              RI    = (1.0 + ALPH) * (RI -G * G / SS / TMEAN / CPAIR(I) *       &
                      ((CHI - ALPH) / (1.0 + CHI)))
            ENDIF
          ENDIF
!--------------------------------------------------------------------------
            
          ZK  = 0.4 * ZF(I,K)
          SQL = (ZK * RLAM / (RLAM + ZK)) ** 2
            
          IF (RI .GE. 0.0) THEN
!	          IF (ZF(I,K).LT.PBL(I).AND.ZOVL.GT.0.0) THEN
!	            FH = MAX((1.-ZF(I,K)/PBL(I))**2,0.01) * PHIH **(-2)
!                  SQL = ZK ** 2
!	          ELSE
!	            FH = (MAX(1.-RI/RC,0.01))**2
!	          ENDIF
            FH=1./(1.+10.*RI+50.*RI**2+5000.*RI**4)+0.0012  !pleim5
            FM= PR*FH + 0.00104

            EDDYZ(I,K) = KZO + SQRT(SS) * FH * SQL
            EDDYZM(I,K) = KZO + SQRT(SS) * FM * SQL
          ELSE
            EDDYZ(I,K) = KZO + SQRT(SS * (1.0 - 25.0 * RI)) * SQL
            EDDYZM(I,K) = EDDYZ(I,K) * PR
          ENDIF
  
          IF(EDYZ.GT.EDDYZ(I,K)) THEN
            EDDYZ(I,K) = EDYZ
            EDDYZM(I,K) = MIN(EDYZM,EDYZ*0.8)  !PR
          ENDIF

          EDDYZ(I,K) = MIN(1000.0,EDDYZ(I,K))
          EDDYZ(I,K) = MAX(KZO,EDDYZ(I,K))
          EDDYZM(I,K) = MIN(1000.0,EDDYZM(I,K))
          EDDYZM(I,K) = MAX(KZO,EDDYZM(I,K))

        ENDDO             ! for I loop
      ENDDO               ! for k loop
!
      DO I = its,ILX
        EDDYZ(I,KL) = 0.0 ! EDDYZ(I,KLM) -- changed jp 3/08
        EDDYZM(I,KL) = 0.0
      ENDDO

   END SUBROUTINE EDDYX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-------------------------------------------------------------------          
   SUBROUTINE ACM (DTPBL, PSTAR,  NOCONV, ZF, DZH, DZHI, JX,       &
                   KLPBL, PBL,   DZFI, MOL,  UST,                  &
                   TST, QST,  USTM,   EDDYZ,  DENSX,               &
                   THETA,  QVS,    QCS,    QIS,                    &
                   THETAX, QVX,    QCX,    QIX,                    &
#if (WRF_CHEM == 1)
                   CHEM,  VD, NCHEM, KDVEL, NDVEL,                 &
                   NUM_VERT_MIX,                                   &
#endif
                   ids,ide, jds,jde, kds,kde,                      &
                   ims,ime, jms,jme, kms,kme,                      &
                   its,ite, jts,jte, kts,kte)
!**********************************************************************
!   PBL model called the Asymmetric Convective Model, Version 2 (ACM2) 
!   -- See top of module for summary and references
!
!---- REVISION HISTORY:
!   AX     3/2005 - developed WRF version based on ACM2 in the MM5 PX LSM
!   JP and RG 8/2006 - updates
!   JP     3/2013 - Chem additions
!
!**********************************************************************
!  ARGUMENTS:
!-- DTPBL           PBL time step
!-- PSTAR           Psurf - Ptop in cb
!-- NOCONV          If free convection =0, no; =1, yes
!-- ZF              Height for full layer
!-- DZH             Layer thickness --> ZF(K) - ZF(K-1)
!-- DZHI            Inverse of layer thickness
!-- JX              N-S index
!-- KLPBL           PBL level at K index
!-- PBL             PBL height in m
!-- DZFI            Inverse layer thickness --> 1/(Z(K+1)-Z(K))
!-- MOL             Monin-Obukhov length in 1D form
!-- UST             U* in 1D form
!-- TST             Theta* in 1D form
!-- QST             Q* in 1D form
!-- USTM            U* for computation of momemtum flux 
!-- EDDYZ           eddy diffusivity KZ
!-- DENSX           dry air density (kg/m^3)
!-- US              U wind 
!-- VS              V wind
!-- THETA           potential temperature
!-- QVS             water vapor mixing ratio (Kg/Kg)
!-- QCS             cloud mixing ratio (Kg/Kg)
!-- QIS             ice mixing ratio (Kg/Kg)
!-- UX              new U wind 
!-- VX              new V wind
!-- THETAX          new potential temperature
!-- QVX             new water vapor mixing ratio (Kg/Kg)
!-- QCX             new cloud mixing ratio (Kg/Kg)
!-- QIX             new ice mixing ratio (Kg/Kg)
!-- CHEM            Chemical species mixing ratios (ppm)  WRFChem   
!-- VD              Dry deposition velocity (m/s)         WRFChem
!-- NCHEM           Number of chemical species            WRFChem
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IMPLICIT NONE

!.......Arguments

!... Integer
      INTEGER,  INTENT(IN)      ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, JX
      INTEGER,  DIMENSION( its:ite ), INTENT(IN)  :: NOCONV
      INTEGER,  DIMENSION( ims:ime ), INTENT(IN)  :: KLPBL

!... Real
      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: PSTAR,    &
                                                           MOL, TST, &
                                                           QST, USTM
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(IN)  :: DZHI, DZH, DZFI
      REAL , DIMENSION( its:ite, 0:kte ),   INTENT(IN)  :: ZF
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(INOUT)  :: EDDYZ
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: THETA,   &
                                                           QVS, QCS, QIS, DENSX
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: THETAX,      &
                                                           QVX, QCX, QIX
#if (WRF_CHEM == 1)
!......Chem
      INTEGER,  INTENT(IN)      ::   NCHEM, KDVEL, NDVEL, NUM_VERT_MIX
      REAL , DIMENSION( ims:ime, kms:kme, nchem ), INTENT(INOUT)  :: CHEM   
      REAL , DIMENSION( ims:ime, KDVEL, NDVEL ), INTENT(IN)  :: VD
#endif
!.......Local variables

!... Parameters
      INTEGER, PARAMETER :: NSP   = 4 
!
      REAL,    PARAMETER :: XX    = 0.5          ! FACTOR APPLIED TO CONV MIXING TIME STEP
      REAL,    PARAMETER :: KARMAN = 0.4

!... Integer
      INTEGER :: ILX, KL, KLM, I, K, NSPX, NLP, NL, JJ, L,LL
      INTEGER :: KCBLMX
      INTEGER, DIMENSION( its:ite ) :: KCBL

!... Real
      REAL                               :: MBMAX, HOVL, MEDDY, MBAR
      REAL                               :: EKZ, RZ, FM, WSPD, DTS, DTRAT, F1
      REAL, DIMENSION( its:ite )         :: PSTARI, FSACM, DTLIM
      REAL, DIMENSION( kts:kte, its:ite) :: MBARKS, MDWN
      REAL, DIMENSION( kts:kte )         :: XPLUS, XMINUS
      REAL  DELC
      REAL, DIMENSION( kts:kte )                :: AI, BI, CI, EI !, Y
      REAL, ALLOCATABLE, DIMENSION( : , : )     :: DI, UI    
      REAL, ALLOCATABLE, DIMENSION( : , : )     :: FS
      REAL, ALLOCATABLE, DIMENSION( : , : , : ) :: VCI

      CHARACTER*80 :: message

!
!--Start Exicutable ----

      ILX = ite
      KL  = kte
      KLM = kte - 1
      NSPX = NSP
#if (WRF_CHEM == 1)
      NSPX = NSPX + NUM_VERT_MIX 
#endif

      KCBLMX = 0
      MBMAX  = 0.0
!...Allocate species variables
      ALLOCATE (DI( 1:NSPX,kts:kte ))       
      ALLOCATE (UI( 1:NSPX,kts:kte ))  
      ALLOCATE (FS( 1:NSPX, its:ite )) 
      ALLOCATE (VCI( 1:NSPX,its:ite,kts:kte  ))

!---COMPUTE ACM MIXING RATE
      DO I = its, ILX
        DTLIM(I)  = DTPBL
        PSTARI(I) = 1.0 / PSTAR(I)
        KCBL(I)   = 1
        FSACM(I)  = 0.0

        IF (NOCONV(I) .EQ. 1) THEN
          KCBL(I) = KLPBL(I)

!-------MBARKS IS UPWARD MIXING RATE; MDWN IS DOWNWARD MIXING RATE
!--New couple ACM & EDDY-------------------------------------------------------------
          HOVL     = -PBL(I) / MOL(I)
          FSACM(I) = 1./(1.+((KARMAN/(HOVL))**0.3333)/(0.72*KARMAN))
          MEDDY    = EDDYZ(I,1) * DZFI(i,1) / (PBL(I) - ZF(i,1))
          MBAR     = MEDDY * FSACM(I)
          DO K = kts,KCBL(I)-1
            EDDYZ(I,K) = EDDYZ(I,K) * (1.0 - FSACM(I))
          ENDDO

          MBMAX = AMAX1(MBMAX,MBAR)
          DO K = kts+1,KCBL(I)
            MBARKS(K,I) = MBAR
            MDWN(K,I)   = MBAR * (PBL(I) - ZF(i,K-1)) * DZHI(i,K)
          ENDDO
          MBARKS(1,I) = MBAR
          MBARKS(KCBL(I),I) = MDWN(KCBL(I),I)
          MDWN(KCBL(I)+1,I) = 0.0
        ENDIF
      ENDDO                              ! end of I loop

      DO K = kts,KLM
        DO I = its,ILX
          EKZ = EDDYZ(I,K) * DZFI(i,K) * DZHI(i,K)
          DTLIM(I) = AMIN1(0.75 / EKZ,DTLIM(I))
        ENDDO
      ENDDO
       
      DO I = its,ILX 
        IF (NOCONV(I) .EQ. 1) THEN
          KCBLMX = AMAX0(KLPBL(I),KCBLMX)
          RZ     = (ZF(i,KCBL(I)) - ZF(i,1)) * DZHI(i,1)
          DTLIM(I)  = AMIN1(XX / (MBARKS(1,I) * RZ),DTLIM(I))
        ENDIF
      ENDDO

      DO K = kts,KL
        DO I = its,ILX
          VCI(1,I,K) = THETA(I,K)
          VCI(2,I,K) = QVS(I,K)
          ! -- Also mix cloud water and ice IF necessary
          ! IF (IMOISTX.NE.1.AND.IMOISTX.NE.3) THEN  !!! Check other PBL models
          VCI(3,I,K) = QCS(I,K)
          VCI(4,I,K) = QIS(I,K)
#if (WRF_CHEM == 1)
          DO L= NSP+1, NSPX
             VCI(L,I,K) = CHEM(I,K,L-NSP)
          ENDDO
#endif
        ENDDO
      ENDDO

      DO I = its,ILX
        FS(1,I) = -UST(I) * TST(I)
        FS(2,I) = -UST(I) * QST(I)
        FS(3,I) = 0.0
        FS(4,I) = 0.0                      ! SURFACE FLUXES OF CLOUD WATER AND ICE = 0
#if (WRF_CHEM == 1)
        DO L= NSP+1, NSPX
          FS(L,I) = -VD(I,1,L-NSP) * CHEM(I,1,L-NSP)
        ENDDO
#endif
      ENDDO
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DO I = its,ILX      

        NLP   = INT(DTPBL / DTLIM(I) + 1.0)
        DTS   = (DTPBL / NLP)
        DTRAT = DTS / DTPBL
        DO NL = 1,NLP           ! LOOP OVER SUB TIME LOOP              

!-- COMPUTE ARRAY ELEMENTS THAT ARE INDEPENDANT OF SPECIES

          DO K = kts,kte
            AI(K) = 0.0
            BI(K) = 0.0
            CI(K) = 0.0
            EI(K) = 0.0
          ENDDO

          DO K = 2, KCBL(I)
            EI(K-1) = -CRANKP * MDWN(K,I) * DTS * DZH(i,K) * DZHI(i,K-1)
            BI(K)   = 1.0 + CRANKP * MDWN(K,I) * DTS
            AI(K)   = -CRANKP * MBARKS(K,I) * DTS
          ENDDO

          EI(1) = EI(1) -EDDYZ(I,1) * CRANKP * DZHI(i,1) * DZFI(i,1) * DTS
          AI(2) = AI(2) -EDDYZ(I,1) * CRANKP * DZHI(i,2) * DZFI(i,1) * DTS

          DO K =  KCBL(I)+1, KL
            BI(K) = 1.0
          ENDDO

          DO K = 2,KL
            XPLUS(K)  = EDDYZ(I,K) * DZHI(i,K) * DZFI(i,K) * DTS
            XMINUS(K) = EDDYZ(I,K-1) * DZHI(i,K) * DZFI(i,K-1) * DTS
            CI(K)     = - XMINUS(K) * CRANKP
            EI(K)     = EI(K) - XPLUS(K) * CRANKP
            BI(K)     = BI(K) + XPLUS(K) * CRANKP + XMINUS(K) * CRANKP
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            BI(1) = 1.0 + CRANKP * MBARKS(1,I) * (PBL(I) - ZF(i,1)) * DTS   &
                  * DZHI(i,1) + EDDYZ(I,1) * CRANKP * DZHI(i,1) * DZFI(i,1) * DTS
          ELSE
            BI(1) = 1.0  + EDDYZ(I,1) * CRANKP * DZHI(i,1) * DZFI(i,1) * DTS
          ENDIF


          DO K = 1,KL
            DO L = 1,NSPX                    
              DI(L,K) = 0.0
            ENDDO
          ENDDO
!
!**   COMPUTE TENDENCY OF CBL CONCENTRATIONS - SEMI-IMPLICIT SOLUTION
          DO K = 2,KCBL(I)
            DO L = 1,NSPX                    
              DELC = DTS * (MBARKS(K,I) * VCI(L,I,1) - MDWN(K,I) *          &
                 VCI(L,I,K) + DZH(i,K+1) * DZHI(i,K) *                      &
                        MDWN(K+1,I) * VCI(L,I,K+1))
              DI(L,K)   = VCI(L,I,K) + (1.0 - CRANKP) * DELC
            ENDDO
          ENDDO

          DO K = KCBL(I)+1, KL
            DO L = 1,NSPX                    
              DI(L,K) = VCI(L,I,K)
            ENDDO
          ENDDO

          DO K = 2,KL
            IF (K .EQ. KL) THEN
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K)  - (1.0 - CRANKP) * XMINUS(K) *           &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ELSE
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K) + (1.0 - CRANKP) * XPLUS(K) *             &
                          (VCI(L,I,K+1) - VCI(L,I,K))  -                    &
                          (1.0 - CRANKP) * XMINUS(K) *                      &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ENDIF
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) + (FS(L,I) - (1.0 - CRANKP)              &
                        * (MBARKS(1,I) *                                    &
                        (PBL(I) - ZF(i,1)) * VCI(L,I,1) -                   &
                        MDWN(2,I) * VCI(L,I,2) * DZH(i,2))) * DZHI(i,1) * DTS
            ENDDO
          ELSE
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) + FS(L,I) * DZHI(i,1) * DTS
            ENDDO
          ENDIF
          DO L = 1,NSPX                    
            DI(L,1) = DI(L,1) + (1.0 - CRANKP) * EDDYZ(I,1) * DZHI(i,1)     &
                    * DZFI(i,1) * DTS * (VCI(L,I,2) - VCI(L,I,1))
          ENDDO
          IF ( NOCONV(I) .EQ. 1 ) THEN
            CALL MATRIX (AI, BI, CI, DI, EI, UI, KL, NSPX)
          ELSE
            CALL TRI (CI, BI, EI, DI, UI, KL, NSPX)
          END IF
!
!-- COMPUTE NEW THETAV AND Q
          DO K = 1,KL
            DO L = 1,NSPX                    
              VCI(L,I,K) = UI(L,K)
            ENDDO
          ENDDO

        ENDDO                   ! END I LOOP
      ENDDO                     ! END SUB TIME LOOP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
      DO K = kts,KL
        DO I = its,ILX
          THETAX(I,K) = VCI(1,I,K)
          QVX(I,K)    = VCI(2,I,K)
          QCX(I,K)    = VCI(3,I,K)
          QIX(I,K)    = VCI(4,I,K)
#if (WRF_CHEM == 1)
          DO LL= 7, NSPX
             CHEM(I,K,LL-NSP) = VCI(LL,I,K) 
          ENDDO
#endif
      ENDDO
      ENDDO

      DEALLOCATE (DI)       
      DEALLOCATE (UI)  
      DEALLOCATE (FS)
      DEALLOCATE (VCI)

   END SUBROUTINE ACM
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-------------------------------------------------------------------          
   SUBROUTINE ACMM (DTPBL, PSTAR,  NOCONV, ZF, DZH, DZHI, JX,      &
                   KLPBL, PBL, DZFI, MOL, UST,                     &
                   TST, QST,  USTM, EDDYZM, DENSX,                 &
                   US,    VS,                                      &
                   UX,    VX,                                      &
                   ids,ide, jds,jde, kds,kde,                      &
                   ims,ime, jms,jme, kms,kme,                      &
                   its,ite, jts,jte, kts,kte)
!**********************************************************************
!   PBL model called the Asymmetric Convective Model, Version 2 (ACM2) 
!   -- See top of module for summary and references
!
!---- REVISION HISTORY:
!   AX     3/2005 - developed WRF version based on ACM2 in the MM5 PX LSM
!   JP and RG 8/2006 - updates
!
!**********************************************************************
!  ARGUMENTS:
!-- DTPBL           PBL time step
!-- PSTAR           Psurf - Ptop in cb
!-- NOCONV          If free convection =0, no; =1, yes
!-- ZF              Height for full layer
!-- DZH             Layer thickness --> ZF(K) - ZF(K-1)
!-- DZHI            Inverse of layer thickness
!-- JX              N-S index
!-- KLPBL           PBL level at K index
!-- PBL             PBL height in m
!-- DZFI            Inverse layer thickness --> 1/(Z(K+1)-Z(K))
!-- MOL             Monin-Obukhov length in 1D form
!-- UST             U* in 1D form
!-- TST             Theta* in 1D form
!-- QST             Q* in 1D form
!-- USTM            U* for computation of momemtum flux 
!-- EDDYZM          eddy diffusivity for momentum KM
!-- DENSX           dry air density (kg/m^3)
!-- US              U wind 
!-- VS              V wind
!-- THETA           potential temperature
!-- QVS             water vapor mixing ratio (Kg/Kg)
!-- QCS             cloud mixing ratio (Kg/Kg)
!-- QIS             ice mixing ratio (Kg/Kg)
!-- UX              new U wind 
!-- VX              new V wind
!-- THETAX          new potential temperature
!-- QVX             new water vapor mixing ratio (Kg/Kg)
!-- QCX             new cloud mixing ratio (Kg/Kg)
!-- QIX             new ice mixing ratio (Kg/Kg)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IMPLICIT NONE

!.......Arguments

!... Integer
      INTEGER,  INTENT(IN)      ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte, JX
      INTEGER,  DIMENSION( its:ite ), INTENT(IN)  :: NOCONV
      INTEGER,  DIMENSION( ims:ime ), INTENT(IN)  :: KLPBL

!... Real
      REAL , DIMENSION( ims:ime ),          INTENT(IN)  :: PBL, UST
      REAL ,                                INTENT(IN)  :: DTPBL
      REAL , DIMENSION( its:ite ),          INTENT(IN)  :: PSTAR,    &
                                                           MOL, TST, &
                                                           QST, USTM
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(IN)  :: DZHI, DZH, DZFI
      REAL , DIMENSION( its:ite, 0:kte ),   INTENT(IN)  :: ZF
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(INOUT)  :: EDDYZM
      REAL , DIMENSION( ims:ime, kms:kme ), INTENT(IN)  :: US,VS,    &
                                                           DENSX
      REAL , DIMENSION( its:ite, kts:kte ), INTENT(OUT) :: UX, VX
!.......Local variables

!... Parameters
      INTEGER, PARAMETER :: NSP   = 2
!
      REAL,    PARAMETER :: XX    = 0.5          ! FACTOR APPLIED TO CONV MIXING TIME STEP
      REAL,    PARAMETER :: KARMAN = 0.4

!... Integer
      INTEGER :: ILX, KL, KLM, I, K, NSPX, NLP, NL, JJ, L
      INTEGER :: KCBLMX
      INTEGER, DIMENSION( its:ite ) :: KCBL

!... Real
      REAL                               :: MBMAX, HOVL, MEDDY, MBAR
      REAL                               :: EKZ, RZ, FM, WSPD, DTS, DTRAT, F1
      REAL, DIMENSION( its:ite )         :: PSTARI, FSACM, DTLIM
      REAL, DIMENSION( kts:kte, its:ite) :: MBARKS, MDWN
      REAL, DIMENSION( 1:NSP, its:ite )  :: FS
      REAL, DIMENSION( kts:kte )         :: XPLUS, XMINUS
      REAL  DELC
      REAL, DIMENSION( 1:NSP,its:ite,kts:kte  ) :: VCI

      REAL, DIMENSION( kts:kte )               :: AI, BI, CI, EI !, Y
      REAL, DIMENSION( 1:NSP,kts:kte )         :: DI, UI    
!
!--Start Exicutable ----

      ILX = ite
      KL  = kte
      KLM = kte - 1

      KCBLMX = 0
      MBMAX  = 0.0

!---COMPUTE ACM MIXING RATE
      DO I = its, ILX
        DTLIM(I)  = DTPBL
        PSTARI(I) = 1.0 / PSTAR(I)
        KCBL(I)   = 1
        FSACM(I)  = 0.0

        IF (NOCONV(I) .EQ. 1) THEN
          KCBL(I) = KLPBL(I)

!-------MBARKS IS UPWARD MIXING RATE; MDWN IS DOWNWARD MIXING RATE
!--New couple ACM & EDDY-------------------------------------------------------------
          HOVL     = -PBL(I) / MOL(I)
          FSACM(I) = 1./(1.+((KARMAN/(HOVL))**0.3333)/(0.72*KARMAN))
          MEDDY    = EDDYZM(I,1) * DZFI(i,1) / (PBL(I) - ZF(i,1))
          MBAR     = MEDDY * FSACM(I)
          DO K = kts,KCBL(I)-1
            EDDYZM(I,K) = EDDYZM(I,K) * (1.0 - FSACM(I))
          ENDDO

          MBMAX = AMAX1(MBMAX,MBAR)
          DO K = kts+1,KCBL(I)
            MBARKS(K,I) = MBAR
            MDWN(K,I)   = MBAR * (PBL(I) - ZF(i,K-1)) * DZHI(i,K)
          ENDDO
          MBARKS(1,I) = MBAR
          MBARKS(KCBL(I),I) = MDWN(KCBL(I),I)
          MDWN(KCBL(I)+1,I) = 0.0
        ENDIF
      ENDDO                              ! end of I loop

      DO K = kts,KLM
        DO I = its,ILX
          EKZ   = EDDYZM(I,K) * DZFI(i,K) * DZHI(i,K)
          DTLIM(I) = AMIN1(0.75 / EKZ,DTLIM(I))
        ENDDO
      ENDDO
       
      DO I = its,ILX 
        IF (NOCONV(I) .EQ. 1) THEN
          KCBLMX = AMAX0(KLPBL(I),KCBLMX)
          RZ     = (ZF(i,KCBL(I)) - ZF(i,1)) * DZHI(i,1)
          DTLIM(I)  = AMIN1(XX / (MBARKS(1,I) * RZ),DTLIM(I))
        ENDIF
      ENDDO

      DO K = kts,KL
        DO I = its,ILX
          VCI(1,I,K) = US(I,K)
          VCI(2,I,K) = VS(I,K)
        ENDDO
      ENDDO

      NSPX=2

      DO I = its,ILX
        FM      = -USTM(I) * USTM(I)
        WSPD    = SQRT(US(I,1) * US(I,1) + VS(I,1) * VS(I,1)) + 1.E-9
        FS(1,I) = FM * US(I,1) / WSPD
        FS(2,I) = FM * VS(I,1) / WSPD
      ENDDO
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DO I = its,ILX      

        NLP   = INT(DTPBL / DTLIM(I) + 1.0)
        DTS   = (DTPBL / NLP)
        DTRAT = DTS / DTPBL
        DO NL = 1,NLP           ! LOOP OVER SUB TIME LOOP              

!-- COMPUTE ARRAY ELEMENTS THAT ARE INDEPENDANT OF SPECIES

          DO K = kts,KL
            AI(K) = 0.0
            BI(K) = 0.0
            CI(K) = 0.0
            EI(K) = 0.0
          ENDDO

          DO K = 2, KCBL(I)
            EI(K-1) = -CRANKP * MDWN(K,I) * DTS * DZH(i,K) * DZHI(i,K-1)
            BI(K)   = 1.0 + CRANKP * MDWN(K,I) * DTS
            AI(K)   = -CRANKP * MBARKS(K,I) * DTS
          ENDDO

          EI(1) = EI(1) -EDDYZM(I,1) * CRANKP * DZHI(i,1) * DZFI(i,1) * DTS
          AI(2) = AI(2) -EDDYZM(I,1) * CRANKP * DZHI(i,2) * DZFI(i,1) * DTS

          DO K =  KCBL(I)+1, KL
            BI(K) = 1.0
          ENDDO

          DO K = 2,KL
            XPLUS(K)  = EDDYZM(I,K) * DZHI(i,K) * DZFI(i,K) * DTS
            XMINUS(K) = EDDYZM(I,K-1) * DZHI(i,K) * DZFI(i,K-1) * DTS
            CI(K)     = - XMINUS(K) * CRANKP
            EI(K)     = EI(K) - XPLUS(K) * CRANKP
            BI(K)     = BI(K) + XPLUS(K) * CRANKP + XMINUS(K) * CRANKP
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            BI(1) = 1.0 + CRANKP * MBARKS(1,I) * (PBL(I) - ZF(i,1)) * DTS   &
                  * DZHI(i,1) + EDDYZM(I,1) * DZHI(i,1) * DZFI(i,1) * CRANKP * DTS
          ELSE
            BI(1) = 1.0  + EDDYZM(I,1) * DZHI(i,1) * DZFI(i,1) * CRANKP * DTS
          ENDIF


          DO K = 1,KL
            DO L = 1,NSPX                    
              DI(L,K) = 0.0
            ENDDO
          ENDDO
!
!**   COMPUTE TENDENCY OF CBL CONCENTRATIONS - SEMI-IMPLICIT SOLUTION
          DO K = 2,KCBL(I)
            DO L = 1,NSPX                    
              DELC = DTS * (MBARKS(K,I) * VCI(L,I,1) - MDWN(K,I) *          &
                 VCI(L,I,K) + DZH(i,K+1) * DZHI(i,K) *                      &
                        MDWN(K+1,I) * VCI(L,I,K+1))
              DI(L,K)   = VCI(L,I,K) + (1.0 - CRANKP) * DELC
            ENDDO
          ENDDO

          DO K = KCBL(I)+1, KL
            DO L = 1,NSPX                    
              DI(L,K) = VCI(L,I,K)
            ENDDO
          ENDDO

          DO K = 2,KL
            IF (K .EQ. KL) THEN
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K)  - (1.0 - CRANKP) * XMINUS(K) *           &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ELSE
              DO L = 1,NSPX                    
                DI(L,K) = DI(L,K) + (1.0 - CRANKP) * XPLUS(K) *             &
                          (VCI(L,I,K+1) - VCI(L,I,K))  -                    &
                          (1.0 - CRANKP) * XMINUS(K) *                      &
                          (VCI(L,I,K) - VCI(L,I,K-1))
              ENDDO
            ENDIF
          ENDDO

          IF (NOCONV(I) .EQ. 1) THEN
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) + (FS(L,I) - (1.0 - CRANKP)              &
                        * (MBARKS(1,I) *                                    &
                        (PBL(I) - ZF(i,1)) * VCI(L,I,1) -                   &
                        MDWN(2,I) * VCI(L,I,2) * DZH(i,2))) * DZHI(i,1) * DTS
            ENDDO
          ELSE
            DO L = 1,NSPX                    
              DI(L,1) = VCI(L,I,1) + FS(L,I) * DZHI(i,1) * DTS
            ENDDO
          ENDIF
          DO L = 1,NSPX                    
            DI(L,1) = DI(L,1) + (1.0 - CRANKP) * EDDYZM(I,1) * DZHI(i,1)    &
                    * DZFI(i,1) * DTS * (VCI(L,I,2) - VCI(L,I,1))
          ENDDO
          IF ( NOCONV(I) .EQ. 1 ) THEN
            CALL MATRIX (AI, BI, CI, DI, EI, UI, KL, NSPX)
          ELSE
            CALL TRI (CI, BI, EI, DI, UI, KL, NSPX)
          END IF
!
!-- COMPUTE NEW THETAV AND Q
          DO K = 1,KL
            DO L = 1,NSPX                    
              VCI(L,I,K) = UI(L,K)
            ENDDO
          ENDDO

        ENDDO                   ! END I LOOP
      ENDDO                     ! END SUB TIME LOOP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
      DO K = kts,KL
        DO I = its,ILX
          UX(I,K)     = VCI(1,I,K)
          VX(I,K)     = VCI(2,I,K)
        ENDDO
      ENDDO

   END SUBROUTINE ACMM
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   SUBROUTINE MATRIX(A,B,C,D,E,X,KL,NSP)
   
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   IMPLICIT NONE
!
!-- Bordered band diagonal matrix solver for ACM2

!-- ACM2 Matrix is in this form:
!   B1 E1
!   A2 B2 E2
!   A3 C3 B3 E3
!   A4    C4 B4 E4
!   A5       C5 B5 E5
!   A6          C6 B6

!--Upper Matrix is
!  U11 U12
!      U22 U23
!          U33 U34
!              U44 U45
!                  U55 U56
!                      U66

!--Lower Matrix is:
!  1
! L21  1
! L31 L32  1
! L41 L42 L43  1
! L51 L52 L53 L54  1
! L61 L62 L63 L64 L65 1
!---------------------------------------------------------
!...Arguments
      INTEGER, INTENT(IN)   :: KL
      INTEGER, INTENT(IN)   :: NSP
      REAL A(KL),B(KL),E(KL)
      REAL C(KL),D(NSP,KL),X(NSP,KL)

!...Locals
      REAL Y(NSP,KL),AIJ,SUM
      REAL L(KL,KL),UII(KL),UIIP1(KL),RUII(KL)
      INTEGER I,J,V

!-- Define Upper and Lower matrices
      L(1,1) = 1.
      UII(1) = B(1)
      RUII(1) = 1./UII(1)
      DO I = 2, KL
	      L(I,I) = 1.
	      L(I,1) = A(I)/B(1)
        UIIP1(I-1)=E(I-1)
	      IF(I.GE.3) THEN
	        DO J = 2,I-1
	          IF(I.EQ.J+1) THEN
	            AIJ = C(I)
	          ELSE
	            AIJ = 0.
	          ENDIF
	          L(I,J) = (AIJ-L(I,J-1)*E(J-1))/      &
                      (B(J)-L(J,J-1)*E(J-1))
	        ENDDO
	      ENDIF
      ENDDO
	  
      DO I = 2,KL
        UII(I) = B(I)-L(I,I-1)*E(I-1)
        RUII(I) = 1./UII(I)
      ENDDO
  
!-- Forward sub for Ly=d
      DO V= 1, NSP
        Y(V,1) = D(V,1)
        DO I=2,KL
	        SUM = D(V,I)
	        DO J=1,I-1
	          SUM = SUM - L(I,J)*Y(V,J)
	        ENDDO
	        Y(V,I) = SUM
        ENDDO
      ENDDO

!-- Back sub for Ux=y

      DO V= 1, NSP
        X(V,KL) = Y(V,KL)*RUII(KL)
      ENDDO
      DO I = KL-1,1,-1
        DO V= 1, NSP
         X(V,I) = (Y(V,I)-UIIP1(I)*X(V,I+1))*RUII(I)
        ENDDO
      ENDDO

   END SUBROUTINE MATRIX


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE TRI ( L, D, U, B, X,KL,NSP)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  FUNCTION:
!    Solves tridiagonal system by Thomas algorithm. 
!   The associated tri-diagonal system is stored in 3 arrays
!   D : diagonal
!   L : sub-diagonal
!   U : super-diagonal
!   B : right hand side function
!   X : return solution from tridiagonal solver

!     [ D(1) U(1) 0    0    0 ...       0     ]
!     [ L(2) D(2) U(2) 0    0 ...       .     ]
!     [ 0    L(3) D(3) U(3) 0 ...       .     ]
!     [ .       .     .     .           .     ] X(i) = B(i)
!     [ .             .     .     .     0     ]
!     [ .                   .     .     .     ]
!     [ 0                           L(n) D(n) ]

!-----------------------------------------------------------------------

      IMPLICIT NONE

! Arguments:

      INTEGER, INTENT(IN)   :: KL
      INTEGER, INTENT(IN)   :: NSP

      REAL        L( KL )               ! subdiagonal
      REAL        D(KL)   ! diagonal
      REAL        U( KL )               ! superdiagonal
      REAL        B(NSP,KL )   ! R.H. side
      REAL        X( NSP,KL )   ! solution

! Local Variables:

      REAL        GAM( KL )
      REAL        BET
      INTEGER     V, K

! Decomposition and forward substitution:
      BET = 1.0 / D( 1 )
      DO V = 1, NSP
         X( V,1 ) = BET * B(V,1 )
      ENDDO

      DO K = 2, KL
        GAM(K ) = BET * U( K-1 )
        BET = 1.0 / ( D( K ) - L( K ) * GAM( K ) )
	      DO V = 1, NSP
           X( V, K ) = BET * ( B( V,K ) - L( K ) * X( V,K-1 ) )
	      ENDDO
      ENDDO

! Back-substitution:

      DO K = KL - 1, 1, -1
        DO V = 1, NSP
          X( V,K ) = X( V,K ) - GAM( K+1 ) * X( V,K+1 )
        ENDDO
      ENDDO
     
  END SUBROUTINE TRI
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

END MODULE module_bl_acm
                        

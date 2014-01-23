!-----------------------------------------------------------------------
!
!WRF:MODEL_LAYER:PHYSICS
!
!####################TIEDTKE SCHEME#########################
!   Taken from the IPRC iRAM - Yuqing Wang, University of Hawaii
!   Added by Chunxi Zhang and Yuqing Wang to WRF3.2, May, 2010
!   refenrence: Tiedtke (1989, MWR, 117, 1779-1800)
!               Nordeng, T.E., (1995), CAPE closure and organized entrainment/detrainment
!               Yuqing Wang et al. (2003,J. Climate, 16, 1721-1738) for improvements 
!                                                  for cloud top detrainment 
!                       (2004, Mon. Wea. Rev., 132, 274-296), improvements for PBL clouds
!                        (2007,Mon. Wea. Rev., 135, 567-585), diurnal cycle of precipitation
!   This scheme is on testing
!###########################################################
MODULE module_cu_tiedtke
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! epsl--- allowed minimum value for floating calculation
!---------------------------------------------------------------
      real,parameter ::  epsl  = 1.0e-20
      real,parameter ::  t000  = 273.15
      real,parameter ::  hgfr  = 233.15   ! defined in param.f in explct
!-------------------------------------------------------------    
!  Ends the parameters set
!++++++++++++++++++++++++++++
     REAL,PRIVATE :: G,CPV
     REAL :: API,A,EOMEGA,RD,RV,CPD,RCPD,VTMPC1,VTMPC2,   &
             RHOH2O,ALV,ALS,ALF,CLW,TMELT,SOLC,STBO,DAYL,YEARL, &
             C1ES,C2ES,C3LES,C3IES,C4LES,C4IES,C5LES,C5IES,ZRG 
    
     REAL :: ENTRPEN,ENTRSCV,ENTRMID,ENTRDD,CMFCTOP,RHM,RHC,    &
             CMFCMAX,CMFCMIN,CMFDEPS,RHCDD,CPRCON,CRIRH,ZBUO0,  &
             fdbk,ZTAU
 
     INTEGER :: orgen,nturben,cutrigger

     REAL :: CVDIFTS, CEVAPCU1, CEVAPCU2,ZDNOPRC
    
  
     PARAMETER(A=6371.22E03,                                    &
      ALV=2.5008E6,                 &                  
      ALS=2.8345E6,                 &
      ALF=ALS-ALV,                  &
      CPD=1005.46,                  &
      CPV=1869.46,                  & ! CPV in module is 1846.4
      RCPD=1.0/CPD,                 &
      RHOH2O=1.0E03,                & 
      TMELT=273.16,                 &
      G=9.806,                      & ! G=9.806
      ZRG=1.0/G,                    &
      RD=287.05,                    &
      RV=461.51,                    &
      C1ES=610.78,                  &
      C2ES=C1ES*RD/RV,              &
      C3LES=17.269,                 &
      C4LES=35.86,                  &
      C5LES=C3LES*(TMELT-C4LES),    &
      C3IES=21.875,                 &
      C4IES=7.66,                   &
      C5IES=C3IES*(TMELT-C4IES),    &
      API=3.141593,                 & ! API=2.0*ASIN(1.)
      VTMPC1=RV/RD-1.0,             &
      VTMPC2=CPV/CPD-1.0,           &
      CVDIFTS=1.0,                  &
      CEVAPCU1=1.93E-6*261.0*0.5/G, & 
      CEVAPCU2=1.E3/(38.3*0.293) )

     
!                SPECIFY PARAMETERS FOR MASSFLUX-SCHEME
!                  --------------------------------------
!                   These are tunable parameters
!
!     ENTRPEN: AVERAGE ENTRAINMENT RATE FOR PENETRATIVE CONVECTION
!     -------
!
      PARAMETER(ENTRPEN=1.0E-4)
!
!     ENTRSCV: AVERAGE ENTRAINMENT RATE FOR SHALLOW CONVECTION
!     -------
!
      PARAMETER(ENTRSCV=1.2E-3)
!
!     ENTRMID: AVERAGE ENTRAINMENT RATE FOR MIDLEVEL CONVECTION
!     -------
!
      PARAMETER(ENTRMID=1.0E-4)
!
!     ENTRDD: AVERAGE ENTRAINMENT RATE FOR DOWNDRAFTS
!     ------
!
      PARAMETER(ENTRDD =2.0E-4)
!
!     CMFCTOP:   RELATIVE CLOUD MASSFLUX AT LEVEL ABOVE NONBUOYANCY LEVEL
!     -------
!
      PARAMETER(CMFCTOP=0.30)
!
!     CMFCMAX:   MAXIMUM MASSFLUX VALUE ALLOWED FOR UPDRAFTS ETC
!     -------
!
      PARAMETER(CMFCMAX=1.0)
!
!     CMFCMIN:   MINIMUM MASSFLUX VALUE (FOR SAFETY)
!     -------
!
      PARAMETER(CMFCMIN=1.E-10)
!
!     CMFDEPS:   FRACTIONAL MASSFLUX FOR DOWNDRAFTS AT LFS
!     -------
!
      PARAMETER(CMFDEPS=0.30)
!
!     CPRCON:  COEFFICIENTS FOR DETERMINING CONVERSION FROM CLOUD WATER
!
      PARAMETER(CPRCON = 1.1E-3/G)
!
!     ZDNOPRC: The pressure depth below which no precipitation
!
      PARAMETER(ZDNOPRC =1.5E4)
!--------------------
     PARAMETER(orgen=1)   ! Old organized entrainment rate
!      PARAMETER(orgen=2)   ! New organized entrainment rate

     PARAMETER(nturben=1) ! old deep turburent entrainment/detrainment rate
!      PARAMETER(nturben=2) ! New deep turburent entrainment/detrainment rate

     PARAMETER(cutrigger=1) ! Old trigger function
!      PARAMETER(cutrigger=2) ! New trigger function
!
!--------------------
      PARAMETER(RHC=0.80,RHM=1.0,ZBUO0=0.50)
!--------------------
      PARAMETER(CRIRH=0.70,fdbk = 1.0,ZTAU = 1800.0)
!--------------------
      LOGICAL :: LMFPEN,LMFMID,LMFSCV,LMFDD,LMFDUDV
      PARAMETER(LMFPEN=.TRUE.,LMFMID=.TRUE.,LMFSCV=.TRUE.,LMFDD=.TRUE.,LMFDUDV=.TRUE.)
!--------------------
!#################### END of Variables definition##########################
!-----------------------------------------------------------------------
!
CONTAINS
!-----------------------------------------------------------------------
     SUBROUTINE CU_TIEDTKE(                                    &
                 DT,ITIMESTEP,STEPCU                            &
                ,RAINCV,PRATEC,QFX,HFX,ZNU                      &
                ,U3D,V3D,W,T3D,QV3D,QC3D,QI3D,PI3D,RHO3D        &
                ,QVFTEN,QVPBLTEN                                &
                ,DZ8W,PCPS,P8W,XLAND,CU_ACT_FLAG                &
                ,ids,ide, jds,jde, kds,kde                      &
                ,ims,ime, jms,jme, kms,kme                      &
                ,its,ite, jts,jte, kts,kte                      &
                ,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN            &
                ,RUCUTEN, RVCUTEN                               &
                ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS       &
                                                                )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- TH3D        3D potential temperature (K)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- QC3D        3D cloud mixing ratio (Kg/Kg)
!-- QI3D        3D ice mixing ratio (Kg/Kg)
!-- RHO3D       3D air density (kg/m^3)
!-- P8w         3D hydrostatic pressure at full levels (Pa)
!-- Pcps        3D hydrostatic pressure at half levels (Pa)
!-- PI3D        3D exner function (dimensionless)
!-- QVFTEN      3D water vapor advection tendency 
!-- QVPBLTEN    3D water vapor tendency due to a PBL
!-- RTHCUTEN      Theta tendency due to 
!                 cumulus scheme precipitation (K/s)
!-- RUCUTEN       U wind tendency due to 
!                 cumulus scheme precipitation (K/s)
!-- RVCUTEN       V wind tendency due to 
!                 cumulus scheme precipitation (K/s)
!-- RQVCUTEN      Qv tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQRCUTEN      Qr tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQCCUTEN      Qc tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQSCUTEN      Qs tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQICUTEN      Qi tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RAINC         accumulated total cumulus scheme precipitation (mm)
!-- RAINCV        cumulus scheme precipitation (mm)
!-- PRATEC        precipitiation rate from cumulus scheme (mm/s)
!-- dz8w        dz between full levels (m)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- DT          time step (s)
!-- F_QV etc    flag values for tendencies, not used
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
      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ITIMESTEP,                      &
                                        STEPCU

      REAL,    INTENT(IN) ::                                            &
                                        DT


      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        RAINCV, PRATEC

      LOGICAL, DIMENSION(IMS:IME,JMS:JME), INTENT(INOUT) ::             &
                                        CU_ACT_FLAG


      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        DZ8W,                           &
                                        P8w,                            &
                                        Pcps,                           &
                                        PI3D,                           &
                                        QC3D,                           &
                                        QVFTEN,                         &
                                        QVPBLTEN,                       &
                                        QI3D,                           &
                                        QV3D,                           &
                                        RHO3D,                          &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D,                            &
                                        W                              

!--------------------------- OPTIONAL VARS ----------------------------
                                                                                                      
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),                       &
               OPTIONAL, INTENT(INOUT) ::                               &
                                        RQCCUTEN,                       &
                                        RQICUTEN,                       &
                                        RQVCUTEN,                       &
                                        RTHCUTEN,                       &
                                        RUCUTEN,                        &
                                        RVCUTEN
                                                                                                      
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
     LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS
 
!--------------------------- LOCAL VARS ------------------------------

      REAL,    DIMENSION(ims:ime, jms:jme) ::                           &
                                        QFX,                            &
                                        HFX        

      REAL      ::                                      &
                                        DELT,                           &
                                        RDELT                          

      REAL     , DIMENSION(its:ite) ::                  &
                                        RCS,                            &
                                        RN,                             &
                                        EVAP,                           &
                                        heatflux,                       &
                                        rho2d                            
      INTEGER  , DIMENSION(its:ite) ::  SLIMSK                         
      

      REAL     , DIMENSION(its:ite, kts:kte+1) ::       &
                                        PRSI                            

      REAL     , DIMENSION(its:ite, kts:kte) ::         &
                                        DEL,                            &
                                        DOT,                            &
                                        PHIL,                           &
                                        PRSL,                           &
                                        Q1,                             & 
                                        Q2,                             &
                                        Q3,                             &
                                        Q1B,                            &
                                        Q1BL,                           &
                                        Q11,                            &
                                        Q12,                            &  
                                        T1,                             & 
                                        U1,                             & 
                                        V1,                             & 
                                        ZI,                             & 
                                        ZL,                             &
                                        OMG,                            &
                                        GHT 

      INTEGER, DIMENSION(its:ite) ::                                    &
                                        KBOT,                           &
                                        KTOP                           

      INTEGER ::                                                        &
                                        I,                              &
                                        IM,                             &
                                        J,                              &
                                        K,                              &
                                        KM,                             &
                                        KP,                             &
                                        KX


!-------other local variables----
      INTEGER,DIMENSION( its:ite ) :: KTYPE
      REAL, DIMENSION( kts:kte )   :: sig1      ! half sigma levels
      REAL, DIMENSION( kms:kme )   :: ZNU
      INTEGER                      :: zz 
!-----------------------------------------------------------------------
!

      DO J=JTS,JTE
         DO I=ITS,ITE
            CU_ACT_FLAG(I,J)=.TRUE.
         ENDDO
      ENDDO
 
      IM=ITE-ITS+1
      KX=KTE-KTS+1
      DELT=DT*STEPCU
      RDELT=1./DELT

!-------------  J LOOP (OUTER) --------------------------------------------------

   DO J=jts,jte

! --------------- compute zi and zl -----------------------------------------
      DO i=its,ite
        ZI(I,KTS)=0.0
      ENDDO

      DO k=kts+1,kte
        KM=k-1
        DO i=its,ite
          ZI(I,K)=ZI(I,KM)+dz8w(i,km,j)
        ENDDO
      ENDDO

      DO k=kts+1,kte
        KM=k-1
        DO i=its,ite
          ZL(I,KM)=(ZI(I,K)+ZI(I,KM))*0.5
        ENDDO
      ENDDO

      DO i=its,ite
        ZL(I,KTE)=2.*ZI(I,KTE)-ZL(I,KTE-1)
      ENDDO

! --------------- end compute zi and zl -------------------------------------
      DO i=its,ite
        SLIMSK(i)=int(ABS(XLAND(i,j)-2.))
      ENDDO

      DO k=kts,kte
        kp=k+1
        DO i=its,ite
          DOT(i,k)=-0.5*g*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j))
        ENDDO
      ENDDO

      DO k=kts,kte
        zz = kte+1-k        
        DO i=its,ite
          U1(i,zz)=U3D(i,k,j)
          V1(i,zz)=V3D(i,k,j)
          T1(i,zz)=T3D(i,k,j)
          Q1(i,zz)= QV3D(i,k,j)
          if(itimestep == 1) then
             Q1B(i,zz)=0.
             Q1BL(i,zz)=0.
          else
             Q1B(i,zz)=QVFTEN(i,k,j)
             Q1BL(i,zz)=QVPBLTEN(i,k,j)
          endif
          Q2(i,zz)=QC3D(i,k,j)
          Q3(i,zz)=QI3D(i,k,j)
          OMG(i,zz)=DOT(i,k)
          GHT(i,zz)=ZL(i,k)
          PRSL(i,zz) = Pcps(i,k,j)
        ENDDO
      ENDDO

      DO k=kts,kte+1
        zz = kte+2-k
        DO i=its,ite
          PRSI(i,zz) = P8w(i,k,j)
        ENDDO
      ENDDO 

      DO k=kts,kte
         zz = kte+1-k
         sig1(zz) = ZNU(k)
      ENDDO

!###############before call TIECNV, we need EVAP########################
!       EVAP is the vapor flux at the surface
!########################################################################
!
      DO i=its,ite
        EVAP(i) = QFX(i,j)
        heatflux(i)=HFX(i,j)
        rho2d(i) = rho3d(i,1,j)
      ENDDO
!########################################################################
      CALL TIECNV(U1,V1,T1,Q1,Q2,Q3,Q1B,Q1BL,GHT,OMG,PRSL,PRSI,EVAP,heatflux,rho2d,             &
                  RN,SLIMSK,KTYPE,IM,KX,KX+1,sig1,DELT)      

      DO I=ITS,ITE
         RAINCV(I,J)=RN(I)/STEPCU
         PRATEC(I,J)=RN(I)/(STEPCU * DT)
      ENDDO

      DO K=KTS,KTE
        zz = kte+1-k
        DO I=ITS,ITE
          RTHCUTEN(I,K,J)=(T1(I,zz)-T3D(I,K,J))/PI3D(I,K,J)*RDELT
          RQVCUTEN(I,K,J)=(Q1(I,zz)-QV3D(I,K,J))*RDELT
          RUCUTEN(I,K,J) =(U1(I,zz)-U3D(I,K,J))*RDELT
          RVCUTEN(I,K,J) =(V1(I,zz)-V3D(I,K,J))*RDELT 
        ENDDO
      ENDDO

      IF(PRESENT(RQCCUTEN))THEN
        IF ( F_QC ) THEN
          DO K=KTS,KTE
            zz = kte+1-k
            DO I=ITS,ITE
              RQCCUTEN(I,K,J)=(Q2(I,zz)-QC3D(I,K,J))*RDELT
            ENDDO
          ENDDO
        ENDIF
      ENDIF

      IF(PRESENT(RQICUTEN))THEN
        IF ( F_QI ) THEN
          DO K=KTS,KTE
            zz = kte+1-k
            DO I=ITS,ITE
              RQICUTEN(I,K,J)=(Q3(I,zz)-QI3D(I,K,J))*RDELT
            ENDDO
          ENDDO
        ENDIF
      ENDIF


   ENDDO

   END SUBROUTINE CU_TIEDTKE

!====================================================================
   SUBROUTINE tiedtkeinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,          &
                     RUCUTEN,RVCUTEN,                                   &
                     RESTART,P_QC,P_QI,P_FIRST_SCALAR,                  &
                     allowed_to_read,                                   &
                     ids, ide, jds, jde, kds, kde,                      &
                     ims, ime, jms, jme, kms, kme,                      &
                     its, ite, jts, jte, kts, kte)
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  allowed_to_read,restart
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::  &
                                                              RTHCUTEN, &
                                                              RQVCUTEN, &
                                                              RQCCUTEN, &
                                                              RQICUTEN, &
                                                              RUCUTEN,RVCUTEN 

   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
       RTHCUTEN(i,k,j)=0.
       RQVCUTEN(i,k,j)=0.
       RUCUTEN(i,k,j)=0.
       RVCUTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQCCUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF
   ENDIF

      END SUBROUTINE tiedtkeinit

! ------------------------------------------------------------------------

!------------This is the combined version for tiedtke---------------
!----------------------------------------------------------------
!  In this module only the mass flux convection scheme of the ECMWF is included
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!#############################################################
!
!             LEVEL 1 SUBROUTINEs
!
!#############################################################
!********************************************************
!        subroutine TIECNV
!********************************************************
      SUBROUTINE TIECNV(pu,pv,pt,pqv,pqc,pqi,pqvf,pqvbl,poz,pomg,  &
           pap,paph,evap,hfx,rho,zprecc,lndj,KTYPE,lq,km,km1,sig1,dt)
!-----------------------------------------------------------------
!  This is the interface between the meso-scale model and the mass 
!  flux convection module
!-----------------------------------------------------------------
      implicit none

      real pu(lq,km),pv(lq,km),pt(lq,km),pqv(lq,km),pqvf(lq,km)
      real poz(lq,km),pomg(lq,km),evap(lq),zprecc(lq),pqvbl(lq,km)
      real PHHFL(lq),RHO(lq),hfx(lq)
      REAL PUM1(lq,km),    PVM1(lq,km),                             &
          PTTE(lq,km),    PQTE(lq,km),  PVOM(lq,km),  PVOL(lq,km),  &
          PVERV(lq,km),   PGEO(lq,km),  PAP(lq,km),   PAPH(lq,km1)
      REAL PQHFL(lq),      ZQQ(lq,km),   PAPRC(lq),    PAPRS(lq),   &
          PRSFC(lq),      PSSFC(lq),    PAPRSM(lq),   PCTE(lq,km)
      REAL ZTP1(lq,km),    ZQP1(lq,km),  ZTU(lq,km),   ZQU(lq,km),  &
          ZLU(lq,km),     ZLUDE(lq,km), ZMFU(lq,km),  ZMFD(lq,km),  &
          ZQSAT(lq,km),   pqc(lq,km),   pqi(lq,km),   ZRAIN(lq)

      REAL sig(km1),sig1(km)
      INTEGER ICBOT(lq),   ICTOP(lq),     KTYPE(lq),   lndj(lq)
      REAL  dt
      LOGICAL LOCUM(lq)

      real PSHEAT,PSRAIN,PSEVAP,PSMELT,PSDISS,TT
      real ZTMST,ZTPP1,fliq,fice,ZTC,ZALF
      integer i,j,k,lq,lp,km,km1
!      real TLUCUA
!      external TLUCUA

      ZTMST=dt
!  Masv flux diagnostics.

      PSHEAT=0.0
      PSRAIN=0.0
      PSEVAP=0.0
      PSMELT=0.0
      PSDISS=0.0
      DO 8 j=1,lq
        ZRAIN(j)=0.0
        LOCUM(j)=.FALSE.
        PRSFC(j)=0.0
        PSSFC(j)=0.0
        PAPRC(j)=0.0
        PAPRS(j)=0.0
        PAPRSM(j)=0.0
        PQHFL(j)=evap(j)
        PHHFL(j)=hfx(j)
    8 CONTINUE

!     CONVERT MODEL VARIABLES FOR MFLUX SCHEME

      DO 10 k=1,km
        DO 10 j=1,lq
          PTTE(j,k)=0.0
          PCTE(j,k)=0.0
          PVOM(j,k)=0.0
          PVOL(j,k)=0.0
          ZTP1(j,k)=pt(j,k)
          ZQP1(j,k)=pqv(j,k)/(1.0+pqv(j,k))
          PUM1(j,k)=pu(j,k)
          PVM1(j,k)=pv(j,k)
          PVERV(j,k)=pomg(j,k)
          PGEO(j,k)=G*poz(j,k)
          TT=ZTP1(j,k)
          ZQSAT(j,k)=TLUCUA(TT)/PAP(j,k)
          ZQSAT(j,k)=MIN(0.5,ZQSAT(j,k))
          ZQSAT(j,k)=ZQSAT(j,k)/(1.-VTMPC1*ZQSAT(j,k))
          PQTE(j,k)=pqvf(j,k)+pqvbl(j,k)
          ZQQ(j,k)=PQTE(j,k)
   10 CONTINUE
!
!-----------------------------------------------------------------------
!*    2.     CALL 'CUMASTR'(MASTER-ROUTINE FOR CUMULUS PARAMETERIZATION)
!
      CALL CUMASTR_NEW &
         (lq,       km,       km1,      km-1,    ZTP1,   &
          ZQP1,     PUM1,     PVM1,     PVERV,   ZQSAT,  &
          PQHFL,    ZTMST,    PAP,      PAPH,    PGEO,   &
          PTTE,     PQTE,     PVOM,     PVOL,    PRSFC,  & 
          PSSFC,    PAPRC,    PAPRSM,   PAPRS,   LOCUM,  &
          KTYPE,    ICBOT,    ICTOP,    ZTU,     ZQU,    &
          ZLU,      ZLUDE,    ZMFU,     ZMFD,    ZRAIN,  &
          PSRAIN,   PSEVAP,   PSHEAT,   PSDISS,  PSMELT, &
          PCTE,     PHHFL,       RHO,    sig1,     lndj)
!
!     TO INCLUDE THE CLOUD WATER AND CLOUD ICE DETRAINED FROM CONVECTION
!
      IF(fdbk.ge.1.0e-9) THEN
      DO 20 K=1,km
      DO 20 j=1,lq
      If(PCTE(j,k).GT.0.0) then
        ZTPP1=pt(j,k)+PTTE(j,k)*ZTMST
        if(ZTPP1.ge.t000) then
           fliq=1.0
           ZALF=0.0
        else if(ZTPP1.le.hgfr) then
           fliq=0.0
           ZALF=ALF
        else
           ZTC=ZTPP1-t000
           fliq=0.0059+0.9941*exp(-0.003102*ZTC*ZTC)
           ZALF=ALF
        endif
        fice=1.0-fliq
        pqc(j,k)=pqc(j,k)+fliq*PCTE(j,k)*ZTMST
        pqi(j,k)=pqi(j,k)+fice*PCTE(j,k)*ZTMST
        PTTE(j,k)=PTTE(j,k)-ZALF*RCPD*fliq*PCTE(j,k)
      Endif
   20 CONTINUE
      ENDIF
!
      DO 75 k=1,km
        DO 75 j=1,lq
          pt(j,k)=ZTP1(j,k)+PTTE(j,k)*ZTMST
          ZQP1(j,k)=ZQP1(j,k)+(PQTE(j,k)-ZQQ(j,k))*ZTMST
          pqv(j,k)=ZQP1(j,k)/(1.0-ZQP1(j,k))
   75 CONTINUE
      DO 85 j=1,lq
        zprecc(j)=amax1(0.0,(PRSFC(j)+PSSFC(j))*ZTMST)
   85 CONTINUE
      IF (LMFDUDV) THEN
        DO 100 k=1,km
          DO 100 j=1,lq
            pu(j,k)=pu(j,k)+PVOM(j,k)*ZTMST
            pv(j,k)=pv(j,k)+PVOL(j,k)*ZTMST
  100   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE TIECNV

!#############################################################
!
!             LEVEL 2 SUBROUTINEs
!
!#############################################################
!***********************************************************
!           SUBROUTINE CUMASTR_NEW
!***********************************************************
      SUBROUTINE CUMASTR_NEW                             &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   PTEN,  &
          PQEN,     PUEN,     PVEN,     PVERV,    PQSEN, &
          PQHFL,    ZTMST,    PAP,      PAPH,     PGEO,  &
          PTTE,     PQTE,     PVOM,     PVOL,     PRSFC, &
          PSSFC,    PAPRC,    PAPRSM,   PAPRS,    LDCUM, &
          KTYPE,    KCBOT,    KCTOP,    PTU,      PQU,   &
          PLU,      PLUDE,    PMFU,     PMFD,     PRAIN, &
          PSRAIN,   PSEVAP,   PSHEAT,   PSDISS,   PSMELT,& 
          PCTE,     PHHFL,       RHO,     sig1,     lndj)
!
!***CUMASTR*  MASTER ROUTINE FOR CUMULUS MASSFLUX-SCHEME
!     M.TIEDTKE      E.C.M.W.F.     1986/1987/1989
!***PURPOSE
!   -------
!          THIS ROUTINE COMPUTES THE PHYSICAL TENDENCIES OF THE
!     PROGNOSTIC VARIABLES T,Q,U AND V DUE TO CONVECTIVE PROCESSES.
!     PROCESSES CONSIDERED ARE: CONVECTIVE FLUXES, FORMATION OF
!     PRECIPITATION, EVAPORATION OF FALLING RAIN BELOW CLOUD BASE,
!     SATURATED CUMULUS DOWNDRAFTS.
!***INTERFACE.
!   ----------
!          *CUMASTR* IS CALLED FROM *MSSFLX*
!     THE ROUTINE TAKES ITS INPUT FROM THE LONG-TERM STORAGE
!     T,Q,U,V,PHI AND P AND MOISTURE TENDENCIES.
!     IT RETURNS ITS OUTPUT TO THE SAME SPACE
!      1.MODIFIED TENDENCIES OF MODEL VARIABLES
!      2.RATES OF CONVECTIVE PRECIPITATION
!        (USED IN SUBROUTINE SURF)
!      3.CLOUD BASE, CLOUD TOP AND PRECIP FOR RADIATION
!        (USED IN SUBROUTINE CLOUD)
!***METHOD
!   ------
!     PARAMETERIZATION IS DONE USING A MASSFLUX-SCHEME.
!        (1) DEFINE CONSTANTS AND PARAMETERS
!        (2) SPECIFY VALUES (T,Q,QS...) AT HALF LEVELS AND
!            INITIALIZE UPDRAFT- AND DOWNDRAFT-VALUES IN 'CUINI'
!        (3) CALCULATE CLOUD BASE IN 'CUBASE'
!            AND SPECIFY CLOUD BASE MASSFLUX FROM PBL MOISTURE BUDGET
!        (4) DO CLOUD ASCENT IN 'CUASC' IN ABSENCE OF DOWNDRAFTS
!        (5) DO DOWNDRAFT CALCULATIONS:
!              (A) DETERMINE VALUES AT LFS IN 'CUDLFS'
!              (B) DETERMINE MOIST DESCENT IN 'CUDDRAF'
!              (C) RECALCULATE CLOUD BASE MASSFLUX CONSIDERING THE
!                  EFFECT OF CU-DOWNDRAFTS
!        (6) DO FINAL CLOUD ASCENT IN 'CUASC'
!        (7) DO FINAL ADJUSMENTS TO CONVECTIVE FLUXES IN 'CUFLX',
!            DO EVAPORATION IN SUBCLOUD LAYER
!        (8) CALCULATE INCREMENTS OF T AND Q IN 'CUDTDQ'
!        (9) CALCULATE INCREMENTS OF U AND V IN 'CUDUDV'
!***EXTERNALS.
!   ----------
!       CUINI:  INITIALIZES VALUES AT VERTICAL GRID USED IN CU-PARAMETR.
!       CUBASE: CLOUD BASE CALCULATION FOR PENETR.AND SHALLOW CONVECTION
!       CUASC:  CLOUD ASCENT FOR ENTRAINING PLUME
!       CUDLFS: DETERMINES VALUES AT LFS FOR DOWNDRAFTS
!       CUDDRAF:DOES MOIST DESCENT FOR CUMULUS DOWNDRAFTS
!       CUFLX:  FINAL ADJUSTMENTS TO CONVECTIVE FLUXES (ALSO IN PBL)
!       CUDQDT: UPDATES TENDENCIES FOR T AND Q
!       CUDUDV: UPDATES TENDENCIES FOR U AND V
!***SWITCHES.
!   --------
!          LMFPEN=.T.   PENETRATIVE CONVECTION IS SWITCHED ON
!          LMFSCV=.T.   SHALLOW CONVECTION IS SWITCHED ON
!          LMFMID=.T.   MIDLEVEL CONVECTION IS SWITCHED ON
!          LMFDD=.T.    CUMULUS DOWNDRAFTS SWITCHED ON
!          LMFDUDV=.T.  CUMULUS FRICTION SWITCHED ON
!***
!     MODEL PARAMETERS (DEFINED IN SUBROUTINE CUPARAM)
!     ------------------------------------------------
!     ENTRPEN    ENTRAINMENT RATE FOR PENETRATIVE CONVECTION
!     ENTRSCV    ENTRAINMENT RATE FOR SHALLOW CONVECTION
!     ENTRMID    ENTRAINMENT RATE FOR MIDLEVEL CONVECTION
!     ENTRDD     ENTRAINMENT RATE FOR CUMULUS DOWNDRAFTS
!     CMFCTOP    RELATIVE CLOUD MASSFLUX AT LEVEL ABOVE NONBUOYANCY
!                LEVEL
!     CMFCMAX    MAXIMUM MASSFLUX VALUE ALLOWED FOR
!     CMFCMIN    MINIMUM MASSFLUX VALUE (FOR SAFETY)
!     CMFDEPS    FRACTIONAL MASSFLUX FOR DOWNDRAFTS AT LFS
!     CPRCON     COEFFICIENT FOR CONVERSION FROM CLOUD WATER TO RAIN
!***REFERENCE.
!   ----------
!          PAPER ON MASSFLUX SCHEME (TIEDTKE,1989)
!-----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KLEVM1
      REAL      ZTMST
      REAL      PSRAIN, PSEVAP, PSHEAT, PSDISS, PSMELT, ZCONS2
      INTEGER   JK,JL,IKB
      REAL      ZQUMQE, ZDQMIN, ZMFMAX, ZALVDCP, ZQALV
      REAL      ZHSAT, ZGAM, ZZZ, ZHHAT, ZBI, ZRO, ZDZ, ZDHDZ, ZDEPTH
      REAL      ZFAC, ZRH, ZPBMPT, DEPT, ZHT, ZEPS
      INTEGER   ICUM, ITOPM2
      REAL     PTEN(KLON,KLEV),        PQEN(KLON,KLEV), &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),  &
              PTTE(KLON,KLEV),        PQTE(KLON,KLEV),  &
              PVOM(KLON,KLEV),        PVOL(KLON,KLEV),  &
              PQSEN(KLON,KLEV),       PGEO(KLON,KLEV),  &
              PAP(KLON,KLEV),         PAPH(KLON,KLEVP1),& 
              PVERV(KLON,KLEV),       PQHFL(KLON),      &
              PHHFL(KLON),            RHO(KLON)
      REAL     PTU(KLON,KLEV),         PQU(KLON,KLEV),  &
              PLU(KLON,KLEV),         PLUDE(KLON,KLEV), &
              PMFU(KLON,KLEV),        PMFD(KLON,KLEV),  &
              PAPRC(KLON),            PAPRS(KLON),      &
              PAPRSM(KLON),           PRAIN(KLON),      &
              PRSFC(KLON),            PSSFC(KLON)
      REAL     ZTENH(KLON,KLEV),       ZQENH(KLON,KLEV),&
              ZGEOH(KLON,KLEV),       ZQSENH(KLON,KLEV),&
              ZTD(KLON,KLEV),         ZQD(KLON,KLEV),   &
              ZMFUS(KLON,KLEV),       ZMFDS(KLON,KLEV), &
              ZMFUQ(KLON,KLEV),       ZMFDQ(KLON,KLEV), &
              ZDMFUP(KLON,KLEV),      ZDMFDP(KLON,KLEV),& 
              ZMFUL(KLON,KLEV),       ZRFL(KLON),       &
              ZUU(KLON,KLEV),         ZVU(KLON,KLEV),   &
              ZUD(KLON,KLEV),         ZVD(KLON,KLEV)
      REAL     ZENTR(KLON),            ZHCBASE(KLON),   &
              ZMFUB(KLON),            ZMFUB1(KLON),     &
              ZDQPBL(KLON),           ZDQCV(KLON) 
      REAL     ZSFL(KLON),             ZDPMEL(KLON,KLEV), &
              PCTE(KLON,KLEV),        ZCAPE(KLON),        &
              ZHEAT(KLON),            ZHHATT(KLON,KLEV),  &
              ZHMIN(KLON),            ZRELH(KLON)
      REAL     sig1(KLEV)
      INTEGER  ILAB(KLON,KLEV),        IDTOP(KLON),   &
              ICTOP0(KLON),           ILWMIN(KLON)    
      INTEGER  KCBOT(KLON),            KCTOP(KLON),   &
              KTYPE(KLON),            IHMIN(KLON),    &
              KTOP0,                  lndj(KLON)
      LOGICAL  LDCUM(KLON)
      LOGICAL  LODDRAF(KLON),          LLO1
      REAL     CRIRH1
!-------------------------------------------
!     1.    SPECIFY CONSTANTS AND PARAMETERS
!-------------------------------------------
  100 CONTINUE
      ZCONS2=1./(G*ZTMST)
!--------------------------------------------------------------
!*    2.    INITIALIZE VALUES AT VERTICAL GRID POINTS IN 'CUINI'
!--------------------------------------------------------------
  200 CONTINUE
      CALL CUINI &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   PTEN,  &
          PQEN,     PQSEN,    PUEN,     PVEN,     PVERV, &
          PGEO,     PAPH,     ZGEOH,    ZTENH,    ZQENH,  &
          ZQSENH,   ILWMIN,   PTU,      PQU,      ZTD,   &
          ZQD,      ZUU,      ZVU,      ZUD,      ZVD,   &
          PMFU,     PMFD,     ZMFUS,    ZMFDS,    ZMFUQ, &
          ZMFDQ,    ZDMFUP,   ZDMFDP,   ZDPMEL,   PLU,  &
          PLUDE,    ILAB)
!----------------------------------
!*    3.0   CLOUD BASE CALCULATIONS
!----------------------------------
  300 CONTINUE
!*         (A) DETERMINE CLOUD BASE VALUES IN 'CUBASE'
!          -------------------------------------------
      CALL CUBASE &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   ZTENH, &
          ZQENH,    ZGEOH,    PAPH,     PTU,      PQU,   &
          PLU,      PUEN,     PVEN,     ZUU,      ZVU,   &
          LDCUM,    KCBOT,    ILAB)
!*          (B) DETERMINE TOTAL MOISTURE CONVERGENCE AND
!*              THEN DECIDE ON TYPE OF CUMULUS CONVECTION
!               -----------------------------------------
       JK=1
       DO 310 JL=1,KLON
       ZDQCV(JL) =PQTE(JL,JK)*(PAPH(JL,JK+1)-PAPH(JL,JK))
       ZDQPBL(JL)=0.0
       IDTOP(JL)=0
  310  CONTINUE
       DO 320 JK=2,KLEV
       DO 315 JL=1,KLON
       ZDQCV(JL)=ZDQCV(JL)+PQTE(JL,JK)*(PAPH(JL,JK+1)-PAPH(JL,JK))
       IF(JK.GE.KCBOT(JL)) ZDQPBL(JL)=ZDQPBL(JL)+PQTE(JL,JK)  &
                                    *(PAPH(JL,JK+1)-PAPH(JL,JK))
  315 CONTINUE
  320 CONTINUE

      if(cutrigger .eq. 1) then
        DO JL=1,KLON
         KTYPE(JL)=0
        IF(ZDQCV(JL).GT.MAX(0.,1.1*PQHFL(JL)*G)) THEN
         KTYPE(JL)=1
        ELSE
         KTYPE(JL)=2
        ENDIF
        END DO
      else if(cutrigger .eq. 2) then
         CALL CUTYPE  &
          ( KLON,     KLEV,     KLEVP1,   KLEVM1,  &
          ZTENH,   ZQENH,       ZQSENH,    ZGEOH,     PAPH,  &
          RHO,     PHHFL,         PQHFL,    KTYPE,    lndj   )
      end if
!*         (C) DETERMINE MOISTURE SUPPLY FOR BOUNDARY LAYER
!*             AND DETERMINE CLOUD BASE MASSFLUX IGNORING
!*             THE EFFECTS OF DOWNDRAFTS AT THIS STAGE
!              ------------------------------------------
!      do jl=1,klon
!        if(ktype(jl) .ge. 1 ) then
!              write(6,*)"ktype=", KTYPE(jl)
!        end if
!      end do

      DO 340 JL=1,KLON
      IKB=KCBOT(JL)
      ZQUMQE=PQU(JL,IKB)+PLU(JL,IKB)-ZQENH(JL,IKB)
      ZDQMIN=MAX(0.01*ZQENH(JL,IKB),1.E-10)
      IF(ZDQPBL(JL).GT.0..AND.ZQUMQE.GT.ZDQMIN.AND.LDCUM(JL)) THEN
         ZMFUB(JL)=ZDQPBL(JL)/(G*MAX(ZQUMQE,ZDQMIN))
      ELSE
         ZMFUB(JL)=0.01
         LDCUM(JL)=.FALSE.
      ENDIF
      ZMFMAX=(PAPH(JL,IKB)-PAPH(JL,IKB-1))*ZCONS2
      ZMFUB(JL)=MIN(ZMFUB(JL),ZMFMAX)
!------------------------------------------------------
!*    4.0   DETERMINE CLOUD ASCENT FOR ENTRAINING PLUME
!------------------------------------------------------
  400 CONTINUE
!*         (A) ESTIMATE CLOUD HEIGHT FOR ENTRAINMENT/DETRAINMENT
!*             CALCULATIONS IN CUASC (MAX.POSSIBLE CLOUD HEIGHT
!*             FOR NON-ENTRAINING PLUME, FOLLOWING A.-S.,1974)
! -------------------------------------------------------------
      IKB=KCBOT(JL)
      ZHCBASE(JL)=CPD*PTU(JL,IKB)+ZGEOH(JL,IKB)+ALV*PQU(JL,IKB)
      ICTOP0(JL)=KCBOT(JL)-1
  340 CONTINUE
      ZALVDCP=ALV/CPD
      ZQALV=1./ALV
      DO 420 JK=KLEVM1,3,-1
      DO 420 JL=1,KLON
      ZHSAT=CPD*ZTENH(JL,JK)+ZGEOH(JL,JK)+ALV*ZQSENH(JL,JK)
      ZGAM=C5LES*ZALVDCP*ZQSENH(JL,JK)/  &
          ((1.-VTMPC1*ZQSENH(JL,JK))*(ZTENH(JL,JK)-C4LES)**2)
      ZZZ=CPD*ZTENH(JL,JK)*0.608
      ZHHAT=ZHSAT-(ZZZ+ZGAM*ZZZ)/(1.+ZGAM*ZZZ*ZQALV)* &
                 MAX(ZQSENH(JL,JK)-ZQENH(JL,JK),0.)
      ZHHATT(JL,JK)=ZHHAT
      IF(JK.LT.ICTOP0(JL).AND.ZHCBASE(JL).GT.ZHHAT) ICTOP0(JL)=JK
  420 CONTINUE
      DO 430 JL=1,KLON
      JK=KCBOT(JL)
      ZHSAT=CPD*ZTENH(JL,JK)+ZGEOH(JL,JK)+ALV*ZQSENH(JL,JK)
      ZGAM=C5LES*ZALVDCP*ZQSENH(JL,JK)/   &
          ((1.-VTMPC1*ZQSENH(JL,JK))*(ZTENH(JL,JK)-C4LES)**2)
      ZZZ=CPD*ZTENH(JL,JK)*0.608
      ZHHAT=ZHSAT-(ZZZ+ZGAM*ZZZ)/(1.+ZGAM*ZZZ*ZQALV)* &
                 MAX(ZQSENH(JL,JK)-ZQENH(JL,JK),0.)
      ZHHATT(JL,JK)=ZHHAT
  430 CONTINUE
!
! Find lowest possible org. detrainment level
!
      DO 440 JL = 1, KLON
         ZHMIN(JL) = 0.
         IF( LDCUM(JL).AND.KTYPE(JL).EQ.1 ) THEN
            IHMIN(JL) = KCBOT(JL)
         ELSE
            IHMIN(JL) = -1
         END IF
 440  CONTINUE 
!
      ZBI = 1./(25.*G)
      DO 450 JK = KLEV, 1, -1
      DO 450 JL = 1, KLON
      LLO1 = LDCUM(JL).AND.KTYPE(JL).EQ.1.AND.IHMIN(JL).EQ.KCBOT(JL)
      IF (LLO1.AND.JK.LT.KCBOT(JL).AND.JK.GE.ICTOP0(JL)) THEN
        IKB = KCBOT(JL)
        ZRO = RD*ZTENH(JL,JK)/(G*PAPH(JL,JK))
        ZDZ = (PAPH(JL,JK)-PAPH(JL,JK-1))*ZRO
        ZDHDZ=(CPD*(PTEN(JL,JK-1)-PTEN(JL,JK))+ALV*(PQEN(JL,JK-1)-   &
          PQEN(JL,JK))+(PGEO(JL,JK-1)-PGEO(JL,JK)))*G/(PGEO(JL,      &
          JK-1)-PGEO(JL,JK))
        ZDEPTH = ZGEOH(JL,JK) - ZGEOH(JL,IKB)
        ZFAC = SQRT(1.+ZDEPTH*ZBI)
        ZHMIN(JL) = ZHMIN(JL) + ZDHDZ*ZFAC*ZDZ
        ZRH = -ALV*(ZQSENH(JL,JK)-ZQENH(JL,JK))*ZFAC
        IF (ZHMIN(JL).GT.ZRH) IHMIN(JL) = JK
      END IF
 450  CONTINUE 
      DO 460 JL = 1, KLON
      IF (LDCUM(JL).AND.KTYPE(JL).EQ.1) THEN
        IF (IHMIN(JL).LT.ICTOP0(JL)) IHMIN(JL) = ICTOP0(JL)
      END IF
      IF(KTYPE(JL).EQ.1) THEN
        ZENTR(JL)=ENTRPEN
      ELSE
        ZENTR(JL)=ENTRSCV
      ENDIF
      if(lndj(JL).eq.1) ZENTR(JL)=ZENTR(JL)*1.05
 460  CONTINUE 
!*         (B) DO ASCENT IN 'CUASC'IN ABSENCE OF DOWNDRAFTS
!----------------------------------------------------------
      CALL CUASC_NEW &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   ZTENH,   &
          ZQENH,    PUEN,     PVEN,     PTEN,     PQEN,    &
          PQSEN,    PGEO,     ZGEOH,    PAP,      PAPH,    &
          PQTE,     PVERV,    ILWMIN,   LDCUM,    ZHCBASE, &
          KTYPE,    ILAB,     PTU,      PQU,      PLU,     &
          ZUU,      ZVU,      PMFU,     ZMFUB,    ZENTR,   &
          ZMFUS,    ZMFUQ,    ZMFUL,    PLUDE,    ZDMFUP,  &
          KCBOT,    KCTOP,    ICTOP0,   ICUM,     ZTMST,   &
          IHMIN,    ZHHATT,   ZQSENH)
      IF(ICUM.EQ.0) GO TO 1000
!*     (C) CHECK CLOUD DEPTH AND CHANGE ENTRAINMENT RATE ACCORDINGLY
!          CALCULATE PRECIPITATION RATE (FOR DOWNDRAFT CALCULATION)
!------------------------------------------------------------------
      DO 480 JL=1,KLON
      ZPBMPT=PAPH(JL,KCBOT(JL))-PAPH(JL,KCTOP(JL))
      IF(LDCUM(JL)) ICTOP0(JL)=KCTOP(JL)
      IF(LDCUM(JL).AND.KTYPE(JL).EQ.1.AND.ZPBMPT.LT.ZDNOPRC) KTYPE(JL)=2
      IF(KTYPE(JL).EQ.2) then
        ZENTR(JL)=ENTRSCV
        if(lndj(JL).eq.1) ZENTR(JL)=ZENTR(JL)*1.05
      endif
      ZRFL(JL)=ZDMFUP(JL,1)
  480 CONTINUE
      DO 490 JK=2,KLEV
      DO 490 JL=1,KLON
          ZRFL(JL)=ZRFL(JL)+ZDMFUP(JL,JK)
  490 CONTINUE
!-----------------------------------------
!*    5.0   CUMULUS DOWNDRAFT CALCULATIONS
!-----------------------------------------
  500 CONTINUE
      IF(LMFDD) THEN
!*      (A) DETERMINE LFS IN 'CUDLFS'
!--------------------------------------
         CALL CUDLFS &
         (KLON,     KLEV,     KLEVP1,   ZTENH,    ZQENH,  &
          PUEN,     PVEN,     ZGEOH,    PAPH,     PTU,    &
          PQU,      ZUU,      ZVU,      LDCUM,    KCBOT,  &
          KCTOP,    ZMFUB,    ZRFL,     ZTD,      ZQD,    &
          ZUD,      ZVD,      PMFD,     ZMFDS,    ZMFDQ,  &
          ZDMFDP,   IDTOP,    LODDRAF)
!*     (B)  DETERMINE DOWNDRAFT T,Q AND FLUXES IN 'CUDDRAF'
!------------------------------------------------------------
         CALL CUDDRAF &
         (KLON,     KLEV,     KLEVP1,   ZTENH,    ZQENH,  &
          PUEN,     PVEN,     ZGEOH,    PAPH,     ZRFL,   &
          LODDRAF,  ZTD,      ZQD,      ZUD,      ZVD,    &
          PMFD,     ZMFDS,    ZMFDQ,    ZDMFDP)
!*     (C)  RECALCULATE CONVECTIVE FLUXES DUE TO EFFECT OF
!           DOWNDRAFTS ON BOUNDARY LAYER MOISTURE BUDGET
!-----------------------------------------------------------
      END IF
!
!-- 5.1 Recalculate cloud base massflux from a cape closure
!       for deep convection (ktype=1) and by PBL equilibrium
!       taking downdrafts into account for shallow convection
!       (ktype=2)
!       implemented by Y. WANG based on ECHAM4 in Nov. 2001.
!
      DO 510 JL=1,KLON
        ZHEAT(JL)=0.0
        ZCAPE(JL)=0.0
        ZRELH(JL)=0.0
        ZMFUB1(JL)=ZMFUB(JL)
  510 CONTINUE
!
      DO 511 JL=1,KLON
      IF(LDCUM(JL).AND.KTYPE(JL).EQ.1) THEN
      do jk=KLEVM1,2,-1
      if(abs(paph(jl,jk)*0.01 - 300) .lt. 50.) then
        KTOP0=MAX(jk,KCTOP(JL))
        exit
      end if
      end do
!      KTOP0=MAX(12,KCTOP(JL))
       DO JK=2,KLEV
       IF(JK.LE.KCBOT(JL).AND.JK.GT.KCTOP(JL)) THEN
         ZRO=PAPH(JL,JK)/(RD*ZTENH(JL,JK))
         ZDZ=(PAPH(JL,JK)-PAPH(JL,JK-1))/(G*ZRO)
         ZHEAT(JL)=ZHEAT(JL)+((PTEN(JL,JK-1)-PTEN(JL,JK)   &
           +G*ZDZ/CPD)/ZTENH(JL,JK)+0.608*(PQEN(JL,JK-1)-  &
           PQEN(JL,JK)))*(PMFU(JL,JK)+PMFD(JL,JK))*G/ZRO
         ZCAPE(JL)=ZCAPE(JL)+G*((PTU(JL,JK)*(1.+.608*PQU(JL,JK) &
           -PLU(JL,JK)))/(ZTENH(JL,JK)*(1.+.608*ZQENH(JL,JK))) &
           -1.0)*ZDZ
       ENDIF
       IF(JK.LE.KCBOT(JL).AND.JK.GT.KTOP0) THEN
         dept=(PAPH(JL,JK)-PAPH(JL,JK-1))/(PAPH(JL,KCBOT(JL))-  &
            PAPH(JL,KTOP0))
         ZRELH(JL)=ZRELH(JL)+dept*PQEN(JL,JK)/PQSEN(JL,JK)
       ENDIF
       ENDDO
!
       
       if(cutrigger .eq. 1 ) then 
         IF(lndj(JL).EQ.1) then
           CRIRH1=CRIRH*0.8
         ELSE
           CRIRH1=CRIRH
         ENDIF
       else
          CRIRH1=0.
       end if

       IF(ZRELH(JL).GE.CRIRH1 .AND. ZCAPE(JL) .GT. 100.) THEN
         IKB=KCBOT(JL)
         ZHT=ZCAPE(JL)/(ZTAU*ZHEAT(JL))
         ZMFUB1(JL)=MAX(ZMFUB(JL)*ZHT,0.01)
         ZMFMAX=(PAPH(JL,IKB)-PAPH(JL,IKB-1))*ZCONS2
         ZMFUB1(JL)=MIN(ZMFUB1(JL),ZMFMAX)
       ELSE
         ZMFUB1(JL)=0.01
         ZMFUB(JL)=0.01
         LDCUM(JL)=.FALSE.
        ENDIF
       ENDIF
  511  CONTINUE
!
!*  5.2   RECALCULATE CONVECTIVE FLUXES DUE TO EFFECT OF
!         DOWNDRAFTS ON BOUNDARY LAYER MOISTURE BUDGET
!--------------------------------------------------------
       DO 512 JL=1,KLON
        IF(KTYPE(JL).NE.1) THEN
           IKB=KCBOT(JL)
           IF(PMFD(JL,IKB).LT.0.0.AND.LODDRAF(JL)) THEN
              ZEPS=CMFDEPS
           ELSE
              ZEPS=0.
           ENDIF
           ZQUMQE=PQU(JL,IKB)+PLU(JL,IKB)-          &
                 ZEPS*ZQD(JL,IKB)-(1.-ZEPS)*ZQENH(JL,IKB)
           ZDQMIN=MAX(0.01*ZQENH(JL,IKB),1.E-10)
           ZMFMAX=(PAPH(JL,IKB)-PAPH(JL,IKB-1))*ZCONS2
           IF(ZDQPBL(JL).GT.0..AND.ZQUMQE.GT.ZDQMIN.AND.LDCUM(JL) &
             .AND.ZMFUB(JL).LT.ZMFMAX) THEN
              ZMFUB1(JL)=ZDQPBL(JL)/(G*MAX(ZQUMQE,ZDQMIN))
           ELSE
              ZMFUB1(JL)=ZMFUB(JL)
           ENDIF
           LLO1=(KTYPE(JL).EQ.2).AND.ABS(ZMFUB1(JL)  &
                -ZMFUB(JL)).LT.0.2*ZMFUB(JL)
           IF(.NOT.LLO1) ZMFUB1(JL)=ZMFUB(JL)
           ZMFUB1(JL)=MIN(ZMFUB1(JL),ZMFMAX)
        END IF
  512   CONTINUE
        DO 530 JK=1,KLEV
        DO 530 JL=1,KLON
        IF(LDCUM(JL)) THEN
           ZFAC=ZMFUB1(JL)/MAX(ZMFUB(JL),1.E-10)
           PMFD(JL,JK)=PMFD(JL,JK)*ZFAC
           ZMFDS(JL,JK)=ZMFDS(JL,JK)*ZFAC
           ZMFDQ(JL,JK)=ZMFDQ(JL,JK)*ZFAC
           ZDMFDP(JL,JK)=ZDMFDP(JL,JK)*ZFAC
        ELSE
           PMFD(JL,JK)=0.0
           ZMFDS(JL,JK)=0.0
           ZMFDQ(JL,JK)=0.0
           ZDMFDP(JL,JK)=0.0
        ENDIF
  530   CONTINUE
        DO 538 JL=1,KLON
           IF(LDCUM(JL)) THEN
              ZMFUB(JL)=ZMFUB1(JL)
           ELSE
              ZMFUB(JL)=0.0
           ENDIF
  538   CONTINUE
!
!---------------------------------------------------------------
!*    6.0      DETERMINE FINAL CLOUD ASCENT FOR ENTRAINING PLUME
!*             FOR PENETRATIVE CONVECTION (TYPE=1),
!*             FOR SHALLOW TO MEDIUM CONVECTION (TYPE=2)
!*             AND FOR MID-LEVEL CONVECTION (TYPE=3).
!---------------------------------------------------------------
  600 CONTINUE
      CALL CUASC_NEW &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   ZTENH,  &
          ZQENH,    PUEN,     PVEN,     PTEN,     PQEN,   &
          PQSEN,    PGEO,     ZGEOH,    PAP,      PAPH,   &
          PQTE,     PVERV,    ILWMIN,   LDCUM,    ZHCBASE,& 
          KTYPE,    ILAB,     PTU,      PQU,      PLU,    &
          ZUU,      ZVU,      PMFU,     ZMFUB,    ZENTR,  &
          ZMFUS,    ZMFUQ,    ZMFUL,    PLUDE,    ZDMFUP, &
          KCBOT,    KCTOP,    ICTOP0,   ICUM,     ZTMST,  &
          IHMIN,    ZHHATT,   ZQSENH)
!----------------------------------------------------------
!*    7.0      DETERMINE FINAL CONVECTIVE FLUXES IN 'CUFLX'
!----------------------------------------------------------
  700 CONTINUE
      CALL CUFLX &
         (KLON,     KLEV,     KLEVP1,   PQEN,     PQSEN,  &
          ZTENH,    ZQENH,    PAPH,     ZGEOH,    KCBOT,  &
          KCTOP,    IDTOP,    KTYPE,    LODDRAF,  LDCUM,  &
          PMFU,     PMFD,     ZMFUS,    ZMFDS,    ZMFUQ,  &
          ZMFDQ,    ZMFUL,    PLUDE,    ZDMFUP,   ZDMFDP, &
          ZRFL,     PRAIN,    PTEN,     ZSFL,     ZDPMEL, &
          ITOPM2,   ZTMST,    sig1)
!----------------------------------------------------------------
!*    8.0      UPDATE TENDENCIES FOR T AND Q IN SUBROUTINE CUDTDQ
!----------------------------------------------------------------
  800 CONTINUE
      CALL CUDTDQ                                          &
         (KLON,     KLEV,     KLEVP1,   ITOPM2,   PAPH,    &
          LDCUM,    PTEN,     PTTE,     PQTE,     ZMFUS,   &
          ZMFDS,    ZMFUQ,    ZMFDQ,    ZMFUL,    ZDMFUP,  &
          ZDMFDP,   ZTMST,    ZDPMEL,   PRAIN,    ZRFL,    &
          ZSFL,     PSRAIN,   PSEVAP,   PSHEAT,   PSMELT,  &
          PRSFC,    PSSFC,    PAPRC,    PAPRSM,   PAPRS,   &
          PQEN,     PQSEN,    PLUDE,    PCTE)
!----------------------------------------------------------------
!*    9.0      UPDATE TENDENCIES FOR U AND U IN SUBROUTINE CUDUDV
!----------------------------------------------------------------
  900 CONTINUE
      IF(LMFDUDV) THEN
      CALL CUDUDV  &
         (KLON,     KLEV,     KLEVP1,   ITOPM2,   KTYPE,   &
          KCBOT,    PAPH,     LDCUM,    PUEN,     PVEN,    &
          PVOM,     PVOL,     ZUU,      ZUD,      ZVU,     &
          ZVD,      PMFU,     PMFD,     PSDISS)
      END IF
 1000 CONTINUE
      RETURN
      END SUBROUTINE CUMASTR_NEW
!

!#############################################################
!
!             LEVEL 3 SUBROUTINEs
!
!#############################################################
!**********************************************
!       SUBROUTINE CUINI
!**********************************************
!
      SUBROUTINE CUINI                                    &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   PTEN,   &
          PQEN,     PQSEN,    PUEN,     PVEN,     PVERV,  &
          PGEO,     PAPH,     PGEOH,    PTENH,    PQENH,  &
          PQSENH,   KLWMIN,   PTU,      PQU,      PTD,    &
          PQD,      PUU,      PVU,      PUD,      PVD,    &
          PMFU,     PMFD,     PMFUS,    PMFDS,    PMFUQ,  &
          PMFDQ,    PDMFUP,   PDMFDP,   PDPMEL,   PLU,    &
          PLUDE,    KLAB)
!      M.TIEDTKE         E.C.M.W.F.     12/89
!***PURPOSE
!   -------
!          THIS ROUTINE INTERPOLATES LARGE-SCALE FIELDS OF T,Q ETC.
!          TO HALF LEVELS (I.E. GRID FOR MASSFLUX SCHEME),
!          AND INITIALIZES VALUES FOR UPDRAFTS AND DOWNDRAFTS
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!***METHOD.
!  --------
!          FOR EXTRAPOLATION TO HALF LEVELS SEE TIEDTKE(1989)
!***EXTERNALS
!   ---------
!          *CUADJTQ* TO SPECIFY QS AT HALF LEVELS
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   klevm1
      INTEGER   JK,JL,IK, ICALL
      REAL      ZDP, ZZS
      REAL     PTEN(KLON,KLEV),        PQEN(KLON,KLEV),    &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),     &
              PQSEN(KLON,KLEV),       PVERV(KLON,KLEV),    &
              PGEO(KLON,KLEV),        PGEOH(KLON,KLEV),    &
              PAPH(KLON,KLEVP1),      PTENH(KLON,KLEV),    &
              PQENH(KLON,KLEV),       PQSENH(KLON,KLEV)
      REAL     PTU(KLON,KLEV),         PQU(KLON,KLEV),     &
              PTD(KLON,KLEV),         PQD(KLON,KLEV),      &
              PUU(KLON,KLEV),         PUD(KLON,KLEV),      &
              PVU(KLON,KLEV),         PVD(KLON,KLEV),      &
              PMFU(KLON,KLEV),        PMFD(KLON,KLEV),     &
              PMFUS(KLON,KLEV),       PMFDS(KLON,KLEV),    &
              PMFUQ(KLON,KLEV),       PMFDQ(KLON,KLEV),    &
              PDMFUP(KLON,KLEV),      PDMFDP(KLON,KLEV),   & 
              PLU(KLON,KLEV),         PLUDE(KLON,KLEV)
      REAL     ZWMAX(KLON),            ZPH(KLON),          &
              PDPMEL(KLON,KLEV)
      INTEGER  KLAB(KLON,KLEV),        KLWMIN(KLON)
      LOGICAL  LOFLAG(KLON)
!------------------------------------------------------------
!*    1.       SPECIFY LARGE SCALE PARAMETERS AT HALF LEVELS
!*             ADJUST TEMPERATURE FIELDS IF STATICLY UNSTABLE
!*             FIND LEVEL OF MAXIMUM VERTICAL VELOCITY
! -----------------------------------------------------------
  100 CONTINUE
      ZDP=0.5
      DO 130 JK=2,KLEV
      DO 110 JL=1,KLON
      PGEOH(JL,JK)=PGEO(JL,JK)+(PGEO(JL,JK-1)-PGEO(JL,JK))*ZDP
      PTENH(JL,JK)=(MAX(CPD*PTEN(JL,JK-1)+PGEO(JL,JK-1),   &
                  CPD*PTEN(JL,JK)+PGEO(JL,JK))-PGEOH(JL,JK))*RCPD
      PQSENH(JL,JK)=PQSEN(JL,JK-1)
      ZPH(JL)=PAPH(JL,JK)
      LOFLAG(JL)=.TRUE.
  110 CONTINUE
      IK=JK
      ICALL=0
      CALL CUADJTQ(KLON,KLEV,IK,ZPH,PTENH,PQSENH,LOFLAG,ICALL)
      DO 120 JL=1,KLON
      PQENH(JL,JK)=MIN(PQEN(JL,JK-1),PQSEN(JL,JK-1))    &
                 +(PQSENH(JL,JK)-PQSEN(JL,JK-1))
      PQENH(JL,JK)=MAX(PQENH(JL,JK),0.)
  120 CONTINUE
  130 CONTINUE
      DO 140 JL=1,KLON
      PTENH(JL,KLEV)=(CPD*PTEN(JL,KLEV)+PGEO(JL,KLEV)-   &
                     PGEOH(JL,KLEV))*RCPD
      PQENH(JL,KLEV)=PQEN(JL,KLEV)
      PTENH(JL,1)=PTEN(JL,1)
      PQENH(JL,1)=PQEN(JL,1)
      PGEOH(JL,1)=PGEO(JL,1)
      KLWMIN(JL)=KLEV
      ZWMAX(JL)=0.
  140 CONTINUE
      DO 160 JK=KLEVM1,2,-1
      DO 150 JL=1,KLON
      ZZS=MAX(CPD*PTENH(JL,JK)+PGEOH(JL,JK),   &
             CPD*PTENH(JL,JK+1)+PGEOH(JL,JK+1))
      PTENH(JL,JK)=(ZZS-PGEOH(JL,JK))*RCPD
  150 CONTINUE
  160 CONTINUE
      DO 190 JK=KLEV,3,-1
      DO 180 JL=1,KLON
      IF(PVERV(JL,JK).LT.ZWMAX(JL)) THEN
         ZWMAX(JL)=PVERV(JL,JK)
         KLWMIN(JL)=JK
      END IF
  180 CONTINUE
  190 CONTINUE
!-----------------------------------------------------------
!*    2.0      INITIALIZE VALUES FOR UPDRAFTS AND DOWNDRAFTS
!-----------------------------------------------------------
  200 CONTINUE
      DO 230 JK=1,KLEV
      IK=JK-1
      IF(JK.EQ.1) IK=1
      DO 220 JL=1,KLON
      PTU(JL,JK)=PTENH(JL,JK)
      PTD(JL,JK)=PTENH(JL,JK)
      PQU(JL,JK)=PQENH(JL,JK)
      PQD(JL,JK)=PQENH(JL,JK)
      PLU(JL,JK)=0.
      PUU(JL,JK)=PUEN(JL,IK)
      PUD(JL,JK)=PUEN(JL,IK)
      PVU(JL,JK)=PVEN(JL,IK)
      PVD(JL,JK)=PVEN(JL,IK)
      PMFU(JL,JK)=0.
      PMFD(JL,JK)=0.
      PMFUS(JL,JK)=0.
      PMFDS(JL,JK)=0.
      PMFUQ(JL,JK)=0.
      PMFDQ(JL,JK)=0.
      PDMFUP(JL,JK)=0.
      PDMFDP(JL,JK)=0.
      PDPMEL(JL,JK)=0.
      PLUDE(JL,JK)=0.
      KLAB(JL,JK)=0
  220 CONTINUE
  230 CONTINUE
      RETURN
      END SUBROUTINE CUINI   

!**********************************************
!       SUBROUTINE CUBASE
!********************************************** 
      SUBROUTINE CUBASE &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   PTENH, &
          PQENH,    PGEOH,    PAPH,     PTU,      PQU,   &
          PLU,      PUEN,     PVEN,     PUU,      PVU,   &
          LDCUM,    KCBOT,    KLAB)
!      THIS ROUTINE CALCULATES CLOUD BASE VALUES (T AND Q)
!      FOR CUMULUS PARAMETERIZATION
!      M.TIEDTKE         E.C.M.W.F.     7/86 MODIF.  12/89
!***PURPOSE.
!   --------
!          TO PRODUCE CLOUD BASE VALUES FOR CU-PARAMETRIZATION
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!          INPUT ARE ENVIRONM. VALUES OF T,Q,P,PHI AT HALF LEVELS.
!          IT RETURNS CLOUD BASE VALUES AND FLAGS AS FOLLOWS;
!                 KLAB=1 FOR SUBCLOUD LEVELS
!                 KLAB=2 FOR CONDENSATION LEVEL
!***METHOD.
!  --------
!          LIFT SURFACE AIR DRY-ADIABATICALLY TO CLOUD BASE
!          (NON ENTRAINING PLUME,I.E.CONSTANT MASSFLUX)
!***EXTERNALS
!   ---------
!          *CUADJTQ* FOR ADJUSTING T AND Q DUE TO CONDENSATION IN ASCENT
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   klevm1
      INTEGER   JL,JK,IS,IK,ICALL,IKB
      REAL      ZBUO,ZZ
      REAL     PTENH(KLON,KLEV),       PQENH(KLON,KLEV),  &
              PGEOH(KLON,KLEV),       PAPH(KLON,KLEVP1)
      REAL     PTU(KLON,KLEV),         PQU(KLON,KLEV),   &
              PLU(KLON,KLEV)
      REAL     PUEN(KLON,KLEV),        PVEN(KLON,KLEV),  &
              PUU(KLON,KLEV),         PVU(KLON,KLEV) 
      REAL     ZQOLD(KLON,KLEV),       ZPH(KLON)
      INTEGER  KLAB(KLON,KLEV),        KCBOT(KLON)
      LOGICAL  LDCUM(KLON),            LOFLAG(KLON)
!***INPUT VARIABLES:
!       PTENH [ZTENH] - Environment Temperature on half levels. (CUINI)
!       PQENH [ZQENH] - Env. specific humidity on half levels. (CUINI)
!       PGEOH [ZGEOH] - Geopotential on half levels, (MSSFLX)
!       PAPH - Pressure of half levels. (MSSFLX)
!***VARIABLES MODIFIED BY CUBASE:
!       LDCUM - Logical denoting profiles. (CUBASE)
!       KTYPE - Convection type - 1: Penetrative  (CUMASTR)
!                                 2: Stratocumulus (CUMASTR)
!                                 3: Mid-level  (CUASC)
!       PTU - Cloud Temperature.
!       PQU - Cloud specific Humidity.
!       PLU - Cloud Liquid Water (Moisture condensed out)
!       KCBOT - Cloud Base Level. (CUBASE)
!       KLAB [ILAB] - Level Label - 1: Sub-cloud layer (CUBASE)
!------------------------------------------------
!     1.       INITIALIZE VALUES AT LIFTING LEVEL
!------------------------------------------------
  100 CONTINUE
      DO 110 JL=1,KLON
        KLAB(JL,KLEV)=1
        KCBOT(JL)=KLEVM1
        LDCUM(JL)=.FALSE.
        PUU(JL,KLEV)=PUEN(JL,KLEV)*(PAPH(JL,KLEVP1)-PAPH(JL,KLEV))
        PVU(JL,KLEV)=PVEN(JL,KLEV)*(PAPH(JL,KLEVP1)-PAPH(JL,KLEV))
  110 CONTINUE
!-------------------------------------------------------
!     2.0      DO ASCENT IN SUBCLOUD LAYER,
!              CHECK FOR EXISTENCE OF CONDENSATION LEVEL,
!              ADJUST T,Q AND L ACCORDINGLY IN *CUADJTQ*,
!              CHECK FOR BUOYANCY AND SET FLAGS
!-------------------------------------------------------
      DO 200 JK=1,KLEV
      DO 200 JL=1,KLON
        ZQOLD(JL,JK)=0.0
  200 CONTINUE
      DO 290 JK=KLEVM1,2,-1
        IS=0
        DO 210 JL=1,KLON
          IF(KLAB(JL,JK+1).EQ.1) THEN
             IS=IS+1
             LOFLAG(JL)=.TRUE.
          ELSE
             LOFLAG(JL)=.FALSE.
          ENDIF
          ZPH(JL)=PAPH(JL,JK)
  210   CONTINUE
        IF(IS.EQ.0) GO TO 290
        DO 220 JL=1,KLON
          IF(LOFLAG(JL)) THEN
             PQU(JL,JK)=PQU(JL,JK+1)
             PTU(JL,JK)=(CPD*PTU(JL,JK+1)+PGEOH(JL,JK+1)  &
                       -PGEOH(JL,JK))*RCPD
             ZBUO=PTU(JL,JK)*(1.+VTMPC1*PQU(JL,JK))-      &
                 PTENH(JL,JK)*(1.+VTMPC1*PQENH(JL,JK))+ZBUO0
             IF(ZBUO.GT.0.) KLAB(JL,JK)=1
             ZQOLD(JL,JK)=PQU(JL,JK)
          END IF
  220   CONTINUE
        IK=JK
        ICALL=1
        CALL CUADJTQ(KLON,KLEV,IK,ZPH,PTU,PQU,LOFLAG,ICALL)
        DO 240 JL=1,KLON
          IF(LOFLAG(JL).AND.PQU(JL,JK).NE.ZQOLD(JL,JK)) THEN
             KLAB(JL,JK)=2
             PLU(JL,JK)=PLU(JL,JK)+ZQOLD(JL,JK)-PQU(JL,JK)
             ZBUO=PTU(JL,JK)*(1.+VTMPC1*PQU(JL,JK))-      &
                 PTENH(JL,JK)*(1.+VTMPC1*PQENH(JL,JK))+ZBUO0
             IF(ZBUO.GT.0.) THEN
                KCBOT(JL)=JK
                LDCUM(JL)=.TRUE.
             END IF
          END IF
  240   CONTINUE
!             CALCULATE AVERAGES OF U AND V FOR SUBCLOUD ARA,.
!             THE VALUES WILL BE USED TO DEFINE CLOUD BASE VALUES.
        IF(LMFDUDV) THEN
           DO 250 JL=1,KLON
             IF(JK.GE.KCBOT(JL)) THEN
                PUU(JL,KLEV)=PUU(JL,KLEV)+           &
                          PUEN(JL,JK)*(PAPH(JL,JK+1)-PAPH(JL,JK))
                PVU(JL,KLEV)=PVU(JL,KLEV)+           &
                          PVEN(JL,JK)*(PAPH(JL,JK+1)-PAPH(JL,JK))
             END IF
 250       CONTINUE
        END IF
  290 CONTINUE
      IF(LMFDUDV) THEN
         DO 310 JL=1,KLON
         IF(LDCUM(JL)) THEN
            IKB=KCBOT(JL)
            ZZ=1./(PAPH(JL,KLEVP1)-PAPH(JL,IKB))
            PUU(JL,KLEV)=PUU(JL,KLEV)*ZZ
            PVU(JL,KLEV)=PVU(JL,KLEV)*ZZ
         ELSE
            PUU(JL,KLEV)=PUEN(JL,KLEVM1)
            PVU(JL,KLEV)=PVEN(JL,KLEVM1)
         END IF
 310     CONTINUE
      END IF
      RETURN
      END SUBROUTINE CUBASE

!**********************************************
!       SUBROUTINE CUTYPE
!********************************************** 
      SUBROUTINE CUTYPE    &
        ( KLON,     KLEV,     KLEVP1,   KLEVM1,&
          PTENH,   PQENH,     PQSENH,    PGEOH,   PAPH,&
          RHO,      HFX,         QFX,    KTYPE,   lndj   )
!      THIS ROUTINE CALCULATES CLOUD BASE and TOP
!      AND RETURN CLOUD TYPES
!      ZHANG & WANG      IPRC           12/2010
!***PURPOSE.
!   --------
!          TO PRODUCE CLOUD TYPE for CU-PARAMETERIZATIONS
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!          INPUT ARE ENVIRONM. VALUES OF T,Q,P,PHI AT HALF LEVELS.
!          IT RETURNS CLOUD TYPES AS FOLLOWS;
!                 KTYPE=1 FOR deep cumulus
!                 KTYPE=2 FOR shallow cumulus
!***METHOD.
!  --------
!          based on a simplified updraught equation
!            partial(Hup)/partial(z)=eta(H - Hup)
!            eta is the entrainment rate for test parcel
!            H stands for dry static energy or the total water specific humidity
!            references: Christian Jakob, 2003: A new subcloud model for mass-flux convection schemes
!                        influence on triggering, updraft properties, and model climate, Mon.Wea.Rev.
!                        131, 2765-2778
!            and
!                        IFS Documentation - Cy33r1 
!          
!***EXTERNALS
!   ---------
!          *CUADJTQ* FOR ADJUSTING T AND Q DUE TO CONDENSATION IN ASCENT
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   klevm1
      INTEGER   JL,JK,IS,IK,ICALL,IKB,LEVELS
      REAL     PTENH(KLON,KLEV),       PQENH(KLON,KLEV), &
                                       PQSENH(KLON,KLEV),&
               PGEOH(KLON,KLEV),       PAPH(KLON,KLEVP1)
      REAL     ZRELH(KLON)
      REAL     QFX(KLON),RHO(KLON),HFX(KLON)
      REAL     ZQOLD(KLON,KLEV),       ZPH(KLON)
      INTEGER  KCTOP(KLON),KCBOT(KLON)
      INTEGER  KTYPE(KLON),LCLFLAG(KLON)
      LOGICAL  TOPFLAG(KLON),DEEPFLAG(KLON),MYFLAG(KLON)

      REAL     part1(klon), part2(klon), root(klon)
      REAL     conw(klon),deltT(klon),deltQ(klon)
      REAL     eta(klon),dz(klon),coef(klon)
      REAL     dhen(KLON,KLEV), dh(KLON,KLEV),qh(KLON,KLEV)
      REAL      Tup(KLON,KLEV),Qup(KLON,KLEV),ql(KLON,KLEV)
      REAL       ww(KLON,KLEV),Kup(KLON,KLEV)
      REAL     Vtup(KLON,KLEV),Vten(KLON,KLEV),buoy(KLON,KLEV)

      INTEGER  lndj(KLON)
      REAL     CRIRH1
!***INPUT VARIABLES:
!       PTENH [ZTENH] - Environment Temperature on half levels. (CUINI)
!       PQENH [ZQENH] - Env. specific humidity on half levels. (CUINI)
!       PGEOH [ZGEOH] - Geopotential on half levels, (MSSFLX)
!       PAPH - Pressure of half levels. (MSSFLX)
!       RHO  - Density of the lowest Model level
!       QFX  - net upward moisture flux at the surface (kg/m^2/s)
!       HFX  - net upward heat flux at the surface (W/m^2)
!***VARIABLES OUTPUT BY CUTYPE:
!       KTYPE - Convection type - 1: Penetrative  (CUMASTR)
!                                 2: Stratocumulus (CUMASTR)
!                                 3: Mid-level  (CUASC)
!--------------------------------------------------------------
      DO JL=1,KLON
        KCBOT(JL)=KLEVM1
        KCTOP(JL)=KLEVM1
        KTYPE(JL)=0
      END DO
!-----------------------------------------------------------
! let's do test,and check the shallow convection first
! the first level is JK+1
! define deltaT and deltaQ
!-----------------------------------------------------------
      DO JK=1,KLEV
      DO JL=1,KLON
        ZQOLD(JL,JK)=0.0
           ql(jl,jk)=0.0  ! parcel liquid water
          Tup(jl,jk)=0.0  ! parcel temperature
          Qup(jl,jk)=0.0  ! parcel specific humidity
           dh(jl,jk)=0.0  ! parcel dry static energy
           qh(jl,jk)=0.0  ! parcel total water specific humidity
           ww(jl,jk)=0.0  ! parcel vertical speed (m/s)
         dhen(jl,jk)=0.0  ! environment dry static energy
          Kup(jl,jk)=0.0  ! updraught kinetic energy for parcel
         Vtup(jl,jk)=0.0  ! parcel virtual temperature considering water-loading
         Vten(jl,jk)=0.0  ! environment virtual temperature
         buoy(jl,jk)=0.0  ! parcel buoyancy
      END DO
      END DO

      do jl=1,klon
         lclflag(jl) = 0  ! flag for the condensation level
         conw(jl)    = 0.0 ! convective-scale velocity,also used for the vertical speed at the first level
         myflag(jl)  = .true. ! just as input for cuadjqt subroutine
        topflag(jl)  = .false.! flag for whether the cloud top is found
      end do

! check the levels from lowest level to second top level
      do JK=KLEVM1,2,-1
        DO JL=1,KLON
          ZPH(JL)=PAPH(JL,JK)
        END DO

! define the variables at the first level      
      if(jk .eq. KLEVM1) then
      do jl=1,klon
        part1(jl) = 1.5*0.4*pgeoh(jl,jk+1)/(rho(jl)*ptenh(jl,jk+1))
        part2(jl) = hfx(jl)/cpd+0.61*ptenh(jl,jk+1)*qfx(jl)
        root(jl) = 0.001-part1(jl)*part2(jl)
        if(root(jl) .gt. 0) then
          conw(jl) = 1.2*(root(jl))**(1.0/3.0)
        else
          conw(jl) = -1.2*(-root(jl))**(1.0/3.0)
        end if
        deltT(jl) = -1.5*hfx(jl)/(rho(jl)*cpd*conw(jl))
        deltQ(jl) = -1.5*qfx(jl)/(rho(jl)*conw(jl))

        Tup(jl,jk+1) = ptenh(jl,jk+1) + deltT(jl)
        Qup(jl,jk+1) = pqenh(jl,jk+1) + deltQ(jl)
         ql(jl,jk+1) = 0.
         dh(jl,jk+1) = pgeoh(jl,jk+1) + Tup(jl,jk+1)*cpd
         qh(jl,jk+1) = pqenh(jl,jk+1) + deltQ(jl) + ql(jl,jk+1)
         ww(jl,jk+1) = conw(jl)
      end do
      end if

! the next levels, we use the variables at the first level as initial values
      do jl=1,klon
      if(.not. topflag(jl)) then
        eta(jl) = 0.5*(0.55/(pgeoh(jl,jk)*zrg)+1.0e-3)
        dz(jl)  = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
        coef(jl)= eta(jl)*dz(jl)
        dhen(jl,jk) = pgeoh(jl,jk) + cpd*ptenh(jl,jk)
        dh(jl,jk) = (coef(jl)*dhen(jl,jk) + dh(jl,jk+1))/(1+coef(jl))
        qh(jl,jk) = (coef(jl)*pqenh(jl,jk)+ qh(jl,jk+1))/(1+coef(jl))
        Tup(jl,jk) = (dh(jl,jk)-pgeoh(jl,jk))*RCPD
        Qup(jl,jk) = qh(jl,jk) - ql(jl,jk+1)
        zqold(jl,jk) = Qup(jl,jk)
      end if
      end do
! check if the parcel is saturated
      ik=jk
      icall=1
      call CUADJTQ(klon,klev,ik,zph,Tup,Qup,myflag,icall)
      do jl=1,klon
        if( .not. topflag(jl) .and. zqold(jl,jk) .ne. Qup(jl,jk) ) then
          lclflag(jl) = lclflag(jl) + 1
          ql(jl,jk) = ql(jl,jk+1) + zqold(jl,jk) - Qup(jl,jk)
          dh(jl,jk) = pgeoh(jl,jk) + cpd*Tup(jl,jk)
        end if
      end do

! compute the updraft speed
      do jl=1,klon
        if(.not. topflag(jl))then
          Kup(jl,jk+1) = 0.5*ww(jl,jk+1)**2
          Vtup(jl,jk) = Tup(jl,jk)*(1.+VTMPC1*Qup(jl,jk)-ql(jl,jk))
          Vten(jl,jk) = ptenh(jl,jk)*(1.+VTMPC1*pqenh(jl,jk))
          buoy(jl,jk) = (Vtup(jl,jk) - Vten(jl,jk))/Vten(jl,jk)*g
          Kup(jl,jk)  = (Kup(jl,jk+1) + 0.333*dz(jl)*buoy(jl,jk))/ &
                        (1+2*2*eta(jl)*dz(jl))
          if(Kup(jl,jk) .gt. 0 ) then
             ww(jl,jk) = sqrt(2*Kup(jl,jk))
             if(lclflag(jl) .eq. 1 ) kcbot(jl) = jk
             if(jk .eq. 2) then
                kctop(jl) = jk
                topflag(jl)= .true.
             end if
          else
             ww(jl,jk) = 0
             kctop(jl) = jk + 1
             topflag(jl) = .true.
          end if
         end if
      end do
      end do ! end all the levels

      do jl=1,klon
        if(paph(jl,kcbot(jl)) - paph(jl,kctop(jl)) .lt. ZDNOPRC .and. &
          paph(jl,kcbot(jl)) - paph(jl,kctop(jl)) .gt. 0 &
           .and. lclflag(jl) .gt. 0) then
           ktype(jl) = 2
         end if
      end do

!-----------------------------------------------------------
! Next, let's check the deep convection
! the first level is JK
! define deltaT and deltaQ
!----------------------------------------------------------
! we check the parcel starting level by level (from the second lowest level to the next 12th level,
! usually, the 12th level around 700 hPa for common eta levels)
      do levels=KLEVM1-1,KLEVM1-12,-1
      DO JK=1,KLEV
      DO JL=1,KLON
        ZQOLD(JL,JK)=0.0
           ql(jl,jk)=0.0  ! parcel liquid water
          Tup(jl,jk)=0.0  ! parcel temperature
          Qup(jl,jk)=0.0  ! parcel specific humidity
           dh(jl,jk)=0.0  ! parcel dry static energy
           qh(jl,jk)=0.0  ! parcel total water specific humidity
           ww(jl,jk)=0.0  ! parcel vertical speed (m/s)
         dhen(jl,jk)=0.0  ! environment dry static energy
          Kup(jl,jk)=0.0  ! updraught kinetic energy for parcel
         Vtup(jl,jk)=0.0  ! parcel virtual temperature considering water-loading
         Vten(jl,jk)=0.0  ! environment virtual temperature
         buoy(jl,jk)=0.0  ! parcel buoyancy
      END DO
      END DO

      do jl=1,klon
         lclflag(jl) = 0  ! flag for the condensation level
         kctop(jl) = levels
         kcbot(jl) = levels
         myflag(jl)  = .true. ! just as input for cuadjqt subroutine
        topflag(jl)  = .false.! flag for whether the cloud top is found
      end do

! check the levels from lowest level to second top level
      do JK=levels,2,-1
        DO JL=1,KLON
          ZPH(JL)=PAPH(JL,JK)
        END DO

! define the variables at the first level      
      if(jk .eq. levels) then
      do jl=1,klon
        deltT(jl) = 0.2
        deltQ(jl) = 1.0e-4

        if(paph(jl,KLEVM1-1)-paph(jl,jk) .le. 6.e3) then
         ql(jl,jk+1) = 0.
        Tup(jl,jk+1) = 0.25*(ptenh(jl,jk+1)+ptenh(jl,jk)+ &
                             ptenh(jl,jk-1)+ptenh(jl,jk-2)) + &
                      deltT(jl)
        dh(jl,jk+1) = 0.25*(pgeoh(jl,jk+1)+pgeoh(jl,jk)+ &
                            pgeoh(jl,jk-1)+pgeoh(jl,jk-2)) + &
                      Tup(jl,jk+1)*cpd 
        qh(jl,jk+1) = 0.25*(pqenh(jl,jk+1)+pqenh(jl,jk)+ &
                            pqenh(jl,jk-1)+pqenh(jl,jk-2))+ &
                      deltQ(jl) + ql(jl,jk+1)
        Qup(jl,jk+1) = qh(jl,jk+1) - ql(jl,jk+1)
        else
         ql(jl,jk+1) = 0.
        Tup(jl,jk+1) = ptenh(jl,jk+1) + deltT(jl)
         dh(jl,jk+1) = pgeoh(jl,jk+1) + Tup(jl,jk+1)*cpd
         qh(jl,jk+1) = pqenh(jl,jk+1) + deltQ(jl)
        Qup(jl,jk+1) =    qh(jl,jk+1) - ql(jl,jk+1)
        end if
      ww(jl,jk+1) = 1.0

      end do
      end if

! the next levels, we use the variables at the first level as initial values
      do jl=1,klon
      if(.not. topflag(jl)) then
        eta(jl) = 1.1e-4
        dz(jl)  = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
        coef(jl)= eta(jl)*dz(jl)
        dhen(jl,jk) = pgeoh(jl,jk) + cpd*ptenh(jl,jk)
        dh(jl,jk) = (coef(jl)*dhen(jl,jk) + dh(jl,jk+1))/(1+coef(jl))
        qh(jl,jk) = (coef(jl)*pqenh(jl,jk)+ qh(jl,jk+1))/(1+coef(jl))
        Tup(jl,jk) = (dh(jl,jk)-pgeoh(jl,jk))*RCPD
        Qup(jl,jk) = qh(jl,jk) - ql(jl,jk+1)
        zqold(jl,jk) = Qup(jl,jk)
      end if
      end do
! check if the parcel is saturated
      ik=jk
      icall=1
      call CUADJTQ(klon,klev,ik,zph,Tup,Qup,myflag,icall)
      do jl=1,klon
        if( .not. topflag(jl) .and. zqold(jl,jk) .ne. Qup(jl,jk) ) then
          lclflag(jl) = lclflag(jl) + 1
          ql(jl,jk) = ql(jl,jk+1) + zqold(jl,jk) - Qup(jl,jk)
          dh(jl,jk) = pgeoh(jl,jk) + cpd*Tup(jl,jk)
        end if
      end do

! compute the updraft speed
      do jl=1,klon
        if(.not. topflag(jl))then
          Kup(jl,jk+1) = 0.5*ww(jl,jk+1)**2
          Vtup(jl,jk) = Tup(jl,jk)*(1.+VTMPC1*Qup(jl,jk)-ql(jl,jk))
          Vten(jl,jk) = ptenh(jl,jk)*(1.+VTMPC1*pqenh(jl,jk))
          buoy(jl,jk) = (Vtup(jl,jk) - Vten(jl,jk))/Vten(jl,jk)*g
          Kup(jl,jk)  = (Kup(jl,jk+1) + 0.333*dz(jl)*buoy(jl,jk))/ &
                        (1+2*2*eta(jl)*dz(jl))
          if(Kup(jl,jk) .gt. 0 ) then
             ww(jl,jk) = sqrt(2*Kup(jl,jk))
             if(lclflag(jl) .eq. 1 ) kcbot(jl) = jk
             if(jk .eq. 2) then
                kctop(jl) = jk
                topflag(jl)= .true.
             end if
          else
             ww(jl,jk) = 0
             kctop(jl) = jk + 1
             topflag(jl) = .true.
          end if
         end if
      end do
      end do ! end all the levels

      do jl = 1, klon
       if(paph(jl,kcbot(jl)) - paph(jl,kctop(jl)) .gt. ZDNOPRC .and. &
              lclflag(jl) .gt. 0 ) then
         ZRELH(JL) = 0.
         do jk=kcbot(jl),kctop(jl),-1
           ZRELH(JL)=ZRELH(JL)+ PQENH(JL,JK)/PQSENH(JL,JK)
         end do
         ZRELH(JL) = ZRELH(JL)/(kcbot(jl)-kctop(jl)+1)

         if(lndj(JL) .eq. 1) then
           CRIRH1 = CRIRH*0.8
         else
           CRIRH1 = CRIRH
         end if
         if(ZRELH(JL) .ge. CRIRH1) ktype(jl)  = 1
       end if
      end do

       end do ! end all cycles

      END SUBROUTINE CUTYPE

!
!**********************************************
!       SUBROUTINE CUASC_NEW
!********************************************** 
      SUBROUTINE CUASC_NEW &
         (KLON,     KLEV,     KLEVP1,   KLEVM1,   PTENH,  &
          PQENH,    PUEN,     PVEN,     PTEN,     PQEN,   &
          PQSEN,    PGEO,     PGEOH,    PAP,      PAPH,   &
          PQTE,     PVERV,    KLWMIN,   LDCUM,    PHCBASE,& 
          KTYPE,    KLAB,     PTU,      PQU,      PLU,    &
          PUU,      PVU,      PMFU,     PMFUB,    PENTR,  &
          PMFUS,    PMFUQ,    PMFUL,    PLUDE,    PDMFUP, & 
          KCBOT,    KCTOP,    KCTOP0,   KCUM,     ZTMST,  &
          KHMIN,    PHHATT,   PQSENH)
!     THIS ROUTINE DOES THE CALCULATIONS FOR CLOUD ASCENTS
!     FOR CUMULUS PARAMETERIZATION
!     M.TIEDTKE         E.C.M.W.F.     7/86 MODIF.  12/89
!     Y.WANG            IPRC           11/01 MODIF.
!***PURPOSE.
!   --------
!          TO PRODUCE CLOUD ASCENTS FOR CU-PARAMETRIZATION
!          (VERTICAL PROFILES OF T,Q,L,U AND V AND CORRESPONDING
!           FLUXES AS WELL AS PRECIPITATION RATES)
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!***METHOD.
!  --------
!          LIFT SURFACE AIR DRY-ADIABATICALLY TO CLOUD BASE
!          AND THEN CALCULATE MOIST ASCENT FOR
!          ENTRAINING/DETRAINING PLUME.
!          ENTRAINMENT AND DETRAINMENT RATES DIFFER FOR
!          SHALLOW AND DEEP CUMULUS CONVECTION.
!          IN CASE THERE IS NO PENETRATIVE OR SHALLOW CONVECTION
!          CHECK FOR POSSIBILITY OF MID LEVEL CONVECTION
!          (CLOUD BASE VALUES CALCULATED IN *CUBASMC*)
!***EXTERNALS
!   ---------
!          *CUADJTQ* ADJUST T AND Q DUE TO CONDENSATION IN ASCENT
!          *CUENTR_NEW*  CALCULATE ENTRAINMENT/DETRAINMENT RATES
!          *CUBASMC* CALCULATE CLOUD BASE VALUES FOR MIDLEVEL CONVECTION
!***REFERENCE
!   ---------
!          (TIEDTKE,1989)
!***INPUT VARIABLES:
!       PTENH [ZTENH] - Environ Temperature on half levels. (CUINI)
!       PQENH [ZQENH] - Env. specific humidity on half levels. (CUINI)
!       PUEN - Environment wind u-component. (MSSFLX)
!       PVEN - Environment wind v-component. (MSSFLX)
!       PTEN - Environment Temperature. (MSSFLX)
!       PQEN - Environment Specific Humidity. (MSSFLX)
!       PQSEN - Environment Saturation Specific Humidity. (MSSFLX)
!       PGEO - Geopotential. (MSSFLX)
!       PGEOH [ZGEOH] - Geopotential on half levels, (MSSFLX)
!       PAP - Pressure in Pa.  (MSSFLX)
!       PAPH - Pressure of half levels. (MSSFLX)
!       PQTE - Moisture convergence (Delta q/Delta t). (MSSFLX)
!       PVERV - Large Scale Vertical Velocity (Omega). (MSSFLX)
!       KLWMIN [ILWMIN] - Level of Minimum Omega. (CUINI)
!       KLAB [ILAB] - Level Label - 1: Sub-cloud layer.
!                                   2: Condensation Level (Cloud Base)
!       PMFUB [ZMFUB] - Updraft Mass Flux at Cloud Base. (CUMASTR)
!***VARIABLES MODIFIED BY CUASC:
!       LDCUM - Logical denoting profiles. (CUBASE)
!       KTYPE - Convection type - 1: Penetrative  (CUMASTR)
!                                 2: Stratocumulus (CUMASTR)
!                                 3: Mid-level  (CUASC)
!       PTU - Cloud Temperature.
!       PQU - Cloud specific Humidity.
!       PLU - Cloud Liquid Water (Moisture condensed out)
!       PUU [ZUU] - Cloud Momentum U-Component.
!       PVU [ZVU] - Cloud Momentum V-Component.
!       PMFU - Updraft Mass Flux.
!       PENTR [ZENTR] - Entrainment Rate. (CUMASTR ) (CUBASMC)
!       PMFUS [ZMFUS] - Updraft Flux of Dry Static Energy. (CUBASMC)
!       PMFUQ [ZMFUQ] - Updraft Flux of Specific Humidity.
!       PMFUL [ZMFUL] - Updraft Flux of Cloud Liquid Water.
!       PLUDE - Liquid Water Returned to Environment by Detrainment.
!       PDMFUP [ZMFUP] - FLUX DIFFERENCE OF PRECIP. IN UPDRAFTS
!       KCBOT - Cloud Base Level. (CUBASE)
!       KCTOP -
!       KCTOP0 [ICTOP0] - Estimate of Cloud Top. (CUMASTR)
!       KCUM [ICUM] -
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   klevm1,kcum
      REAL      ZTMST,ZCONS2,ZDZ,ZDRODZ
      INTEGER   JL,JK,IKB,IK,IS,IKT,ICALL
      REAL      ZMFMAX,ZFAC,ZMFTEST,ZDPRHO,ZMSE,ZNEVN,ZODMAX
      REAL      ZQEEN,ZSEEN,ZSCDE,ZGA,ZDT,ZSCOD
      REAL      ZQUDE,ZQCOD, ZMFUSK, ZMFUQK,ZMFULK
      REAL      ZBUO, ZPRCON, ZLNEW, ZZ, ZDMFEU, ZDMFDU
      REAL      ZBUOYZ,ZZDMF
      REAL     PTENH(KLON,KLEV),       PQENH(KLON,KLEV), &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),   &
              PTEN(KLON,KLEV),        PQEN(KLON,KLEV),   &
              PGEO(KLON,KLEV),        PGEOH(KLON,KLEV),  &
              PAP(KLON,KLEV),         PAPH(KLON,KLEVP1), &
              PQSEN(KLON,KLEV),       PQTE(KLON,KLEV),   &
              PVERV(KLON,KLEV),       PQSENH(KLON,KLEV)  
      REAL     PTU(KLON,KLEV),         PQU(KLON,KLEV),   &
              PUU(KLON,KLEV),         PVU(KLON,KLEV),    &
              PMFU(KLON,KLEV),        ZPH(KLON),         &
              PMFUB(KLON),            PENTR(KLON),       &
              PMFUS(KLON,KLEV),       PMFUQ(KLON,KLEV),  &
              PLU(KLON,KLEV),         PLUDE(KLON,KLEV),  &
              PMFUL(KLON,KLEV),       PDMFUP(KLON,KLEV)
      REAL     ZDMFEN(KLON),           ZDMFDE(KLON),     &
              ZMFUU(KLON),            ZMFUV(KLON),       &
              ZPBASE(KLON),           ZQOLD(KLON),       &
              PHHATT(KLON,KLEV),      ZODETR(KLON,KLEV), &
              ZOENTR(KLON,KLEV),      ZBUOY(KLON)
      REAL     PHCBASE(KLON)
      INTEGER  KLWMIN(KLON),           KTYPE(KLON),      &
              KLAB(KLON,KLEV),        KCBOT(KLON),       &
              KCTOP(KLON),            KCTOP0(KLON),      &
              KHMIN(KLON)
      LOGICAL LDCUM(KLON),            LOFLAG(KLON)
      integer leveltop,levelbot
      real    tt(klon),ttb(klon)
      real    zqsat(klon), zqsatb(klon)
      real    fscale(klon)

!--------------------------------
!*    1.       SPECIFY PARAMETERS
!--------------------------------
  100 CONTINUE
      ZCONS2=1./(G*ZTMST)
!---------------------------------
!     2.        SET DEFAULT VALUES
!---------------------------------
  200 CONTINUE
      DO 210 JL=1,KLON
        ZMFUU(JL)=0.
        ZMFUV(JL)=0.
        ZBUOY(JL)=0.
        IF(.NOT.LDCUM(JL)) KTYPE(JL)=0
  210 CONTINUE
      DO 230 JK=1,KLEV
      DO 230 JL=1,KLON
          PLU(JL,JK)=0.
          PMFU(JL,JK)=0.
          PMFUS(JL,JK)=0.
          PMFUQ(JL,JK)=0.
          PMFUL(JL,JK)=0.
          PLUDE(JL,JK)=0.
          PDMFUP(JL,JK)=0.
          ZOENTR(JL,JK)=0.
          ZODETR(JL,JK)=0.
          IF(.NOT.LDCUM(JL).OR.KTYPE(JL).EQ.3) KLAB(JL,JK)=0
          IF(.NOT.LDCUM(JL).AND.PAPH(JL,JK).LT.4.E4) KCTOP0(JL)=JK
  230 CONTINUE
!------------------------------------------------
!     3.0      INITIALIZE VALUES AT LIFTING LEVEL
!------------------------------------------------
      DO 310 JL=1,KLON
        KCTOP(JL)=KLEVM1
        IF(.NOT.LDCUM(JL)) THEN
           KCBOT(JL)=KLEVM1
           PMFUB(JL)=0.
           PQU(JL,KLEV)=0.
        END IF
        PMFU(JL,KLEV)=PMFUB(JL)
        PMFUS(JL,KLEV)=PMFUB(JL)*(CPD*PTU(JL,KLEV)+PGEOH(JL,KLEV))
        PMFUQ(JL,KLEV)=PMFUB(JL)*PQU(JL,KLEV)
        IF(LMFDUDV) THEN
           ZMFUU(JL)=PMFUB(JL)*PUU(JL,KLEV)
           ZMFUV(JL)=PMFUB(JL)*PVU(JL,KLEV)
        END IF
  310 CONTINUE
!
!-- 3.1 Find organized entrainment at cloud base
!
      DO 322 JL=1,KLON
      LDCUM(JL)=.FALSE.
      IF (KTYPE(JL).EQ.1) THEN
       IKB = KCBOT(JL)
       if(orgen .eq. 1 ) then
! old scheme
       ZBUOY(JL)=G*((PTU(JL,IKB)-PTENH(JL,IKB))/PTENH(JL,IKB)+ &
               0.608*(PQU(JL,IKB)-PQENH(JL,IKB)))
       IF (ZBUOY(JL).GT.0.) THEN
        ZDZ = (PGEO(JL,IKB-1)-PGEO(JL,IKB))*ZRG
        ZDRODZ = -LOG(PTEN(JL,IKB-1)/PTEN(JL,IKB))/ZDZ -  &
                 G/(RD*PTENH(JL,IKB))
        ZOENTR(JL,IKB-1)=ZBUOY(JL)*0.5/(1.+ZBUOY(JL)*ZDZ) &
                +ZDRODZ
        ZOENTR(JL,IKB-1) = MIN(ZOENTR(JL,IKB-1),1.E-3)
        ZOENTR(JL,IKB-1) = MAX(ZOENTR(JL,IKB-1),0.)
       END IF
! New scheme
! Let's define the fscale
        else if(orgen .eq. 2 ) then
        tt(jl) = ptenh(jl,ikb-1)
        zqsat(jl) = TLUCUA(tt(jl))/paph(jl,ikb-1)
        zqsat(jl) = zqsat(jl)/(1.-VTMPC1*zqsat(jl))
        ttb(jl) = ptenh(jl,ikb)
        zqsatb(jl) = TLUCUA(ttb(jl))/paph(jl,ikb)
        zqsatb(jl) = zqsatb(jl)/(1.-VTMPC1*zqsatb(jl))
        fscale(jl) = (zqsat(jl)/zqsatb(jl))**3
! end of defining the fscale
        zoentr(jl,ikb-1) = 1.E-3*(1.3-PQEN(jl,ikb-1)/PQSEN(jl,ikb-1))*fscale(jl)
        zoentr(jl,ikb-1) = MIN(zoentr(jl,ikb-1),1.E-3)
        zoentr(jl,ikb-1) = MAX(zoentr(jl,ikb-1),0.)
       end if
      END IF
  322 CONTINUE 
!
!-----------------------------------------------------------------
!     4.       DO ASCENT: SUBCLOUD LAYER (KLAB=1) ,CLOUDS (KLAB=2)
!              BY DOING FIRST DRY-ADIABATIC ASCENT AND THEN
!              BY ADJUSTING T,Q AND L ACCORDINGLY IN *CUADJTQ*,
!              THEN CHECK FOR BUOYANCY AND SET FLAGS ACCORDINGLY
!-----------------------------------------------------------------
  400 CONTINUE

! let's define the levels in which the middle level convection could be activated
      do jk=KLEVM1,2,-1
      if(abs(paph(1,jk)*0.01 - 250) .lt. 50.) then
        leveltop = jk
        exit
      end if
      end do
      leveltop = min(KLEV-15,leveltop)
      levelbot = KLEVM1 - 4
        
      DO 480 JK=KLEVM1,2,-1
!                  SPECIFY CLOUD BASE VALUES FOR MIDLEVEL CONVECTION
!                  IN *CUBASMC* IN CASE THERE IS NOT ALREADY CONVECTION
! ---------------------------------------------------------------------
      IK=JK
      IF(LMFMID.AND.IK.LT.levelbot.AND.IK.GT.leveltop) THEN
      CALL CUBASMC  &
         (KLON,     KLEV,     KLEVM1,   IK,      PTEN,  &
          PQEN,     PQSEN,    PUEN,     PVEN,    PVERV, &
          PGEO,     PGEOH,    LDCUM,    KTYPE,   KLAB,  &
          PMFU,     PMFUB,    PENTR,    KCBOT,   PTU,   &
          PQU,      PLU,      PUU,     PVU,      PMFUS, &
          PMFUQ,    PMFUL,    PDMFUP,  ZMFUU,    ZMFUV)
      ENDIF
      IS=0
      DO 410 JL=1,KLON
        ZQOLD(JL)=0.0
        IS=IS+KLAB(JL,JK+1)
        IF(KLAB(JL,JK+1).EQ.0) KLAB(JL,JK)=0
        LOFLAG(JL)=KLAB(JL,JK+1).GT.0
        ZPH(JL)=PAPH(JL,JK)
        IF(KTYPE(JL).EQ.3.AND.JK.EQ.KCBOT(JL)) THEN
           ZMFMAX=(PAPH(JL,JK)-PAPH(JL,JK-1))*ZCONS2
           IF(PMFUB(JL).GT.ZMFMAX) THEN
              ZFAC=ZMFMAX/PMFUB(JL)
              PMFU(JL,JK+1)=PMFU(JL,JK+1)*ZFAC
              PMFUS(JL,JK+1)=PMFUS(JL,JK+1)*ZFAC
              PMFUQ(JL,JK+1)=PMFUQ(JL,JK+1)*ZFAC
              ZMFUU(JL)=ZMFUU(JL)*ZFAC
              ZMFUV(JL)=ZMFUV(JL)*ZFAC
              PMFUB(JL)=ZMFMAX
           END IF
        END IF
  410 CONTINUE
      IF(IS.EQ.0) GO TO 480
!
!*     SPECIFY ENTRAINMENT RATES IN *CUENTR_NEW*
! -------------------------------------
      IK=JK
      CALL CUENTR_NEW &
         (KLON,     KLEV,     KLEVP1,   IK,       PTENH,&
          PAPH,     PAP,      PGEOH,    KLWMIN,   LDCUM,&
          KTYPE,    KCBOT,    KCTOP0,   ZPBASE,   PMFU, &
          PENTR,    ZDMFEN,   ZDMFDE,   ZODETR,   KHMIN)
!
!      DO ADIABATIC ASCENT FOR ENTRAINING/DETRAINING PLUME
! -------------------------------------------------------
! Do adiabatic ascent for entraining/detraining plume
! the cloud ensemble entrains environmental values
! in turbulent detrainment cloud ensemble values are detrained
! in organized detrainment the dry static energy and
! moisture that are neutral compared to the
! environmental air are detrained
!
      DO 420 JL=1,KLON
      IF(LOFLAG(JL)) THEN
        IF(JK.LT.KCBOT(JL)) THEN
         ZMFTEST=PMFU(JL,JK+1)+ZDMFEN(JL)-ZDMFDE(JL)
         ZMFMAX=MIN(ZMFTEST,(PAPH(JL,JK)-PAPH(JL,JK-1))*ZCONS2)
         ZDMFEN(JL)=MAX(ZDMFEN(JL)-MAX(ZMFTEST-ZMFMAX,0.),0.)
        END IF
        ZDMFDE(JL)=MIN(ZDMFDE(JL),0.75*PMFU(JL,JK+1))
        PMFU(JL,JK)=PMFU(JL,JK+1)+ZDMFEN(JL)-ZDMFDE(JL)
        IF (JK.LT.kcbot(jl)) THEN
          zdprho = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
          zoentr(jl,jk) = zoentr(jl,jk)*zdprho*pmfu(jl,jk+1)
          zmftest = pmfu(jl,jk) + zoentr(jl,jk)-zodetr(jl,jk)
          zmfmax = MIN(zmftest,(paph(jl,jk)-paph(jl,jk-1))*zcons2)
          zoentr(jl,jk) = MAX(zoentr(jl,jk)-MAX(zmftest-zmfmax,0.),0.)
        END IF
!
! limit organized detrainment to not allowing for too deep clouds
!
        IF (ktype(jl).EQ.1.AND.jk.LT.kcbot(jl).AND.jk.LE.khmin(jl)) THEN
          zmse = cpd*ptu(jl,jk+1) + alv*pqu(jl,jk+1) + pgeoh(jl,jk+1)
          ikt = kctop0(jl)
          znevn=(pgeoh(jl,ikt)-pgeoh(jl,jk+1))*(zmse-phhatt(jl,  &
               jk+1))*zrg
          IF (znevn.LE.0.) znevn = 1.
          zdprho = (pgeoh(jl,jk)-pgeoh(jl,jk+1))*zrg
          zodmax = ((phcbase(jl)-zmse)/znevn)*zdprho*pmfu(jl,jk+1)
          zodmax = MAX(zodmax,0.)
          zodetr(jl,jk) = MIN(zodetr(jl,jk),zodmax)
        END IF
        zodetr(jl,jk) = MIN(zodetr(jl,jk),0.75*pmfu(jl,jk))
        pmfu(jl,jk) = pmfu(jl,jk) + zoentr(jl,jk) - zodetr(jl,jk)
        ZQEEN=PQENH(JL,JK+1)*ZDMFEN(JL)
        zqeen=zqeen + pqenh(jl,jk+1)*zoentr(jl,jk)
        ZSEEN=(CPD*PTENH(JL,JK+1)+PGEOH(JL,JK+1))*ZDMFEN(JL)
        zseen=zseen+(cpd*ptenh(jl,jk+1)+pgeoh(jl,jk+1))*  &
             zoentr(jl,jk)
        ZSCDE=(CPD*PTU(JL,JK+1)+PGEOH(JL,JK+1))*ZDMFDE(JL)
! find moist static energy that give nonbuoyant air
        zga = alv*pqsenh(jl,jk+1)/(rv*(ptenh(jl,jk+1)**2))
        zdt = (plu(jl,jk+1)-0.608*(pqsenh(jl,jk+1)-pqenh(jl, &
               jk+1)))/(1./ptenh(jl,jk+1)+0.608*zga)
        zscod = cpd*ptenh(jl,jk+1) + pgeoh(jl,jk+1) + cpd*zdt
        zscde = zscde + zodetr(jl,jk)*zscod
        zqude = pqu(jl,jk+1)*zdmfde(jl)
        zqcod = pqsenh(jl,jk+1) + zga*zdt
        zqude = zqude + zodetr(jl,jk)*zqcod
        plude(jl,jk) = plu(jl,jk+1)*zdmfde(jl)
        plude(jl,jk) = plude(jl,jk)+plu(jl,jk+1)*zodetr(jl,jk)
        zmfusk = pmfus(jl,jk+1) + zseen - zscde
        zmfuqk = pmfuq(jl,jk+1) + zqeen - zqude
        zmfulk = pmful(jl,jk+1) - plude(jl,jk)
        plu(jl,jk) = zmfulk*(1./MAX(cmfcmin,pmfu(jl,jk)))
        pqu(jl,jk) = zmfuqk*(1./MAX(cmfcmin,pmfu(jl,jk)))
        ptu(jl,jk)=(zmfusk*(1./MAX(cmfcmin,pmfu(jl,jk)))-  &
            pgeoh(jl,jk))*rcpd
        ptu(jl,jk) = MAX(100.,ptu(jl,jk))
        ptu(jl,jk) = MIN(400.,ptu(jl,jk))
        zqold(jl) = pqu(jl,jk)
      END IF
  420 CONTINUE
!*             DO CORRECTIONS FOR MOIST ASCENT
!*             BY ADJUSTING T,Q AND L IN *CUADJTQ*
!------------------------------------------------
      IK=JK
      ICALL=1
!
      CALL CUADJTQ(KLON,KLEV,IK,ZPH,PTU,PQU,LOFLAG,ICALL)
!
      DO 440 JL=1,KLON
      IF(LOFLAG(JL).AND.PQU(JL,JK).NE.ZQOLD(JL)) THEN
         KLAB(JL,JK)=2
         PLU(JL,JK)=PLU(JL,JK)+ZQOLD(JL)-PQU(JL,JK)
         ZBUO=PTU(JL,JK)*(1.+VTMPC1*PQU(JL,JK)-PLU(JL,JK))-  &
        PTENH(JL,JK)*(1.+VTMPC1*PQENH(JL,JK))
         IF(KLAB(JL,JK+1).EQ.1) ZBUO=ZBUO+ZBUO0
         IF(ZBUO.GT.0..AND.PMFU(JL,JK).GT.0.01*PMFUB(JL).AND. &
                            JK.GE.KCTOP0(JL)) THEN
            KCTOP(JL)=JK
            LDCUM(JL)=.TRUE.
            IF(ZPBASE(JL)-PAPH(JL,JK).GE.ZDNOPRC) THEN
               ZPRCON=CPRCON
            ELSE
               ZPRCON=0.
            ENDIF
            ZLNEW=PLU(JL,JK)/(1.+ZPRCON*(PGEOH(JL,JK)-PGEOH(JL,JK+1)))
            PDMFUP(JL,JK)=MAX(0.,(PLU(JL,JK)-ZLNEW)*PMFU(JL,JK))
            PLU(JL,JK)=ZLNEW
         ELSE
            KLAB(JL,JK)=0
            PMFU(JL,JK)=0.
         END IF
      END IF
      IF(LOFLAG(JL)) THEN
         PMFUL(JL,JK)=PLU(JL,JK)*PMFU(JL,JK)
         PMFUS(JL,JK)=(CPD*PTU(JL,JK)+PGEOH(JL,JK))*PMFU(JL,JK)
         PMFUQ(JL,JK)=PQU(JL,JK)*PMFU(JL,JK)
      END IF
  440 CONTINUE
!
      IF(LMFDUDV) THEN
!
        DO 460 JL=1,KLON
        zdmfen(jl) = zdmfen(jl) + zoentr(jl,jk)
        zdmfde(jl) = zdmfde(jl) + zodetr(jl,jk)
           IF(LOFLAG(JL)) THEN
              IF(KTYPE(JL).EQ.1.OR.KTYPE(JL).EQ.3) THEN
                 IF(ZDMFEN(JL).LE.1.E-20) THEN
                    ZZ=3.
                 ELSE
                    ZZ=2.
                 ENDIF
              ELSE
                 IF(ZDMFEN(JL).LE.1.0E-20) THEN
                    ZZ=1.
                 ELSE
                    ZZ=0.
                 ENDIF
              END IF
              ZDMFEU=ZDMFEN(JL)+ZZ*ZDMFDE(JL)
              ZDMFDU=ZDMFDE(JL)+ZZ*ZDMFDE(JL)
              ZDMFDU=MIN(ZDMFDU,0.75*PMFU(JL,JK+1))
              ZMFUU(JL)=ZMFUU(JL)+                              &
                       ZDMFEU*PUEN(JL,JK)-ZDMFDU*PUU(JL,JK+1)   
              ZMFUV(JL)=ZMFUV(JL)+                              &
                       ZDMFEU*PVEN(JL,JK)-ZDMFDU*PVU(JL,JK+1)   
              IF(PMFU(JL,JK).GT.0.) THEN
                 PUU(JL,JK)=ZMFUU(JL)*(1./PMFU(JL,JK))
                 PVU(JL,JK)=ZMFUV(JL)*(1./PMFU(JL,JK))
              END IF
           END IF
  460   CONTINUE
!
        END IF
!
! Compute organized entrainment
! for use at next level
!
      DO 470 jl = 1, klon
       IF (loflag(jl).AND.ktype(jl).EQ.1) THEN
! old scheme
       if(orgen .eq. 1 ) then
        zbuoyz=g*((ptu(jl,jk)-ptenh(jl,jk))/ptenh(jl,jk)+  &
              0.608*(pqu(jl,jk)-pqenh(jl,jk))-plu(jl,jk))
        zbuoyz = MAX(zbuoyz,0.0)
        zdz = (pgeo(jl,jk-1)-pgeo(jl,jk))*zrg
        zdrodz = -LOG(pten(jl,jk-1)/pten(jl,jk))/zdz -  &
                 g/(rd*ptenh(jl,jk))
        zbuoy(jl) = zbuoy(jl) + zbuoyz*zdz
        zoentr(jl,jk-1) = zbuoyz*0.5/(1.+zbuoy(jl))+zdrodz
        zoentr(jl,jk-1) = MIN(zoentr(jl,jk-1),1.E-3)
        zoentr(jl,jk-1) = MAX(zoentr(jl,jk-1),0.)
       else if(orgen .eq. 2 ) then
! Let's define the fscale
        tt(jl) = ptenh(jl,jk-1)
        zqsat(jl) = TLUCUA(tt(jl))/paph(jl,jk-1)
        zqsat(jl) = zqsat(jl)/(1.-VTMPC1*zqsat(jl))
        ttb(jl) = ptenh(jl,kcbot(jl))
        zqsatb(jl) = TLUCUA(ttb(jl))/paph(jl,kcbot(jl))
        zqsatb(jl) = zqsatb(jl)/(1.-VTMPC1*zqsatb(jl))
        fscale(jl) = (zqsat(jl)/zqsatb(jl))**3
! end of defining the fscale
        zoentr(jl,jk-1) = 1.E-3*(1.3-PQEN(jl,jk-1)/PQSEN(jl,jk-1))*fscale(jl)
        zoentr(jl,jk-1) = MIN(zoentr(jl,jk-1),1.E-3)
        zoentr(jl,jk-1) = MAX(zoentr(jl,jk-1),0.)
!        write(6,*) "zoentr=",zoentr(jl,jk-1) 
       end if
       END IF
  470 CONTINUE 
!
  480 CONTINUE
! -----------------------------------------------------------------
!     5.       DETERMINE CONVECTIVE FLUXES ABOVE NON-BUOYANCY LEVEL
! -----------------------------------------------------------------
!                  (NOTE: CLOUD VARIABLES LIKE T,Q AND L ARE NOT
!                         AFFECTED BY DETRAINMENT AND ARE ALREADY KNOWN
!                         FROM PREVIOUS CALCULATIONS ABOVE)
  500 CONTINUE
      DO 510 JL=1,KLON
      IF(KCTOP(JL).EQ.KLEVM1) LDCUM(JL)=.FALSE.
      KCBOT(JL)=MAX(KCBOT(JL),KCTOP(JL))
  510 CONTINUE
      IS=0
      DO 520 JL=1,KLON
      IF(LDCUM(JL)) THEN
         IS=IS+1
      ENDIF
  520 CONTINUE
      KCUM=IS
      IF(IS.EQ.0) GO TO 800
      DO 530 JL=1,KLON
      IF(LDCUM(JL)) THEN
         JK=KCTOP(JL)-1
         ZZDMF=CMFCTOP
         ZDMFDE(JL)=(1.-ZZDMF)*PMFU(JL,JK+1)
         PLUDE(JL,JK)=ZDMFDE(JL)*PLU(JL,JK+1)
         PMFU(JL,JK)=PMFU(JL,JK+1)-ZDMFDE(JL)
         PMFUS(JL,JK)=(CPD*PTU(JL,JK)+PGEOH(JL,JK))*PMFU(JL,JK)
         PMFUQ(JL,JK)=PQU(JL,JK)*PMFU(JL,JK)
         PMFUL(JL,JK)=PLU(JL,JK)*PMFU(JL,JK)
         PLUDE(JL,JK-1)=PMFUL(JL,JK)
         PDMFUP(JL,JK)=0.
      END IF
  530 CONTINUE
        IF(LMFDUDV) THEN
           DO 540 JL=1,KLON
           IF(LDCUM(JL)) THEN
              JK=KCTOP(JL)-1
              PUU(JL,JK)=PUU(JL,JK+1)
              PVU(JL,JK)=PVU(JL,JK+1)
           END IF
  540      CONTINUE
        END IF
  800 CONTINUE
      RETURN
      END SUBROUTINE CUASC_NEW
!

!**********************************************
!       SUBROUTINE CUDLFS
!********************************************** 
      SUBROUTINE CUDLFS &
         (KLON,     KLEV,     KLEVP1,   PTENH,    PQENH,  &
          PUEN,     PVEN,     PGEOH,    PAPH,     PTU,    &
          PQU,      PUU,      PVU,      LDCUM,    KCBOT,  &
          KCTOP,    PMFUB,    PRFL,     PTD,      PQD,    &
          PUD,      PVD,      PMFD,     PMFDS,    PMFDQ,  &
          PDMFDP,   KDTOP,    LDDRAF)
!      THIS ROUTINE CALCULATES LEVEL OF FREE SINKING FOR
!      CUMULUS DOWNDRAFTS AND SPECIFIES T,Q,U AND V VALUES
!      M.TIEDTKE         E.C.M.W.F.    12/86 MODIF.  12/89
!***PURPOSE.
!   --------
!          TO PRODUCE LFS-VALUES FOR CUMULUS DOWNDRAFTS
!          FOR MASSFLUX CUMULUS PARAMETERIZATION
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!          INPUT ARE ENVIRONMENTAL VALUES OF T,Q,U,V,P,PHI
!          AND UPDRAFT VALUES T,Q,U AND V AND ALSO
!          CLOUD BASE MASSFLUX AND CU-PRECIPITATION RATE.
!          IT RETURNS T,Q,U AND V VALUES AND MASSFLUX AT LFS.
!***METHOD.
!  --------
!          CHECK FOR NEGATIVE BUOYANCY OF AIR OF EQUAL PARTS OF
!          MOIST ENVIRONMENTAL AIR AND CLOUD AIR.
!***EXTERNALS
!   ---------
!          *CUADJTQ* FOR CALCULATING WET BULB T AND Q AT LFS
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   JL,KE,JK,IS,IK,ICALL
      REAL      ZTTEST, ZQTEST, ZBUO, ZMFTOP
      REAL     PTENH(KLON,KLEV),       PQENH(KLON,KLEV),   &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),     &
              PGEOH(KLON,KLEV),       PAPH(KLON,KLEVP1),   &
              PTU(KLON,KLEV),         PQU(KLON,KLEV),      &
              PUU(KLON,KLEV),         PVU(KLON,KLEV),      &
              PMFUB(KLON),            PRFL(KLON)
      REAL     PTD(KLON,KLEV),         PQD(KLON,KLEV),     &
              PUD(KLON,KLEV),         PVD(KLON,KLEV),      &
              PMFD(KLON,KLEV),        PMFDS(KLON,KLEV),    &
              PMFDQ(KLON,KLEV),       PDMFDP(KLON,KLEV)    
      REAL     ZTENWB(KLON,KLEV),      ZQENWB(KLON,KLEV),  &
              ZCOND(KLON),            ZPH(KLON)
      INTEGER  KCBOT(KLON),            KCTOP(KLON),        &
              KDTOP(KLON)
      LOGICAL  LDCUM(KLON),            LLo2(KLON),         &
              LDDRAF(KLON)
!-----------------------------------------------
!     1.       SET DEFAULT VALUES FOR DOWNDRAFTS
!-----------------------------------------------
  100 CONTINUE
      DO 110 JL=1,KLON
      LDDRAF(JL)=.FALSE.
      KDTOP(JL)=KLEVP1
  110 CONTINUE
      IF(.NOT.LMFDD) GO TO 300
!------------------------------------------------------------
!     2.       DETERMINE LEVEL OF FREE SINKING BY
!              DOING A SCAN FROM TOP TO BASE OF CUMULUS CLOUDS
!              FOR EVERY POINT AND PROCEED AS FOLLOWS:
!                (1) DETEMINE WET BULB ENVIRONMENTAL T AND Q
!                (2) DO MIXING WITH CUMULUS CLOUD AIR
!                (3) CHECK FOR NEGATIVE BUOYANCY
!              THE ASSUMPTION IS THAT AIR OF DOWNDRAFTS IS MIXTURE
!              OF 50% CLOUD AIR + 50% ENVIRONMENTAL AIR AT WET BULB
!              TEMPERATURE (I.E. WHICH BECAME SATURATED DUE TO
!              EVAPORATION OF RAIN AND CLOUD WATER)
!------------------------------------------------------------------
  200 CONTINUE
      KE=KLEV-3
      DO 290 JK=3,KE
!   2.1      CALCULATE WET-BULB TEMPERATURE AND MOISTURE
!            FOR ENVIRONMENTAL AIR IN *CUADJTQ*
! -----------------------------------------------------
  210 CONTINUE
      IS=0
      DO 212 JL=1,KLON
      ZTENWB(JL,JK)=PTENH(JL,JK)
      ZQENWB(JL,JK)=PQENH(JL,JK)
      ZPH(JL)=PAPH(JL,JK)
      LLO2(JL)=LDCUM(JL).AND.PRFL(JL).GT.0..AND..NOT.LDDRAF(JL).AND. &
              (JK.LT.KCBOT(JL).AND.JK.GT.KCTOP(JL))
      IF(LLO2(JL))THEN
         IS=IS+1
      ENDIF
  212 CONTINUE
      IF(IS.EQ.0) GO TO 290
      IK=JK
      ICALL=2
      CALL CUADJTQ(KLON,KLEV,IK,ZPH,ZTENWB,ZQENWB,LLO2,ICALL)
!   2.2      DO MIXING OF CUMULUS AND ENVIRONMENTAL AIR
!            AND CHECK FOR NEGATIVE BUOYANCY.
!            THEN SET VALUES FOR DOWNDRAFT AT LFS.
! -----------------------------------------------------
  220 CONTINUE
      DO 222 JL=1,KLON
      IF(LLO2(JL)) THEN
         ZTTEST=0.5*(PTU(JL,JK)+ZTENWB(JL,JK))
         ZQTEST=0.5*(PQU(JL,JK)+ZQENWB(JL,JK))
         ZBUO=ZTTEST*(1.+VTMPC1*ZQTEST)-  &
             PTENH(JL,JK)*(1.+VTMPC1*PQENH(JL,JK))
         ZCOND(JL)=PQENH(JL,JK)-ZQENWB(JL,JK)
         ZMFTOP=-CMFDEPS*PMFUB(JL)
         IF(ZBUO.LT.0..AND.PRFL(JL).GT.10.*ZMFTOP*ZCOND(JL)) THEN
            KDTOP(JL)=JK
            LDDRAF(JL)=.TRUE.
            PTD(JL,JK)=ZTTEST
            PQD(JL,JK)=ZQTEST
            PMFD(JL,JK)=ZMFTOP
            PMFDS(JL,JK)=PMFD(JL,JK)*(CPD*PTD(JL,JK)+PGEOH(JL,JK))
            PMFDQ(JL,JK)=PMFD(JL,JK)*PQD(JL,JK)
            PDMFDP(JL,JK-1)=-0.5*PMFD(JL,JK)*ZCOND(JL)
            PRFL(JL)=PRFL(JL)+PDMFDP(JL,JK-1)
         END IF
      END IF
  222 CONTINUE
         IF(LMFDUDV) THEN
            DO 224 JL=1,KLON
            IF(PMFD(JL,JK).LT.0.) THEN
               PUD(JL,JK)=0.5*(PUU(JL,JK)+PUEN(JL,JK-1))
               PVD(JL,JK)=0.5*(PVU(JL,JK)+PVEN(JL,JK-1))
            END IF
  224       CONTINUE
         END IF
  290 CONTINUE
 300  CONTINUE
      RETURN
      END SUBROUTINE CUDLFS
!

!**********************************************
!       SUBROUTINE CUDDRAF
!********************************************** 
      SUBROUTINE CUDDRAF &
         (KLON,     KLEV,     KLEVP1,   PTENH,    PQENH, &
          PUEN,     PVEN,     PGEOH,    PAPH,     PRFL,  &
          LDDRAF,   PTD,      PQD,      PUD,      PVD,   &
          PMFD,     PMFDS,    PMFDQ,    PDMFDP)
!     THIS ROUTINE CALCULATES CUMULUS DOWNDRAFT DESCENT
!     M.TIEDTKE         E.C.M.W.F.    12/86 MODIF.  12/89
!***PURPOSE.
!   --------
!          TO PRODUCE THE VERTICAL PROFILES FOR CUMULUS DOWNDRAFTS
!          (I.E. T,Q,U AND V AND FLUXES)
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!          INPUT IS T,Q,P,PHI,U,V AT HALF LEVELS.
!          IT RETURNS FLUXES OF S,Q AND EVAPORATION RATE
!          AND U,V AT LEVELS WHERE DOWNDRAFT OCCURS
!***METHOD.
!  --------
!          CALCULATE MOIST DESCENT FOR ENTRAINING/DETRAINING PLUME BY
!          A) MOVING AIR DRY-ADIABATICALLY TO NEXT LEVEL BELOW AND
!          B) CORRECTING FOR EVAPORATION TO OBTAIN SATURATED STATE.
!***EXTERNALS
!   ---------
!          *CUADJTQ* FOR ADJUSTING T AND Q DUE TO EVAPORATION IN
!          SATURATED DESCENT
!***REFERENCE
!   ---------
!          (TIEDTKE,1989)
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   JK,IS,JL,ITOPDE, IK, ICALL
      REAL      ZENTR,ZSEEN, ZQEEN, ZSDDE, ZQDDE,ZMFDSK, ZMFDQK
      REAL      ZBUO, ZDMFDP, ZMFDUK, ZMFDVK
      REAL     PTENH(KLON,KLEV),       PQENH(KLON,KLEV),  &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),    &
              PGEOH(KLON,KLEV),       PAPH(KLON,KLEVP1) 
      REAL     PTD(KLON,KLEV),         PQD(KLON,KLEV),    &
              PUD(KLON,KLEV),         PVD(KLON,KLEV),     &
              PMFD(KLON,KLEV),        PMFDS(KLON,KLEV),   &
              PMFDQ(KLON,KLEV),       PDMFDP(KLON,KLEV),  &
              PRFL(KLON)
      REAL     ZDMFEN(KLON),           ZDMFDE(KLON),      &
              ZCOND(KLON),            ZPH(KLON)       
      LOGICAL  LDDRAF(KLON),           LLO2(KLON)
!--------------------------------------------------------------
!     1.       CALCULATE MOIST DESCENT FOR CUMULUS DOWNDRAFT BY
!                (A) CALCULATING ENTRAINMENT RATES, ASSUMING
!                     LINEAR DECREASE OF MASSFLUX IN PBL
!                 (B) DOING MOIST DESCENT - EVAPORATIVE COOLING
!                     AND MOISTENING IS CALCULATED IN *CUADJTQ*
!                 (C) CHECKING FOR NEGATIVE BUOYANCY AND
!                     SPECIFYING FINAL T,Q,U,V AND DOWNWARD FLUXES
! ----------------------------------------------------------------
  100 CONTINUE
      DO 180 JK=3,KLEV
      IS=0
      DO 110 JL=1,KLON
      ZPH(JL)=PAPH(JL,JK)
      LLO2(JL)=LDDRAF(JL).AND.PMFD(JL,JK-1).LT.0.
      IF(LLO2(JL)) THEN
         IS=IS+1
      ENDIF
  110 CONTINUE
      IF(IS.EQ.0) GO TO 180
      DO 122 JL=1,KLON
      IF(LLO2(JL)) THEN
         ZENTR=ENTRDD*PMFD(JL,JK-1)*RD*PTENH(JL,JK-1)/   &
              (G*PAPH(JL,JK-1))*(PAPH(JL,JK)-PAPH(JL,JK-1))
         ZDMFEN(JL)=ZENTR
         ZDMFDE(JL)=ZENTR
      END IF
  122 CONTINUE
      ITOPDE=KLEV-2
         IF(JK.GT.ITOPDE) THEN
            DO 124 JL=1,KLON
            IF(LLO2(JL)) THEN
               ZDMFEN(JL)=0.
               ZDMFDE(JL)=PMFD(JL,ITOPDE)*      &
              (PAPH(JL,JK)-PAPH(JL,JK-1))/     &
              (PAPH(JL,KLEVP1)-PAPH(JL,ITOPDE))
            END IF
  124       CONTINUE
         END IF
      DO 126 JL=1,KLON
         IF(LLO2(JL)) THEN
            PMFD(JL,JK)=PMFD(JL,JK-1)+ZDMFEN(JL)-ZDMFDE(JL)
            ZSEEN=(CPD*PTENH(JL,JK-1)+PGEOH(JL,JK-1))*ZDMFEN(JL)
            ZQEEN=PQENH(JL,JK-1)*ZDMFEN(JL)
            ZSDDE=(CPD*PTD(JL,JK-1)+PGEOH(JL,JK-1))*ZDMFDE(JL)
            ZQDDE=PQD(JL,JK-1)*ZDMFDE(JL)
            ZMFDSK=PMFDS(JL,JK-1)+ZSEEN-ZSDDE
            ZMFDQK=PMFDQ(JL,JK-1)+ZQEEN-ZQDDE
            PQD(JL,JK)=ZMFDQK*(1./MIN(-CMFCMIN,PMFD(JL,JK)))
            PTD(JL,JK)=(ZMFDSK*(1./MIN(-CMFCMIN,PMFD(JL,JK)))- &
                       PGEOH(JL,JK))*RCPD
            PTD(JL,JK)=MIN(400.,PTD(JL,JK))
            PTD(JL,JK)=MAX(100.,PTD(JL,JK))
            ZCOND(JL)=PQD(JL,JK)
         END IF
  126 CONTINUE
      IK=JK
      ICALL=2
      CALL CUADJTQ(KLON,KLEV,IK,ZPH,PTD,PQD,LLO2,ICALL)
      DO 150 JL=1,KLON
         IF(LLO2(JL)) THEN
            ZCOND(JL)=ZCOND(JL)-PQD(JL,JK)
            ZBUO=PTD(JL,JK)*(1.+VTMPC1*PQD(JL,JK))- &
           PTENH(JL,JK)*(1.+VTMPC1*PQENH(JL,JK))
            IF(ZBUO.GE.0..OR.PRFL(JL).LE.(PMFD(JL,JK)*ZCOND(JL))) THEN
               PMFD(JL,JK)=0.
            ENDIF
            PMFDS(JL,JK)=(CPD*PTD(JL,JK)+PGEOH(JL,JK))*PMFD(JL,JK)
            PMFDQ(JL,JK)=PQD(JL,JK)*PMFD(JL,JK)
            ZDMFDP=-PMFD(JL,JK)*ZCOND(JL)
            PDMFDP(JL,JK-1)=ZDMFDP
            PRFL(JL)=PRFL(JL)+ZDMFDP
         END IF
  150 CONTINUE
        IF(LMFDUDV) THEN
          DO 160 JL=1,KLON
             IF(LLO2(JL).AND.PMFD(JL,JK).LT.0.) THEN
                ZMFDUK=PMFD(JL,JK-1)*PUD(JL,JK-1)+   &
               ZDMFEN(JL)*PUEN(JL,JK-1)-ZDMFDE(JL)*PUD(JL,JK-1)
                ZMFDVK=PMFD(JL,JK-1)*PVD(JL,JK-1)+   &
               ZDMFEN(JL)*PVEN(JL,JK-1)-ZDMFDE(JL)*PVD(JL,JK-1)
                PUD(JL,JK)=ZMFDUK*(1./MIN(-CMFCMIN,PMFD(JL,JK)))
                PVD(JL,JK)=ZMFDVK*(1./MIN(-CMFCMIN,PMFD(JL,JK)))
             END IF
  160     CONTINUE
        END IF
  180 CONTINUE
      RETURN
      END SUBROUTINE CUDDRAF
!

!**********************************************
!       SUBROUTINE CUFLX
!********************************************** 
      SUBROUTINE CUFLX &
         (KLON,     KLEV,     KLEVP1,   PQEN,    PQSEN,     &
          PTENH,    PQENH,    PAPH,     PGEOH,   KCBOT,    &
          KCTOP,    KDTOP,    KTYPE,    LDDRAF,  LDCUM,  &
          PMFU,     PMFD,     PMFUS,    PMFDS,   PMFUQ,  &
          PMFDQ,    PMFUL,    PLUDE,    PDMFUP,  PDMFDP, &
          PRFL,     PRAIN,    PTEN,     PSFL,    PDPMEL, &
          KTOPM2,   ZTMST,    sig1)
!      M.TIEDTKE         E.C.M.W.F.     7/86 MODIF.  12/89
!***PURPOSE
!   -------
!          THIS ROUTINE DOES THE FINAL CALCULATION OF CONVECTIVE
!          FLUXES IN THE CLOUD LAYER AND IN THE SUBCLOUD LAYER
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUMASTR*.
!***EXTERNALS
!   ---------
!          NONE
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KTOPM2, ITOP, JL, JK, IKB
      REAL      ZTMST, ZCONS1, ZCONS2, ZCUCOV, ZTMELP2
      REAL      ZZP, ZFAC, ZSNMLT, ZRFL, CEVAPCU, ZRNEW
      REAL      ZRMIN, ZRFLN, ZDRFL, ZDPEVAP
      REAL     PQEN(KLON,KLEV),        PQSEN(KLON,KLEV),  &
              PTENH(KLON,KLEV),       PQENH(KLON,KLEV),   &
              PAPH(KLON,KLEVP1),      PGEOH(KLON,KLEV)    
      REAL     PMFU(KLON,KLEV),        PMFD(KLON,KLEV),   &
              PMFUS(KLON,KLEV),       PMFDS(KLON,KLEV),   &
              PMFUQ(KLON,KLEV),       PMFDQ(KLON,KLEV),   &
              PDMFUP(KLON,KLEV),      PDMFDP(KLON,KLEV),  &
              PMFUL(KLON,KLEV),       PLUDE(KLON,KLEV),   &
              PRFL(KLON),             PRAIN(KLON)
      REAL     PTEN(KLON,KLEV),        PDPMEL(KLON,KLEV), &
              PSFL(KLON),             ZPSUBCL(KLON)
      REAL     sig1(KLEV)
      INTEGER  KCBOT(KLON),            KCTOP(KLON),     &
              KDTOP(KLON),            KTYPE(KLON)
      LOGICAL  LDDRAF(KLON),           LDCUM(KLON)
!*       SPECIFY CONSTANTS
      ZCONS1=CPD/(ALF*G*ZTMST)
      ZCONS2=1./(G*ZTMST)
      ZCUCOV=0.05
      ZTMELP2=TMELT+2.
!*  1.0      DETERMINE FINAL CONVECTIVE FLUXES
!---------------------------------------------
  100 CONTINUE
      ITOP=KLEV
      DO 110 JL=1,KLON
      PRFL(JL)=0.
      PSFL(JL)=0.
      PRAIN(JL)=0.
!     SWITCH OFF SHALLOW CONVECTION
      IF(.NOT.LMFSCV.AND.KTYPE(JL).EQ.2)THEN
        LDCUM(JL)=.FALSE.
        LDDRAF(JL)=.FALSE.
      ENDIF
      ITOP=MIN(ITOP,KCTOP(JL))
      IF(.NOT.LDCUM(JL).OR.KDTOP(JL).LT.KCTOP(JL)) LDDRAF(JL)=.FALSE.
      IF(.NOT.LDCUM(JL)) KTYPE(JL)=0
  110 CONTINUE
      KTOPM2=ITOP-2
      DO 120 JK=KTOPM2,KLEV
      DO 115 JL=1,KLON
      IF(LDCUM(JL).AND.JK.GE.KCTOP(JL)-1) THEN
         PMFUS(JL,JK)=PMFUS(JL,JK)-PMFU(JL,JK)*  &
                     (CPD*PTENH(JL,JK)+PGEOH(JL,JK))
         PMFUQ(JL,JK)=PMFUQ(JL,JK)-PMFU(JL,JK)*PQENH(JL,JK)
         IF(LDDRAF(JL).AND.JK.GE.KDTOP(JL)) THEN
            PMFDS(JL,JK)=PMFDS(JL,JK)-PMFD(JL,JK)*  &
                        (CPD*PTENH(JL,JK)+PGEOH(JL,JK))
            PMFDQ(JL,JK)=PMFDQ(JL,JK)-PMFD(JL,JK)*PQENH(JL,JK)
         ELSE
            PMFD(JL,JK)=0.
            PMFDS(JL,JK)=0.
            PMFDQ(JL,JK)=0.
            PDMFDP(JL,JK-1)=0.
         END IF
      ELSE
         PMFU(JL,JK)=0.
         PMFD(JL,JK)=0.
         PMFUS(JL,JK)=0.
         PMFDS(JL,JK)=0.
         PMFUQ(JL,JK)=0.
         PMFDQ(JL,JK)=0.
         PMFUL(JL,JK)=0.
         PDMFUP(JL,JK-1)=0.
         PDMFDP(JL,JK-1)=0.
         PLUDE(JL,JK-1)=0.
      END IF
  115 CONTINUE
  120 CONTINUE
      DO 130 JK=KTOPM2,KLEV
      DO 125 JL=1,KLON
      IF(LDCUM(JL).AND.JK.GT.KCBOT(JL)) THEN
         IKB=KCBOT(JL)
         ZZP=((PAPH(JL,KLEVP1)-PAPH(JL,JK))/  &
             (PAPH(JL,KLEVP1)-PAPH(JL,IKB)))
         IF(KTYPE(JL).EQ.3) THEN
            ZZP=ZZP**2
         ENDIF
         PMFU(JL,JK)=PMFU(JL,IKB)*ZZP
         PMFUS(JL,JK)=PMFUS(JL,IKB)*ZZP
         PMFUQ(JL,JK)=PMFUQ(JL,IKB)*ZZP
         PMFUL(JL,JK)=PMFUL(JL,IKB)*ZZP
      END IF
!*    2.        CALCULATE RAIN/SNOW FALL RATES
!*              CALCULATE MELTING OF SNOW
!*              CALCULATE EVAPORATION OF PRECIP
!----------------------------------------------
      IF(LDCUM(JL)) THEN
         PRAIN(JL)=PRAIN(JL)+PDMFUP(JL,JK)
         IF(PTEN(JL,JK).GT.TMELT) THEN
            PRFL(JL)=PRFL(JL)+PDMFUP(JL,JK)+PDMFDP(JL,JK)
            IF(PSFL(JL).GT.0..AND.PTEN(JL,JK).GT.ZTMELP2) THEN
               ZFAC=ZCONS1*(PAPH(JL,JK+1)-PAPH(JL,JK))
               ZSNMLT=MIN(PSFL(JL),ZFAC*(PTEN(JL,JK)-ZTMELP2))
               PDPMEL(JL,JK)=ZSNMLT
               PSFL(JL)=PSFL(JL)-ZSNMLT
               PRFL(JL)=PRFL(JL)+ZSNMLT
            END IF
         ELSE
            PSFL(JL)=PSFL(JL)+PDMFUP(JL,JK)+PDMFDP(JL,JK)
         END IF
      END IF
  125 CONTINUE
  130 CONTINUE
      DO 230 JL=1,KLON
        PRFL(JL)=MAX(PRFL(JL),0.)
        PSFL(JL)=MAX(PSFL(JL),0.)
        ZPSUBCL(JL)=PRFL(JL)+PSFL(JL)
  230 CONTINUE
      DO 240 JK=KTOPM2,KLEV
      DO 235 JL=1,KLON
      IF(LDCUM(JL).AND.JK.GE.KCBOT(JL).AND. &
             ZPSUBCL(JL).GT.1.E-20) THEN
          ZRFL=ZPSUBCL(JL)
          CEVAPCU=CEVAPCU1*SQRT(CEVAPCU2*SQRT(sig1(JK)))
          ZRNEW=(MAX(0.,SQRT(ZRFL/ZCUCOV)-   &
                  CEVAPCU*(PAPH(JL,JK+1)-PAPH(JL,JK))* &
                MAX(0.,PQSEN(JL,JK)-PQEN(JL,JK))))**2*ZCUCOV
          ZRMIN=ZRFL-ZCUCOV*MAX(0.,0.8*PQSEN(JL,JK)-PQEN(JL,JK)) &
               *ZCONS2*(PAPH(JL,JK+1)-PAPH(JL,JK))
          ZRNEW=MAX(ZRNEW,ZRMIN)
          ZRFLN=MAX(ZRNEW,0.)
          ZDRFL=MIN(0.,ZRFLN-ZRFL)
          PDMFUP(JL,JK)=PDMFUP(JL,JK)+ZDRFL
          ZPSUBCL(JL)=ZRFLN
      END IF
  235 CONTINUE
  240 CONTINUE
      DO 250 JL=1,KLON
        ZDPEVAP=ZPSUBCL(JL)-(PRFL(JL)+PSFL(JL))
        PRFL(JL)=PRFL(JL)+ZDPEVAP*PRFL(JL)*  &
                  (1./MAX(1.E-20,PRFL(JL)+PSFL(JL)))
        PSFL(JL)=PSFL(JL)+ZDPEVAP*PSFL(JL)*  &
                  (1./MAX(1.E-20,PRFL(JL)+PSFL(JL)))
  250 CONTINUE
      RETURN
      END SUBROUTINE CUFLX
!

!**********************************************
!       SUBROUTINE CUDTDQ
!********************************************** 
      SUBROUTINE CUDTDQ &
         (KLON,     KLEV,     KLEVP1,   KTOPM2,   PAPH,   &
          LDCUM,    PTEN,     PTTE,     PQTE,     PMFUS,  &
          PMFDS,    PMFUQ,    PMFDQ,    PMFUL,    PDMFUP, &
          PDMFDP,   ZTMST,    PDPMEL,   PRAIN,    PRFL,   &
          PSFL,     PSRAIN,   PSEVAP,   PSHEAT,   PSMELT, &
          PRSFC,    PSSFC,    PAPRC,    PAPRSM,   PAPRS,  &
          PQEN,     PQSEN,    PLUDE,    PCTE)
!**** *CUDTDQ* - UPDATES T AND Q TENDENCIES, PRECIPITATION RATES
!                DOES GLOBAL DIAGNOSTICS
!      M.TIEDTKE         E.C.M.W.F.     7/86 MODIF.  12/89
!***INTERFACE.
!   ----------
!          *CUDTDQ* IS CALLED FROM *CUMASTR*
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KTOPM2,JL, JK
      REAL      ZTMST, PSRAIN, PSEVAP, PSHEAT, PSMELT, ZDIAGT, ZDIAGW
      REAL      ZALV, RHK, RHCOE, PLDFD, ZDTDT, ZDQDT
      REAL     PTTE(KLON,KLEV),        PQTE(KLON,KLEV),  &
              PTEN(KLON,KLEV),        PLUDE(KLON,KLEV),  &
              PGEO(KLON,KLEV),        PAPH(KLON,KLEVP1), &
              PAPRC(KLON),            PAPRS(KLON),       &
              PAPRSM(KLON),           PCTE(KLON,KLEV),   &
              PRSFC(KLON),            PSSFC(KLON)
      REAL     PMFUS(KLON,KLEV),       PMFDS(KLON,KLEV), &
              PMFUQ(KLON,KLEV),       PMFDQ(KLON,KLEV), &
              PMFUL(KLON,KLEV),       PQSEN(KLON,KLEV), &
              PDMFUP(KLON,KLEV),      PDMFDP(KLON,KLEV),& 
              PRFL(KLON),             PRAIN(KLON),      &
              PQEN(KLON,KLEV)
      REAL     PDPMEL(KLON,KLEV),      PSFL(KLON)
      REAL     ZSHEAT(KLON),           ZMELT(KLON)
      LOGICAL  LDCUM(KLON)
!--------------------------------
!*    1.0      SPECIFY PARAMETERS
!--------------------------------
  100 CONTINUE
      ZDIAGT=ZTMST
      ZDIAGW=ZDIAGT/RHOH2O
!--------------------------------------------------
!*    2.0      INCREMENTATION OF T AND Q TENDENCIES
!--------------------------------------------------
  200 CONTINUE
      DO 210 JL=1,KLON
      ZMELT(JL)=0.
      ZSHEAT(JL)=0.
  210 CONTINUE
      DO 250 JK=KTOPM2,KLEV
      IF(JK.LT.KLEV) THEN
         DO 220 JL=1,KLON
         IF(LDCUM(JL)) THEN
            IF(PTEN(JL,JK).GT.TMELT) THEN
               ZALV=ALV
            ELSE
               ZALV=ALS
            ENDIF
            RHK=MIN(1.0,PQEN(JL,JK)/PQSEN(JL,JK))
            RHCOE=MAX(0.0,(RHK-RHC)/(RHM-RHC))
            pldfd=MAX(0.0,RHCOE*fdbk*PLUDE(JL,JK))
            ZDTDT=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*RCPD*      &
              (PMFUS(JL,JK+1)-PMFUS(JL,JK)+                  &
              PMFDS(JL,JK+1)-PMFDS(JL,JK)-ALF*PDPMEL(JL,JK)  &
              -ZALV*(PMFUL(JL,JK+1)-PMFUL(JL,JK)-pldfd-      &
              (PDMFUP(JL,JK)+PDMFDP(JL,JK))))
            PTTE(JL,JK)=PTTE(JL,JK)+ZDTDT
            ZDQDT=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*& 
              (PMFUQ(JL,JK+1)-PMFUQ(JL,JK)+       &
              PMFDQ(JL,JK+1)-PMFDQ(JL,JK)+        &
              PMFUL(JL,JK+1)-PMFUL(JL,JK)-pldfd-  &
              (PDMFUP(JL,JK)+PDMFDP(JL,JK)))
            PQTE(JL,JK)=PQTE(JL,JK)+ZDQDT
            PCTE(JL,JK)=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*pldfd
            ZSHEAT(JL)=ZSHEAT(JL)+ZALV*(PDMFUP(JL,JK)+PDMFDP(JL,JK))
            ZMELT(JL)=ZMELT(JL)+PDPMEL(JL,JK)
         END IF
  220 CONTINUE
      ELSE
         DO 230 JL=1,KLON
         IF(LDCUM(JL)) THEN
            IF(PTEN(JL,JK).GT.TMELT) THEN
               ZALV=ALV
            ELSE
               ZALV=ALS
            ENDIF
            RHK=MIN(1.0,PQEN(JL,JK)/PQSEN(JL,JK))
            RHCOE=MAX(0.0,(RHK-RHC)/(RHM-RHC))
            pldfd=MAX(0.0,RHCOE*fdbk*PLUDE(JL,JK))
            ZDTDT=-(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*RCPD*           &
                (PMFUS(JL,JK)+PMFDS(JL,JK)+ALF*PDPMEL(JL,JK)-ZALV* &
                (PMFUL(JL,JK)+PDMFUP(JL,JK)+PDMFDP(JL,JK)+pldfd))  
            PTTE(JL,JK)=PTTE(JL,JK)+ZDTDT
            ZDQDT=-(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*                &
                     (PMFUQ(JL,JK)+PMFDQ(JL,JK)+pldfd+             &
                     (PMFUL(JL,JK)+PDMFUP(JL,JK)+PDMFDP(JL,JK)))   
            PQTE(JL,JK)=PQTE(JL,JK)+ZDQDT
            PCTE(JL,JK)=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))*pldfd
            ZSHEAT(JL)=ZSHEAT(JL)+ZALV*(PDMFUP(JL,JK)+PDMFDP(JL,JK))
            ZMELT(JL)=ZMELT(JL)+PDPMEL(JL,JK)
         END IF
  230    CONTINUE
      END IF
  250 CONTINUE
!---------------------------------------------------------
!      3.      UPDATE SURFACE FIELDS AND DO GLOBAL BUDGETS
!---------------------------------------------------------
  300 CONTINUE
      DO 310 JL=1,KLON
      PRSFC(JL)=PRFL(JL)
      PSSFC(JL)=PSFL(JL)
      PAPRC(JL)=PAPRC(JL)+ZDIAGW*(PRFL(JL)+PSFL(JL))
      PAPRS(JL)=PAPRSM(JL)+ZDIAGW*PSFL(JL)
      PSHEAT=PSHEAT+ZSHEAT(JL)
      PSRAIN=PSRAIN+PRAIN(JL)
      PSEVAP=PSEVAP-(PRFL(JL)+PSFL(JL))
      PSMELT=PSMELT+ZMELT(JL)
  310 CONTINUE
      PSEVAP=PSEVAP+PSRAIN
      RETURN
      END SUBROUTINE CUDTDQ

!
!**********************************************
!       SUBROUTINE CUDUDV
!********************************************** 
      SUBROUTINE CUDUDV &
         (KLON,     KLEV,     KLEVP1,   KTOPM2,   KTYPE,  &
          KCBOT,    PAPH,     LDCUM,    PUEN,     PVEN,   &
          PVOM,     PVOL,     PUU,      PUD,      PVU,    &
          PVD,      PMFU,     PMFD,     PSDISS)
!**** *CUDUDV* - UPDATES U AND V TENDENCIES,
!                DOES GLOBAL DIAGNOSTIC OF DISSIPATION
!      M.TIEDTKE         E.C.M.W.F.     7/86 MODIF.  12/89
!***INTERFACE.
!   ----------
!          *CUDUDV* IS CALLED FROM *CUMASTR*
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KTOPM2, JK, IK, JL, IKB
      REAL      PSDISS,ZZP, ZDUDT ,ZDVDT, ZSUM
      REAL     PUEN(KLON,KLEV),        PVEN(KLON,KLEV),   &
              PVOL(KLON,KLEV),        PVOM(KLON,KLEV),    &
              PAPH(KLON,KLEVP1)
      REAL     PUU(KLON,KLEV),         PUD(KLON,KLEV),    &
              PVU(KLON,KLEV),         PVD(KLON,KLEV),     &
              PMFU(KLON,KLEV),        PMFD(KLON,KLEV)
      REAL     ZMFUU(KLON,KLEV),       ZMFDU(KLON,KLEV),  &
              ZMFUV(KLON,KLEV),       ZMFDV(KLON,KLEV),   &
              ZDISS(KLON)
      INTEGER  KTYPE(KLON),            KCBOT(KLON)
      LOGICAL  LDCUM(KLON)
!------------------------------------------------------------
!*    1.0      CALCULATE FLUXES AND UPDATE U AND V TENDENCIES
! -----------------------------------------------------------
  100 CONTINUE
      DO 120 JK=KTOPM2,KLEV
      IK=JK-1
      DO 110 JL=1,KLON
      IF(LDCUM(JL)) THEN
        ZMFUU(JL,JK)=PMFU(JL,JK)*(PUU(JL,JK)-PUEN(JL,IK))
        ZMFUV(JL,JK)=PMFU(JL,JK)*(PVU(JL,JK)-PVEN(JL,IK))
        ZMFDU(JL,JK)=PMFD(JL,JK)*(PUD(JL,JK)-PUEN(JL,IK))
        ZMFDV(JL,JK)=PMFD(JL,JK)*(PVD(JL,JK)-PVEN(JL,IK))
      END IF
  110 CONTINUE
  120 CONTINUE
      DO 140 JK=KTOPM2,KLEV
      DO 130 JL=1,KLON
      IF(LDCUM(JL).AND.JK.GT.KCBOT(JL)) THEN
         IKB=KCBOT(JL)
         ZZP=((PAPH(JL,KLEVP1)-PAPH(JL,JK))/  &
             (PAPH(JL,KLEVP1)-PAPH(JL,IKB)))
         IF(KTYPE(JL).EQ.3) THEN
            ZZP=ZZP**2
         ENDIF
         ZMFUU(JL,JK)=ZMFUU(JL,IKB)*ZZP
         ZMFUV(JL,JK)=ZMFUV(JL,IKB)*ZZP
         ZMFDU(JL,JK)=ZMFDU(JL,IKB)*ZZP
         ZMFDV(JL,JK)=ZMFDV(JL,IKB)*ZZP
      END IF
  130 CONTINUE
  140 CONTINUE
      DO 150 JL=1,KLON
      ZDISS(JL)=0.
  150 CONTINUE
      DO 190 JK=KTOPM2,KLEV
      IF(JK.LT.KLEV) THEN
         DO 160 JL=1,KLON
            IF(LDCUM(JL)) THEN
               ZDUDT=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))* &
                    (ZMFUU(JL,JK+1)-ZMFUU(JL,JK)+     &
                     ZMFDU(JL,JK+1)-ZMFDU(JL,JK))
               ZDVDT=(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))* &
                    (ZMFUV(JL,JK+1)-ZMFUV(JL,JK)+     &
                     ZMFDV(JL,JK+1)-ZMFDV(JL,JK))
               ZDISS(JL)=ZDISS(JL)+        &
                        PUEN(JL,JK)*(ZMFUU(JL,JK+1)-ZMFUU(JL,JK)+   &
                                     ZMFDU(JL,JK+1)-ZMFDU(JL,JK))+  &
                        PVEN(JL,JK)*(ZMFUV(JL,JK+1)-ZMFUV(JL,JK)+   &
                                     ZMFDV(JL,JK+1)-ZMFDV(JL,JK))
               PVOM(JL,JK)=PVOM(JL,JK)+ZDUDT
               PVOL(JL,JK)=PVOL(JL,JK)+ZDVDT
            END IF
  160    CONTINUE
      ELSE
         DO 170 JL=1,KLON
            IF(LDCUM(JL)) THEN
               ZDUDT=-(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))* &
                        (ZMFUU(JL,JK)+ZMFDU(JL,JK))
               ZDVDT=-(G/(PAPH(JL,JK+1)-PAPH(JL,JK)))* &
                        (ZMFUV(JL,JK)+ZMFDV(JL,JK))
               ZDISS(JL)=ZDISS(JL)-        &
      (PUEN(JL,JK)*(ZMFUU(JL,JK)+ZMFDU(JL,JK))+ &
      PVEN(JL,JK)*(ZMFUV(JL,JK)+ZMFDV(JL,JK)))
               PVOM(JL,JK)=PVOM(JL,JK)+ZDUDT
               PVOL(JL,JK)=PVOL(JL,JK)+ZDVDT
            END IF
  170    CONTINUE
       END IF
  190 CONTINUE
      ZSUM=SSUM(KLON,ZDISS(1),1)
      PSDISS=PSDISS+ZSUM
      RETURN
      END SUBROUTINE CUDUDV
!

!#################################################################
!
!                 LEVEL 4 SUBROUTINES
!
!#################################################################
!**************************************************************
!             SUBROUTINE CUBASMC
!**************************************************************
      SUBROUTINE CUBASMC   &
         (KLON,     KLEV,     KLEVM1,  KK,     PTEN,  &
          PQEN,     PQSEN,    PUEN,    PVEN,   PVERV, &
          PGEO,     PGEOH,    LDCUM,   KTYPE,  KLAB,  &
          PMFU,     PMFUB,    PENTR,   KCBOT,  PTU,   &
          PQU,      PLU,      PUU,     PVU,    PMFUS, &
          PMFUQ,    PMFUL,    PDMFUP,  PMFUU,  PMFUV) 
!      M.TIEDTKE         E.C.M.W.F.     12/89
!***PURPOSE.
!   --------
!          THIS ROUTINE CALCULATES CLOUD BASE VALUES
!          FOR MIDLEVEL CONVECTION
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUASC*.
!          INPUT ARE ENVIRONMENTAL VALUES T,Q ETC
!          IT RETURNS CLOUDBASE VALUES FOR MIDLEVEL CONVECTION
!***METHOD.
!   -------
!          S. TIEDTKE (1989)
!***EXTERNALS
!   ---------
!          NONE
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KLEVM1,KK, JL
      REAL      zzzmb
      REAL     PTEN(KLON,KLEV),        PQEN(KLON,KLEV),  &
              PUEN(KLON,KLEV),        PVEN(KLON,KLEV),   &
              PQSEN(KLON,KLEV),       PVERV(KLON,KLEV),  & 
              PGEO(KLON,KLEV),        PGEOH(KLON,KLEV)
      REAL     PTU(KLON,KLEV),         PQU(KLON,KLEV),   &
              PUU(KLON,KLEV),         PVU(KLON,KLEV),    &
              PLU(KLON,KLEV),         PMFU(KLON,KLEV),   &
              PMFUB(KLON),            PENTR(KLON),       &
              PMFUS(KLON,KLEV),       PMFUQ(KLON,KLEV),  &
              PMFUL(KLON,KLEV),       PDMFUP(KLON,KLEV), &
              PMFUU(KLON),            PMFUV(KLON)
      INTEGER  KTYPE(KLON),            KCBOT(KLON),      &
              KLAB(KLON,KLEV)
      LOGICAL  LDCUM(KLON)
!--------------------------------------------------------
!*    1.      CALCULATE ENTRAINMENT AND DETRAINMENT RATES
! -------------------------------------------------------
  100 CONTINUE
         DO 150 JL=1,KLON
          IF( .NOT. LDCUM(JL).AND.KLAB(JL,KK+1).EQ.0.0.AND.  &
             PQEN(JL,KK).GT.0.80*PQSEN(JL,KK)) THEN
            PTU(JL,KK+1)=(CPD*PTEN(JL,KK)+PGEO(JL,KK)-PGEOH(JL,KK+1)) &
                               *RCPD
            PQU(JL,KK+1)=PQEN(JL,KK)
            PLU(JL,KK+1)=0.
            ZZZMB=MAX(CMFCMIN,-PVERV(JL,KK)/G)
            ZZZMB=MIN(ZZZMB,CMFCMAX)
            PMFUB(JL)=ZZZMB
            PMFU(JL,KK+1)=PMFUB(JL)
            PMFUS(JL,KK+1)=PMFUB(JL)*(CPD*PTU(JL,KK+1)+PGEOH(JL,KK+1))
            PMFUQ(JL,KK+1)=PMFUB(JL)*PQU(JL,KK+1)
            PMFUL(JL,KK+1)=0.
            PDMFUP(JL,KK+1)=0.
            KCBOT(JL)=KK
            KLAB(JL,KK+1)=1
            KTYPE(JL)=3
            PENTR(JL)=ENTRMID
               IF(LMFDUDV) THEN
                  PUU(JL,KK+1)=PUEN(JL,KK)
                  PVU(JL,KK+1)=PVEN(JL,KK)
                  PMFUU(JL)=PMFUB(JL)*PUU(JL,KK+1)
                  PMFUV(JL)=PMFUB(JL)*PVU(JL,KK+1)
               END IF
         END IF
  150   CONTINUE
      RETURN
      END SUBROUTINE CUBASMC

!
!**************************************************************
!             SUBROUTINE CUADJTQ
!**************************************************************
      SUBROUTINE CUADJTQ(KLON,KLEV,KK,PP,PT,PQ,LDFLAG,KCALL)
!      M.TIEDTKE         E.C.M.W.F.     12/89
!      D.SALMOND         CRAY(UK))      12/8/91
!***PURPOSE.
!   --------
!          TO PRODUCE T,Q AND L VALUES FOR CLOUD ASCENT
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM SUBROUTINES:
!              *CUBASE*   (T AND Q AT CONDENSTION LEVEL)
!              *CUASC*    (T AND Q AT CLOUD LEVELS)
!              *CUINI*    (ENVIRONMENTAL T AND QS VALUES AT HALF LEVELS)
!          INPUT ARE UNADJUSTED T AND Q VALUES,
!          IT RETURNS ADJUSTED VALUES OF T AND Q
!          NOTE: INPUT PARAMETER KCALL DEFINES CALCULATION AS
!               KCALL=0    ENV. T AND QS IN*CUINI*
!               KCALL=1  CONDENSATION IN UPDRAFTS  (E.G.  CUBASE, CUASC)
!               KCALL=2  EVAPORATION IN DOWNDRAFTS (E.G.  CUDLFS,CUDDRAF
!***EXTERNALS
!   ---------
!          3 LOOKUP TABLES ( TLUCUA, TLUCUB, TLUCUC )
!          FOR CONDENSATION CALCULATIONS.
!          THE TABLES ARE INITIALISED IN *SETPHYS*.
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV
      INTEGER   KK, KCALL, ISUM, JL
      REAL      ZQSAT, ZCOR, ZCOND1, TT
      REAL     PT(KLON,KLEV),          PQ(KLON,KLEV),  &
              ZCOND(KLON),            ZQP(KLON),       &
              PP(KLON)
      LOGICAL  LDFLAG(KLON)
!------------------------------------------------------------------
!     2.      CALCULATE CONDENSATION AND ADJUST T AND Q ACCORDINGLY
!------------------------------------------------------------------
  200 CONTINUE
      IF (KCALL.EQ.1 ) THEN
         ISUM=0
         DO 210 JL=1,KLON
         ZCOND(JL)=0.
         IF(LDFLAG(JL)) THEN
            ZQP(JL)=1./PP(JL)
            TT=PT(JL,KK)
            ZQSAT=TLUCUA(TT)*ZQP(JL)
            ZQSAT=MIN(0.5,ZQSAT)
            ZCOR=1./(1.-VTMPC1*ZQSAT)
            ZQSAT=ZQSAT*ZCOR
            ZCOND(JL)=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
            ZCOND(JL)=MAX(ZCOND(JL),0.)
            PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND(JL)
            PQ(JL,KK)=PQ(JL,KK)-ZCOND(JL)
            IF(ZCOND(JL).NE.0.0) ISUM=ISUM+1
         END IF
  210    CONTINUE
         IF(ISUM.EQ.0) GO TO 230
         DO 220 JL=1,KLON
         IF(LDFLAG(JL).AND.ZCOND(JL).NE.0.) THEN
            TT=PT(JL,KK)
            ZQSAT=TLUCUA(TT)*ZQP(JL)
            ZQSAT=MIN(0.5,ZQSAT)
            ZCOR=1./(1.-VTMPC1*ZQSAT)
            ZQSAT=ZQSAT*ZCOR
            ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
            PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND1
            PQ(JL,KK)=PQ(JL,KK)-ZCOND1
         END IF
  220    CONTINUE
  230    CONTINUE
      END IF
      IF(KCALL.EQ.2) THEN
         ISUM=0
         DO 310 JL=1,KLON
         ZCOND(JL)=0.
         IF(LDFLAG(JL)) THEN
            TT=PT(JL,KK)
            ZQP(JL)=1./PP(JL)
            ZQSAT=TLUCUA(TT)*ZQP(JL)
            ZQSAT=MIN(0.5,ZQSAT)
            ZCOR=1./(1.-VTMPC1*ZQSAT)
            ZQSAT=ZQSAT*ZCOR
            ZCOND(JL)=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
            ZCOND(JL)=MIN(ZCOND(JL),0.)
            PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND(JL)
            PQ(JL,KK)=PQ(JL,KK)-ZCOND(JL)
            IF(ZCOND(JL).NE.0.0) ISUM=ISUM+1
         END IF
  310    CONTINUE
         IF(ISUM.EQ.0) GO TO 330
         DO 320 JL=1,KLON
         IF(LDFLAG(JL).AND.ZCOND(JL).NE.0.) THEN
            TT=PT(JL,KK)
            ZQSAT=TLUCUA(TT)*ZQP(JL)
            ZQSAT=MIN(0.5,ZQSAT)
            ZCOR=1./(1.-VTMPC1*ZQSAT)
            ZQSAT=ZQSAT*ZCOR
            ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
            PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND1
            PQ(JL,KK)=PQ(JL,KK)-ZCOND1
         END IF
  320    CONTINUE
  330    CONTINUE
      END IF
      IF(KCALL.EQ.0) THEN
         ISUM=0
         DO 410 JL=1,KLON
           TT=PT(JL,KK)
           ZQP(JL)=1./PP(JL)
           ZQSAT=TLUCUA(TT)*ZQP(JL)
           ZQSAT=MIN(0.5,ZQSAT)
           ZCOR=1./(1.-VTMPC1*ZQSAT)
           ZQSAT=ZQSAT*ZCOR
           ZCOND(JL)=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
           PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND(JL)
           PQ(JL,KK)=PQ(JL,KK)-ZCOND(JL)
           IF(ZCOND(JL).NE.0.0) ISUM=ISUM+1
  410    CONTINUE
         IF(ISUM.EQ.0) GO TO 430
         DO 420 JL=1,KLON
           TT=PT(JL,KK)
           ZQSAT=TLUCUA(TT)*ZQP(JL)
           ZQSAT=MIN(0.5,ZQSAT)
           ZCOR=1./(1.-VTMPC1*ZQSAT)
           ZQSAT=ZQSAT*ZCOR
           ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
           PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND1
           PQ(JL,KK)=PQ(JL,KK)-ZCOND1
  420    CONTINUE
  430    CONTINUE
      END IF
      IF(KCALL.EQ.4) THEN
         DO 510 JL=1,KLON
           TT=PT(JL,KK)
           ZQP(JL)=1./PP(JL)
           ZQSAT=TLUCUA(TT)*ZQP(JL)
           ZQSAT=MIN(0.5,ZQSAT)
           ZCOR=1./(1.-VTMPC1*ZQSAT)
           ZQSAT=ZQSAT*ZCOR
           ZCOND(JL)=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
           PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND(JL)
           PQ(JL,KK)=PQ(JL,KK)-ZCOND(JL)
  510    CONTINUE
         DO 520 JL=1,KLON
           TT=PT(JL,KK)
           ZQSAT=TLUCUA(TT)*ZQP(JL)
           ZQSAT=MIN(0.5,ZQSAT)
           ZCOR=1./(1.-VTMPC1*ZQSAT)
           ZQSAT=ZQSAT*ZCOR
           ZCOND1=(PQ(JL,KK)-ZQSAT)/(1.+ZQSAT*ZCOR*TLUCUB(TT))
           PT(JL,KK)=PT(JL,KK)+TLUCUC(TT)*ZCOND1
           PQ(JL,KK)=PQ(JL,KK)-ZCOND1
  520    CONTINUE
      END IF
      RETURN
      END SUBROUTINE CUADJTQ

!
!**********************************************************
!        SUBROUTINE CUENTR_NEW
!**********************************************************
      SUBROUTINE CUENTR_NEW                              &   
         (KLON,     KLEV,     KLEVP1,   KK,       PTENH, &
          PAPH,     PAP,      PGEOH,    KLWMIN,   LDCUM, &
          KTYPE,    KCBOT,    KCTOP0,   ZPBASE,   PMFU,  &
          PENTR,    ZDMFEN,   ZDMFDE,   ZODETR,   KHMIN)
!      M.TIEDTKE         E.C.M.W.F.     12/89
!      Y.WANG            IPRC           11/01
!***PURPOSE.
!   --------
!          THIS ROUTINE CALCULATES ENTRAINMENT/DETRAINMENT RATES
!          FOR UPDRAFTS IN CUMULUS PARAMETERIZATION
!***INTERFACE
!   ---------
!          THIS ROUTINE IS CALLED FROM *CUASC*.
!          INPUT ARE ENVIRONMENTAL VALUES T,Q ETC
!          AND UPDRAFT VALUES T,Q ETC
!          IT RETURNS ENTRAINMENT/DETRAINMENT RATES
!***METHOD.
!  --------
!          S. TIEDTKE (1989), NORDENG(1996)
!***EXTERNALS
!   ---------
!          NONE
! ----------------------------------------------------------------
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
      INTEGER   KLON, KLEV, KLEVP1
      INTEGER   KK, JL, IKLWMIN,IKB, IKT, IKH
      REAL      ZRRHO, ZDPRHO, ZPMID, ZENTR, ZZMZK, ZTMZK, ARG, ZORGDE
      REAL     PTENH(KLON,KLEV),                           &
              PAP(KLON,KLEV),         PAPH(KLON,KLEVP1),   &
              PMFU(KLON,KLEV),        PGEOH(KLON,KLEV),    &
              PENTR(KLON),            ZPBASE(KLON),        &
              ZDMFEN(KLON),           ZDMFDE(KLON),        &
              ZODETR(KLON,KLEV)
      INTEGER  KLWMIN(KLON),           KTYPE(KLON),        &
              KCBOT(KLON),            KCTOP0(KLON),        &
              KHMIN(KLON)
      LOGICAL  LDCUM(KLON),LLO1,LLO2

      real    tt(klon),ttb(klon)
      real    zqsat(klon), zqsatb(klon)
      real    fscale(klon)
!---------------------------------------------------------
!*    1.       CALCULATE ENTRAINMENT AND DETRAINMENT RATES
!---------------------------------------------------------
!*    1.1      SPECIFY ENTRAINMENT RATES FOR SHALLOW CLOUDS
!----------------------------------------------------------
!*    1.2      SPECIFY ENTRAINMENT RATES FOR DEEP CLOUDS
!-------------------------------------------------------
      DO jl = 1, klon
        zpbase(jl) = paph(jl,kcbot(jl))
        zrrho = (rd*ptenh(jl,kk+1))/paph(jl,kk+1)
        zdprho = (paph(jl,kk+1)-paph(jl,kk))*zrg
! old or new choice
        zpmid = 0.5*(zpbase(jl)+paph(jl,kctop0(jl)))
        zentr = pentr(jl)*pmfu(jl,kk+1)*zdprho*zrrho
        llo1 = kk.LT.kcbot(jl).AND.ldcum(jl)
! old or new choice
        if(llo1) then
           if(nturben.eq.1) zdmfde(jl) = zentr
           if(nturben.eq.2) zdmfde(jl) = zentr*1.2
        else
          zdmfde(jl) = 0.0
        endif
! old or new choice
        if(nturben .eq. 1) then
          fscale(jl) = 1.0
        elseif (nturben .eq. 2) then
! defining the facale
        tt(jl) = ptenh(jl,kk+1)
        zqsat(jl) = TLUCUA(tt(jl))/paph(jl,kk+1)
        zqsat(jl) = zqsat(jl)/(1.-VTMPC1*zqsat(jl))
        ttb(jl) = ptenh(jl,kcbot(jl))
        zqsatb(jl) = TLUCUA(ttb(jl))/zpbase(jl)
        zqsatb(jl) = zqsatb(jl)/(1.-VTMPC1*zqsatb(jl))
        fscale(jl) = 4.0*(zqsat(jl)/zqsatb(jl))**2
        end if
! end of defining the fscale
        llo2 = llo1.AND.ktype(jl).EQ.2.AND.((zpbase(jl)-paph(jl,kk)) &
             .LT.ZDNOPRC.OR.paph(jl,kk).GT.zpmid)
        if(llo2) then
            zdmfen(jl) = zentr*fscale(jl)
        else
            zdmfen(jl) = 0.0
        endif
        iklwmin = MAX(klwmin(jl),kctop0(jl)+2)
        llo2 = llo1.AND.ktype(jl).EQ.3.AND.(kk.GE.iklwmin.OR.pap(jl,kk) &
             .GT.zpmid)
        IF (llo2) zdmfen(jl) = zentr*fscale(jl)
        llo2 = llo1.AND.ktype(jl).EQ.1
! Turbulent entrainment
        IF (llo2) zdmfen(jl) = zentr*fscale(jl)
! Organized detrainment, detrainment starts at khmin
        ikb = kcbot(jl)
        zodetr(jl,kk) = 0.
        IF (llo2.AND.kk.LE.khmin(jl).AND.kk.GE.kctop0(jl)) THEN
          ikt = kctop0(jl)
          ikh = khmin(jl)
          IF (ikh.GT.ikt) THEN
            zzmzk = -(pgeoh(jl,ikh)-pgeoh(jl,kk))*zrg
            ztmzk = -(pgeoh(jl,ikh)-pgeoh(jl,ikt))*zrg
            arg = 3.1415*(zzmzk/ztmzk)*0.5
            zorgde = TAN(arg)*3.1415*0.5/ztmzk
            zdprho = (paph(jl,kk+1)-paph(jl,kk))*(zrg*zrrho)
            zodetr(jl,kk) = MIN(zorgde,1.E-3)*pmfu(jl,kk+1)*zdprho
          END IF
        END IF
      ENDDO
!
      RETURN
      END SUBROUTINE CUENTR_NEW

!**********************************************************
!        FUNCTION SSUM, TLUCUA, TLUCUB, TLUCUC
!**********************************************************
      REAL FUNCTION SSUM ( N, X, IX )
!
! COMPUTES SSUM = SUM OF [X(I)]
!     FOR N ELEMENTS OF X WITH SKIP INCREMENT IX FOR VECTOR X
!
      IMPLICIT NONE
      REAL X(*)
      REAL ZSUM
      INTEGER N, IX, JX, JL
!
      JX = 1
      ZSUM = 0.0
      DO JL = 1, N
        ZSUM = ZSUM + X(JX)
        JX = JX + IX
      enddo
!
      SSUM=ZSUM
!
      RETURN
      END FUNCTION SSUM

      REAL FUNCTION TLUCUA(TT)
!
!  Set up lookup tables for cloud ascent calculations.
!
      IMPLICIT NONE
      REAL ZCVM3,ZCVM4,TT
!
      IF(TT-TMELT.GT.0.) THEN
         ZCVM3=C3LES
         ZCVM4=C4LES
      ELSE
         ZCVM3=C3IES
         ZCVM4=C4IES
      END IF
      TLUCUA=C2ES*EXP(ZCVM3*(TT-TMELT)*(1./(TT-ZCVM4)))
!
      RETURN
      END FUNCTION TLUCUA
!
      REAL FUNCTION TLUCUB(TT)
!
!  Set up lookup tables for cloud ascent calculations.
!
      IMPLICIT NONE
      REAL Z5ALVCP,Z5ALSCP,ZCVM4,ZCVM5,TT
!
      Z5ALVCP=C5LES*ALV/CPD
      Z5ALSCP=C5IES*ALS/CPD
      IF(TT-TMELT.GT.0.) THEN
         ZCVM4=C4LES
         ZCVM5=Z5ALVCP
      ELSE
         ZCVM4=C4IES
         ZCVM5=Z5ALSCP
      END IF
      TLUCUB=ZCVM5*(1./(TT-ZCVM4))**2
!
      RETURN
      END FUNCTION TLUCUB
!
      REAL FUNCTION TLUCUC(TT)
!
!  Set up lookup tables for cloud ascent calculations.
!
      IMPLICIT NONE
      REAL ZALVDCP,ZALSDCP,TT,ZLDCP
!
      ZALVDCP=ALV/CPD
      ZALSDCP=ALS/CPD
      IF(TT-TMELT.GT.0.) THEN
         ZLDCP=ZALVDCP
      ELSE
         ZLDCP=ZALSDCP
      END IF
      TLUCUC=ZLDCP
!
      RETURN
      END FUNCTION TLUCUC
!

END MODULE module_cu_tiedtke

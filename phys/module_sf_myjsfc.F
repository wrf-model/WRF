!----------------------------------------------------------------------
!
      MODULE MODULE_SF_MYJSFC
!
!----------------------------------------------------------------------
!
      USE MODULE_MODEL_CONSTANTS
#ifdef DM_PARALLEL
      USE MODULE_DM, ONLY : WRF_DM_MAXVAL
#endif
!
!----------------------------------------------------------------------
!
! REFERENCES:  Janjic (2002), NCEP Office Note 437
!
! ABSTRACT:
!     MYJSFC GENERATES THE SURFACE EXCHANGE COEFFICIENTS FOR VERTICAL
!     TURBULENT EXCHANGE BASED UPON MONIN_OBUKHOV THEORY WITH
!     VARIOUS REFINEMENTS.
!
!----------------------------------------------------------------------
!
      INTEGER :: ITRMX=5 ! Iteration count for sfc layer computations
!
      REAL,PARAMETER :: VKARMAN=0.4
      REAL,PARAMETER :: CAPA=R_D/CP,ELOCP=2.72E6/CP,RCAP=1./CAPA
      REAL,PARAMETER :: GOCP02=G/CP*2.,GOCP10=G/CP*10.
!      REAL,PARAMETER :: EPSU2=1.E-4,EPSUST=0.07,EPSZT=1.E-5
!	ECMWF sets lower limit on ln(Z0h)=-20 --> EPSZT=2.e-9
      REAL,PARAMETER :: EPSU2=1.E-6,EPSUST=1.e-9,EPSZT=1.E-28
      REAL,PARAMETER :: A2S=17.2693882,A3S=273.16,A4S=35.86
      REAL,PARAMETER :: SEAFC=0.98,PQ0SEA=PQ0*SEAFC
      REAL,PARAMETER :: BETA=1./273.,EXCML=0.0001,EXCMS=0.0001  &
     &                 ,GLKBR=10.,GLKBS=30.,PI=3.1415926               &
     &                 ,QVISC=2.1E-5,RIC=0.505,SMALL=0.35              &
     &                 ,SQPR=0.84,SQSC=0.84,SQVISC=258.2,TVISC=2.1E-5  &
     &                 ,USTC=0.7,USTR=0.225,VISC=1.5E-5,FH=1.01        &
     &                 ,WWST=1.2,ZTFC=1.,TOPOFAC=9.0e-6
!
      REAL,PARAMETER :: BTG=BETA*G,CZIV=SMALL*GLKBS                    &
     &                 ,GRRS=GLKBR/GLKBS                               &
     &                 ,RTVISC=1./TVISC,RVISC=1./VISC                  &
     &                 ,ZQRZT=SQSC/SQPR                                &
     &                 ,FZQ1=RTVISC*QVISC*ZQRZT                        &
     &                 ,FZQ2=RTVISC*QVISC*ZQRZT                        &
     &                 ,FZT1=RVISC *TVISC*SQPR                         &
     &                 ,FZT2=CZIV*GRRS*TVISC*SQPR                      &
     &                 ,FZU1=CZIV*VISC                                 &
     &                 ,PIHF=0.5*PI                                    &
     &                 ,RQVISC=1./QVISC                                &
     &                 ,USTFC=0.018/G                                  &
     &                 ,WWST2=WWST*WWST
!
!----------------------------------------------------------------------
      INTEGER, PARAMETER :: KZTM=10001,KZTM2=KZTM-2
!
      REAL :: DZETA1,DZETA2,FH01,FH02,ZTMAX1,ZTMAX2,ZTMIN1,ZTMIN2
!
      REAL,DIMENSION(KZTM) :: PSIH1,PSIH2,PSIM1,PSIM2
!
!----------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------
      SUBROUTINE MYJSFC(ITIMESTEP,HT,DZ                                &
     &                 ,PMID,PINT,TH,T,QV,QC,U,V,Q2                    &
     &                 ,TSK,QSFC,THZ0,QZ0,UZ0,VZ0                      &
     &                 ,LOWLYR,XLAND,IVGTYP,ISURBAN,IZ0TLND            &
     &                 ,USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL              &
     &                 ,AKHS,AKMS                                      &
     &                 ,RIB                                            &
     &                 ,CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC         &
     &                 ,QGH,CPM,CT                                     &
     &                 ,U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR          &
     &                 ,P1000mb,U10E,V10E                              &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                 ,IMS,IME,JMS,JME,KMS,KME                        &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: ITIMESTEP
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LOWLYR
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT,MAVAIL,TSK      &
     &                                             ,XLAND,Z0BASE
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ         &
     &                                                     ,PMID,PINT  &
     &                                                     ,Q2,QC,QV   &
     &                                                     ,T,TH       &
     &                                                     ,U,V
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IVGTYP
      INTEGER,                           INTENT(IN) :: ISURBAN
      INTEGER,                           INTENT(IN) :: IZ0TLND
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: FLX_LH,HFX,PSHLTR &
     &                                              ,QFX,Q10,QSHLTR    &
     &                                              ,TH10,TSHLTR,T02   &
     &                                              ,U10,V10,TH02,Q02  &
     &                                              ,U10E,V10E
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: AKHS,AKMS       &
     &                                                ,PBLH,QSFC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT)   :: RIB
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: QZ0,RMOL,THZ0   &
     &                                                ,USTAR,UZ0,VZ0   &
     &                                                ,ZNT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CHS,CHS2,CQS2     &
     &                                              ,CPM,CT,FLHC,FLQC  &
     &                                              ,QGH
!
      REAL,INTENT(IN) :: P1000mb
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: I,J,K,KFLIP,LMH,LPBL,NTSD
!
      REAL :: A,APESFC,B,BTGX,CWMLOW                                   &
     &       ,DQDT,DTDIF,DTDT,DUDT,DVDT                                &
     &       ,FIS                                                      &
     &       ,P02P,P10P,PLOW,PSFC,PTOP,QLOW,QS02,QS10                  &
     &       ,RAPA,RAPA02,RAPA10,RATIOMX,RDZ,SEAMASK,SM                &
     &       ,T02P,T10P,TEM,TH02P,TH10P,THLOW,THELOW,THM               &
     &       ,TLOW,TZ0,ULOW,VLOW,ZSL
!
      REAL,DIMENSION(KTS:KTE) :: CWMK,PK,Q2K,QK,THEK,THK,TK,UK,VK
!
      REAL,DIMENSION(KTS:KTE-1) :: EL,ELM
!
      REAL,DIMENSION(KTS:KTE+1) :: ZHK
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: THSK
!
      REAL,DIMENSION(ITS:ITE,KTS:KTE+1,JTS:JTE) :: ZINT
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
!***  MAKE PREPARATIONS
!
!----------------------------------------------------------------------
      DO J=JTS,JTE
      DO K=KTS,KTE+1
      DO I=ITS,ITE
        ZINT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        ZINT(I,KTE+1,J)=HT(I,J)     ! Z at bottom of lowest sigma layer
        PBLH(I,J)=-1.
!
!!!!!!!!!
!!!!!! UNCOMMENT THESE LINES IF USING ETA COORDINATES
!!!!!!!!!
!!!!!!  ZINT(I,KTE+1,J)=1.E-4       ! Z of bottom of lowest eta layer
!!!!!!  ZHK(KTE+1)=1.E-4            ! Z of bottom of lowest eta layer
!
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO K=KTE,KTS,-1
        KFLIP=KTE+1-K
        DO I=ITS,ITE
          ZINT(I,K,J)=ZINT(I,K+1,J)+DZ(I,KFLIP,J)
        ENDDO
      ENDDO
      ENDDO
!
      NTSD=ITIMESTEP
!
#if ( NMM_CORE == 1 )
     if(NTSD+1.eq.1) then
#else
     IF(NTSD==1)THEN
#endif
!tgs       IF(NTSD==1)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          USTAR(I,J)=0.1
          FIS=HT(I,J)*G
          SM=XLAND(I,J)-1.
!!!       Z0(I,J)=SM*Z0SEA+(1.-SM)*(Z0(I,J)*Z0MAX+FIS*FCM+Z0LAND)
!!!       ZNT(I,J)=SM*Z0SEA+(1.-SM)*(ZNT(I,J)*Z0MAX+FIS*FCM+Z0LAND)
        ENDDO
        ENDDO
      ENDIF
!
!!!!  IF(NTSD==1)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          CT(I,J)=0.
        ENDDO
        ENDDO
!!!!  ENDIF
!
!----------------------------------------------------------------------
        setup_integration:  DO J=JTS,JTE
!----------------------------------------------------------------------
!
        DO I=ITS,ITE
!
!***  LOWEST LAYER ABOVE GROUND MUST BE FLIPPED
!
          LMH=KTE-LOWLYR(I,J)+1
!
          PTOP=PINT(I,KTE+1,J)      ! KTE+1=KME
          PSFC=PINT(I,LOWLYR(I,J),J)
! Define THSK here (for first timestep mostly)
!          THSK(I,J)=TSK(I,J)/(PSFC*1.E-5)**CAPA
          THSK(I,J)=TSK(I,J)/(PSFC/P1000mb)**CAPA
!
!***  CONVERT LAND MASK (1 FOR SEA; 0 FOR LAND)
!
          SEAMASK=XLAND(I,J)-1.
!
!***  FILL 1-D VERTICAL ARRAYS
!***  AND FLIP DIRECTION SINCE MYJ SCHEME
!***  COUNTS DOWNWARD FROM THE DOMAIN'S TOP
!
          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            THK(K)=TH(I,KFLIP,J)
            TK(K)=T(I,KFLIP,J)
            RATIOMX=QV(I,KFLIP,J)
            QK(K)=RATIOMX/(1.+RATIOMX)
            PK(K)=PMID(I,KFLIP,J)
            CWMK(K)=QC(I,KFLIP,J)
            THEK(K)=(CWMK(K)*(-ELOCP/TK(K))+1.)*THK(K)
            Q2K(K)=2.*Q2(I,KFLIP,J)
!
!***  COMPUTE THE HEIGHTS OF THE LAYER INTERFACES
!
            ZHK(K)=ZINT(I,K,J)
!
          ENDDO
          ZHK(KTE+1)=HT(I,J)          ! Z at bottom of lowest sigma layer
!
          DO K=KTE,KTS,-1
            KFLIP=KTE+1-K
            UK(K)=U(I,KFLIP,J)
            VK(K)=V(I,KFLIP,J)
          ENDDO
!
!***  FIND THE HEIGHT OF THE PBL
!
          LPBL=LMH
          DO K=LMH-1,1,-1
            IF(Q2K(K)<=EPSQ2*FH) THEN
              LPBL=K
              GO TO 110
            ENDIF
          ENDDO
!
          LPBL=1
!
!-----------------------------------------------------------------------
!--------------THE HEIGHT OF THE PBL------------------------------------
!-----------------------------------------------------------------------
!
 110      PBLH(I,J)=ZHK(LPBL)-ZHK(LMH+1)
!
!----------------------------------------------------------------------
!***
!***  FIND THE SURFACE EXCHANGE COEFFICIENTS
!***
!----------------------------------------------------------------------
          PLOW=PK(LMH)
          TLOW=TK(LMH)
          THLOW=THK(LMH)
          THELOW=THEK(LMH)
          QLOW=QK(LMH)
          CWMLOW=CWMK(LMH)
          ULOW=UK(LMH)
          VLOW=VK(LMH)
          ZSL=(ZHK(LMH)-ZHK(LMH+1))*0.5
!          APESFC=(PSFC*1.E-5)**CAPA
          APESFC=(PSFC/P1000mb)**CAPA
!tgs - in ARW THZ0 is not initialized when MYJSFC is called first time
#if ( NMM_CORE == 1 )
     if(NTSD+1.eq.1) then
#else
     IF(NTSD==1)THEN
#endif
!       if(itimestep.le.1) then
          TZ0=TSK(I,J)
       else
          TZ0=THZ0(I,J)*APESFC
       endif
!
          CALL SFCDIF(NTSD,SEAMASK,THSK(I,J),QSFC(I,J),PSFC            &
     &               ,UZ0(I,J),VZ0(I,J),TZ0,TSK(I,J),THZ0(I,J),QZ0(I,J)         &
     &               ,USTAR(I,J),ZNT(I,J),Z0BASE(I,J),CT(I,J),RMOL(I,J) &
     &               ,AKMS(I,J),AKHS(I,J),RIB(I,J),PBLH(I,J),MAVAIL(I,J) &
     &               ,IVGTYP(I,J),ISURBAN,IZ0TLND                      &
     &               ,CHS(I,J),CHS2(I,J),CQS2(I,J)                     &
     &               ,HFX(I,J),QFX(I,J),FLX_LH(I,J)                    &
     &               ,FLHC(I,J),FLQC(I,J),QGH(I,J),CPM(I,J)            &
     &               ,ULOW,VLOW,TLOW,THLOW,THELOW,QLOW,CWMLOW          &
     &               ,ZSL,PLOW                                         &
     &               ,U10(I,J),V10(I,J),TSHLTR(I,J),TH10(I,J)          &
     &               ,QSHLTR(I,J),Q10(I,J),PSHLTR(I,J)                 &
     &               ,U10E(I,J),V10E(I,J)                              &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE,i,j,ZHK(LMH+1))
!
!***  REMOVE SUPERATURATION AT 2M AND 10M
!
          RAPA=APESFC
          TH02P=TSHLTR(I,J)
          TH10P=TH10(I,J)
          TH02(I,J)=TSHLTR(I,J)
!
          RAPA02=RAPA-GOCP02/TH02P
          RAPA10=RAPA-GOCP10/TH10P
!
          T02P=TH02P*RAPA02
          T10P=TH10P*RAPA10
! 1 may 06 tgs         T02(I,J) = T02P
          T02(I,J) = TH02(I,J)*APESFC
!
!          P02P=(RAPA02**RCAP)*1.E5
!          P10P=(RAPA10**RCAP)*1.E5
          P02P=(RAPA02**RCAP)*P1000mb
          P10P=(RAPA10**RCAP)*P1000mb
!
          QS02=PQ0/P02P*EXP(A2*(T02P-A3)/(T02P-A4))
          QS10=PQ0/P10P*EXP(A2*(T10P-A3)/(T10P-A4))
!
          IF(QSHLTR(I,J)>QS02)QSHLTR(I,J)=QS02
          IF(Q10   (I,J)>QS10)Q10   (I,J)=QS10
          Q02(I,J)=QSHLTR(I,J)/(1.-QSHLTR(I,J))
!----------------------------------------------------------------------
!
        ENDDO
!
!----------------------------------------------------------------------
      ENDDO setup_integration
!----------------------------------------------------------------------

      END SUBROUTINE MYJSFC
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!----------------------------------------------------------------------
      SUBROUTINE SFCDIF(NTSD,SEAMASK,THS,QS,PSFC                       &
     &                 ,UZ0,VZ0,TZ0,TSK,THZ0,QZ0                       &
     &                 ,USTAR,Z0,Z0BASE,CT,RLMO,AKMS,AKHS,RIB,PBLH,WETM &
     &                 ,VEGTYP,ISURBAN,IZ0TLND                         &
     &                 ,CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,QGH,CPM &
     &                 ,ULOW,VLOW,TLOW,THLOW,THELOW,QLOW,CWMLOW        &
     &                 ,ZSL,PLOW                                       &
     &                 ,U10,V10,TH02,TH10,Q02,Q10,PSHLTR               &
     &                 ,U10E,V10E                                      &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                 ,IMS,IME,JMS,JME,KMS,KME                        &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE,i,j,ZSFC)
!     ****************************************************************
!     *                                                              *
!     *                       SURFACE LAYER                          *
!     *                                                              *
!     ****************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE,i,j
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: CWMLOW,PBLH,PLOW,QLOW,PSFC,SEAMASK,ZSFC       &
     &                  ,THELOW,THLOW,THS,TSK,TLOW,TZ0,ULOW,VLOW,WETM,ZSL  &
     &                  ,Z0BASE
      INTEGER, INTENT(IN) :: VEGTYP, ISURBAN, IZ0TLND
!
      REAL,INTENT(OUT) :: CHS,CHS2,CPM,CQS2,CT,FLHC,FLQC,FLX_LH,HFX    &
     &                   ,PSHLTR,Q02,Q10,QFX,QGH,RIB,RLMO              &
     &                   ,TH02,TH10,U10,V10,U10E,V10E
!
      REAL,INTENT(INOUT) :: AKHS,AKMS,QZ0,THZ0,USTAR,UZ0,VZ0,Z0,QS
!----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: ITR,K
!
      REAL :: A,B,BTGH,BTGX,CXCHL,CXCHS,CZETMAX,DTHV,DU2,ELFC,FCT      &
     &       ,HLFLX,HSFLX,HV,PSH02,PSH10,PSHZ,PSHZL,PSM10,PSMZ,PSMZL   &
     &       ,RDZ,RDZT,RLMA,RLMN,RLMP                                  &
     &       ,RLOGT,RLOGU,RWGH,RZ,RZST,RZSU,SIMH,SIMM,TEM,THM          &
     &       ,UMFLX,USTARK,VMFLX,WGHT,WGHTT,WGHTQ,WSTAR2               &
     &       ,X,XLT,XLT4,XLU,XLU4,XT,XT4,XU,XU4,ZETALT,ZETALU          &
     &       ,ZETAT,ZETAU,ZQ,ZSLT,ZSLU,ZT,ZU,TOPOTERM,ZZIL
      REAL :: CZIL,ZILFC
!
!***  DIAGNOSTICS
!
      REAL :: AKHS02,AKHS10,AKMS02,AKMS10,EKMS10,QSAT10,QSAT2          &
     &       ,RLNT02,RLNT10,RLNU10,SIMH02,SIMH10,SIMM10,T02,T10        &
     &       ,TERM1,RLOW,WSTAR,XLT02,XLT024,XLT10            &
     &       ,XLT104,XLU10,XLU104,XU10,XU104,ZT02,ZT10,ZTAT02,ZTAT10   &
     &       ,ZTAU,ZTAU10,ZU10,ZUUZ
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      RDZ=1./ZSL
      CXCHL=EXCML*RDZ
      CXCHS=EXCMS*RDZ
!
      BTGX=G/THLOW
      ELFC=VKARMAN*BTGX
!
      IF(PBLH>1000.)THEN
        BTGH=BTGX*PBLH
      ELSE
        BTGH=BTGX*1000.
      ENDIF
!
!----------------------------------------------------------------------
!
!***  SEA POINTS
!
!----------------------------------------------------------------------
!
      IF(SEAMASK>0.5)THEN
!
!----------------------------------------------------------------------
        DO ITR=1,ITRMX
!----------------------------------------------------------------------
          Z0=MAX(USTFC*USTAR*USTAR,1.59E-5)
!
!***  VISCOUS SUBLAYER, JANJIC MWR 1994
!
!----------------------------------------------------------------------
          IF(USTAR<USTC)THEN
!----------------------------------------------------------------------
!
            IF(USTAR<USTR)THEN
!
#if ( NMM_CORE == 1 )
     if(NTSD+1.eq.1) then
#else
     IF(NTSD==1)THEN
#endif
!tgs              IF(NTSD==1)THEN
                AKMS=CXCHS
                AKHS=CXCHS
                QS=QLOW
              ENDIF
!
              ZU=FZU1*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
              WGHT=AKMS*ZU*RVISC
              RWGH=WGHT/(WGHT+1.)
              UZ0=(ULOW*RWGH+UZ0)*0.5
              VZ0=(VLOW*RWGH+VZ0)*0.5
!
              ZT=FZT1*ZU
              ZQ=FZQ1*ZT
              WGHTT=AKHS*ZT*RTVISC
              WGHTQ=AKHS*ZQ*RQVISC
!
#if ( NMM_CORE == 1 )
     if(NTSD+1>1) then
#else
     IF(NTSD>1)THEN
#endif
!tgs              IF(NTSD>1)THEN
                THZ0=((WGHTT*THLOW+THS)/(WGHTT+1.)+THZ0)*0.5
                QZ0=((WGHTQ*QLOW+QS)/(WGHTQ+1.)+QZ0)*0.5
              ELSE
                THZ0=(WGHTT*THLOW+THS)/(WGHTT+1.)
                QZ0=(WGHTQ*QLOW+QS)/(WGHTQ+1.)
              ENDIF
!
            ENDIF
!
            IF(USTAR>=USTR.AND.USTAR<USTC)THEN
              ZU=Z0
              UZ0=0.
              VZ0=0.
!
              ZT=FZT2*SQRT(SQRT(Z0*USTAR*RVISC))/USTAR
              ZQ=FZQ2*ZT
              WGHTT=AKHS*ZT*RTVISC
              WGHTQ=AKHS*ZQ*RQVISC
!
#if ( NMM_CORE == 1 )
     if(NTSD+1>1) then
#else
     IF(NTSD>1)THEN
#endif

!tgs              IF(NTSD>1)THEN
                THZ0=((WGHTT*THLOW+THS)/(WGHTT+1.)+THZ0)*0.5
                QZ0=((WGHTQ*QLOW+QS)/(WGHTQ+1.)+QZ0)*0.5
              ELSE
                THZ0=(WGHTT*THLOW+THS)/(WGHTT+1.)
                QZ0=(WGHTQ*QLOW+QS)/(WGHTQ+1.)
              ENDIF
!
            ENDIF
!----------------------------------------------------------------------
          ELSE
!----------------------------------------------------------------------
            ZU=Z0
            UZ0=0.
            VZ0=0.
!
            ZT=Z0
            THZ0=THS
!
            ZQ=Z0
            QZ0=QS
!----------------------------------------------------------------------
          ENDIF
!----------------------------------------------------------------------
          TEM=(TLOW+TZ0)*0.5
          THM=(THELOW+THZ0)*0.5
!
          A=THM*P608
          B=(ELOCP/TEM-1.-P608)*THM
!
          DTHV=((THELOW-THZ0)*((QLOW+QZ0+CWMLOW)*(0.5*P608)+1.)        &
     &        +(QLOW-QZ0+CWMLOW)*A+CWMLOW*B)
!
          DU2=MAX((ULOW-UZ0)**2+(VLOW-VZ0)**2,EPSU2)
          RIB=BTGX*DTHV*ZSL/DU2
!----------------------------------------------------------------------
!         IF(RIB>=RIC)THEN
!----------------------------------------------------------------------
!           AKMS=MAX( VISC*RDZ,CXCHS)
!           AKHS=MAX(TVISC*RDZ,CXCHS)
!----------------------------------------------------------------------
!         ELSE  !  turbulent branch
!----------------------------------------------------------------------
            ZSLU=ZSL+ZU
            ZSLT=ZSL+ZT
!
            RZSU=ZSLU/ZU
            RZST=ZSLT/ZT
!
            RLOGU=LOG(RZSU)
            RLOGT=LOG(RZST)
!
!----------------------------------------------------------------------
!***  1./MONIN-OBUKHOV LENGTH
!----------------------------------------------------------------------
!
            RLMO=ELFC*AKHS*DTHV/USTAR**3
!
            ZETALU=ZSLU*RLMO
            ZETALT=ZSLT*RLMO
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO
!
            ZETALU=MIN(MAX(ZETALU,ZTMIN1),ZTMAX1)
            ZETALT=MIN(MAX(ZETALT,ZTMIN1),ZTMAX1)
            ZETAU=MIN(MAX(ZETAU,ZTMIN1/RZSU),ZTMAX1/RZSU)
            ZETAT=MIN(MAX(ZETAT,ZTMIN1/RZST),ZTMAX1/RZST)
!
!----------------------------------------------------------------------
!***   WATER FUNCTIONS
!----------------------------------------------------------------------
!
            RZ=(ZETAU-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZ=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)
!
            RZ=(ZETALU-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZL=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)
!
            SIMM=PSMZL-PSMZ+RLOGU
!
            RZ=(ZETAT-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZ=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)
!
            RZ=(ZETALT-ZTMIN1)/DZETA1
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZL=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)
!
            SIMH=(PSHZL-PSHZ+RLOGT)*FH01

!----------------------------------------------------------------------
            USTARK=USTAR*VKARMAN
!!!!!!!!!!!!!!!!!!!!!
            AKMS=MAX(USTARK/SIMM,CXCHS)
            AKHS=MAX(USTARK/SIMH,CXCHS)
!
!----------------------------------------------------------------------
!***  BELJAARS CORRECTION FOR USTAR
!----------------------------------------------------------------------
!
            IF(DTHV<=0.)THEN                                           !zj
              WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                !zj
            ELSE                                                       !zj
              WSTAR2=0.                                                !zj
            ENDIF                                                      !zj
            USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
!
!----------------------------------------------------------------------
!         ENDIF  !  End of turbulent branch
!----------------------------------------------------------------------
!
        ENDDO  !  End of the iteration loop over sea points
!
!----------------------------------------------------------------------
!
!***  LAND POINTS
!
!----------------------------------------------------------------------
!
      ELSE
!
!----------------------------------------------------------------------
        IF(NTSD==1)THEN
          QS=QLOW
        ENDIF
!
        ZU=Z0
        UZ0=0.
        VZ0=0.
!
        ZT=ZU*ZTFC
        THZ0=THS
!
        ZQ=ZT
        QZ0=QS
!----------------------------------------------------------------------
        TEM=(TLOW+TZ0)*0.5
        THM=(THELOW+THZ0)*0.5
!
        A=THM*P608
        B=(ELOCP/TEM-1.-P608)*THM
!
        DTHV=((THELOW-THZ0)*((QLOW+QZ0+CWMLOW)*(0.5*P608)+1.)          &
     &        +(QLOW-QZ0+CWMLOW)*A+CWMLOW*B)
!
        DU2=MAX((ULOW-UZ0)**2+(VLOW-VZ0)**2,EPSU2)
        RIB=BTGX*DTHV*ZSL/DU2
!----------------------------------------------------------------------
!       IF(RIB>=RIC)THEN
!         AKMS=MAX( VISC*RDZ,CXCHL)
!         AKHS=MAX(TVISC*RDZ,CXCHL)
!----------------------------------------------------------------------
!       ELSE  !  Turbulent branch
!----------------------------------------------------------------------
          ZSLU=ZSL+ZU
!
          RZSU=ZSLU/ZU
!
          RLOGU=LOG(RZSU)

          ZSLT=ZSL+ZU ! u,v and t are at the same level

!-- BSF on 10/19/2011: Added new IF-ELSE block but commented out until 
!   final matter is resolved.
!BSF          IF ( (IZ0TLND==0) .or. (VEGTYP == ISURBAN) ) THEN
!BSF             ! Just use the original CZIL value.
!BSF             CZIL = 0.1
!BSF          ELSE
!BSF             ! Modify CZIL according to Chen & Zhang, 2009
!BSF             ! CZIL = 10 ** -0.40 H, ( where H = 10*Zo )
!BSF             CZIL = 10.0 ** ( -0.40 * ( Z0 / 0.07 ) )
!BSF          ENDIF
          CZIL=0.1
          ZILFC=-CZIL*VKARMAN*SQVISC
          
!----------------------------------------------------------------------
!
!mp   Topo modification of ZILFC term
!	
!         TOPOTERM=TOPOFAC*ZSFC**2.
!         TOPOTERM=MAX(TOPOTERM,3.0)
!
!  RIB modification to ZILFC term
!
!         CZETMAX = 20.
          CZETMAX = 10.
! stable
          IF(DTHV>0.)THEN
!            ZZIL=ZILFC*TOPOTERM
            IF (RIB<RIC) THEN
              ZZIL=ZILFC*(1.0+(RIB/RIC)*(RIB/RIC)*CZETMAX)
            ELSE
              ZZIL=ZILFC*(1.0+CZETMAX)
            ENDIF
! unstable
          ELSE
            ZZIL=ZILFC
          ENDIF
!
!----------------------------------------------------------------------
!
          land_point_iteration: DO ITR=1,ITRMX
!
!----------------------------------------------------------------------
!***  ZILITINKEVITCH FIX FOR ZT
!----------------------------------------------------------------------
!
! oldform   ZT=MAX(EXP(ZZIL*SQRT(USTAR*ZU))*ZU,EPSZT)
            ZT=MAX(EXP(ZZIL*SQRT(USTAR*Z0BASE))*Z0BASE,EPSZT)
!
            RZST=ZSLT/ZT
            RLOGT=LOG(RZST)
!
!----------------------------------------------------------------------
!***  1./MONIN-OBUKHOV LENGTH-SCALE
!----------------------------------------------------------------------
!
            RLMO=ELFC*AKHS*DTHV/USTAR**3
            ZETALU=ZSLU*RLMO
            ZETALT=ZSLT*RLMO
            ZETAU=ZU*RLMO
            ZETAT=ZT*RLMO
!
            ZETALU=MIN(MAX(ZETALU,ZTMIN2),ZTMAX2)
            ZETALT=MIN(MAX(ZETALT,ZTMIN2),ZTMAX2)
            ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)
            ZETAT=MIN(MAX(ZETAT,ZTMIN2/RZST),ZTMAX2/RZST)
!
!----------------------------------------------------------------------
!***  LAND FUNCTIONS
!----------------------------------------------------------------------
!
            RZ=(ZETAU-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZ=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
!
            RZ=(ZETALU-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSMZL=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
!
            SIMM=PSMZL-PSMZ+RLOGU
!
            RZ=(ZETAT-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZ=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)
!
            RZ=(ZETALT-ZTMIN2)/DZETA2
            K=INT(RZ)
            RDZT=RZ-REAL(K)
            K=MIN(K,KZTM2)
            K=MAX(K,0)
            PSHZL=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)
!
            SIMH=(PSHZL-PSHZ+RLOGT)*FH02
!----------------------------------------------------------------------
            USTARK=USTAR*VKARMAN
            AKMS=MAX(USTARK/SIMM,CXCHL)
            AKHS=MAX(USTARK/SIMH,CXCHL)
!
!----------------------------------------------------------------------
!***  BELJAARS CORRECTION FOR USTAR
!----------------------------------------------------------------------
!
            IF(DTHV<=0.)THEN                                           !zj
              WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)                !zj
            ELSE                                                       !zj
              WSTAR2=0.                                                !zj
            ENDIF                                                      !zj
!
            USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)
!
!----------------------------------------------------------------------
          ENDDO land_point_iteration
!----------------------------------------------------------------------
!
!       ENDIF  !  End of turbulant branch over land
!
!----------------------------------------------------------------------
!
      ENDIF  !  End of land/sea branch
!
!----------------------------------------------------------------------
!***  COUNTERGRADIENT FIX
!----------------------------------------------------------------------
!
!     HV=-AKHS*DTHV
!     IF(HV>0.)THEN
!       FCT=-10.*(BTGX)**(-1./3.)
!       CT=FCT*(HV/(PBLH*PBLH))**(2./3.)
!     ELSE
       CT=0.
!     ENDIF
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  THE FOLLOWING DIAGNOSTIC BLOCK PRODUCES 2-m and 10-m VALUES
!***  FOR TEMPERATURE, MOISTURE, AND WINDS.  IT IS DONE HERE SINCE
!***  THE VARIOUS QUANTITIES NEEDED FOR THE COMPUTATION ARE LOST
!***  UPON EXIT FROM THE ROTUINE.
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      WSTAR=SQRT(WSTAR2)/WWST
!
      UMFLX=AKMS*(ULOW -UZ0 )
      VMFLX=AKMS*(VLOW -VZ0 )
      HSFLX=AKHS*(THLOW-THZ0)
      HLFLX=AKHS*(QLOW -QZ0 )
!----------------------------------------------------------------------
!     IF(RIB>=RIC)THEN
!----------------------------------------------------------------------
!       IF(SEAMASK>0.5)THEN
!         AKMS10=MAX( VISC/10.,CXCHS)
!         AKHS02=MAX(TVISC/02.,CXCHS)
!         AKHS10=MAX(TVISC/10.,CXCHS)
!       ELSE
!         AKMS10=MAX( VISC/10.,CXCHL)
!         AKHS02=MAX(TVISC/02.,CXCHL)
!         AKHS10=MAX(TVISC/10.,CXCHL)
!       ENDIF
!----------------------------------------------------------------------
!     ELSE
!----------------------------------------------------------------------
        ZU10=ZU+10.
        ZT02=ZT+02.
        ZT10=ZT+10.
!
        RLNU10=LOG(ZU10/ZU)
        RLNT02=LOG(ZT02/ZT)
        RLNT10=LOG(ZT10/ZT)
!
        ZTAU10=ZU10*RLMO
        ZTAT02=ZT02*RLMO
        ZTAT10=ZT10*RLMO
!
!----------------------------------------------------------------------
!***  SEA
!----------------------------------------------------------------------
!
        IF(SEAMASK>0.5)THEN
!
!----------------------------------------------------------------------
          ZTAU10=MIN(MAX(ZTAU10,ZTMIN1),ZTMAX1)
          ZTAT02=MIN(MAX(ZTAT02,ZTMIN1),ZTMAX1)
          ZTAT10=MIN(MAX(ZTAT10,ZTMIN1),ZTMAX1)
!----------------------------------------------------------------------
          RZ=(ZTAU10-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSM10=(PSIM1(K+2)-PSIM1(K+1))*RDZT+PSIM1(K+1)
!
          SIMM10=PSM10-PSMZ+RLNU10
!
          RZ=(ZTAT02-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH02=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)
!
          SIMH02=(PSH02-PSHZ+RLNT02)*FH01
!
          RZ=(ZTAT10-ZTMIN1)/DZETA1
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH10=(PSIH1(K+2)-PSIH1(K+1))*RDZT+PSIH1(K+1)
!
          SIMH10=(PSH10-PSHZ+RLNT10)*FH01
!
          AKMS10=MAX(USTARK/SIMM10,CXCHS)
          AKHS02=MAX(USTARK/SIMH02,CXCHS)
          AKHS10=MAX(USTARK/SIMH10,CXCHS)
!
!----------------------------------------------------------------------
!***  LAND
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
          ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
          ZTAT02=MIN(MAX(ZTAT02,ZTMIN2),ZTMAX2)
          ZTAT10=MIN(MAX(ZTAT10,ZTMIN2),ZTMAX2)
!----------------------------------------------------------------------
          RZ=(ZTAU10-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
!
          SIMM10=PSM10-PSMZ+RLNU10
!
          RZ=(ZTAT02-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH02=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)
!
          SIMH02=(PSH02-PSHZ+RLNT02)*FH02
!
          RZ=(ZTAT10-ZTMIN2)/DZETA2
          K=INT(RZ)
          RDZT=RZ-REAL(K)
          K=MIN(K,KZTM2)
          K=MAX(K,0)
          PSH10=(PSIH2(K+2)-PSIH2(K+1))*RDZT+PSIH2(K+1)
!
          SIMH10=(PSH10-PSHZ+RLNT10)*FH02
!
          AKMS10=MAX(USTARK/SIMM10,CXCHL)
          AKHS02=MAX(USTARK/SIMH02,CXCHL)
          AKHS10=MAX(USTARK/SIMH10,CXCHL)
!----------------------------------------------------------------------
        ENDIF
!----------------------------------------------------------------------
!     ENDIF
!----------------------------------------------------------------------
      U10 =UMFLX/AKMS10+UZ0
      V10 =VMFLX/AKMS10+VZ0
      TH02=HSFLX/AKHS02+THZ0
!
!***  BE CERTAIN THAT THE 2-M THETA AND 10-M THETA ARE BRACKETED BY
!***  THE VALUES OF THZ0 AND THLOW.
!
      IF(THLOW>THZ0.AND.(TH02<THZ0.OR.TH02>THLOW).OR.                  &
         THLOW<THZ0.AND.(TH02>THZ0.OR.TH02<THLOW))THEN
           TH02=THZ0+2.*RDZ*(THLOW-THZ0)
      ENDIF
!
      TH10=HSFLX/AKHS10+THZ0
!
      IF(THLOW>THZ0.AND.(TH10<THZ0.OR.TH10>THLOW).OR.                  &
         THLOW<THZ0.AND.(TH10>THZ0.OR.TH10<THLOW))THEN
           TH10=THZ0+10.*RDZ*(THLOW-THZ0)
      ENDIF
!
      Q02 =HLFLX/AKHS02+QZ0
      Q10 =HLFLX/AKHS10+QZ0
      TERM1=-0.068283/TLOW
      PSHLTR=PSFC*EXP(TERM1)
!
!----------------------------------------------------------------------
!***  COMPUTE "EQUIVALENT" Z0 TO APPROXIMATE LOCAL SHELTER READINGS.
!----------------------------------------------------------------------
!
      U10E=U10
      V10E=V10
!
      IF(SEAMASK<0.5)THEN

!1st        ZUUZ=MIN(0.5*ZU,0.1)
!1st        ZU=MAX(0.1*ZU,ZUUZ)
!tst        ZUUZ=amin1(ZU*0.50,0.3)
!tst        ZU=amax1(ZU*0.3,ZUUZ)

        ZUUZ=AMIN1(ZU*0.50,0.18)
        ZU=AMAX1(ZU*0.35,ZUUZ)
!
        ZU10=ZU+10.
        RZSU=ZU10/ZU
        RLNU10=LOG(RZSU)

        ZETAU=ZU*RLMO
        ZTAU10=ZU10*RLMO

        ZTAU10=MIN(MAX(ZTAU10,ZTMIN2),ZTMAX2)
        ZETAU=MIN(MAX(ZETAU,ZTMIN2/RZSU),ZTMAX2/RZSU)

        RZ=(ZTAU10-ZTMIN2)/DZETA2
        K=INT(RZ)
        RDZT=RZ-REAL(K)
        K=MIN(K,KZTM2)
        K=MAX(K,0)
        PSM10=(PSIM2(K+2)-PSIM2(K+1))*RDZT+PSIM2(K+1)
        SIMM10=PSM10-PSMZ+RLNU10
        EKMS10=MAX(USTARK/SIMM10,CXCHL)

        U10E=UMFLX/EKMS10+UZ0
        V10E=VMFLX/EKMS10+VZ0

      ENDIF
!
!     U10=U10E
!     V10=V10E
!
!----------------------------------------------------------------------
!***  SET OTHER WRF DRIVER ARRAYS
!----------------------------------------------------------------------
!
      RLOW=PLOW/(R_D*TLOW)
      CHS=AKHS
      CHS2=AKHS02
      CQS2=AKHS02
      HFX=-RLOW*CP*HSFLX
      QFX=-RLOW*HLFLX*WETM
      FLX_LH=XLV*QFX
      FLHC=RLOW*CP*AKHS
      FLQC=RLOW*AKHS*WETM
!!!   QGH=PQ0/PSHLTR*EXP(A2S*(TSK-A3S)/(TSK-A4S))
      QGH=((1.-SEAMASK)*PQ0+SEAMASK*PQ0SEA)                            &
     &     /PLOW*EXP(A2S*(TLOW-A3S)/(TLOW-A4S))
      QGH=QGH/(1.-QGH)    !Convert to mixing ratio
      CPM=CP*(1.+0.8*QLOW)
!
!***  DO NOT COMPUTE QS OVER LAND POINTS HERE SINCE IT IS
!***  A PROGNOSTIC VARIABLE THERE.  IT IS OKAY TO USE IT
!***  AS A DIAGNOSTIC OVER WATER SINCE IT WILL CAUSE NO
!***  INTERFERENCE BEFORE BEING RECOMPUTED IN MYJPBL.
!
      IF(SEAMASK>0.5)THEN
          QS=PQ0SEA/PSFC                                      &
     &         *EXP(A2S*(TSK-A3S)/(TSK-A4S)) 
        QS=QS/(1.-QS)
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE SFCDIF
!
!----------------------------------------------------------------------
      SUBROUTINE MYJSFCINIT(LOWLYR,USTAR,Z0                            &
     &                     ,SEAMASK,XICE,IVGTYP,RESTART                &
     &                     ,ALLOWED_TO_READ                            &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: RESTART,ALLOWED_TO_READ
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IVGTYP
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: LOWLYR
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: SEAMASK,XICE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: USTAR,Z0
!
      REAL,DIMENSION(0:30) :: VZ0TBL
      REAL,DIMENSION(0:30) :: VZ0TBL_24
!
      INTEGER :: I,IDUM,IRECV,J,JDUM,K,ITF,JTF,KTF,MAXGBL_IVGTYP       &
     &,          MAXLOC_IVGTYP,MPI_COMM_COMP
!
!     INTEGER :: MPI_INTEGER,MPI_MAX
!
      REAL :: SM,X,ZETA1,ZETA2,ZRNG1,ZRNG2
!
      REAL :: PIHF=3.1415926/2.,EPS=1.E-6
!----------------------------------------------------------------------
      VZ0TBL=                                                          &
     &  (/0.,                                                          &
     &    2.653,0.826,0.563,1.089,0.854,0.856,0.035,0.238,0.065,0.076  &
     &   ,0.011,0.035,0.011,0.000,0.000,0.000,0.000,0.000,0.000,0.000  &
     &   ,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000/)

      VZ0TBL_24= (/0.,                                                 &
     &            1.00,  0.07,  0.07,  0.07,  0.07,  0.15,             &
     &            0.08,  0.03,  0.05,  0.86,  0.80,  0.85,             &
     &            2.65,  1.09,  0.80,  0.001, 0.04,  0.05,             &
     &            0.01,  0.04,  0.06,  0.05,  0.03,  0.001,            &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000 /)

!----------------------------------------------------------------------
!
      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
!
!
!***  FOR NOW, ASSUME SIGMA MODE FOR LOWEST MODEL LAYER
!
      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
!       USTAR(I,J)=EPSUST
      ENDDO
      ENDDO
!----------------------------------------------------------------------
#if (NMM_CORE == 1)
!
      IF(.NOT.RESTART)THEN
!       CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
!       MAXLOC_IVGTYP=MAXVAL(IVGTYP)
!       CALL MPI_ALLREDUCE(MAXLOC_IVGTYP,MAXGBL_IVGTYP,1,MPI_INTEGER   &
!    &,                    MPI_MAX,MPI_COMM_COMP,IRECV)
        MAXGBL_IVGTYP=MAXVAL(IVGTYP)
#ifdef DM_PARALLEL
        CALL WRF_DM_MAXVAL(MAXGBL_IVGTYP,IDUM,JDUM)
#endif
!
        IF (MAXGBL_IVGTYP<13) THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            SM=SEAMASK(I,J)-1.
            IF(SM+XICE(I,J)<0.5)THEN
              Z0(I,J)=VZ0TBL(IVGTYP(I,J))
            ENDIF
          ENDDO
          ENDDO
!
        ELSE
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            SM=SEAMASK(I,J)-1.
            IF(SM+XICE(I,J)<0.5)THEN
              Z0(I,J)=VZ0TBL_24(IVGTYP(I,J))
            ENDIF
          ENDDO
          ENDDO
!
        ENDIF ! Vegtype check
!
      ENDIF ! Restart check

#endif
!----------------------------------------------------------------------
      IF(.NOT.RESTART)THEN
        DO J=JTS,JTE
        DO I=ITS,ITF
          USTAR(I,J)=0.1
        ENDDO
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
!***  COMPUTE SURFACE LAYER INTEGRAL FUNCTIONS
!
!----------------------------------------------------------------------
      FH01=1.
      FH02=1.
!
!      ZTMIN1=-10.0
!      ZTMAX1=2.0
!      ZTMIN2=-10.0
!      ZTMAX2=2.0
      ZTMIN1=-5.0
      ZTMAX1=1.0
      ZTMIN2=-5.0
      ZTMAX2=1.0
!
      ZRNG1=ZTMAX1-ZTMIN1
      ZRNG2=ZTMAX2-ZTMIN2
!
      DZETA1=ZRNG1/(KZTM-1)
      DZETA2=ZRNG2/(KZTM-1)
!
!----------------------------------------------------------------------
!***  FUNCTION DEFINITION LOOP
!----------------------------------------------------------------------
!
      ZETA1=ZTMIN1
      ZETA2=ZTMIN2
!
      DO K=1,KZTM
!
!----------------------------------------------------------------------
!***  UNSTABLE RANGE
!----------------------------------------------------------------------
!
        IF(ZETA1<0.)THEN
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS
!----------------------------------------------------------------------
          X=SQRT(SQRT(1.-16.*ZETA1))
!
          PSIM1(K)=-2.*LOG((X+1.)/2.)-LOG((X*X+1.)/2.)+2.*ATAN(X)-PIHF
          PSIH1(K)=-2.*LOG((X*X+1.)/2.)
!
!----------------------------------------------------------------------
!***  STABLE RANGE
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS
!----------------------------------------------------------------------
!
!         PSIM1(K)=5.*ZETA1
!         PSIH1(K)=5.*ZETA1
!----------------------------------------------------------------------
!***   HOLTSLAG AND DE BRUIN 1988
!----------------------------------------------------------------------
!
          PSIM1(K)=0.7*ZETA1+0.75*ZETA1*(6.-0.35*ZETA1)*EXP(-0.35*ZETA1)
          PSIH1(K)=0.7*ZETA1+0.75*ZETA1*(6.-0.35*ZETA1)*EXP(-0.35*ZETA1)
!----------------------------------------------------------------------
!
        ENDIF
!
!----------------------------------------------------------------------
!***  UNSTABLE RANGE
!----------------------------------------------------------------------
!
        IF(ZETA2<0.)THEN
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS
!----------------------------------------------------------------------
!
          X=SQRT(SQRT(1.-16.*ZETA2))
!
          PSIM2(K)=-2.*LOG((X+1.)/2.)-LOG((X*X+1.)/2.)+2.*ATAN(X)-PIHF
          PSIH2(K)=-2.*LOG((X*X+1.)/2.)
!----------------------------------------------------------------------
!***  STABLE RANGE
!----------------------------------------------------------------------
!
        ELSE
!
!----------------------------------------------------------------------
!***  PAULSON 1970 FUNCTIONS
!----------------------------------------------------------------------
!
!         PSIM2(K)=5.*ZETA2
!         PSIH2(K)=5.*ZETA2
!
!----------------------------------------------------------------------
!***  HOLTSLAG AND DE BRUIN 1988
!----------------------------------------------------------------------
!
          PSIM2(K)=0.7*ZETA2+0.75*ZETA2*(6.-0.35*ZETA2)*EXP(-0.35*ZETA2)
          PSIH2(K)=0.7*ZETA2+0.75*ZETA2*(6.-0.35*ZETA2)*EXP(-0.35*ZETA2)
!----------------------------------------------------------------------
!
        ENDIF
!
!----------------------------------------------------------------------
        IF(K==KZTM)THEN
          ZTMAX1=ZETA1
          ZTMAX2=ZETA2
        ENDIF
!
        ZETA1=ZETA1+DZETA1
        ZETA2=ZETA2+DZETA2
!----------------------------------------------------------------------
      ENDDO
!----------------------------------------------------------------------
      ZTMAX1=ZTMAX1-EPS
      ZTMAX2=ZTMAX2-EPS
!----------------------------------------------------------------------
!
      END SUBROUTINE MYJSFCINIT
!
!----------------------------------------------------------------------
!
      END MODULE MODULE_SF_MYJSFC
!
!----------------------------------------------------------------------

!
!NCEP_MESO:MODEL_LAYER: RADIATION
!
!***********************************************************************
      SUBROUTINE RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON    &
     &                    ,NRADS,NRADL                                  &
     &                    ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT   &
     &                    ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR          &
     &                    ,F_ICE,F_RAIN                                 &
!    &                    ,SM,HBM2,LMH,ZERO_3D,WATER,N_MOIST,RESTRT     &
     &                    ,SM,HBM2,LMH,ZERO_3D,N_MOIST,RESTRT           &
     &                    ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT               &
     &                    ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN         &
     &                    ,CFRACL,CFRACM,CFRACH,SIGT4                   &
     &                    ,ACFRST,NCFRST,ACFRCV,NCFRCV                  &
     &                    ,CUPPT,VEGFRA,SNOW,HTOP,HBOT                  &
     &                    ,CONFIG_FLAGS                                 &
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    RADIATION   RADIATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 2002-06-04       
!     
! ABSTRACT:
!     RADIATION SERVES AS THE INTERFACE BETWEEN THE NCEP NONHYDROSTATIC
!     MESOSCALE MODEL AND THE WRF RADIATION DRIVER.
!     
! PROGRAM HISTORY LOG:
!   02-06-04  BLACK      - ORIGINATOR
!   02-09-09  WOLFE      - CONVERTING TO GLOBAL INDEXING
!     
! USAGE: CALL RADIATION FROM SOLVE_RUNSTREAM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!-----------------------------------------------------------------------
      USE MODULE_CONFIGURE
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_MPP
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH
      USE MODULE_RADIATION_DRIVER
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,IHRST,JULDAY,JULYR                          &
     &                     ,N_MOIST,NPHS,NRADL,NRADS,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: NCFRCV,NCFRST
!
      REAL,INTENT(IN) :: DT,PDTOP,PT
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CUPPT
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ALBEDO              &
     &                                             ,EPSR,GLAT,GLON      &
     &                                             ,HBM2,PD,RES,SM      &
     &                                             ,SNOW,THS,VEGFRA
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &
     &                                                     ,F_RAIN
!
!     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST)                   &
!    &                                          ,INTENT(INOUT) :: WATER
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM,Q,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACFRCV,ACFRST    &
     &                                                ,HBOT,HTOP        &
     &                                                ,RLWIN,RLWTOA     &
     &                                                ,RSWIN,RSWOUT     &
     &                                                ,RSWTOA
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CFRACH,CFRACL      &
     &                                              ,CFRACM,CZMEAN      &
     &                                              ,SIGT4              &
     &                                              ,TOTLWDN,TOTSWDN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RLWTT    &
     &                                                        ,RSWTT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT        &
     &                                                     ,ZERO_3D
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,ICLOUD,IENDX,II,J,JDAY,JMONTH,K,KMNTH,LMHIJ,NRAD
!
      INTEGER,DIMENSION(3) :: IDAT
      INTEGER,DIMENSION(12) :: MONTH=(/31,28,31,30,31,30,31,31          &
     &                                ,30,31,30,31/)
!
      INTEGER, DIMENSION(1) :: I_START,I_END,J_START,J_END
!
      REAL :: CAPA,DAYI,DPL,FICE,FRAIN,GMT,HOUR,PDSL,PLYR,PSFC          &
     &       ,QI,QR,QW,RADT,TIMES,WC
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: GLW,REXNSFC,SWNETDN            &
     &                                  ,TOT,TSFC,XLAND,XLAT,XLON
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RR,T8W,THRATEN         &
     &                                          ,THRATENLW,THRATENSW    &
     &                                          ,TH_PHY,T_PHY
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST) :: WATER
!
! not used by NMM but needed for driver
      REAL, DIMENSION( ims:ime, jms:jme )             :: SWDOWN
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CZEN
!
      LOGICAL :: WARM_RAIN
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!*****
!***** NOTE: THIS IS HARDWIRED FOR CALLS TO LONGWAVE AND SHORTWAVE
!*****       AT EQUAL INTERVALS
!*****
      NRAD=NRADS
      RADT=DT*NRADS/60.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      MYIS1=MAX(IDS+1,ITS)
      MYIE1=MIN(IDE-1,ITE)
      MYJS2=MAX(JDS+2,JTS)
      MYJE2=MIN(JDE-2,JTE)
!
      I_START(1)=MYIS1
      I_END(1)  =MYIE1
      J_START(1)=MYJS2
      J_END(1)  =MYJE2
!-----------------------------------------------------------------------
      CAPA=R_D/CP
!-----------------------------------------------------------------------
      DO J=MYJS,MYJE
      DO K=KTS,KTE
      DO I=MYIS,MYIE
        WATER(I,K,J,P_QS)=0.
        WATER(I,K,J,P_QG)=0.
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        XLAT(I,J)=GLAT(I,J)/DEGRAD
        XLON(I,J)=GLON(I,J)/DEGRAD
        XLAND(I,J)=SM(I,J)+1.
        PSFC=PD(I,J)+PDTOP+PT
        REXNSFC(I,J)=(PSFC*1.E-5)**CAPA
        TSFC(I,J)=THS(I,J)*REXNSFC(I,J)
        T8W(I,1,J)=TSFC(I,J)
        P8W(I,KTS,J)=ETA1(KTS)*PDTOP+ETA2(KTS)*PDSL+PT
!
!***  FILL THE SINGLE-COLUMN INPUT
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          T_PHY(I,K,J)=TL(K)
          WATER(I,K,J,P_QV)=QL(K)/(1.-QL(K))
          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
!!!  &                 *ALOG(P8W(I,KFLIP,J)/P8W(I,KFLIP+1,J))/G         &
!!!  &                 *ALOG(PINT(I,K+1,J)/PINT(I,K,J))/G               &
!
          THRATEN(I,K,J)=0.
          THRATENLW(I,K,J)=0.
          THRATENSW(I,K,J)=0.
!
!***  DECOMPOSE CLOUDS TO CLOUD LIQUID, RAIN, AND CLOUD ICE + SNOW.
!
          WC=CWM(I,K,J)
          QI=0.
          QR=0.
          QW=0.
          FICE=F_ICE(I,K,J)
          FRAIN=F_RAIN(I,K,J)
!
          IF(FICE.GE.1.)THEN
            QI=WC
          ELSEIF(FICE.LE.0.)THEN
            QW=WC
          ELSE
            QI=FICE*WC
            QW=WC-QI
          ENDIF
!
          IF(QW.GT.0..AND.FRAIN.GT.0.)THEN
            IF(FRAIN.GE.1.)THEN
              QR=QW
              QW=0.
            ELSE
              QR=FRAIN*QW
              QW=QW-QR
            ENDIF
          ENDIF
!
          WATER(I,K,J,P_QC)=QW
          WATER(I,K,J,P_QR)=QR
          WATER(I,K,J,P_QI)=QI
        ENDDO
!
        DO K=KTS+1,KTE
          T8W(I,K,J)=0.5*(TL(K-1)+TL(K))
        ENDDO
        T8W(I,KTE+1,J)=-1.E20
!
      ENDDO
      ENDDO
!
      ICLOUD=999
!
      GMT=REAL(IHRST)
!
!-----------------------------------------------------------------------
!
!***  CALL THE INNER DRIVER.
!
!-----------------------------------------------------------------------
!
      GLW = 0.
      CFRACH = 0.
      CFRACL = 0.
      CFRACM = 0.
      CZMEAN = 0.
      SIGT4  = 0.
      TOTLWDN =0.
      TOTSWDN =0.
      SWNETDN =0.
      SWDOWN =0.
      CALL RADIATION_DRIVER(NTSD,DT                                     &
     &                     ,ZERO_3D,ZERO_3D                             & !aer_dry, aer_water
     &                     ,THRATENLW,THRATENSW,THRATEN                 &
     &                     ,GLW,SWNETDN,SWDOWN                          &
     &                     ,XLAT,XLON,ALBEDO,ZERO_3D,EPSR               &
     &                     ,RR,WATER,N_MOIST                            &
     &                     ,P8W,P_PHY,ZERO_3D,PI_PHY,DZ,T_PHY,T8W       &
     &                     ,GMT,JULDAY,CONFIG_FLAGS,RADT                &
     &                     ,NRAD                                        & !Modified
     &                     ,ICLOUD,ZERO_3D,ZERO_3D,WARM_RAIN            & 
     &                     ,XLAND,TSFC,HTOP,HBOT,CUPPT,VEGFRA,SNOW      & 
     &                     ,F_ICE,F_RAIN                                & 
     &                     ,JULYR                                       & 
     &                     ,NPHS                                        &
     &                     ,TOTSWDN ,TOTLWDN                            &
     &                     ,RSWTOA ,RLWTOA                              &
     &                     ,CZMEAN ,CFRACL                              &
     &                     ,CFRACM ,CFRACH                              &
     &                     ,ACFRST ,NCFRST                              &
     &                     ,ACFRCV ,NCFRCV                              &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,I_START,I_END,J_START,J_END,KTS,KTE,1       )
!
!-----------------------------------------------------------------------
!
!***  UPDATE FLUXES AND TEMPERATURE TENDENCIES.
!
!-----------------------------------------------------------------------
!***  SHORTWAVE
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NRADS).EQ.0)THEN
!-----------------------------------------------------------------------
!
        IF(CONFIG_FLAGS%RA_SW_PHYSICS.NE.GFDLSWSCHEME)THEN
!
!***  COMPUTE CZMEAN FOR NON-GFDL SHORTWAVE
!
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            CZMEAN(I,J)=0.
            TOT(I,J)=0.
          ENDDO
          ENDDO
!
          CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)
          IDAT(1)=JMONTH
          IDAT(2)=JDAY
          IDAT(3)=JULYR
!
          DO II=0,NRADS,NPHS
            TIMES=NTSD*DT+II*DT
            CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN       &
     &                 ,MYIS,MYIE,MYJS,MYJE                             &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
              IF(CZEN(I,J).GT.0.)THEN
                CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
                TOT(I,J)=TOT(I,J)+1.
              ENDIF
            ENDDO
            ENDDO
!
          ENDDO
!
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            IF(TOT(I,J).GT.0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
          ENDDO
          ENDDO
!
!***  COMPUTE TOTAL SFC SHORTWAVE DOWN FOR NON-GFDL SCHEMES
!
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
!
            IF(HBM2(I,J).GT.0.5)THEN
              TOTSWDN(I,J)=SWNETDN(I,J)/(1.-ALBEDO(I,J))  
            ENDIF
!
          ENDDO
          ENDDO
        ENDIF   !End non-GFDL block
!
        DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2).EQ.0.AND.ITE+1.EQ.IDE)IENDX=IENDX-1
        DO I=MYIS1,IENDX
!
          RSWIN(I,J)=TOTSWDN(I,J)
          RSWOUT(I,J)=TOTSWDN(I,J)-SWNETDN(I,J)
!
          DO K=KTS,KTE
            RSWTT(I,K,J)=THRATENSW(I,K,J)*PI_PHY(I,K,J)
          ENDDO
!
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  LONGWAVE
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADL).EQ.0)THEN
!
        DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2).EQ.0.AND.ITE+1.EQ.IDE)IENDX=IENDX-1
        DO I=MYIS1,IENDX
!
          IF(HBM2(I,J).GT.0.5)THEN
            LMHIJ=KTE+1-LMH(I,J)
            SIGT4(I,J)=STBOLT*T(I,LMHIJ,J)**4
            DO K=KTS,KTE
              RLWTT(I,K,J)=THRATENLW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
            TOTLWDN(I,J)=GLW(I,J)
            RLWIN(I,J)=TOTLWDN(I,J)
          ENDIF
!
        ENDDO
        ENDDO
!
      ENDIF
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE RADIATION
!
!-----------------------------------------------------------------------

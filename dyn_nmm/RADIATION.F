!
!NCEP_MESO:MODEL_LAYER: RADIATION
!
!***********************************************************************
      SUBROUTINE RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON    &
     &                    ,NRADS,NRADL                                  &
     &                    ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT   &
     &                    ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR          &
!    &                    ,SM,HBM2,LMH,ZERO_3D,N_MOIST,LTOP,RESTRT      &
     &                    ,SM,HBM2,LMH,ZERO_3D,WATER,N_MOIST,LTOP3,RESTRT     &
     &                    ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT               &
     &                    ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN         &
     &                    ,CFRACL,CFRACM,CFRACH,SIGT4                   &
     &                    ,ACFRST,NCFRST,ACFRCV,NCFRCV                  &
     &                    ,CUPPT,VEGFRC,SNOW,HTOP,HBOT                  &
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
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC
      USE module_model_constants
      USE MODULE_MPP
      USE MODULE_RA_GFDLETA
      USE module_radiation_driver
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
!     INTEGER,DIMENSION(3),INTENT(IN) :: LTOP
      INTEGER,DIMENSION(3),INTENT(IN) :: LTOP3
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
     &                                             ,SNOW,THS,VEGFRC
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
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST), INTENT(INOUT) :: WATER
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
      INTEGER :: I,ICLOUD,J,K,KMNTH,LMHIJ,NRAD
!
      INTEGER,DIMENSION(12) :: MONTH=(/31,28,31,30,31,30,31,31          &
     &                                ,30,31,30,31/)
      INTEGER, DIMENSION(1) :: I_START,I_END,J_START,J_END
!
      REAL :: CAPA,DPL,GMT,PDSL,PLYR,PSFC,RADT
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: GLW,REXNSFC,SWNETDN,TSFC       &
     &                                  ,XLAND,XLAT,XLON,ZERO_2D
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RR,THRATEN             &
     &                                          ,THRATENLW,THRATENSW    &
     &                                          ,TH_PHY,T_PHY
!
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      MYIS1=MAX(IDS+1,ITS)
      MYIE1=MIN(IDE-1,ITE)
      MYJS2=MAX(JDS+2,JTS)
      MYJE2=MIN(JDE-2,JTE)
!
      I_START(1)  = MYIS1
      I_END(1)    = MYIE1
      J_START(1)  = MYJS2
      J_END(1)    = MYJE2

!-----------------------------------------------------------------------
      CAPA=R_D/CP
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
          WATER(I,K,J,P_QC)=CWM(I,K,J)
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
        ENDDO
!
      ENDDO
      ENDDO
!
      RADT=999.
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
      CFRACH = 0.
      CFRACL = 0.
      CFRACM = 0.
      CZMEAN = 0.
      SIGT4  = 0.
      TOTLWDN =0.
      TOTSWDN =0.
      SWNETDN =0.
      CALL RADIATION_DRIVER(NTSD,DT                                     &
     &                     ,THRATENLW,THRATENSW,THRATEN                 &
     &                     ,GLW,SWNETDN                                 &
     &                     ,ZERO_2D                                     & !SWDOWN
     &                     ,XLAT,XLON,ALBEDO,ZERO_3D,EPSR               &
     &                     ,RR,WATER,N_MOIST                            &
     &                     ,P8W,P_PHY,ZERO_3D,PI_PHY,DZ,T_PHY,ZERO_3D   &
     &                     ,GMT,JULDAY,CONFIG_FLAGS,RADT                &
     &                     ,NRAD                                        & !Modified
     &                     ,ICLOUD,ZERO_3D,ZERO_3D,WARM_RAIN            & 
     &                     ,XLAND,TSFC,HTOP,HBOT,CUPPT,VEGFRC,SNOW      & 
     &                     ,JULYR                                       & 
!    &                     ,LTOP=LTOP, NPHS=NPHS                        & 
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
      IF(MOD(NTSD,NRADS).EQ.0)THEN
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
!
          IF(HBM2(I,J).GT.0.5)THEN
!
            RSWIN(I,J)=TOTSWDN(I,J)
            RSWOUT(I,J)=TOTSWDN(I,J)-SWNETDN(I,J)
!
            DO K=KTS,KTE
              RSWTT(I,K,J)=THRATENSW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
!
          ENDIF
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
        DO I=MYIS1,MYIE1
!
          IF(HBM2(I,J).GT.0.5)THEN
            LMHIJ=KTE+1-LMH(I,J)
            SIGT4(I,J)=STBOLT*T(I,LMHIJ,J)**4
            DO K=KTS,KTE
              RLWTT(I,K,J)=THRATENLW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
            RLWIN(I,J)=TOTLWDN(I,J)
          ENDIF
!
        ENDDO
        ENDDO
!
      ENDIF
!-----------------------------------------------------------------------
!
      END SUBROUTINE RADIATION
!
!-----------------------------------------------------------------------

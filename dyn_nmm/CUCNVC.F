!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!***********************************************************************
      SUBROUTINE CUCNVC(NTSD,DT,NCNVC,GPS,RESTRT                        &
     &                 ,CLDEFI,LMH,WATER,N_MOIST,ENSDIM                 &
     &                 ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2               &
     &                 ,PDTOP,PT,PD,RES,PINT,T,Q,TCUCN                  &
     &                 ,PREC,ACPREC,CUPREC,CUPPT                        &
     &                 ,SM,HBM2,LPBL,HBOT,HTOP,CNVBOT,CNVTOP            &
     &                 ,AVCNVC,ACUTIM,ZERO_3D                           &
     &                 ,CONFIG_FLAGS                                    &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CUCNVC      CONVECTIVE PRECIPITATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-21       
!     
! ABSTRACT:
!     CUCVNC DRIVES THE WRF CONVECTION SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-21  BLACK      - ORIGINATOR
!     
! USAGE: CALL CUCNVC FROM SOLVE_RUNSTREAM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!-----------------------------------------------------------------------
      USE MODULE_CONFIGURE
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV
      USE module_model_constants
      USE MODULE_MPP
      USE MODULE_CU_BMJ
      USE module_cumulus_driver
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                     &
                           ,N_MOIST,NCNVC,NTSD,ENSDIM
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH,LPBL
!
      REAL,INTENT(IN) :: DT,GPS,PDTOP,PT
!
      REAL,INTENT(INOUT) :: ACUTIM,AVCNVC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME  ),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,CLDEFI    &
     &                                                ,CNVBOT,CNVTOP    &
     &                                                ,CUPPT,CUPREC     &
     &                                                ,HBOT,HTOP,PREC
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: Q,T      &
     &                                                        ,TCUCN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST), INTENT(INOUT) :: WATER
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: I,ICLDCK,J,K
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LBOT,LOWLYR,LTOP,KPBL
      INTEGER, DIMENSION(1) :: I_START,I_END,J_START,J_END
      REAL, DIMENSION(IMS:IME,JMS:JME)   :: NCA
!
      REAL :: CAPA,DPL,DQDT,DTCNVC,DTDT,PCPCOL,PDSL,PLYR,RDTCNVC
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,RAINC,RAINCV,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCCUTEN,RQRCUTEN      &
     &                                          ,RQVCUTEN,RR,RTHCUTEN   &
     &                                          ,T_PHY,TH_PHY
!
      REAL,DIMENSION(IMS:IME,JMS:JME,ENSDIM) :: ZERO_GD
      REAL,DIMENSION(IMS:IME,JMS:JME)        :: ZERO_2D
!
      LOGICAL :: RESTART,WARM_RAIN
      LOGICAL,DIMENSION(IMS:IME,JMS:JME) :: CU_ACT_FLAG
!
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
!
!***  INITIALIZE
!
      RESTART=RESTRT
!
      IF(NTSD.EQ.0)THEN
        RESTART=.FALSE.
        CALL BMJINIT(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN                &
     &              ,CLDEFI,LOWLYR,CP,R_D,RESTART                       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ENDIF
!-----------------------------------------------------------------------
!
      AVCNVC=AVCNVC+1.
      ACUTIM=ACUTIM+1.
!
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      CAPA=R_D/CP
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        RAINCV(I,J)=0.
        RAINC(I,J)=0.
        P8W(I,KTS,J)=PD(I,J)+PDTOP+PT
        LOWLYR(I,J)=KTE+1-LMH(I,J)
        XLAND(I,J)=SM(I,J)+1.
        NCA(I,J)=0
        KPBL(I,J)=KTE-LPBL(I,J)+1
        ZERO_2D(I,J)=0
!
!***  FILL THE INPUT ARRAYS.
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)
          WATER(I,K,J,P_QV)=QL(K)/(1.-QL(K))
          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
!!!       P8W(I,KFLIP,J)=PINT(I,K+1,J)
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                          &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                      &
     &                 /(P_PHY(I,K,J)*G)
!
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
        ENDDO
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  SINGLE-COLUMN CONVECTION
!
!-----------------------------------------------------------------------
      NCA=0
!
      CALL CUMULUS_DRIVER(NTSD,DT,GPS,N_MOIST                           &
     &                   ,RTHCUTEN,RQVCUTEN,ZERO_3D,ZERO_3D             &
     &                   ,ZERO_3D,ZERO_3D,RAINC,RAINCV,NCA              &
     &                   ,ZERO_3D,ZERO_3D,ZERO_3D                       &
     &                   ,ZERO_3D,ZERO_3D                               &
     &                   ,ZERO_3D,ZERO_3D,TH_PHY,T_PHY,ZERO_3D,WATER    &
     &                   ,DZ,P8W,P_PHY,PI_PHY                           &
     &                   ,CONFIG_FLAGS,ZERO_3D,RR,NCNVC                 &
     &                   ,CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,WARM_RAIN     &
     &                   ,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D       &
     &                   ,ZERO_2D,ZERO_2D,ZERO_2D                       &
     &                   ,HTOP,HBOT,KPBL                                &
     &                   ,ZERO_2D,ZERO_GD,ZERO_GD,ZERO_2D               & ! for Grell-Devenyi
     &                   ,ensdim,1,1,1,1                                &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,I_START,I_END,J_START,J_END,KTS,KTE,1         )
!    &                   ,HTOP,HBOT                                     )  !optional arguments
!    &                   ,LBOT=LBOT,LTOP=LTOP,LPBL=LPBL                 )  !optional arguments
!
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        IF(HBM2(I,J).GT.0.5)THEN
!
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, AND HEATING.
!***  THE FLIP IS BECAUSE RTHCUTEN AND RQVCUTEN REACH THIS POINT
!***  WITH LAYER 1 AT THE BOTTOM.
!
          DO K=KTS,KTE
!
!***  RQVCUTEN IN BMJDRV IS THE MIXING RATIO TENDENCY,
!***  SO RETRIEVE DQDT BY CONVERTING TO SPECIFIC HUMIDITY.
!
            DQDT=RQVCUTEN(I,K,J)/(1.+WATER(I,K,J,P_QV))**2
!
!***  RTHCUTEN IN BMJDRV IS DTDT OVER PI,
!
            DTDT=RTHCUTEN(I,K,J)*PI_PHY(I,K,J)
            T(I,K,J)=T(I,K,J)+DTDT*DTCNVC
            Q(I,K,J)=Q(I,K,J)+DQDT*DTCNVC
            TCUCN(I,K,J)=TCUCN(I,K,J)+DTDT
          ENDDO
!
!***  UPDATE PRECIPITATION
!
          PCPCOL=RAINCV(I,J)*1.E-3*NCNVC
          PREC(I,J)=PREC(I,J)+PCPCOL
          ACPREC(I,J)=ACPREC(I,J)+PCPCOL
          CUPREC(I,J)=CUPREC(I,J)+PCPCOL
          CUPPT(I,J)=CUPPT(I,J)+PCPCOL
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION.
!
         IF(LTOP(I,J).GT.0.AND.LTOP(I,J).LT.KTE+1)THEN
            HTOP(I,J)=MAX(REAL(LTOP(I,J)),HTOP(I,J))
            CNVTOP(I,J)=MAX(REAL(LTOP(I,J)),CNVTOP(I,J))
          ENDIF
          IF(LBOT(I,J).GT.0.AND.LBOT(I,J).LT.KTE+1)THEN
            HBOT(I,J)=MIN(REAL(LBOT(I,J)),HBOT(I,J))
            CNVBOT(I,J)=MIN(REAL(LBOT(I,J)),CNVBOT(I,J))
          ENDIF
        ENDIF
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------

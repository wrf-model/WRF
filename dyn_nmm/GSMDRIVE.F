!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!***********************************************************************
!     SUBROUTINE GSMDRIVE(NTSD,DT,NPHS,WATER,N_MOIST                    &
      SUBROUTINE GSMDRIVE(NTSD,DT,NPHS,N_MOIST                          &
     &                   ,DX,DY,LMH,SM,HBM2,FIS                         &
     &                   ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2             &
     &                   ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN            &
     &                   ,F_ICE,F_RAIN,F_RIMEF,SR                       &
     &                   ,PREC,ACPREC,ZERO_3D                           &
     &                   ,CONFIG_FLAGS                                  &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    GSMDRIVE    MICROPHYSICS OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-26       
!     
! ABSTRACT:
!     GSMDRIVE DRIVES THE MICROPHYSICS SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-26  BLACK      - ORIGINATOR
!     
! USAGE: CALL GSMDRIVE FROM SOLVE_RUNSTREAM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!-----------------------------------------------------------------------
      USE MODULE_CONFIGURE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC
      USE MODULE_MPP
      USE MODULE_MP_ETANEW
      USE MODULE_MICROPHYSICS_DRIVER
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NPHS,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DX,DY,PDTOP,PT
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,PREC
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM,Q,T  &
     &                                                        ,TRAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &
     &                                                        ,F_RAIN   &
     &                                                        ,F_RIMEF
!
!     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST)                   &
!    &                                          ,INTENT(INOUT) :: WATER
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SR
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!***
!***  LOCAL VARIABLES
!***
      INTEGER :: I,IENDX,J,K
!
      INTEGER,DIMENSION(1) :: I_START,I_END,J_START,J_END
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LOWLYR
!
      REAL :: CAPA,DPL,DTPHS,PCPCOL,PDSL,PLYR,RDTPHS,RG,TNEW
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,RAINNCV,XLAND      &
     &                                  ,ZERO_2D
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RR,T_PHY,TH_PHY
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST) :: WATER
!
      LOGICAL :: E_BDY,WARM_RAIN
!
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
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      CAPA=R_D/CP
      RG=1./G
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS
!
!-----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
!
        PDSL=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        LOWLYR(I,J)=KTE+1-LMH(I,J)
        XLAND(I,J)=SM(I,J)+1.
        ZERO_2D(I,J)=0.
!
!***  FILL THE SINGLE-COLUMN INPUT
!
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
          TL(K)=T(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)
          WATER(I,K,J,P_QV)=QL(K)/(1.-QL(K))  !Convert to mixing ratio 
          WATER(I,K,J,P_QC)=CWM(I,K,J)
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          TH_PHY(I,K,J)=TL(K)/PI_PHY(I,K,J)
!!!       P8W(I,KFLIP,J)=PINT(I,K+1,J)
          P8W(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          DZ(I,K,J)=DPL*RG/RR(I,K,J)
        ENDDO
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  CALL MICROPHYSICS
!
!-----------------------------------------------------------------------
!
      CALL MICROPHYSICS_DRIVER(TH_PHY,WATER,ZERO_3D,ZERO_3D             &
     &                        ,RR,PI_PHY,P_PHY,ZERO_2D,RAINNCV          &
     &                        ,ZERO_3D,ZERO_2D,DZ,P8W,DTPHS,DX,DY       &
     &                        ,CONFIG_FLAGS,0,N_MOIST,WARM_RAIN         &
     &                        ,XLAND,NTSD-1                             &
     &                        ,F_ICE,F_RAIN,F_RIMEF                     &
     &                        ,LOWLYR,SR                                &
     &                        ,IDS,IDE,JDS,JDE,KDS,KDE                  &
     &                        ,IMS,IME,JMS,JME,KMS,KME                  &
     &                        ,I_START,I_END,J_START,J_END,KTS,KTE,1)
!
!-----------------------------------------------------------------------
      E_BDY=(ITE>=IDE)
!
      DO J=MYJS2,MYJE2
      IENDX=MYIE1
      IF(E_BDY.AND.MOD(J,2).EQ.0)IENDX=IENDX-1
      DO I=MYIS1,IENDX
!
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD WATER, AND HEATING.
!
        DO K=KTS,KTE
          TNEW=TH_PHY(I,K,J)*PI_PHY(I,K,J)
          TRAIN(I,K,J)=(TNEW-T(I,K,J))*RDTPHS
          T(I,K,J)=TNEW
          Q(I,K,J)=WATER(I,K,J,P_QV)/(1.+WATER(I,K,J,P_QV)) !To s.h.
          CWM(I,K,J)=WATER(I,K,J,P_QC)
        ENDDO
!
!***  UPDATE PRECIPITATION
!
        PCPCOL=RAINNCV(I,J)*1.E-3
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
!
      ENDDO
      ENDDO
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-------------------------------------------------------------------
!
      END SUBROUTINE GSMDRIVE
!
!-------------------------------------------------------------------

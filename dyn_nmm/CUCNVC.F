!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!***********************************************************************
      SUBROUTINE CUCNVC(grid,NTSD,DT,NCNVC,GPS,RESTRT,HYDRO             &
!    &                 ,CLDEFI,LMH,WATER,N_MOIST,ENSDIM                 &
     &                 ,CLDEFI,LMH,N_MOIST,ENSDIM                       &
     &                 ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2               &
     &                 ,PDTOP,PT,PD,RES,PINT,T,Q,TCUCN                  &
     &                 ,OMGALF,U,V,VTM,WINT,Z,FIS,W0AVG                 &
     &                 ,PREC,ACPREC,CUPREC,CUPPT                        &
     &                 ,SM,HBM2,LPBL,HBOT,HTOP,CNVBOT,CNVTOP            &
     &                 ,AVCNVC,ACUTIM,ZERO_3D,IHE,IHW                   &
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
      USE MODULE_DOMAIN
      USE MODULE_CONFIGURE
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_MPP
      USE MODULE_CU_BMJ
      USE MODULE_CUMULUS_DRIVER
      USE WRF_ESMF_MOD
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      TYPE(DOMAIN)       :: GRID
!
      INTEGER,INTENT(IN) :: ENSDIM                                      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                     &
                           ,N_MOIST,NCNVC,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW
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
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,CLDEFI    &
     &                                                ,CNVBOT,CNVTOP    &
     &                                                ,CUPPT,CUPREC     &
     &                                                ,HBOT,HTOP,PREC
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: Q,T      &
     &                                                        ,TCUCN    &
     &                                                        ,W0AVG    &
     &                                                        ,WINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: OMGALF      &
     &                                                     ,PINT,U,V    &
     &                                                     ,VTM,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
!
!     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST)                   &
!    &                                           ,INTENT(INOUT) :: WATER
!
      LOGICAL,INTENT(IN) :: HYDRO,RESTRT
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!***
!***  LOCAL VARIABLES
!***
      TYPE( ESMF_Time ) CurrTime, PrevRingTime
      INTEGER :: rc
!
      INTEGER :: I,ICLDCK,IENDX,J,K,MNTO,NCUBOT,NCUTOP,NSTEP_CNV        &
     &          ,N_TIMSTPS_OUTPUT
!
      INTEGER, DIMENSION(1) :: I_START,I_END,J_START,J_END
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LBOT,LOWLYR,LTOP
!
      REAL :: CAPA,CF_HI,DPL,DQDT,DTCNVC,DTDT,G_INV,PCPCOL,PDSL,PLYR    &
     &       ,RDTCNVC,RWMSK,WMSK
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,NCA,RAINC,RAINCV   &
     &                                  ,SFCZ,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME) :: WMID
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCCUTEN,RQRCUTEN      &
     &                                          ,RQVCUTEN,RR,RTHCUTEN   &
     &                                          ,T_PHY,TH_PHY           &
     &                                          ,U_PHY,V_PHY
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST) :: WATER
!
      REAL,DIMENSION(IMS:IME,JMS:JME)        :: ZERO_2D
      REAL,DIMENSION(IMS:IME,JMS:JME,ENSDIM) :: ZERO_GD
!
      LOGICAL :: RESTART,WARM_RAIN
      LOGICAL,DIMENSION(IMS:IME,JMS:JME) :: CU_ACT_FLAG
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC).NE.0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS.EQ.BMJSCHEME)RETURN
!-----------------------------------------------------------------------
      MYIS1=MAX(IDS+1,ITS)
      MYIE1=MIN(IDE-1,ITE)
      MYJS2=MAX(JDS+2,JTS)
      MYJE2=MIN(JDE-2,JTE)
!
      MYIS_P1 =MAX(IDS  ,ITS-1)
      MYIE_P1 =MIN(IDE  ,ITE+1)
      MYJS1_P1=MAX(JDS+1,JTS-1)
      MYJE1_P1=MIN(JDE-1,JTE+1)
!
      NSTEP_CNV=NCNVC
!
      I_START(1)= MYIS1
      I_END(1)  = MYIE1
      J_START(1)= MYJS2
      J_END(1)  = MYJE2
!
      RESTART=RESTRT
!-----------------------------------------------------------------------
      IF(CONFIG_FLAGS%CU_PHYSICS.EQ.KFETASCHEME)THEN
!
        IF(.NOT.RESTART.AND.NTSD.EQ.0)THEN
          DO J=JTS,JTE
          DO K=KTS,KTE
          DO I=ITS,ITE
            W0AVG(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  GENERAL PREPARATION 
!-----------------------------------------------------------------------
!
      AVCNVC=AVCNVC+1.
      ACUTIM=ACUTIM+1.
!
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      CAPA=R_D/CP
      G_INV=1./G
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
        NCA(I,J)=0.
        SFCZ(I,J)=FIS(I,J)*G_INV
!
!	if (mod(I,20) .eq. 0 .and. mod(J,20) .eq. 0) then
!	write(0,*) 'I,J,FIS, SFCZ: ', I,J,FIS(I,J),SFCZ(I,J)
!	endif
!
!
!***  LPBL IS THE MODEL LAYER CONTAINING THE PBL TOP
!***  COUNTING DOWNWARD FROM THE TOP OF THE DOMAIN
!***  SO KPBL IS THE SAME LAYER COUNTING UPWARD FROM 
!***  THE GROUND.
!
        KPBL(I,J)=KTE-LPBL(I,J)+1
        ZERO_2D(I,J)=0
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
!
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
        ENDDO
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!

      IF(.NOT.HYDRO)THEN
        DO J=MYJS2,MYJE2
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          DZ(I,K,J)=Z(I,K+1,J)-Z(I,K,J)
        ENDDO
        ENDDO
        ENDDO
!
        IF(NTSD.EQ.0)THEN
          DO J=MYJS2,MYJE2
          DO K=KTS,KTE
          DO I=MYIS1,MYIE1
            WINT(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!mptest
!	I=(MYIE1+MYIS1)/2
!	J=(MYJS2+MYJE2)/2
!	do K=KTS,KTE
!	write(0,*) 'K, DZ, WINT: ', K, DZ(I,K,J),WINT(I,K,J)
!	enddo
!mptest
      ELSE
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          WINT(I,1,J)=0.
          WINT(I,KTE+1,J)=0.
        ENDDO
        ENDDO
!
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            WMID(I,KTS)=-OMGALF(I,KTS,J)*CP/(G*DT)
            PLYR=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL+PT
            DZ(I,KTS,J)=T(I,KTS,J)*(P608*Q(I,KTS,J)+1.)*R_D             &
     &                 *(P8W(I,KTS,J)-P8W(I,KTS+1,J))                   &
     &                 /(PLYR*G)
          ENDDO
!
          DO K=KTS+1,KTE
          DO I=MYIS1,MYIE1
            WMID(I,K)=-OMGALF(I,K,J)*CP/(G*DT)
            WINT(I,K,J)=0.5*(WMID(I,K-1)+WMID(I,K))
            DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                         &
     &               *(P8W(I,K,J)-P8W(I,K+1,J))                         &
     &               /(P_PHY(I,K,J)*G)
          ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%CU_PHYSICS.NE.BMJSCHEME)THEN
!
        DO J=MYJS1_P1,MYJE1_P1
!
          DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            WMSK=VTM(I+IHE(J),K,J)+VTM(I+IHW(J),K,J)                    &
     &          +VTM(I,K,J+1)+VTM(I,K,J-1)
            IF(WMSK.GT.0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(U(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +U(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +U(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +U(I,K,J-1)*VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(V(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +V(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +V(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +V(I,K,J-1)*VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF
!-----------------------------------------------------------------------
!
!***  SINGLE-COLUMN CONVECTION
!
!-----------------------------------------------------------------------
!
      CALL CUMULUS_DRIVER(NTSD,DT,GPS,N_MOIST                           &
     &                   ,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN           &
     &                   ,ZERO_3D,ZERO_3D,RAINC,RAINCV,NCA              &
     &                   ,ZERO_3D,ZERO_3D,ZERO_3D                       &
     &                   ,ZERO_3D,ZERO_3D                               &
     &                   ,U_PHY,V_PHY,TH_PHY,T_PHY,WINT,WATER           &
     &                   ,DZ,P8W,P_PHY,PI_PHY                           &
     &                   ,CONFIG_FLAGS,W0AVG,RR,NSTEP_CNV               &
     &                   ,CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,WARM_RAIN     &
     &                   ,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D       &
     &                   ,ZERO_2D,ZERO_2D,ZERO_2D                       &
     &                   ,CUTOP,CUBOT,KPBL                              &
     &                   ,ZERO_2D,ZERO_GD,ZERO_GD,SFCZ                  & 
     &                   ,ENSDIM,1,1,1,1                                &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,I_START,I_END,J_START,J_END,KTS,KTE,1         )
!
!-----------------------------------------------------------------------
!
!***  CNVTOP AND CNVBOT HOLD THE MAXIMUM VERTICAL LIMITS OF
!***  CONVECTIVE CLOUD BETWEEN HISTORY OUTPUT TIMES.
!***  IF WE HAVE JUST PASSED SUCH A TIME THEN REINITIALIZE THE ARRAYS.
!
!      CF_HI=CONFIG_FLAGS%HISTORY_INTERVAL
!      N_TIMSTPS_OUTPUT=NINT(60.*CF_HI/DT)
!      MNTO=MOD(NTSD,N_TIMSTPS_OUTPUT)
!
!      IF(MNTO.GT.0.AND.MNTO.LE.NCNVC)THEN
      CALL ESMF_ClockGet( grid%domain_clock , currTime=CurrTime, rc=rc )
      CALL ESMF_AlarmGet(grid%alarms( HISTORY_ALARM ), prevRingTime=PrevRingTime, rc=rc)

      IF ( CurrTime .EQ. PrevRingTime ) THEN
write(0,*) 'CUCNVC: REINITIALIZING CNVTOP AND CNVBOT ARRAYS'
        DO J=MYJS2,MYJE2
        IENDX=MYIE1
        IF(MOD(J,2).EQ.0.AND.ITE.EQ.IDE-1)IENDX=IENDX-1
        DO I=MYIS1,IENDX
          CNVTOP(I,J)=0.
          CNVBOT(I,J)=1000.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      IENDX=MYIE1
      IF(MOD(J,2).EQ.0.AND.ITE.EQ.IDE-1)IENDX=IENDX-1
      DO I=MYIS1,IENDX
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
!***  RTHCUTEN IN BMJDRV IS DTDT OVER PI.
!
          DTDT=RTHCUTEN(I,K,J)*PI_PHY(I,K,J)
          T(I,K,J)=T(I,K,J)+DTDT*DTCNVC
          Q(I,K,J)=Q(I,K,J)+DQDT*DTCNVC
          TCUCN(I,K,J)=TCUCN(I,K,J)+DTDT
	if ( abs(DTDT*DTCNVC) .gt. 1.0) then
	write(0,*) 'big T change by convection:: ', DTDT*DTCNVC
	endif
        ENDDO
!
!***  UPDATE PRECIPITATION
!
        PCPCOL=RAINCV(I,J)*1.E-3*NSTEP_CNV
        PREC(I,J)=PREC(I,J)+PCPCOL
        ACPREC(I,J)=ACPREC(I,J)+PCPCOL
        CUPREC(I,J)=CUPREC(I,J)+PCPCOL
        CUPPT(I,J)=CUPPT(I,J)+PCPCOL
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION (HTOP, HBOT)
!***  AND FOR OUTPUT (CNVTOP, CNVBOT).  THESE ARRAYS
!***  MUST BE TREATED SEPARATELY FROM EACH OTHER.
!
        NCUTOP=NINT(CUTOP(I,J))
        NCUBOT=NINT(CUBOT(I,J))
!
        IF(NCUTOP.GT.0.AND.NCUTOP.LT.KTE+1)THEN
          HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
          CNVTOP(I,J)=MAX(CUTOP(I,J),CNVTOP(I,J))
        ENDIF
        IF(NCUBOT.GT.0.AND.NCUBOT.LT.KTE+1)THEN
          HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
          CNVBOT(I,J)=MIN(CUBOT(I,J),CNVBOT(I,J))
        ENDIF

!	if (mod(I,30) .eq. 0 .and. mod(J,30) .eq. 0) then
!	write(0,*) 'I,J,HTOP,CNVTOP:: ', I,J,HTOP(I,J),CNVTOP(I,J)
!	write(0,*) 'I,J,HBOT,CNVBOT:: ', I,J,HBOT(I,J),CNVBOT(I,J)
!	endif
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
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------

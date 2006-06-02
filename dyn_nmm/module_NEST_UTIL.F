!
!NCEP_MESO:MODEL_LAYER: BOUNDARY CONDITION UPDATES
!
!----------------------------------------------------------------------
!
      MODULE module_NEST_UTIL
!
!----------------------------------------------------------------------
      USE MODULE_MPP
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_DM
!
!#ifdef DM_PARALLEL
!      INCLUDE "mpif.h"
!#endif
!----------------------------------------------------------------------
      CONTAINS
!
!*********************************************************************************************
      SUBROUTINE NESTBC_PATCH(PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                             &
                             ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT                      & 
                             ,PDTMP_B,TTMP_B,QTMP_B,UTMP_B,VTMP_B,Q2TMP_B,CWMTMP_B        &
                             ,PDTMP_BT,TTMP_BT,QTMP_BT,UTMP_BT,VTMP_BT,Q2TMP_BT,CWMTMP_BT &
                             ,IJDS,IJDE,SPEC_BDY_WIDTH                                    &  ! min/max(id,jd)
                             ,IDS,IDE,JDS,JDE,KDS,KDE                                     &
                             ,IMS,IME,JMS,JME,KMS,KME                                     &
                             ,ITS,ITE,JTS,JTE,KTS,KTE                                     )
!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    PATCH       
!   PRGRMMR: gopal 
!     
! ABSTRACT:
!         THIS IS JUST A FIX FOR USING NESTED BOUNDARIES IN THE HALLO REGION     
! PROGRAM HISTORY LOG:
!   09-23-2004  : gopal 
!     
! USAGE: CALL PATCH FROM SUBROUTINE SOLVE_RUNSTREAM FOR NESTED DOMAIN ONLY
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: IJDS,IJDE,SPEC_BDY_WIDTH
!
!
      REAL,DIMENSION(IJDS:IJDE,1,SPEC_BDY_WIDTH,4)                     &
                                           ,INTENT(INOUT) :: PD_B,PD_BT
!
      REAL,DIMENSION(IJDS:IJDE,KMS:KME,SPEC_BDY_WIDTH,4)                &
                                      ,INTENT(INOUT) :: CWM_B,Q_B,Q2_B, &
                                                        T_B,U_B,V_B 
      REAL,DIMENSION(IJDS:IJDE,KMS:KME,SPEC_BDY_WIDTH,4)                &
                                   ,INTENT(INOUT) :: CWM_BT,Q_BT,Q2_BT, &
                                                     T_BT,U_BT,V_BT 

      REAL,DIMENSION(IMS:IME,JMS:JME), INTENT(IN) :: PDTMP_B,PDTMP_BT

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),                          &
                                  INTENT(IN) :: TTMP_B,QTMP_B,UTMP_B,   &
                                                VTMP_B,Q2TMP_B,CWMTMP_B

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),                           &
                                  INTENT(IN) :: TTMP_BT,QTMP_BT,UTMP_BT, &
                                                VTMP_BT,Q2TMP_BT,CWMTMP_BT
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY
      INTEGER :: I,J,K,IBDY,II,JJ,IB,JB,IIM,JJM,BF
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)

!----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST.

!      WRITE(0,*)'WESTERN BC FOR PATCH',IDS,MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
!

      DO IBDY=1,2
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((W_BDY.AND.IBDY.EQ.1).OR.(E_BDY.AND.IBDY.EQ.2))THEN
          IF(IBDY.EQ.1)THEN
            BF=P_XSB     ! Which boundary (XSB=the boundary where X is at its start)
            IB=1         ! Which cell in from boundary
            II=1         ! Which cell in the domain
          ELSE
            BF=P_XEB     ! Which boundary (XEB=the boundary where X is at its end)
            IB=1         ! Which cell in from boundary
            II=IDE       ! Which cell in the domain
          ENDIF

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
             IF(MOD(J,2).EQ.1)THEN                 ! J=3,5,7,9
                PD_B(J,1,IB,BF)  =PDTMP_B(II,J)
                PD_BT(J,1,IB,BF) =PDTMP_BT(II,J)
             ENDIF
          ENDDO

!
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)  
              IF(MOD(J,2).EQ.1)THEN                  ! J=3,5,7,9
                T_B(J,K,IB,BF)    = TTMP_B(II,K,J)
                T_BT(J,K,IB,BF)   = TTMP_BT(II,K,J)
                Q_B(J,K,IB,BF)    = QTMP_B(II,K,J)
                Q_BT(J,K,IB,BF)   = QTMP_BT(II,K,J)
                Q2_B(J,K,IB,BF)   = Q2TMP_B(II,K,J)
                Q2_BT(J,K,IB,BF)  = Q2TMP_BT(II,K,J)
                CWM_B(J,K,IB,BF)  = CWMTMP_B(II,K,J)
                CWM_BT(J,K,IB,BF) = CWMTMP_BT(II,K,J)
              ENDIF
            ENDDO
          ENDDO

          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)   
              IF(MOD(J,2).EQ.0)THEN                  ! J=2,4,6,8  
                U_B(J,K,IB,BF)    = UTMP_B(II,K,J)
                U_BT(J,K,IB,BF)   = UTMP_BT(II,K,J)
                V_B(J,K,IB,BF)    = VTMP_B(II,K,J)
                V_BT(J,K,IB,BF)   = VTMP_BT(II,K,J)
              ENDIF
            ENDDO
          ENDDO

        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
!***  SOUTH AND NORTH BOUNDARIES
!----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH
!
      DO IBDY=1,2
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((S_BDY.AND.IBDY.EQ.1).OR.(N_BDY.AND.IBDY.EQ.2))THEN
!
          IF(IBDY.EQ.1)THEN
            BF=P_YSB     ! Which boundary (YSB=the boundary where Y is at its start)
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
          ELSE
            BF=P_YEB      ! Which boundary (YEB=the boundary where Y is at its end)
            JB=1          ! Which cell in from boundary
            JJ=JDE        ! Which cell in the domain
          ENDIF
!
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_B(I,1,JB,BF) = PDTMP_B(I,JJ)
            PD_BT(I,1,JB,BF)= PDTMP_BT(I,JJ)
          ENDDO

!
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_B(I,K,JB,BF)   = TTMP_B(I,K,JJ)
              T_BT(I,K,JB,BF)  = TTMP_BT(I,K,JJ)
              Q_B(I,K,JB,BF)   = QTMP_B(I,K,JJ)
              Q_BT(I,K,JB,BF)  = QTMP_BT(I,K,JJ)
              Q2_B(I,K,JB,BF)  = Q2TMP_B(I,K,JJ)
              Q2_BT(I,K,JB,BF) = Q2TMP_BT(I,K,JJ) 
              CWM_B(I,K,JB,BF) = CWMTMP_B(I,K,JJ)
              CWM_BT(I,K,JB,BF)= CWMTMP_BT(I,K,JJ)
            ENDDO
          ENDDO

          DO K=KTS,KTE
           DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_B(I,K,JB,BF)   = UTMP_B(I,K,JJ)
              U_BT(I,K,JB,BF)  = UTMP_BT(I,K,JJ)
              V_B(I,K,JB,BF)   = VTMP_B(I,K,JJ)
              V_BT(I,K,JB,BF)  = VTMP_BT(I,K,JJ)
           ENDDO
          ENDDO

        ENDIF
      ENDDO
END  SUBROUTINE NESTBC_PATCH 

!----------------------------------------------------------------------
!
SUBROUTINE STATS_FOR_MOVE (XLOC,YLOC,PDYN,MSLP,SQWS              &
                          ,PINT,T,Q,U,V                          &
                          ,FIS,PD,SM,PDTOP,PTOP                  &
                          ,DETA1,DETA2                           &
                          ,MOVED,MVNEST,NTSD,NPHS                &
                          ,IDS,IDE,JDS,JDE,KDS,KDE               &
                          ,IMS,IME,JMS,JME,KMS,KME               &
                          ,ITS,ITE,JTS,JTE,KTS,KTE               )

!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:  STATS_FOR_MOVE  
!   PRGRMMR: gopal
!
! ABSTRACT:
!         THIS ROUTINE COMPUTES SOME STATS REQUIRED FOR AUTOMATIC GRID MOTION 
! PROGRAM HISTORY LOG:
!   05-18-2005  : gopal
!
! USAGE: CALL STATS_FOR_MOVE FROM SUBROUTINE SOLVE_RUNSTREAM FOR NESTED DOMAIN ONLY
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$
!**********************************************************************

      USE MODULE_MODEL_CONSTANTS
      USE MODULE_DM 

      IMPLICIT NONE
!
      LOGICAL,EXTERNAL                                      :: wrf_dm_on_monitor
      LOGICAL,INTENT(INOUT)                                 :: MVNEST  ! NMM SWITCH FOR GRID MOTION
      LOGICAL,INTENT(IN)                                    :: MOVED   
      INTEGER,INTENT(IN)                                    :: IDS,IDE,JDS,JDE,KDS,KDE   &
                                                              ,IMS,IME,JMS,JME,KMS,KME   &
                                                              ,ITS,ITE,JTS,JTE,KTS,KTE   &
                                                              ,NTSD,NPHS
!
      INTEGER, INTENT(OUT)                                  :: XLOC,YLOC
      REAL, DIMENSION(KMS:KME),                 INTENT(IN)  :: DETA1,DETA2 
      REAL,                                     INTENT(IN)  :: PDTOP,PTOP
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)  :: FIS,PD,SM
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(IN)  :: PINT,T,Q,U,V
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(OUT) :: PDYN,MSLP,SQWS
!
!     LOCAL

      INTEGER,SAVE                                          :: NTIME0
      INTEGER                                               :: IM,JM,IP,JP
      INTEGER                                               :: I,K,J,XR,YR,DTMOVE,IDUM,JDUM,ITF,JTF
      REAL, PARAMETER                                       :: LAPSR=6.5E-3, GI=1./G,D608=0.608
      REAL, PARAMETER                                       :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
      REAL, PARAMETER                                       :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
      REAL                                                  :: DZ,RTOPP,APELP,A,TSFC,STMP0,STMP1
      REAL                                                  :: SMSUM,SMOUT,XDIFF,YDIFF,PCUT,PGR
      REAL                                                  :: MINGBL_PDYN,MAXGBL_PDYN,MAXGBL_SQWS
      REAL                                                  :: MINGBL_MIJ
      REAL, DIMENSION(IMS:IME,JMS:JME)                      :: MIJ
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME)              :: Z

!    EXEC 

     ITF=MIN(ITE,IDE-1)
     JTF=MIN(JTE,JDE-1)     

!----------------------------------------------------------------------------------

!     KEEP NEST MOTION IN SINK WITH PHYSICS TIME STEPS

      IF(MOD(NTSD+1,NPHS)/=0)THEN
        MVNEST=.FALSE.
        RETURN
      ENDIF

      WRITE(0,*)'PHYSICS IN SINK',NTSD,NPHS

!     DETERMINE THE HEIGHTS ON THE PARENT DOMAIN

      DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
         Z(I,1,J)=FIS(I,J)*GI
       ENDDO
      ENDDO
!
      DO J = JTS, MIN(JTE,JDE)
       DO K = KTS,KTE
        DO I = ITS, MIN(ITE,IDE)
          APELP      = (PINT(I,K+1,J)+PINT(I,K,J))
          RTOPP      = TRG*T(I,K,J)*(1.0+Q(I,K,J)*P608)/APELP
          DZ         = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J)) 
          Z(I,K+1,J) = Z(I,K,J) + DZ
        ENDDO
       ENDDO
      ENDDO

!     DETERMINE THE MEAN SEA LEVEL PRESSURE, THE VERTICALLY AVERAGED WIND
!     SPEED AT ABOUT LEVELS 9 10 AND 11 AND THE DYNAMIC PRESSURES DEFINED
!     FROM BASIC BERNOULLI's THEOREM

      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
            TSFC      = T(I,1,J)*(1.+D608*Q(I,1,J)) + LAPSR*(Z(I,1,J)+Z(I,2,J))*0.5
            A         = LAPSR*Z(I,1,J)/TSFC
            MSLP(I,J) = PINT(I,1,J)*(1-A)**COEF2
            SQWS(I,J) =  (U(I,9,J)*U(I,9,J) + V(I,9,J)*V(I,9,J)           &
                      +   U(I,10,J)*U(I,10,J) + V(I,10,J)*V(I,10,J)       &
                      +   U(I,11,J)*U(I,11,J) + V(I,11,J)*V(I,11,J))/3.0
            PDYN(I,J) =   MSLP(I,J)  + 1.1*SQWS(I,J)/2.0
        ENDDO
      ENDDO

!     FILTER OUT PDYN AND STORE THAT IN MIJ. THE MAXIMUM VALUE OF MIJ GIVES THE STORM CENTER
!     ALSO DO THAT WITHIN A SUB DOMAIN

      MAXGBL_PDYN=MAXVAL(PDYN(ITS:ITF,JTS:JTF))       
      CALL WRF_DM_MAXVAL(MAXGBL_PDYN,IDUM,JDUM)    
      MINGBL_PDYN=MINVAL(PDYN(ITS:ITF,JTS:JTF))
      CALL WRF_DM_MINVAL(MINGBL_PDYN,IDUM,JDUM)            
      PCUT = 0.5*(MAXGBL_PDYN + MINGBL_PDYN)
!
      IM=IDE/2 - IDE/6 
      IP=IDE/2 + IDE/6
      JM=JDE/2 - JDE/4
      JP=JDE/2 + JDE/4
!
      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
          IF(I .GE. IM .AND. I .LE. IP .AND. J .GE. JM .AND. J .LE. JP  &
                       .AND. PCUT .GT. PDYN(I,J))THEN
             MIJ(I,J) = PDYN(I,J)   
          ELSE
             MIJ(I,J) = 105000. 
          ENDIF
        ENDDO
      ENDDO

      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
          PDYN(I,J)=MIJ(I,J) 
        ENDDO
      ENDDO

!     DETERMINE THE LOCATION OF CENTER OF THE CIRCULATION DEFINED BY MIJ AND FIND THE CORRESPONDING MSLP 

      MINGBL_MIJ=MINVAL(MIJ(ITS:ITF,JTS:JTF))
      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
           IF(MIJ(I,J) .EQ. MINGBL_MIJ)THEN
             XLOC=I
             YLOC=J
             STMP0=MSLP(I,J)
           ENDIF
        ENDDO
      ENDDO

      CALL WRF_DM_MINVAL(MINGBL_MIJ,XLOC,YLOC)
      CALL WRF_DM_MINVAL(STMP0,IDUM,JDUM)

!     DETERMINE THE MAXIMUM MSLP AT ABOUT 18 GRID POINTS AWAY FROM THE STORM CENTER 

      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
           IF(I .EQ. XLOC+18)THEN
             XR=I
             YR=J
             STMP1=MSLP(I,J)
           ENDIF
        ENDDO
      ENDDO

      CALL WRF_DM_MAXVAL(STMP1,XR,YR)

!
!     DETERMINE IF THE ENTIRE NESTED DOMAIN IS OVER LAND (SM=0)
!

      SMSUM = 0.0
      DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
         SMSUM = SMSUM + SM(I,J)
       ENDDO
      ENDDO

      SMOUT=WRF_DM_SUM_REAL(SMSUM)/(IDE*JDE)

!     STOP GRID MOTION. AVOID MOVING TOO RAPID GRID MOTION, SAY SOMETHING LIKE EVERY
!     OTHER TIME STEP OR SO  

      PGR=STMP1-STMP0
      XDIFF=ABS(XLOC - IDE/2)
      YDIFF=ABS(YLOC - JDE/2)
      IF(NTSD==0 .OR. MOVED)NTIME0=NTSD 
      DTMOVE=NTSD-NTIME0                    ! TIME INTERVAL SINCE THE PREVIOUS MOVE
!
      IF(DTMOVE .LE. 45 .OR. PGR .LE. 200.)THEN       
        WRITE(0,*)'SUSPEND MOTION: SMALL DTMOVE OR WEAK PGF:','DTMOVE=',DTMOVE,'PGR=',PGR 
        MVNEST=.FALSE.                               ! SET STATIC GRID
      ELSE IF(STMP0 .GE. STMP1)THEN
        WRITE(0,*)'SUSPEND MOTION: THERE IS NO VORTEX IN THE DOMAIN:','STMP0=',STMP0,'STMP1=',STMP1
        MVNEST=.FALSE.
      ELSE IF(XDIFF .GT. 24 .OR. YDIFF .GT. 24)THEN
        WRITE(0,*)'SUSPEND MOTION: LOST VORTEX ','DTMOVE=',DTMOVE,'XDIFF=',XDIFF,'YDIFF=',YDIFF
        MVNEST=.FALSE.
      ELSE IF(SMOUT .LE. 0.2 .AND. XDIFF .GT. 12 .AND. YDIFF .GT. 12)THEN  
        WRITE(0,*)'SUSPEND MOTION: VORTEX LOST OVER LAND ','DTMOVE=',DTMOVE,'XDIFF=',XDIFF,'YDIFF=',YDIFF
        MVNEST=.FALSE. 
      ELSE IF(SMOUT .LE. 0.2 .AND. PGR .LE. 400.)THEN
        WRITE(0,*)'SUSPEND MOTION: VORTEX WEAK OVER LAND ','SMOUT=',SMOUT,'PGR=',PGR
        MVNEST=.FALSE.
      ELSE IF(SMOUT .LE. 0.2 .AND. DTMOVE .GE. 1500)THEN
        WRITE(0,*)'SUSPEND MOTION: STOP MOTION  OVER LAND','SMOUT=',SMOUT,'DTMOVE=',DTMOVE
        MVNEST=.FALSE.
      ELSE
        MVNEST=.TRUE.
      ENDIF

      RETURN

END SUBROUTINE STATS_FOR_MOVE
!----------------------------------------------------------------------------------

END  MODULE module_NEST_UTIL


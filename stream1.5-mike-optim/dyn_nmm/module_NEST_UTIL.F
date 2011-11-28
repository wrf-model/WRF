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
      SUBROUTINE NESTBC_PATCH(PD_BXS,PD_BXE,PD_BYS,PD_BYE                                 &
                             ,T_BXS,T_BXE,T_BYS,T_BYE,Q_BXS,Q_BXE,Q_BYS,Q_BYE             &
                             ,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS,V_BXE,V_BYS,V_BYE             &
                             ,Q2_BXS,Q2_BXE,Q2_BYS,Q2_BYE                                 &
                             ,CWM_BXS,CWM_BXE,CWM_BYS,CWM_BYE                             &
                             ,PD_BTXS,PD_BTXE,PD_BTYS,PD_BTYE                             &
                             ,T_BTXS,T_BTXE,T_BTYS,T_BTYE,Q_BTXS,Q_BTXE,Q_BTYS,Q_BTYE     &
                             ,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS,V_BTXE,V_BTYS,V_BTYE     &
                             ,Q2_BTXS,Q2_BTXE,Q2_BTYS,Q2_BTYE                             &
                             ,CWM_BTXS,CWM_BTXE,CWM_BTYS,CWM_BTYE                         &
!
                             ,PDTMP_B,TTMP_B, QTMP_B,UTMP_B,VTMP_B,Q2TMP_B,CWMTMP_B       &
                             ,PDTMP_BT,TTMP_BT,QTMP_BT,UTMP_BT,VTMP_BT,Q2TMP_BT,CWMTMP_BT &
!
                             ,SPEC_BDY_WIDTH                                              &  
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
!         THIS IS JUST A FIX FOR USING NESTED BOUNDARIES IN THE HALO REGION     
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
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
!
!
      REAL,DIMENSION(IMS:IME,1,SPEC_BDY_WIDTH)                     &
                                           ,INTENT(INOUT) :: PD_BYS,PD_BYE &
                                                          ,PD_BTYS,PD_BTYE

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BYS,CWM_BYE &
                                                       ,Q_BYS,Q_BYE     &
                                                       ,Q2_BYS,Q2_BYE   &
                                                       ,T_BYS,T_BYE     &
                                                       ,U_BYS,U_BYE     &
                                                       ,V_BYS,V_BYE     

      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BTYS,CWM_BTYE &
                                                       ,Q_BTYS,Q_BTYE     &
                                                       ,Q2_BTYS,Q2_BTYE   &
                                                       ,T_BTYS,T_BTYE     &
                                                       ,U_BTYS,U_BTYE     &
                                                       ,V_BTYS,V_BTYE     

!

      REAL,DIMENSION(JMS:JME,1,SPEC_BDY_WIDTH)                     &
                                           ,INTENT(INOUT) :: PD_BXS,PD_BXE &
                                                          ,PD_BTXS,PD_BTXE

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BXS,CWM_BXE &
                                                       ,Q_BXS,Q_BXE     &
                                                       ,Q2_BXS,Q2_BXE   &
                                                       ,T_BXS,T_BXE     &
                                                       ,U_BXS,U_BXE     &
                                                       ,V_BXS,V_BXE     

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                &
                                      ,INTENT(INOUT) :: CWM_BTXS,CWM_BTXE &
                                                       ,Q_BTXS,Q_BTXE     &
                                                       ,Q2_BTXS,Q2_BTXE   &
                                                       ,T_BTXS,T_BTXE     &
                                                       ,U_BTXS,U_BTXE     &
                                                       ,V_BTXS,V_BTXE     

!

      REAL,DIMENSION(IMS:IME,JMS:JME)                     &
                                      ,INTENT(IN) :: PDTMP_B,PDTMP_BT

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME)                     &
                                      ,INTENT(IN) :: CWMTMP_B,CWMTMP_BT  &
                                                    ,QTMP_B,QTMP_BT     &
                                                    ,Q2TMP_B,Q2TMP_BT   &
                                                    ,TTMP_B,TTMP_BT     &
                                                    ,UTMP_B,UTMP_BT     &
                                                    ,VTMP_B,VTMP_BT    

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
        IF(W_BDY.AND.IBDY.EQ.1)THEN
!            BF=P_XSB     ! Which boundary (XSB=the boundary where X is at its start)
            IB=1         ! Which cell in from boundary
            II=1         ! Which cell in the domain

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
             IF(MOD(J,2).EQ.1)THEN                 ! J=3,5,7,9
                PD_BXS(J,1,IB)  =PDTMP_B(II,J)
                PD_BTXS(J,1,IB) =PDTMP_BT(II,J)
             ENDIF
          ENDDO
!
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
              IF(MOD(J,2).EQ.1)THEN                  ! J=3,5,7,9
                T_BXS(J,K,IB)    = TTMP_B(II,J,K)
                T_BTXS(J,K,IB)   = TTMP_BT(II,J,K)
                Q_BXS(J,K,IB)    = QTMP_B(II,J,K)
                Q_BTXS(J,K,IB)   = QTMP_BT(II,J,K)
                Q2_BXS(J,K,IB)   = Q2TMP_B(II,J,K)
                Q2_BTXS(J,K,IB)  = Q2TMP_BT(II,J,K)
                CWM_BXS(J,K,IB)  = CWMTMP_B(II,J,K)
                CWM_BTXS(J,K,IB) = CWMTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2).EQ.0)THEN                  ! J=2,4,6,8  
                U_BXS(J,K,IB)    = UTMP_B(II,J,K)
                U_BTXS(J,K,IB)   = UTMP_BT(II,J,K)
                V_BXS(J,K,IB)    = VTMP_B(II,J,K)
                V_BTXS(J,K,IB)   = VTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

        ELSEIF (E_BDY.AND.IBDY.EQ.2) THEN

!            BF=P_XEB     ! Which boundary (XEB=the boundary where X is at its end)
            IB=1         ! Which cell in from boundary
            II=IDE       ! Which cell in the domain

          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
             IF(MOD(J,2).EQ.1)THEN                 ! J=3,5,7,9
                PD_BXE(J,1,IB)  =PDTMP_B(II,J)
                PD_BTXE(J,1,IB) =PDTMP_BT(II,J)
             ENDIF
          ENDDO
!
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
              IF(MOD(J,2).EQ.1)THEN                  ! J=3,5,7,9
                T_BXE(J,K,IB)    = TTMP_B(II,J,K)
                T_BTXE(J,K,IB)   = TTMP_BT(II,J,K)
                Q_BXE(J,K,IB)    = QTMP_B(II,J,K)
                Q_BTXE(J,K,IB)   = QTMP_BT(II,J,K)
                Q2_BXE(J,K,IB)   = Q2TMP_B(II,J,K)
                Q2_BTXE(J,K,IB)  = Q2TMP_BT(II,J,K)
                CWM_BXE(J,K,IB)  = CWMTMP_B(II,J,K)
                CWM_BTXE(J,K,IB) = CWMTMP_BT(II,J,K)
              ENDIF
            ENDDO
          ENDDO

          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2).EQ.0)THEN                  ! J=2,4,6,8  
                U_BXE(J,K,IB)    = UTMP_B(II,J,K)
                U_BTXE(J,K,IB)   = UTMP_BT(II,J,K)
                V_BXE(J,K,IB)    = VTMP_B(II,J,K)
                V_BTXE(J,K,IB)   = VTMP_BT(II,J,K)
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
        IF(S_BDY.AND.IBDY.EQ.1) THEN 
!
!            BF=P_YSB     ! Which boundary (YSB=the boundary where Y is at its start)
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
!
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYS(I,1,JB) = PDTMP_B(I,JJ)
            PD_BTYS(I,1,JB)= PDTMP_BT(I,JJ)
          ENDDO

!
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYS(I,K,JB)   = TTMP_B(I,JJ,K)
              T_BTYS(I,K,JB)  = TTMP_BT(I,JJ,K)
              Q_BYS(I,K,JB)   = QTMP_B(I,JJ,K)
              Q_BTYS(I,K,JB)  = QTMP_BT(I,JJ,K)
              Q2_BYS(I,K,JB)  = Q2TMP_B(I,JJ,K)
              Q2_BTYS(I,K,JB) = Q2TMP_BT(I,JJ,K)
              CWM_BYS(I,K,JB) = CWMTMP_B(I,JJ,K)
              CWM_BTYS(I,K,JB)= CWMTMP_BT(I,JJ,K)
            ENDDO
          ENDDO

          DO K=KTS,KTE
           DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYS(I,K,JB)   = UTMP_B(I,JJ,K)
              U_BTYS(I,K,JB)  = UTMP_BT(I,JJ,K)
              V_BYS(I,K,JB)   = VTMP_B(I,JJ,K)
              V_BTYS(I,K,JB)  = VTMP_BT(I,JJ,K)
           ENDDO
          ENDDO

          ELSEIF (N_BDY.AND.IBDY.EQ.2) THEN
!            BF=P_YEB      ! Which boundary (YEB=the boundary where Y is at its end)
            JB=1          ! Which cell in from boundary
            JJ=JDE        ! Which cell in the domain

          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYE(I,1,JB) = PDTMP_B(I,JJ)
            PD_BTYE(I,1,JB)= PDTMP_BT(I,JJ)
          ENDDO

!
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYE(I,K,JB)   = TTMP_B(I,JJ,K)
              T_BTYE(I,K,JB)  = TTMP_BT(I,JJ,K)
              Q_BYE(I,K,JB)   = QTMP_B(I,JJ,K)
              Q_BTYE(I,K,JB)  = QTMP_BT(I,JJ,K)
              Q2_BYE(I,K,JB)  = Q2TMP_B(I,JJ,K)
              Q2_BTYE(I,K,JB) = Q2TMP_BT(I,JJ,K)
              CWM_BYE(I,K,JB) = CWMTMP_B(I,JJ,K)
              CWM_BTYE(I,K,JB)= CWMTMP_BT(I,JJ,K)
            ENDDO
          ENDDO

          DO K=KTS,KTE
           DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYE(I,K,JB)   = UTMP_B(I,JJ,K)
              U_BTYE(I,K,JB)  = UTMP_BT(I,JJ,K)
              V_BYE(I,K,JB)   = VTMP_B(I,JJ,K)
              V_BTYE(I,K,JB)  = VTMP_BT(I,JJ,K)
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
#ifdef HWRF
                          ,RESTART,NTIME0                        & ! zhang's doing
                          ,MOVED,MVNEST,NTSD,NPHS,CFREQ          & ! CFREQ*DT*NPHS=540s
#else
                          ,MOVED,MVNEST,NTSD,NPHS                &
#endif
                          ,vortex_tracker                        &
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
!         THERE ARE THREE DIFFERENT MODES:
!             vortex_tracker=1 -- follow vortex using old algorithm
!             vortex_tracker=2 -- follow child
!             vortex_tracker=3 -- follow vortex using new HRD algorithm
!               otherwise   -- do nothing
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
      INTEGER,INTENT(IN)                                    :: vortex_tracker
      INTEGER,INTENT(IN)                                    :: IDS,IDE,JDS,JDE,KDS,KDE   &
                                                              ,IMS,IME,JMS,JME,KMS,KME   &
                                                              ,ITS,ITE,JTS,JTE,KTS,KTE   &
#ifdef HWRF
                                                              ,NTSD,NPHS,CFREQ
#else
                                                              ,NTSD,NPHS
#endif
!
      INTEGER, INTENT(OUT)                                  :: XLOC,YLOC
      INTEGER                                               :: NXLOC,NYLOC
      REAL                                                  :: NSUM1,NSUM2,NSUM3
      REAL, DIMENSION(KMS:KME),                 INTENT(IN)  :: DETA1,DETA2
      REAL,                                     INTENT(IN)  :: PDTOP,PTOP
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)  :: FIS,PD,SM
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)  :: PINT,T,Q,U,V
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(OUT) :: PDYN,MSLP,SQWS
!
!     LOCAL

#ifdef HWRF
!zhang's doing
      INTEGER,INTENT(INOUT)                                 :: NTIME0
      LOGICAL,INTENT(IN)                                    :: RESTART
#else
      INTEGER,SAVE                                          :: NTIME0
#endif
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
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME)              :: Z

!    EXEC 

     ITF=MIN(ITE,IDE-1)
     JTF=MIN(JTE,JDE-1)

!----------------------------------------------------------------------------------

!     KEEP NEST MOTION IN SINK WITH PHYSICS TIME STEPS
#ifdef HWRF
      IF(MOD(NTSD+1,CFREQ*NPHS)/=0)THEN   !FOR FULL COUPLING
        IF(MOVED) NTIME0=NTSD             !FOR UPDATING NTIM0
#else
      IF(MOD(NTSD+1,NPHS)/=0)THEN
#endif
        MVNEST=.FALSE.
        RETURN
      ENDIF

!     DETERMINE THE HEIGHTS ON THE PARENT DOMAIN

      DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
         Z(I,J,1)=FIS(I,J)*GI
       ENDDO
      ENDDO
!
      DO K = KTS,KTE
       DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
          APELP      = (PINT(I,J,K+1)+PINT(I,J,K))
          RTOPP      = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
          DZ         = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))
          Z(I,J,K+1) = Z(I,J,K) + DZ
        ENDDO
       ENDDO
      ENDDO

!     DETERMINE THE MEAN SEA LEVEL PRESSURE, THE VERTICALLY AVERAGED WIND
!     SPEED AT ABOUT LEVELS 9 10 AND 11 AND THE DYNAMIC PRESSURES DEFINED
!     FROM BASIC BERNOULLI's THEOREM

      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
            TSFC      = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z(I,J,1)+Z(I,J,2))*0.5
            A         = LAPSR*Z(I,J,1)/TSFC
            MSLP(I,J) = PINT(I,J,1)*(1-A)**COEF2
            SQWS(I,J) =  (U(I,J,9)*U(I,J,9) + V(I,J,9)*V(I,J,9)           &
                      +   U(I,J,10)*U(I,J,10) + V(I,J,10)*V(I,J,10)       &
                      +   U(I,J,11)*U(I,J,11) + V(I,J,11)*V(I,J,11))/3.0
#ifdef HWRF
            PDYN(I,J) =   MSLP(I,J)
#else
            PDYN(I,J) =   MSLP(I,J)  + 1.1*SQWS(I,J)/2.0
#endif
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
             MIJ(I,J) = 105000.0
          ENDIF
        ENDDO
      ENDDO

      DO J = JTS, MIN(JTE,JDE)
        DO I = ITS, MIN(ITE,IDE)
          PDYN(I,J)=MIJ(I,J)
        ENDDO
      ENDDO

!   BEGIN OLD TRACKER CODE ----------------------------------------------------
    old_tracker_1: if(vortex_tracker == 1) then
!
!     DETERMINE THE LOCATION OF CENTER OF THE CIRCULATION DEFINED BY MIJ AND FIND THE CORRESPONDING MSLP 
!
      STMP0=MAXGBL_PDYN*100.                 ! define arbitrary maximum
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
   endif old_tracker_1
!   END OLD TRACKER CODE ------------------------------------------------------


!   BEGIN HRD TRACKER CODE ----------------------------------------------------
    hwrfx_tracker: if(vortex_tracker == 3) then
!     USE CENTROID TO FIND THE CENTER    Xuejin's doing

      NSUM1=0.0
      NSUM2=0.0
      NSUM3=0.0
      DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
        IF(I .GE. IM .AND. I .LE. IP .AND. J .GE. JM .AND. J .LE. JP )THEN
!       IF(I .EQ. IM .AND. J .EQ. JM)THEN
          NSUM1 = NSUM1 + I*(105000.1 - PDYN(I,J))
          NSUM2 = NSUM2 + J*(105000.1 - PDYN(I,J))
          NSUM3 = NSUM3 + (105000.1 - PDYN(I,J))
!         WRITE(0,*)'TEST',NSUM1,I,J,0.01*(105000.0 - PDYN(I,J)),PDYN(I,J) 
        ENDIF
       ENDDO
      ENDDO
      NSUM1 = WRF_DM_SUM_REAL(NSUM1)
      NSUM2 = WRF_DM_SUM_REAL(NSUM2)
      NSUM3 = WRF_DM_SUM_REAL(NSUM3)
      NXLOC = NINT(NSUM1/NSUM3)
      NYLOC = NINT(NSUM2/NSUM3)

      WRITE(0,*)'NEW CALC',NSUM1,NSUM2,NSUM3
      WRITE(0,*)'XLOC,YLOC',NXLOC,XLOC,NYLOC,YLOC

      XLOC = NXLOC
      YLOC = NYLOC

    endif hwrfx_tracker
!   END HRD TRACKER CODE ------------------------------------------------------
 

!   BEGIN OLD TRACKER CODE ----------------------------------------------------
!     DETERMINE THE MAXIMUM MSLP AT ABOUT 18 GRID POINTS AWAY FROM THE STORM CENTER 
    old_tracker_2: if ( vortex_tracker == 1 ) then

      STMP1=0.0
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
   endif old_tracker_2
!   END OLD TRACKER CODE ------------------------------------------------

 
      XDIFF=ABS(XLOC - IDE/2)
      YDIFF=ABS(YLOC - JDE/2)
#ifdef HWRF
!zhang's doing
      IF((.NOT.RESTART .AND. NTSD==0) .OR. MOVED)NTIME0=NTSD
#else
      IF(NTSD==0 .OR. MOVED)NTIME0=NTSD
#endif
      DTMOVE=NTSD-NTIME0                    ! TIME INTERVAL SINCE THE PREVIOUS MOVE
!
!    DECIDE IF NEST MOTION SHOULD HAPPEN
!
    if(vortex_tracker == 3) then
      ! Using new HRD tracker.  Move if centroid moved.
       IF(XDIFF .GE. 1 .OR. YDIFF .GE. 2) THEN
        MVNEST=.TRUE.
        NTIME0=NTSD
       ELSE
        ! Centroid has not moved one parent gridpoint yet.
        MVNEST=.FALSE.
       ENDIF
    elseif(vortex_tracker==2) then
       ! Tracking child domain.  Nest motion check happens in
       ! direction_of_move2, but we MUST set mvnest=true here:
       MVNEST=.TRUE.
    elseif(vortex_tracker==1) then
      ! Using old HWRF tracker.  Decide if it wants to move.
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
   else
      ! Not using any valid trackers, so set MVNEST to false.
      MVNEST=.false.
   endif

      RETURN

END SUBROUTINE STATS_FOR_MOVE
!----------------------------------------------------------------------------------
SUBROUTINE MSLP_DIAG (MSLP,PINT,T,Q               &
                     ,FIS,PD,DETA1,DETA2,PDTOP    &
                     ,IDS,IDE,JDS,JDE,KDS,KDE     &
                     ,IMS,IME,JMS,JME,KMS,KME     &
                     ,ITS,ITE,JTS,JTE,KTS,KTE     )


!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:  MSLP_DIAG 
!   PRGRMMR: gopal
!
! ABSTRACT:
!         THIS ROUTINE COMPUTES MSLP OVER THE PARENT DOMAIN FOR DIAGONOSTIC PURPOSE 
! PROGRAM HISTORY LOG:
!   07-21-2005  : gopal
!
! USAGE: CALL MSLP_DIAG FROM THE SOLVER 
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP/Linux cluster
!$$$

      USE MODULE_MODEL_CONSTANTS
      USE MODULE_DM

      IMPLICIT NONE

!     global variables

      INTEGER,INTENT(IN)                                      :: IDS,IDE,JDS,JDE,KDS,KDE   &
                                                                ,IMS,IME,JMS,JME,KMS,KME   & 
                                                                ,ITS,ITE,JTS,JTE,KTS,KTE   

      REAL,                                     INTENT(IN)    :: PDTOP
      REAL, DIMENSION(KMS:KME),                 INTENT(IN)    :: DETA1,DETA2
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(INOUT) :: MSLP
      REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)    :: FIS,PD
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)    :: PINT,T,Q

!     local variables

      REAL, PARAMETER                                       :: LAPSR=6.5E-3, GI=1./G,D608=0.608
      REAL, PARAMETER                                       :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
      REAL, PARAMETER                                       :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
      REAL                                                  :: RTOPP,APELP,DZ,SFCT,A
      REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME)              :: Z
      INTEGER                                               :: I,J,K
!-----------------------------------------------------------------------------------------------------


     DO J = JTS, MIN(JTE,JDE)
      DO I = ITS, MIN(ITE,IDE)
         Z(I,J,1)=FIS(I,J)*GI
      ENDDO
     ENDDO

     DO K = KTS,KTE
      DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
         APELP      = (PINT(I,J,K+1)+PINT(I,J,K))
         RTOPP      = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
         DZ         = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))
         Z(I,J,K+1) = Z(I,J,K) + DZ
       ENDDO
      ENDDO
     ENDDO

     MSLP=-9999.99
     DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
         SFCT      = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z(I,J,1)+Z(I,J,2))*0.5
         A         = LAPSR*Z(I,J,1)/SFCT
         MSLP(I,J) = PINT(I,J,1)*(1-A)**COEF2
       ENDDO
     ENDDO


END SUBROUTINE MSLP_DIAG
!------------------------------------------------------------------------------------------------------

END  MODULE module_NEST_UTIL

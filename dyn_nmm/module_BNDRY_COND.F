!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: BOUNDARY CONDITION UPDATES
!
!-----------------------------------------------------------------------
!
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!
!-----------------------------------------------------------------------
!
      MODULE MODULE_BNDRY_COND
!
!-----------------------------------------------------------------------
      USE MODULE_STATE_DESCRIPTION
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
      INCLUDE "mpif.h"
#endif
!-----------------------------------------------------------------------
      REAL :: D06666=0.06666666
!-----------------------------------------------------------------------
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE BOCOH(NTSD,DT0,NEST,NBC,NBOCO,LAST_TIME,TSPH           &
     &                ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM                    &
     &                ,PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                  &
     &                ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT           &
     &                ,PD,T,Q,Q2,CWM,PINT                               &
     &                ,IJDS,IJDE,SPEC_BDY_WIDTH                         &  ! min/max(id,jd)
     &                ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    BOCOH       UPDATE MASS POINTS ON BOUNDARY
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
!     
! ABSTRACT:
!     TEMPERATURE, SPECIFIC HUMIDITY, AND SURFACE PRESSURE
!     ARE UPDATED ON THE DOMAIN BOUNDARY BY APPLYING THE
!     PRE-COMPUTED TENDENCIES AT EACH TIME STEP.
!     
! PROGRAM HISTORY LOG:
!   87-??-??  MESINGER   - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D in HORIZONTAL
!   96-12-13  BLACK      - FINAL MODIFICATION FOR NESTED RUNS
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   00-01-06  BLACK      - MODIFIED FOR JANJIC NONHYDROSTATIC CODE
!   00-09-14  BLACK      - MODIFIED FOR DIRECT ACCESS READ
!   01-03-12  BLACK      - CONVERTED TO WRF STRUCTURE
!   02-08-29  MICHALAKES - CHANGED II=I-MY_IS_GLB+1 TO II=I
!                          ADDED CONDITIONAL COMPILATION AROUND MPI
!                          CONVERT INDEXING FROM LOCAL TO GLOBAL
!   02-09-06  WOLFE      - MORE CONVERSION TO GLOBAL INDEXING 
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL BOCOH FROM SUBROUTINE SOLVE_NMM
!   INPUT ARGUMENT LIST:
!
!     NOTE THAT IDE AND JDE INSIDE ROUTINE SHOULD BE PASSED IN
!     AS WHAT WRF CONSIDERS THE UNSTAGGERED GRID DIMENSIONS; THAT
!     IS, 1 LESS THAN THE IDE AND JDE SET BY WRF FRAMEWORK, JM
!  
!   OUTPUT ARGUMENT LIST: 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!  
!     UNIQUE: NONE
!  
!     LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: NEST
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: IJDS,IJDE,SPEC_BDY_WIDTH
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NMM_MAX_DIM is set in configure.wrf and must agree with the value of
! dimspec q in the Registry/Registry
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: LB,NBC,NTSD
      LOGICAL,INTENT(IN) :: LAST_TIME
      INTEGER,INTENT(INOUT) :: NBOCO
!
      REAL,INTENT(IN) :: DT0,PDTOP,PT,TSPH
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM
!
      REAL,DIMENSION(IJDS:IJDE,1,SPEC_BDY_WIDTH,4)                      &
     &                                     ,INTENT(INOUT) :: PD_B,PD_BT
!
      REAL,DIMENSION(IJDS:IJDE,KMS:KME,SPEC_BDY_WIDTH,4)                &
     &                                ,INTENT(INOUT) :: CWM_B,Q_B,Q2_B  &
     &                                                 ,T_B,U_B,V_B 
      REAL,DIMENSION(IJDS:IJDE,KMS:KME,SPEC_BDY_WIDTH,4)                &
     &                             ,INTENT(INOUT) :: CWM_BT,Q_BT,Q2_BT  &
     &                                              ,T_BT,U_BT,V_BT 
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM      &
     &                                                        ,PINT,Q   &
     &                                                        ,Q2,T
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: BF,I,IB,IBDY,II,IIM,IM,IRTN,ISIZ1,ISIZ2                &
     &          ,J,JB,JJ,JJM,JM,K,N,NN,NREC,REC
      INTEGER :: MY_IS_GLB,MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: ILPAD1,IRPAD1,JBPAD1,JTPAD1
!
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY
!
      REAL :: BCHR,RHTM,SHTM,DT
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      IM=IDE-IDS+1
      JM=JDE-JDS+1
      IIM=IM
      JJM=JM
!
      ISIZ1=2*LB
      ISIZ2=2*LB*(KME-KMS)
!
      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)
!
      ILPAD1=1
      IF(W_BDY)ILPAD1=0
      IRPAD1=1
      IF(E_BDY)IRPAD1=0
      JBPAD1=1
      IF(S_BDY)JBPAD1=0
      JTPAD1=1
      IF(N_BDY)JTPAD1=0
!
      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE
!
      DT=DT0
!
!-----------------------------------------------------------------------
!***  SOUTH AND NORTH BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH
!
      DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((S_BDY.AND.IBDY==1).OR.(N_BDY.AND.IBDY==2))THEN
!
          IF(IBDY==1)THEN
            BF=P_YSB     ! Which boundary (YSB=the boundary where Y is at its start)
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
          ELSE
            BF=P_YEB     ! Which boundary (YEB=the boundary where Y is at its end)
            JB=1         ! Which cell in from boundary
            JJ=JJM       ! Which cell in the domain
          ENDIF
!
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_B(I,1,JB,BF)=PD_B(I,1,JB,BF)+PD_BT(I,1,JB,BF)*DT
            PD(I,JJ)=PD_B(I,1,JB,BF)
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_B(I,K,JB,BF)=T_B(I,K,JB,BF)+T_BT(I,K,JB,BF)*DT
              Q_B(I,K,JB,BF)=Q_B(I,K,JB,BF)+Q_BT(I,K,JB,BF)*DT
              Q2_B(I,K,JB,BF)=Q2_B(I,K,JB,BF)+Q2_BT(I,K,JB,BF)*DT
              CWM_B(I,K,JB,BF)=CWM_B(I,K,JB,BF)+CWM_BT(I,K,JB,BF)*DT
              T(I,K,JJ)=T_B(I,K,JB,BF)
              Q(I,K,JJ)=Q_B(I,K,JB,BF)
              Q2(I,K,JJ)=Q2_B(I,K,JB,BF)
              CWM(I,K,JJ)=CWM_B(I,K,JB,BF)
              PINT(I,K,JJ)=ETA1(K)*PDTOP                                &
     &                    +ETA2(K)*PD(I,JJ)*RES(I,JJ)+PT
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST. 
!
      DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((W_BDY.AND.IBDY==1).OR.(E_BDY.AND.IBDY==2))THEN
          IF(IBDY==1)THEN
            BF=P_XSB     ! Which boundary (XSB=the boundary where X is at its start)
            IB=1         ! Which cell in from boundary 
            II=1         ! Which cell in the domain
          ELSE
            BF=P_XEB     ! Which boundary (XEB=the boundary where X is at its end)
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain
          ENDIF
!
          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
            IF(MOD(J,2)==1)THEN
              PD_B(J,1,IB,BF)=PD_B(J,1,IB,BF)+PD_BT(J,1,IB,BF)*DT
              PD(II,J)=PD_B(J,1,IB,BF)
            ENDIF
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
!
              IF(MOD(J,2)==1)THEN
                T_B(J,K,IB,BF)=T_B(J,K,IB,BF)+T_BT(J,K,IB,BF)*DT
                Q_B(J,K,IB,BF)=Q_B(J,K,IB,BF)+Q_BT(J,K,IB,BF)*DT
                Q2_B(J,K,IB,BF)=Q2_B(J,K,IB,BF)+Q2_BT(J,K,IB,BF)*DT
                CWM_B(J,K,IB,BF)=CWM_B(J,K,IB,BF)+CWM_BT(J,K,IB,BF)*DT
                T(II,K,J)=T_B(J,K,IB,BF)
                Q(II,K,J)=Q_B(J,K,IB,BF)
                Q2(II,K,J)=Q2_B(J,K,IB,BF)
                CWM(II,K,J)=CWM_B(J,K,IB,BF)
                PINT(II,K,J)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(II,J)*RES(II,J)+PT
              ENDIF
!
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF PD THEN REMAINING MASS VARIABLES
!***  AT INNER BOUNDARY
!-----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,1)+HTM(I+1,KTE,1)+HTM(I,KTE,3)+HTM(I+1,KTE,3)
          PD(I,2)=(PD(I,1)*HTM(I,KTE,1)+PD(I+1,1)*HTM(I+1,KTE,1)        &
     &            +PD(I,3)*HTM(I,KTE,3)+PD(I+1,3)*HTM(I+1,KTE,3))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,JJM-2)+HTM(I+1,KTE,JJM-2)+HTM(I,KTE,JJM)       &
     &                                          +HTM(I+1,KTE,JJM)
          PD(I,JJM-1)=(PD(I,JJM-2)*HTM(I,KTE,JJM-2)                     &
     &                +PD(I+1,JJM-2)*HTM(I+1,KTE,JJM-2)                 &
     &                +PD(I,JJM)*HTM(I,KTE,JJM)                         &
     &                +PD(I+1,JJM)*HTM(I+1,KTE,JJM))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(W_BDY)THEN
        DO J=4,JM-3,2
!
          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            SHTM=HTM(1,KTE,JJ-1)+HTM(2,KTE,JJ-1)+HTM(1,KTE,JJ+1)        &
     &                                          +HTM(2,KTE,JJ+1)
            PD(1,JJ)=(PD(1,JJ-1)*HTM(1,KTE,JJ-1)                        &
     &               +PD(2,JJ-1)*HTM(2,KTE,JJ-1)                        &
     &               +PD(1,JJ+1)*HTM(1,KTE,JJ+1)                        &
     &               +PD(2,JJ+1)*HTM(2,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(E_BDY)THEN
        DO J=4,JM-3,2
!
          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            SHTM=HTM(IIM-1,KTE,JJ-1)+HTM(IIM,KTE,JJ-1)                  &
     &          +HTM(IIM-1,KTE,JJ+1)+HTM(IIM,KTE,JJ+1)
            PD(IIM-1,JJ)=(PD(IIM-1,JJ-1)*HTM(IIM-1,KTE,JJ-1)            &
     &                   +PD(IIM,JJ-1)*HTM(IIM,KTE,JJ-1)                &
     &                   +PD(IIM-1,JJ+1)*HTM(IIM-1,KTE,JJ+1)            &
     &                   +PD(IIM,JJ+1)*HTM(IIM,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,jj,k,rhtm)
      DO 200 K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,1)+HTM(I+1,K,1)+HTM(I,K,3)+HTM(I+1,K,3))
          T(I,K,2)=(T(I,K,1)*HTM(I,K,1)+T(I+1,K,1)*HTM(I+1,K,1)         &
     &             +T(I,K,3)*HTM(I,K,3)+T(I+1,K,3)*HTM(I+1,K,3))        &
     &             *RHTM
          Q(I,K,2)=(Q(I,K,1)*HTM(I,K,1)+Q(I+1,K,1)*HTM(I+1,K,1)         &
     &             +Q(I,K,3)*HTM(I,K,3)+Q(I+1,K,3)*HTM(I+1,K,3))        &
     &             *RHTM
          Q2(I,K,2)=(Q2(I,K,1)*HTM(I,K,1)+Q2(I+1,K,1)*HTM(I+1,K,1)      &
     &              +Q2(I,K,3)*HTM(I,K,3)+Q2(I+1,K,3)*HTM(I+1,K,3))     &
     &              *RHTM
          CWM(I,K,2)=(CWM(I,K,1)*HTM(I,K,1)+CWM(I+1,K,1)*HTM(I+1,K,1)   &
     &               +CWM(I,K,3)*HTM(I,K,3)+CWM(I+1,K,3)*HTM(I+1,K,3))  &
     &               *RHTM
          PINT(I,K,2)=ETA1(K)*PDTOP+ETA2(K)*PD(I,2)*RES(I,2)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,JJM-2)+HTM(I+1,K,JJM-2)                      &
     &            +HTM(I,K,JJM)+HTM(I+1,K,JJM))
          T(I,K,JJM-1)=(T(I,K,JJM-2)*HTM(I,K,JJM-2)                     &
     &                 +T(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)                 &
     &                 +T(I,K,JJM)*HTM(I,K,JJM)                         &
     &                 +T(I+1,K,JJM)*HTM(I+1,K,JJM))                    &
     &                 *RHTM
          Q(I,K,JJM-1)=(Q(I,K,JJM-2)*HTM(I,K,JJM-2)                     &
     &                 +Q(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)                 &
     &                 +Q(I,K,JJM)*HTM(I,K,JJM)                         &
     &                 +Q(I+1,K,JJM)*HTM(I+1,K,JJM))                    &
     &                 *RHTM
          Q2(I,K,JJM-1)=(Q2(I,K,JJM-2)*HTM(I,K,JJM-2)                   &
     &                  +Q2(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)               &
     &                  +Q2(I,K,JJM)*HTM(I,K,JJM)                       &
     &                  +Q2(I+1,K,JJM)*HTM(I+1,K,JJM))                  &
     &                  *RHTM
          CWM(I,K,JJM-1)=(CWM(I,K,JJM-2)*HTM(I,K,JJM-2)                 &
     &                   +CWM(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)             &
     &                   +CWM(I,K,JJM)*HTM(I,K,JJM)                     &
     &                   +CWM(I+1,K,JJM)*HTM(I+1,K,JJM))                &
     &                   *RHTM
          PINT(I,K,JJM-1)=ETA1(K)*PDTOP                                 &
     &                   +ETA2(K)*PD(I,JJM-1)*RES(I,JJM-1)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(W_BDY)THEN
        DO J=4,JM-3,2
!
          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            RHTM=1./(HTM(1,K,JJ-1)+HTM(2,K,JJ-1)                        &
     &              +HTM(1,K,JJ+1)+HTM(2,K,JJ+1))
            T(1,K,JJ)=(T(1,K,JJ-1)*HTM(1,K,JJ-1)                        &
     &                +T(2,K,JJ-1)*HTM(2,K,JJ-1)                        &
     &                +T(1,K,JJ+1)*HTM(1,K,JJ+1)                        &
     &                +T(2,K,JJ+1)*HTM(2,K,JJ+1))                       &
     &                *RHTM
            Q(1,K,JJ)=(Q(1,K,JJ-1)*HTM(1,K,JJ-1)                        &
     &                +Q(2,K,JJ-1)*HTM(2,K,JJ-1)                        &
     &                +Q(1,K,JJ+1)*HTM(1,K,JJ+1)                        &
     &                +Q(2,K,JJ+1)*HTM(2,K,JJ+1))                       &
     &                *RHTM
            Q2(1,K,JJ)=(Q2(1,K,JJ-1)*HTM(1,K,JJ-1)                      &
     &                 +Q2(2,K,JJ-1)*HTM(2,K,JJ-1)                      &
     &                 +Q2(1,K,JJ+1)*HTM(1,K,JJ+1)                      &
     &                 +Q2(2,K,JJ+1)*HTM(2,K,JJ+1))                     &
     &                 *RHTM
            CWM(1,K,JJ)=(CWM(1,K,JJ-1)*HTM(1,K,JJ-1)                    &
     &                  +CWM(2,K,JJ-1)*HTM(2,K,JJ-1)                    &
     &                  +CWM(1,K,JJ+1)*HTM(1,K,JJ+1)                    &
     &                  +CWM(2,K,JJ+1)*HTM(2,K,JJ+1))                   &
     &                  *RHTM
            PINT(1,K,JJ)=ETA1(K)*PDTOP                                  &
     &                  +ETA2(K)*PD(1,JJ)*RES(1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(E_BDY)THEN
        DO J=4,JM-3,2
!
          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            RHTM=1./(HTM(IIM-1,K,JJ-1)+HTM(IIM,K,JJ-1)                  &
     &              +HTM(IIM-1,K,JJ+1)+HTM(IIM,K,JJ+1))
            T(IIM-1,K,JJ)=(T(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)            &
     &                    +T(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)                &
     &                    +T(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)            &
     &                    +T(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))               &
     &                    *RHTM
            Q(IIM-1,K,JJ)=(Q(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)            &
     &                    +Q(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)                &
     &                    +Q(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)            &
     &                    +Q(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))               &
     &                    *RHTM
            Q2(IIM-1,K,JJ)=(Q2(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)          &
     &                     +Q2(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)              &
     &                     +Q2(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)          &
     &                     +Q2(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))             &
     &                     *RHTM
            CWM(IIM-1,K,JJ)=(CWM(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)        &
     &                      +CWM(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)            &
     &                      +CWM(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)        &
     &                      +CWM(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))           &
     &                      *RHTM
            PINT(IIM-1,K,JJ)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(IIM-1,JJ)*RES(IIM-1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
  200 CONTINUE
!
!-----------------------------------------------------------------------
      END SUBROUTINE BOCOH
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE BOCOV(NTSD,DT,LB,VTM,U_B,V_B,U_BT,V_BT                 &
     &                ,U,V                                              &
     &                ,IJDS,IJDE,SPEC_BDY_WIDTH                         &  ! min/max(id,jd)
     &                ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    BOCOV       UPDATE WIND POINTS ON BOUNDARY
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
!     
! ABSTRACT:
!     U AND V COMPONENTS OF THE WIND ARE UPDATED ON THE
!     DOMAIN BOUNDARY BY APPLYING THE PRE-COMPUTED
!     TENDENCIES AT EACH TIME STEP.  AN EXTRAPOLATION FROM
!     INSIDE THE DOMAIN IS USED FOR THE COMPONENT TANGENTIAL
!     TO THE BOUNDARY IF THE NORMAL COMPONENT IS OUTWARD.
!     
! PROGRAM HISTORY LOG:
!   87-??-??  MESINGER   - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   01-03-13  BLACK      - CONVERTED TO WRF STRUCTURE
!   02-09-06  WOLFE      - MORE CONVERSION TO GLOBAL INDEXING 
!   04-11-23  BLACK      - THREADED
!     
! USAGE: CALL BOCOH FROM SUBROUTINE SOLVE_NMM
!   INPUT ARGUMENT LIST:
!
!     NOTE THAT IDE AND JDE INSIDE ROUTINE SHOULD BE PASSED IN
!     AS WHAT WRF CONSIDERS THE UNSTAGGERED GRID DIMENSIONS; THAT
!     IS, 1 LESS THAN THE IDE AND JDE SET BY WRF FRAMEWORK, JM
!  
!   OUTPUT ARGUMENT LIST: 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!  
!     UNIQUE: NONE
!  
!     LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: IJDS,IJDE,SPEC_BDY_WIDTH
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NMM_MAX_DIM is set in configure.wrf and must agree with
! the value of dimspec q in the Registry/Registry
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: LB,NTSD
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: VTM
!
      REAL,DIMENSION(IJDS:IJDE,KMS:KME,SPEC_BDY_WIDTH,4),INTENT(INOUT)  &
     &                                         :: U_B,V_B,U_BT,V_BT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: U,V
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,II,IIM,IM,J,JJ,JJM,JM,K,N
      INTEGER :: MY_IS_GLB, MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: IBDY,BF,JB,IB
      INTEGER :: ILPAD1,IRPAD1,JBPAD1,JTPAD1
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  TIME INTERPOLATION OF U AND V AT THE OUTER BOUNDARY
!-----------------------------------------------------------------------
!
      IM=IDE-IDS+1
      JM=JDE-JDS+1
      IIM=IM
      JJM=JM
!
      W_BDY=(ITS==IDS)
      E_BDY=(ITE==IDE)
      S_BDY=(JTS==JDS)
      N_BDY=(JTE==JDE)
!
      ILPAD1=1
      IF(ITS==IDS)ILPAD1=0
      IRPAD1=1
      IF(ITE==IDE)ILPAD1=0
      JBPAD1=1
      IF(JTS==JDS)JBPAD1=0
      JTPAD1=1
      IF(JTE==JDE)JTPAD1=0
!
      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE
!
!-----------------------------------------------------------------------
!***  SOUTH AND NORTH BOUNDARIES
!***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH.
!-----------------------------------------------------------------------
!
      DO IBDY=1,2  
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((S_BDY.AND.IBDY==1).OR.(N_BDY.AND.IBDY==2))THEN
!
          IF(IBDY==1)THEN 
            BF=P_YSB     ! Which boundary (YSB=the boundary where Y is at its start)
            JB=1         ! Which cell in from Boundary 
            JJ=1         ! Which cell in the Domain
          ELSE
            BF=P_YEB     ! Which boundary (YEB=the boundary where Y is at its end)
            JB=1         ! Which cell in from Boundary
            JJ=JJM       ! Which cell in the Domain
          ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_B(I,K,JB,BF)=U_B(I,K,JB,BF)+U_BT(I,K,JB,BF)*DT
              V_B(I,K,JB,BF)=V_B(I,K,JB,BF)+V_BT(I,K,JB,BF)*DT
              U(I,K,JJ)=U_B(I,K,JB,BF)
              V(I,K,JJ)=V_B(I,K,JB,BF)
            ENDDO
          ENDDO
!
        ENDIF
      ENDDO

!
!-----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!***  USE IBDY=1 FOR WEST; 2 FOR EAST.
!-----------------------------------------------------------------------
!
      DO IBDY=1,2    
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        IF((W_BDY.AND.IBDY==1).OR.(E_BDY.AND.IBDY==2))THEN
!
          IF(IBDY==1)THEN 
            BF=P_XSB     ! Which boundary (YSB=the boundary where Y is at its start)
            IB=1         ! Which cell in from boundary
            II=1         ! Which cell in the domain
          ELSE
            BF=P_XEB     ! Which boundary (YEB=the boundary where Y is at its end)
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain
          ENDIF
!
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2)==0)THEN
                U_B(J,K,IB,BF)=U_B(J,K,IB,BF)+U_BT(J,K,IB,BF)*DT
                V_B(J,K,IB,BF)=V_B(J,K,IB,BF)+V_BT(J,K,IB,BF)*DT
                U(II,K,J)=U_B(J,K,IB,BF)
                V(II,K,J)=V_B(J,K,IB,BF)
              ENDIF
            ENDDO
          ENDDO
!
        ENDIF
      ENDDO

!
!-----------------------------------------------------------------------
!***  EXTRAPOLATION OF TANGENTIAL VELOCITY AT OUTFLOW POINTS
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 200 K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!***  SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS1_P1,MYIE2_P1
          IF(V(I,K,1)<0.)U(I,K,1)=(VTM(I,K,5)+1.)*U(I,K,3)              &
     &                            -VTM(I,K,5)    *U(I,K,5)
        ENDDO
      ENDIF
!
!***  NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS1_P1,MYIE2_P1
          IF(V(I,K,JJM)>0.)                                             &
     &        U(I,K,JJM)=(VTM(I,K,JJM-4)+1.)*U(I,K,JJM-2)               &
     &                   -VTM(I,K,JJM-4)    *U(I,K,JJM-4)
        ENDDO
      ENDIF
!
!***  WESTERN BOUNDARY
!
      DO J=4,JM-3,2
        IF(W_BDY)THEN
!
          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            IF(U(1,K,JJ)<0.)                                            &
     &          V(1,K,JJ)=(VTM(3,K,JJ)+1.)*V(2,K,JJ)                    &
     &                    -VTM(3,K,JJ)    *V(3,K,JJ)
          ENDIF
!
        ENDIF
      ENDDO
!
!***  EASTERN BOUNDARY
!
      DO J=4,JM-3,2
        IF(E_BDY)THEN
!
          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            IF(U(IIM,K,JJ)>0.)                                          &
     &          V(IIM,K,JJ)=(VTM(IIM-2,K,JJ)+1.)*V(IIM-1,K,JJ)          &
     &                      -VTM(IIM-2,K,JJ)    *V(IIM-2,K,JJ)
          ENDIF
!
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
!
  200 CONTINUE
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 300 K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!***  SOUTHWEST CORNER
!
      IF(S_BDY.AND.W_BDY)THEN
        U(2,K,2)=D06666*(4.*(U(1,K,1)+U(2,K,1)+U(2,K,3))                &
     &                     + U(1,K,2)+U(1,K,4)+U(2,K,4))               
        V(2,K,2)=D06666*(4.*(V(1,K,1)+V(2,K,1)+V(2,K,3))                &
     &                      +V(1,K,2)+V(1,K,4)+V(2,K,4))
      ENDIF
!
!***  SOUTHEAST CORNER
!
      IF(S_BDY.AND.E_BDY)THEN
        U(IIM-1,K,2)=D06666*(4.*(U(IIM-2,K,1)+U(IIM-1,K,1)              &
     &                          +U(IIM-2,K,3))                          &
     &                          +U(IIM,K,2)+U(IIM,K,4)+U(IIM-1,K,4))
        V(IIM-1,K,2)=D06666*(4.*(V(IIM-2,K,1)+V(IIM-1,K,1)              &
     &                          +V(IIM-2,K,3))                          &
     &                          +V(IIM,K,2)+V(IIM,K,4)+V(IIM-1,K,4))
      ENDIF
!
!***  NORTHWEST CORNER
!
      IF(N_BDY.AND.W_BDY)THEN
        U(2,K,JJM-1)=D06666*(4.*(U(1,K,JJM)+U(2,K,JJM)+U(2,K,JJM-2))    &
     &                          +U(1,K,JJM-1)+U(1,K,JJM-3)              &
     &                          +U(2,K,JJM-3))
        V(2,K,JJM-1)=D06666*(4.*(V(1,K,JJM)+V(2,K,JJM)+V(2,K,JJM-2))    &
     &                          +V(1,K,JJM-1)+V(1,K,JJM-3)              &
     &                          +V(2,K,JJM-3))
      ENDIF
!
!***  NORTHEAST CORNER
!
      IF(N_BDY.AND.E_BDY)THEN
        U(IIM-1,K,JJM-1)=                                               &
     &    D06666*(4.*(U(IIM-2,K,JJM)+U(IIM-1,K,JJM)+U(IIM-2,K,JJM-2))   &
     &               +U(IIM,K,JJM-1)+U(IIM,K,JJM-3)+U(IIM-1,K,JJM-3))
        V(IIM-1,K,JJM-1)=                                               &
     &    D06666*(4.*(V(IIM-2,K,JJM)+V(IIM-1,K,JJM)+V(IIM-2,K,JJM-2))   &
     &               +V(IIM,K,JJM-1)+V(IIM,K,JJM-3)+V(IIM-1,K,JJM-3))
      ENDIF
!
!-----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY
!-----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS2,MYIE2
          U(I,K,2)=(U(I-1,K,1)+U(I,K,1)+U(I-1,K,3)+U(I,K,3))*0.25
          V(I,K,2)=(V(I-1,K,1)+V(I,K,1)+V(I-1,K,3)+V(I,K,3))*0.25
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS2,MYIE2
          U(I,K,JJM-1)=(U(I-1,K,JJM-2)+U(I,K,JJM-2)                     &
     &                 +U(I-1,K,JJM)+U(I,K,JJM))*0.25
          V(I,K,JJM-1)=(V(I-1,K,JJM-2)+V(I,K,JJM-2)                     &
     &                 +V(I-1,K,JJM)+V(I,K,JJM))*0.25
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      DO J=3,JM-2,2
        IF(W_BDY)THEN
          IF(W_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            U(1,K,JJ)=(U(1,K,JJ-1)+U(2,K,JJ-1)                          &
     &                +U(1,K,JJ+1)+U(2,K,JJ+1))*0.25
            V(1,K,JJ)=(V(1,K,JJ-1)+V(2,K,JJ-1)                          &
     &                +V(1,K,JJ+1)+V(2,K,JJ+1))*0.25
          ENDIF
        ENDIF
      ENDDO
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(E_BDY)THEN
        DO J=3,JM-2,2
          IF(E_BDY.AND.J>=MY_JS_GLB-JBPAD1                              &
     &            .AND.J<=MY_JE_GLB+JTPAD1)THEN
            JJ=J
            U(IIM-1,K,JJ)=0.25*(U(IIM-1,K,JJ-1)+U(IIM,K,JJ-1)           &
     &                         +U(IIM-1,K,JJ+1)+U(IIM,K,JJ+1))
            V(IIM-1,K,JJ)=0.25*(V(IIM-1,K,JJ-1)+V(IIM,K,JJ-1)           &
     &                         +V(IIM-1,K,JJ+1)+V(IIM,K,JJ+1))
          ENDIF
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
  300 CONTINUE
!
!-----------------------------------------------------------------------
      END SUBROUTINE BOCOV
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
      END MODULE MODULE_BNDRY_COND
!-----------------------------------------------------------------------

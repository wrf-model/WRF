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
      USE MODULE_MODEL_CONSTANTS
!-----------------------------------------------------------------------
      REAL :: D06666=0.06666666
!-----------------------------------------------------------------------
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE MASS_BOUNDARY(GRIDID,NTSD,DT0,NEST,NBC,NBOCO,LAST_TIME,TSPH    & 
     &                ,LB,ETA1,ETA2,PDTOP,PT,RES                        &
     &                ,PD_BXS, PD_BXE, PD_BYS, PD_BYE                   &
     &                ,T_BXS, T_BXE, T_BYS, T_BYE                       &
     &                ,Q_BXS, Q_BXE, Q_BYS, Q_BYE                       &
     &                ,U_BXS, U_BXE, U_BYS, U_BYE                       &
     &                ,V_BXS, V_BXE, V_BYS, V_BYE                       &
     &                ,Q2_BXS, Q2_BXE, Q2_BYS, Q2_BYE                   &
     &                ,PD_BTXS, PD_BTXE, PD_BTYS, PD_BTYE               &
     &                ,T_BTXS, T_BTXE, T_BTYS, T_BTYE                   &
     &                ,Q_BTXS, Q_BTXE, Q_BTYS, Q_BTYE                   &
     &                ,U_BTXS, U_BTXE, U_BTYS, U_BTYE                   &
     &                ,V_BTXS, V_BTXE, V_BTYS, V_BTYE                   &
     &                ,Q2_BTXS, Q2_BTXE, Q2_BTYS, Q2_BTYE               &
     &                ,PD,T,Q,Q2,PINT                                   &
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
     &                ,SM_BXS, SM_BXE, SM_BYS, SM_BYE                   &
     &                ,SM_BTXS, SM_BTXE, SM_BTYS, SM_BTYE               &
     &                ,SM                                               &
     &                ,TH                                               &
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
     &                ,SPEC_BDY_WIDTH,Z                                 &  
     &                ,IHE,IHW,IVE,IVW                                  &
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
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
!   06-06-02  GOPAL      - MODIFICATIONS FOR NESTING
!   07-11-14  PYLE       - UPDATED FOR NEW WRF BOUNDARY FILE STRUCTURE
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
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,INTENT(IN) :: GRIDID
      INTEGER,INTENT(IN) :: LB,NBC,NTSD
      LOGICAL,INTENT(IN) :: LAST_TIME
      INTEGER,INTENT(INOUT) :: NBOCO
!
      REAL,INTENT(IN) :: DT0,PDTOP,PT,TSPH
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,1,SPEC_BDY_WIDTH)                          &
     &                           ,INTENT(INOUT) :: PD_BYS, PD_BYE       &
     &                                            ,PD_BTYS,PD_BTYE
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
      REAL,DIMENSION(IMS:IME,1,SPEC_BDY_WIDTH)                          &
     &                           ,INTENT(INOUT) :: SM_BYS, SM_BYE       &
     &                                            ,SM_BTYS,SM_BTYE
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: T_BYS, T_BYE        &
     &                                            ,U_BYS, U_BYE        &
     &                                            ,V_BYS, V_BYE        &
     &                                            ,Q_BYS, Q_BYE        &
     &                                            ,Q2_BYS, Q2_BYE      &
     &                                            ,T_BTYS, T_BTYE      &
     &                                            ,U_BTYS, U_BTYE      &
     &                                            ,V_BTYS, V_BTYE      &
     &                                            ,Q_BTYS, Q_BTYE      &
     &                                            ,Q2_BTYS, Q2_BTYE    

      REAL,DIMENSION(JMS:JME,1,SPEC_BDY_WIDTH)                         &
     &                           ,INTENT(INOUT) :: PD_BXS, PD_BXE      &
     &                                            ,PD_BTXS,PD_BTXE
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
      REAL,DIMENSION(JMS:JME,1,SPEC_BDY_WIDTH)                         &
     &                           ,INTENT(INOUT) :: SM_BXS, SM_BXE      &
     &                                            ,SM_BTXS,SM_BTXE
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: T_BXS, T_BXE        &
     &                                            ,U_BXS, U_BXE        &
     &                                            ,V_BXS, V_BXE        &
     &                                            ,Q_BXS, Q_BXE        &
     &                                            ,Q2_BXS, Q2_BXE      &
     &                                            ,T_BTXS, T_BTXE      &
     &                                            ,U_BTXS, U_BTXE      &
     &                                            ,V_BTXS, V_BTXE      &
     &                                            ,Q_BTXS, Q_BTXE      &
     &                                            ,Q2_BTXS, Q2_BTXE    
                                               
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SM,TH
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) ::          &
     &                                                         PINT,Q   &
     &                                                        ,Q2,T,Z
!
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,IB,IBDY,II,IIM,IM,IRTN,ISIZ1,ISIZ2                &
     &          ,J,JB,JJ,JJM,JM,K,KK,N,NN,NREC,NUMGAS,NV,REC
      INTEGER :: MY_IS_GLB,MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: I_M,ILPAD1,IRPAD1,JBPAD1,JTPAD1
!
      REAL :: BCHR,CONVFAC,CWK,DT,PLYR,RRI
!
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY, activate
!
      CHARACTER(LEN=255) :: message
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
      ns_do: DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        activate=.false.
        ns_if: IF(S_BDY.AND.IBDY==1) THEN 
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
            activate=.true.
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYS(I,1,JB)=PD_BYS(I,1,JB)+PD_BTYS(I,1,JB)*DT
            PD(I,JJ)=PD_BYS(I,1,JB)
          ENDDO

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYS(I,K,JB)=T_BYS(I,K,JB)+T_BTYS(I,K,JB)*DT
              Q_BYS(I,K,JB)=Q_BYS(I,K,JB)+Q_BTYS(I,K,JB)*DT
              Q2_BYS(I,K,JB)=Q2_BYS(I,K,JB)+Q2_BTYS(I,K,JB)*DT
!
              T(I,JJ,K)=T_BYS(I,K,JB)
              Q(I,JJ,K)=Q_BYS(I,K,JB)
              Q2(I,JJ,K)=Q2_BYS(I,K,JB)
              PINT(I,JJ,K)=ETA1(K)*PDTOP                                &
     &                    +ETA2(K)*PD(I,JJ)*RES(I,JJ)+PT
            ENDDO
           ENDDO

          ELSEIF(N_BDY.AND.IBDY==2) THEN
            JB=1         ! Which cell in from boundary
            JJ=JJM       ! Which cell in the domain
            activate=.true.
!
          DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
            PD_BYE(I,1,JB)=PD_BYE(I,1,JB)+PD_BTYE(I,1,JB)*DT
            PD(I,JJ)=PD_BYE(I,1,JB)
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              T_BYE(I,K,JB)=T_BYE(I,K,JB)+T_BTYE(I,K,JB)*DT
              Q_BYE(I,K,JB)=Q_BYE(I,K,JB)+Q_BTYE(I,K,JB)*DT
              Q2_BYE(I,K,JB)=Q2_BYE(I,K,JB)+Q2_BTYE(I,K,JB)*DT
!
              T(I,JJ,K)=T_BYE(I,K,JB)
              Q(I,JJ,K)=Q_BYE(I,K,JB)
              Q2(I,JJ,K)=Q2_BYE(I,K,JB)
              PINT(I,JJ,K)=ETA1(K)*PDTOP                                &
     &                    +ETA2(K)*PD(I,JJ)*RES(I,JJ)+PT
            ENDDO
          ENDDO
        ENDIF ns_if

      ENDDO ns_do
!
!-----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST. 
!
      ew_do: DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        activate=.false.
        ew_if: IF(W_BDY.AND.IBDY==1) THEN  
            IB=1         ! Which cell in from boundary 
            II=1         ! Which cell in the domain
            activate=.true.
          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
            IF(MOD(J,2)==1)THEN
              PD_BXS(J,1,IB)=PD_BXS(J,1,IB)+PD_BTXS(J,1,IB)*DT
              PD(II,J)=PD_BXS(J,1,IB)
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
              IF(GRIDID.GT.1)THEN
                SM(II,J)=SM_BXS(J,1,IB)   ! for W-E motion
!               TH(II,J)=TH_BXS(J,1,IB)
              ENDIF
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
            ENDIF
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
!
              IF(MOD(J,2)==1)THEN
                T_BXS(J,K,IB)=T_BXS(J,K,IB)+T_BTXS(J,K,IB)*DT
                Q_BXS(J,K,IB)=Q_BXS(J,K,IB)+Q_BTXS(J,K,IB)*DT
                Q2_BXS(J,K,IB)=Q2_BXS(J,K,IB)+Q2_BTXS(J,K,IB)*DT
!
                T(II,J,K)=T_BXS(J,K,IB)
                Q(II,J,K)=Q_BXS(J,K,IB)
                Q2(II,J,K)=Q2_BXS(J,K,IB)
                PINT(II,J,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(II,J)*RES(II,J)+PT
              ENDIF
!
            ENDDO
          ENDDO

          ELSEIF(E_BDY.AND.IBDY==2) THEN
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain
            activate=.true.
          DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
            IF(MOD(J,2)==1)THEN
              PD_BXE(J,1,IB)=PD_BXE(J,1,IB)+PD_BTXE(J,1,IB)*DT
              PD(II,J)=PD_BXE(J,1,IB)
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
              IF(GRIDID.GT.1)THEN
                 SM(II,J)=SM_BXE(J,1,IB)    ! for E-W motion
!                TH(II,J)=TH_BXE(J,1,IB)
              ENDIF
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
            ENDIF
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
!
              IF(MOD(J,2)==1)THEN
                T_BXE(J,K,IB)=T_BXE(J,K,IB)+T_BTXE(J,K,IB)*DT
                Q_BXE(J,K,IB)=Q_BXE(J,K,IB)+Q_BTXE(J,K,IB)*DT
                Q2_BXE(J,K,IB)=Q2_BXE(J,K,IB)+Q2_BTXE(J,K,IB)*DT
!
                T(II,J,K)=T_BXE(J,K,IB)
                Q(II,J,K)=Q_BXE(J,K,IB)
                Q2(II,J,K)=Q2_BXE(J,K,IB)
                PINT(II,J,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(II,J)*RES(II,J)+PT
              ENDIF
!
            ENDDO
          ENDDO

       ENDIF ew_if

      ENDDO ew_do
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
          CWK=PD(I,2)
          PD(I,2)=0.25*(PD(I,1)+PD(I+1,1)+PD(I,3)+PD(I+1,3))
!
!***  NESTING TEST
!
          IF(I<=IDE-1.AND.ABS(CWK-PD(I,2))>=300.)THEN
            WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE SOUTHERN BOUNDARY AT',I,2,'GRID #',GRIDID
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',CWK/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,3)/100.,'               ',PD(I+1,3)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',PD(I,2)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,1)/100.,'             ',PD(I+1,1)/100.
            CALL wrf_message(trim(message))
            CALL wrf_message('   ')
          ENDIF

        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN

        DO I=MYIS,MYIE1
          CWK=PD(I,JJM-1)
!!        write(message,*)'I, PD is:', PD(I,JJM), PD(I,JJM-2),PD(I+1,JJM-2),PD(I+1,JJM)
!!        call wrf_message(trim(message))
          PD(I,JJM-1)=0.25*(PD(I,JJM-2)+PD(I+1,JJM-2)                   &
     &                     +PD(I,JJM)+PD(I+1,JJM))
!
!***  NESTING TEST
!
          IF(I<=IDE-1.AND.ABS(CWK-PD(I,JJM-1))>=300.)THEN
            WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE NORTHERN BOUNDARY AT',I,JJM-1,'GRID #',GRIDID
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',CWK/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,JJM)/100.,'               ',PD(I+1,JJM)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)'             ',PD(I,JJM-1)/100.
            CALL wrf_message(trim(message))
            WRITE(message,*)PD(I,JJM-2)/100.,'             ',PD(I+1,JJM-2)/100.
            CALL wrf_message(trim(message))
            CALL wrf_message('   ')
          ENDIF

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
            CWK=PD(1,J)
            JJ=J
            PD(1,JJ)=0.25*(PD(1,JJ-1)+PD(2,JJ-1)+PD(1,JJ+1)+PD(2,JJ+1))
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
           IF(GRIDID.GT.1)THEN
             SM(1,JJ)=0.5*(SM(1,JJ-1)+SM(1,JJ+1))  ! updates only along W Boundary
!            TH(IIM-1,JJ)=0.5*(TH(IIM,JJ-1)+TH(IIM,JJ+1))
           ENDIF
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
!
!***  NESTING TEST
!
             IF(ABS(CWK-PD(1,JJ))>300.)THEN
              WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE WESTERN BOUNDARY AT',1,JJ,'GRID #',GRIDID
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',CWK/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(1,JJ+1)/100.,'               ',PD(2,JJ+1)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',PD(1,JJ)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(1,JJ-1)/100.,'               ',PD(2,JJ-1)/100.
              CALL wrf_message(trim(message))
              CALL wrf_message('   ')
            ENDIF

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
            CWK=PD(IIM-1,J)
            JJ=J
            PD(IIM-1,JJ)=0.25*(PD(IIM-1,JJ-1)+PD(IIM,JJ-1)              &
     &                        +PD(IIM-1,JJ+1)+PD(IIM,JJ+1))
!!BEGIN: LSM changes for LANDFALL: Subashini 7/27/2016
#ifdef IDEAL_NMM_TC
           IF(GRIDID.GT.1)THEN
             SM(IIM-1,JJ)=0.5*(SM(IIM,JJ-1)+SM(IIM,JJ+1))  ! updates only along E Boundary
!            TH(IIM-1,JJ)=0.5*(TH(IIM,JJ-1)+TH(IIM,JJ+1))
           ENDIF
#endif
!!END: LSM changes for LANDFALL : Subashini 7/27/2016
!
!***  NESTING TEST
!
             IF(ABS(CWK-PD(IIM-1,JJ))>300.)THEN
              WRITE(message,*)'PSEUDO HYDROSTATIC IMBALANCE AT THE EASTERN BOUNDARY AT',IIM-1,JJ,'GRID #',GRIDID
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',CWK/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(IIM-1,JJ+1)/100.,'               ',PD(IIM,JJ+1)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)'             ',PD(IIM-1,JJ)/100.
              CALL wrf_message(trim(message))
              WRITE(message,*)PD(IIM-1,JJ-1)/100.,'               ',PD(IIM,JJ-1)/100.
              CALL wrf_message(trim(message))
              CALL wrf_message('   ')
            ENDIF

          ENDIF
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO 200 K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS,MYIE1
          T(I,2,K)=(T(I,1,K)+T(I+1,1,K)+T(I,3,K)+T(I+1,3,K))*0.25
          Q(I,2,K)=(Q(I,1,K)+Q(I+1,1,K)+Q(I,3,K)+Q(I+1,3,K))*0.25
          Q2(I,2,K)=(Q2(I,1,K)+Q2(I+1,1,K)+Q2(I,3,K)+Q2(I+1,3,K))*0.25
          PINT(I,2,K)=ETA1(K)*PDTOP+ETA2(K)*PD(I,2)*RES(I,2)+PT
        ENDDO
!
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS,MYIE1
          T(I,JJM-1,K)=(T(I,JJM-2,K)+T(I+1,JJM-2,K)                     &
     &                 +T(I,JJM,K)+T(I+1,JJM,K))                        &
     &                 *0.25
          Q(I,JJM-1,K)=(Q(I,JJM-2,K)+Q(I+1,JJM-2,K)                     &
     &                 +Q(I,JJM,K)+Q(I+1,JJM,K))                        &
     &                 *0.25
          Q2(I,JJM-1,K)=(Q2(I,JJM-2,K)+Q2(I+1,JJM-2,K)                  &
     &                  +Q2(I,JJM,K)+Q2(I+1,JJM,K))                     &
     &                  *0.25
          PINT(I,JJM-1,K)=ETA1(K)*PDTOP                                 &
     &                   +ETA2(K)*PD(I,JJM-1)*RES(I,JJM-1)+PT
        ENDDO
!
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
            T(1,JJ,K)=(T(1,JJ-1,K)+T(2,JJ-1,K)                          &
     &                +T(1,JJ+1,K)+T(2,JJ+1,K))                         &
     &                *0.25
            Q(1,JJ,K)=(Q(1,JJ-1,K)+Q(2,JJ-1,K)                          &
     &                +Q(1,JJ+1,K)+Q(2,JJ+1,K))                         &
     &                *0.25
            Q2(1,JJ,K)=(Q2(1,JJ-1,K)+Q2(2,JJ-1,K)                       &
     &                 +Q2(1,JJ+1,K)+Q2(2,JJ+1,K))                      &
     &                 *0.25
            PINT(1,JJ,K)=ETA1(K)*PDTOP                                  &
     &                  +ETA2(K)*PD(1,JJ)*RES(1,JJ)+PT
!
          ENDIF
!
        ENDDO
!
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
            T(IIM-1,JJ,K)=(T(IIM-1,JJ-1,K)+T(IIM,JJ-1,K)                &
     &                    +T(IIM-1,JJ+1,K)+T(IIM,JJ+1,K))               &
     &                    *0.25
            Q(IIM-1,JJ,K)=(Q(IIM-1,JJ-1,K)+Q(IIM,JJ-1,K)                &
     &                    +Q(IIM-1,JJ+1,K)+Q(IIM,JJ+1,K))               &
     &                    *0.25
            Q2(IIM-1,JJ,K)=(Q2(IIM-1,JJ-1,K)+Q2(IIM,JJ-1,K)             &
     &                     +Q2(IIM-1,JJ+1,K)+Q2(IIM,JJ+1,K))            &
     &                     *0.25
            PINT(IIM-1,JJ,K)=ETA1(K)*PDTOP                              &
     &                      +ETA2(K)*PD(IIM-1,JJ)*RES(IIM-1,JJ)+PT
!
          ENDIF
!
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
  200 CONTINUE
!
!-----------------------------------------------------------------------
      END SUBROUTINE MASS_BOUNDARY
      SUBROUTINE MP_BULK_BOUNDARY(GRIDID,NTSD,DT0    & 
     &                ,LB,ETA1,ETA2,PDTOP,PT                            &
     &                ,CWM_BXS, CWM_BXE, CWM_BYS, CWM_BYE               &
     &                ,CWM_BTXS, CWM_BTXE, CWM_BTYS, CWM_BTYE           &
     &                ,Q,CWM                                            &
     &                ,MOIST,N_MOIST,SCALAR,N_SCALAR                    &
     &                ,SPEC_BDY_WIDTH                                   &  
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
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
!   06-06-02  GOPAL      - MODIFICATIONS FOR NESTING
!   07-11-14  PYLE       - UPDATED FOR NEW WRF BOUNDARY FILE STRUCTURE
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
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
      INTEGER,INTENT(IN) :: N_MOIST, N_SCALAR
!
      INTEGER,INTENT(IN) :: GRIDID
      INTEGER,INTENT(IN) :: LB,NTSD
!
      REAL,INTENT(IN) :: DT0,PDTOP,PT
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: CWM_BYS, CWM_BYE    &
     &                                            ,CWM_BTYS, CWM_BTYE  

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH)                   &
     &                           ,INTENT(INOUT) :: CWM_BXS, CWM_BXE    &
     &                                            ,CWM_BTXS, CWM_BTXE  
                                               
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM,Q
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,NUM_MOIST)                 &
     &                                           ,INTENT(INOUT) :: MOIST
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,NUM_SCALAR)                &
     &                                          ,INTENT(INOUT) :: SCALAR
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,IB,IBDY,II,IIM,IM,IRTN,ISIZ1,ISIZ2                &
     &          ,J,JB,JJ,JJM,JM,K,KK,N,NN,NREC,NUMGAS,NV,REC
      INTEGER :: MY_IS_GLB,MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: I_M,ILPAD1,IRPAD1,JBPAD1,JTPAD1
      INTEGER :: I1,I2,J1,J2,J1B,J2B
!
      REAL :: BCHR,CONVFAC,CWK,DT,PLYR,RRI
!
      LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY,ACTIVATE
!
      CHARACTER(LEN=255) :: message
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

      ! I loop ends for N/S bdy copying:
      I1=MAX(ITS-1,IDS)
      I2=MIN(ITE+1,IDE)

      ! J loop ends for E/W bdy copying:
      J1=MAX(JTS-1,JDS+3-1)
      IF(MOD(J1,2)/=1) J1=J1+1
      J2=MIN(JTE+1,JDE-2)
      IF(MOD(J2,2)/=1) J2=J2-1

      ! J loop ends for E/W bdy 4-point averaging:
      J1B=MAX(4,MY_JS_GLB-JBPAD1)
      IF(MOD(J1B,2)/=0) J1B=J1B+1
      J2B=MIN(JM-3,MY_JE_GLB+JTPAD1)
      IF(MOD(J2B,2)/=0) J2B=J2B-1
!
!-----------------------------------------------------------------------
!***  SOUTH AND NORTH BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH
!
      ns_do: DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
        activate=.false.
        if_ns: IF(S_BDY.AND.IBDY==1) THEN 
            JB=1         ! Which cell in from boundary
            JJ=1         ! Which cell in the domain
            activate=.true.
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=I1,I2
              CWM_BYS(I,K,JB)=CWM_BYS(I,K,JB)+CWM_BTYS(I,K,JB)*DT
              CWM(I,JJ,K)=CWM_BYS(I,K,JB)
            ENDDO
           ENDDO

          ELSEIF(N_BDY.AND.IBDY==2) THEN
            JB=1         ! Which cell in from boundary
            JJ=JJM       ! Which cell in the domain
            activate=.true.
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=I1,I2
              CWM_BYE(I,K,JB)=CWM_BYE(I,K,JB)+CWM_BTYE(I,K,JB)*DT
              CWM(I,JJ,K)=CWM_BYE(I,K,JB)
            ENDDO
          ENDDO

       ENDIF if_ns   ! for N/S boundaries

       ns_moist: IF(activate) then
          DO I_M=1,N_MOIST
            IF(I_M==P_QV)THEN
!$omp parallel do                                                       &
!$omp& private(i,k)
              DO K=KTS,KTE
              DO I=I1,I2
                MOIST(I,JJ,K,I_M)=Q(I,JJ,K)/(1.-Q(I,JJ,K))
              ENDDO
              ENDDO
            ELSE
!$omp parallel do                                                       &
!$omp& private(i,k)
              DO K=KTS,KTE
              DO I=I1,I2
                MOIST(I,JJ,K,I_M)=0.
              ENDDO
              ENDDO
            ENDIF
          ENDDO
          DO I_M=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,k)
            DO K=KTS,KTE
            DO I=I1,I2
              SCALAR(I,JJ,K,I_M)=0.
            ENDDO
            ENDDO
          ENDDO
       ENDIF ns_moist
      ENDDO ns_do
!
!-----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST. 
!
      ew_do: DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
         activate=.false.
         if_ew: IF(W_BDY.AND.IBDY==1) THEN  
            IB=1         ! Which cell in from boundary 
            II=1         ! Which cell in the domain
            activate=.true.
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=J1,J2,2
!
              IF(MOD(J,2)==1)THEN
                CWM_BXS(J,K,IB)=CWM_BXS(J,K,IB)+CWM_BTXS(J,K,IB)*DT
                CWM(II,J,K)=CWM_BXS(J,K,IB)
              ENDIF
!
            ENDDO
          ENDDO

          ELSEIF(E_BDY.AND.IBDY==2) THEN
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain
            activate=.true.
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=J1,J2,2
!
              IF(MOD(J,2)==1)THEN
                CWM_BXE(J,K,IB)=CWM_BXE(J,K,IB)+CWM_BTXE(J,K,IB)*DT
                CWM(II,J,K)=CWM_BXE(J,K,IB)
              ENDIF
!
            ENDDO
          ENDDO
!
       ENDIF if_ew  ! for W/E boundaries
       ew_moist: if(activate) then
          DO I_M=1,N_MOIST
            IF(I_M==P_QV)THEN
!$omp parallel do                                                       &
!$omp& private(j,k)
              DO K=KTS,KTE
              DO J=J1,J2,2
                IF(MOD(J,2)==1)THEN
                  MOIST(II,J,K,I_M)=Q(II,J,K)/(1.-Q(II,J,K))
                ENDIF
              ENDDO
              ENDDO
!
            ELSE
!$omp parallel do                                                       &
!$omp& private(j,k)
              DO K=KTS,KTE
              DO J=J1,J2,2
                IF(MOD(J,2)==1)THEN
                  MOIST(II,J,K,I_M)=0.
                ENDIF
              ENDDO
              ENDDO
!
            ENDIF
          ENDDO
!
          DO I_M=2,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(j,k)
            DO K=KTS,KTE
            DO J=J1,J2,2
              IF(MOD(J,2)==1)THEN
                SCALAR(II,J,K,I_M)=0.
              ENDIF
            ENDDO
            ENDDO
         ENDDO
      ENDIF ew_moist
   ENDDO ew_do
!
!-----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF PD THEN REMAINING MASS VARIABLES
!***  AT INNER BOUNDARY
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
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(S_BDY)THEN
        DO I=MYIS,MYIE1
          CWM(I,2,K)=(CWM(I,1,K)+CWM(I+1,1,K)+CWM(I,3,K)+CWM(I+1,3,K))  &
     &               *0.25
        ENDDO
!
        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
            DO I=MYIS,MYIE1
              MOIST(I,2,K,I_M)=Q(I,2,K)/(1.-Q(I,2,K))
            ENDDO
          ELSE
            DO I=MYIS,MYIE1
              MOIST(I,2,K,I_M)=(MOIST(I,1,K,I_M)                        &
     &                         +MOIST(I+1,1,K,I_M)                      &
     &                         +MOIST(I,3,K,I_M)                        &
     &                         +MOIST(I+1,3,K,I_M))*0.25
            ENDDO
          ENDIF
        ENDDO
!
        DO I_M=2,N_SCALAR
          DO I=MYIS,MYIE1
            SCALAR(I,2,K,I_M)=(SCALAR(I,1,K,I_M)                        &
     &                        +SCALAR(I+1,1,K,I_M)                      &
     &                        +SCALAR(I,3,K,I_M)                        &
     &                        +SCALAR(I+1,3,K,I_M))*0.25
          ENDDO
        ENDDO
!
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS,MYIE1
          CWM(I,JJM-1,K)=(CWM(I,JJM-2,K)+CWM(I+1,JJM-2,K)               &
     &                   +CWM(I,JJM,K)+CWM(I+1,JJM,K))                  &
     &                   *0.25
        ENDDO
!
        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
            DO I=MYIS,MYIE1
              MOIST(I,JJM-1,K,I_M)=Q(I,JJM-1,K)/(1.-Q(I,JJM-1,K))
            ENDDO
          ELSE
            DO I=MYIS,MYIE1
              MOIST(I,JJM-1,K,I_M)=(MOIST(I,JJM-2,K,I_M)                &
     &                             +MOIST(I+1,JJM-2,K,I_M)              &
     &                             +MOIST(I,JJM,K,I_M)                  &
     &                             +MOIST(I+1,JJM,K,I_M))*0.25
            ENDDO

          ENDIF
        ENDDO
!
        DO I_M=2,N_SCALAR
          DO I=MYIS,MYIE1
            SCALAR(I,JJM-1,K,I_M)=(SCALAR(I,JJM-2,K,I_M)                &
     &                            +SCALAR(I+1,JJM-2,K,I_M)              &
     &                            +SCALAR(I,JJM,K,I_M)                  &
     &                            +SCALAR(I+1,JJM,K,I_M))*0.25
          ENDDO
        ENDDO
!
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(W_BDY)THEN
        DO J=J1B,J2B,2
            JJ=J
            CWM(1,JJ,K)=(CWM(1,JJ-1,K)+CWM(2,JJ-1,K)                    &
     &                  +CWM(1,JJ+1,K)+CWM(2,JJ+1,K))                   &
     &                  *0.25
!
            DO I_M=1,N_MOIST
              IF(I_M==P_QV)THEN
                MOIST(1,JJ,K,I_M)=Q(1,JJ,K)/(1.-Q(1,JJ,K))     
              ELSE  
                MOIST(1,JJ,K,I_M)=(MOIST(1,JJ-1,K,I_M)                  &
     &                            +MOIST(2,JJ-1,K,I_M)                  &
     &                            +MOIST(1,JJ+1,K,I_M)                  &
     &                            +MOIST(2,JJ+1,K,I_M))*0.25
              ENDIF
            ENDDO    
!
            DO I_M=2,N_SCALAR
              SCALAR(1,JJ,K,I_M)=(SCALAR(1,JJ-1,K,I_M)                  &
     &                           +SCALAR(2,JJ-1,K,I_M)                  &
     &                           +SCALAR(1,JJ+1,K,I_M)                  &
     &                           +SCALAR(2,JJ+1,K,I_M))*0.25
            ENDDO
!
        ENDDO
!
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(E_BDY)THEN
        DO J=J1B,J2B,2
            JJ=J
            CWM(IIM-1,JJ,K)=(CWM(IIM-1,JJ-1,K)+CWM(IIM,JJ-1,K)          &
     &                      +CWM(IIM-1,JJ+1,K)+CWM(IIM,JJ+1,K))         &
     &                      *0.25
!
            DO I_M=1,N_MOIST
              IF(I_M==P_QV)THEN
                MOIST(IIM-1,JJ,K,I_M)=Q(IIM-1,JJ,K)/(1.-Q(IIM-1,JJ,K))
              ELSE
                MOIST(IIM-1,JJ,K,I_M)=(MOIST(IIM-1,JJ-1,K,I_M)                   &
     &                                +MOIST(IIM,JJ-1,K,I_M)                     &
     &                                +MOIST(IIM-1,JJ+1,K,I_M)                   &
     &                                +MOIST(IIM,JJ+1,K,I_M))*0.25
                ENDIF
              ENDDO
!
              DO I_M=2,N_SCALAR
                SCALAR(IIM-1,JJ,K,I_M)=(SCALAR(IIM-1,JJ-1,K,I_M)                    &
     &                                 +SCALAR(IIM,JJ-1,K,I_M)                      &
     &                                 +SCALAR(IIM-1,JJ+1,K,I_M)                    &
     &                                 +SCALAR(IIM,JJ+1,K,I_M))*0.25
              ENDDO
!
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
  200 CONTINUE
!
!-----------------------------------------------------------------------
      END SUBROUTINE MP_BULK_BOUNDARY
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE BOCOV(GRIDID,NTSD,DT,LB                                &
     &                ,U_BXS,U_BXE,U_BYS,U_BYE                         &  
     &                ,V_BXS,V_BXE,V_BYS,V_BYE                         &  
     &                ,U_BTXS,U_BTXE,U_BTYS,U_BTYE                     &  
     &                ,V_BTXS,V_BTXE,V_BTYS,V_BTYE                     &  
     &                ,U,V                                              &
     &                ,SPEC_BDY_WIDTH                                   &  
     &                ,IHE,IHW,IVE,IVW                                  &
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
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
!   06-06-02  GOPAL      - MODIFICATIONS FOR NESTING
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
      INTEGER,INTENT(IN) :: SPEC_BDY_WIDTH
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,INTENT(IN) :: GRIDID
      INTEGER,INTENT(IN) :: LB,NTSD
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,SPEC_BDY_WIDTH),INTENT(INOUT) ::     &
     &                                          U_BYS,U_BYE,V_BYS,V_BYE &
     &                                         ,U_BTYS,U_BTYE           &
     &                                         ,V_BTYS,V_BTYE           

      REAL,DIMENSION(JMS:JME,KMS:KME,SPEC_BDY_WIDTH),INTENT(INOUT) ::     &
     &                                          U_BXS,U_BXE,V_BXS,V_BXE &
     &                                         ,U_BTXS,U_BTXE           &
     &                                         ,V_BTXS,V_BTXE 
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: U,V
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,II,IIM,IM,J,JJ,JJM,JM,K,N
      INTEGER :: MY_IS_GLB, MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
      INTEGER :: IBDY,JB,IB
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
        IF(S_BDY.AND.IBDY==1) THEN
!
            JB=1         ! Which cell in from Boundary 
            JJ=1         ! Which cell in the Domain
!
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYS(I,K,JB)=U_BYS(I,K,JB)+U_BTYS(I,K,JB)*DT
              V_BYS(I,K,JB)=V_BYS(I,K,JB)+V_BTYS(I,K,JB)*DT
              U(I,JJ,K)=U_BYS(I,K,JB)
              V(I,JJ,K)=V_BYS(I,K,JB)
            ENDDO
          ENDDO
!

          ELSEIF(N_BDY.AND.IBDY==2) THEN
            JB=1         ! Which cell in from Boundary
            JJ=JJM       ! Which cell in the Domain

!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE
            DO I=MAX(ITS-1,IDS),MIN(ITE+1,IDE)
              U_BYE(I,K,JB)=U_BYE(I,K,JB)+U_BTYE(I,K,JB)*DT
              V_BYE(I,K,JB)=V_BYE(I,K,JB)+V_BTYE(I,K,JB)*DT
              U(I,JJ,K)=U_BYE(I,K,JB)
              V(I,JJ,K)=V_BYE(I,K,JB)
            ENDDO
          ENDDO


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
        IF(W_BDY.AND.IBDY==1) THEN
            IB=1         ! Which cell in from boundary
            II=1         ! Which cell in the domain
!
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2)==0)THEN
                U_BXS(J,K,IB)=U_BXS(J,K,IB)+U_BTXS(J,K,IB)*DT
                V_BXS(J,K,IB)=V_BXS(J,K,IB)+V_BTXS(J,K,IB)*DT
                U(II,J,K)=U_BXS(J,K,IB)
                V(II,J,K)=V_BXS(J,K,IB)
              ENDIF
            ENDDO
          ENDDO

        ELSEIF (E_BDY.AND.IBDY==2) THEN
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain

!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=MAX(JTS-1,JDS+2-1),MIN(JTE+1,JDE-1)
              IF(MOD(J,2)==0)THEN
                U_BXE(J,K,IB)=U_BXE(J,K,IB)+U_BTXE(J,K,IB)*DT
                V_BXE(J,K,IB)=V_BXE(J,K,IB)+V_BTXE(J,K,IB)*DT
                U(II,J,K)=U_BXE(J,K,IB)
                V(II,J,K)=V_BXE(J,K,IB)
              ENDIF
            ENDDO
          ENDDO

!
        ENDIF



      ENDDO

!
!-----------------------------------------------------------------------
!***  EXTRAPOLATION OF TANGENTIAL VELOCITY AT OUTFLOW POINTS
!***  BASED ON SOME DISCUSSIONS WITH ZAVISA, AND MY EXPERIMENTS
!***  ON GRAVITY PULSE FOR NESTED DOMAIN.
!-----------------------------------------------------------------------
!
      IF(GRIDID/=1)GO TO 201
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
          IF(V(I,1,K)<0.)U(I,1,K)=2.*U(I,3,K)-U(I,5,K)
        ENDDO
      ENDIF
!
!***  NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS1_P1,MYIE2_P1
          IF(V(I,JJM,K)>0.)                                             &
     &        U(I,JJM,K)=2.*U(I,JJM-2,K)-U(I,JJM-4,K)
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
            IF(U(1,JJ,K)<0.)                                            &
     &          V(1,JJ,K)=2.*V(2,JJ,K)-V(3,JJ,K)
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
            IF(U(IIM,JJ,K)>0.)                                          &
     &          V(IIM,JJ,K)=2.*V(IIM-1,JJ,K)-V(IIM-2,JJ,K)
          ENDIF
!
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
!
  200 CONTINUE

  201 CONTINUE
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
        U(2,2,K)=D06666*(4.*(U(1,1,K)+U(2,1,K)+U(2,3,K))                &
     &                     + U(1,2,K)+U(1,4,K)+U(2,4,K))
        V(2,2,K)=D06666*(4.*(V(1,1,K)+V(2,1,K)+V(2,3,K))                &
     &                      +V(1,2,K)+V(1,4,K)+V(2,4,K))
      ENDIF
!
!***  SOUTHEAST CORNER
!
      IF(S_BDY.AND.E_BDY)THEN
        U(IIM-1,2,K)=D06666*(4.*(U(IIM-2,1,K)+U(IIM-1,1,K)              &
     &                          +U(IIM-2,3,K))                          &
     &                          +U(IIM,2,K)+U(IIM,4,K)+U(IIM-1,4,K))
        V(IIM-1,2,K)=D06666*(4.*(V(IIM-2,1,K)+V(IIM-1,1,K)              &
     &                          +V(IIM-2,3,K))                          &
     &                          +V(IIM,2,K)+V(IIM,4,K)+V(IIM-1,4,K))
      ENDIF
!
!***  NORTHWEST CORNER
!
      IF(N_BDY.AND.W_BDY)THEN
        U(2,JJM-1,K)=D06666*(4.*(U(1,JJM,K)+U(2,JJM,K)+U(2,JJM-2,K))    &
     &                          +U(1,JJM-1,K)+U(1,JJM-3,K)              &
     &                          +U(2,JJM-3,K))
        V(2,JJM-1,K)=D06666*(4.*(V(1,JJM,K)+V(2,JJM,K)+V(2,JJM-2,K))    &
     &                          +V(1,JJM-1,K)+V(1,JJM-3,K)              &
     &                          +V(2,JJM-3,K))
      ENDIF
!
!***  NORTHEAST CORNER
!
      IF(N_BDY.AND.E_BDY)THEN
        U(IIM-1,JJM-1,K)=                                               &
     &    D06666*(4.*(U(IIM-2,JJM,K)+U(IIM-1,JJM,K)+U(IIM-2,JJM-2,K))   &
     &               +U(IIM,JJM-1,K)+U(IIM,JJM-3,K)+U(IIM-1,JJM-3,K))
        V(IIM-1,JJM-1,K)=                                               &
     &    D06666*(4.*(V(IIM-2,JJM,K)+V(IIM-1,JJM,K)+V(IIM-2,JJM-2,K))   &
     &               +V(IIM,JJM-1,K)+V(IIM,JJM-3,K)+V(IIM-1,JJM-3,K))
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
          U(I,2,K)=(U(I-1,1,K)+U(I,1,K)+U(I-1,3,K)+U(I,3,K))*0.25
          V(I,2,K)=(V(I-1,1,K)+V(I,1,K)+V(I-1,3,K)+V(I,3,K))*0.25
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(N_BDY)THEN
        DO I=MYIS2,MYIE2
          U(I,JJM-1,K)=(U(I-1,JJM-2,K)+U(I,JJM-2,K)                     &
     &                 +U(I-1,JJM,K)+U(I,JJM,K))*0.25
          V(I,JJM-1,K)=(V(I-1,JJM-2,K)+V(I,JJM-2,K)                     &
     &                 +V(I-1,JJM,K)+V(I,JJM,K))*0.25
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
            U(1,JJ,K)=(U(1,JJ-1,K)+U(2,JJ-1,K)                          &
     &                +U(1,JJ+1,K)+U(2,JJ+1,K))*0.25
            V(1,JJ,K)=(V(1,JJ-1,K)+V(2,JJ-1,K)                          &
     &                +V(1,JJ+1,K)+V(2,JJ+1,K))*0.25


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
            U(IIM-1,JJ,K)=0.25*(U(IIM-1,JJ-1,K)+U(IIM,JJ-1,K)           &
     &                         +U(IIM-1,JJ+1,K)+U(IIM,JJ+1,K))
            V(IIM-1,JJ,K)=0.25*(V(IIM-1,JJ-1,K)+V(IIM,JJ-1,K)           &
     &                         +V(IIM-1,JJ+1,K)+V(IIM,JJ+1,K))
          ENDIF
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!
  300 CONTINUE
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE BOCOV
!
!-----------------------------------------------------------------------
!
    ! ------------------------------------------------------------
    ! Non-bulk boundary/tendancy function (mass points)
    ! ------------------------------------------------------------

    SUBROUTINE MP_SPECIES_BDY(gridid, spec_bdy_width, dt,    &
             CWM,Q,                                          &
             MOIST,N_MOIST,                                  &
             MOIST_bxs,MOIST_bxe,MOIST_bys,MOIST_bye,        &
             MOIST_btxs,MOIST_btxe,MOIST_btys,MOIST_btye,    &
             SCALAR,N_SCALAR,                                &
             SCALAR_bxs,SCALAR_bxe,SCALAR_bys,SCALAR_bye,    &
             SCALAR_btxs,SCALAR_btxe,SCALAR_btys,SCALAR_btye,&
             ids,ide,jds,jde,kds,kde,                        &
             ims,ime,jms,jme,kms,kme,                        &
             its,ite,jts,jte,kts,kte)

          implicit none

          integer, intent(in) :: N_MOIST,N_SCALAR,           &
               ids,ide,jds,jde,kds,kde,                      &
               ims,ime,jms,jme,kms,kme,                      &
               its,ite,jts,jte,kts,kte
          integer, intent(in) :: spec_bdy_width,gridid
          real, intent(in) :: dt

          real, intent(inout),dimension(ims:ime,jms:jme,kms:kme) :: &
               CWM,Q

          real,dimension(ims:ime,kms:kme,spec_bdy_width,n_moist), &
               intent(inout) :: MOIST_bys,MOIST_bye,MOIST_btys,MOIST_btye

          real,dimension(jms:jme,kms:kme,spec_bdy_width,n_moist), &
               intent(inout) :: MOIST_bxs,MOIST_bxe,MOIST_btxs,MOIST_btxe

          real,dimension(ims:ime,kms:kme,spec_bdy_width,n_scalar), &
               intent(inout) :: SCALAR_bys,SCALAR_bye,SCALAR_btys,SCALAR_btye

          real,dimension(jms:jme,kms:kme,spec_bdy_width,n_scalar), &
               intent(inout) :: SCALAR_bxs,SCALAR_bxe,SCALAR_btxs,SCALAR_btxe

          real,intent(inout) :: MOIST(ims:ime,jms:jme,kms:kme,N_MOIST)
          real,intent(inout) :: SCALAR(ims:ime,jms:jme,kms:kme,N_SCALAR)

          ! ------------------------------------------------------------
          ! LOCAL VARIABLES
          ! ------------------------------------------------------------

          INTEGER :: I,IB,IBDY,II,IIM,IM,IRTN,ISIZ1,ISIZ2                &
         &          ,J,JB,JJ,JJM,JM,K,KK,N,NN,NREC,NUMGAS,NV,REC
          INTEGER :: MY_IS_GLB,MY_JS_GLB,MY_IE_GLB,MY_JE_GLB  
          INTEGER :: I_M,ILPAD1,IRPAD1,JBPAD1,JTPAD1
          REAL :: BCHR,CONVFAC,CWK,PLYR,RRI
          LOGICAL :: E_BDY,W_BDY,N_BDY,S_BDY
          INTEGER :: i1,i2,j1,j2,j1b,j2b

          ! ------------------------------------------------------------

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

          ! I loop ends for N/S bdy copying:
          i1=MAX(ITS-1,IDS)
          i2=MIN(ITE+1,IDE)
          
          ! J loop ends for E/W bdy copying:
          j1=MAX(JTS-1,JDS+3-1)
          if(mod(j1,2)/=1) j1=j1+1
          j2=MIN(JTE+1,JDE-2)
          if(mod(j2,2)/=1) j2=j2-1

          ! J loop ends for E/W bdy 4-point averaging:
          J1B=MAX(4,MY_JS_GLB-JBPAD1)
          IF(MOD(J1B,2)/=0) J1B=J1B+1
          J2B=MIN(JM-3,MY_JE_GLB+JTPAD1)
          IF(MOD(J2B,2)/=0) J2B=J2B-1


          !
          !-----------------------------------------------------------------------
          !***  SOUTH AND NORTH BOUNDARIES
          !-----------------------------------------------------------------------
          !
          !***  USE IBDY=1 FOR SOUTH; 2 FOR NORTH
          !
          n_s_bdy: DO IBDY=1,2 
             !
             !***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
             !
             if_n_s_bdy: IF(S_BDY.AND.IBDY==1) THEN 
                JB=1         ! Which cell in from boundary
                JJ=1         ! Which cell in the domain
                !$omp parallel do                        &
                !$omp& private(i,k)
                DO K=KTS,KTE
                   DO I=i1,i2
                      cwm(i,jj,k)=0.
                   END DO
                END DO
                !$omp parallel do                        &
                !$omp& private(i,k,im)
                sbdy_type: DO IM=1,N_MOIST
                   if(IM==P_QV) cycle sbdy_type
                   DO K=KTS,KTE
                      DO I=i1,i2
                         MOIST_BYS(I,K,JB,IM)=MOIST_BYS(I,K,JB,IM)+MOIST_BTYS(I,K,JB,IM)*DT
                         MOIST(I,JJ,K,IM)=MOIST_BYS(I,K,JB,IM)
                         CWM(I,JJ,K)=CWM(I,JJ,K) + MOIST(I,JJ,K,IM)
                      ENDDO
                   ENDDO
                ENDDO sbdy_type
                !$omp parallel do                        &
                !$omp& private(i,k,im)
                sbdy_Stype: DO IM=2,N_SCALAR
                   DO K=KTS,KTE
                      DO I=i1,i2
                         SCALAR_BYS(I,K,JB,IM)=SCALAR_BYS(I,K,JB,IM)+SCALAR_BTYS(I,K,JB,IM)*DT
                         SCALAR(I,JJ,K,IM)=SCALAR_BYS(I,K,JB,IM)
                      ENDDO
                   ENDDO
                ENDDO sbdy_Stype

             ELSEIF(N_BDY.AND.IBDY==2) THEN
                JB=1         ! Which cell in from boundary
                JJ=JJM       ! Which cell in the domain

                !$omp parallel do                        &
                !$omp& private(i,k)
                DO K=KTS,KTE
                   DO I=i1,i2
                      CWM(i,jj,k)=0.
                   END DO
                END DO
                !$omp parallel do                        &
                !$omp& private(i,k)
                nbdy_type: DO IM=1,N_MOIST
                   if(IM==P_QV) cycle nbdy_type
                   DO K=KTS,KTE
                      DO I=i1,i2
                         MOIST_BYE(I,K,JB,IM)=MOIST_BYE(I,K,JB,IM)+MOIST_BTYE(I,K,JB,IM)*DT
                         MOIST(I,JJ,K,IM)=MOIST_BYE(I,K,JB,IM)
                         CWM(I,JJ,K)=CWM(I,JJ,K) + MOIST(I,JJ,K,IM)
                      ENDDO
                   ENDDO
                ENDDO nbdy_type
                !$omp parallel do                        &
                !$omp& private(i,k)
                nbdy_Stype: DO IM=1,N_SCALAR
                   DO K=KTS,KTE
                      DO I=i1,i2
                         SCALAR_BYE(I,K,JB,IM)=SCALAR_BYE(I,K,JB,IM)+SCALAR_BTYE(I,K,JB,IM)*DT
                         SCALAR(I,JJ,K,IM)=SCALAR_BYE(I,K,JB,IM)
                      ENDDO
                   ENDDO
                ENDDO nbdy_Stype
             ENDIF if_n_s_bdy
          ENDDO n_s_bdy
!
!-----------------------------------------------------------------------
!***  WEST AND EAST BOUNDARIES
!-----------------------------------------------------------------------
!
!***  USE IBDY=1 FOR WEST; 2 FOR EAST. 
!
      east_west_bt: DO IBDY=1,2 
!
!***  MAKE SURE THE PROCESSOR HAS THIS BOUNDARY.
!
         if_e_w_bt: IF(W_BDY.AND.IBDY==1) THEN  
            IB=1         ! Which cell in from boundary 
            II=1         ! Which cell in the domain
!$omp parallel do                                                       &
!$omp& private(j,k)
            DO K=KTS,KTE
               DO J=MAX(JTS-1,JDS+3-1),MIN(JTE+1,JDE-2)
                  cwm(ii,j,k)=0.
               ENDDO
            ENDDO
!$omp parallel do                                                       &
!$omp& private(j,k,im)
            w_bdy_type: DO IM=1,N_MOIST
               if(IM==P_QV) cycle w_bdy_type
               DO K=KTS,KTE
                  DO J=j1,j2,2
                     MOIST_BXS(J,K,IB,IM)=MOIST_BXS(J,K,IB,IM)+MOIST_BTXS(J,K,IB,IM)*DT
                     MOIST(II,J,K,IM)=MOIST_BXS(J,K,IB,IM)
                     CWM(II,J,K)=CWM(II,J,K)+MOIST(II,J,K,IM)
                  ENDDO
               ENDDO
            ENDDO w_bdy_type
!$omp parallel do                                                       &
!$omp& private(j,k,im)
            w_bdy_Stype: DO IM=2,N_SCALAR
               DO K=KTS,KTE
                  DO J=j1,j2,2
                     SCALAR_BXS(J,K,IB,IM)=SCALAR_BXS(J,K,IB,IM)+SCALAR_BTXS(J,K,IB,IM)*DT
                     SCALAR(II,J,K,IM)=SCALAR_BXS(J,K,IB,IM)
                  ENDDO
               ENDDO
            ENDDO w_bdy_Stype
          ELSEIF(E_BDY.AND.IBDY==2) THEN
            IB=1         ! Which cell in from boundary
            II=IIM       ! Which cell in the domain
!$omp parallel do                                                       &
!$omp& private(j,k)
          DO K=KTS,KTE
            DO J=j1,j2
               CWM(II,J,K)=0.
            ENDDO
         ENDDO
!$omp parallel do                                                       &
!$omp& private(j,k,im)
         e_bdy_type: DO IM=1,N_MOIST
            if(IM==P_QV) cycle e_bdy_type
            DO K=KTS,KTE
               DO J=j1,j2
                  MOIST_BXE(J,K,IB,IM)=MOIST_BXE(J,K,IB,IM)+MOIST_BTXE(J,K,IB,IM)*DT
                  MOIST(II,J,K,IM)=MOIST_BXE(J,K,IB,IM)
                  CWM(II,J,K)=CWM(II,J,K)+MOIST(II,J,K,IM)
               ENDDO
            ENDDO
         ENDDO e_bdy_type
!$omp parallel do                                                       &
!$omp& private(j,k,im)
         e_bdy_Stype: DO IM=2,N_SCALAR
            DO K=KTS,KTE
               DO J=j1,j2
                  SCALAR_BXE(J,K,IB,IM)=SCALAR_BXE(J,K,IB,IM)+SCALAR_BTXE(J,K,IB,IM)*DT
                  SCALAR(II,J,K,IM)=SCALAR_BXE(J,K,IB,IM)
               ENDDO
            ENDDO
         ENDDO e_bdy_Stype
      ENDIF if_e_w_bt
   ENDDO east_west_bt


!-----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF MICROPHYSICS VARIABLES
!***  AT INNER BOUNDARY
!-----------------------------------------------------------------------


!
!-----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
     s_bdy_avg: IF(S_BDY)THEN
        DO K=KTS,KTE
         DO I=MYIS,MYIE1
          CWM(I,2,K)=(CWM(I,1,K)+CWM(I+1,1,K)+CWM(I,3,K)+CWM(I+1,3,K))  &
     &               *0.25
         ENDDO
        ENDDO
!
        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
           DO K=KTS,KTE
            DO I=MYIS,MYIE1
              MOIST(I,2,K,I_M)=Q(I,2,K)/(1.-Q(I,2,K))
            ENDDO
           ENDDO
          ELSE
           DO K=KTS,KTE
            DO I=MYIS,MYIE1
              MOIST(I,2,K,I_M)=(MOIST(I,1,K,I_M)                        &
     &                         +MOIST(I+1,1,K,I_M)                      &
     &                         +MOIST(I,3,K,I_M)                        &
     &                         +MOIST(I+1,3,K,I_M))*0.25
            ENDDO
           ENDDO
          ENDIF
        ENDDO
!
        DO I_M=2,N_SCALAR
         DO K=KTS,KTE
          DO I=MYIS,MYIE1
            SCALAR(I,2,K,I_M)=(SCALAR(I,1,K,I_M)                        &
     &                        +SCALAR(I+1,1,K,I_M)                      &
     &                        +SCALAR(I,3,K,I_M)                        &
     &                        +SCALAR(I+1,3,K,I_M))*0.25
          ENDDO
         ENDDO
        ENDDO
!
     ENDIF s_bdy_avg
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
     n_bdy_avg: IF(N_BDY)THEN
        DO K=KTS,KTE
         DO I=MYIS,MYIE1
          CWM(I,JJM-1,K)=(CWM(I,JJM-2,K)+CWM(I+1,JJM-2,K)               &
     &                   +CWM(I,JJM,K)+CWM(I+1,JJM,K))                  &
     &                   *0.25
         ENDDO
        ENDDO
!
        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
           DO K=KTS,KTE
            DO I=MYIS,MYIE1
              MOIST(I,JJM-1,K,I_M)=Q(I,JJM-1,K)/(1.-Q(I,JJM-1,K))
            ENDDO
           ENDDO
          ELSE
           DO K=KTS,KTE
            DO I=MYIS,MYIE1
              MOIST(I,JJM-1,K,I_M)=(MOIST(I,JJM-2,K,I_M)                &
     &                             +MOIST(I+1,JJM-2,K,I_M)              &
     &                             +MOIST(I,JJM,K,I_M)                  &
     &                             +MOIST(I+1,JJM,K,I_M))*0.25
            ENDDO
           ENDDO
          ENDIF
        ENDDO
!
        DO I_M=2,N_SCALAR
         DO K=KTS,KTE
          DO I=MYIS,MYIE1
            SCALAR(I,JJM-1,K,I_M)=(SCALAR(I,JJM-2,K,I_M)                &
     &                            +SCALAR(I+1,JJM-2,K,I_M)              &
     &                            +SCALAR(I,JJM,K,I_M)                  &
     &                            +SCALAR(I+1,JJM,K,I_M))*0.25
          ENDDO
         ENDDO
        ENDDO
!
     ENDIF n_bdy_avg
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
     w_bdy_avg:IF(W_BDY)THEN
       DO K=KTS,KTE
          DO J=J1B,J2B
             CWM(1,J,K)=(CWM(1,J-1,K)+CWM(2,J-1,K) &
                       +CWM(1,J+1,K)+CWM(2,J+1,K)) &
                       *0.25
          ENDDO
       ENDDO
       DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
             DO K=KTS,KTE
                DO J=J1B,J2B
                   MOIST(1,J,K,I_M)=Q(1,J,K)/(1.-Q(1,J,K))     
                ENDDO
             ENDDO
          ELSE  
             DO K=KTS,KTE
                DO J=J1B,J2B
                   MOIST(1,J,K,I_M)=(MOIST(1,J-1,K,I_M) &
                                  +MOIST(2,J-1,K,I_M) &
                                  +MOIST(1,J+1,K,I_M) &
                                  +MOIST(2,J+1,K,I_M))*0.25
                ENDDO
             ENDDO
          ENDIF
       ENDDO
       DO I_M=2,N_SCALAR
          DO K=KTS,KTE
             DO J=J1B,J2B
              SCALAR(1,J,K,I_M)=(SCALAR(1,J-1,K,I_M)                  &
     &                           +SCALAR(2,J-1,K,I_M)                  &
     &                           +SCALAR(1,J+1,K,I_M)                  &
     &                           +SCALAR(2,J+1,K,I_M))*0.25
             ENDDO
          ENDDO
       ENDDO
     ENDIF w_bdy_avg
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
     e_bdy_avg:IF(E_BDY)THEN
        DO K=KTS,KTE
           DO J=J1B,J2B
            CWM(IIM-1,J,K)=(CWM(IIM-1,J-1,K)+CWM(IIM,J-1,K) &
                           +CWM(IIM-1,J+1,K)+CWM(IIM,J+1,K)) &
                           *0.25
           ENDDO
        ENDDO
        DO I_M=1,N_MOIST
          IF(I_M==P_QV)THEN
             DO K=KTS,KTE
                DO J=J1B,J2B
                   MOIST(IIM-1,J,K,I_M)=Q(IIM-1,J,K)/(1.-Q(IIM-1,J,K))
                ENDDO
             ENDDO
          ELSE
             DO K=KTS,KTE
                DO J=J1B,J2B
                   MOIST(IIM-1,J,K,I_M)=(MOIST(IIM-1,J-1,K,I_M)   &
                                      +MOIST(IIM,J-1,K,I_M)        &
                                      +MOIST(IIM-1,J+1,K,I_M)      &
                                      +MOIST(IIM,J+1,K,I_M))*0.25
                ENDDO
             ENDDO
          ENDIF
        ENDDO
        DO I_M=2,N_SCALAR
          DO K=KTS,KTE
             DO J=J1B,J2B
                SCALAR(IIM-1,J,K,I_M)=(SCALAR(IIM-1,J-1,K,I_M)     &
                                       +SCALAR(IIM,J-1,K,I_M)       &
                                       +SCALAR(IIM-1,J+1,K,I_M)     &
                                       +SCALAR(IIM,J+1,K,I_M))*0.25
             ENDDO
          ENDDO
        ENDDO
     ENDIF e_bdy_avg
       
      END SUBROUTINE MP_SPECIES_BDY
!
!-----------------------------------------------------------------------
!
      END MODULE MODULE_BNDRY_COND
!
!-----------------------------------------------------------------------

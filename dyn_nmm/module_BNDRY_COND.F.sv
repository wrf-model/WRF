!
!NCEP_MESO:MODEL_LAYER: BOUNDARY CONDITION UPDATES
!
!----------------------------------------------------------------------
!
      MODULE MODULE_BNDRY_COND
!
!----------------------------------------------------------------------
      USE MODULE_MPP
      USE MODULE_INDX
!
#ifdef DM_PARALLEL
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
      REAL :: D06666=0.06666666
!----------------------------------------------------------------------
!
      CONTAINS
!
!**********************************************************************
      SUBROUTINE BOCOH(NTSD,DT,NEST,NBC,NBOCO,NTSTM,TSPH               &
                      ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM                   &
                      ,PDB,TB,QB,UB,VB,Q2B,CWMB                        &
                      ,PD,T,Q,Q2,CWM,PINT                              &
                      ,IDS,IDE,JDS,JDE,KDS,KDE                         &
                      ,IMS,IME,JMS,JME,KMS,KME                         &
                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
!     
! USAGE: CALL BOCOH FROM SUBROUTINE SOLVE_RUNSTREAM
!   INPUT ARGUMENT LIST:
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
!   MACHINE : IBM SP
!$$$  
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: NEST
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LB,NBC,NTSD,NTSTM
      INTEGER,INTENT(INOUT) :: NBOCO
!
      REAL,INTENT(IN) :: DT,PDTOP,PT,TSPH
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM
!
      REAL,DIMENSION(LB,2),INTENT(INOUT) :: PDB
!
      REAL,DIMENSION(LB,KMS:KME,2),INTENT(INOUT) :: TB,QB,UB,VB      &
                                                     ,Q2B,CWMB
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: T,Q   &
                                                                ,Q2    &
                                                                ,CWM
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,II,IIM,IM,IRTN,ISIZ1,ISIZ2                          &
                ,J,JJ,JJM,JM,K,N,NN,NREC,REC
!
      REAL :: BCHR,RHTM,SHTM
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IM=IDE-IDS+1
      JM=JDE-JDS+1
      ISIZ1=2*LB
!cjm      ISIZ2=2*LB*(KME-KMS)
      ISIZ2=2*LB*(KTE-KTS)
!
!----------------------------------------------------------------------
!***  READ FRESH BOUNDARY DATA IF NECESSARY
!----------------------------------------------------------------------
!
      IF(NTSD-1.EQ.NBOCO.and.ntsd.lt.8641)THEN
!
        IF(MYPE.EQ.0.AND.NEST)THEN
          NREC=NINT((NTSD-1)*DT/3600.)+2
          READ(NBC,REC=NREC)BCHR                                       &
                         ,((PDB(N,NN),N=1,LB),NN=1,2)                  &
                         ,(((TB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((QB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((UB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((VB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                        ,(((Q2B(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                       ,(((CWMB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)
        ENDIF
!
        IF(MYPE.EQ.0.AND..NOT.NEST)THEN
          READ(NBC)PDB
          READ(NBC)TB
          READ(NBC)QB
          READ(NBC)UB
          READ(NBC)VB
          READ(NBC)Q2B
          READ(NBC)CWMB
        ENDIF
!
#ifdef DM_PARALLEL
        CALL MPI_BCAST(PDB,ISIZ1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(TB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(QB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(UB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(Q2B,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(CWMB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
#endif
!***
!***    FIND NEXT BOUNDARY CONDITION READ
!***
        IF(NTSD.LT.NTSTM)THEN
          IF(MYPE.EQ.0.AND.NEST)BCHR=BCHR+1    ! This assumes 1-hrly BC's
          IF(MYPE.EQ.0.AND..NOT.NEST)READ(NBC)BCHR
#ifdef DM_PARALLEL
          CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
#endif
          NBOCO=INT(BCHR*TSPH+0.5)
        ENDIF
!
      ENDIF
!----------------------------------------------------------------------
!cjm      IIM=IM-MY_IS_GLB+1
!cjm      JJM=JM-MY_JS_GLB+1
      IIM=ide
      JJM=jde

      ILPAD1 = 1
      IF ( its == ids ) ILPAD1 = 0
      IRPAD1 = 1
      IF ( ite == ide ) ILPAD1 = 0
      JBPAD1 = 1
      IF ( jts == jds ) JBPAD1 = 0
      JTPAD1 = 1
      IF ( jte == jde ) JTPAD1 = 0
!
!----------------------------------------------------------------------
!***  UPDATE THE SURFACE PRESSURE
!----------------------------------------------------------------------
!
      N=1
!
!***  SOUTH BOUNDARY
!
!cjm      DO I=1,IM
        DO I=ids,ide
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
!jm        IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1                    &
!jm                         .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
!jm          II=I-MY_IS_GLB+1
        IF( JTS == 1 .AND.I.GE.ITS - ILPAD1                    &
                     .AND.I.LE.ITE + IRPAD1 )THEN
!jm          II=I-MY_IS_GLB+1
          II=I
          PD(II,1)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  NORTH BOUNDARY
!
!jm      DO I=1,IM
      DO I=ids,ide
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
        IF( jte.EQ.jde .AND. I.GE.its-ILPAD1                   &
                       .AND. I.LE.ITE+IRPAD1)THEN
!jm          II=I-MY_IS_GLB+1
          II=I
          PD(II,JJM)=PDB(N,1)
        ENDIF
        N=N+1
!
      ENDDO
!
!***  WEST BOUNDARY
!
!jm      DO J=3,JM-2,2
      DO J=jds+2,jde-2,2
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
!jm        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                    &
!jm                         .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        IF( its .EQ.1.AND.J.GE.jts-JBPAD1                    &
                     .AND.J.LE.jte+JTPAD1)THEN
!jm          JJ=J-MY_JS_GLB+1
          JJ=J
          PD(1,JJ)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  EAST BOUNDARY
!
!jm      DO J=3,JM-2,2
      DO J=jds+2,jde-2,2
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
!jm      IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                   &
!jm                        .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
      IF( ite .EQ. ide .AND. J.GE.jts-JBPAD1                   &
                       .AND. J.LE.jte+JTPAD1)THEN
!jm        JJ=J-MY_JS_GLB+1
          JJ=J
          PD(IIM,JJ)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO

!jm  MORE TO DO... same stuff all the way down.... 
#if 0
!
!----------------------------------------------------------------------
!***  UPDATE THE 3-D MASS VARIABLES
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      DO 100 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  SOUTHERN BOUNDARY
!
      N=1
      DO I=1,IM
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
        CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT                        
!
        IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1                    &
                         .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          T(II,K,1)=TB(N,K,1)
          Q(II,K,1)=QB(N,K,1)
          Q2(II,K,1)=Q2B(N,K,1)
          CWM(II,K,1)=CWMB(N,K,1)                                                 
          PINT(II,K+1,1)=ETA1(K+1)*PDTOP                               &
                        +ETA2(K+1)*PD(II,1)*RES(II,1)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  NORTHERN BOUNDARY
!
      DO I=1,IM
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
        CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT                       
!
        IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1                   &
                          .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          T(II,K,JJM)=TB(N,K,1)
          Q(II,K,JJM)=QB(N,K,1)
          Q2(II,K,JJM)=Q2B(N,K,1)
          CWM(II,K,JJM)=CWMB(N,K,1)                                                 
          PINT(II,K+1,JJM)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(II,JJM)*RES(II,JJM)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  WESTERN BOUNDARY
!
      DO J=3,JM-2,2
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
        CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT                       
!
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                    &
                         .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          T(1,K,JJ)=TB(N,K,1)
          Q(1,K,JJ)=QB(N,K,1)
          Q2(1,K,JJ)=Q2B(N,K,1)
          CWM(1,K,JJ)=CWMB(N,K,1)                                                 
          PINT(1,K+1,JJ)=ETA1(K+1)*PDTOP                               &
                        +ETA2(K+1)*PD(1,JJ)*RES(1,JJ)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  EASTERN BOUNDARY
!
      DO J=3,JM-2,2
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        QB(N,K,1)=QB(N,K,1)+QB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
        CWMB(N,K,1)=CWMB(N,K,1)+CWMB(N,K,2)*DT                       
!
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                   &
                          .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          T(IIM,K,JJ)=TB(N,K,1)
          Q(IIM,K,JJ)=QB(N,K,1)
          Q2(IIM,K,JJ)=Q2B(N,K,1)
          CWM(IIM,K,JJ)=CWMB(N,K,1)                                                 
          PINT(IIM,K+1,JJ)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(IIM,JJ)*RES(IIM,JJ)+PT
        ENDIF
!
        N=N+1
      ENDDO
!----------------------------------------------------------------------
!
  100 CONTINUE
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF PD THEN REMAINING MASS VARIABLES
!***  AT INNER BOUNDARY
!----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,1)+HTM(I+1,KTE,1)+HTM(I,KTE,3)+HTM(I+1,KTE,3)
          PD(I,2)=(PD(I,1)*HTM(I,KTE,1)+PD(I+1,1)*HTM(I+1,KTE,1)       &
                  +PD(I,3)*HTM(I,KTE,3)+PD(I+1,3)*HTM(I+1,KTE,3))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,JJM-2)+HTM(I+1,KTE,JJM-2)+HTM(I,KTE,JJM)      &
                                                +HTM(I+1,KTE,JJM)
          PD(I,JJM-1)=(PD(I,JJM-2)*HTM(I,KTE,JJM-2)                    &
                      +PD(I+1,JJM-2)*HTM(I+1,KTE,JJM-2)                &
                      +PD(I,JJM)*HTM(I,KTE,JJM)                        &
                      +PD(I+1,JJM)*HTM(I+1,KTE,JJM))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(ILCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            SHTM=HTM(1,KTE,JJ-1)+HTM(2,KTE,JJ-1)+HTM(1,KTE,JJ+1)       &
                                              +HTM(2,KTE,JJ+1)
            PD(1,JJ)=(PD(1,JJ-1)*HTM(1,KTE,JJ-1)                       &
                     +PD(2,JJ-1)*HTM(2,KTE,JJ-1)                       &
                     +PD(1,JJ+1)*HTM(1,KTE,JJ+1)                       &
                     +PD(2,JJ+1)*HTM(2,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(IRCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            SHTM=HTM(IIM-1,KTE,JJ-1)+HTM(IIM,KTE,JJ-1)                 &
                +HTM(IIM-1,KTE,JJ+1)+HTM(IIM,KTE,JJ+1)
            PD(IIM-1,JJ)=(PD(IIM-1,JJ-1)*HTM(IIM-1,KTE,JJ-1)           &
                         +PD(IIM,JJ-1)*HTM(IIM,KTE,JJ-1)               &
                         +PD(IIM-1,JJ+1)*HTM(IIM-1,KTE,JJ+1)           &
                         +PD(IIM,JJ+1)*HTM(IIM,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!----------------------------------------------------------------------
!
      DO 200 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,1)+HTM(I+1,K,1)+HTM(I,K,3)+HTM(I+1,K,3))
          T(I,K,2)=(T(I,K,1)*HTM(I,K,1)+T(I+1,K,1)*HTM(I+1,K,1)        &
                   +T(I,K,3)*HTM(I,K,3)+T(I+1,K,3)*HTM(I+1,K,3))       &
                   *RHTM
          Q(I,K,2)=(Q(I,K,1)*HTM(I,K,1)+Q(I+1,K,1)*HTM(I+1,K,1)        &
                   +Q(I,K,3)*HTM(I,K,3)+Q(I+1,K,3)*HTM(I+1,K,3))       &
                   *RHTM
          Q2(I,K,2)=(Q2(I,K,1)*HTM(I,K,1)+Q2(I+1,K,1)*HTM(I+1,K,1)     &
                    +Q2(I,K,3)*HTM(I,K,3)+Q2(I+1,K,3)*HTM(I+1,K,3))    &
                    *RHTM
          CWM(I,K,2)=(CWM(I,K,1)*HTM(I,K,1)+CWM(I+1,K,1)*HTM(I+1,K,1)  &
                     +CWM(I,K,3)*HTM(I,K,3)+CWM(I+1,K,3)*HTM(I+1,K,3)) &
                     *RHTM
          PINT(I,K+1,2)=ETA1(K+1)*PDTOP+ETA2(K+1)*PD(I,2)*RES(I,2)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,JJM-2)+HTM(I+1,K,JJM-2)                     &
                  +HTM(I,K,JJM)+HTM(I+1,K,JJM))
          T(I,K,JJM-1)=(T(I,K,JJM-2)*HTM(I,K,JJM-2)                    &
                       +T(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)                &
                       +T(I,K,JJM)*HTM(I,K,JJM)                        &
                       +T(I+1,K,JJM)*HTM(I+1,K,JJM))                   &
                       *RHTM
          Q(I,K,JJM-1)=(Q(I,K,JJM-2)*HTM(I,K,JJM-2)                    &
                       +Q(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)                &
                       +Q(I,K,JJM)*HTM(I,K,JJM)                        &
                       +Q(I+1,K,JJM)*HTM(I+1,K,JJM))                   &
                       *RHTM
          Q2(I,K,JJM-1)=(Q2(I,K,JJM-2)*HTM(I,K,JJM-2)                  &
                        +Q2(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)              &
                        +Q2(I,K,JJM)*HTM(I,K,JJM)                      &
                        +Q2(I+1,K,JJM)*HTM(I+1,K,JJM))                 &
                        *RHTM
          CWM(I,K,JJM-1)=(CWM(I,K,JJM-2)*HTM(I,K,JJM-2)                &
                         +CWM(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)            &
                         +CWM(I,K,JJM)*HTM(I,K,JJM)                    &
                         +CWM(I+1,K,JJM)*HTM(I+1,K,JJM))               &
                         *RHTM
          PINT(I,K+1,JJM-1)=ETA1(K+1)*PDTOP                            &
                           +ETA2(K+1)*PD(I,JJM-1)*RES(I,JJM-1)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(ILCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            RHTM=1./(HTM(1,K,JJ-1)+HTM(2,K,JJ-1)                       &
                    +HTM(1,K,JJ+1)+HTM(2,K,JJ+1))
            T(1,K,JJ)=(T(1,K,JJ-1)*HTM(1,K,JJ-1)                       &
                      +T(2,K,JJ-1)*HTM(2,K,JJ-1)                       &
                      +T(1,K,JJ+1)*HTM(1,K,JJ+1)                       &
                      +T(2,K,JJ+1)*HTM(2,K,JJ+1))                      &
                      *RHTM
            Q(1,K,JJ)=(Q(1,K,JJ-1)*HTM(1,K,JJ-1)                       &
                      +Q(2,K,JJ-1)*HTM(2,K,JJ-1)                       &
                      +Q(1,K,JJ+1)*HTM(1,K,JJ+1)                       &
                      +Q(2,K,JJ+1)*HTM(2,K,JJ+1))                      &
                      *RHTM
            Q2(1,K,JJ)=(Q2(1,K,JJ-1)*HTM(1,K,JJ-1)                     &
                       +Q2(2,K,JJ-1)*HTM(2,K,JJ-1)                     &
                       +Q2(1,K,JJ+1)*HTM(1,K,JJ+1)                     &
                       +Q2(2,K,JJ+1)*HTM(2,K,JJ+1))                    &
                       *RHTM
            CWM(1,K,JJ)=(CWM(1,K,JJ-1)*HTM(1,K,JJ-1)                   &
                        +CWM(2,K,JJ-1)*HTM(2,K,JJ-1)                   &
                        +CWM(1,K,JJ+1)*HTM(1,K,JJ+1)                   &
                        +CWM(2,K,JJ+1)*HTM(2,K,JJ+1))                  &
                        *RHTM
            PINT(1,K+1,JJ)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(1,JJ)*RES(1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(IRCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            RHTM=1./(HTM(IIM-1,K,JJ-1)+HTM(IIM,K,JJ-1)                 &
                    +HTM(IIM-1,K,JJ+1)+HTM(IIM,K,JJ+1))
            T(IIM-1,K,JJ)=(T(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)           &
                          +T(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)               &
                          +T(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)           &
                          +T(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))              &
                          *RHTM
            Q(IIM-1,K,JJ)=(Q(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)           &
                          +Q(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)               &
                          +Q(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)           &
                          +Q(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))              &
                          *RHTM
            Q2(IIM-1,K,JJ)=(Q2(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)         &
                           +Q2(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)             &
                           +Q2(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)         &
                           +Q2(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))            &
                           *RHTM
            CWM(IIM-1,K,JJ)=(CWM(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)       &
                            +CWM(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)           &
                            +CWM(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)       &
                            +CWM(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))          &
                            *RHTM
            PINT(IIM-1,K+1,JJ)=ETA1(K+1)*PDTOP                         &
                              +ETA2(K+1)*PD(IIM-1,JJ)*RES(IIM-1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
  200 CONTINUE
#endif
!
!----------------------------------------------------------------------
      END SUBROUTINE BOCOH
!----------------------------------------------------------------------
!**********************************************************************
      SUBROUTINE BOCOV(NTSD,DT,LB,VTM,UB,VB,U,V                        &
                      ,IDS,IDE,JDS,JDE,KDS,KDE                         &
                      ,IMS,IME,JMS,JME,KMS,KME                         &
                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
!     
! USAGE: CALL BOCOH FROM MAIN PROGRAM EBU
!   INPUT ARGUMENT LIST:
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
!   MACHINE : IBM SP
!$$$  
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LB,NTSD
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME),INTENT(IN) :: VTM
!
      REAL,DIMENSION(LB,KMS:KME-1,2),INTENT(INOUT) :: UB,VB
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME),INTENT(INOUT) :: U,V
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,II,IIM,IM,J,JJ,JJM,JM,K,N
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  TIME INTERPOLATION OF U AND V AT THE OUTER BOUNDARY
!----------------------------------------------------------------------
!
      IM=IDE-IDS+1
      JM=JDE-JDS+1
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
!
!----------------------------------------------------------------------
!
      DO 100 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  SOUTHERN BOUNDARY
!
      N=1
      DO I=1,IM-1
        UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT
        VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT
!
        IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1                    &
                         .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          U(II,K,1)=UB(N,K,1)
          V(II,K,1)=VB(N,K,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  NORTHERN BOUNDARY
!
      DO I=1,IM-1
        UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT
        VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT
!
        IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1                   &
                          .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          U(II,K,JJM)=UB(N,K,1)
          V(II,K,JJM)=VB(N,K,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  WESTERN BOUNDARY
!
      DO J=2,JM-1,2
        UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT
        VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT
!
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                    &
                         .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(1,K,JJ)=UB(N,K,1)
          V(1,K,JJ)=VB(N,K,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  EASTERN BOUNDARY
!
      DO J=2,JM-1,2
        UB(N,K,1)=UB(N,K,1)+UB(N,K,2)*DT
        VB(N,K,1)=VB(N,K,1)+VB(N,K,2)*DT
!
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                   &
                          .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          U(IIM,K,JJ)=UB(N,K,1)
          V(IIM,K,JJ)=VB(N,K,1)
        ENDIF
!
        N=N+1
      ENDDO
!----------------------------------------------------------------------
!
  100 CONTINUE
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  EXTRAPOLATION OF TANGENTIAL VELOCITY AT OUTFLOW POINTS
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      DO 200 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS1_P1,MYIE2_P1
          IF(V(I,K,1).LT.0.)U(I,K,1)=(VTM(I,K,5)+1.)*U(I,K,3)          &
                                     -VTM(I,K,5)    *U(I,K,5)
        ENDDO
      ENDIF
!
!***  NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS1_P1,MYIE2_P1
          IF(V(I,K,JJM).GT.0.)                                         &
              U(I,K,JJM)=(VTM(I,K,JJM-4)+1.)*U(I,K,JJM-2)              &
                         -VTM(I,K,JJM-4)    *U(I,K,JJM-4)
        ENDDO
      ENDIF
!
!***  WESTERN BOUNDARY
!
      DO J=4,JM-3,2
        IF(ILCOL.EQ.1)THEN
!
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            IF(U(1,K,JJ).LT.0.)                                        &
                V(1,K,JJ)=(VTM(3,K,JJ)+1.)*V(2,K,JJ)                   &
                          -VTM(3,K,JJ)    *V(3,K,JJ)
          ENDIF
!
        ENDIF
      ENDDO
!
!***  EASTERN BOUNDARY
!
      DO J=4,JM-3,2
        IF(IRCOL.EQ.1)THEN
!
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            IF(U(IIM,K,JJ).GT.0.)                                      &
                V(IIM,K,JJ)=(VTM(IIM-2,K,JJ)+1.)*V(IIM-1,K,JJ)         &
                            -VTM(IIM-2,K,JJ)    *V(IIM-2,K,JJ)
          ENDIF
!
        ENDIF
      ENDDO
!----------------------------------------------------------------------
!
  200 CONTINUE
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      DO 300 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  SOUTHWEST CORNER
!
      IF(IBROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,K,2)=D06666*(4.*(U(1,K,1)+U(2,K,1)+U(2,K,3))               &
                           + U(1,K,2)+U(1,K,4)+U(2,K,4))               
        V(2,K,2)=D06666*(4.*(V(1,K,1)+V(2,K,1)+V(2,K,3))               &
                            +V(1,K,2)+V(1,K,4)+V(2,K,4))
      ENDIF
!
!***  SOUTHEAST CORNER
!
      IF(IBROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,K,2)=D06666*(4.*(U(IIM-2,K,1)+U(IIM-1,K,1)             &
                                +U(IIM-2,K,3))                         &
                                +U(IIM,K,2)+U(IIM,K,4)+U(IIM-1,K,4))
        V(IIM-1,K,2)=D06666*(4.*(V(IIM-2,K,1)+V(IIM-1,K,1)             &
                                +V(IIM-2,K,3))                         &
                                +V(IIM,K,2)+V(IIM,K,4)+V(IIM-1,K,4))
      ENDIF
!
!***  NORTHWEST CORNER
!
      IF(ITROW.EQ.1.AND.ILCOL.EQ.1)THEN
        U(2,K,JJM-1)=D06666*(4.*(U(1,K,JJM)+U(2,K,JJM)+U(2,K,JJM-2))   &
                                +U(1,K,JJM-1)+U(1,K,JJM-3)             &
                                +U(2,K,JJM-3))
        V(2,K,JJM-1)=D06666*(4.*(V(1,K,JJM)+V(2,K,JJM)+V(2,K,JJM-2))   &
                                +V(1,K,JJM-1)+V(1,K,JJM-3)             &
                                +V(2,K,JJM-3))
      ENDIF
!
!***  NORTHEAST CORNER
!
      IF(ITROW.EQ.1.AND.IRCOL.EQ.1)THEN
        U(IIM-1,K,JJM-1)=                                              &
          D06666*(4.*(U(IIM-2,K,JJM)+U(IIM-1,K,JJM)+U(IIM-2,K,JJM-2))  &
                     +U(IIM,K,JJM-1)+U(IIM,K,JJM-3)+U(IIM-1,K,JJM-3))
        V(IIM-1,K,JJM-1)=                                              &
          D06666*(4.*(V(IIM-2,K,JJM)+V(IIM-1,K,JJM)+V(IIM-2,K,JJM-2))  &
                     +V(IIM,K,JJM-1)+V(IIM,K,JJM-3)+V(IIM-1,K,JJM-3))
      ENDIF
!
!----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF U AND V AT THE INNER BOUNDARY
!----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS2,MYIE2
          U(I,K,2)=(U(I-1,K,1)+U(I,K,1)+U(I-1,K,3)+U(I,K,3))*0.25
          V(I,K,2)=(V(I-1,K,1)+V(I,K,1)+V(I-1,K,3)+V(I,K,3))*0.25
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS2,MYIE2
          U(I,K,JJM-1)=(U(I-1,K,JJM-2)+U(I,K,JJM-2)                    &
                       +U(I-1,K,JJM)+U(I,K,JJM))*0.25
          V(I,K,JJM-1)=(V(I-1,K,JJM-2)+V(I,K,JJM-2)                    &
                       +V(I-1,K,JJM)+V(I,K,JJM))*0.25
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      DO J=3,JM-2,2
        IF(ILCOL.EQ.1)THEN
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            U(1,K,JJ)=(U(1,K,JJ-1)+U(2,K,JJ-1)                         &
                      +U(1,K,JJ+1)+U(2,K,JJ+1))*0.25
            V(1,K,JJ)=(V(1,K,JJ-1)+V(2,K,JJ-1)                         &
                      +V(1,K,JJ+1)+V(2,K,JJ+1))*0.25
          ENDIF
        ENDIF
      ENDDO
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(IRCOL.EQ.1)THEN
        DO J=3,JM-2,2
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            U(IIM-1,K,JJ)=0.25*(U(IIM-1,K,JJ-1)+U(IIM,K,JJ-1)          &
                               +U(IIM-1,K,JJ+1)+U(IIM,K,JJ+1))
            V(IIM-1,K,JJ)=0.25*(V(IIM-1,K,JJ-1)+V(IIM,K,JJ-1)          &
                               +V(IIM-1,K,JJ+1)+V(IIM,K,JJ+1))
          ENDIF
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
  300 CONTINUE
!
!----------------------------------------------------------------------
      END SUBROUTINE BOCOV
!----------------------------------------------------------------------
!**********************************************************************
      SUBROUTINE BOCOH_DRY(NTSD,DT,NEST,NBC,NBOCO,NTSTM,TSPH           &
                          ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM               &
                          ,PDB,TB,QB,UB,VB,Q2B,CWMB                    &
                          ,PD,T,Q2,PINT                                &
                          ,IDS,IDE,JDS,JDE,KDS,KDE                     &
                          ,IMS,IME,JMS,JME,KMS,KME                     &
                          ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    BOCOH_DRY   UPDATE MASS POINTS ON BOUNDARY
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
!     
! ABSTRACT:
!     TEMPERATURE, SURFACE PRESSURE, AND TKE
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
!   02-04-17  BLACK      - REMOVED WATER FROM BOCOH
!     
! USAGE: CALL BOCOH_DRY FROM SUBROUTINE DIGITAL_FILTER
!   INPUT ARGUMENT LIST:
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
!   MACHINE : IBM SP
!$$$  
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: NEST
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: LB,NBC,NTSD,NTSTM
      INTEGER,INTENT(INOUT) :: NBOCO
!
      REAL,INTENT(IN) :: DT,PDTOP,PT,TSPH
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME),INTENT(IN) :: HTM
!
      REAL,DIMENSION(LB,2),INTENT(INOUT) :: PDB
!
      REAL,DIMENSION(LB,KMS:KME-1,2),INTENT(INOUT) :: TB,QB,UB,VB      &
                                                     ,Q2B,CWMB
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME),INTENT(INOUT) :: T,Q2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,II,IIM,IM,IRTN,ISIZ1,ISIZ2                          &
                ,J,JJ,JJM,JM,K,N,NN,NREC,REC
!
      REAL :: BCHR,RHTM,SHTM
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IM=IDE-IDS+1
      JM=JDE-JDS+1
      ISIZ1=2*LB
      ISIZ2=2*LB*(KME-KMS)
!
!----------------------------------------------------------------------
!***  READ FRESH BOUNDARY DATA IF NECESSARY
!----------------------------------------------------------------------
!
      IF(NTSD-1.EQ.NBOCO.and.ntsd.lt.8641)THEN
!
        IF(MYPE.EQ.0.AND.NEST)THEN
          NREC=NINT((NTSD-1)*DT/3600.)+2
          READ(NBC,REC=NREC)BCHR                                       &
                         ,((PDB(N,NN),N=1,LB),NN=1,2)                  &
                         ,(((TB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((QB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((UB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                         ,(((VB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                        ,(((Q2B(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)     &
                       ,(((CWMB(N,K,NN),N=1,LB),K=KTS,KTE),NN=1,2)
        ENDIF
!
        IF(MYPE.EQ.0.AND..NOT.NEST)THEN
          READ(NBC)PDB
          READ(NBC)TB
          READ(NBC)QB
          READ(NBC)UB
          READ(NBC)VB
          READ(NBC)Q2B
          READ(NBC)CWMB
        ENDIF
!
#ifdef DM_PARALLEL
        CALL MPI_BCAST(PDB,ISIZ1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(TB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(UB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VB,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(Q2B,ISIZ2,MPI_REAL,0,MPI_COMM_COMP,IRTN)
#endif
!***
!***    FIND NEXT BOUNDARY CONDITION READ
!***
        IF(NTSD.LT.NTSTM)THEN
          IF(MYPE.EQ.0.AND.NEST)BCHR=BCHR+1    ! This assumes 1-hrly BC's
          IF(MYPE.EQ.0.AND..NOT.NEST)READ(NBC)BCHR
#ifdef DM_PARALLEL
          CALL MPI_BCAST(BCHR,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)
#endif
          NBOCO=INT(BCHR*TSPH+0.5)
        ENDIF
!
      ENDIF
!----------------------------------------------------------------------
      IIM=IM-MY_IS_GLB+1
      JJM=JM-MY_JS_GLB+1
!
!----------------------------------------------------------------------
!***  UPDATE THE SURFACE PRESSURE
!----------------------------------------------------------------------
!
      N=1
!
!***  SOUTH BOUNDARY
!
      DO I=1,IM
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
        IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1                    &
                         .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          PD(II,1)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  NORTH BOUNDARY
!
      DO I=1,IM
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
        IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1                   &
                          .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          PD(II,JJM)=PDB(N,1)
        ENDIF
        N=N+1
!
      ENDDO
!
!***  WEST BOUNDARY
!
      DO J=3,JM-2,2
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                    &
                         .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          PD(1,JJ)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!***  EAST BOUNDARY
!
      DO J=3,JM-2,2
        PDB(N,1)=PDB(N,1)+PDB(N,2)*DT
!
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                   &
                          .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          PD(IIM,JJ)=PDB(N,1)
        ENDIF
!
        N=N+1
      ENDDO
!
!----------------------------------------------------------------------
!***  UPDATE THE 3-D MASS VARIABLES
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      DO 100 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  SOUTHERN BOUNDARY
!
      N=1
      DO I=1,IM
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
!
        IF(MY_JS_GLB.EQ.1.AND.I.GE.MY_IS_GLB-ILPAD1                    &
                         .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          T(II,K,1)=TB(N,K,1)
          Q2(II,K,1)=Q2B(N,K,1)
          PINT(II,K+1,1)=ETA1(K+1)*PDTOP                               &
                        +ETA2(K+1)*PD(II,1)*RES(II,1)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  NORTHERN BOUNDARY
!
      DO I=1,IM
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
!
        IF(MY_JE_GLB.EQ.JM.AND.I.GE.MY_IS_GLB-ILPAD1                   &
                          .AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          II=I-MY_IS_GLB+1
          T(II,K,JJM)=TB(N,K,1)
          Q2(II,K,JJM)=Q2B(N,K,1)
          PINT(II,K+1,JJM)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(II,JJM)*RES(II,JJM)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  WESTERN BOUNDARY
!
      DO J=3,JM-2,2
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
!
        IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                    &
                         .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          T(1,K,JJ)=TB(N,K,1)
          Q2(1,K,JJ)=Q2B(N,K,1)
          PINT(1,K+1,JJ)=ETA1(K+1)*PDTOP                               &
                        +ETA2(K+1)*PD(1,JJ)*RES(1,JJ)+PT
        ENDIF
!
        N=N+1
      ENDDO
!
!***  EASTERN BOUNDARY
!
      DO J=3,JM-2,2
        TB(N,K,1)=TB(N,K,1)+TB(N,K,2)*DT
        Q2B(N,K,1)=Q2B(N,K,1)+Q2B(N,K,2)*DT
!
        IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                   &
                          .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
          JJ=J-MY_JS_GLB+1
          T(IIM,K,JJ)=TB(N,K,1)
          Q2(IIM,K,JJ)=Q2B(N,K,1)
          PINT(IIM,K+1,JJ)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(IIM,JJ)*RES(IIM,JJ)+PT
        ENDIF
!
        N=N+1
      ENDDO
!----------------------------------------------------------------------
!
  100 CONTINUE
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  SPACE INTERPOLATION OF PD THEN REMAINING MASS VARIABLES
!***  AT INNER BOUNDARY
!----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,1)+HTM(I+1,KTE,1)+HTM(I,KTE,3)+HTM(I+1,KTE,3)
          PD(I,2)=(PD(I,1)*HTM(I,KTE,1)+PD(I+1,1)*HTM(I+1,KTE,1)       &
                  +PD(I,3)*HTM(I,KTE,3)+PD(I+1,3)*HTM(I+1,KTE,3))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          SHTM=HTM(I,KTE,JJM-2)+HTM(I+1,KTE,JJM-2)+HTM(I,KTE,JJM)      &
                                                +HTM(I+1,KTE,JJM)
          PD(I,JJM-1)=(PD(I,JJM-2)*HTM(I,KTE,JJM-2)                    &
                      +PD(I+1,JJM-2)*HTM(I+1,KTE,JJM-2)                &
                      +PD(I,JJM)*HTM(I,KTE,JJM)                        &
                      +PD(I+1,JJM)*HTM(I+1,KTE,JJM))/SHTM
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(ILCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            SHTM=HTM(1,KTE,JJ-1)+HTM(2,KTE,JJ-1)+HTM(1,KTE,JJ+1)       &
                                              +HTM(2,KTE,JJ+1)
            PD(1,JJ)=(PD(1,JJ-1)*HTM(1,KTE,JJ-1)                       &
                     +PD(2,JJ-1)*HTM(2,KTE,JJ-1)                       &
                     +PD(1,JJ+1)*HTM(1,KTE,JJ+1)                       &
                     +PD(2,JJ+1)*HTM(2,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(IRCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            SHTM=HTM(IIM-1,KTE,JJ-1)+HTM(IIM,KTE,JJ-1)                 &
                +HTM(IIM-1,KTE,JJ+1)+HTM(IIM,KTE,JJ+1)
            PD(IIM-1,JJ)=(PD(IIM-1,JJ-1)*HTM(IIM-1,KTE,JJ-1)           &
                         +PD(IIM,JJ-1)*HTM(IIM,KTE,JJ-1)               &
                         +PD(IIM-1,JJ+1)*HTM(IIM-1,KTE,JJ+1)           &
                         +PD(IIM,JJ+1)*HTM(IIM,KTE,JJ+1))/SHTM
          ENDIF
!
        ENDDO
      ENDIF
!
!----------------------------------------------------------------------
!
      DO 200 K=KTS,KTE
!
!----------------------------------------------------------------------
!
!***  ONE ROW NORTH OF SOUTHERN BOUNDARY
!
      IF(IBROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,1)+HTM(I+1,K,1)+HTM(I,K,3)+HTM(I+1,K,3))
          T(I,K,2)=(T(I,K,1)*HTM(I,K,1)+T(I+1,K,1)*HTM(I+1,K,1)        &
                   +T(I,K,3)*HTM(I,K,3)+T(I+1,K,3)*HTM(I+1,K,3))       &
                   *RHTM
          Q2(I,K,2)=(Q2(I,K,1)*HTM(I,K,1)+Q2(I+1,K,1)*HTM(I+1,K,1)     &
                    +Q2(I,K,3)*HTM(I,K,3)+Q2(I+1,K,3)*HTM(I+1,K,3))    &
                    *RHTM
          PINT(I,K+1,2)=ETA1(K+1)*PDTOP+ETA2(K+1)*PD(I,2)*RES(I,2)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW SOUTH OF NORTHERN BOUNDARY
!
      IF(ITROW.EQ.1)THEN
        DO I=MYIS,MYIE1
          RHTM=1./(HTM(I,K,JJM-2)+HTM(I+1,K,JJM-2)                     &
                  +HTM(I,K,JJM)+HTM(I+1,K,JJM))
          T(I,K,JJM-1)=(T(I,K,JJM-2)*HTM(I,K,JJM-2)                    &
                       +T(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)                &
                       +T(I,K,JJM)*HTM(I,K,JJM)                        &
                       +T(I+1,K,JJM)*HTM(I+1,K,JJM))                   &
                       *RHTM
          Q2(I,K,JJM-1)=(Q2(I,K,JJM-2)*HTM(I,K,JJM-2)                  &
                        +Q2(I+1,K,JJM-2)*HTM(I+1,K,JJM-2)              &
                        +Q2(I,K,JJM)*HTM(I,K,JJM)                      &
                        +Q2(I+1,K,JJM)*HTM(I+1,K,JJM))                 &
                        *RHTM
          PINT(I,K+1,JJM-1)=ETA1(K+1)*PDTOP                            &
                           +ETA2(K+1)*PD(I,JJM-1)*RES(I,JJM-1)+PT
        ENDDO
      ENDIF
!
!***  ONE ROW EAST OF WESTERN BOUNDARY
!
      IF(ILCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IS_GLB.EQ.1.AND.J.GE.MY_JS_GLB-JBPAD1                  &
                           .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            RHTM=1./(HTM(1,K,JJ-1)+HTM(2,K,JJ-1)                       &
                    +HTM(1,K,JJ+1)+HTM(2,K,JJ+1))
            T(1,K,JJ)=(T(1,K,JJ-1)*HTM(1,K,JJ-1)                       &
                      +T(2,K,JJ-1)*HTM(2,K,JJ-1)                       &
                      +T(1,K,JJ+1)*HTM(1,K,JJ+1)                       &
                      +T(2,K,JJ+1)*HTM(2,K,JJ+1))                      &
                      *RHTM
            Q2(1,K,JJ)=(Q2(1,K,JJ-1)*HTM(1,K,JJ-1)                     &
                       +Q2(2,K,JJ-1)*HTM(2,K,JJ-1)                     &
                       +Q2(1,K,JJ+1)*HTM(1,K,JJ+1)                     &
                       +Q2(2,K,JJ+1)*HTM(2,K,JJ+1))                    &
                       *RHTM
            PINT(1,K+1,JJ)=ETA1(K+1)*PDTOP                             &
                          +ETA2(K+1)*PD(1,JJ)*RES(1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!
!***  ONE ROW WEST OF EASTERN BOUNDARY
!
      IF(IRCOL.EQ.1)THEN
        DO J=4,JM-3,2
!
          IF(MY_IE_GLB.EQ.IM.AND.J.GE.MY_JS_GLB-JBPAD1                 &
                            .AND.J.LE.MY_JE_GLB+JTPAD1)THEN
            JJ=J-MY_JS_GLB+1
            RHTM=1./(HTM(IIM-1,K,JJ-1)+HTM(IIM,K,JJ-1)                 &
                    +HTM(IIM-1,K,JJ+1)+HTM(IIM,K,JJ+1))
            T(IIM-1,K,JJ)=(T(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)           &
                          +T(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)               &
                          +T(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)           &
                          +T(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))              &
                          *RHTM
            Q2(IIM-1,K,JJ)=(Q2(IIM-1,K,JJ-1)*HTM(IIM-1,K,JJ-1)         &
                           +Q2(IIM,K,JJ-1)*HTM(IIM,K,JJ-1)             &
                           +Q2(IIM-1,K,JJ+1)*HTM(IIM-1,K,JJ+1)         &
                           +Q2(IIM,K,JJ+1)*HTM(IIM,K,JJ+1))            &
                           *RHTM
            PINT(IIM-1,K+1,JJ)=ETA1(K+1)*PDTOP                         &
                              +ETA2(K+1)*PD(IIM-1,JJ)*RES(IIM-1,JJ)+PT
          ENDIF
!
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!
  200 CONTINUE
!
!----------------------------------------------------------------------
      END SUBROUTINE BOCOH_DRY
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      END MODULE MODULE_BNDRY_COND

!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: SOLVER
!
!-----------------------------------------------------------------------
!
      MODULE MODULE_PATCH_QUILT
!
!-----------------------------------------------------------------------
      USE MODULE_EXT_INTERNAL
!-----------------------------------------------------------------------
!
      PRIVATE
      PUBLIC :: PATCH,QUILT_2
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE PATCH(ARRAYG,ARRAYL                                    &
     &,                IDS,IDE,JDS,JDE,KDS,KDE                          &
     &,                IMS,IME,JMS,JME,KMS,KME                          &
     &,                ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!     PATCH DISTRIBUTES THE ELEMENTS OF REAL GLOBAL 2-D ARRAY ARRAYG TO
!     THE REAL LOCAL 2-D ARRAY ARRAYL.
!
!     AUTHOR: TOM BLACK
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INCLUDE "mpif.h"
!
!-----------------------------------------------------------------------
!***  ARGUMENT VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &,                     IMS,IME,JMS,JME,KMS,KME                     &
     &,                     ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE),INTENT(IN) :: ARRAYG
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ARRAYL
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:) :: ARRAYX
!
      INTEGER :: I,IEND,IPE,IRECV,IRTN,ISEND,ISTART,J,JEND,JSTART,KNT   &
     &,          L,MPI_COMM_COMP,NUMVALS,MYPE,NPES
!
      INTEGER,DIMENSION(4) :: LIMITS
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GET OUR TASK ID AND THE COMMUNICATOR
!-----------------------------------------------------------------------
!
      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE OUTPUT ARRAY
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
        ARRAYL(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TASK 0 FILLS ITS OWN LOCAL DOMAIN THEN PARCELS OUT ALL THE OTHER
!***  PIECES TO THE OTHER TASKS.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      tasks : IF(MYPE==0)THEN
!-----------------------------------------------------------------------
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          ARRAYL(I,J)=ARRAYG(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  TASK 0 NEEDS THE LIMITS FROM EACH OF THE OTHER TASKS AND THEN
!***  SENDS OUT THE APPROPRIATE PIECE OF THE GLOBAL ARRAY.
!-----------------------------------------------------------------------
!
        DO IPE=1,NPES-1
!
          CALL MPI_RECV(LIMITS,4,MPI_INTEGER,IPE,IPE,MPI_COMM_COMP      &
      &                ,ISTAT,IRECV)
!
          ISTART=LIMITS(1)
          IEND=LIMITS(2)
          JSTART=LIMITS(3)
          JEND=LIMITS(4)
!
          NUMVALS=(IEND-ISTART+1)*(JEND-JSTART+1)
          ALLOCATE(ARRAYX(NUMVALS),STAT=I)
 
          KNT=0
!
          DO J=JSTART,JEND
          DO I=ISTART,IEND
            KNT=KNT+1
            ARRAYX(KNT)=ARRAYG(I,J)
          ENDDO
          ENDDO
!
          CALL MPI_SEND(ARRAYX,KNT,MPI_REAL,IPE,IPE,MPI_COMM_COMP,ISEND)
!
          DEALLOCATE(ARRAYX)
!
        ENDDO
!
!-----------------------------------------------------------------------
!***  ALL OTHER TASKS TELL TASK 0 WHAT THEIR HORIZONTAL LIMITS ARE AND
!***  RECEIVE THEIR PIECE OF THE GLOBAL ARRAY FROM TASK 0.
!-----------------------------------------------------------------------
!
      ELSE
!
        LIMITS(1)=ITS
        LIMITS(2)=ITE
        LIMITS(3)=JTS
        LIMITS(4)=JTE
!
        CALL MPI_SEND(LIMITS,4,MPI_INTEGER,0,MYPE,MPI_COMM_COMP,ISEND)
!
        NUMVALS=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(ARRAYX(NUMVALS),STAT=I)
!
        CALL MPI_RECV(ARRAYX,NUMVALS,MPI_REAL,0,MYPE,MPI_COMM_COMP      &
     &,               ISTAT,IRECV)
!
        KNT=0
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          KNT=KNT+1
          ARRAYL(I,J)=ARRAYX(KNT)
        ENDDO
        ENDDO
!
        DEALLOCATE(ARRAYX)
!
!-----------------------------------------------------------------------
!
      ENDIF tasks
!
!-----------------------------------------------------------------------
!     CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!-----------------------------------------------------------------------
!
      END SUBROUTINE PATCH
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE QUILT_2(ARRAYL,ARRAYG                                  &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!     QUILT_2 PULLS TOGETHER THE MPI TASKS' LOCAL ARRAYS ARRAYL AND 
!     THEN QUILTS THEM TOGETHER INTO A SINGLE GLOBAL ARRAY ARRAYG.
!
!     AUTHOR: TOM BLACK
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INCLUDE "mpif.h"
!
!-----------------------------------------------------------------------
!***  ARGUMENT VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &,                     IMS,IME,JMS,JME,KMS,KME                     &
     &,                     ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN)  :: ARRAYL
      REAL,DIMENSION(IDS:IDE,JDS:JDE),INTENT(OUT) :: ARRAYG
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:) :: ARRAYX
!
      INTEGER :: I,IEND,IPE,IRECV,IRTN,ISEND,ISTART,J,JEND,JSTART,KNT   &
     &,          L,MPI_COMM_COMP,NUMVALS,MYPE,NPES
!
      INTEGER,DIMENSION(4) :: LIMITS
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GET OUR TASK ID AND THE COMMUNICATOR
!-----------------------------------------------------------------------
!
      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE OUTPUT ARRAY
!-----------------------------------------------------------------------
!
      DO J=JDS,JDE
      DO I=IDS,IDE
        ARRAYG(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TASK 0 FILLS ITS OWN PART OF THE GLOBAL FIRST.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      tasks : IF(MYPE==0)THEN
!-----------------------------------------------------------------------
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          ARRAYG(I,J)=ARRAYL(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  TASK 0 NEEDS THE LIMITS FROM EACH OF THE OTHER TASKS AND THEN
!***  PULLS IN THE APPROPRIATE PIECES FROM ALL OTHER TASKS.
!-----------------------------------------------------------------------
!
        DO IPE=1,NPES-1
!
          CALL MPI_RECV(LIMITS,4,MPI_INTEGER,IPE,IPE,MPI_COMM_COMP      &
      &                ,ISTAT,IRECV)
!
          ISTART=LIMITS(1)
          IEND=LIMITS(2)
          JSTART=LIMITS(3)
          JEND=LIMITS(4)
!
          NUMVALS=(IEND-ISTART+1)*(JEND-JSTART+1)
          ALLOCATE(ARRAYX(NUMVALS),STAT=I)
!
          CALL MPI_RECV(ARRAYX,NUMVALS,MPI_REAL,IPE,IPE,MPI_COMM_COMP   &
     &                 ,ISTAT,IRECV)
!
          KNT=0
!
          DO J=JSTART,JEND
          DO I=ISTART,IEND
            KNT=KNT+1
            ARRAYG(I,J)=ARRAYX(KNT)
          ENDDO
          ENDDO
!
          DEALLOCATE(ARRAYX)
!
        ENDDO
!
!-----------------------------------------------------------------------
!***  ALL OTHER TASKS TELL TASK 0 WHAT THEIR HORIZONTAL LIMITS ARE AND
!***  SEND THEIR LOCAL ARRAY TO TASK 0.
!-----------------------------------------------------------------------
!
      ELSE
!
        LIMITS(1)=ITS
        LIMITS(2)=ITE
        LIMITS(3)=JTS
        LIMITS(4)=JTE
!
        CALL MPI_SEND(LIMITS,4,MPI_INTEGER,0,MYPE,MPI_COMM_COMP,ISEND)
!
        NUMVALS=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(ARRAYX(NUMVALS),STAT=I)
!
        KNT=0
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          KNT=KNT+1
          ARRAYX(KNT)=ARRAYL(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_SEND(ARRAYX,NUMVALS,MPI_REAL,0,MYPE,MPI_COMM_COMP      &
     &,               ISEND)
!
        DEALLOCATE(ARRAYX)
!
!-----------------------------------------------------------------------
!
      ENDIF tasks
!
!-----------------------------------------------------------------------
!     CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!-----------------------------------------------------------------------
!
      END SUBROUTINE QUILT_2
!
!-----------------------------------------------------------------------
!
      END MODULE MODULE_PATCH_QUILT
!
!-----------------------------------------------------------------------

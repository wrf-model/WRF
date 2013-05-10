!-----------------------------------------------------------------------
      SUBROUTINE DSTRB(ARRAYG,ARRAYL,LGS,LGE,LLS,LLE,L1                 &
     &,                IDS,IDE,JDS,JDE,KDS,KDE                          &
     &,                IMS,IME,JMS,JME,KMS,KME                          &
     &,                ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!     DSTRB DISTRIBUTES THE ELEMENTS OF REAL GLOBAL ARRAY ARRG TO THE
!     REAL LOCAL ARRAYS ARRL.  LG IS THE VERTICAL DIMENSION OF THE
!     GLOBAL ARRAY.  LL IS THE VERTICAL DIMENSION OF THE LOCAL ARRAY.
!     L1 IS THE SPECIFIC LEVEL OF ARRL THAT IS BEING FILLED DURING
!     THIS CALL (PERTINENT WHEN LG=1 AND LL>1).
!-----------------------------------------------------------------------
      USE MODULE_EXT_INTERNAL
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!-----------------------------------------------------------------------
!***
!***  ARGUMENT VARIABLES
!***
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &,                     IMS,IME,JMS,JME,KMS,KME                     &
     &,                     ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: L1,LGE,LGS,LLE,LLS
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE,LGS:LGE),INTENT(IN) :: ARRAYG
      REAL,DIMENSION(IMS:IME,JMS:JME,LLS:LLE),INTENT(OUT) :: ARRAYL
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      REAL,ALLOCATABLE,DIMENSION(:) :: ARRAYX
!
      INTEGER :: I,IEND,IPE,IRECV,IRTN,ISEND,ISTART,J,JEND,JSTART,KNT   &
     &,          L,MPI_COMM_COMP,NUMVALS,MYPE,NPES
      INTEGER,DIMENSION(4) :: LIMITS
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
#else
      INTEGER :: I,L,J
#endif
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
!
!***  GET OUR TASK ID AND THE COMMUNICATOR
!
      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
!
!***  INITIALIZE THE OUTPUT ARRAY
!
      DO L=LLS,LLE
      DO J=JMS,JME
      DO I=IMS,IME
        ARRAYL(I,J,L)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TASK 0 FILLS ITS OWN LOCAL DOMAIN THEN PARCELS OUT ALL THE OTHER 
!***  PIECES TO THE OTHER TASKS.
!-----------------------------------------------------------------------
!
      tasks : IF(MYPE==0)THEN
!
        IF(LGE==LGS)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            ARRAYL(I,J,L1)=ARRAYG(I,J,LGS)
          ENDDO
          ENDDO
!
        ELSE
!
          DO L=LGS,LGE
            DO J=JTS,JTE
            DO I=ITS,ITE
              ARRAYL(I,J,L)=ARRAYG(I,J,L)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
!
!***  TASK 0 NEEDS THE LIMITS FROM EACH OF THE OTHER TASKS AND THEN
!***  SENDS OUT THE APPROPRIATE PIECE OF THE GLOBAL ARRAY.
!
        DO IPE=1,NPES-1
!
          CALL MPI_RECV(LIMITS,4,MPI_INTEGER,IPE,IPE,MPI_COMM_COMP      &
     &,                 ISTAT,IRECV)
          ISTART=LIMITS(1)
          IEND=LIMITS(2)
          JSTART=LIMITS(3)
          JEND=LIMITS(4)
!
          NUMVALS=(IEND-ISTART+1)*(JEND-JSTART+1)*(LGE-LGS+1)
          ALLOCATE(ARRAYX(NUMVALS),STAT=I)
          
          KNT=0
!
          DO L=LGS,LGE
            DO J=JSTART,JEND
            DO I=ISTART,IEND
              KNT=KNT+1
              ARRAYX(KNT)=ARRAYG(I,J,L)
            ENDDO
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
        NUMVALS=(ITE-ITS+1)*(JTE-JTS+1)*(LGE-LGS+1)
        ALLOCATE(ARRAYX(NUMVALS),STAT=I)
!
        CALL MPI_RECV(ARRAYX,NUMVALS,MPI_REAL,0,MYPE,MPI_COMM_COMP      &
     &,               ISTAT,IRECV)
!
        KNT=0
        IF(LGE==LGS)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            KNT=KNT+1
            ARRAYL(I,J,L1)=ARRAYX(KNT)
          ENDDO
          ENDDO
        ELSE
          DO L=LGS,LGE
            DO J=JTS,JTE
            DO I=ITS,ITE
              KNT=KNT+1
              ARRAYL(I,J,L)=ARRAYX(KNT)
            ENDDO
            ENDDO
          ENDDO
        ENDIF
!
        DEALLOCATE(ARRAYX)
!
!-----------------------------------------------------------------------
!
      ENDIF tasks
!
!-----------------------------------------------------------------------
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!-----------------------------------------------------------------------
!
!
!***  INITIALIZE THE OUTPUT ARRAY
!
      ARRAYL=0.0

      DO L=LGS,LGE
      DO J=JDS,JDE
      DO I=IDS,IDE
        ARRAYL(I,J,L)=ARRAYG(I,J,L)
      ENDDO
      ENDDO
      ENDDO

#endif
      END SUBROUTINE DSTRB
!
!-----------------------------------------------------------------------

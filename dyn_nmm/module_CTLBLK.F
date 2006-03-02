!
      MODULE MODULE_CTLBLK
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!***  FUNDAMENTAL DOMAIN VARIABLES
!
      INTEGER :: IM,JM,LM
      INTEGER :: NROOT
!----------------------------------------------------------------------
!
!***  SET SOME CONSTANTS
!
      INTEGER :: LIST=6            ! STANDARD OUT UNIT NUMBER
      INTEGER,PARAMETER :: LSM=39  ! NUMBER OF OUTPUT PRESSURE LEVELS
!----------------------------------------------------------------------
!
!***  SINGLE GLOBAL OR MULTIPLE LOCAL RESTART FILES
!
      LOGICAL :: SINGLRST
!----------------------------------------------------------------------
!
!***  VARIOUS CONTROL VARIABLES
!
      LOGICAL :: RUN,FIRST,RESTRT,NEST
!
!JW   REAL :: DT,TLM0D,TPH0D,TSPH
      REAL :: TLM0D,TPH0D,TSPH
!
      INTEGER :: IHRST,NFCST,NUNIT_NBC,IOUT                            &
                ,NTSTM,NSTART,NTDDMP,NBOCO,NSHDE 
!               ,NTSD,NTSTM,NSTART,NTDDMP,NPREC,NBOCO,NSHDE 
!
      INTEGER,DIMENSION(3) :: IDAT
!
      INTEGER,DIMENSION(99) :: ISHDE
!
      REAL,DIMENSION(LSM) :: SPL
!----------------------------------------------------------------------
      END MODULE MODULE_CTLBLK

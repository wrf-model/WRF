!
      MODULE MODULE_PHYS
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  PHYSICS-RELATED QUANTITIES
!
!jw   INTEGER,PARAMETER :: ITB=76,ITBQ=152,JTB=134,JTBQ=440
!
!jw   REAL,DIMENSION(ITB) :: STHE,THE0
!jw   REAL,DIMENSION(JTB) :: QS0,SQS
!jw   REAL,DIMENSION(ITBQ) :: STHEQ,THE0Q
!jw   REAL,DIMENSION(ITB,JTB) :: PTBL
!jw   REAL,DIMENSION(JTB,ITB) :: TTBL
!jw   REAL,DIMENSION(JTBQ,ITBQ) :: TTBLQ
!
!
!
!jw   REAL :: CI,CS,DI,DS,DTD,DTQ2,PL,PLQ,RDP,RDPQ,RDQ,RDTH,RDTHE &
!jw          ,RDTHEQ,ROI,ROS,SUN_DIST,TDTQ2,THL
!
!jw   REAL,DIMENSION(4) :: PTOPC
!
!jw   INTEGER :: KTM
!
!jw   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: LVL
!
      INTEGER,DIMENSION(3) :: LPTOP
!
      CHARACTER(20) :: CNV_SCHEME,LAND_SCHEME,MP_SCHEME,PBL_SCHEME     &
                      ,RADLW_SCHEME,RADSW_SCHEME,SFC_SCHEME
!----------------------------------------------------------------------
      END MODULE MODULE_PHYS

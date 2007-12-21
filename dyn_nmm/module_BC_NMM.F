!
      MODULE MODULE_BC_NMM
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!***  PRIMARY BOUNDARY CONDITION ARRAYS
!
      REAL,ALLOCATABLE,DIMENSION(:,:) :: PDB_ORIG
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TB_ORIG,QB_ORIG             &
     &                                    ,UB_ORIG,VB_ORIG             &
     &                                    ,Q2B_ORIG,CWMB_ORIG
!
!----------------------------------------------------------------------
      END MODULE MODULE_BC_NMM

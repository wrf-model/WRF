!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Driver for the Adjoint (ADJ) model
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM KPP_ROOT_ADJ_Driver

  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

      KPP_REAL :: T, DVAL(NSPEC)
      INTEGER :: i, j, ind_1 = 1, ind_2 = 2
      ! INTEGER :: ind_1 = ind_NO2, ind_2 = ind_O3

! ---  Number of functional for which sensitivities are computed
! ---  Note: this value is set for sensitivities w.r.t. all initial values
! ---       it may have to be changed for other applications
      INTEGER NADJ
      PARAMETER (NADJ = 2)
      KPP_REAL  Y_ADJ(NVAR,NADJ)
  
! ---- TIME VARIABLES ------------------      

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      DO i=1,NVAR
        RTOL(i) = 1.0d-4
        ATOL(i) = 1.0d-3
      END DO
     
      CALL Initialize()
      
!~~~>  Note: the initial values below are adjoint values at the final time
      Y_ADJ(1:NVAR,1:NADJ) = 0.0d0
      Y_ADJ(ind_1,1) = 1.0d0
      Y_ADJ(ind_2,2) = 1.0d0

! ~~~~~~~~~ BEGIN TIME LOOP ~~~~~~~~~~

      CALL InitSaveData()

      T = TSTART

      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,        &
                  (TRIM(SPC_NAMES(MONITOR(i))),            &
                    C(MONITOR(i))/CFACTOR, i=1,NMONITOR),  &
                  (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

      CALL INTEGRATE_ADJ( NADJ, VAR, Y_ADJ, T, TEND )


      CALL GetMass( C, DVAL )
      WRITE(6,991) (TEND-TSTART)/(TEND-TSTART)*100, TEND,  &
                  (TRIM(SPC_NAMES(MONITOR(i))),            &
                    C(MONITOR(i))/CFACTOR, i=1,NMONITOR),  &
                  (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

! ~~~~~~~~~ END TIME LOOP ~~~~~~~~~~

      OPEN(20, FILE='KPP_ROOT_ADJ_results.m')
      WRITE(6,*) '**************************************************'
      WRITE(6,*) ' Concentrations and Sensitivities at final time '
      WRITE(6,*) ' were written in the file KPP_ROOT_ADJ_results.m'
      WRITE(6,*) '**************************************************'
      DO j=1,NADJ
        WRITE(20,993) ( Y_ADJ(i,j), i=1,NVAR )          
      END DO

      WRITE(6,995) TRIM(SPC_NAMES(ind_1)),TRIM(SPC_NAMES(ind_1)), &
                   Y_ADJ(ind_1,1)
      WRITE(6,995) TRIM(SPC_NAMES(ind_2)),TRIM(SPC_NAMES(ind_2)), &
                   Y_ADJ(ind_2,2)
      WRITE(6,995) TRIM(SPC_NAMES(ind_2)),TRIM(SPC_NAMES(ind_1)), &
                   Y_ADJ(ind_1,2)
      WRITE(6,995) TRIM(SPC_NAMES(ind_1)),TRIM(SPC_NAMES(ind_2)), &
                   Y_ADJ(ind_2,1)

      CALL CloseSaveData()
      
 991  FORMAT(F6.1,'%. T=',E10.3,3X,20(A,'=',E10.4,';',1X))
 993  FORMAT(1000(E24.16,2X))
 995  FORMAT('ADJ: d[',A,'](tf)/d[',A,'](t0)=',E14.7)

END PROGRAM KPP_ROOT_ADJ_Driver


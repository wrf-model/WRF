!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Driver for the tangent linear model (TLM)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM KPP_ROOT_TLM_Driver

  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

      KPP_REAL :: T, DVAL(NSPEC)
      INTEGER :: i, j, ind_1 = 1, ind_2 = 2
      ! INTEGER :: ind_1 = ind_NO2, ind_2 = ind_O3

! ---  Number of sensitivity coefficients to compute
! ---  Note: this value is set for sensitivities w.r.t. all initial values
! ---       it may have to be changed for other applications
      INTEGER NTLM
      PARAMETER (NTLM = 2)
      KPP_REAL  Y_TLM(NVAR,NTLM)
  
! ---- TIME VARIABLES ------------------      

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      DO i=1,NVAR
        RTOL(i) = 1.0d-4
        ATOL(i) = 1.0d-2
      END DO
     
      CALL Initialize()
!~~~> Note: the initial values below are for sensitivities 
!           w.r.t. initial values;
!           they have to be changed for other applications
      DO j=1,NTLM
        DO i=1,NVAR
           Y_TLM(i,j) = 0.0d0
        END DO
      END DO
      Y_TLM(ind_1,1) = 1.0d0
      Y_TLM(ind_2,2) = 1.0d0

! ~~~~~~~~~ BEGIN TIME LOOP ~~~~~~~~~~

      CALL InitSaveData()

      T = TSTART
      
kron: DO WHILE (T < TEND)

        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,      &
                    (TRIM(SPC_NAMES(MONITOR(i))),          &
                      C(MONITOR(i))/CFACTOR, i=1,NMONITOR),&
                    (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

        CALL SaveData()

        CALL INTEGRATE_TLM( NTLM, VAR, Y_TLM, T, T+DT )

        T = T+DT

      END DO kron

      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,        &
                  (TRIM(SPC_NAMES(MONITOR(i))),            &
                    C(MONITOR(i))/CFACTOR, i=1,NMONITOR),  &
                  (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

! ~~~~~~~~~ END TIME LOOP ~~~~~~~~~~

      OPEN(20, FILE='KPP_ROOT_TLM_results.m')
      WRITE(6,*) '**************************************************'
      WRITE(6,*) ' Concentrations and Sensitivities at final time '
      WRITE(6,*) ' were written in the file KPP_ROOT_TLM_results.m'
      WRITE(6,*) '**************************************************'
      DO j=1,NTLM
        WRITE(20,993) ( Y_TLM(i,j), i=1,NVAR )          
      END DO

      CALL CloseSaveData()

      WRITE(6,995) TRIM(SPC_NAMES(ind_1)),TRIM(SPC_NAMES(ind_1)), &
                   Y_TLM(ind_1,1)
      WRITE(6,995) TRIM(SPC_NAMES(ind_2)),TRIM(SPC_NAMES(ind_2)), &
                   Y_TLM(ind_2,2)
      WRITE(6,995) TRIM(SPC_NAMES(ind_1)),TRIM(SPC_NAMES(ind_2)), &
                   Y_TLM(ind_2,1)
      WRITE(6,995) TRIM(SPC_NAMES(ind_2)),TRIM(SPC_NAMES(ind_1)), &
                   Y_TLM(ind_1,2)
                 
 991  FORMAT(F6.1,'%. T=',E10.3,3X,20(A,'=',E10.4,';',1X))
 993  FORMAT(1000(E24.16,2X))
 995  FORMAT('TLM: d[',A,'](tf)/d[',A,'](t0)=',E14.7)

END PROGRAM KPP_ROOT_TLM_Driver


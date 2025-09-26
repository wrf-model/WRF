!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Driver for the tangent linear model
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM KPP_ROOT_ADJ_Driver

  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

      KPP_REAL :: T, DVAL(NSPEC)
      INTEGER :: i, j, ind_1 = ind_NO2, ind_2 = ind_O3

! ---  Number of functional for which sensitivities are computed
! ---  Note: this value is set for sensitivities w.r.t. all initial values
! ---       it may have to be changed for other applications
      INTEGER NADJ
      PARAMETER (NADJ = 2)
      KPP_REAL  Y_ADJ(NVAR,NADJ)
      REAL(kind=dble_p)  R1(NVAR), R2(NVAR), V1, V2
  
! ---- TIME VARIABLES ------------------      

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      CALL SRAND(89)
      RTOLS = 1.0d-3
      DO i=1,NVAR
        RTOL(i) = RTOLS
        ATOL(i) = 1.0d-2
        R1(i) = 10*(RAND()-0.5d0)
        R2(i) = 10*(RAND()-0.5d0)
      END DO
     
      CALL Initialize()
! ---  Note: the initial values below are adjoint values at the final time
     Y_ADJ(1:NVAR,1) = R1(1:NVAR)
     Y_ADJ(1:NVAR,2) = R2(1:NVAR)

! ********** T LOOP *************************

      CALL InitSaveData()

      WRITE(6,990) (SPC_NAMES(MONITOR(i)), i=1,NMONITOR)
990   FORMAT('DOne[%] Time[h] ',20(4X,A12))

      T = TSTART

        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T/3600.,   &
                    (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),  &
                   (DVAL(i)/CFACTOR, i=1,NMASS)
991     FORMAT(F6.1,'% ',F7.2,3X,20(E10.4,2X))

        CALL SaveData()

        CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE_ADJ( NADJ, Y_ADJ, T, TEND )

      V1 = 0.0d0
      V2 = 0.0d0
      DO i=1,NVAR
        V1 = V1 + Y_ADJ(i,1)*R2(i)
        V2 = V2 + Y_ADJ(i,2)*R1(i)
      END DO

      PRINT*,'**************************************************'
      WRITE(6,887) V1
      WRITE(6,888) V2
      WRITE(6,889) ABS(V1-V2)/MAX(ABS(V1),ABS(V2))
887   FORMAT('u.M''*M''.v = ',E24.14 )
888   FORMAT('v.M''*M''.u = ',E24.14 )
889   FORMAT('RelativeErr=',E10.3 )
      PRINT*,'**************************************************'

      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T/3600.,    &
                    (C(MONITOR(i))/CFACTOR, i=1,NMONITOR), &
                    (DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

! *********** END TIME LOOP ********
      OPEN(20, FILE='KPP_ROOT_ADJ_results.m')
      WRITE(6,*) '**************************************************'
      WRITE(6,*) ' Concentrations and Sensitivities at final time '
      WRITE(6,*) ' were written in the file KPP_ROOT_ADJ_results.m'
      WRITE(6,*) '**************************************************'
      DO j=1,NADJ
        WRITE(20,993) ( Y_ADJ(i,j), i=1,NVAR )          
      END DO
 993  FORMAT(1000(E24.16,2X))

      PRINT*,'ADJ: d[',TRIM(SPC_NAMES(ind_1)),'](tf) / d[', &
            TRIM(SPC_NAMES(ind_1)),'](t0)=', Y_ADJ(ind_1,1)
      PRINT*,'ADJ: d[',TRIM(SPC_NAMES(ind_2)),'](tf) / d[', &
            TRIM(SPC_NAMES(ind_2)),'](t0)=', Y_ADJ(ind_2,2)
      PRINT*,'ADJ: d[',TRIM(SPC_NAMES(ind_1)),'](tf) / d[', &
            TRIM(SPC_NAMES(ind_2)),'](t0)=', Y_ADJ(ind_1,2)
      PRINT*,'ADJ: d[',TRIM(SPC_NAMES(ind_2)),'](tf) / d[', &
            TRIM(SPC_NAMES(ind_1)),'](t0)=', Y_ADJ(ind_2,1)

      PRINT*,'TLM: d[NO2](tf) / d[NO2](t0)=  1.714961808143527E-002'
      PRINT*,'TLM: d[ O3](tf) / d[ O3](t0)= -4.447774183920545E-003'
      PRINT*,'TLM: d[NO2](tf) / d[ O3](t0)=  0.897512294491540'
      PRINT*,'TLM: d[ O3](tf) / d[NO2](t0)= -5.543729901774693E-005'
 

      CALL CloseSaveData()

END PROGRAM KPP_ROOT_ADJ_Driver


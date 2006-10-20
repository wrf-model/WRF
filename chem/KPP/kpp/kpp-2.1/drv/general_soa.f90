!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Driver for the Second Order Adjoint (SOA) model
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM KPP_ROOT_SOA_Driver

  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

      KPP_REAL :: T, DVAL(NSPEC)
      INTEGER :: i, j, ind_1 = 1, ind_2 = 2
      ! INTEGER :: ind_1 = ind_NO2, ind_2 = ind_O3
 
!~~~>  Number of second order adjoints
!      i.e., number of vectors U_i s.t. SOA_i = Hess*U_i
!      1 <= i <= NSOA

      INTEGER, PARAMETER :: NSOA = 2

      KPP_REAL :: Y_tlm(NVAR,NSOA)
      KPP_REAL :: Y_adj(NVAR)
      KPP_REAL :: Y_soa(NVAR,NSOA)
  
      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      DO i=1,NVAR
        RTOL(i) = 1.0d-4
        ATOL(i) = 1.0d-3
      END DO
     
      CALL Initialize()
!~~~>  Tangent linear variable values at the initial time
      Y_tlm(1:NVAR,1:NSOA) = 0.0d0
      Y_tlm(ind_1,1) = 1.0d0
      Y_tlm(ind_2,2) = 1.0d0
!~~~>  Adjoint values at the final time
      Y_adj(1:NVAR) = 0.0d0
      Y_adj(ind_1)  = 1.0d0
!~~~>  2nd order adjoint values at the final time      
      Y_soa(1:NVAR,1:NSOA) = 0.0d0
      Y_soa(ind_1,1) = 1.0d0
      Y_soa(ind_2,2) = 1.0d0

!~~~~~~~~~~~ Time LOOP ~~~~~~~~~~~~~

      CALL InitSaveData()

      T = TSTART

        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,      &
                    (TRIM(SPC_NAMES(MONITOR(i))),          &
                      C(MONITOR(i))/CFACTOR, i=1,NMONITOR),&
                    (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

        CALL SaveData()

        CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE_SOA(NSOA, VAR, Y_tlm, Y_adj, Y_soa, T, TEND)


      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,        &
                    (TRIM(SPC_NAMES(MONITOR(i))),          &
                      C(MONITOR(i))/CFACTOR, i=1,NMONITOR),&
                    (TRIM(SMASS(i)),DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

!~~~~~~~~~~~ END Time LOOP ~~~~~~~~~~~~~

      WRITE(6,*) '**************************************************'
      WRITE(6,*) ' Results were written in the files'
      WRITE(6,*) ' KPP_ROOT_[TLM|ADJ|SOA].m'
      WRITE(6,*) '**************************************************'
      
      PRINT 995,TRIM(SPC_NAMES(ind_1)),       &
            TRIM(SPC_NAMES(ind_1)),           &
            Y_tlm(ind_1,1), Y_adj(ind_1)
      PRINT 995,TRIM(SPC_NAMES(ind_1)),       &
            TRIM(SPC_NAMES(ind_2)),           &
            Y_tlm(ind_1,2), Y_adj(ind_2)

      DO j=1,NSOA
         PRINT 997, j,(TRIM(SPC_NAMES(i)),Y_soa(i,j),i=1,NVAR)
      END DO
      
      OPEN(53,FILE='KPP_ROOT_TLM.m')
      DO j=1, NSOA
         WRITE(53,993), (Y_tlm(i,j),i=1,NVAR)
      END DO
      CLOSE(53)
      
      OPEN(54,FILE='KPP_ROOT_ADJ.m')
      WRITE(54,993), (Y_adj(i),i=1,NVAR)
      CLOSE(54)
      
      OPEN(55,FILE='KPP_ROOT_SOA.m')
      DO j=1, NSOA
         WRITE(55,993), (Y_soa(i,j),i=1,NVAR)
      END DO
      CLOSE(55)
      
 991  FORMAT(F6.1,'%. T=',E10.3,3X,20(A,'=',E10.4,';',1X))
 993  FORMAT(1000(E24.16,2X))
 995  FORMAT('d[',A,'](tf)/d[',A,'](t0). TLM=',E14.7,'  ADJ=',E14.7)
 997  FORMAT('2nd ADJ (',I3,'): ',200(A,'=',E10.3,'; '))

      CALL CloseSaveData()

END PROGRAM KPP_ROOT_SOA_Driver


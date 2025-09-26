C --- Driver for computing sensitivity coefficients w.r.t. all initial values
      PROGRAM driver

      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

C ---  Number of sensitivity coefficients to compute
C ---  Note: this value is set for sensitivities w.r.t. all initial values
C ---       it may have to be changed for other applications
      INTEGER NSENSIT
      PARAMETER (NSENSIT = NVAR)
      
      KPP_REAL  DVAL(NSPEC)
      KPP_REAL  Y(NVAR*(NSENSIT+1))
C ---  Note: Y contains: (1:NVAR) concentrations, followed by
C ---                   (1:NVAR) sensitivities w.r.t. first parameter, followed by
C ---                   etc.,  followed by          
C ---                   (1:NVAR) sensitivities w.r.t. NSENSIT's parameter

      INTEGER i

C --- The type of sensitivity coefficients to compute
C --- DDMTYPE = 0 : sensitivities w.r.t. initial values
C --- DDMTYPE = 1 : sensitivities w.r.t. parameters
      DDMTYPE = 0
   
C ---- TIME VARIABLES ------------------      

      TSTART = 0
      TEND = TSTART + 600
      DT = 60.
      TEMP = 298

      STEPMIN = 0.01
      STEPMAX = 900

      PRINT*,'Please provide: RTOL = , ATOL = '
      READ*,RTOLS, ATOLS
      do i=1,NVAR
        RTOL(i) = RTOLS
        ATOL(i) = ATOLS
      end do
    
C ********** TIME LOOP *************************

      CALL Initialize()

C -- Initialize Concentrations and Sensitivities
      DO i=1,NVAR
         Y(i) = VAR(i)
      END DO
	
C ---  Note: the initial values below are for sensitivities w.r.t. initial values;
C ---       they may have to be changed for other applications
      DO j=1,NSENSIT
        DO i=1,NVAR
	   Y(i+NVAR*j) = 0.0D0
	END DO
	Y(j+NVAR*j) = 1.0D0
      END DO

      CALL InitSaveData()

      WRITE(6,990) (SPC_NAMES(MONITOR(i)), i=1,NMONITOR), 
     *             (SMASS(i), i=1,NMASS )
990   FORMAT('done[%] Time[h] ',20(4X,A6))

      TIME = TSTART
      DO WHILE (TIME .lt. TEND)

        CALL GetMass( C, DVAL )
        WRITE(6,991) (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600.,
     *               (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),
     *               (DVAL(i)/CFACTOR, i=1,NMASS)
991   FORMAT(F6.1,'% ',F7.2,3X,20(E10.4,2X))

        CALL SaveData()

        CALL Update_SUN() 
        CALL Update_RCONST()
	
        CALL INTEGRATE( NSENSIT, Y, TIME, TIME+DT )

        DO i=1,NVAR
           VAR(i) = Y(i)
        END DO
	
      END DO

      CALL GetMass( C, DVAL )
      WRITE(6,991) (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600.,
     *               (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),
     *               (DVAL(i)/CFACTOR, i=1,NMASS)

C      DO i=1,NSENSIT
C        WRITE(6,992) i, ( Y(NVAR*i+j), j=1,NVAR )          
C      END DO
C 992  FORMAT('SEN(',I3,') = ',1000(E10.4,2X))

      CALL SaveData()

C *********** END TIME LOOP ********
      OPEN(20, FILE='KPP_ROOT_results.m')
      WRITE(6,*) '**************************************************'
      WRITE(6,*) '* Concentrations and Sensitivities at final time *'
      WRITE(6,*) '*  were written in the file KPP_ROOT_results.m  *'
      WRITE(6,*) '**************************************************'
      DO i=0,NSENSIT
        WRITE(20,993) ( Y(NVAR*i+j), j=1,NVAR )          
      END DO
 993  FORMAT(1000(E24.16,2X))

      CALL CloseSaveData()

      STOP
      END


      PROGRAM driver

      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

      KPP_REAL DVAL(NSPEC)
      INTEGER i
  
C ---- TIME VARIABLES ------------------      

      TSTART = 0
      TEND = TSTART + 600
      DT = 60.
      TEMP = 298

      STEPMIN = 0.01
      STEPMAX = 900

      RTOLS = 1e-3
      DO i=1,NVAR
        RTOL(i) = RTOLS
        ATOL(i) = 1
      END DO
     
      CALL Initialize()

C ********** TIME LOOP *************************

      CALL InitSaveData()

      WRITE(6,990) (SPC_NAMES(MONITOR(i)), i=1,NMONITOR), 
     *             (SMASS(i), i=1,NMASS )
990   FORMAT('DOne[%] Time[h] ',20(4X,A6))

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

        CALL INTEGRATE( TIME, TIME+DT )

      END DO

      CALL GetMass( C, DVAL )
      WRITE(6,991) (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600.,
     *               (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),
     *               (DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

C *********** END TIME LOOP ********

      CALL CloseSaveData()

      open(75, file='reference.data')
      do i=1,KPP_NVAR
        write(75,75) VAR(i)
      end do
75    FORMAT(100(E24.14,1X))      

      STOP
      END


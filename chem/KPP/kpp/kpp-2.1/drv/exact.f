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

      RTOLS = 1e-8
      do i=1,NVAR
        RTOL(i) = RTOLS
        ATOL(i) = 1e-3
      end do
     
      CALL Initialize()

C ********** TIME LOOP *************************

      CALL InitSaveData()

      write(6,990) (SPC_NAMES[MONITOR(i)], i=1,NMONITOR), 
     *             (SMASS(i), i=1,NMASS )
990   FORMAT('done[%] Time[h] ',20(4X,A6))

      TIME = TSTART
      do while (TIME .lt. TEND)

        CALL GetMass( C, DVAL )
        write(6,991) (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600.,
     *               (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),
     *               (DVAL(i)/CFACTOR, i=1,NMASS)
991   FORMAT(F6.1,'% ',F7.2,3X,20(E10.4,2X))

        CALL SaveData()

        CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE( TIME, TIME+DT )

      end do

      CALL GetMass( C, DVAL )
      write(6,991) (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600.,
     *               (C(MONITOR(i))/CFACTOR, i=1,NMONITOR),
     *               (DVAL(i)/CFACTOR, i=1,NMASS)

      CALL SaveData()

C *********** END TIME LOOP ********

      CALL CloseSaveData()
      CALL GenerateMatlab(' ')

      STOP
      END


      PROGRAM driver

      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

      INTEGER i
  
C ---- TIME VARIABLES ------------------      

      RTOLS = 1e-3
      do i=1,NVAR
        RTOL(i) = RTOLS
        ATOL(i) = 1E-18
      end do
     
      CALL Initialize()
      
      open(10, file='Extrapd.m')
      write(10,*) 'ed=['
       
      TSTART = 3600*12
      TEND = TSTART + 3600*24*5
      STEPMIN = 0.01
      STEPMAX = 900
      DT = 3600.
      TEMP = 236.21
      
C -- BELOW THIS LIMIT USE TAYLOR INSTEAD OF EXP ---
C ********** TIME LOOP *************************
      TIME = TSTART
      do while (TIME .le. TEND)

        write(6,991) (C(MONITOR(i))/CFACTOR, i=1,NMONITOR)

        write(10,992) (TIME-TSTART)/3600.D0, 
     *                (C(LOOKAT(i))/CFACTOR, i=1,NLOOKAT)

        call Update_SUN() 
        call Update_RCONST()

        call INTEGRATE( TIME, TIME+DT )

      end do
C *********** END TIME LOOP ********

      write(10,*) '];'
      close(10)
      STOP

991   FORMAT('Monitor:',10(1X,E12.6))
992   FORMAT(F6.1,100(1X,D24.16))
      END


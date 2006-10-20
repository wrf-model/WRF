C ****************************************************************
C                            
C InitSaveData - Opens the data file for writing
C   Parameters :                                                  
C
C ****************************************************************

      SUBROUTINE InitSaveData ()

      INCLUDE 'KPP_ROOT_Parameters.h'

      open(10, file='KPP_ROOT.dat')

      RETURN
      END

C End of InitSaveData function
C ****************************************************************

C ****************************************************************
C                            
C SaveData - Write LOOKAT species in the data file 
C   Parameters :                                                  
C
C ****************************************************************

      SUBROUTINE SaveData ()

      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

      INTEGER i

      WRITE(10,999) (TIME-TSTART)/3600.D0,
     *              (C(LOOKAT(i))/CFACTOR, i=1,NLOOKAT)
999   FORMAT(E24.16,100(1X,E24.16))

      RETURN
      END

C End of SaveData function
C ****************************************************************

C ****************************************************************
C                            
C CloseSaveData - Close the data file 
C   Parameters :                                                  
C
C ****************************************************************

      SUBROUTINE CloseSaveData ()

      INCLUDE 'KPP_ROOT_Parameters.h'

      CLOSE(10)

      RETURN
      END

C End of CloseSaveData function
C ****************************************************************

C ****************************************************************
C                            
C GenerateMatlab - Generates MATLAB file to load the data file 
C   Parameters : 
C                It will have a character string to prefix each 
C                species name with.                                                 
C
C ****************************************************************

      SUBROUTINE GenerateMatlab ( PREFIX )

      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
      
      CHARACTER*8 PREFIX 
      INTEGER i

      open(20, file='KPP_ROOT.m')
      write(20,*) 'load KPP_ROOT.dat;'
      write(20,990) PREFIX
990   FORMAT(A1,'c = KPP_ROOT;')
      write(20,*) 'clear KPP_ROOT;'
      write(20,991) PREFIX, PREFIX
991   FORMAT(A1,'t=',A1,'c(:,1);')
      write(20,992) PREFIX
992   FORMAT(A1,'c(:,1)=[];')

      do i=1,NLOOKAT
        write(20,993) PREFIX, SPC_NAMES(LOOKAT(i)), PREFIX, i
993     FORMAT(A1,A6,' = ',A1,'c(:,',I2,');')
      end do
      
      CLOSE(20)

      RETURN
      END

C End of GenerateMatlab function
C ****************************************************************


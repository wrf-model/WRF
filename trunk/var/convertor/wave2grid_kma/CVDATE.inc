      SUBROUTINE CVDATE (IDNEW, ID, KTLAG)                                     
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
C     -------------------------------------------------------------             
C     ---  DATE CALCULATION ---                                                 
C        ID   (5) :  INPUT  DATE  (YEAR, MONTH, DAY, HOUR, DAY OF WEEK)         
C        IDNEW(5) :  OUTPUT DATE                                                
C        KTLAG    :  TIME LAG BETWEEN (ID) AND (IDNEW)                          
C                    IDNEW = ID  +  KTLAG (HOUR)                                
C     -------------------------------------------------------------             
      DIMENSION ID(5), IDNEW(5)                                                 
      INTEGER  MON(12) / 31,28,31,30,31,30,31,31,30,31,30,31 /                  
C     -------------------------------------------------------------             
      NTY = 365*24 ; NTY0=NTY ; NTYL=NTY                                        
C     -------------------------------------------------------------             
      IF (MOD(ID(1)  , 4) .EQ. 0) NTY0 = NTY + 24                               
      IF (MOD(ID(1)-1, 4) .EQ. 0) NTYL = NTY + 24                               
C     -------------------------------------------------------------             
      IF (NTY0 .EQ. NTY) THEN ; MON(2) = 28                                     
                         ELSE ; MON(2) = 29                                     
      END IF                                                                    
C     -------------------------------------------------------------             
      MONTH = ID(2)                                                             
      NTIME = 0                                                                 
C     -------------------------------------------------------------             
      IF (MONTH .GE. 2) THEN                                                    
          DO 110 M=1,MONTH-1                                                    
                 NTIME = NTIME + MON(M)*24                                      
 110      CONTINUE                                                              
      END IF                                                                    
C     -------------------------------------------------------------             
      NTIME = NTIME + 24*(ID(3)-1) + ID(4)                                      
      NTIME = NTIME + KTLAG                                                     
C     -------------------------------------------------------------             
      IF (NTIME .LT. 0) THEN                                                    
          IDNEW(1) = ID(1) - 1                                                  
          NTIME = NTIME + NTYL                                                  
      ELSE IF (NTIME .GE. NTY0) THEN                                            
          IDNEW(1) = ID(1) + 1                                                  
          NTIME = NTIME - NTY0                                                  
      ELSE                                                                      
          IDNEW(1) = ID(1)                                                      
      END IF                                                                    
C     -------------------------------------------------------------             
      IF (MOD(IDNEW(1),4) .EQ. 0) THEN ; MON(2) = 29                            
                                  ELSE ; MON(2) = 28                            
      END IF                                                                    
C     -------------------------------------------------------------             
      DO 150 M=1,12                                                             
             NTIME = NTIME - 24*MON(M)                                          
             IF(NTIME .LT. 0) GO TO 160                                         
 150  CONTINUE                                                                  
C     -------------------------------------------------------------             
 160  CONTINUE                                                                  
      IDNEW(2) = M                                                              
      NTIME    = NTIME + 24*MON(M)                                              
      IDNEW(3) = NTIME / 24 + 1                                                 
      IDNEW(4) = MOD(NTIME, 24)                                                 
      IHOUR    = ID(4) + KTLAG  +  (7 * 24*10000)                               
      IDNEW(5) = MOD (ID(5)+IHOUR/24 , 7)                                       
      IF (IDNEW(5) .EQ. 0) IDNEW(5) = 7                                         
      RETURN                                                                    
      END SUBROUTINE CVDATE 
C
! WRFVAR compiles at double precision by default, so DOUBLE PRECISION is 
! overkill
C      SUBROUTINE DATECK (ISTAT, IDATE, IBDATE, NDATE)                           
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
C                                                                               
C JUDGE WHETHER IDATE IS YOUNGER OR OLDER THAN IDATEB                           
C                                                                               
C (OUTPUT)                                                                      
C   ISTAT     : 'PAST', 'FUTR' AND 'SAME'                                       
C (INPUT)                                                                       
C   IDATE (5) : DATE.                                                           
C   IBDATE(5) : BASE DATE.                                                      
C   NDATE     : =1 ,COMPARE YEAR                                                
C               =2 ,        YEAR, MONTH                                         
C               =3 ,        YEAR, MONTH, DAY                                    
C               =4 ,        YEAR, MONTH, DAY, HOUR                              
C                                                                               
C      DIMENSION IDATE(5), IBDATE(5)                                             
C                                                                               
C      DO 1000 J=1,NDATE                                                         
C        IF(IDATE(J) .LT. IBDATE(J)) GO TO 1100                                  
C        IF(IDATE(J) .GT. IBDATE(J)) GO TO 1200                                  
C 1000 CONTINUE                                                                  
                                                                                
C        ISTAT='SAME'                                                            
C        RETURN                                                                  
                                                                                
C 1100 CONTINUE                                                                  
C        ISTAT='PAST'                                                            
C        RETURN                                                                  
                                                                                
C 1200 CONTINUE                                                                  
C        ISTAT='FUTR'                                                            
C        RETURN                                                                  
C                                                                               
C        END SUBROUTINE DATECK 

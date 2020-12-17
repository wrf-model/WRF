      SUBROUTINE ZMNT                                                           
     O ( ZDAT, MAXJZ, KMX,                                                      
     I   DATA, MAXIT, MAXJT )                                                   
C                                                                               
      DIMENSION DATA( MAXIT*MAXJT/MAXJZ, MAXJZ, KMX )                           
      DIMENSION ZDAT( MAXJZ, KMX )                                              
C                                                                               
      CALL RESET( ZDAT, MAXJZ*KMX )                                             
C                                                                               
C   ==================                                                          
C   >>>   ‘S•½‹Ï   <<<                                                      
C   ==================                                                          
      DO 10 K= 1, KMX                                                           
        NMAX = 0                                                                
        DO 20 J = 1, MAXJZ                                                      
          NMAX = 0                                                              
          DO 30 I = 1, MAXIT*MAXJT/MAXJZ                                        
            ZDAT( J, K ) = ZDAT( J, K ) + DATA( I, J, K )                       
            NMAX = NMAX + 1                                                     
   30     CONTINUE                                                              
          IF( NMAX.GT.0 ) THEN                                                  
            ZDAT( J, K ) = ZDAT( J, K ) / FLOAT( NMAX )                         
          ENDIF                                                                 
   20   CONTINUE                                                                
   10 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE ZMNT                                                           

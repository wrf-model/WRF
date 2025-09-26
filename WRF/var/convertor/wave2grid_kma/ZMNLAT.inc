      SUBROUTINE ZMNLAT                                                         
     O( RLAT,                                                                   
     I  MAXJZ, COL, JMX )                                                       
      DIMENSION RLAT( MAXJZ ), COL( JMX )                                       
C                                                                               
      RADEG  =180.0D0/pi
      CALL RESET( RLAT, MAXJZ )                                                 
      DO 10 J = 1, MAXJZ                                                        
        DO 20 I = 1, JMX/MAXJZ                                                  
          RLAT( J ) = RLAT( J )                                                 
     1              + 90.0 - RADEG*COL( (J-1)*JMX/MAXJZ + I )                   
   20   CONTINUE                                                                
   10 CONTINUE                                                                  
      DO 30 J = 1, MAXJZ                                                        
        RLAT( J ) = RLAT( J ) / (JMX/MAXJZ)                                     
   30 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE ZMNLAT                                                         

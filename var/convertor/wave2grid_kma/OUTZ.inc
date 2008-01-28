      SUBROUTINE OUTZ                                                           
     1  (ASTAT, JMX, KMX , AKWD, ATTL, AUNT, IPWR, RLAT, AREA)                  
C----------------------------------------------------------------------         
C ASTAT(JMX,KMX) ZONAL MEAN VALUE                                               
C AKWD           要素についての KEYWORD                                     
C ATTL           要素についてのコメント                                     
C IPWR           べき乗                                                     
C RLAT(JMX)    GAUSSIAN LATITUDE                                                
C AREA       'HEAD' HEADER だけ出力                                         
C            'LAT ' LAT だけ出力                                            
C            'KMAX' 鉛直レベル毎の値を出力                                  
C            'LAND' 陸の値を出力                                            
C            'SEA ' 海の値を出力                                            
C            'BOTH' 海陸別・全体の値を出力                                  
C            'ALL ' 全体の値を出力                                          
C----------------------------------------------------------------------         
C                                                                               
      DIMENSION ASTAT(JMX,KMX)                                                  
      DIMENSION RLAT(JMX)                                                       
      CHARACTER*4  AKWD                                                         
      CHARACTER*32 ATTL                                                         
      CHARACTER*16 AUNT                                                         
      CHARACTER*4 AREA                                                          
C                                                                               
      CHARACTER*3 LEVL(3)                                                       
      DATA LEVL/'LND','SEA','TOT'/                                              
C                                                                               
C   ------------------------------------------------------                      
C   >>>   AREA='LAT ' なら RLAT だけを出力しておわり   <<<              
C   ------------------------------------------------------                      
      IF(AREA.EQ.'LAT ') THEN                                                   
        WRITE(6,801) ( RLAT(J), J = 1, JMX )                                    
        RETURN                                                                  
      END IF                                                                    
  801 FORMAT(1H ,'LAT ',16F8.1)                                                 
C                                                                               
C   ------------------                                                          
C   >>>   HEADER   <<<                                                          
C   ------------------                                                          
      IF(AREA.EQ.'HEAD') THEN                                                   
        WRITE(6,802) AKWD, ATTL, AUNT, IPWR                                     
        RETURN                                                                  
      END IF                                                                    
  802 FORMAT(1X,A4,4X,A32,'(',A16,') **',I2)                                    
C                                                                               
C   ==================================================================          
C   >>>   出力   <<<                                                        
C   ==================================================================          
C                                                                               
C   -------------------------------------                                       
C   >>>   AREA='KMAX' なら LAT を出力する   <<<                         
C   -------------------------------------                                       
      IF(AREA.EQ.'KMAX') THEN                                                   
        WRITE(6,802) AKWD, ATTL, AUNT, IPWR                                     
        WRITE(6,801) ( RLAT(J), J = 1, JMX )                                    
      END IF                                                                    
C                                                                               
C   -----------------------------------------                                   
C   >>>   海陸別値を出力する   <<<                                          
C   -----------------------------------------                                   
      IF( AREA.EQ.'LAND'.OR.AREA.EQ.'BOTH' ) THEN                               
        WRITE(6,803) LEVL(1), ( ASTAT( J, 1 )*10**IPWR, J = 1, JMX )            
      ENDIF                                                                     
      IF( AREA.EQ.'SEA '.OR.AREA.EQ.'BOTH' ) THEN                               
        WRITE(6,803) LEVL(2), ( ASTAT( J, 2 )*10**IPWR, J = 1, JMX )            
      ENDIF                                                                     
      IF( AREA.EQ.'ALL '.OR.AREA.EQ.'BOTH' ) THEN                               
        WRITE(6,803) LEVL(3), ( ASTAT( J, 3 )*10**IPWR, J = 1, JMX )            
      ENDIF                                                                     
  803 FORMAT(1H ,A3,1X,16F8.1)                                                  
      IF( AREA.EQ.'KMAX' ) THEN                                                 
C   ------------------------------------------------------                      
C   >>>   AREA='KMAX' なら鉛直レベル毎の値を出力する   <<<                  
C   ------------------------------------------------------                      
        DO 120 K=1,KMX                                                          
          WRITE(6,804) K, ( ASTAT( J, K )*10**IPWR, J = 1, JMX )                
  120   CONTINUE                                                                
  804   FORMAT(1H ,I2,2X,16F8.1)                                                
      ENDIF                                                                     
C                                                                               
C   ===================================================================         
C                                                                               
      RETURN                                                                    
      END SUBROUTINE OUTZ                                                           

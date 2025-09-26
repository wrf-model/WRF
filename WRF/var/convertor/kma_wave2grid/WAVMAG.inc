      SUBROUTINE WAVMAG (WDATA,MNWAV,KMAXX,LTTL)                                 
crizvi	  USE GSM_PARAMS, ONLY:KMAX
crizvi  DIMENSION   WDATA(KMAXX*2,MNWAV),ABS(KMAX)                                   
      DIMENSION   WDATA(KMAXX*2,MNWAV),ABS(KMAXX)                                   
      CHARACTER*(*) LTTL
C                                                                               
      DO 100 K=1,KMAXX                                                           
       ABS(K)=0.0                                                               
       DO 120 L=1,MNWAV                                                         
        ABS(K)=ABS(K)+WDATA(2*K-1,L)*WDATA(2*K-1,L)                             
     1               +WDATA(2*K  ,L)*WDATA(2*K  ,L)                             
  120  CONTINUE                                                                 
       ABS(K)=SQRT(ABS(K))                                                      
  100 CONTINUE                                                                  
C                                                                               
      WRITE(96,888) LTTL                                                         
  888 FORMAT(1H ,A4,'===ABSOLUTE AND MEAN VALUE ON EACH LEVEL===')              
      WRITE(96,999) (ABS(K),K=1,KMAXX)                                            
      WRITE(96,999) (WDATA(2*K-1,1),K=1,KMAXX)                                    
  999 FORMAT(1H ,4X,10E12.5)                                                    
C                                                                               
      RETURN                                                                    
      END SUBROUTINE WAVMAG


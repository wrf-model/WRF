      SUBROUTINE WEIHT2                                                         
     I  (IMAX,MEND1,JBLK1,JBLK2,KMAX,KGX,KQX,KQWX,GW,GD,                        
     O   WD)                                                                    
C                                                                               
      DIMENSION   GD(IMAX,JBLK2,*),WD(KQWX,MEND1,JBLK2),GW(JBLK2)               
C                                                                               
      DO 100 K=1,KMAX                                                           
      DO 100 J=1,JBLK1                                                          
C      JS=J                                                                     
C      JA=J+JBLK1                                                               
*vdir nodep                                                                  
      DO 100 M=1,MEND1                                                          
       WD(KQX+2*K-1,M,J      )=                                                 
     &             GW(J)*(GD(2*M-1,J,KGX+K)+GD(2*M-1,JBLK2+1-J,KGX+K))          
       WD(KQX+2*K  ,M,J      )=                                                 
     &             GW(J)*(GD(2*M  ,J,KGX+K)+GD(2*M  ,JBLK2+1-J,KGX+K))          
       WD(KQX+2*K-1,M,J+JBLK1)=                                                 
     &             GW(J)*(GD(2*M-1,J,KGX+K)-GD(2*M-1,JBLK2+1-J,KGX+K))          
       WD(KQX+2*K  ,M,J+JBLK1)=                                                 
     &             GW(J)*(GD(2*M  ,J,KGX+K)-GD(2*M  ,JBLK2+1-J,KGX+K))          
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE WEIHT2


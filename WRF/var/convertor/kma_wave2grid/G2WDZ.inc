      SUBROUTINE G2WDZ                                                          
     I(MEND1,NEND1 ,JEND1,MNWAV,IMAX,JMAX  ,IMX ,JMAXHF,KMAX ,                  
     I PNMGC,DPNMGC,GU   ,GV   ,ER  ,SINCLT,IFAX,TRIGS,                         
     O QROT ,QDIV ,                                                             
     W GWRK)                                                                    
C                                                                               
      DIMENSION QROT  (2,KMAX,MNWAV),QDIV  (2,KMAX,MNWAV)                       
      DIMENSION DPNMGC(MNWAV,JMAXHF),PNMGC (MNWAV,JMAXHF),SINCLT(JMAX)          
      DIMENSION GU  (IMAX,JMAX,KMAX),GV    (IMAX,JMAX,KMAX)                     
      DIMENSION GWRK(IMX,JMAX,KMAX),IFAX(10),TRIGS(IMAX)                        
C                                                                               
      ERIV=1.0/ER                                                               
      LOT =JMAX*KMAX                                                            
      CALL FFT991(GU,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
      CALL FFT991(GV,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
C     CALL FFT991(GU  ,GWRK,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
C     CALL FFT991(GV  ,GWRK,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
C                                                                               
      CALL RESET(QROT,2*KMAX*MNWAV)                                             
      CALL RESET(QDIV,2*KMAX*MNWAV)                                             
C                                                                               
      DO 100 K=1,KMAX                                                           
C                                                                               
      DO 120 J=1,JMAXHF                                                         
      JM=J                                                                      
      JP=JMAX+1-J                                                               
      ASINCL=ERIV*SINCLT(J)                                                     
      IF(MOD(JMAX,2).EQ.1.AND.J.EQ.JMAXHF) THEN                                 
      DO 140 M=1,MEND1*2                                                        
      GWRK(M,JM,1)=ASINCL*(GU(M,JM,K)+GU(M,JP,K))                               
      GWRK(M,JM,2)=ASINCL*(GV(M,JM,K)+GV(M,JP,K))                               
  140 CONTINUE                                                                  
      ELSE                                                                      
      DO 160 M=1,MEND1*2                                                        
      GWRK(M,JM,1)=ASINCL*(GU(M,JM,K)+GU(M,JP,K))                               
      GWRK(M,JP,1)=ASINCL*(GU(M,JM,K)-GU(M,JP,K))                               
      GWRK(M,JM,2)=ASINCL*(GV(M,JM,K)+GV(M,JP,K))                               
      GWRK(M,JP,2)=ASINCL*(GV(M,JM,K)-GV(M,JP,K))                               
  160 CONTINUE                                                                  
      END IF                                                                    
  120 CONTINUE                                                                  
C                                                                               
      L =0                                                                      
      DO 200 M=1,MEND1                                                          
      QM  =FLOAT(M-1)                                                           
      NMAX=MIN(JEND1+1-M,NEND1)                                                 
      DO 220 N=1,NMAX                                                           
      IF(MOD(N-1,2).EQ.0) THEN                                                  
      DO 240 J=1,JMAXHF                                                         
      JM=       J                                                               
      JP=JMAX+1-J                                                               
      QROT(1,K,L+N)=QROT(1,K,L+N)-QM*GWRK(2*M  ,JM,2)* PNMGC(L+N,J)             
     1                           -   GWRK(2*M-1,JP,1)*DPNMGC(L+N,J)             
      QROT(2,K,L+N)=QROT(2,K,L+N)+QM*GWRK(2*M-1,JM,2)* PNMGC(L+N,J)             
     1                           -   GWRK(2*M  ,JP,1)*DPNMGC(L+N,J)             
      QDIV(1,K,L+N)=QDIV(1,K,L+N)-QM*GWRK(2*M  ,JM,1)* PNMGC(L+N,J)             
     1                           +   GWRK(2*M-1,JP,2)*DPNMGC(L+N,J)             
      QDIV(2,K,L+N)=QDIV(2,K,L+N)+QM*GWRK(2*M-1,JM,1)* PNMGC(L+N,J)             
     1                           +   GWRK(2*M  ,JP,2)*DPNMGC(L+N,J)             
  240 CONTINUE                                                                  
      ELSE                                                                      
      DO 260 J=1,JMAXHF                                                         
      JM=       J                                                               
      JP=JMAX+1-J                                                               
      QROT(1,K,L+N)=QROT(1,K,L+N)-QM*GWRK(2*M  ,JP,2)* PNMGC(L+N,J)             
     1                           -   GWRK(2*M-1,JM,1)*DPNMGC(L+N,J)             
      QROT(2,K,L+N)=QROT(2,K,L+N)+QM*GWRK(2*M-1,JP,2)* PNMGC(L+N,J)             
     1                           -   GWRK(2*M  ,JM,1)*DPNMGC(L+N,J)             
      QDIV(1,K,L+N)=QDIV(1,K,L+N)-QM*GWRK(2*M  ,JP,1)* PNMGC(L+N,J)             
     1                           +   GWRK(2*M-1,JM,2)*DPNMGC(L+N,J)             
      QDIV(2,K,L+N)=QDIV(2,K,L+N)+QM*GWRK(2*M-1,JP,1)* PNMGC(L+N,J)             
     1                           +   GWRK(2*M  ,JM,2)*DPNMGC(L+N,J)             
  260 CONTINUE                                                                  
      END IF                                                                    
  220 CONTINUE                                                                  
      L=L+NMAX                                                                  
  200 CONTINUE                                                                  
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE G2WDZ


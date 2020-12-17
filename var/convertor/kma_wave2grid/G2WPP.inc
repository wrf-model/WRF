      SUBROUTINE G2WPP                                                          
     1(MEND1 ,NEND1 ,JEND1 ,KQS,KQE  ,KQMAX ,MNWAV   ,JBLK1,JBLK2 ,             
     2 PNM,WDATA,DATA )                                                         
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z,\)                                     
C                                                                               
      DIMENSION PNM(MNWAV,JBLK1)                                                
      DIMENSION DATA(KQMAX,MEND1,JBLK2),WDATA(KQMAX,MNWAV)                      
C                                                                               
      NJ=JBLK1/8                                                                
      JREM=MOD(JBLK1,8)                                                         
      L=0                                                                       
      DO 300 M=1,MEND1                                                          
      NMAX=MIN(NEND1,JEND1+1-M)                                                 
      J0=0                                                                      
      DO 310 IJ=1,NJ                                                            
      DO 320 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 330 K=KQS,KQE                                                          
C     WRITE (6,*) 'G2WPP:K,KQS,KQE',K,KQS,KQE                                   
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
     1  +PNM(L+N-1,J0+4)*DATA(K,M,J0+ 4)                                        
     1  +PNM(L+N-1,J0+5)*DATA(K,M,J0+ 5)                                        
     1  +PNM(L+N-1,J0+6)*DATA(K,M,J0+ 6)                                        
     1  +PNM(L+N-1,J0+7)*DATA(K,M,J0+ 7)                                        
     1  +PNM(L+N-1,J0+8)*DATA(K,M,J0+ 8)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
     1  +PNM(L+N,J0+4)*DATA(K,M,JBLK1+J0+ 4)                                    
     1  +PNM(L+N,J0+5)*DATA(K,M,JBLK1+J0+ 5)                                    
     1  +PNM(L+N,J0+6)*DATA(K,M,JBLK1+J0+ 6)                                    
     1  +PNM(L+N,J0+7)*DATA(K,M,JBLK1+J0+ 7)                                    
     1  +PNM(L+N,J0+8)*DATA(K,M,JBLK1+J0+ 8)                                    
  330 CONTINUE                                                                  
  320 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 340 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
     1  +PNM(L+NMAX,J0+4)*DATA(K,M,J0+ 4)                                       
     1  +PNM(L+NMAX,J0+5)*DATA(K,M,J0+ 5)                                       
     1  +PNM(L+NMAX,J0+6)*DATA(K,M,J0+ 6)                                       
     1  +PNM(L+NMAX,J0+7)*DATA(K,M,J0+ 7)                                       
     1  +PNM(L+NMAX,J0+8)*DATA(K,M,J0+ 8)                                       
  340 CONTINUE                                                                  
      END IF                                                                    
      J0=J0+8                                                                   
  310 CONTINUE                                                                  
      IF (JREM.EQ.7) THEN                                                       
      DO 350 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 360 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
     1  +PNM(L+N-1,J0+4)*DATA(K,M,J0+ 4)                                        
     1  +PNM(L+N-1,J0+5)*DATA(K,M,J0+ 5)                                        
     1  +PNM(L+N-1,J0+6)*DATA(K,M,J0+ 6)                                        
     1  +PNM(L+N-1,J0+7)*DATA(K,M,J0+ 7)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
     1  +PNM(L+N,J0+4)*DATA(K,M,JBLK1+J0+ 4)                                    
     1  +PNM(L+N,J0+5)*DATA(K,M,JBLK1+J0+ 5)                                    
     1  +PNM(L+N,J0+6)*DATA(K,M,JBLK1+J0+ 6)                                    
     1  +PNM(L+N,J0+7)*DATA(K,M,JBLK1+J0+ 7)                                    
  360 CONTINUE                                                                  
  350 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 370 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
     1  +PNM(L+NMAX,J0+4)*DATA(K,M,J0+ 4)                                       
     1  +PNM(L+NMAX,J0+5)*DATA(K,M,J0+ 5)                                       
     1  +PNM(L+NMAX,J0+6)*DATA(K,M,J0+ 6)                                       
     1  +PNM(L+NMAX,J0+7)*DATA(K,M,J0+ 7)                                       
  370 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.6) THEN                                                  
      DO 380 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 390 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
     1  +PNM(L+N-1,J0+4)*DATA(K,M,J0+ 4)                                        
     1  +PNM(L+N-1,J0+5)*DATA(K,M,J0+ 5)                                        
     1  +PNM(L+N-1,J0+6)*DATA(K,M,J0+ 6)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
     1  +PNM(L+N,J0+4)*DATA(K,M,JBLK1+J0+ 4)                                    
     1  +PNM(L+N,J0+5)*DATA(K,M,JBLK1+J0+ 5)                                    
     1  +PNM(L+N,J0+6)*DATA(K,M,JBLK1+J0+ 6)                                    
  390 CONTINUE                                                                  
  380 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 400 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
     1  +PNM(L+NMAX,J0+4)*DATA(K,M,J0+ 4)                                       
     1  +PNM(L+NMAX,J0+5)*DATA(K,M,J0+ 5)                                       
     1  +PNM(L+NMAX,J0+6)*DATA(K,M,J0+ 6)                                       
  400 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.5) THEN                                                  
      DO 410 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 420 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
     1  +PNM(L+N-1,J0+4)*DATA(K,M,J0+ 4)                                        
     1  +PNM(L+N-1,J0+5)*DATA(K,M,J0+ 5)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
     1  +PNM(L+N,J0+4)*DATA(K,M,JBLK1+J0+ 4)                                    
     1  +PNM(L+N,J0+5)*DATA(K,M,JBLK1+J0+ 5)                                    
  420 CONTINUE                                                                  
  410 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 430 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
     1  +PNM(L+NMAX,J0+4)*DATA(K,M,J0+ 4)                                       
     1  +PNM(L+NMAX,J0+5)*DATA(K,M,J0+ 5)                                       
  430 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.4) THEN                                                  
      DO 440 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 450 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
     1  +PNM(L+N-1,J0+4)*DATA(K,M,J0+ 4)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
     1  +PNM(L+N,J0+4)*DATA(K,M,JBLK1+J0+ 4)                                    
  450 CONTINUE                                                                  
  440 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 460 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
     1  +PNM(L+NMAX,J0+4)*DATA(K,M,J0+ 4)                                       
  460 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.3) THEN                                                  
      DO 470 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 480 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
     1  +PNM(L+N-1,J0+3)*DATA(K,M,J0+ 3)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
     1  +PNM(L+N,J0+3)*DATA(K,M,JBLK1+J0+ 3)                                    
  480 CONTINUE                                                                  
  470 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 490 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
     1  +PNM(L+NMAX,J0+3)*DATA(K,M,J0+ 3)                                       
  490 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.2) THEN                                                  
      DO 500 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 510 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
     1  +PNM(L+N-1,J0+2)*DATA(K,M,J0+ 2)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
     1  +PNM(L+N,J0+2)*DATA(K,M,JBLK1+J0+ 2)                                    
  510 CONTINUE                                                                  
  500 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 520 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
     1  +PNM(L+NMAX,J0+2)*DATA(K,M,J0+ 2)                                       
  520 CONTINUE                                                                  
      END IF                                                                    
      ELSE IF (JREM.EQ.1) THEN                                                  
      DO 530 N=2,NMAX,2                                                         
*vdir nodep                                                                  
      DO 540 K=KQS,KQE                                                          
      WDATA(K,L+N-1)=WDATA(K,L+N-1)                                             
     1  +PNM(L+N-1,J0+1)*DATA(K,M,J0+ 1)                                        
      WDATA(K,L+N  )=WDATA(K,L+N  )                                             
     1  +PNM(L+N,J0+1)*DATA(K,M,JBLK1+J0+ 1)                                    
  540 CONTINUE                                                                  
  530 CONTINUE                                                                  
      IF(MOD(NMAX,2).NE.0) THEN                                                 
      DO 550 K=KQS,KQE                                                          
      WDATA(K,L+NMAX  )=WDATA(K,L+NMAX  )                                       
     1  +PNM(L+NMAX,J0+1)*DATA(K,M,J0+ 1)                                       
  550 CONTINUE                                                                  
      END IF                                                                    
      END IF                                                                    
      L=L+NMAX                                                                  
  300 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE G2WPP


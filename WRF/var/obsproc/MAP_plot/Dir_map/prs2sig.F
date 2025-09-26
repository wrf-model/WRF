C                                                                             
       SUBROUTINE PRS2SIG(PP,SIG,FSG,KX,PRES,FP,KXP,PS,PTOP,IWT,NAME)         
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
C                                                                      C      
C     ...THIS PROGRAM WILL GET THE VALUES ON SIGMA LEVELS (SIG)        C
C        FROM THE VALUES (FP) ON THE PRESSURE LEVELS (SIG). THE RESULT C      
C        WILL BE PUT BACK INTO  FSG.                                   C       
C                                                                      C       
C        SIG is the half sigma values (up-to-bottom)                   C
C        PRES is the values from large to small in hPa                 C
C                                                                      C       
C     ...PP is coupled P-perturbation (cb*Pa),                         C
C        PS is Pstar; PTOP is Ptop in cb                               C
C                                                                      C       
C     ... IWT = 0  LINEAR INTERPOLATION                                C       
C         IWT = 1  LOG LINEAR INTERPOLATION                            C
C                                                                      C
C     ... ICRS = 1 cross points field, = 0 dot point field             C
C                                                                      C       
C                                                                      C
C  -- Input:  PP, FP                                                   C
C     Constants: SIG, PRES, PS, PTOP                                   C
C                                                                      C
C  -- Output: FSG                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
C                                                                              
      DIMENSION PP(KX)   ,  SIG(KX+1), FSG(KX),
     +          PRES(KXP),    FP(KXP),  
     +          ALP(80),ALS(80),D1(80),D2(80),SS(80)
      CHARACTER*6 NAME
C                                                                              
C   inverse the sigma values and stored in to SS:          
C              
C     SIG(1) = 0.0, SID(2) = 0.10----> SIG(KX) = 0.96, SIG(KX+1) = 1.0
C          SS (KX)) = .05,       ---->        SS (1) = 0.98 
C
      DO 10 K = 1,KX                                                        
        SS(K) = SIG(KX-K+1)                             
10    CONTINUE                                                               
C
cc      PRINT 11,NAME,IWT
11    FORMAT(/'CALL SIG2PRS ==> ',A6,'  IWT=',I2)
      DO K = 1,KX
         IF (K.LE.KXP) THEN
c           PRINT 12,K,SIG(K),SS(K),PRES(K)
         ELSE IF (K.LE.KX) THEN
c           PRINT 12,K,SIG(K),SS(K)
         ELSE
c           PRINT 12,K,SIG(K)
         ENDIF
      END DO
12    FORMAT(5X,I3,2X,3F10.4)
C       
      IF (IWT.EQ.0) THEN     
C                                                                              
C     ... LINEAR INTERPOLATION                                                 
C                            | FIELDS RELATED TO WIND DO LINEAR
         DO 20 K=1,KX       ! INTERPOLATION                                 
            ALS(K)=10.*(SS(K)*PS+PTOP) + PP(K)/PS/100.
20       CONTINUE         
      ELSE IF (IWT.EQ.1) THEN                                                
C                                                                              
C        ... LOG INTERPOLATION                                                 
C                            | FIELDS RELATED TO MASS FIELD DO LOG
         DO 21 K=1,KX       ! IS LINEARLY INTEPOLATION
            ALS(K)=ALOG(10.*(SS(K)*PS+PTOP) + PP(K)/PS/100.)                
21       CONTINUE                                                              
      END IF                                                                   
C
C  .. to get FSG at pressure levels:
C
            IF (IWT.EQ.0) THEN     
C                                                                              
C     ... LINEAR INTERPOLATION                                                 
C                           
              DO 30 K=1,KXP                                                
                ALP(K)=PRES(K)
                D1(K)=FP(K)                                        
30            CONTINUE                                                         
            ELSE IF (IWT.EQ.1) THEN                                           
C                                                                              
C        ... LOG INTERPOLATION                                                 
C                           
              DO 31 K=1,KXP
                ALP(K)=ALOG(PRES(K))                                          
                D1(K)=FP(K)                                          
31            CONTINUE                                                         
            END IF                                                            
C
C                                                                             
            DO 40 K=1,KX                                                    
               DO 50 LL=1,KXP                                                
                  IF(ALP(LL).LE.ALS(K)) GO TO 100                            
50             CONTINUE                                                       
100             CONTINUE                                                      
               L=LL                                                           
C                                                                             
               IF (L.EQ.1) THEN                                               
C                                                                             
C                 ... DO THE REQUESTED EXTRAPOLATION AT BOTTOM                
                                                                               
CC                  D2(K)=D1(L)+(ALP(L)-ALS(K))*                          
CC     +                 (D1(L+1)-D1(L))/(ALP(L)-ALP(L+1))
C  .. REGARDS AS MISSING:
                  D2(K) = -999.
C                                                                             
               ELSE IF(ALS(K).GE.ALP(KXP))THEN                               
C                                                                             
C                 ... DO THE INTERPOLATION BETWEEN SIGMA LEVELS               
C                                                                             
                  AD=ALP(L-1)-ALS(K)                                          
                  AU=(ALS(K)-ALP(L))                                          
                  D2(K)=(D1(L-1)*AU+D1(L)*AD)/(ALP(L-1)-ALP(L))               
C                                                                              
               ELSE                                                           
C                                                                             
C                 ... DO THE REQUESTED EXTRAPOLATION T TOP                  
C                                                                             
cc                  IF (IWT.EQ.0) THEN                                         
cc                     D2(K)=D1(L)+(ALP(L)-ALS(K))*                           
cc     +                     (D1(L)-D1(L-1))/(ALP(L-1)-ALP(L))                
cc                  ELSE                                                      
C           above the old highest pressure level, T, Qv, .... will take       
C           the values on the old highest pressure level.                     
cc                     D2(K) = D1(KX)                                         
cc                  ENDIF                                                     
C  .. REGARDS AS MISSING:
                  D2(K) = -999.
               END IF                                                         
C                                                                              
               FSG(K) = D2(K)                                          
40          CONTINUE                                                          
C
cc        PRINT 310,NAME,PS
310     FORMAT(/5X,'NAME=',A6,'  PS=',F10.2)
        DO K = 1,KX
          PSIG =  10.*(SS(K)*PS+PTOP) + PP(K)/PS/100.
          IF (K.LE.KXP) THEN
cc            PRINT 311,K,PSIG,FSG(K),PRES(K),FP(K)
311         FORMAT('K=',I2,' PSIG=',F8.2,' FSG=',F8.2,' PRES=',F8.2,
     >              ' FP=',F8.2)
          ELSE
cc            PRINT 311,K,PSIG,FSG(K)
          ENDIF
        END DO
C
      RETURN                                                                  
      END                                                                     








      SUBROUTINE GPLHGT                                                 
     I  (GPS ,GTMP,GWV ,GPHIS,IMAX,JMAX,KMAX,RGAS,G,A,B,                
     I   JSTA,JFIN,
     O   GHGT)                                                          
C***********************************************************************
C                CALCULATION OF GEOPOTENTIAL HEIGHT                     
C***********************************************************************
! WRFVAR compiles at double precision by default, so DOUBLE PRECISION is 
! overkill
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                             
C                                                                       
      REAL*8    RGAS,G                                                  
      REAL*8    GPS (IMAX,JMAX     ), GTMP(IMAX,JMAX,KMAX),             
     &          GWV (IMAX,JMAX,KMAX), GPHIS(IMAX,JMAX     )             
      REAL*8    GHGT(IMAX,JMAX,KMAX)                                    
C                                                                       
      PARAMETER (KM=50)                                                 
C                                                                       
C     DIMENSION PHALF(KM), DELP(KM), ALPHA(KM), TV(KM)                  
      DIMENSION PHALF(KM),           ALPHA(KM), TV(KM)                  
      DIMENSION PHALFL(KM)                                              
Crizvi      REAL*8    A(50), B(50)                                            
      REAL*8    A(KMAX+1), B(KMAX+1)                                            
C                                                                       
C     DATA COEF /0.608D0/                                               
C********************* PROCEDURE *************************************  
      COEF=0.608D0                                               
      IF (KMAX.GT.KM) THEN                                              
        WRITE(6,*) ' ERROR: <KMAX> IS TOO LARGE. in GPLHGT'                             
        STOP 100
      END IF                                                            
      RGASG = RGAS/G                                                    
      ALPHA(KMAX) = LOG(2.D0)                                           
CPOPTION PARALLEL,DIVNUM(12),PRIND((J,1))                               
C2000.08.25
CLSW*POPTION PARALLEL                               
CLSW*POPTION TLOCAL(J,I,K,DELP,SHGT,HYDRO,                                  
CLSW*POPTION PHALF,TV,ALPHA,PHALFL)                                         
CLSW*POPTION INIT(ALPHA(KMAX))
      DO 1000 J = JSTA,JFIN                                               
C     DO 1000 J = 1, JMAX                                               
      DO 1000 I = 1, IMAX                                               
        DO 100 K = 1, KMAX                                              
          PHALF(K) = A(K) + B(K)*GPS (I,J)                              
C         TV   (K) = (1.D0+COEF*GWV(I,J,K))*GTMP(I,J,K)                 
          TV   (K) = (1.D0+COEF*GWV(I,J,K))*GTMP(I,J,K)*RGASG           
C         WRITE(6,*) ' K,PHALF,TV=',K,PHALF(K),TV(K)                    
  100   CONTINUE                                                        
        DO 200 K = 1, KMAX-1                                            
C         DELP (K) = PHALF(K) - PHALF(K+1)                              
          DELP     = PHALF(K) - PHALF(K+1)                              
C         ALPHA(K) = 1.D0-PHALF(K+1)*LOG(PHALF(K)/PHALF(K+1))/DELP(K)   
          PHALFL(K)= LOG(PHALF(K)/PHALF(K+1))                           
C         ALPHA(K) = 1.D0-PHALF(K+1)*PHALFL(K)/DELP(K)                  
          ALPHA(K) = 1.D0-PHALF(K+1)*PHALFL(K)/DELP                     
  200   CONTINUE                                                        
C       ALPHA(KMAX) = LOG(2.D0)                                         
        SHGT = GPHIS(I,J)/G !SHCO
C       SHGT = GPHIS(I,J)   !SHCN 
C         WRITE(6,*) ' SHGT=',SHGT                                      
        DO 300 K = 1, KMAX                                              
C         GHGT(I,J,K) = SHGT + ALPHA(K)*RGASG*TV(K)                     
          GHGT(I,J,K) = SHGT + ALPHA(K)*TV(K)                           
C         WRITE(6,*) ' K,SHGT+LEVEL K-1/2 TO K=',K,GHGT(I,J,K)          
  300   CONTINUE                                                        
        HYDRO = 0.D0                                                    
C         WRITE(6,*) ' K,GHGT=',1,GHGT(I,J,1)                           
        DO 400 K = 2, KMAX                                              
C         HYDRO = HYDRO + RGASG*TV(K-1)*LOG(PHALF(K-1)/PHALF(K))        
          HYDRO = HYDRO + TV(K-1)*PHALFL(K-1)                           
          GHGT(I,J,K) = GHGT(I,J,K) + HYDRO                             
C         WRITE(6,*) ' K,GHGT=',K,GHGT(I,J,K)                           
  400   CONTINUE                                                        
C                                                                       
 1000 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END SUBROUTINE GPLHGT                                                 

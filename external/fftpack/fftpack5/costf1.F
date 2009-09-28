!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: costf1.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSTF1(N,INC,X,WSAVE,WORK,IER) 
      REAL       X(INC,*)       ,WSAVE(*) 
      DOUBLE PRECISION           DSUM 
      IER = 0 
      NM1 = N-1 
      NP1 = N+1 
      NS2 = N/2 
      IF (N-2) 200,101,102 
  101 X1H = X(1,1)+X(1,2) 
      X(1,2) = .5*(X(1,1)-X(1,2)) 
      X(1,1) = .5*X1H 
      GO TO 200 
  102 IF (N .GT. 3) GO TO 103 
      X1P3 = X(1,1)+X(1,3) 
      TX2 = X(1,2)+X(1,2) 
      X(1,2) = .5*(X(1,1)-X(1,3)) 
      X(1,1) = .25*(X1P3+TX2) 
      X(1,3) = .25*(X1P3-TX2) 
      GO TO 200 
  103 DSUM = X(1,1)-X(1,N) 
      X(1,1) = X(1,1)+X(1,N) 
      DO 104 K=2,NS2 
         KC = NP1-K 
         T1 = X(1,K)+X(1,KC) 
         T2 = X(1,K)-X(1,KC) 
         DSUM = DSUM+WSAVE(KC)*T2 
         T2 = WSAVE(K)*T2 
         X(1,K) = T1-T2 
         X(1,KC) = T1+T2 
  104 END DO 
      MODN = MOD(N,2) 
      IF (MODN .EQ. 0) GO TO 124 
      X(1,NS2+1) = X(1,NS2+1)+X(1,NS2+1) 
  124 LENX = INC*(NM1-1)  + 1 
      LNSV = NM1 + INT(LOG(REAL(NM1))) + 4 
      LNWK = NM1 
!                                                                       
      CALL RFFT1F(NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,                  &
     &            LNWK,IER1)                                            
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSTF1',-5) 
        GO TO 200 
      ENDIF 
!                                                                       
      SNM1 = 1./FLOAT(NM1) 
      DSUM = SNM1*DSUM 
      IF(MOD(NM1,2) .NE. 0) GO TO 30 
      X(1,NM1) = X(1,NM1)+X(1,NM1) 
   30 DO 105 I=3,N,2 
         XI = .5*X(1,I) 
         X(1,I) = .5*X(1,I-1) 
         X(1,I-1) = DSUM 
         DSUM = DSUM+XI 
  105 END DO 
      IF (MODN .NE. 0) GO TO 117 
      X(1,N) = DSUM 
  117 X(1,1) = .5*X(1,1) 
      X(1,N) = .5*X(1,N) 
  200 RETURN 
      END                                           

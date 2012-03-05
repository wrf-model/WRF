!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: costb1.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSTB1(N,INC,X,WSAVE,WORK,IER) 
      REAL       X(INC,*)       ,WSAVE(*) 
      DOUBLE PRECISION           DSUM 
      IER = 0 
      NM1 = N-1 
      NP1 = N+1 
      NS2 = N/2 
      IF (N-2) 106,101,102 
  101 X1H = X(1,1)+X(1,2) 
      X(1,2) = X(1,1)-X(1,2) 
      X(1,1) = X1H 
      RETURN 
  102 IF (N .GT. 3) GO TO 103 
      X1P3 = X(1,1)+X(1,3) 
      X2 = X(1,2) 
      X(1,2) = X(1,1)-X(1,3) 
      X(1,1) = X1P3+X2 
      X(1,3) = X1P3-X2 
      RETURN 
  103 X(1,1) = X(1,1)+X(1,1) 
      X(1,N) = X(1,N)+X(1,N) 
      DSUM = X(1,1)-X(1,N) 
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
        CALL XERFFT ('COSTB1',-5) 
        RETURN 
      ENDIF 
!                                                                       
      FNM1S2 = FLOAT(NM1)/2. 
      DSUM = .5*DSUM 
      X(1,1) = FNM1S2*X(1,1) 
      IF(MOD(NM1,2) .NE. 0) GO TO 30 
      X(1,NM1) = X(1,NM1)+X(1,NM1) 
   30 FNM1S4 = FLOAT(NM1)/4. 
      DO 105 I=3,N,2 
         XI = FNM1S4*X(1,I) 
         X(1,I) = FNM1S4*X(1,I-1) 
         X(1,I-1) = DSUM 
         DSUM = DSUM+XI 
  105 END DO 
      IF (MODN .NE. 0) RETURN 
         X(1,N) = DSUM 
  106 RETURN 
      END                                           

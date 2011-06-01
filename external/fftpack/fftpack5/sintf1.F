!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: sintf1.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE SINTF1(N,INC,X,WSAVE,XH,WORK,IER) 
      REAL       X(INC,*)       ,WSAVE(*)   ,XH(*) 
      DOUBLE PRECISION           DSUM 
      IER = 0 
      IF (N-2) 200,102,103 
  102 SSQRT3 = 1./SQRT(3.) 
      XHOLD = SSQRT3*(X(1,1)+X(1,2)) 
      X(1,2) = SSQRT3*(X(1,1)-X(1,2)) 
      X(1,1) = XHOLD 
      GO TO 200 
  103 NP1 = N+1 
      NS2 = N/2 
      DO 104 K=1,NS2 
         KC = NP1-K 
         T1 = X(1,K)-X(1,KC) 
         T2 = WSAVE(K)*(X(1,K)+X(1,KC)) 
         XH(K+1) = T1+T2 
         XH(KC+1) = T2-T1 
  104 END DO 
      MODN = MOD(N,2) 
      IF (MODN .EQ. 0) GO TO 124 
      XH(NS2+2) = 4.*X(1,NS2+1) 
  124 XH(1) = 0. 
      LNXH = NP1 
      LNSV = NP1 + INT(LOG(REAL(NP1))) + 4 
      LNWK = NP1 
!                                                                       
      CALL RFFT1F(NP1,1,XH,LNXH,WSAVE(NS2+1),LNSV,WORK,                 &
     &            LNWK,IER1)                                            
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('SINTF1',-5) 
        GO TO 200 
      ENDIF 
!                                                                       
      IF(MOD(NP1,2) .NE. 0) GO TO 30 
      XH(NP1) = XH(NP1)+XH(NP1) 
   30 SFNP1 = 1./FLOAT(NP1) 
         X(1,1) = .5*XH(1) 
         DSUM = X(1,1) 
      DO 105 I=3,N,2 
            X(1,I-1) = .5*XH(I) 
            DSUM = DSUM+.5*XH(I-1) 
            X(1,I) = DSUM 
  105 END DO 
      IF (MODN .NE. 0) GO TO 200 
      X(1,N) = .5*XH(N+1) 
  200 RETURN 
      END                                           

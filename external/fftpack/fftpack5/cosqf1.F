!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cosqf1.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSQF1 (N,INC,X,WSAVE,WORK,IER) 
      DIMENSION       X(INC,*)      ,WSAVE(*)      ,WORK(*) 
      IER = 0 
      NS2 = (N+1)/2 
      NP2 = N+2 
      DO 101 K=2,NS2 
         KC = NP2-K 
         WORK(K)  = X(1,K)+X(1,KC) 
         WORK(KC) = X(1,K)-X(1,KC) 
  101 END DO 
      MODN = MOD(N,2) 
      IF (MODN .NE. 0) GO TO 301 
      WORK(NS2+1) = X(1,NS2+1)+X(1,NS2+1) 
  301 DO 102 K=2,NS2 
         KC = NP2-K 
         X(1,K)  = WSAVE(K-1)*WORK(KC)+WSAVE(KC-1)*WORK(K) 
         X(1,KC) = WSAVE(K-1)*WORK(K) -WSAVE(KC-1)*WORK(KC) 
  102 END DO 
      IF (MODN .NE. 0) GO TO 303 
      X(1,NS2+1) = WSAVE(NS2)*WORK(NS2+1) 
  303 LENX = INC*(N-1)  + 1 
      LNSV = N + INT(LOG(REAL(N))) + 4 
      LNWK = N 
!                                                                       
      CALL RFFT1F(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSQF1',-5) 
        GO TO 400 
      ENDIF 
!                                                                       
      DO 103 I=3,N,2 
         XIM1 = .5*(X(1,I-1)+X(1,I)) 
         X(1,I) = .5*(X(1,I-1)-X(1,I)) 
         X(1,I-1) = XIM1 
  103 END DO 
  400 RETURN 
      END                                           

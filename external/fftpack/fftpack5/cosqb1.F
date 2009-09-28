!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cosqb1.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSQB1 (N,INC,X,WSAVE,WORK,IER) 
      DIMENSION       X(INC,*)     ,WSAVE(*)     ,WORK(*) 
      IER = 0 
      NS2 = (N+1)/2 
      NP2 = N+2 
      DO 101 I=3,N,2 
         XIM1 = X(1,I-1)+X(1,I) 
         X(1,I) = .5*(X(1,I-1)-X(1,I)) 
         X(1,I-1) = .5*XIM1 
  101 END DO 
      X(1,1) = .5*X(1,1) 
      MODN = MOD(N,2) 
      IF (MODN .NE. 0) GO TO 302 
      X(1,N) = .5*X(1,N) 
  302 LENX = INC*(N-1)  + 1 
      LNSV = N + INT(LOG(REAL(N))) + 4 
      LNWK = N 
!                                                                       
      CALL RFFT1B(N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSQB1',-5) 
        GO TO 400 
      ENDIF 
!                                                                       
      DO 102 K=2,NS2 
         KC = NP2-K 
         WORK(K) = WSAVE(K-1)*X(1,KC)+WSAVE(KC-1)*X(1,K) 
         WORK(KC) = WSAVE(K-1)*X(1,K)-WSAVE(KC-1)*X(1,KC) 
  102 END DO 
      IF (MODN .NE. 0) GO TO 305 
      X(1,NS2+1) = WSAVE(NS2)*(X(1,NS2+1)+X(1,NS2+1)) 
  305 DO 103 K=2,NS2 
         KC = NP2-K 
         X(1,K) = WORK(K)+WORK(KC) 
         X(1,KC) = WORK(K)-WORK(KC) 
  103 END DO 
      X(1,1) = X(1,1)+X(1,1) 
  400 RETURN 
      END                                           

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mcsqf1.f,v 1.2 2004/06/15 21:29:19 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MCSQF1 (LOT,JUMP,N,INC,X,WSAVE,WORK,IER) 
      DIMENSION       X(INC,*)      ,WSAVE(*)      ,WORK(LOT,*) 
      IER = 0 
      LJ = (LOT-1)*JUMP+1 
      NS2 = (N+1)/2 
      NP2 = N+2 
      DO 101 K=2,NS2 
         KC = NP2-K 
         M1 = 0 
         DO 201 M=1,LJ,JUMP 
         M1 = M1 + 1 
         WORK(M1,K)  = X(M,K)+X(M,KC) 
         WORK(M1,KC) = X(M,K)-X(M,KC) 
  201    CONTINUE 
  101 END DO 
      MODN = MOD(N,2) 
      IF (MODN .NE. 0) GO TO 301 
         M1 = 0 
         DO 202 M=1,LJ,JUMP 
         M1 = M1 + 1 
         WORK(M1,NS2+1) = X(M,NS2+1)+X(M,NS2+1) 
  202    CONTINUE 
  301    DO 102 K=2,NS2 
         KC = NP2-K 
         M1 = 0 
         DO 302 M=1,LJ,JUMP 
         M1 = M1 + 1 
         X(M,K)  = WSAVE(K-1)*WORK(M1,KC)+WSAVE(KC-1)*WORK(M1,K) 
         X(M,KC) = WSAVE(K-1)*WORK(M1,K) -WSAVE(KC-1)*WORK(M1,KC) 
  302    CONTINUE 
  102 END DO 
      IF (MODN .NE. 0) GO TO 303 
      M1 = 0 
      DO 304 M=1,LJ,JUMP 
         M1 = M1 + 1 
         X(M,NS2+1) = WSAVE(NS2)*WORK(M1,NS2+1) 
  304 END DO 
  303 CONTINUE 
      LENX = (LOT-1)*JUMP + INC*(N-1)  + 1 
      LNSV = N + INT(LOG(REAL(N))) + 4 
      LNWK = LOT*N 
!                                                                       
      CALL RFFTMF(LOT,JUMP,N,INC,X,LENX,WSAVE(N+1),LNSV,WORK,LNWK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('MCSQF1',-5) 
        GO TO 400 
      ENDIF 
!                                                                       
      DO 103 I=3,N,2 
         DO 203 M=1,LJ,JUMP 
            XIM1 = .5*(X(M,I-1)+X(M,I)) 
            X(M,I) = .5*(X(M,I-1)-X(M,I)) 
            X(M,I-1) = XIM1 
  203    CONTINUE 
  103 END DO 
  400 CONTINUE 
      RETURN 
      END                                           

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: sinqmb.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE SINQMB (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV,     &
     &                   WORK, LENWRK, IER)                             
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER 
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK) 
      LOGICAL    XERCON 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('SINQMB', 6) 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('SINQMB', 8) 
      ELSEIF (LENWRK .LT. LOT*N) THEN 
        IER = 3 
        CALL XERFFT ('SINQMB', 10) 
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN 
        IER = 4 
        CALL XERFFT ('SINQMB', -1) 
      ENDIF 
!                                                                       
      LJ = (LOT-1)*JUMP+1 
      IF (N .GT. 1) GO TO 101 
      DO 201 M=1,LJ,JUMP 
         X(M,1) = 4.*X(M,1) 
  201 END DO 
      RETURN 
  101 NS2 = N/2 
      DO 102 K=2,N,2 
         DO 202 M=1,LJ,JUMP 
         X(M,K) = -X(M,K) 
  202    CONTINUE 
  102 END DO 
      CALL COSQMB (LOT,JUMP,N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('SINQMB',-5) 
        GO TO 300 
      ENDIF 
      DO 103 K=1,NS2 
         KC = N-K 
         DO 203 M=1,LJ,JUMP 
         XHOLD = X(M,K) 
         X(M,K) = X(M,KC+1) 
         X(M,KC+1) = XHOLD 
  203    CONTINUE 
  103 END DO 
  300 CONTINUE 
      RETURN 
      END                                           

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: sinq1b.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE SINQ1B ( N, INC, X, LENX, WSAVE, LENSAV,               &
     &                   WORK, LENWRK, IER)                             
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER 
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENX .LT. INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('SINQ1B', 6) 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('SINQ1B', 8) 
      ELSEIF (LENWRK .LT. N) THEN 
        IER = 3 
        CALL XERFFT ('SINQ1B', 10) 
      ENDIF 
!                                                                       
      IF (N .GT. 1) GO TO 101 
      X(1,1) = 4.*X(1,1) 
      RETURN 
  101 NS2 = N/2 
      DO 102 K=2,N,2 
         X(1,K) = -X(1,K) 
  102 END DO 
      CALL COSQ1B (N,INC,X,LENX,WSAVE,LENSAV,WORK,LENWRK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('SINQ1B',-5) 
        GO TO 300 
      ENDIF 
      DO 103 K=1,NS2 
         KC = N-K 
         XHOLD = X(1,K) 
         X(1,K) = X(1,KC+1) 
         X(1,KC+1) = XHOLD 
  103 END DO 
  300 RETURN 
      END                                           

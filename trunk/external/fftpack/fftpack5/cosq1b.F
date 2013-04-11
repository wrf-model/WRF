!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cosq1b.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSQ1B (N, INC, X, LENX, WSAVE, LENSAV,                &
     &                   WORK, LENWRK, IER)                             
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER 
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENX .LT. INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('COSQ1B', 6) 
        GO TO 300 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('COSQ1B', 8) 
        GO TO 300 
      ELSEIF (LENWRK .LT. N) THEN 
        IER = 3 
        CALL XERFFT ('COSQ1B', 10) 
        GO TO 300 
      ENDIF 
!                                                                       
      IF (N-2) 300,102,103 
  102 SSQRT2 = 1./SQRT(2.) 
      X1 = X(1,1)+X(1,2) 
      X(1,2) = SSQRT2*(X(1,1)-X(1,2)) 
      X(1,1) = X1 
      RETURN 
  103 CALL COSQB1 (N,INC,X,WSAVE,WORK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSQ1B',-5) 
      ENDIF 
!                                                                       
  300 RETURN 
      END                                           

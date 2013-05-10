!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cost1f.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COST1F ( N, INC, X, LENX, WSAVE, LENSAV,               &
     &                   WORK, LENWRK, IER)                             
      INTEGER    N, INC, LENX, LENSAV, LENWRK, IER 
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK) 
!                                                                       
      IER = 0 
      IF (LENX .LT. INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('COST1F', 6) 
        GO TO 100 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('COST1F', 8) 
        GO TO 100 
      ELSEIF (LENWRK .LT. N-1) THEN 
        IER = 3 
        CALL XERFFT ('COST1F', 10) 
        GO TO 100 
      ENDIF 
!                                                                       
      CALL COSTF1(N,INC,X,WSAVE,WORK,IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COST1F',-5) 
      ENDIF 
!                                                                       
  100 CONTINUE 
      RETURN 
      END                                           

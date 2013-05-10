!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: sinqmi.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE SINQMI (N, WSAVE, LENSAV, IER) 
      INTEGER    N, LENSAV, IER 
      REAL       WSAVE(LENSAV) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('SINQMI', 3) 
        GO TO 300 
      ENDIF 
!                                                                       
      CALL COSQMI (N, WSAVE, LENSAV, IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('SINQMI',-5) 
      ENDIF 
  300 CONTINUE 
      RETURN 
      END                                           

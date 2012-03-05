!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfftmi.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFTMI (N, WSAVE, LENSAV, IER) 
      INTEGER    N, LENSAV, IER 
      REAL       WSAVE(LENSAV) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('RFFTMI ', 3) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      CALL MRFTI1 (N,WSAVE(1),WSAVE(N+1)) 
      RETURN 
      END                                           

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cfft1i.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CFFT1I (N, WSAVE, LENSAV, IER) 
      INTEGER    N, LENSAV, IER 
      REAL       WSAVE(LENSAV) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) + 4) THEN 
        IER = 2 
        CALL XERFFT ('CFFTMI ', 3) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      IW1 = N+N+1 
      CALL MCFTI1 (N,WSAVE,WSAVE(IW1),WSAVE(IW1+1)) 
      RETURN 
      END                                           

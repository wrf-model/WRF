!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfft1f.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFT1F ( N, INC, R, LENR, WSAVE, LENSAV,               &
     &                  WORK, LENWRK, IER)                              
      INTEGER  N, INC, LENR, LENSAV, LENWRK, IER 
      REAL     R(LENR), WSAVE(LENSAV), WORK(LENWRK) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENR .LT. INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('RFFT1F ', 6) 
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('RFFT1F ', 8) 
      ELSEIF (LENWRK .LT. N) THEN 
        IER = 3 
        CALL XERFFT ('RFFT1F ', 10) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      CALL RFFTF1 (N,INC,R,WORK,WSAVE,WSAVE(N+1)) 
      RETURN 
      END                                           

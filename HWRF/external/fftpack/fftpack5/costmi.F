!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: costmi.f,v 1.2 2004/06/15 21:29:19 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSTMI (N, WSAVE, LENSAV, IER) 
      INTEGER    N, LENSAV, IER 
      REAL       WSAVE(LENSAV) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('COSTMI', 3) 
        GO TO 300 
      ENDIF 
!                                                                       
      IF (N .LE. 3) RETURN 
      NM1 = N-1 
      NP1 = N+1 
      NS2 = N/2 
      PI = 4.*ATAN(1.) 
      DT = PI/FLOAT(NM1) 
      FK = 0. 
      DO 101 K=2,NS2 
         KC = NP1-K 
         FK = FK+1. 
         WSAVE(K) = 2.*SIN(FK*DT) 
         WSAVE(KC) = 2.*COS(FK*DT) 
  101 END DO 
      LNSV = NM1 + INT(LOG(REAL(NM1))) +4 
      CALL RFFTMI (NM1, WSAVE(N+1), LNSV, IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSTMI',-5) 
      ENDIF 
  300 CONTINUE 
      RETURN 
      END                                           

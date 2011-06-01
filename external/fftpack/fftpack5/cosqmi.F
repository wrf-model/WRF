!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cosqmi.f,v 1.2 2004/06/15 21:14:57 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE COSQMI (N, WSAVE, LENSAV, IER) 
      INTEGER    N, LENSAV, IER 
      REAL       WSAVE(LENSAV) 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('COSQMI', 3) 
        GO TO 300 
      ENDIF 
!                                                                       
      PIH = 2.*ATAN(1.) 
      DT = PIH/FLOAT(N) 
      FK = 0. 
      DO 101 K=1,N 
         FK = FK+1. 
         WSAVE(K) = COS(FK*DT) 
  101 END DO 
      LNSV = N + INT(LOG(REAL(N))) +4 
      CALL RFFTMI (N, WSAVE(N+1), LNSV, IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('COSQMI',-5) 
      ENDIF 
  300 CONTINUE 
      RETURN 
      END                                           

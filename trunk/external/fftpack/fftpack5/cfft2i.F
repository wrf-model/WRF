!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cfft2i.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CFFT2I (L, M, WSAVE, LENSAV, IER) 
      INTEGER L, M, IER 
      REAL WSAVE(LENSAV) 
!                                                                       
! Initialize error return                                               
!                                                                       
      IER = 0 
!                                                                       
      IF (LENSAV .LT. 2*L + INT(LOG(REAL(L))) +                         &
     &                    2*M + INT(LOG(REAL(M))) +8) THEN              
        IER = 2 
        CALL XERFFT ('CFFT2I', 4) 
        GO TO 100 
      ENDIF 
!                                                                       
      CALL CFFTMI (L, WSAVE(1), 2*L + INT(LOG(REAL(L))) + 4, IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('CFFT2I',-5) 
        GO TO 100 
      ENDIF 
      CALL CFFTMI (M, WSAVE(2*L+INT(LOG(REAL(L))*LOG(2.)) + 3),         &
     &            2*M + INT(LOG(REAL(M))) + 4, IER1)                    
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('CFFT2I',-5) 
      ENDIF 
!                                                                       
  100 CONTINUE 
      RETURN 
      END                                           

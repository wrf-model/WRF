!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfft2i.f,v 1.3 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFT2I (L, M, WSAVE, LENSAV, IER) 
      INTEGER L, M, LENSAV, IER 
      INTEGER LWSAV,MWSAV 
      REAL WSAVE(LENSAV) 
!                                                                       
! Initialize IER                                                        
!                                                                       
      IER = 0 
!                                                                       
! Verify LENSAV                                                         
!                                                                       
      LWSAV =   L+INT(LOG(REAL(L)))+4 
      MWSAV =   2*M+INT(LOG(REAL(M)))+4 
      IF (LENSAV .LT. LWSAV+MWSAV) THEN 
        IER = 2 
        CALL XERFFT ('RFFT2I', 4) 
        GO TO 100 
      ENDIF 
!                                                                       
      CALL RFFTMI (L, WSAVE(1), L + INT(LOG(REAL(L))) + 4, IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('RFFT2I',-5) 
        GO TO 100 
      ENDIF 
      CALL CFFTMI (M, WSAVE(L+INT(LOG(REAL(L)))+5),                     &
     &            2*M+INT(LOG(REAL(M)))+4,IER1)                         
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('RFFT2I',-5) 
      ENDIF 
!                                                                       
  100 CONTINUE 
      RETURN 
      END                                           

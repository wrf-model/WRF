!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: cfftmb.f,v 1.2 2004/06/15 21:08:32 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE CFFTMB (LOT, JUMP, N, INC, C, LENC, WSAVE, LENSAV,     &
     &                  WORK, LENWRK, IER)                              
      INTEGER  LOT, JUMP, N, INC, LENC, LENSAV, LENWRK, IER 
      COMPLEX       C(LENC) 
      REAL     WSAVE(LENSAV)     ,WORK(LENWRK) 
      LOGICAL XERCON 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENC .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('CFFTMB ', 6) 
      ELSEIF (LENSAV .LT. 2*N + INT(LOG(REAL(N))) + 4) THEN 
        IER = 2 
        CALL XERFFT ('CFFTMB ', 8) 
      ELSEIF (LENWRK .LT. 2*LOT*N) THEN 
        IER = 3 
        CALL XERFFT ('CFFTMB ', 10) 
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN 
        IER = 4 
        CALL XERFFT ('CFFTMB ', -1) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      IW1 = N+N+1 
      CALL CMFM1B (LOT,JUMP,N,INC,C,WORK,WSAVE,WSAVE(IW1),              &
     &                           WSAVE(IW1+1))                          
      RETURN 
      END                                           

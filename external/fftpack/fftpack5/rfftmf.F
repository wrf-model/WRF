!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfftmf.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFTMF (LOT, JUMP, N, INC, R, LENR, WSAVE, LENSAV,     &
     &                  WORK, LENWRK, IER)                              
      INTEGER  LOT, JUMP, N, INC, LENR, LENSAV, LENWRK, IER 
      REAL     R(LENR), WSAVE(LENSAV), WORK(LENWRK) 
      LOGICAL  XERCON 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENR .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('RFFTMF ', 6) 
      ELSEIF (LENSAV .LT. N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('RFFTMF ', 8) 
      ELSEIF (LENWRK .LT. LOT*N) THEN 
        IER = 3 
        CALL XERFFT ('RFFTMF ', 10) 
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN 
        IER = 4 
        CALL XERFFT ('RFFTMF ', -1) 
      ENDIF 
!                                                                       
      IF (N .EQ. 1) RETURN 
!                                                                       
      CALL MRFTF1 (LOT,JUMP,N,INC,R,WORK,WSAVE,WSAVE(N+1)) 
      RETURN 
      END                                           

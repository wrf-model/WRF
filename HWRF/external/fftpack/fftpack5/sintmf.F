!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: sintmf.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE SINTMF (LOT, JUMP, N, INC, X, LENX, WSAVE, LENSAV,     &
     &                   WORK, LENWRK, IER)                             
      INTEGER    LOT, JUMP, N, INC, LENX, LENSAV, LENWRK, IER 
      REAL       X(INC,*), WSAVE(LENSAV), WORK(LENWRK) 
      LOGICAL    XERCON 
!                                                                       
      IER = 0 
!                                                                       
      IF (LENX .LT. (LOT-1)*JUMP + INC*(N-1) + 1) THEN 
        IER = 1 
        CALL XERFFT ('SINTMF', 6) 
        GO TO 100 
      ELSEIF (LENSAV .LT. N/2 + N + INT(LOG(REAL(N))) +4) THEN 
        IER = 2 
        CALL XERFFT ('SINTMF', 8) 
        GO TO 100 
      ELSEIF (LENWRK .LT. LOT*(2*N+4)) THEN 
        IER = 3 
        CALL XERFFT ('SINTMF', 10) 
        GO TO 100 
      ELSEIF (.NOT. XERCON(INC,JUMP,N,LOT)) THEN 
        IER = 4 
        CALL XERFFT ('SINTMF', -1) 
        GO TO 100 
      ENDIF 
!                                                                       
      IW1 = LOT+LOT+1 
      IW2 = IW1+LOT*(N+1) 
      CALL MSNTF1(LOT,JUMP,N,INC,X,WSAVE,WORK,WORK(IW1),WORK(IW2),IER1) 
      IF (IER1 .NE. 0) THEN 
        IER = 20 
        CALL XERFFT ('SINTMF',-5) 
      ENDIF 
  100 CONTINUE 
      RETURN 
      END                                           

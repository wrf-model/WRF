!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: xercon.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      LOGICAL FUNCTION XERCON (INC,JUMP,N,LOT) 
      INTEGER INC, JUMP, N, LOT 
      INTEGER I, J, JNEW, LCM 
!                                                                       
!     Definition: positive integers INC, JUMP, N and LOT are consistent 
!                                                            ---------- 
!     if I1*INC + J1*JUMP = I2*INC + J2*JUMP for I1,I2 < N and J1,J2    
!     < LOT implies I1=I2 and J1=J2.                                    
!                                                                       
!     For multiple FFTs to execute correctly, input parameters INC,     
!     JUMP, N and LOT must be consistent ... otherwise at least one     
!     array element mistakenly is transformed more than once.           
!                                                                       
!     XERCON = .TRUE. if and only if INC, JUMP, N and LOT are           
!     consistent.                                                       
!                                                                       
!     ------------------------------------------------------------------
!                                                                       
!     Compute I = greatest common divisor (INC, JUMP)                   
!                                                                       
      I = INC 
      J = JUMP 
   10 CONTINUE 
      IF (J .NE. 0) THEN 
        JNEW = MOD(I,J) 
        I    = J 
        J    = JNEW 
        GO TO 10 
      ENDIF 
!                                                                       
! Compute LCM = least common multiple (INC, JUMP)                       
!                                                                       
      LCM = (INC*JUMP)/I 
!                                                                       
! Check consistency of INC, JUMP, N, LOT                                
!                                                                       
      IF (LCM .LE. (N-1)*INC .AND. LCM .LE. (LOT-1)*JUMP) THEN 
        XERCON = .FALSE. 
      ELSE 
        XERCON = .TRUE. 
      ENDIF 
!                                                                       
      RETURN 
      END                                           

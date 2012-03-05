!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfft2b.f,v 1.5 2004/07/06 00:58:41 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFT2B (LDIM, L, M, R, WSAVE, LENSAV, WORK,            &
     &  LENWRK, IER)                                                    
      INTEGER LDIM, L, M, LENSAV, LENWRK, IER 
      REAL    R(LDIM,M), WSAVE(LENSAV), WORK(LENWRK) 
!                                                                       
!                                                                       
! Initialize IER                                                        
!                                                                       
      IER = 0 
!                                                                       
! Verify LENSAV                                                         
!                                                                       
      LWSAV =   L + INT(LOG (REAL(L))) +4 
      MWSAV =   2*M + INT(LOG (REAL(M))) +4 
      IF (LENSAV .LT. LWSAV+MWSAV) THEN 
        IER = 2 
        CALL XERFFT ('RFFT2B', 6) 
        GO TO 100 
      ENDIF 
!                                                                       
! Verify LENWRK                                                         
!                                                                       
      IF (LENWRK .LT. 2*(L/2+1)*M) THEN 
        IER = 3 
        CALL XERFFT ('RFFT2B', 8) 
        GO TO 100 
      ENDIF 
!                                                                       
! Verify LDIM is as big as L                                            
!                                                                       
      IF (LDIM .LT. 2*(L/2+1)) THEN 
        IER = 5 
        CALL XERFFT ('RFFT2B', -6) 
        GO TO 100 
      ENDIF 
!                                                                       
! transform second dimension of array                                   
!                                                                       
      CALL CFFTMB(L/2+1,1,M,LDIM/2,R,M*LDIM/2,                          &
     &     WSAVE(L+INT(LOG(REAL(L)))+5),                                &
     &     2*M+INT(LOG(REAL(M)))+4,WORK,2*(L/2+1)*M,IER1)               
      IF(IER1.NE.0) THEN 
         IER=20 
         CALL XERFFT('RFFT2B',-5) 
         GO TO 100 
      ENDIF 
!                                                                       
! reshuffle                                                             
!                                                                       
      DO J=1,M 
         DO I=2,L 
            R(I,J)=R(I+1,J) 
         ENDDO 
      ENDDO 
!                                                                       
! Transform first dimension of array                                    
!                                                                       
      CALL RFFTMB(M,LDIM,L,1,R,M*LDIM,WSAVE(1),                         &
     &     L+INT(LOG(REAL(L)))+4,WORK,2*(L/2+1)*M,IER1)                 
      IF(IER1.NE.0) THEN 
         IER=20 
         CALL XERFFT('RFFT2F',-5) 
         GO TO 100 
      ENDIF 
!                                                                       
  100 CONTINUE 
!                                                                       
      RETURN 
      END                                           

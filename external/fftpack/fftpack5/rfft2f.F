!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: rfft2f.f,v 1.5 2004/07/06 00:58:41 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE RFFT2F (LDIM, L, M, R, WSAVE, LENSAV, WORK,            &
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
        CALL XERFFT ('RFFT2F', 6) 
        GO TO 100 
      ENDIF 
!                                                                       
! Verify LENWRK                                                         
!                                                                       
      IF (LENWRK .LT. 2*(L/2+1)*M) THEN 
        IER = 3 
        CALL XERFFT ('RFFT2F', 8) 
        GO TO 100 
      ENDIF 
!                                                                       
! Verify LDIM is as big as L                                            
!                                                                       
      IF (LDIM .LT. 2*(L/2+1)) THEN 
        IER = 5 
        CALL XERFFT ('RFFT2F', -6) 
        GO TO 100 
      ENDIF 
!                                                                       
! Transform first dimension of array                                    
!                                                                       
      CALL RFFTMF(M,LDIM,L,1,R,M*LDIM,WSAVE(1),                         &
     &     L+INT(LOG(REAL(L)))+4,WORK,2*(L/2+1)*M,IER1)                 
      IF(IER1.NE.0) THEN 
         IER=20 
         CALL XERFFT('RFFT2F',-5) 
         GO TO 100 
      ENDIF 
!                                                                       
! reshuffle to add in nyquist imaginary components                      
!                                                                       
      DO J=1,M 
         IF(MOD(L,2).EQ.0) R(L+2,J)=0.0 
         DO I=L,2,-1 
            R(I+1,J)=R(I,J) 
         ENDDO 
         R(2,J)=0.0 
      ENDDO 
!                                                                       
! transform second dimension of array                                   
!                                                                       
      CALL CFFTMF(L/2+1,1,M,LDIM/2,R,M*LDIM/2,                          &
     &     WSAVE(L+INT(LOG(REAL(L)))+5),                                &
     &     2*M+INT(LOG(REAL(M)))+4,WORK,2*(L/2+1)*M,IER1)               
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

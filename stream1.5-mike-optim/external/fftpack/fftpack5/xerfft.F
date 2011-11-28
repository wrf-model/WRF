!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: xerfft.f,v 1.3 2004/07/06 00:58:41 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE XERFFT( SRNAME, INFO) 
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER*6        SRNAME 
      INTEGER            INFO 
!                                                                       
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  XERFFT  is an error handler for library FFTPACK version 5.0 routines.
!  It is called by an FFTPACK 5.0 routine if an input parameter has an  
!  invalid value.  A message is printed and execution stops.            
!                                                                       
!  Installers may consider modifying the STOP statement in order to     
!  call system-specific exception-handling facilities.                  
!                                                                       
!  Arguments                                                            
!  =========                                                            
!                                                                       
!  SRNAME  (input) CHARACTER*6                                          
!          The name of the routine which called XERFFT.                 
!                                                                       
!  INFO    (input) INTEGER                                              
!          When a single  invalid parameter in the parameter list of    
!          the calling routine has been detected, INFO is the position  
!          of that parameter.  In the case when an illegal combination  
!          of LOT, JUMP, N, and INC has been detected, the calling      
!          subprogram calls XERFFT with INFO = -1.                      
!                                                                       
! ===================================================================== 
!                                                                       
!     .. Executable Statements ..                                       
!                                                                       
      IF (INFO .GE. 1) THEN 
        WRITE( *, '(A,A,A,I3,A)') ' ** On entry to ', SRNAME,           &
     &    ' parameter number ', INFO, ' had an illegal value'           
      ELSEIF (INFO .EQ. -1) THEN 
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,              &
     &    ' parameters LOT, JUMP, N and INC are inconsistent'           
      ELSEIF (INFO .EQ. -2) THEN 
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,              &
     &    ' parameter L is greater than LDIM'                           
      ELSEIF (INFO .EQ. -3) THEN 
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,              &
     &    ' parameter M is greater than MDIM'                           
      ELSEIF (INFO .EQ. -5) THEN 
        WRITE( *, '(A,A,A,A)') ' ** Within ', SRNAME,                   &
     &    ' input error returned by lower level routine'                
      ELSEIF (INFO .EQ. -6) THEN 
        WRITE( *, '(A,A,A,A)') ' ** On entry to ', SRNAME,              &
     &    ' parameter LDIM is less than 2*(L/2+1)'                      
      ENDIF 
!                                                                       
      STOP 
!                                                                       
!     End of XERFFT                                                     
!                                                                       
      END                                           

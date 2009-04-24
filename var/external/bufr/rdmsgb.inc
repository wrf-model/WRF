      SUBROUTINE RDMSGB (LUNIT, MESG, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMSGB                                                 
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS THE NEXT BUFR MESSAGE FROM LOGICAL    
!   UNIT LUNIT AS AN ARRAY OF BYTES, WHICH ARE THEN TRANSFERRED TO      
!   AN ARRAY OF INTEGER WORDS FOR OUTPUT.                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL RDMSGB (LUNIT, MESG, IRET)                             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MESG     - *-WORD ARRAY CONTAINING BUFR MESSAGE READ FROM LUNIT   
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = end-of-file encountered while reading       
!                           from LUNIT                                  
!                      -2 = I/O error encountered while reading         
!                           from LUNIT                                  
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ICHKSTR  IUPBS01  LMSG                  
!    THIS ROUTINE IS CALLED BY: READMG                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      DIMENSION MESG ( * ) 
                                                                        
      CHARACTER(8) SEC0 
      CHARACTER(1) CBAY (8 * MXMSGLD4) 
      DIMENSION JBAY (MXMSGLD4) 
                                                                        
      EQUIVALENCE (CBAY (1), JBAY (1), SEC0) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      SEC0 = ' ' 
      READ (LUNIT, END = 100, ERR = 200) SEC0, (CBAY (I) , I = 9,       &
      IUPBS01 (JBAY, 'LENM') )                                          
                                                                        
!     Confirm that the first 4 bytes of CBAY contain 'BUFR' encoded in  
!     CCITT IA5 (i.e. ASCII).  Note that, since CBAY(1) is equivalenced 
!     to SEC0, then CBAY already contains the entire BUFR message!      
                                                                        
      IF (ICHKSTR ('BUFR', CBAY, 4) .NE.0) GOTO 200 
                                                                        
!     Transfer the message from CBAY into MESG.                         
                                                                        
      DO I = 1, LMSG (SEC0) 
      MESG (I) = JBAY (I) 
      ENDDO 
                                                                        
      IRET = 0 
      RETURN 
                                                                        
  100 IRET = - 1 
      RETURN 
                                                                        
  200 IRET = - 2 
      RETURN 
                                                                        
      END SUBROUTINE RDMSGB                         

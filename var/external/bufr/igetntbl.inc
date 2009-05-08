      FUNCTION IGETNTBL (LUNT, LINE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IGETNTBL                                               
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS FUNCTION GETS THE NEXT LINE FROM THE ASCII MASTER     
!   TABLE FILE SPECIFIED BY LUNT, IGNORING ANY BLANK LINES OR COMMENT   
!   LINES IN THE PROCESS.                                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    IGETNTBL ( LUNT, LINE )                                     
!   INPUT ARGUMENT LIST:                                                
!     LUNT     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING MASTER TABLE INFORMATION                    
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     LINE     - CHARACTER*(*): NEXT NON-BLANK, NON-COMMENT LINE READ   
!                FROM LUNT                                              
!     IGETNTBL - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = end-of-file encountered while reading       
!                           from LUNT                                   
!                      -2 = I/O error encountered while reading         
!                           from LUNT                                   
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: GETNTBE  GETTBH   SNTBDE                
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) LINE 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
   10 READ (LUNT, '(A)', END = 100, ERR = 200) LINE 
      IF ( (LINE.EQ.' ') .OR. (LINE (1:1) .EQ.'#') ) GOTO 10 
      IF (LINE (1:3) .EQ.'END') GOTO 100 
                                                                        
      IGETNTBL = 0 
      RETURN 
                                                                        
  100 IGETNTBL = - 1 
      RETURN 
                                                                        
  200 IGETNTBL = - 2 
      RETURN 
                                                                        
      END FUNCTION IGETNTBL                         

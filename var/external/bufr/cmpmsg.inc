      SUBROUTINE CMPMSG (CF) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CMPMSG                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2005-03-09            
!                                                                       
! ABSTRACT: THIS SUBROUTINE IS USED TO SPECIFY WHETHER OR NOT BUFR      
!   MESSAGES CREATED BY FUTURE CALLS TO EITHER OF THE BUFR ARCHIVE      
!   LIBRARY SUBROUTINES WRITSB OR WRITSA ARE TO BE COMPRESSED.          
!   THIS SUBROUTINE CAN BE CALLED AT ANY TIME AFTER THE FIRST CALL      
!   TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF, AND THE POSSIBLE VALUES  
!   FOR CF ARE 'N' (= 'NO', WHICH IS THE DEFAULT) AND 'Y' (= 'YES').    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-03-09  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL CMPMSG (CF)                                            
!   INPUT ARGUMENT LIST:                                                
!     CF       - CHARACTER*1: FLAG INDICATING WHETHER BUFR MESSAGES     
!                OUTPUT BY FUTURE CALLS TO WRITSB OR WRITSA ARE TO      
!                BE COMPRESSED:                                         
!                       'N' = 'NO' (THE DEFAULT)                        
!                       'Y' = 'YES'                                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CAPIT                          
!    THIS ROUTINE IS CALLED BY: COPYSB   WRITCP                         
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / MSGCMP / CCMF 
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(1) CCMF, CF 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CALL CAPIT (CF) 
      IF (CF.NE.'Y'.AND.CF.NE.'N') GOTO 900 
      CCMF = CF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: CMPMSG - INPUT ARGUMENT IS ",A1,'//'"&
     &, IT MUST BE EITHER Y OR N")') CF                                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE CMPMSG                         

      SUBROUTINE STDMSG (CF) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    STDMSG                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2004-08-18            
!                                                                       
! ABSTRACT: THIS SUBROUTINE IS USED TO SPECIFY WHETHER OR NOT BUFR      
!   MESSAGES THAT WILL BE OUTPUT BY FUTURE CALLS TO ANY OF THE BUFR     
!   ARCHIVE LIBRARY SUBROUTINES WHICH CREATE SUCH MESSAGES (E.G. WRITCP,
!   WRITSB, COPYMG, WRITSA, ETC.) ARE TO BE "STANDARDIZED".  SEE THE    
!   DOCUMENTATION BLOCK WITHIN BUFR ARCHIVE LIBRARY SUBROUTINE STNDRD   
!   FOR AN EXPLANATION OF WHAT "STANDARDIZATION" MEANS.  THIS SUBROUTINE
!   CAN BE CALLED AT ANY TIME AFTER THE FIRST CALL TO BUFR ARCHIVE      
!   LIBRARY SUBROUTINE OPENBF, AND THE POSSIBLE VALUES FOR CF ARE 'N'   
!   (= 'NO', WHICH IS THE DEFAULT) AND 'Y' (= 'YES').                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2004-08-18  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL STDMSG (CF)                                            
!   INPUT ARGUMENT LIST:                                                
!     CF       - CHARACTER*1: FLAG INDICATING WHETHER BUFR MESSAGES     
!                OUTPUT BY FUTURE CALLS TO WRITCP, WRITSB, COPYMG, ETC. 
!                SHOULD BE "STANDARDIZED":                              
!                       'N' = 'NO' (THE DEFAULT)                        
!                       'Y' = 'YES'                                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CAPIT                          
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / MSGSTD / CSMF 
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(1) CSMF, CF 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CALL CAPIT (CF) 
      IF (CF.NE.'Y'.AND.CF.NE.'N') GOTO 900 
      CSMF = CF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: STDMSG - INPUT ARGUMENT IS ",A1,'//'"&
     &, IT MUST BE EITHER Y OR N")') CF                                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE STDMSG                         

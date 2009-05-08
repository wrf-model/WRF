      FUNCTION IREADMM (IMSG, SUBSET, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IREADMM                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READMM  
!   AND PASSES BACK ITS RETURN CODE.  SEE READMM FOR MORE DETAILS.      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1999-11-18  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN IREADMG)    
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS              
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    IREADMM (IMSG, SUBSET, IDATE)                               
!   INPUT ARGUMENT LIST:                                                
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER (RECORD) IN    
!                STORAGE                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     IDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IREADMM  - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = IMSG is either 0 or greater than the        
!                           number of messages in memory                
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READMM                                  
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(8) SUBSET 
      CALL READMM (IMSG, SUBSET, IDATE, IRET) 
      IREADMM = IRET 
      RETURN 
      END FUNCTION IREADMM                          

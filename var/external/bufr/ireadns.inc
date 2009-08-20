      FUNCTION IREADNS (LUNIT, SUBSET, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IREADNS                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READNS  
!   AND PASSES BACK ITS RETURN CODE.  SEE READNS FOR MORE DETAILS.      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN IREADMG)    
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS              
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    IREADNS (LUNIT, SUBSET, IDATE)                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR BUFR MESSAGE         
!                CONTAINING SUBSET BEING READ                           
!     IDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE CONTAINING SUBSET BEING READ, IN FORMAT OF     
!                EITHER YYMMDDHH OR YYYYMMDDHH, DEPENDING ON DATELEN()  
!                VALUE                                                  
!     IREADNS  - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more subsets in the BUFR file  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READNS                                  
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
      CALL READNS (LUNIT, SUBSET, IDATE, IRET) 
      IREADNS = IRET 
      RETURN 
      END FUNCTION IREADNS                          

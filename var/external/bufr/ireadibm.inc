      FUNCTION IREADIBM (LUNIT, SUBSET, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IREADIBM                                               
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READIBM 
!   AND PASSES BACK ITS RETURN CODE.  SEE READIBM FOR MORE DETAILS. IT  
!   IS CONSIDERED OBSOLETE AND MAY BE REMOVED FROM THE BUFR ARCHIVE     
!   LIBRARY IN A FUTURE VERSION.  USERS SHOULD MIGRATE TO THE DIRECT    
!   USE OF IREADMG WHICH CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READMG.  
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
! USAGE:    IREADIBM  (LUNIT, SUBSET, IDATE)                            
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     IDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IREADIBM - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more BUFR messages in LUNIT    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READIBM                                 
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
      CALL READIBM (LUNIT, SUBSET, IDATE, IRET) 
      IREADIBM = IRET 
      RETURN 
      END FUNCTION IREADIBM                         

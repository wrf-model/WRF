      FUNCTION IREADMG (LUNIT, SUBSET, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IREADMG                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READMG  
!   AND PASSES BACK ITS RETURN CODE.  SEE READMG FOR MORE DETAILS.      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1999-11-18  J. WOOLLEN -- ADDED NEW FUNCTION ENTRY POINTS IREADMM AND 
!                           IREADIBM                                    
! 2002-05-14  J. WOOLLEN -- REMOVED ENTRY POINTS ICOPYSB, IREADFT,      
!                           IREADIBM, IREADMM, IREADNS AND IREADSB      
!                           (THEY BECAME SEPARATE ROUTINES IN THE       
!                           BUFRLIB TO INCREASE PORTABILITY TO OTHER    
!                           PLATFORMS)                                  
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    IREADMG (LUNIT, SUBSET, IDATE)                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     IDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IREADMG  - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more BUFR messages in LUNIT    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READMG                                  
!    THIS ROUTINE IS CALLED BY: UFBTAB                                  
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(8) SUBSET 
      CALL READMG (LUNIT, SUBSET, IDATE, IRET) 
      IREADMG = IRET 
      RETURN 
      END FUNCTION IREADMG                          

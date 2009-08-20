      FUNCTION IREADSB (LUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IREADSB                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CALLS BUFR ARCHIVE LIBRARY SUBROUTINE READSB  
!   AND PASSES BACK ITS RETURN CODE.  SEE READSB FOR MORE DETAILS.      
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
! USAGE:    IREADSB (LUNIT)                                             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IREADSB  - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more subsets in the BUFR       
!                           message                                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        READSB                                  
!    THIS ROUTINE IS CALLED BY: UFBTAB                                  
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CALL READSB (LUNIT, IRET) 
      IREADSB = IRET 
      RETURN 
      END FUNCTION IREADSB                          

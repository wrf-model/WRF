      FUNCTION IFBGET (LUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IFBGET                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CHECKS TO SEE IF ANY UNREAD SUBSETS ARE IN    
!   AN INPUT BUFR MESSAGE PREVIOUSLY OPENED BY BUFR ARCHIVE LIBRARY     
!   SUBROUTINE OPENMG OR OPENMB.                                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    IFBGET (LUNIT)                                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IFBGET   - INTEGER: RETURN CODE:                                  
!                       0 = there is at least one more subset in the    
!                           message                                     
!                      -1 = there are no more subsets in the message    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STATUS                         
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT                           
!  ------------------------------------------                           
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
!  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE                        
!  ---------------------------------------------                        
                                                                        
      IF (NSUB (LUN) .LT.MSUB (LUN) ) THEN 
         IFBGET = 0 
      ELSE 
         IFBGET = - 1 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: IFBGET - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: IFBGET - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: IFBGET - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
      END FUNCTION IFBGET                           

      SUBROUTINE UFBCNT (LUNIT, KMSG, KSUB) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBCNT                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS A COUNT OF THE CURRENT MESSAGE      
!   NUMBER AND SUBSET NUMBER, WHERE THE MESSAGE NUMBER IS RELATIVE TO   
!   ALL MESSAGES IN THE BUFR FILE AND THE SUBSET NUMBER IS RELATIVE TO  
!   ALL SUBSETS IN THE MESSAGE.  IF THE MESSAGE/SUBSET ARE BEING READ,  
!   THE MESSAGE COUNT ADVANCES EACH TIME BUFR ARCHIVE LIBRARY           
!   SUBROUTINE READMG (OR EQUIVALENT) IS CALLED AND THE SUBSET COUNT    
!   ADVANCES EACH TIME BUFR ARCHIVE LIBRARY SUBROUTINE READSB (OR       
!   EQUIVALENT) IS CALLED FOR A PARTICULAR MESSAGE.  IF THE MESSAGE/    
!   SUBSET ARE BEING WRITTEN, THE MESSAGE COUNT ADVANCES EACH TIME      
!   BUFR ARCHIVE LIBRARY SUBROUTINE OPENMG (OR EQUIVALENT) IS CALLED    
!   AND THE SUBSET COUNT ADVANCES EACH TIME BUFR ARCHIVE LIBRARY        
!   SUBROUTINE WRITSB (OR EQUIVALENT) IS CALLED.                        
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
! USAGE:    CALL UFBCNT (LUNIT, KMSG, KSUB)                             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     KMSG     - INTEGER: POINTER TO MESSAGE COUNT IN BUFR FILE         
!                (INCLUDING MESSAGE CURRENTLY OPEN FOR READING/WRITING) 
!     KSUB     - INTEGER: POINTER TO SUBSET COUNT IN BUFR MESSAGE       
!                (INCLUDING SUBSET CURRENTLY OPEN FOR READING/WRITING)  
!                                                                       
! REMARKS:                                                              
!    IF AN APPLICATION PROGRAM DESIRES TO KNOW THE NUMBER OF SUBSETS IN 
!    A BUFR MESSAGES JUST OPENED, IT MUST USE THE FUNCTION NMSUB RATHER 
!    THAN THIS SUBROUTINE BECAUSE KSUB ONLY INCREMENTS BY ONE FOR EACH  
!    CALL TO READSB (I.E., KSUB = 0 IMMEDIATELY AFTER READMG IS         
!    CALLED).                                                           
!                                                                       
!    THIS ROUTINE CALLS:        BORT     STATUS                         
!    THIS ROUTINE IS CALLED BY: UFBPOS                                  
!                               Also called by application programs.    
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
                                                                        
!  CHECK THE FILE STATUS - RETURN THE MESSAGE AND SUBSET COUNTERS       
!  --------------------------------------------------------------       
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      KMSG = NMSG (LUN) 
      KSUB = NSUB (LUN) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: STATUS - BUFR FILE IS CLOSED, IT MUST BE '// &
      'OPEN FOR EITHER INPUT OR OUTPUT')                                
      END SUBROUTINE UFBCNT                         

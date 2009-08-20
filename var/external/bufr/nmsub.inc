      FUNCTION NMSUB (LUNIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NMSUB                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION RETURNS THE NUMBER OF SUBSETS IN A BUFR       
!   MESSAGE OPEN FOR INPUT VIA A PREVIOUS CALL TO BUFR ARCHIVE LIBRARY  
!   SUBROUTINE READMG OR EQUIVALENT.  THE SUBSETS THEMSELVES DO NOT     
!   HAVE TO BE READ.                                                    
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
! USAGE:    NMSUB (LUNIT)                                               
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NMSUB    - INTEGER: NUMBER OF SUBSETS IN BUFR MESSAGE             
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STATUS                         
!    THIS ROUTINE IS CALLED BY: UFBMNS   UFBPOS   UFBTAB   UFBTAM       
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
                                                                        
      NMSUB = 0 
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
      NMSUB = MSUB (LUN) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: NMSUB - INPUT BUFR FILE IS CLOSED, IT MUST '/&
     &/'BE OPEN FOR INPUT')                                             
  901 CALL BORT ('BUFRLIB: NMSUB - INPUT BUFR FILE IS OPEN FOR OUTPUT,'/&
     &/' IT MUST BE OPEN FOR INPUT')                                    
  902 CALL BORT ('BUFRLIB: NMSUB - A MESSAGE MUST BE OPEN IN INPUT '//  &
      'BUFR FILE, NONE ARE')                                            
      END FUNCTION NMSUB                            

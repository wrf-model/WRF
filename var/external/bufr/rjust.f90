      FUNCTION RJUST (STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RJUST                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION RIGHT JUSTIFIES A CHARACTER STRING.           
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    RJUST (STR)                                                 
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING TO BE RIGHT-JUSTIFED             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): RIGHT-JUSTIFIED STRING                  
!     RJUST    - REAL: ALWAYS RETURNED AS 0 (DUMMY)                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: SNTBBE   UFBDMP   UFDUMP   VALX         
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      RJUST = 0. 
      IF (STR.EQ.' ') GOTO 100 
      LSTR = LEN (STR) 
      DO WHILE (STR (LSTR:LSTR) .EQ.' ') 
      DO I = LSTR, 2, - 1 
      STR (I:I) = STR (I - 1:I - 1) 
      ENDDO 
      STR (1:1) = ' ' 
      ENDDO 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION RJUST                            

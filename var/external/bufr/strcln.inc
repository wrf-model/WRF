      SUBROUTINE STRCLN 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    STRCLN                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RESETS THE MNEMONIC STRING CACHE IN THE     
!   BUFR INTERFACE (ARRAYS IN COMMON BLOCK /STCACH/).  THE MNEMONIC     
!   STRING CACHE IS A PERFORMANCE ENHANCING DEVICE WHICH SAVES TIME     
!   WHEN THE SAME MNEMONIC STRINGS ARE ENCOUNTERED IN A PROGRAM, OVER   
!   AND OVER AGAIN (THE TYPICAL SCENARIO).                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-04-02  J. WOOLLEN -- MODIFIED TO ENLARGE THE CACHE FROM 50       
!                           ELEMENTS TO 1000, MAXIMUM                   
! 1998-07-08  J. WOOLLEN -- CORRECTED SOME MINOR ERRORS                 
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    CALL STRCLN                                                 
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: MAKESTAB                                
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / STCACH / MSTR, NSTR, LSTR, LUNS (MXS, 2), USRS (MXS),    &
      ICON (52, MXS)                                                    
      CHARACTER(80) USRS 
                                                                        
      MSTR = MXS 
      NSTR = 0 
      LSTR = 0 
      RETURN 
      END SUBROUTINE STRCLN                         

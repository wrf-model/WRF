      LOGICAL FUNCTION DIGIT (STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    DIGIT                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS LOGICAL FUNCTION TESTS THE CHARACTERS IN A STRING TO   
!   DETERMINE IF THEY ARE ALL DIGITS ('0','1','2','3','4','5','6','7',  
!   '8' or '9').                                                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
! 2007-01-19  J. ATOR    -- SIMPLIFIED LOGIC                            
!                                                                       
! USAGE:    DIGIT (STR)                                                 
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING                                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     DIGIT    - LOGICAL: TRUE IF ALL CHARACTERS IN STR ARE DIGITS      
!                ('0' - '9'), OTHERWISE FALSE                           
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: CKTABA   NUMBCK   RDBFDX   RDUSDX       
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      DIGIT = .FALSE. 
      DO I = 1, LEN (STR) 
      IF (LLT (STR (1:1) , '0') .OR.LGT (STR (1:1) , '9') ) GOTO 100 
      ENDDO 
      DIGIT = .TRUE. 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION DIGIT                            

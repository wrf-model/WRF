      FUNCTION NEMOCK (NEMO) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEMOCK                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CHECKS A MNEMONIC TO VERIFY THAT IT HAS A     
!   LENGTH OF BETWEEN ONE AND EIGHT CHARACTERS AND THAT IT ONLY         
!   CONTAINS CHARACTERS FROM THE ALLOWABLE CHARACTER SET.               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- SPLIT NON-ZERO RETURN INTO -1 FOR LENGTH    
!                           NOT 1-8 CHARACTERS AND -2 FOR INVALID       
!                           CHARACTERS (RETURN ONLY -1 BEFORE FOR ALL   
!                           PROBLEMATIC CASES); UNIFIED/PORTABLE FOR    
!                           WRF; ADDED HISTORY DOCUMENTATION            
!                                                                       
! USAGE:    NEMOCK (NEMO)                                               
!   INPUT ARGUMENT LIST:                                                
!     NEMO     - CHARACTER*(*): MNEMONIC TO BE CHECKED                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NEMOCK   - INTEGER: INDICATOR AS TO WHETHER NEMO IS VALID:        
!                       0 = yes                                         
!                      -1 = no, length not between 1 and 8 characters   
!                      -2 = no, it does not contain characters from the 
!                           allowable character set                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: RDUSDX   SEQSDX   SNTBBE   SNTBDE       
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) NEMO 
      CHARACTER(38) CHRSET 
                                                                        
      DATA CHRSET / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.' / 
      DATA NCHR / 38 / 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  GET THE LENGTH OF NEMO                                               
!  ----------------------                                               
                                                                        
      LNEMO = 0 
                                                                        
      DO I = LEN (NEMO), 1, - 1 
      IF (NEMO (I:I) .NE.' ') THEN 
      LNEMO = I 
      GOTO 1 
      ENDIF 
      ENDDO 
                                                                        
    1 IF (LNEMO.LT.1.OR.LNEMO.GT.8) THEN 
      NEMOCK = - 1 
      GOTO 100 
      ENDIF 
                                                                        
!  SCAN NEMO FOR ALLOWABLE CHARACTERS                                   
!  ----------------------------------                                   
                                                                        
      DO 10 I = 1, LNEMO 
      DO J = 1, NCHR 
      IF (NEMO (I:I) .EQ.CHRSET (J:J) ) GOTO 10 
      ENDDO 
      NEMOCK = - 2 
      GOTO 100 
   10 ENDDO 
                                                                        
      NEMOCK = 0 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION NEMOCK                           

      SUBROUTINE RSVFVM (NEM1, NEM2) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RSVFVM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE STEPS THROUGH THE "FOLLOWING VALUE"         
!   MNEMONIC NEM1 AND, FOR EACH "." CHARACTER ENCOUNTERED (EXCEPT FOR   
!   THE INITIAL ONE), OVERWRITES IT WITH THE NEXT CORRESPONDING         
!   CHARACTER FROM NEM2 (SEE REMARKS).                                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION                               
!                                                                       
! USAGE:    CALL RSVFVM (NEM1, NEM2)                                    
!   INPUT ARGUMENT LIST:                                                
!     NEM1     - CHARACTER*8: "FOLLOWING VALUE" MNEMONIC                
!     NEM2     - CHARACTER*8: MNEMONIC IMMEDIATELY FOLLOWING NEM1       
!                WITHIN USER DICTIONARY TABLE                           
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NEM1     - CHARACTER*8: COPY OF INPUT NEM1 WITH ALL "."           
!                CHARACTERS (EXCEPT INITIAL ONE) OVERWRITTEN WITH       
!                CORRESPONDING CHARACTERS FROM NEM2                     
!                                                                       
! REMARKS:                                                              
!    FOR EXAMPLE:                                                       
!     if, on input:    NEM1 = ".DTH...."                                
!                      NEM2 = "MXTM    "                                
!     then, on output: NEM1 = ".DTHMXTM"                                
!                                                                       
!                                                                       
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: NEMTBD   SEQSDX                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(8) NEM1, NEM2 
                                                                        
      DO I = 1, LEN (NEM1) 
      IF (I.EQ.1) THEN 
                                                                        
!        Skip initial "." and initialize J.                             
                                                                        
      J = 1 
      ELSE 
      IF (NEM1 (I:I) .EQ.'.') THEN 
      NEM1 (I:I) = NEM2 (J:J) 
      J = J + 1 
      ENDIF 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
      END SUBROUTINE RSVFVM                         

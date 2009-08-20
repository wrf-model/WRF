      SUBROUTINE INCTAB (ATAG, ATYP, NODE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    INCTAB                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS THE NEXT AVAILABLE POSITIONAL INDEX 
!   FOR WRITING INTO THE INTERNAL JUMP/LINK TABLE IN COMMON BLOCK       
!   /TABLES/, AND IT ALSO USES THAT INDEX TO STORE ATAG AND ATYP        
!   WITHIN, RESPECTIVELY, THE INTERNAL JUMP/LINK TABLE ARRAYS TAG(*)    
!   AND TYP(*).  IF THERE IS NO MORE ROOM FOR ADDITIONAL ENTRIES WITHIN 
!   THE INTERNAL JUMP/LINK TABLE, THEN AN APPROPRIATE CALL IS MADE TO   
!   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED HISTORY DOCUMENTATION; OUTPUTS   
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    CALL INCTAB (ATAG, ATYP, NODE)                              
!   INPUT ARGUMENT LIST:                                                
!     ATAG     - CHARACTER*(*): MNEMONIC NAME                           
!     ATYP     - CHARACTER*(*): MNEMONIC TYPE                           
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NODE     - INTEGER: NEXT AVAILABLE POSITIONAL INDEX FOR WRITING   
!                INTO THE INTERNAL JUMP/LINK TABLE                      
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: TABENT   TABSUB                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
      MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
      IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
      ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      CHARACTER ( * ) ATAG, ATYP 
      CHARACTER(128) BORT_STR 
      CHARACTER(10) TAG 
      CHARACTER(3) TYP 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      NTAB = NTAB + 1 
      IF (NTAB.GT.MAXTAB) GOTO 900 
      TAG (NTAB) = ATAG 
      TYP (NTAB) = ATYP 
      NODE = NTAB 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: INCTAB - THE NUMBER OF JUMP/LINK '// &
      'TABLE ENTRIES EXCEEDS THE LIMIT, MAXTAB (",I7,")")') MAXTAB      
      CALL BORT (BORT_STR) 
      END SUBROUTINE INCTAB                         

      SUBROUTINE NEMTBAX (LUN, NEMO, MTYP, MSBT, INOD) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEMTBAX                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18           
!                                                                       
! ABSTRACT: THIS SUBROUTINE SEARCHES FOR MNEMONIC NEMO WITHIN THE       
!   INTERNAL TABLE A ARRAYS HOLDING THE DICTIONARY TABLE (ARRAYS IN     
!   COMMON BLOCK /TABABD/) AND, IF FOUND, RETURNS INFORMATION ABOUT     
!   THAT MNEMONIC FROM WITHIN THESE ARRAYS.  IT IS IDENTICAL TO BUFR    
!   ARCHIVE LIBRARY SUBROUTINE NEMTBA EXCEPT IF NEMO IS NOT FOUND, THIS 
!   SUBROUTINE RETURNS WITH INOD EQUAL TO ZERO (NEMTBA CALLS BUFR       
!   ARCHIVE LIBRARY SUBROUTINE BORT IN THIS CASE).                      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1999-11-18  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    CALL NEMTBAX (LUN, NEMO, MTYP, MSBT, INOD)                  
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     NEMO     - CHARACTER*(*): TABLE A MNEMONIC TO SEARCH FOR          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MTYP     - INTEGER: MESSAGE TYPE CORRESPONDING TO NEMO            
!     MSBT     - INTEGER: MESSAGE SUBTYPE CORRESPONDING TO NEMO         
!     INOD     - INTEGER: POSITIONAL INDEX OF NEMO WITHIN INTERNAL      
!                         JUMP/LINK TABLE IF NEMO FOUND                 
!                       0 = NEMO not found                              
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: CKTABA                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
                                                                        
      CHARACTER ( * ) NEMO 
      CHARACTER(600) TABD 
      CHARACTER(128) BORT_STR 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      INOD = 0 
                                                                        
!  LOOK FOR NEMO IN TABLE A                                             
!  ------------------------                                             
                                                                        
      DO I = 1, NTBA (LUN) 
      IF (TABA (I, LUN) (4:11) .EQ.NEMO) THEN 
      MTYP = IDNA (I, LUN, 1) 
      MSBT = IDNA (I, LUN, 2) 
      INOD = MTAB (I, LUN) 
      IF (MTYP.LT.0.OR.MTYP.GT.255) GOTO 900 
      IF (MSBT.LT.0.OR.MSBT.GT.255) GOTO 901 
      GOTO 100 
      ENDIF 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NEMTBAX - INVALID MESSAGE TYPE (",I4'&
     &//',") RETURNED FOR MENMONIC ",A)') MTYP, NEMO                    
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: NEMTBAX - INVALID MESSAGE SUBTYPE ("'&
     &//',I4,") RETURNED FOR MENMONIC ",A)') MSBT, NEMO                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE NEMTBAX                        

      SUBROUTINE REWNBF (LUNIT, ISR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    REWNBF                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE, DEPENDING ON THE VALUE OF ISR, WILL        
!   EITHER:                                                             
!        1) STORE THE CURRENT PARAMETERS ASSOCIATED WITH A BUFR FILE    
!   CONNECTED TO LUNIT (READ/WRITE POINTERS, ETC.), SET THE FILE STATUS 
!   TO READ, THEN REWIND THE BUFR FILE AND POSITION IT SUCH THAT THE    
!   NEXT BUFR MESSAGE READ WILL BE THE FIRST MESSAGE IN THE FILE        
!   CONTAINING ACTUAL SUBSETS WITH DATA; OR                             
!        2) RESTORE THE BUFR FILE CONNECTED TO LUNIT TO THE PARAMETERS  
!   IT HAD PRIOR TO 1) ABOVE USING THE INFORMATION SAVED IN 1) ABOVE.   
!                                                                       
!   THIS ALLOWS INFORMATION TO BE EXTRACTED FROM A PARTICULAR SUBSET IN 
!   A BUFR FILE WHICH IS IN THE MIDST OF BEING READ FROM OR WRITTEN TO  
!   BY AN APPLICATION PROGRAM.  NOTE THAT FOR A PARTICULAR BUFR FILE 1) 
!   ABOVE MUST PRECEDE 2) ABOVE.  AN APPLICATION PROGRAM MIGHT FIRST    
!   CALL THIS SUBROUTINE WITH ISR = 0, THEN CALL EITHER BUFR ARCHIVE    
!   LIBRARY SUBROUTINE RDMGSB OR UFBINX TO GET INFO FROM A SUBSET, THEN 
!   CALL THIS ROUTINE AGAIN WITH ISR = 1 TO RESTORE THE POINTERS IN THE 
!   BUFR FILE TO THEIR ORIGINAL LOCATION.  ALSO, BUFR ARCHIVE LIBRARY   
!   SUBROUTINE UFBTAB WILL CALL THIS ROUTINE IF THE BUFR FILE IT IS     
!   ACTING UPON IS ALREADY OPEN FOR INPUT OR OUTPUT.                    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION BUT MAY HAVE BEEN IN THE PRODUCTION 
!                           VERSION AT ONE TIME AND THEN REMOVED)       
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL REWNBF (LUNIT, ISR)                                    
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     ISR      - INTEGER: SWITCH:                                       
!                       0 = store current parameters associated with    
!                           BUFR file, set file status to read, rewind  
!                           file such that next message read is first   
!                           message containing subset data              
!                       1 = restore BUFR file with parameters saved     
!                           from the previous call to this routine with 
!                           ISR=0                                       
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     I4DY     RDMSGW   STATUS       
!                               WTSTAT                                  
!    THIS ROUTINE IS CALLED BY: UFBINX   UFBTAB                         
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
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
      COMMON / BUFRSR / JUNN, JILL, JIMM, JBIT, JBYT, JMSG, JSUB, KSUB, &
      JNOD, JDAT, JSR (NFILES), JBAY (MXMSGLD4)                         
                                                                        
      CHARACTER(128) BORT_STR 
                                                                        
      DIMENSION MESG (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  TRY TO TRAP BAD CALL PROBLEMS                                        
!  -----------------------------                                        
                                                                        
      IF (ISR.EQ.0) THEN 
         CALL STATUS (LUNIT, LUN, IL, IM) 
         IF (JSR (LUN) .NE.0) GOTO 900 
         IF (IL.EQ.0) GOTO 901 
      ELSEIF (ISR.EQ.1) THEN 
         LUN = JUNN 
         IF (JSR (JUNN) .NE.1) GOTO 902 
      ELSE 
         GOTO 903 
      ENDIF 
                                                                        
!  STORE FILE PARAMETERS AND SET FOR READING                            
!  -----------------------------------------                            
                                                                        
      IF (ISR.EQ.0) THEN 
         JUNN = LUN 
         JILL = IL 
         JIMM = IM 
         JBIT = IBIT 
         JBYT = MBYT (LUN) 
         JMSG = NMSG (LUN) 
         JSUB = NSUB (LUN) 
         KSUB = MSUB (LUN) 
         JNOD = INODE (LUN) 
         JDAT = IDATE (LUN) 
         DO I = 1, JBYT 
         JBAY (I) = MBAY (I, LUN) 
         ENDDO 
         CALL WTSTAT (LUNIT, LUN, - 1, 0) 
      ENDIF 
                                                                        
!  REWIND THE FILE AND POSITION AFTER THE DICTIONARY                    
!  -------------------------------------------------                    
                                                                        
      REWIND LUNIT 
    1 CALL RDMSGW (LUNIT, MESG, IER) 
      IF (IER.EQ. - 1) GOTO 2 
      IF (IER.EQ. - 2) GOTO 904 
      IF (IUPBS01 (MESG, 'MTYP') .EQ.11) GOTO 1 
    2 BACKSPACE LUNIT 
                                                                        
!  RESTORE FILE PARAMETERS AND POSITION IT TO WHERE IT WAS SAVED        
!  -------------------------------------------------------------        
                                                                        
      IF (ISR.EQ.1) THEN 
         LUN = JUNN 
         IL = JILL 
         IM = JIMM 
         IBIT = JBIT 
         MBYT (LUN) = JBYT 
         NMSG (LUN) = JMSG 
         NSUB (LUN) = JSUB 
         MSUB (LUN) = KSUB 
         INODE (LUN) = JNOD 
         IDATE (LUN) = I4DY (JDAT) 
         DO I = 1, JBYT 
         MBAY (I, LUN) = JBAY (I) 
         ENDDO 
         DO IMSG = 1, JMSG 
         READ (LUNIT, ERR = 905, END = 906) 
         ENDDO 
         CALL WTSTAT (LUNIT, LUN, IL, IM) 
      ENDIF 
                                                                        
      JSR (LUN) = MOD (JSR (LUN) + 1, 2) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: REWNBF - ATTEMPING TO SAVE '//'PARAME&
     &TERS FOR FILE FOR WHICH THEY HAVE ALREADY BEEN SAVED '//'(AND NOT &
     &YET RESTORED) (UNIT",I3,")")') LUNIT                              
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: REWNBF - ATTEMPING TO SAVE '//       &
      'PARAMETERS FOR BUFR FILE WHICH IS NOT OPENED FOR EITHER INPUT'// &
      ' OR OUTPUT) (UNIT",I3,")")') LUNIT                               
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: REWNBF - ATTEMPING TO RESTORE '//    &
      'PARAMETERS TO BUFR FILE WHICH WERE NEVER SAVED (UNIT",I3,")")')  &
      LUNIT                                                             
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: REWNBF - SAVE/RESTORE SWITCH (INPUT '&
     &//'ARGUMENT ISR) IS NOT ZERO OR ONE (HERE =",I4,") (UNIT",I3,")")'&
     &)                                                                 
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: REWNBF - ERROR READING A DICTIONARY '&
     &//'MESSAGE AFTER REWIND OF BUFR FILE IN UNIT",I4,")")') LUNIT     
      CALL BORT (BORT_STR) 
  905 WRITE (BORT_STR, '("BUFRLIB: REWNBF - ERROR READING MSG (RECORD) '&
     &//'NO.",I5," IN ATTEMPT TO REPOSITION BUFR FILE IN UNIT",I3," TO'/&
     &/' ORIGINAL MSG NO.",I5)') IMSG, LUNIT, JMSG                      
      CALL BORT (BORT_STR) 
  906 WRITE (BORT_STR, '("BUFRLIB: REWNBF - HIT END OF FILE BEFORE '//  &
      'REPOSITIONING BUFR FILE IN UNIT",I3," TO ORIGINAL MESSAGE '//    &
      'NO.",I5)') LUNIT, JMSG                                           
      CALL BORT (BORT_STR) 
      END SUBROUTINE REWNBF                         

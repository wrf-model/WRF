      SUBROUTINE UFBCUP (LUBIN, LUBOT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBCUP                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE MAKES ONE COPY OF EACH UNIQUE ELEMENT IN AN 
!   INPUT SUBSET BUFFER INTO THE IDENTICAL MNEMONIC SLOT IN THE OUTPUT  
!   SUBSET BUFFER.                                                      
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
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
!                                                                       
! USAGE:    CALL UFBCUP (LUBIN, LUBOT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUBIN    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUBOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     STATUS                         
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
      CHARACTER(10) TAGI (MAXJL), TAGO !, TAG 
!     CHARACTER(3) TYP 
      DIMENSION NINI (MAXJL) 
!     REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!  CHECK THE FILE STATUSES AND I-NODE                                   
!  ----------------------------------                                   
                                                                        
      CALL STATUS (LUBIN, LUI, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUI) .NE.INV (1, LUI) ) GOTO 903 
                                                                        
      CALL STATUS (LUBOT, LUO, IL, IM) 
      IF (IL.EQ.0) GOTO 904 
      IF (IL.LT.0) GOTO 905 
      IF (IM.EQ.0) GOTO 906 
                                                                        
!  MAKE A LIST OF UNIQUE TAGS IN INPUT BUFFER                           
!  ------------------------------------------                           
                                                                        
      NTAG = 0 
                                                                        
      DO 5 NI = 1, NVAL (LUI) 
         NIN = INV (NI, LUI) 
         IF (ITP (NIN) .GE.2) THEN 
            DO NV = 1, NTAG 
            IF (TAGI (NV) .EQ.TAG (NIN) ) GOTO 5 
            ENDDO 
            NTAG = NTAG + 1 
            NINI (NTAG) = NI 
            TAGI (NTAG) = TAG (NIN) 
         ENDIF 
    5 ENDDO 
                                                                        
      IF (NTAG.EQ.0) GOTO 907 
                                                                        
!  GIVEN A LIST MAKE ONE COPY OF COMMON ELEMENTS TO OUTPUT BUFFER       
!  --------------------------------------------------------------       
                                                                        
      DO 10 NV = 1, NTAG 
         NI = NINI (NV) 
         DO NO = 1, NVAL (LUO) 
         TAGO = TAG (INV (NO, LUO) ) 
         IF (TAGI (NV) .EQ.TAGO) THEN 
            VAL (NO, LUO) = VAL (NI, LUI) 
            GOTO 10 
         ENDIF 
         ENDDO 
   10 ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UFBCUP - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: UFBCUP - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBCUP - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
  904 CALL BORT ('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  905 CALL BORT ('BUFRLIB: UFBCUP - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  906 CALL BORT ('BUFRLIB: UFBCUP - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  907 CALL BORT ('BUFRLIB: UFBCUP - THERE ARE NO ELEMENTS (TAGS) IN '// &
      'INPUT SUBSET BUFFER')                                            
      END SUBROUTINE UFBCUP                         

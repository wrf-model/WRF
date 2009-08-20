      SUBROUTINE ELEMDX (CARD, LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    ELEMDX                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE DECODES THE SCALE FACTOR, REFERENCE VALUE,  
!   BIT WIDTH AND UNITS (I.E., THE "ELEMENTS") FROM A TABLE B MNEMONIC  
!   DEFINITION CARD THAT WAS PREVIOUSLY READ FROM A USER-SUPPLIED BUFR  
!   DICTIONARY TABLE FILE IN CHARACTER FORMAT BY BUFR ARCHIVE LIBRARY   
!   SUBROUTINE RDUSDX.  THESE DECODED VALUES ARE THEN ADDED TO THE      
!   ALREADY-EXISTING ENTRY FOR THAT MNEMONIC (BUILT IN RDUSDX) WITHIN   
!   THE INTERNAL BUFR TABLE B ARRAY TABB(*,LUN) IN COMMON BLOCK         
!   /TABABD/.                                                           
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE   
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2 
! 2007-01-19  J. ATOR    -- ADDED EXTRA ARGUMENT FOR CALL TO JSTCHR     
!                                                                       
! USAGE:    CALL ELEMDX (CARD, LUN)                                     
!   INPUT ARGUMENT LIST:                                                
!     CARD     - CHARACTER*80: MNEMONIC DEFINITION CARD THAT WAS READ   
!                FROM A USER-SUPPLIED BUFR DICTIONARY TABLE             
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT2    CAPIT    JSTCHR   JSTNUM       
!                               NEMTAB                                  
!    THIS ROUTINE IS CALLED BY: RDUSDX                                  
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
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(80) CARD 
      CHARACTER(24) UNIT 
      CHARACTER(11) REFR, REFR_ORIG 
      CHARACTER(8) NEMO 
      CHARACTER(4) SCAL, SCAL_ORIG 
      CHARACTER(3) BITW, BITW_ORIG 
      CHARACTER(1) SIGN, TAB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CAPTURE THE VARIOUS ELEMENTS CHARACTERISTICS                         
!  --------------------------------------------                         
                                                                        
      NEMO = CARD (3:10) 
      SCAL = CARD (14:17) 
      REFR = CARD (21:31) 
      BITW = CARD (35:37) 
      UNIT = CARD (41:64) 
!  .... Make sure the units are all capitalized                         
      CALL CAPIT (UNIT) 
                                                                        
!  FIND THE ELEMENT TAG IN TABLE B                                      
!  -------------------------------                                      
                                                                        
!     Note that an entry for this mnemonic should already exist within  
!     the internal BUFR Table B array TABB(*,LUN); this entry should    
!     have been created by subroutine RDUSDX when the mnemonic and its  
!     associated FXY value and description were initially defined within
!     a card read from the "Descriptor Definition" section at the top of
!     the user-supplied BUFR dictionary table in character format.  Now,
!     we need to retrieve the positional index for that entry within    
!     TABB(*,LUN) so that we can access the entry and then add the scale
!     factor, reference value, bit width, and units to it.              
                                                                        
      CALL NEMTAB (LUN, NEMO, IDSN, TAB, IELE) 
      IF (TAB.NE.'B') GOTO 900 
                                                                        
!  LEFT JUSTIFY AND STORE CHARACTERISTICS                               
!  --------------------------------------                               
                                                                        
      CALL JSTCHR (UNIT, IRET) 
      IF (IRET.NE.0) GOTO 904 
      TABB (IELE, LUN) (71:94) = UNIT 
                                                                        
      SCAL_ORIG = SCAL 
      CALL JSTNUM (SCAL, SIGN, IRET) 
      IF (IRET.NE.0) GOTO 901 
      TABB (IELE, LUN) (95:95) = SIGN 
      TABB (IELE, LUN) (96:98) = SCAL 
                                                                        
      REFR_ORIG = REFR 
      CALL JSTNUM (REFR, SIGN, IRET) 
      IF (IRET.NE.0) GOTO 902 
      TABB (IELE, LUN) (99:99) = SIGN 
      TABB (IELE, LUN) (100:109) = REFR 
                                                                        
      BITW_ORIG = BITW 
      CALL JSTNUM (BITW, SIGN, IRET) 
      IF (IRET.NE.0) GOTO 903 
      IF (SIGN.EQ.'-') GOTO 903 
      TABB (IELE, LUN) (110:112) = BITW 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR1, '("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)')    &
      CARD                                                              
      WRITE (BORT_STR2, '(18X,"MNEMONIC ",A," IS NOT A TABLE B ENTRY '//&
      '(UNDEFINED, TAB=",A,")")') NEMO, TAB                             
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  901 WRITE (BORT_STR1, '("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)')    &
      CARD                                                              
      WRITE (BORT_STR2, '(18X,"PARSED SCALE VALUE (=",A,") IS NOT '//   &
      'NUMERIC")') SCAL_ORIG                                            
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  902 WRITE (BORT_STR1, '("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)')    &
      CARD                                                              
      WRITE (BORT_STR2, '(18X,"PARSED REFERENCE VALUE (=",A,") IS NOT '/&
     &/'NUMERIC")') REFR_ORIG                                           
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  903 WRITE (BORT_STR1, '("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)')    &
      CARD                                                              
      WRITE (BORT_STR2, '(18X,"PARSED BIT WIDTH VALUE (=",A,") IS NOT '/&
     &/'NUMERIC")') BITW_ORIG                                           
      CALL BORT2 (BORT_STR1, BORT_STR2) 
  904 WRITE (BORT_STR1, '("BUFRLIB: ELEMDX - CARD READ IN IS: ",A)')    &
      CARD                                                              
      WRITE (BORT_STR2, '(18X,"UNITS FIELD IS EMPTY")') 
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END SUBROUTINE ELEMDX                         

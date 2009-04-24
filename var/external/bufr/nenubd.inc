      SUBROUTINE NENUBD (NEMO, NUMB, LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NENUBD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CHECKS A MNEMONIC AND FXY VALUE PAIR THAT   
!   WERE READ FROM A USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER   
!   FORMAT, IN ORDER TO MAKE SURE THAT NEITHER VALUE HAS ALREADY BEEN   
!   DEFINED WITHIN INTERNAL BUFR TABLE B OR D (IN COMMON BLOCK          
!   /TABABD/) FOR THE GIVEN LUN.  IF EITHER VALUE HAS ALREADY BEEN      
!   DEFINED FOR THIS LUN, THEN AN APPROPRIATE CALL IS MADE TO           
!   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN NENUCK)     
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE   
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT" (IN PARENT ROUTINE NENUCK)   
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI) (IN PARENT        
!                           ROUTINE NENUCK)                             
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS (NENUCK WAS  
!                           THEN REMOVED BECAUSE IT WAS JUST A DUMMY    
!                           ROUTINE WITH ENTRIES)                       
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    CALL NENUBD (NEMO, NUMB, LUN)                               
!   INPUT ARGUMENT LIST:                                                
!     NEMO     - CHARACTER*8: MNEMONIC                                  
!     NUMB     - CHARACTER*6: FXY VALUE ASSOCIATED WITH NEMO            
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    EXAMPLE SHOWING LAYOUT OF INTERNAL BUFR TABLE B (FROM A DBX DEBUG  
!    SESSION USING "bufrtab.002", AND WHERE LUN = 1)                    
!                                                                       
!   (dbx) print NTBB[1]                                                 
!   95                                                                  
!                                                                       
!   (dbx) print TABB[1,1]                                               
!   0x1003c164 = "063000BYTCNT                                  ",      
!                "                        BYTES                 ",      
!                "  +0  +0         16                 "                 
!                                                                       
!   (dbx) print TABB[2,1]                                               
!   0x1003c1e4 = "063255BITPAD                                  ",      
!                "                        NONE                  ",      
!                "  +0  +0         1                  "                 
!                                                                       
!   (dbx) print TABB[3,1]                                               
!   0x1003c264 = "031000DRF1BIT                                 ",      
!                "                        NUMERIC               ",      
!                "  +0  +0         1                  "                 
!                                                                       
!   (dbx) print TABB[8,1]                                               
!   0x1003c4e4 = "001003WMOR     WMO REGION NUMBER              ",      
!                "                        CODE TABLE            ",      
!                "  +0  +0         3                  "                 
!                                                                       
!   (dbx) print TABB[11,1]                                              
!   0x1003c664 = "001194BUHD     BULLETIN HEADER                ",      
!                "                        CCITT IA5             ",      
!                "  +0  +0         64                 "                 
!                                                                       
!   (dbx) print TABB[21,1]                                              
!   0x1003cb64 = "004003DAYS     DAY                            ",      
!                "                        DAY                   ",      
!                "  +0  +0         6                  "                 
!                                                                       
!   (dbx) print TABB[33,1]                                              
!   0x1003d164 = "005002CLAT     LATITUDE (COARSE ACCURACY)     ",      
!                "                        DEGREES               ",      
!                "  +2  -0000"     15                 "                 
!                                                                       
!   and so on, up through TABB[95,1] ( = TABB[NTBB[LUN],LUN] )          
!                                                                       
!                                                                       
!                                                                       
!    EXAMPLE SHOWING LAYOUT OF INTERNAL BUFR TABLE D (FROM A DBX DEBUG  
!    SESSION USING "bufrtab.002", AND WHERE LUN = 1)                    
!                                                                       
!   (dbx) print NTBD[1]                                                 
!   43                                                                  
!                                                                       
!   (dbx) &TABD[1,1]/14c                                                
!   1008a364:  '3' '6' '0' '0' '0' '1' 'D' 'R' 'P' '1' '6' 'B' 'I' 'T'  
!                                                                       
!   (dbx) &TABD[2,1]/14c                                                
!   1008a5bc:  '3' '6' '0' '0' '0' '2' 'D' 'R' 'P' '8' 'B' 'I' 'T' ' '  
!                                                                       
!   (dbx) &TABD[3,1]/14c                                                
!   1008a814:  '3' '6' '0' '0' '0' '3' 'D' 'R' 'P' 'S' 'T' 'A' 'K' ' '  
!                                                                       
!   (dbx) &TABD[4,1]/14c                                                
!   1008aa6c:  '3' '6' '0' '0' '0' '4' 'D' 'R' 'P' '1' 'B' 'I' 'T' ' '  
!                                                                       
!   (dbx) &TABD[5,1]/14c                                                
!   1008acc4:  '3' '6' '3' '2' '1' '8' 'N' 'C' '0' '0' '2' '0' '0' '1'  
!                                                                       
!   (dbx) &TABD[6,1]/14c                                                
!   1008af1c:  '3' '6' '3' '2' '1' '9' 'N' 'C' '0' '0' '2' '0' '0' '2'  
!                                                                       
!   (dbx) &TABD[24,1]/14c                                               
!   1008d94c:  '3' '6' '1' '1' '3' '0' 'U' 'A' 'A' 'D' 'F' ' ' ' ' ' '  
!                                                                       
!   and so on, up through TABD[43,1] ( = TABD[NTBD[LUN],LUN] )          
!                                                                       
!                                                                       
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: RDBFDX   RDUSDX                         
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
      CHARACTER(128) BORT_STR 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(8) NEMO 
      CHARACTER(6) NUMB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  CHECK TABLE B AND D                                                  
!  -------------------                                                  
                                                                        
      DO N = 1, NTBB (LUN) 
      IF (NUMB.EQ.TABB (N, LUN) (1:6) ) GOTO 900 
      IF (NEMO.EQ.TABB (N, LUN) (7:14) ) GOTO 901 
      ENDDO 
                                                                        
      DO N = 1, NTBD (LUN) 
      IF (NUMB.EQ.TABD (N, LUN) (1:6) ) GOTO 902 
      IF (NEMO.EQ.TABD (N, LUN) (7:14) ) GOTO 903 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NENUBD - TABLE B FXY VALUE (",A,") '/&
     &/'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB                   
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: NENUBD - TABLE B MNEMONIC (",A,") '//&
      'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO                    
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: NENUBD - TABLE D FXY VALUE (",A,") '/&
     &/'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB                   
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: NENUBD - TABLE D MNEMONIC (",A,") '//&
      'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO                    
      CALL BORT (BORT_STR) 
      END SUBROUTINE NENUBD                         

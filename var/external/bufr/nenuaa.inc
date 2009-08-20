      SUBROUTINE NENUAA (NEMO, NUMB, LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NENUAA                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CHECKS A MNEMONIC AND FXY VALUE PAIR THAT   
!   WERE READ FROM A USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER   
!   FORMAT, IN ORDER TO MAKE SURE THAT NEITHER VALUE HAS ALREADY BEEN   
!   DEFINED WITHIN INTERNAL BUFR TABLE A (IN COMMON BLOCK /TABABD/) FOR 
!   THE GIVEN LUN.  IF EITHER VALUE HAS ALREADY BEEN DEFINED FOR THIS   
!   LUN, THEN AN APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY       
!   SUBROUTINE BORT.                                                    
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
! USAGE:    CALL NENUAA (NEMO, NUMB, LUN)                               
!   INPUT ARGUMENT LIST:                                                
!     NEMO     - CHARACTER*8: MNEMONIC                                  
!     NUMB     - CHARACTER*6: FXY VALUE ASSOCIATED WITH NEMO            
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!                                                                       
!    EXAMPLE SHOWING LAYOUT OF INTERNAL BUFR TABLE A (FROM A DBX DEBUG  
!    SESSION USING "bufrtab.002", AND WHERE LUN = 1)                    
!                                                                       
!   (dbx) print NTBA[1]                                                 
!   8                                                                   
!                                                                       
!   (dbx) print TABA[1,1]                                               
!   0x1002c764 = "218NC002001 MESSAGE TYPE 002-001  RAWINSONDE -",      
!                " FIXED LAND                                   ",      
!                "                                    "                 
!                                                                       
!   (dbx) print TABA[2,1]                                               
!   0x1002c7e4 = "219NC002002 MESSAGE TYPE 002-002  RAWINSONDE -",      
!                " MOBIL LAND                                   ",      
!                "                                    "                 
!                                                                       
!   (dbx) print TABA[3,1]                                               
!   0x1002c864 = "220NC002003 MESSAGE TYPE 002-003  RAWINSONDE -",      
!                " SHIP                                         ",      
!                "                                    "                 
!                                                                       
!   and so on, up through TABA[8,1] ( = TABA[NTBA[LUN],LUN] )           
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
                                                                        
!  CHECK TABLE A                                                        
!  -------------                                                        
                                                                        
      DO N = 1, NTBA (LUN) 
      IF (NUMB (4:6) .EQ.TABA (N, LUN) (1:3) ) GOTO 900 
      IF (NEMO.EQ.TABA (N, LUN) (4:11) ) GOTO 901 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NENUAA - TABLE A FXY VALUE (",A,") '/&
     &/'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB                   
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: NENUAA - TABLE A MNEMONIC (",A,") '//&
      'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO                    
      CALL BORT (BORT_STR) 
      END SUBROUTINE NENUAA                         

      SUBROUTINE NEMTBB (LUN, ITAB, UNIT, ISCL, IREF, IBIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEMTBB                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CHECKS ALL OF THE PROPERTIES (E.G. FXY      
!   VALUE, UNITS, SCALE FACTOR, REFERENCE VALUE, ETC.) OF A SPECIFIED   
!   MNEMONIC WITHIN THE INTERNAL BUFR TABLE B ARRAYS (IN COMMON BLOCK   
!   /TABABD/) IN ORDER TO VERIFY THAT THE VALUES OF THOSE PROPERTIES    
!   ARE ALL LEGAL AND WELL-DEFINED.  IF ANY ERRORS ARE FOUND, THEN AN   
!   APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT.   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE   
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS 
! 1999-11-18  J. WOOLLEN -- CHANGED CALL TO FUNCTION "VAL$" TO "VALX"   
!                           (IT HAS BEEN RENAMED TO REMOVE THE          
!                           POSSIBILITY OF THE "$" SYMBOL CAUSING       
!                           PROBLEMS ON OTHER PLATFORMS)                
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    CALL NEMTBB (LUN, ITAB, UNIT, ISCL, IREF, IBIT)             
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     ITAB     - INTEGER: POSITIONAL INDEX INTO INTERNAL BUFR TABLE B   
!                ARRAYS FOR MNEMONIC TO BE CHECKED                      
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     UNIT     - CHARACTER*24: UNITS OF MNEMONIC                        
!     ISCL     - INTEGER: SCALE FACTOR OF MNEMONIC                      
!     IREF     - INTEGER: REFERENCE VALUE OF MNEMONIC                   
!     IBIT     - INTEGER: BIT WIDTH OF MNEMONIC                         
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IFXY     VALX                  
!    THIS ROUTINE IS CALLED BY: CHEKSTAB RESTD    TABENT                
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
      CHARACTER(24) UNIT 
      CHARACTER(8) NEMO 
      REAL(8) MXR 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      MXR = 1E11 - 1 
                                                                        
      IF (ITAB.LE.0.OR.ITAB.GT.NTBB (LUN) ) GOTO 900 
                                                                        
!  PULL OUT TABLE B INFORMATION                                         
!  ----------------------------                                         
                                                                        
      IDN = IDNB (ITAB, LUN) 
      NEMO = TABB (ITAB, LUN) (7:14) 
      UNIT = TABB (ITAB, LUN) (71:94) 
      ISCL = VALX (TABB (ITAB, LUN) (95:98) ) 
      IREF = VALX (TABB (ITAB, LUN) (99:109) ) 
      IBIT = VALX (TABB (ITAB, LUN) (110:112) ) 
                                                                        
!  CHECK TABLE B CONTENTS                                               
!  ----------------------                                               
                                                                        
      IF (IDN.LT.IFXY ('000000') ) GOTO 901 
      IF (IDN.GT.IFXY ('063255') ) GOTO 901 
                                                                        
      IF (ISCL.LT. - 999.OR.ISCL.GT.999) GOTO 902 
      IF (IREF.LE. - MXR.OR.IREF.GE.MXR) GOTO 903 
      IF (IBIT.LE.0) GOTO 904 
      IF (UNIT (1:5) .NE.'CCITT'.AND.IBIT.GT.32) GOTO 904 
      IF (UNIT (1:5) .EQ.'CCITT'.AND.MOD (IBIT, 8) .NE.0) GOTO 905 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - ITAB (",I7,") NOT FOUND IN '&
     &//'TABLE B")') ITAB                                               
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - INTEGER REPRESENTATION OF '/&
     &/'DESCRIPTOR FOR TABLE B MNEMONIC ",A," (",I7,") IS OUTSIDE '//'RA&
     &NGE 0-16383 (16383 -> 0-63-255)")') NEMO, IDN                     
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - SCALE VALUE FOR TABLE B '// &
      'MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE -999 TO 999")') NEMO,   &
      ISCL                                                              
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - REFERENCE VALUE FOR TABLE B'&
     &//' MNEMONIC ",A," (",I7,") IS OUTSIDE RANGE +/- 1E11-1")') NEMO, &
     &IREF                                                              
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - BIT WIDTH FOR NON-CHARACTER'&
     &//' TABLE B MNEMONIC ",A," (",I7,") IS > 32")') NEMO, IBIT        
      CALL BORT (BORT_STR) 
  905 WRITE (BORT_STR, '("BUFRLIB: NEMTBB - BIT WIDTH FOR CHARACTER '// &
      'TABLE B MNEMONIC ",A," (",I7,") IS NOT A MULTIPLE OF 8")') NEMO, &
      IBIT                                                              
      CALL BORT (BORT_STR) 
      END SUBROUTINE NEMTBB                         

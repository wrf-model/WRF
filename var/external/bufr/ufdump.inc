      SUBROUTINE UFDUMP (LUNIT, LUPRT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFDUMP                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT: THIS SUBROUTINE DUMPS A DETAILED PRINT LISTING OF THE       
!   CONTENTS OF THE UNPACKED DATA SUBSET CURRENTLY RESIDING IN THE      
!   INTERNAL ARRAYS ASSOCIATED WITH THE BUFR FILE IN LOGICAL UNIT LUNIT.
!   LUNIT MUST HAVE BEEN OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR   
!   ARCHIVE LIBRARY SUBROUTINE OPENBF.  THE DATA SUBSET MUST HAVE BEEN  
!   SUBSEQUENTLY READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY ARRAYS VIA 
!   A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR READERME,       
!   FOLLOWED BY A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READSB (OR VIA
!   A SINGLE CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READNS!).  FOR A   
!   PARTICULAR SUBSET, THE PRINT LISTING CONTAINS EACH MNEMONIC         
!   ACCOMPANIED BY ITS CORRESPONDING DATA VALUE (INCLUDING THE ACTUAL   
!   BITS THAT WERE SET FOR FLAG TABLE VALUES!) AS WELL AS OTHER USEFUL  
!   IDENTIFICATION INFORMATION.  THIS SUBROUTINE IS SIMILAR TO BUFR     
!   ARCHIVE LIBRARY SUBROUTINE UFBDMP EXCEPT THAT IT DOES NOT PRINT     
!   POINTERS, COUNTERS AND OTHER MORE ESOTERIC INFORMATION DESCRIBING   
!   THE INTERNAL SUBSET STRUCTURES.  EACH SUBROUTINE, UFBDMP AND UFDUMP,
!   IS USEFUL FOR DIFFERENT DIAGNOSTIC PURPOSES, BUT IN GENERAL UFDUMP  
!   IS MORE USEFUL FOR JUST LOOKING AT THE DATA ELEMENTS.               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. WOOLLEN -- MODIFIED TO HANDLE PRINT OF CHARACTER       
!                           VALUES GREATER THAN EIGHT BYTES             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2004-08-18  J. ATOR    -- ADDED FUZZINESS TEST AND THRESHOLD FOR      
!                           MISSING VALUE; ADDED INTERACTIVE AND        
!                           SCROLLING CAPABILITY SIMILAR TO UFBDMP      
! 2006-04-14  J. ATOR    -- ADD CALL TO UPFTBV FOR FLAG TABLES TO GET   
!                           ACTUAL BITS THAT WERE SET TO GENERATE VALUE 
! 2007-01-19  J. ATOR    -- USE FUNCTION IBFMS                          
!                                                                       
! USAGE:    CALL UFDUMP (LUNIT, LUPRT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     LUPRT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR PRINT OUTPUT  
!                FILE                                                   
!                       0 = LUPRT is set to 06                          
!                                                                       
!   OUTPUT FILES:                                                       
!     IF LUPRT > 0: UNIT "LUPRT" - PRINT (IF LUPRT=6, STANDARD OUTPUT)  
!     IF LUPRT = 0: UNIT 06      - STANDARD OUTPUT PRINT                
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE WILL SCROLL THROUGH THE DATA SUBSET, TWENTY ELEMENTS  
!    AT A TIME WHEN LUPRT IS INPUT AS "0".  IN THIS CASE, THE EXECUTING 
!    SHELL SCRIPT SHOULD USE THE TERMINAL AS BOTH STANDARD INPUT AND    
!    STANDARD OUTPUT.  INITIALLY, THE FIRST TWENTY ELEMENTS OF THE      
!    CURRENT UNPACKED SUBSET WILL BE DISPLAYED ON THE TERMIMAL,         
!    FOLLOWED BY THE PROMPT "(<enter> for MORE, q <enter> to QUIT)".    
!    IF THE TERMINAL ENTERS ANYTHING OTHER THAN "q" FOLLOWED BY         
!    "<enter>" (e.g., "<enter>"), THE NEXT TWENTY ELEMENTS WILL BE      
!    DISPLAYED, AGAIN FOLLOWED BY THE SAME PROMPT.  THIS CONTINUES      
!    UNTIL EITHER THE ENTIRE SUBSET HAS BEEN DISPLAYED, OR THE TERMINAL 
!    ENTERS "q" FOLLOWED BY "<enter>" AFTER THE PROMPT, IN WHICH CASE   
!    THIS SUBROUTINE STOPS THE SCROLL AND RETURNS TO THE CALLING        
!    PROGRAM (PRESUMABLY TO READ IN THE NEXT SUBSET IN THE BUFR FILE).  
!                                                                       
!    THIS ROUTINE CALLS:        BORT     IBFMS    NEMTAB   READLC       
!                               RJUST    STATUS   UPFTBV                
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
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
                                                                        
      CHARACTER(80) FMT 
      CHARACTER(64) DESC 
      CHARACTER(24) UNIT 
      CHARACTER(20) LCHR 
      CHARACTER(10) NEMO !,TAG 
      CHARACTER(6) NUMB 
      CHARACTER(7) FMTF 
      CHARACTER(8) CVAL, PMISS 
!     CHARACTER(3) TYP 
      CHARACTER(1) TAB, YOU 
      EQUIVALENCE (RVAL, CVAL) 
      REAL(8) RVAL !, VAL 
                                                                        
      PARAMETER (MXFV = 31) 
      INTEGER IFV (MXFV) 
                                                                        
      DATA PMISS / ' MISSING' / 
      DATA YOU / 'Y' / 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (LUPRT.EQ.0) THEN 
         LUOUT = 6 
      ELSE 
         LUOUT = LUPRT 
      ENDIF 
                                                                        
!  CHECK THE FILE STATUS AND I-NODE                                     
!  --------------------------------                                     
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUN) .NE.INV (1, LUN) ) GOTO 903 
                                                                        
      WRITE (LUOUT, * ) 
      WRITE (LUOUT, * ) 'MESSAGE TYPE ', TAG (INODE (LUN) ) 
      WRITE (LUOUT, * ) 
                                                                        
!  DUMP THE CONTENTS OF COMMON /USRINT/ FOR UNIT LUNIT                  
!  ---------------------------------------------------                  
                                                                        
      DO NV = 1, NVAL (LUN) 
      IF (LUPRT.EQ.0.AND.MOD (NV, 20) .EQ.0) THEN 
                                                                        
!  When LUPRT=0, the output will be scrolled, 20 elements at a time     
!  ----------------------------------------------------------------     
                                                                        
         PRINT * , '(<enter> for MORE, q <enter> to QUIT)' 
         READ (5, '(A1)') YOU 
                                                                        
!  If the terminal enters "q" followed by "<enter>" after the prompt    
!  "(<enter> for MORE, q <enter> to QUIT)", scrolling will end and the  
!  subroutine will return to the calling program                        
!  -------------------------------------------------------------------  
                                                                        
         IF (YOU.EQ.'q') THEN 
            PRINT * 
      PRINT * , '==> You have chosen to stop the dumping of this subset' 
            PRINT * 
            GOTO 100 
         ENDIF 
      ENDIF 
                                                                        
      NODE = INV (NV, LUN) 
      NEMO = TAG (NODE) 
      ITYP = ITP (NODE) 
      IF (ITYP.GE.1.AND.ITYP.LE.3) THEN 
         CALL NEMTAB (LUN, NEMO, IDN, TAB, N) 
         NUMB = TABB (N, LUN) (1:6) 
         DESC = TABB (N, LUN) (16:70) 
         UNIT = TABB (N, LUN) (71:94) 
         RVAL = VAL (NV, LUN) 
      ENDIF 
      IF (ITYP.EQ.1) THEN 
                                                                        
!        Delayed descriptor replication factor                          
                                                                        
         FMT = '(7X,A10,2X,I6,1X,A)' 
         WRITE (LUOUT, FMT) NEMO, NINT (RVAL) , 'REPLICATIONS' 
      ELSEIF (ITYP.EQ.2) THEN 
                                                                        
!        Other numeric value                                            
                                                                        
         IF (IBFMS (RVAL) .NE.0) THEN 
                                                                        
!           The value is "missing".                                     
                                                                        
            FMT = '(A6,2X,A10,2X,A20,2X,A24,6X,A48)' 
            WRITE (LUOUT, FMT) NUMB, NEMO, PMISS, UNIT, DESC 
         ELSE 
            FMT = '(A6,2X,A10,2X,F20.00,2X,A24,6X,A48)' 
                                                                        
!           Based upon the corresponding scale factor, select an        
!           appropriate format for the printing of this value.          
                                                                        
            WRITE (FMT (19:20) , '(I2)') MAX (1, ISC (NODE) ) 
            IF (UNIT (1:4) .EQ.'FLAG') THEN 
                                                                        
!              Print a listing of the bits corresponding to             
!              this value.                                              
                                                                        
               CALL UPFTBV (LUNIT, NEMO, RVAL, MXFV, IFV, NIFV) 
               IF (NIFV.GT.0) THEN 
                  UNIT (11:11) = '(' 
                  IPT = 12 
                  DO II = 1, NIFV 
                  IF (IFV (II) .LT.10) THEN 
                     ISZ = 1 
                  ELSE 
                     ISZ = 2 
                  ENDIF 
                  WRITE (FMTF, '(A2,I1,A4)') '(I', ISZ, ',A1)' 
                  IF ( (IPT + ISZ) .LE.24) THEN 
                     WRITE (UNIT (IPT:IPT + ISZ) , FMTF) IFV (II) , ',' 
                     IPT = IPT + ISZ + 1 
                  ENDIF 
                  ENDDO 
                  UNIT (IPT - 1:IPT - 1) = ')' 
               ENDIF 
            ENDIF 
            WRITE (LUOUT, FMT) NUMB, NEMO, RVAL, UNIT, DESC 
         ENDIF 
      ELSEIF (ITYP.EQ.3) THEN 
                                                                        
!        Character (CCITT IA5) value                                    
                                                                        
         NCHR = IBT (NODE) / 8 
         IF (NCHR.GT.8) THEN 
            CALL READLC (LUNIT, LCHR, NEMO) 
         ELSE 
            LCHR = CVAL 
         ENDIF 
         IF (IBFMS (RVAL) .NE.0) LCHR = PMISS 
         IRET = RJUST (LCHR) 
         FMT = '(A6,2X,A10,2X,A20,2X,"(",I2,")",A24,2X,A48)' 
         WRITE (LUOUT, FMT) NUMB, NEMO, LCHR, NCHR, UNIT, DESC 
      ENDIF 
      ENDDO 
                                                                        
      WRITE (LUOUT, 3) 
    3 FORMAT(/' >>> END OF SUBSET <<< '/) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFDUMP - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: UFDUMP - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: UFDUMP - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFDUMP - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
      END SUBROUTINE UFDUMP                         

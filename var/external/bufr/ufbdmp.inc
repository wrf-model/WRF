      SUBROUTINE UFBDMP (LUNIN, LUPRT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBDMP                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE DUMPS A DETAILED PRINT LISTING OF THE       
!   CONTENTS OF THE UNPACKED DATA SUBSET CURRENTLY RESIDING IN THE      
!   INTERNAL ARRAYS ASSOCIATED WITH THE BUFR FILE IN LOGICAL UNIT       
!   ABS(LUNIN).  ABS(LUNIN) MUST HAVE BEEN OPENED FOR INPUT VIA A       
!   PREVIOUS CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF.  THE DATA  
!   SUBSET MUST HAVE BEEN SUBSEQUENTLY READ INTO THE INTERNAL BUFR      
!   ARCHIVE LIBRARY ARRAYS VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE
!   READMG OR READERME, FOLLOWED BY A CALL TO BUFR ARCHIVE LIBRARY      
!   SUBROUTINE READSB (OR VIA A SINGLE CALL TO BUFR ARCHIVE LIBRARY     
!   SUBROUTINE READNS!).  FOR A PARTICULAR SUBSET, THE PRINT LISTING    
!   CONTAINS EACH MNEMONIC ACCOMPANIED BY ITS CORRESPONDING DATA VALUE  
!   (INCLUDING THE ACTUAL BITS THAT WERE SET FOR FLAG TABLE VALUES!)    
!   ALONG WITH OTHER POTENTIALLY USEFUL INFORMATION SUCH AS WHICH OTHER 
!   MNEMONIC(S) THAT MNEMONIC WAS A CONSTITUENT OF WITHIN THE OVERALL   
!   DATA SUBSET.  HOWEVER, THE LISTING ALSO CONTAINS OTHER MORE ESOTERIC
!   INFORMATION SUCH AS BUFR STORAGE CHARACTERISTICS AND A COPY OF THE  
!   JUMP/LINK TABLE USED INTERNALLY WITHIN THE BUFR ARCHIVE LIBRARY     
!   SOFTWARE.  THIS SUBROUTINE IS SIMILAR TO BUFR ARCHIVE LIBRARY       
!   SUBROUTINE UFDUMP, EXCEPT THAT UFDUMP DOES NOT PRINT POINTERS,      
!   COUNTERS AND THE OTHER MORE ESOTERIC INFORMATION DESCRIBING THE     
!   INTERNAL SUBSET STRUCTURES.  EACH SUBROUTINE, UFBDMP AND UFDUMP,    
!   IS USEFUL FOR DIFFERENT DIAGNOSTIC PURPOSES, BUT IN GENERAL UFDUMP  
!   IS MORE USEFUL FOR JUST LOOKING AT THE DATA ELEMENTS.               
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
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR  
!                           FOR INFORMATIONAL PURPOSES; TEST FOR A      
!                           MISSING VALUE NOW ALLOWS SOME FUZZINESS     
!                           ABOUT 10E10 (RATHER THAN TRUE EQUALITY AS   
!                           BEFORE) BECAUSE SOME MISSING VALUES (E.G.,  
!                           CHARACTER STRINGS < 8 CHARACTERS) WERE NOT  
!                           GETTING STAMPED OUT AS "MISSING"; ADDED     
!                           OPTION TO PRINT VALUES USING FORMAT EDIT    
!                           DESCRIPTOR "F15.6" IF LUNIN IS < ZERO,      
!                           IF LUNIN IS > ZERO EDIT DESCRIPTOR EXPANDED 
!                           FROM "G10.3" TO "G15.6" {REGARDLESS OF      
!                           LUNIN, ADDITIONAL VALUES                    
!                           "IB,IS,IR,ND,JP,LK,JB" NOW PRINTED (THEY    
!                           WERE COMMENTED OUT)}                        
! 2004-08-18  J. ATOR    -- MODIFIED FUZZINESS TEST;ADDED READLC OPTION;
!                           RESTRUCTURED SOME LOGIC FOR CLARITY         
! 2006-04-14  D. KEYSER  -- ADD CALL TO UPFTBV FOR FLAG TABLES TO GET   
!                           ACTUAL BITS THAT WERE SET TO GENERATE VALUE 
! 2007-01-19  J. ATOR    -- USE FUNCTION IBFMS                          
!                                                                       
! USAGE:    CALL UFBDMP (LUNIN, LUPRT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER 
!                FOR BUFR FILE                                          
!                  - IF LUNIN IS GREATER THAN ZERO, DATA VALUES ARE     
!                    PRINTED OUT USING FORMAT DATA EDIT DESCRIPTOR      
!                    "G15.6" (all values are printed since output       
!                    format adapts to the magnitude of the data, but    
!                    they are not lined up in columns according to      
!                    decimal point)                                     
!                  - IF LUNIN IS LESS THAN ZERO, DATA VALUES ARE        
!                    PRINTED OUT USING FORMAT DATA EDIT DESCRIPTOR      
!                    "F15.6" {values are lined up in columns according  
!                    to decimal point, but data of large magnitude,     
!                    (i.e., exceeding the format width of 15) get the   
!                    overflow ("***************") print}                
!     LUPRT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR PRINT OUTPUT  
!                FILE                                                   
!                       0 = LUPRT is set to 06 (standard output) and    
!                           the subroutine will scroll the output,      
!                           twenty elements at a time (see REMARKS)     
!                                                                       
!   INPUT FILES:                                                        
!     UNIT 05  - STANDARD INPUT (SEE REMARKS)                           
!                                                                       
!   OUTPUT FILES:                                                       
!     IF LUPRT > 0: UNIT "LUPRT" - PRINT (IF LUPRT=6, STANDARD OUTPUT)  
!     IF LUPRT = 0: UNIT 06      - STANDARD OUTPUT PRINT (SEE REMARKS)  
!                                                                       
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
!    THIS ROUTINE CALLS:        BORT     IBFMS    RJUST    STATUS       
!                               UPFTBV                                  
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
                                                                        
      CHARACTER(20) LCHR 
      CHARACTER(14) BITS 
      CHARACTER(10) TG, TG_RJ !,TAG
      CHARACTER(8) VC 
      CHARACTER(7) FMTF 
      CHARACTER(3) TP !, TYP
      CHARACTER(1) TAB, YOU 
      EQUIVALENCE (VL, VC) 
      REAL(8) VL!, VAL 
                                                                        
      PARAMETER (MXFV = 31) 
      INTEGER IFV (MXFV) 
                                                                        
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
                                                                        
      LUNIT = ABS (LUNIN) 
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUN) .NE.INV (1, LUN) ) GOTO 903 
                                                                        
!  DUMP THE CONTENTS OF COMMON /USRINT/ FOR UNIT ABS(LUNIN)             
!  --------------------------------------------------------             
                                                                        
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
      ND = INV (NV, LUN) 
      VL = VAL (NV, LUN) 
      TG = TAG (ND) 
      TP = TYP (ND) 
      IT = ITP (ND) 
      IB = IBT (ND) 
      IS = ISC (ND) 
      IR = IRF (ND) 
      JP = JUMP (ND) 
      LK = LINK (ND) 
      JB = JMPB (ND) 
      TG_RJ = TG 
      RJ = RJUST (TG_RJ) 
      IF (TP.NE.'CHR') THEN 
      BITS = '              ' 
         IF (IT.EQ.2) THEN 
            CALL NEMTAB (LUN, TG, IDN, TAB, N) 
            IF (TABB (N, LUN) (71:75) .EQ.'FLAG') THEN 
                                                                        
!              Print a listing of the bits corresponding to             
!              this value.                                              
                                                                        
               CALL UPFTBV (LUNIT, TG, VL, MXFV, IFV, NIFV) 
               IF (NIFV.GT.0) THEN 
                  BITS (1:1) = '(' 
                  IPT = 2 
                  DO II = 1, NIFV 
                  IF (IFV (II) .LT.10) THEN 
                     ISZ = 1 
                  ELSE 
                     ISZ = 2 
                  ENDIF 
                  WRITE (FMTF, '(A2,I1,A4)') '(I', ISZ, ',A1)' 
                  IF ( (IPT + ISZ) .LE.14) THEN 
                     WRITE (BITS (IPT:IPT + ISZ) , FMTF) IFV (II) , ',' 
                     IPT = IPT + ISZ + 1 
                  ENDIF 
                  ENDDO 
                  BITS (IPT - 1:IPT - 1) = ')' 
               ENDIF 
            ENDIF 
         ENDIF 
         IF (IBFMS (VL) .NE.0) THEN 
            LCHR = 'MISSING' 
            RJ = RJUST (LCHR) 
            WRITE (LUOUT, 2) NV, TP, IT, TG_RJ, LCHR, IB, IS, IR, ND,   &
            JP, LK, JB                                                  
         ELSE 
            IF (LUNIT.EQ.LUNIN) THEN 
               WRITE (LUOUT, 1) NV, TP, IT, TG_RJ, VL, BITS, IB, IS, IR,&
               ND, JP, LK, JB                                           
            ELSE 
               WRITE (LUOUT, 10) NV, TP, IT, TG_RJ, VL, BITS, IB, IS,   &
               IR, ND, JP, LK, JB                                       
            ENDIF 
         ENDIF 
      ELSE 
         IF (IB.GT.64) THEN 
            CALL READLC (LUNIT, LCHR, TG_RJ) 
         ELSE 
            LCHR = VC 
         ENDIF 
         IF (IBFMS (VL) .NE.0) LCHR = 'MISSING' 
         RJ = RJUST (LCHR) 
         WRITE (LUOUT, 2) NV, TP, IT, TG_RJ, LCHR, IB, IS, IR, ND, JP,  &
         LK, JB                                                         
      ENDIF 
      ENDDO 
                                                                        
      WRITE (LUOUT, 3) 
                                                                        
    1 FORMAT(I5,1X,A3,'-',I1,1X,A10,5X,G15.6,1X,A14,7(1X,I5)) 
   10 FORMAT(I5,1X,A3,'-',I1,1X,A10,5X,F15.6,1X,A14,7(1X,I5)) 
    2 FORMAT(I5,1X,A3,'-',I1,1X,A10,   A20,  15X,   7(1X,I5)) 
    3 FORMAT(/' >>> END OF SUBSET <<< '/) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFBDMP - INPUT BUFR FILE IS CLOSED, IT '//   &
      'MUST BE OPEN FOR INPUT')                                         
  901 CALL BORT ('BUFRLIB: UFBDMP - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: UFBDMP - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBDMP - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
      END SUBROUTINE UFBDMP                         

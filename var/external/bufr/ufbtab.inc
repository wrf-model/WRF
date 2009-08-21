      SUBROUTINE UFBTAB (LUNIN, TAB, I1, I2, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBTAB                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2005-09-16           
!                                                                       
! ABSTRACT: THIS SUBROUTINE EITHER OPENS A BUFR FILE CONNECTED TO       
!   ABS(LUNIN) FOR INPUT OPERATIONS (IF IT IS NOT ALREADY OPENED AS     
!   SUCH), OR SAVES ITS POSITION AND REWINDS IT TO THE FIRST DATA       
!   MESSAGE (IF BUFR FILE ALREADY OPENED), THE EXTENT OF ITS PROCESSING 
!   IS DETERMINED BY THE SIGN OF LUNIN.  IF LUNIN IS GREATER THAN ZERO, 
!   THIS SUBROUTINE READS SPECIFIED VALUES FROM ALL DATA SUBSETS IN THE 
!   BUFR FILE INTO INTERNAL ARRAYS AND RETURNS THESE VALUES ALONG WITH  
!   A COUNT OF THE SUBSETS.  IF LUNIN IS LESS THAN ZERO, IT JUST        
!   RETURNS A COUNT OF THE SUBSETS.  FINALLY, THIS SUBROUTINE EITHER    
!   CLOSES THE BUFR FILE IN ABS(LUNIN) (IF IT WAS OPENED HERE) OR       
!   RESTORES IT TO ITS PREVIOUS READ/WRITE STATUS AND POSITION (IF IT   
!   WAS NOT OPENED HERE).  THE DATA VALUES CORRESPOND TO MNEMONICS,     
!   NORMALLY WHERE THERE IS NO REPLICATION (THERE CAN BE REGULAR OR     
!   DELAYED REPLICATION, BUT THIS SUBROUTINE WILL ONLY READ THE FIRST   
!   OCCURRENCE OF THE MNEMONIC IN EACH SUBSET).  UFBTAB PROVIDES A      
!   MECHANISM WHEREBY A USER CAN DO A QUICK SCAN OF THE RANGE OF VALUES 
!   CORRESPONDING TO ONE OR MORE MNEMNONICS AMONGST ALL DATA SUBSETS    
!   FOR AN ENTIRE BUFR FILE; NO OTHER BUFR ARCHIVE LIBRARY ROUTINES     
!   HAVE TO BE CALLED.  THIS SUBROUTINE IS SIMILAR TO BUFR ARCHIVE      
!   LIBRARY SUBROUTINE UFBTAM EXCEPT UFBTAM READS SUBSETS FROM MESSAGES 
!   STORED IN INTERNAL MEMORY AND IT CURRENTLY CANNOT READ DATA FROM    
!   COMPRESSED BUFR MESSAGES.  UFBTAB CAN READ DATA FROM BOTH           
!   UNCOMPRESSED AND COMPRESSED BUFR MESSAGES.                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY                
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-  
!                           LINING CODE WITH FPP DIRECTIVES             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THERE ARE TOO    
!                           MANY SUBSETS COMING IN (I.E., .GT. "I2"),   
!                           BUT RATHER JUST PROCESS "I2" REPORTS AND    
!                           PRINT A DIAGNOSTIC; MAXJL (MAXIMUM NUMBER   
!                           OF JUMP/LINK ENTRIES) INCREASED FROM 15000  
!                           TO 16000 (WAS IN VERIFICATION VERSION);     
!                           MODIFIED TO CALL ROUTINE REWNBF WHEN THE    
!                           BUFR FILE IS ALREADY OPENED, ALLOWS         
!                           SPECIFIC SUBSET INFORMATION TO BE READ FROM 
!                           A FILE IN THE MIDST OF ITS BEING READ FROM  
!                           OR WRITTEN TO), BEFORE OPENBF WAS ALWAYS    
!                           CALLED AND THIS WOULD HAVE LED TO AN ABORT  
!                           OF THE APPLICATION PROGRAM (WAS IN          
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY)                                    
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-09-16  J. WOOLLEN -- WORKS FOR COMPRESSED BUFR MESSAGES; ADDED   
!                           OPTION TO RETURN ONLY SUBSET COUNT (WHEN    
!                           INPUT UNIT NUMBER IS LESS THAN ZERO)        
! 2006-04-14  J. ATOR    -- ADD DECLARATION FOR CREF                    
! 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR 
!                                                                       
! USAGE:    CALL UFBTAB (LUNIN, TAB, I1, I2, IRET, STR)                 
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER 
!                FOR BUFR FILE                                          
!                  - IF LUNIN IS LESS THAN ZERO, UFBTAB WILL JUST       
!                    RETURN, WITHIN IRET, THE NUMBER OF SUBSETS IN      
!                    THE BUFR FILE                                      
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF TAB OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE .GE. LATTER)                                   
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF TAB             
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF TAB                                       
!                  - THERE ARE THREE "GENERIC" MNEMONICS NOT RELATED    
!                     TO TABLE B, THESE RETURN THE FOLLOWING            
!                     INFORMATION IN CORRESPONDING TAB LOCATION:        
!                     'NUL'  WHICH ALWAYS RETURNS MISSING (10E10)       
!                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR      
!                            MESSAGE (RECORD) NUMBER IN WHICH THIS      
!                            SUBSET RESIDES                             
!                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET    
!                            NUMBER OF THIS SUBSET WITHIN THE BUFR      
!                            MESSAGE (RECORD) NUMBER 'IREC'             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     TAB      - REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES READ   
!                FROM BUFR FILE                                         
!                  - THIS IS RETURNED AS MISSING IF LUNIN IS LESS THAN  
!                    ZERO                                               
!     IRET     - INTEGER: NUMBER OF DATA SUBSETS IN BUFR FILE (MUST BE  
!                NO LARGER THAN I2 IF LUNIN IS GREATER THAN ZERO)       
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM CAN BE CALLED PRIOR TO THIS TO STORE THE BUFR     
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     CLOSBF   IREADMG  IREADSB      
!                               MESGBC   NMSUB    OPENBF   PARSTR       
!                               REWNBF   STATUS   STRING   UPB          
!                               UPBB     UPC      USRTPL                
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
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
      COMMON / ACMODE / IAC 
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(128) BORT_STR 
      CHARACTER(40) CREF 
      CHARACTER(10) TGS (100) !, TAG 
      CHARACTER(8) SUBSET, CVAL 
!     CHARACTER(3) TYP 
      EQUIVALENCE (CVAL, RVAL) 
      LOGICAL OPENIT, JUST_COUNT 
      REAL(8) TAB (I1, I2), RVAL, UPS, TEN !,VAL 
                                                                        
      DATA MAXTG / 100 / 
      DATA TEN / 10 / 
                                                                        
!-----------------------------------------------------------------------
      MPS (NODE) = 2** (IBT (NODE) ) - 1 
      LPS (LBIT) = MAX (2** (LBIT) - 1, 1) 
      UPS (NODE) = (IVAL + IRF (NODE) ) * TEN** ( - ISC (NODE) ) 
!-----------------------------------------------------------------------
                                                                        
!  SET COUNTERS TO ZERO                                                 
!  --------------------                                                 
                                                                        
      IRET = 0 
      IREC = 0 
      ISUB = 0 
      IACC = IAC 
                                                                        
!  CHECK FOR COUNT SUBSET ONLY OPTION INDICATED BY NEGATIVE UNIT        
!  -------------------------------------------------------------        
                                                                        
      LUNIT = ABS (LUNIN) 
      JUST_COUNT = LUNIN.LT.LUNIT 
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      OPENIT = IL.EQ.0 
                                                                        
      IF (OPENIT) THEN 
                                                                        
!  OPEN BUFR FILE CONNECTED TO UNIT LUNIT IF IT IS NOT ALREADY OPEN     
!  ----------------------------------------------------------------     
                                                                        
         CALL OPENBF (LUNIT, 'IN', LUNIT) 
      ELSE 
                                                                        
!  IF BUFR FILE ALREADY OPENED, SAVE POSITION & REWIND TO FIRST DATA MSG
!  ---------------------------------------------------------------------
                                                                        
         CALL REWNBF (LUNIT, 0) 
      ENDIF 
                                                                        
      IAC = 1 
                                                                        
!  SET THE OUTPUT ARRAY TO MISSING VALUES                               
!  --------------------------------------                               
                                                                        
      DO J = 1, I2 
      DO I = 1, I1 
      TAB (I, J) = BMISS 
      ENDDO 
      ENDDO 
                                                                        
!  HERE FOR COUNT ONLY OPTION                                           
!  --------------------------                                           
                                                                        
      IF (JUST_COUNT) THEN 
         DO WHILE (IREADMG (LUNIT, SUBSET, IDATE(1)) .EQ.0) 
         IRET = IRET + NMSUB (LUNIT) 
         ENDDO 
         GOTO 25 
      ENDIF 
                                                                        
!  CHECK FOR SPECIAL TAGS IN STRING                                     
!  --------------------------------                                     
                                                                        
      CALL PARSTR (STR, TGS, MAXTG, NTG, ' ', .TRUE.) 
      DO I = 1, NTG 
      IF (TGS (I) .EQ.'IREC') IREC = I 
      IF (TGS (I) .EQ.'ISUB') ISUB = I 
      ENDDO 
                                                                        
!  READ A MESSAGE AND PARSE A STRING                                    
!  ---------------------------------                                    
                                                                        
   10 IF (IREADMG (LUNIT, SUBSET, JDATE) .NE.0) GOTO 25 
      CALL STRING (STR, LUN, I1, 0) 
      IF (IREC.GT.0) NODS (IREC) = 0 
      IF (ISUB.GT.0) NODS (ISUB) = 0 
                                                                        
!  PARSE THE MESSAGE DEPENDING ON WHETHER COMPRESSED OR NOT             
!  --------------------------------------------------------             
                                                                        
      CALL MESGBC ( - LUNIT, MTYP, ICMP) 
      IF (ICMP.EQ.0) THEN 
         GOTO 15 
      ELSEIF (ICMP.EQ.1) then 
         GOTO 115 
      ELSE 
         GOTO 900 
      ENDIF 
                                                                        
!  ---------------------------------------------                        
!  THIS BRANCH IS FOR UNCOMPRESSED MESSAGES                             
!  ---------------------------------------------                        
!  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE                        
!  ---------------------------------------------                        
                                                                        
   15 IF (NSUB (LUN) .EQ.MSUB (LUN) ) GOTO 10 
      IF (IRET + 1.GT.I2) GOTO 99 
      IRET = IRET + 1 
                                                                        
      DO I = 1, NNOD 
      NODS (I) = ABS (NODS (I) ) 
      ENDDO 
                                                                        
!  PARSE THE STRING NODES FROM A SUBSET                                 
!  ------------------------------------                                 
                                                                        
      MBIT = MBYT (LUN) * 8 + 16 
      NBIT = 0 
      N = 1 
      CALL USRTPL (LUN, N, N) 
   20 IF (N + 1.LE.NVAL (LUN) ) THEN 
         N = N + 1 
         NODE = INV (N, LUN) 
         MBIT = MBIT + NBIT 
         NBIT = IBT (NODE) 
         IF (ITP (NODE) .EQ.1) THEN 
            CALL UPBB (IVAL, NBIT, MBIT, MBAY (1, LUN) ) 
            CALL USRTPL (LUN, N, IVAL) 
         ENDIF 
         DO I = 1, NNOD 
         IF (NODS (I) .EQ.NODE) THEN 
            IF (ITP (NODE) .EQ.1) THEN 
               CALL UPBB (IVAL, NBIT, MBIT, MBAY (1, LUN) ) 
               TAB (I, IRET) = IVAL 
            ELSEIF (ITP (NODE) .EQ.2) THEN 
               CALL UPBB (IVAL, NBIT, MBIT, MBAY (1, LUN) ) 
               IF (IVAL.LT.MPS (NODE) ) TAB (I, IRET) = UPS (NODE) 
            ELSEIF (ITP (NODE) .EQ.3) THEN 
               CVAL = ' ' 
               KBIT = MBIT 
               CALL UPC (CVAL, NBIT / 8, MBAY (1, LUN), KBIT) 
               TAB (I, IRET) = RVAL 
            ENDIF 
            NODS (I) = - NODS (I) 
            GOTO 20 
         ENDIF 
         ENDDO 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) GOTO 20 
         ENDDO 
      ENDIF 
                                                                        
!  UPDATE THE SUBSET POINTERS BEFORE NEXT READ                          
!  -------------------------------------------                          
                                                                        
      IBIT = MBYT (LUN) * 8 
      CALL UPB (NBYT, 16, MBAY (1, LUN), IBIT) 
      MBYT (LUN) = MBYT (LUN) + NBYT 
      NSUB (LUN) = NSUB (LUN) + 1 
      IF (IREC.GT.0) TAB (IREC, IRET) = NMSG (LUN) 
      IF (ISUB.GT.0) TAB (ISUB, IRET) = NSUB (LUN) 
      GOTO 15 
                                                                        
!  ---------------------------------------------                        
!  THIS BRANCH IS FOR COMPRESSED MESSAGES                               
!  ---------------------------------------------                        
!  STORE ANY MESSAGE AND/OR SUBSET COUNTERS                             
!  ---------------------------------------------                        
                                                                        
!  CHECK ARRAY BOUNDS                                                   
!  ------------------                                                   
                                                                        
  115 IF (IRET + MSUB (LUN) .GT.I2) GOTO 99 
                                                                        
!  STORE MESG/SUBS TOKENS                                               
!  ----------------------                                               
                                                                        
      IF (IREC.GT.0.OR.ISUB.GT.0) THEN 
         DO NSB = 1, MSUB (LUN) 
         IF (IREC.GT.0) TAB (IREC, IRET + NSB) = NMSG (LUN) 
         IF (ISUB.GT.0) TAB (ISUB, IRET + NSB) = NSB 
         ENDDO 
      ENDIF 
                                                                        
!  SETUP A NEW SUBSET TEMPLATE, PREPARE TO SUB-SURF                     
!  ------------------------------------------------                     
                                                                        
      CALL USRTPL (LUN, 1, 1) 
      IBIT = MBYT (LUN) 
      N = 0 
                                                                        
!  UNCOMPRESS CHOSEN NODES INTO THE TAB ARRAY (FIRST OCCURANCES ONLY)   
!  ------------------------------------------------------------------   
                                                                        
!  READ ELEMENTS LOOP                                                   
!  ------------------                                                   
                                                                        
  120 DO N = N + 1, NVAL (LUN) 
      NODE = INV (N, LUN) 
      NBIT = IBT (NODE) 
      ITYP = ITP (NODE) 
                                                                        
!  FIRST TIME IN RESET NODE INDEXES, OR CHECK FOR NODE(S) STILL NEEDED  
!  -------------------------------------------------------------------  
                                                                        
      IF (N.EQ.1) THEN 
         DO I = 1, NNOD 
         NODS (I) = ABS (NODS (I) ) 
         ENDDO 
      ELSE 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) GOTO 125 
         ENDDO 
         GOTO 135 
      ENDIF 
                                                                        
!  FIND THE EXTENT OF THE NEXT SUB-GROUP                                
!  -------------------------------------                                
                                                                        
  125 IF (ITYP.EQ.1.OR.ITYP.EQ.2) THEN 
         CALL UPB (LREF, NBIT, MBAY (1, LUN), IBIT) 
         CALL UPB (LINC, 6, MBAY (1, LUN), IBIT) 
         NIBIT = IBIT + LINC * MSUB (LUN) 
      ELSEIF (ITYP.EQ.3) THEN 
         CALL UPC (CREF, NBIT / 8, MBAY (1, LUN), IBIT) 
         CALL UPB (LINC, 6, MBAY (1, LUN), IBIT) 
         NIBIT = IBIT + 8 * LINC * MSUB (LUN) 
      ELSE 
         GOTO 120 
      ENDIF 
                                                                        
!  LOOP OVER STRING NODES                                               
!  ----------------------                                               
                                                                        
      DO I = 1, NNOD 
                                                                        
!  CHOSEN NODES LOOP - KEEP TRACK OF NODES NEEDED AND NODES FOUND       
!  --------------------------------------------------------------       
                                                                        
      IF (NODE.NE.NODS (I) ) GOTO 130 
      NODS (I) = - NODS (I) 
      LRET = IRET 
                                                                        
!  PROCESS A FOUND NODE INTO TAB                                        
!  -----------------------------                                        
                                                                        
      IF (ITYP.EQ.1.OR.ITYP.EQ.2) THEN 
         DO NSB = 1, MSUB (LUN) 
         JBIT = IBIT + LINC * (NSB - 1) 
         CALL UPB (NINC, LINC, MBAY (1, LUN), JBIT) 
         IVAL = LREF + NINC 
         LRET = LRET + 1 
         IF (NINC.LT.LPS (LINC) ) TAB (I, LRET) = UPS (NODE) 
         ENDDO 
      ELSEIF (ITYP.EQ.3) THEN 
         DO NSB = 1, MSUB (LUN) 
         JBIT = IBIT + LINC * (NSB - 1) * 8 
         CALL UPC (CVAL, LINC, MBAY (1, LUN), JBIT) 
         LRET = LRET + 1 
         TAB (I, LRET) = RVAL 
         ENDDO 
      ELSE 
         CALL BORT ('UFBTAB - INVALID ELEMENT TYPE SPECIFIED') 
      ENDIF 
                                                                        
!  END OF LOOPS FOR COMPRESSED MESSAGE PARSING                          
!  -------------------------------------------                          
                                                                        
  130 CONTINUE 
      ENDDO 
      IF (ITYP.EQ.1) CALL USRTPL (LUN, N, IVAL) 
      IBIT = NIBIT 
                                                                        
!  END OF READ ELEMENTS LOOP                                            
!  -------------------------                                            
                                                                        
      ENDDO 
  135 IRET = IRET + MSUB (LUN) 
                                                                        
!  END OF MESSAGE PARSING - GO BACK FOR ANOTHER                         
!  --------------------------------------------                         
                                                                        
      GOTO 10 
                                                                        
!  -------------------------------------------                          
!  ERROR PROCESSING AND EXIT ROUTES BELOW                               
!  -------------------------------------------                          
!  EMERGENCY ROOM TREATMENT FOR ARRAY OVERFLOW                          
!  -------------------------------------------                          
                                                                        
   99 NREP = IRET 
      DO WHILE (IREADSB (LUNIT) .EQ.0) 
      NREP = NREP + 1 
      ENDDO 
      DO WHILE (IREADMG (LUNIT, SUBSET, JDATE) .EQ.0) 
      NREP = NREP + NMSUB (LUNIT) 
      ENDDO 
      IF (IPRT.GE.0) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBTAB - THE NO. OF DATA SUBSETS IN THE BUFR',&
     & ' FILE IS .GT. LIMIT OF ', I2, ' IN THE 4-TH ARG. (INPUT) - ', 'I&
     &NCOMPLETE READ'                                                   
         PRINT * , '>>>UFBTAB STORED ', IRET, ' REPORTS OUT OF ', NREP, &
         '<<<'                                                          
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * 
      ENDIF 
                                                                        
                                                                        
   25 IF (OPENIT) THEN 
                                                                        
!  CLOSE BUFR FILE IF IT WAS OPENED HERE                                
!  -------------------------------------                                
                                                                        
         CALL CLOSBF (LUNIT) 
      ELSE 
                                                                        
!  RESTORE BUFR FILE TO PREV. STATUS & POSITION IF NOT ORIG. OPENED HERE
!  ---------------------------------------------------------------------
                                                                        
         CALL REWNBF (LUNIT, 1) 
      ENDIF 
                                                                        
      IAC = IACC 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UFBTAB - INVALID COMPRESSION '//     &
      'INDICATOR (ICMP=",I3," RETURNED FROM BUFR ARCHIVE LIBRARY '//    &
      'ROUTINE MESGBF")') ICMP                                          
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBTAB                         

      SUBROUTINE CKTABA (LUN, SUBSET, JDATE, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CKTABA                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2000-09-19           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PARSES THE TABLE A MNEMONIC AND THE DATE    
!   OUT OF SECTION 1 OF A BUFR MESSAGE PREVIOUSLY READ FROM UNIT LUNIT  
!   USING BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR EQUIVALENT (AND NOW 
!   STORED IN THE INTERNAL MESSAGE BUFFER, ARRAY MBAY IN COMMON BLOCK   
!   /BITBUF/).  THE TABLE A MNEMONIC IS ASSOCIATED WITH THE BUFR        
!   MESSAGE TYPE/SUBTYPE IN SECTION 1.  IT ALSO FILLS IN THE MESSAGE    
!   CONTROL WORD PARTITION ARRAYS IN COMMON BLOCK /MSGCWD/.             
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR - CONSOLIDATED MESSAGE      
!                           DECODING LOGIC THAT HAD BEEN REPLICATED IN  
!                           READMG, READFT, READERME, RDMEMM AND READIBM
!                           (CKTABA IS NOW CALLED BY THESE CODES);      
!                           LOGIC ENHANCED HERE TO ALLOW COMPRESSED AND 
!                           STANDARD BUFR MESSAGES TO BE READ           
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THE SECTION 1    
!                           MESSAGE SUBTYPE DOES NOT AGREE WITH THE     
!                           SECTION 1 MESSAGE SUBTYPE IN THE DICTIONARY 
!                           IF THE MESSAGE TYPE MNEMONIC IS NOT OF THE  
!                           FORM "NCtttsss", WHERE ttt IS THE BUFR TYPE 
!                           AND sss IS THE BUFR SUBTYPE (E.G., IN       
!                           "PREPBUFR" FILES); MODIFIED DATE            
!                           CALCULATIONS TO NO LONGER USE FLOATING      
!                           POINT ARITHMETIC SINCE THIS CAN LEAD TO     
!                           ROUND OFF ERROR AND AN IMPROPER RESULTING   
!                           DATE ON SOME MACHINES (E.G., NCEP IBM       
!                           FROST/SNOW), INCREASES PORTABILITY;         
!                           UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY OR UNUSUAL THINGS     
!                           HAPPEN; SUBSET DEFINED AS "        " IF     
!                           IRET RETURNED AS 11 (BEFORE WAS UNDEFINED)  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE IUPBS01, IGETDATE AND GETLENS           
! 2006-04-14  J. ATOR    -- ALLOW "FRtttsss" AND "FNtttsss" AS POSSIBLE 
!                           TABLE A MNEMONICS, WHERE ttt IS THE BUFR    
!                           TYPE AND sss IS THE BUFR SUBTYPE            
!                                                                       
! USAGE:    CALL CKTABA (LUN, SUBSET, JDATE, IRET)                      
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING CHECKED                                          
!              "        " = IRET equal to 11 (see IRET below)           
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING CHECKED, IN FORMAT OF EITHER YYMMDDHH OR 
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = unrecognized Table A (message type) value   
!                      11 = this is a BUFR table (dictionary) message   
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     DIGIT    GETLENS  I4DY         
!                               IGETDATE IUPB     IUPBS01  NEMTBAX      
!                               NUMTAB   OPENBT   RDUSDX                
!    THIS ROUTINE IS CALLED BY: RDMEMM   READERME READMG   READMM       
!                               Normally not called by any application  
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
      COMMON / PADESC / IBCT, IPD1, IPD2, IPD3, IPD4 
      COMMON / UNPTYP / MSGUNP (NFILES) 
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
      CHARACTER(2) CPFX (3) 
      CHARACTER(1) TAB 
      LOGICAL TRYBT
                                                                        
      DATA CPFX / 'NC', 'FR', 'FN' / 
      DATA NCPFX / 3 / 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      TRYBT = .TRUE. 
                                                                        
      JDATE = IGETDATE (MBAY (1, LUN), IYR, IMO, IDY, IHR) 
                                                                        
!  .... Message type                                                    
      MTYP = IUPBS01 (MBAY (1, LUN) , 'MTYP') 
!  .... Message subtype                                                 
      MSBT = IUPBS01 (MBAY (1, LUN) , 'MSBT') 
                                                                        
!  DON'T PARSE BUFR TABLE (DICTIONARY) MESSAGES                         
!  --------------------------------------------                         
                                                                        
      IF (MTYP.EQ.11) THEN 
      IF (IPRT.GE.2) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: CKTABA - TABLE A VALUE IS 11 (THIS IS A ',    &
      'BUFR TABLE DICTIONARY MESSAGE) - RETURN WITH IRET = 11'          
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  Return with IRET=1 and SUBSET all blanks                             
!  ----------------------------------------                             
                                                                        
      IRET = 11 
      SUBSET = "" 
      GOTO 100 
      ENDIF 
                                                                        
!  PARSE SECTION 3                                                      
!  ---------------                                                      
                                                                        
      CALL GETLENS (MBAY (1, LUN), 3, LEN0, LEN1, LEN2, LEN3, L4, L5) 
                                                                        
      IAD3 = LEN0 + LEN1 + LEN2 
                                                                        
!  .... Number of subsets in msg                                        
      JSUB = IUPB (MBAY (1, LUN), IAD3 + 5, 16) 
!  .... Compression indicator                                           
      NCMP = IUPB (MBAY (1, LUN), IAD3 + 7, 8) 
!  .... First descriptor (integer)                                      
      KSUB = IUPB (MBAY (1, LUN), IAD3 + 8, 16) 
!  .... Second descriptor (integer)                                     
      ISUB = IUPB (MBAY (1, LUN), IAD3 + 10, 16) 
                                                                        
!  LOCATE SECTION 4                                                     
!  ----------------                                                     
                                                                        
      IAD4 = IAD3 + LEN3 
                                                                        
!  NOW, TRY TO GET "SUBSET" (MNEMONIC ASSOCIATED WITH TABLE A) FROM MSG 
!  -------------------------------------------------------------------- 
                                                                        
!  IF ISUB FROM SECTION 3 DEFINES TABLE A THEN MSGUNP=0                 
!  ----------------------------------------------------                 
                                                                        
!  .... get SUBSET from ISUB                                            
    5 CALL NUMTAB (LUN, ISUB, SUBSET, TAB, ITAB) 
!  .... is SUBSET from Tbl A?                                           
      CALL NEMTBAX (LUN, SUBSET, MTY1, MSB1, INOD) 
      IF (INOD.GT.0) THEN 
!  .... yes it is                                                       
      MBYT (LUN) = (IAD4 + 4) 
      MSGUNP (LUN) = 0 
      GOTO 10 
      ENDIF 
                                                                        
!  IF KSUB FROM SECTION 3 DEFINES TABLE A THEN MSGUNP=1 (standard)      
!  ---------------------------------------------------------------      
                                                                        
!  .... get SUBSET from KSUB                                            
      CALL NUMTAB (LUN, KSUB, SUBSET, TAB, ITAB) 
!  .... is SUBSET from Tbl A?                                           
      CALL NEMTBAX (LUN, SUBSET, MTY1, MSB1, INOD) 
      IF (INOD.GT.0) THEN 
!  .... yes it is                                                       
      MBYT (LUN) = 8 * (IAD4 + 4) 
      MSGUNP (LUN) = 1 
      GOTO 10 
      ENDIF 
                                                                        
!  OKAY, STILL NO "SUBSET", LETS MAKE IT "NCtttsss" (where ttt=MTYP     
!  and sss=MSBT) AND SEE IF IT DEFINES TABLE A.  IF NOT, THEN ALSO      
!  TRY "FRtttsss" AND "FNtttsss".                                       
!  ----------------------------------------------------------------     
                                                                        
      II = 1 
      DO WHILE (II.LE.NCPFX) 
      WRITE (SUBSET, '(A2,2I3.3)') CPFX (II) , MTYP, MSBT 
!  ....    is SUBSET from Tbl A?                                        
      CALL NEMTBAX (LUN, SUBSET, MTY1, MSB1, INOD) 
      IF (INOD.GT.0) THEN 
!  ....     yes it is                                                   
      IF (KSUB.EQ.IBCT) THEN 
      MBYT (LUN) = (IAD4 + 4) 
      MSGUNP (LUN) = 0 
      ELSE 
      MBYT (LUN) = 8 * (IAD4 + 4) 
      MSGUNP (LUN) = 1 
      ENDIF 
      GOTO 10 
      ENDIF 
      II = II + 1 
      ENDDO 
                                                                        
!  NOW WE HAVE A GENERATED "SUBSET", BUT IT STILL DOES NOT DEFINE       
!  TABLE A - MAKE ONE LAST DESPERATE ATTEMPT - SEE IF AN EXTERNAL       
!  USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT IS DEFINED   
!  IN OPENBT (ONLY POSSIBLE IF APPLICATION PROGRAM HAS AN IN-LINE       
!  OPENBT OVERRIDING THE ONE IN THE BUFR ARCHIVE LIBRARY)               
!  ------------------------------------------------------------------   
                                                                        
      IF (TRYBT) THEN 
      TRYBT = .FALSE. 
      IF (IPRT.GE.1) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: CKTABA - LAST RESORT, CHECK FOR EXTERNAL', ' B&
     &UFR TABLE VIA CALL TO IN-LINE OPENBT'                             
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
      CALL OPENBT (LUNDX, MTYP) 
      IF (LUNDX.GT.0) THEN 
!  .... Good news, there is a unit (LUNDX) connected to a table file,   
!  .... so store the table internally                                   
      CALL RDUSDX (LUNDX, LUN) 
      GOTO 5 
      ENDIF 
      ENDIF 
                                                                        
!  IF ALL ATTEMPTS TO DEFINE TABLE A FAIL SKIP GIVE UP                  
!  ---------------------------------------------------                  
                                                                        
      IF (IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: CKTABA - UNRECOGNIZED TABLE A MESSAGE TYPE (',&
      SUBSET, ') - RETURN WITH IRET = -1'                               
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * 
      ENDIF 
      IRET = - 1 
      GOTO 100 
                                                                        
!  CHECK THE VALIDITY OF THE MTYP/MSBT AND FOR COMPRESSION (MSGUNP=2)   
!  ------------------------------------------------------------------   
                                                                        
   10 CONTINUE 
      IF (MTYP.NE.MTY1) GOTO 900 
      IF (MSBT.NE.MSB1.AND.DIGIT (SUBSET (3:8) ) ) GOTO 901 
      IF (IAND (NCMP, 64) .GT.0) MSGUNP (LUN) = 2 
                                                                        
!  SET THE OTHER REQUIRED PARAMETERS IN MESSAGE CONTROL WORD PARTITION  
!  -------------------------------------------------------------------  
                                                                        
!  .... Date for this message                                           
      IDATE (LUN) = I4DY (JDATE) 
!  .... Number of messages so far in file                               
      NMSG (LUN) = NMSG (LUN) + 1 
!  .... Positional index of Table A mnem.                               
      INODE (LUN) = INOD 
!  .... Number of subsets in this message                               
      MSUB (LUN) = JSUB 
!  .... Number of subsets actually read so far                          
      NSUB (LUN) = 0 
                                                                        
!  .... Return successfully                                             
      IRET = 0 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: CKTABA - MESSAGE TYPE MISMATCH '//'(S&
     &UBSET=",A8,", MTYP=",I3,", MTY1=",I3)') SUBSET, MTYP, MTY1        
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: CKTABA - MESSAGE SUBTYPE MISMATCH '//&
      '(SUBSET=",A8,", MSBT=",I3,", MSB1=",I3)') SUBSET, MSBT, MSB1     
      CALL BORT (BORT_STR) 
      END SUBROUTINE CKTABA                         

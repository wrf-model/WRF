      SUBROUTINE UFBEVN (LUNIT, USR, I1, I2, I3, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBEVN                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS SPECIFIED VALUES FROM THE CURRENT     
!   BUFR DATA SUBSET WITHIN INTERNAL ARRAYS.  THE DATA VALUES           
!   CORRESPOND TO MNEMONICS WHICH ARE PART OF A MULTIPLE-REPLICATION    
!   SEQUENCE WITHIN ANOTHER MULTIPLE-REPLICATION SEQUENCE.  THE INNER   
!   SEQUENCE IS USUALLY ASSOCIATED WITH DATA "LEVELS" AND THE OUTER     
!   SEQUENCE WITH DATA "EVENTS".  THE BUFR FILE IN LOGICAL UNIT LUNIT   
!   MUST HAVE BEEN OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR ARCHIVE 
!   LIBRARY SUBROUTINE OPENBF.   IN ADDITION, THE DATA SUBSET MUST HAVE 
!   SUBSEQUENTLY BEEN READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY       
!   ARRAYS VIA CALLS TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR       
!   READERME FOLLOWED BY A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE      
!   READSB (OR VIA A SINGLE CALL TO BUFR ARCHIVE LIBRARY                
!   SUBROUTINE READNS).  OTHER THAN THE ADDITION OF A THIRD             
!   DIMENSION AND THE READ ONLY RESTRICTION, THE CONTEXT AND USAGE OF   
!   UFBEVN IS EXACTLY THE SAME AS FOR BUFR ARCHIVE LIBRARY SUBROUTINES  
!   UFBINT, UFBREP AND UFBSEQ.  THIS SUBROUTINE IS DESIGNED TO READ     
!   EVENT INFORMATION FROM "PREPBUFR" TYPE BUFR FILES.  PREPBUFR FILES  
!   HAVE THE FOLLOWING BUFR TABLE EVENT STRUCTURE (NOTE SIXTEEN         
!   CHARACTERS HAVE BEEN REMOVED FROM THE LAST COLUMN TO ALLOW THE      
!   TABLE TO FIT IN THIS DOCBLOCK):                                     
!                                                                       
!   | ADPUPA   | HEADR  {PLEVL}                                    |    
!   | HEADR    | SID  XOB  YOB  DHR  ELV  TYP  T29  TSB  ITP  SQN  |    
!   | PLEVL    | CAT <PINFO> <QINFO> <TINFO> <ZINFO> <WINFO>       |    
!   | PINFO    | [PEVN]             <PBACKG>  <PPOSTP>             |    
!   | QINFO    | [QEVN]    TDO      <QBACKG>  <QPOSTP>             |    
!   | TINFO    | [TEVN]    TVO      <TBACKG>  <TPOSTP>             |    
!   | ZINFO    | [ZEVN]             <ZBACKG>  <ZPOSTP>             |    
!   | WINFO    | [WEVN]             <WBACKG>  <WPOSTP>             |    
!   | PEVN     | POB  PQM  PPC  PRC                                |    
!   | QEVN     | QOB  QQM  QPC  QRC                                |    
!   | TEVN     | TOB  TQM  TPC  TRC                                |    
!   | ZEVN     | ZOB  ZQM  ZPC  ZRC                                |    
!   | WEVN     | UOB  WQM  WPC  WRC  VOB                           |    
!   | PBACKG   | POE  PFC                                          |    
!   | QBACKG   | QOE  QFC                                          |    
!   | TBACKG   | TOE  TFC                                          |    
!   | ZBACKG   | ZOE  ZFC                                          |    
!   | WBACKG   | WOE  UFC  VFC                                     |    
!   | PPOSTP   | PAN                                               |    
!   | QPOSTP   | QAN                                               |    
!   | TPOSTP   | TAN                                               |    
!   | ZPOSTP   | ZAN                                               |    
!   | WPOSTP   | UAN  VAN                                          |    
!                                                                       
!   NOTE THAT THE EIGHT-BIT DELAYED REPLIATION EVENT SEQUENCES "[xxxx]" 
!   ARE NESTED INSIDE ONE-BIT DELAYED REPLICATED SEQUENCES "<yyyy>".    
!   THE ANALOGOUS BUFR ARCHIVE LIBRARY SUBROUTINE UFBIN3 DOES NOT WORK  
!   PROPERLY ON THIS TYPE OF EVENT STRUCTURE.  IT WORKS ONLY ON THE     
!   EVENT STRUCTURE FOUND IN "PREPFITS" TYPE BUFR FILES (SEE UFBIN3 FOR 
!   MORE DETAILS).  IN TURN, UFBEVN DOES NOT WORK PROPERLY ON THE EVENT 
!   STRUCTURE FOUND IN PREPFITS FILES (ALWAYS USE UFBIN3 IN THIS CASE). 
!   ONE OTHER DIFFERENCE BETWEEN UFBEVN AND UFBIN3 IS THAT UFBEVN       
!   STORES THE MAXIMUM NUMBER OF EVENTS FOUND FOR ALL DATA VALUES       
!   SPECIFIED AMONGST ALL LEVELS RETURNED INTERNALLY IN COMMON BLOCK    
!   /UFBN3C/.  UFBIN3 RETURNS THIS VALUE AS AN ADDITIONAL OUTPUT        
!   ARGUMENT.                                                           
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; IMPROVED MACHINE            
!                           PORTABILITY                                 
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  J. WOOLLEN -- SAVES THE MAXIMUM NUMBER OF EVENTS FOUND    
!                           FOR ALL DATA VALUES SPECIFIED AMONGST ALL   
!                           LEVELS RETURNED AS VARIABLE MAXEVN IN NEW   
!                           COMMON BLOCK /UFBN3C/                       
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); ADDED CALL TO BORT   
!                           IF BUFR FILE IS OPEN FOR OUTPUT; UNIFIED/   
!                           PORTABLE FOR WRF; ADDED DOCUMENTATION       
!                           (INCLUDING HISTORY); OUTPUTS MORE COMPLETE  
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY OR UNUSUAL THINGS HAPPEN         
!                                                                       
! USAGE:    CALL UFBEVN (LUNIT, USR, I1, I2, I3, IRET, STR)             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE .GE. LATTER)                                   
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     I3       - INTEGER: LENGTH OF THIRD DIMENSION OF USR (MAXIMUM     
!                VALUE IS 255)                                          
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF USR                                       
!                  - THERE ARE THREE "GENERIC" MNEMONICS NOT RELATED    
!                     TO TABLE B, THESE RETURN THE FOLLOWING            
!                     INFORMATION IN CORRESPONDING USR LOCATION:        
!                     'NUL'  WHICH ALWAYS RETURNS MISSING (10E10)       
!                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR      
!                            MESSAGE (RECORD) NUMBER IN WHICH THIS      
!                            SUBSET RESIDES                             
!                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET    
!                            NUMBER OF THIS SUBSET WITHIN THE BUFR      
!                            MESSAGE (RECORD) NUMBER 'IREC'             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - REAL*8: (I1,I2,I3) STARTING ADDRESS OF DATA VALUES     
!                READ FROM DATA SUBSET                                  
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES READ FROM   
!                DATA SUBSET (MUST BE NO LARGER THAN I2)                
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    APPLICATION PROGRAMS READING PREPFITS FILES SHOULD NOT CALL THIS   
!    ROUTINE.                                                           
!                                                                       
!    THIS ROUTINE CALLS:        BORT     CONWIN   GETWIN   NVNWIN       
!                               NXTWIN   STATUS   STRING                
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
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
      COMMON / UFBN3C / MAXEVN 
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER ( * ) STR 
      DIMENSION INVN (255) 
      REAL(8) USR (I1, I2, I3) !, VAL
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      MAXEVN = 0 
      IRET = 0 
                                                                        
!  CHECK THE FILE STATUS AND I-NODE                                     
!  --------------------------------                                     
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUN) .NE.INV (1, LUN) ) GOTO 903 
                                                                        
      IF (I1.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBEVN - THIRD ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH SIXTH ARGUMENT (IRET) = 0'                         
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I2.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBEVN - FOURTH ARGUMENT (INPUT) IS .LE. 0', '&
     & -  RETURN WITH SIXTH ARGUMENT (IRET) = 0'                        
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I3.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBEVN - FIFTH ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH SIXTH ARGUMENT (IRET) = 0'                         
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ENDIF 
                                                                        
!  PARSE OR RECALL THE INPUT STRING                                     
!  --------------------------------                                     
                                                                        
      CALL STRING (STR, LUN, I1, 0) 
                                                                        
!  INITIALIZE USR ARRAY                                                 
!  --------------------                                                 
                                                                        
      DO K = 1, I3 
      DO J = 1, I2 
      DO I = 1, I1 
      USR (I, J, K) = BMISS 
      ENDDO 
      ENDDO 
      ENDDO 
                                                                        
!  LOOP OVER COND WINDOWS                                               
!  ----------------------                                               
                                                                        
      INC1 = 1 
      INC2 = 1 
                                                                        
    1 CALL CONWIN (LUN, INC1, INC2, I2) 
      IF (NNOD.EQ.0) THEN 
         IRET = I2 
         GOTO 100 
      ELSEIF (INC1.EQ.0) THEN 
         GOTO 100 
      ELSE 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) THEN 
            INS2 = INC1 
            CALL GETWIN (NODS (I), LUN, INS1, INS2) 
            IF (INS1.EQ.0) GOTO 100 
            GOTO 2 
         ENDIF 
         ENDDO 
         INS1 = INC1 
         INS2 = INC2 
      ENDIF 
                                                                        
!  READ PUSH DOWN STACK DATA INTO 3D ARRAYS                             
!  ----------------------------------------                             
                                                                        
    2 IRET = IRET + 1 
      IF (IRET.LE.I2) THEN 
         DO I = 1, NNOD 
         IF (NODS (I) .GT.0) THEN 
            NNVN = NVNWIN (NODS (I), LUN, INS1, INS2, INVN, I3) 
            MAXEVN = MAX (NNVN, MAXEVN) 
            DO N = 1, NNVN 
            USR (I, IRET, N) = VAL (INVN (N), LUN) 
            ENDDO 
         ENDIF 
         ENDDO 
      ENDIF 
                                                                        
!  DECIDE WHAT TO DO NEXT                                               
!  ----------------------                                               
                                                                        
      CALL NXTWIN (LUN, INS1, INS2) 
      IF (INS1.GT.0.AND.INS1.LT.INC2) GOTO 2 
      IF (NCON.GT.0) GOTO 1 
                                                                        
      IF (IRET.EQ.0) THEN 
         IF (IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBEVN - NO SPECIFIED VALUES READ IN - ', 'RET&
     &URN WITH SIXTH ARGUMENT (IRET) = 0'                               
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFBEVN - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: UFBEVN - INPUT BUFR FILE IS OPEN FOR OUTPUT'/&
     &/', IT MUST BE OPEN FOR INPUT')                                   
  902 CALL BORT ('BUFRLIB: UFBEVN - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBEVN - LOCATION OF INTERNAL TABLE FOR '//  &
      'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//     &
      'INTERNAL SUBSET ARRAY')                                          
      END SUBROUTINE UFBEVN                         

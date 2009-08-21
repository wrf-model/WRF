      SUBROUTINE UFBTAM (TAB, I1, I2, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBTAM                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS SPECIFIED VALUES INTO INTERNAL ARRAYS 
!   FROM ALL DATA SUBSETS IN BUFR MESSAGES STORED IN INTERNAL MEMORY.   
!   THE DATA VALUES CORRESPOND TO MNEMONICS, NORMALLY WHERE THERE IS NO 
!   REPLICATION (THERE CAN BE REGULAR OR DELAYED REPLICATION, BUT THIS  
!   SUBROUTINE WILL ONLY READ THE FIRST OCCURRENCE OF THE MNEMONIC IN   
!   EACH SUBSET).  UFBTAM PROVIDES A MECHANISM WHEREBY A USER CAN DO A  
!   QUICK SCAN OF THE RANGE OF VALUES CORRESPONDING TO ONE OR MORE      
!   MNEMNONICS AMONGST ALL DATA SUBSETS FOR A GROUP OF BUFR MESSAGES    
!   STORED IN INTERNAL MEMORY, NO OTHER BUFR ARCHIVE LIBRARY ROUTINES   
!   HAVE TO BE CALLED.  THIS SUBROUTINE IS SIMILAR TO BUFR ARCHIVE      
!   LIBRARY SUBROUTINE UFBTAB EXCEPT UFBTAB READS SUBSETS FROM MESSAGES 
!   IN A PHYSICAL BUFR FILE.  UFBTAM CURRENTLY CANNOT READ DATA FROM    
!   COMPRESSED BUFR MESSAGES.                                           
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-  
!                           LINING CODE WITH FPP DIRECTIVES             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2001-08-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 8 MBYTES TO  
!                           16 MBYTES; MODIFIED TO NOT ABORT WHEN THERE 
!                           ARE TOO MANY SUBSETS COMING IN (I.E., .GT.  
!                           I2), BUT RATHER JUST PROCESS I2 REPORTS AND 
!                           PRINT A DIAGNOSTIC                          
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- PARAMETER MAXMSG (THE MAXIMUM NUMBER OF     
!                           BUFR MESSAGES WHICH CAN BE STORED           
!                           INTERNALLY) INCREASED FROM 50000 TO 200000; 
!                           MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2004-11-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 16 MBYTES TO 
!                           50 MBYTES                                   
! 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR 
!                                                                       
! USAGE:    CALL UFBTAM (TAB, I1, I2, IRET, STR)                        
!   INPUT ARGUMENT LIST:                                                
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
!     IRET     - INTEGER: NUMBER OF DATA SUBSETS IN BUFR FILE (MUST BE  
!                NO LARGER THAN I2)                                     
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM IS CALLED PRIOR TO THIS TO STORE THE BUFR         
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     NMSUB    PARSTR   RDMEMM       
!                               STATUS   STRING   UPB      UPBB         
!                               UPC      USRTPL                         
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
                                                                        
!     COMMON / MSGMEM / MUNIT, MLAST, MSGP (0:MAXMSG), MSGS (MAXMEM) 
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), VALS (10),    &
      KONS (10)                                                         
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(128) BORT_STR 
      CHARACTER(10) TGS (100) !, TAG 
      CHARACTER(8) SUBSET, CVAL 
!     CHARACTER(3) TYP 
      EQUIVALENCE (CVAL, RVAL) 
      REAL(8) TAB (I1, I2), RVAL, UPS, TEN !, VAL
                                                                        
      DATA MAXTG / 100 / 
      DATA TEN / 10 / 
                                                                        
!-----------------------------------------------------------------------
      MPS (NODE) = 2** (IBT (NODE) ) - 1 
      UPS (NODE) = (IVAL + IRF (NODE) ) * TEN** ( - ISC (NODE) ) 
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
                                                                        
      IF (MSGP (0) .EQ.0) GOTO 100 
                                                                        
      DO J = 1, I2 
      DO I = 1, I1 
      TAB (I, J) = BMISS 
      ENDDO 
      ENDDO 
                                                                        
!  CHECK FOR SPECIAL TAGS IN STRING                                     
!  --------------------------------                                     
                                                                        
      CALL PARSTR (STR, TGS, MAXTG, NTG, ' ', .TRUE.) 
      IREC = 0 
      ISUB = 0 
      DO I = 1, NTG 
      IF (TGS (I) .EQ.'IREC') IREC = I 
      IF (TGS (I) .EQ.'ISUB') ISUB = I 
      ENDDO 
                                                                        
!  READ A MESSAGE AND PARSE A STRING                                    
!  ---------------------------------                                    
                                                                        
      CALL STATUS (MUNIT, LUN, IL, IM) 
                                                                        
      DO IMSG = 1, MSGP (0) 
      CALL RDMEMM (IMSG, SUBSET, JDATE, MRET) 
      IF (MRET.NE.0) GOTO 900 
                                                                        
      CALL STRING (STR, LUN, I1, 0) 
      IF (IREC.GT.0) NODS (IREC) = 0 
      IF (ISUB.GT.0) NODS (ISUB) = 0 
                                                                        
!  PROCESS ALL THE SUBSETS IN THE MEMORY MESSAGE                        
!  ---------------------------------------------                        
                                                                        
      DO WHILE (NSUB (LUN) .LT.MSUB (LUN) ) 
      IF (IRET + 1.GT.I2) GOTO 99 
      IRET = IRET + 1 
                                                                        
      DO I = 1, NNOD 
      NODS (I) = ABS (NODS (I) ) 
      ENDDO 
                                                                        
      CALL USRTPL (LUN, 1, 1) 
      MBIT = MBYT (LUN) * 8 + 16 
      NBIT = 0 
      N = 1 
                                                                        
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
      ENDDO 
                                                                        
      ENDDO 
                                                                        
      GOTO 200 
                                                                        
!  EMERGENCY ROOM TREATMENT FOR ARRAY OVERFLOW                          
!  -------------------------------------------                          
                                                                        
   99 CALL RDMEMM (0, SUBSET, JDATE, MRET) 
      NREP = 0 
      DO IMSG = 1, MSGP (0) 
      CALL RDMEMM (IMSG, SUBSET, JDATE, MRET) 
      IF (MRET.NE.0) GOTO 900 
      NREP = NREP + NMSUB (MUNIT) 
      ENDDO 
      IF (IPRT.GE.0) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBTAM - THE NO. OF DATA SUBSETS IN MEMORY IS &
     &', '.GT. LIMIT OF ', I2, ' IN THE 3-RD ARG. (INPUT) - INCOMPLETE R&
     &EAD'                                                              
         PRINT * , '>>>UFBTAM STORED ', IRET, ' REPORTS OUT OF ', NREP, &
         '<<<'                                                          
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * 
      ENDIF 
                                                                        
!  RESET THE MEMORY FILE                                                
!  ---------------------                                                
                                                                        
  200 CALL RDMEMM (0, SUBSET, JDATE, MRET) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UFBTAM - HIT END-OF-FILE READING '// &
      'MESSAGE NUMBER",I5," IN INTERNAL MEMORY")') IMSG                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBTAM                         

      SUBROUTINE WRITLC (LUNIT, CHR, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    WRITLC                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PACKS A CHARACTER DATA ELEMENT ASSOCIATED   
!   WITH A PARTICULAR SUBSET MNEMONIC FROM THE INTERNAL MESSAGE BUFFER  
!   (ARRAY MBAY IN COMMON BLOCK /BITBUF/).  IT IS DESIGNED TO BE USED   
!   TO STORE CHARACTER ELEMENTS GREATER THAN THE USUAL LENGTH OF EIGHT  
!   BYTES.                                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE GETLENS                                 
! 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR 
!                                                                       
! USAGE:    CALL WRITLC (LUNIT, CHR, STR)                               
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     CHR      - CHARACTER*(*): UNPACKED CHARACTER STRING (I.E.,        
!                CHARACTER DATA ELEMENT GREATER THAN EIGHT BYTES)       
!     STR      - CHARACTER*(*): STRING (I.E., MNEMONIC)                 
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     GETLENS  PARSTR   PKC          
!                               STATUS   UPB      UPBB     USRTPL       
!    THIS ROUTINE IS CALLED BY: None (currently)                        
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
      CHARACTER ( * ) CHR, STR 
      CHARACTER(128) BORT_STR 
      CHARACTER(10) TGS (100) !, TAG 
      CHARACTER(8) CTAG 
!     CHARACTER(3) TYP 
!     REAL(8) VAL 
                                                                        
      DATA MAXTG / 100 / 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.LT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
!  CHECK FOR TAGS (MNEMONICS) IN INPUT STRING (THERE CAN ONLY BE ONE)   
!  ------------------------------------------------------------------   
                                                                        
      CALL PARSTR (STR, TGS, MAXTG, NTG, ' ', .TRUE.) 
      IF (NTG.GT.1) GOTO 903 
      CTAG = TGS (1) 
                                                                        
!  CHECK THAT THE INPUT TAG IS A CHARACTER STRING                       
!  ----------------------------------------------                       
                                                                        
      INOD = INODE (LUN) 
      DO NOD = INOD, ISC (INOD) 
      IF (CTAG.EQ.TAG (NOD) ) GOTO 1 
      ENDDO 
      GOTO 904 
    1 IF (TYP (NOD) .NE.'CHR') GOTO 905 
                                                                        
!  LOCATE THE BEGINNING OF THE DATA IN SECTION 4 (MBYTE)                
!  -----------------------------------------------------                
                                                                        
      CALL GETLENS (MBAY (1, LUN), 3, LEN0, LEN1, LEN2, LEN3, L4, L5) 
                                                                        
      MBYTE = LEN0 + LEN1 + LEN2 + LEN3 + 4 
      NSUBS = 1 
                                                                        
!  FIND THE MOST RECENTLY WRITTEN SUBSET IN THE MESSAGE                 
!  ----------------------------------------------------                 
                                                                        
      DO WHILE (NSUBS.LT.NSUB (LUN) ) 
      IBIT = MBYTE * 8 
      CALL UPB (NBYT, 16, MBAY (1, LUN), IBIT) 
      MBYTE = MBYTE+NBYT 
      NSUBS = NSUBS + 1 
      ENDDO 
                                                                        
      IF (NSUBS.NE.NSUB (LUN) ) GOTO 906 
                                                                        
!  LOCATE THE STRING ELEMENT TO WRITE                                   
!  ----------------------------------                                   
                                                                        
      MBIT = MBYTE * 8 + 16 
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
            GOTO 20 
         ENDIF 
         IF (NOD.EQ.NODE) THEN 
            IF (ITP (NODE) .EQ.3) THEN 
               NCHR = NBIT / 8 
               IBIT = MBIT 
               DO N = 1, NCHR 
               CALL PKC (' ', 1, MBAY (1, LUN) , IBIT) 
               ENDDO 
               CALL PKC (CHR, NCHR, MBAY (1, LUN), MBIT) 
               CALL USRTPL (LUN, 1, 1) 
               GOTO 100 
            ENDIF 
         ENDIF 
         GOTO 20 
      ENDIF 
      GOTO 907 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  901 CALL BORT ('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  902 CALL BORT ('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  903 WRITE (BORT_STR, '("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '/&
     &/' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4'//&
     &',")")') STR, NTG                                                 
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: WRITLC - MNEMONIC ",A," NOT LOCATED '&
     &//'IN REPORT SUBSET")') CTAG                                      
      CALL BORT (BORT_STR) 
  905 WRITE (BORT_STR, '("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT '// &
      'REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') CTAG, TYP (NOD)    
      CALL BORT (BORT_STR) 
  906 WRITE (BORT_STR, '("BUFRLIB: WRITLC - THE MOST RECENTLY WRITTEN '/&
     &/' SUBSET NO. (",I3,") IN MSG .NE. THE STORED VALUE FOR THE NO.'//&
     &' OF SUBSETS (",I3,") IN MSG")') NSUBS, NSUB (LUN)                
      CALL BORT (BORT_STR) 
  907 WRITE (BORT_STR, '("BUFRLB: WRITLC - UNABLE TO FIND ",A," IN '//  &
      'SUBSET")') CTAG                                                  
      CALL BORT (BORT_STR) 
      END SUBROUTINE WRITLC                         

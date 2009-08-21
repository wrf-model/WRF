      SUBROUTINE READLC (LUNIT, CHR, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    READLC                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS A CHARACTER DATA ELEMENT ASSOCIATED 
!   WITH A PARTICULAR SUBSET MNEMONIC FROM THE INTERNAL MESSAGE BUFFER  
!   (ARRAY MBAY IN COMMON BLOCK /BITBUF/).  IT IS DESIGNED TO BE USED   
!   TO RETURN CHARACTER ELEMENTS GREATER THAN THE USUAL LENGTH OF EIGHT 
!   BYTES.  IT CURRENTLY WILL NOT WORK FOR COMPRESSED BUFR MESSAGES.    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY OR UNUSUAL THINGS HAPPEN         
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR 
!                                                                       
! USAGE:    CALL READLC (LUNIT, CHR, STR)                               
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     STR      - CHARACTER*(*): STRING (I.E., MNEMONIC)                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     CHR      - CHARACTER*(*): UNPACKED CHARACTER STRING (I.E.,        
!                CHARACTER DATA ELEMENT GREATER THAN EIGHT BYTES)       
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     PARSTR   STATUS   UPC          
!    THIS ROUTINE IS CALLED BY: UFDUMP                                  
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
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRBIT / NBIT (MAXJL), MBIT (MAXJL) 
      COMMON / UNPTYP / MSGUNP (NFILES) 
      COMMON / QUIET / IPRT 
                                                                        
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
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
!  CHECK FOR TAGS (MNEMONICS) IN INPUT STRING (THERE CAN ONLY BE ONE)   
!  ------------------------------------------------------------------   
                                                                        
      CALL PARSTR (STR, TGS, MAXTG, NTG, ' ', .TRUE.) 
      IF (NTG.GT.1) GOTO 903 
      CTAG = TGS (1) 
      CHR = ' ' 
                                                                        
!  FIND THE TAG IN THE SUBSET OR RETURN A BLANK STRING                  
!  ---------------------------------------------------                  
                                                                        
      DO N = 1, NVAL (LUN) 
      NOD = INV (N, LUN) 
      IF (CTAG.EQ.TAG (NOD) ) GOTO 1 
      ENDDO 
      IF (IPRT.GE.0) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: READLC - MNEMONIC ', CTAG, ' NOT LOCATED IN ',&
     & 'REPORT SUBSET - RETURN WITH BLANK STRING FOR CHARACTER ', 'DATA &
     &ELEMENT'                                                          
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
         PRINT * 
      ENDIF 
      GOTO 100 
    1 IF (ITP (NOD) .NE.3) GOTO 904 
                                                                        
!  DECIPHER THE LONG CHARACTER                                          
!  ---------------------------                                          
                                                                        
      IF (MSGUNP (LUN) .EQ.0.OR.MSGUNP (LUN) .EQ.1) THEN 
         NCHR = NBIT (N) / 8 
         KBIT = MBIT (N) 
         CALL UPC (CHR, NCHR, MBAY (1, LUN), KBIT) 
      ELSEIF (MSGUNP (LUN) .EQ.2) THEN 
!  .... compressed message                                              
         GOTO 905 
      ELSE 
         GOTO 906 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: READLC - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: READLC - INPUT BUFR FILE IS OPEN FOR '//     &
      'OUTPUT, IT MUST BE OPEN FOR INPUT')                              
  902 CALL BORT ('BUFRLIB: READLC - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
  903 WRITE (BORT_STR, '("BUFRLIB: READLC - THERE CANNOT BE MORE THAN '/&
     &/'ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE ",'//'I&
     &3,")")') STR, NTG                                                 
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: READLC - MNEMONIC ",A," DOES NOT '// &
      'REPRESENT A CHARACTER ELEMENT (ITP=",I2,")")') CTAG, ITP (NOD)   
      CALL BORT (BORT_STR) 
  905 CALL BORT ('BUFRLIB: READLC - NOT ENABLED FOR COMPRESSED BUFR '// &
      'MESSAGES')                                                       
  906 WRITE (BORT_STR, '("BUFRLIB: READLC - MESSAGE UNPACK TYPE",I3,'// &
      '" IS NOT RECOGNIZED")') MSGUNP                                   
      CALL BORT (BORT_STR) 
      END SUBROUTINE READLC                         

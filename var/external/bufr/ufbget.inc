      SUBROUTINE UFBGET (LUNIT, TAB, I1, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBGET                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE UNPACKS AND RETURNS THE VALUES FOR ONE-     
!   DIMENSIONAL DESCRIPTORS IN THE INPUT STRING WITHOUT ADVANCING THE   
!   SUBSET POINTER.                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; IMPROVED MACHINE            
!                           PORTABILITY                                 
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
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL UFBGET (LUNIT, TAB, I1, IRET, STR)                     
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     I1       - INTEGER: LENGTH OF TAB                                 
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH THE WORDS  
!                IN THE ARRAY TAB                                       
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
!     TAB      - REAL*8: (I1) STARTING ADDRESS OF DATA VALUES READ FROM 
!                DATA SUBSET                                            
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = there are no more subsets in the BUFR       
!                           message                                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     INVWIN   STATUS   STRING       
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
      COMMON / USRBIT / NBIT (MAXJL), MBIT (MAXJL) 
                                                                        
      CHARACTER ( * ) STR 
!     CHARACTER(10) TAG 
      CHARACTER(8) CVAL 
!     CHARACTER(3) TYP 
      EQUIVALENCE (CVAL, RVAL) 
      REAL(8) RVAL, TAB (I1), UPS, TEN !, VAL
                                                                        
      DATA TEN / 10 / 
                                                                        
!-----------------------------------------------------------------------
      MPS (NODE) = 2** (IBT (NODE) ) - 1 
      UPS (NODE) = (IVAL + IRF (NODE) ) * TEN** ( - ISC (NODE) ) 
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
                                                                        
      DO I = 1, I1 
      TAB (I) = BMISS 
      ENDDO 
                                                                        
!  MAKE SURE A FILE/MESSAGE IS OPEN FOR INPUT                           
!  ------------------------------------------                           
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
!  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE                        
!  ---------------------------------------------                        
                                                                        
      IF (NSUB (LUN) .EQ.MSUB (LUN) ) THEN 
         IRET = - 1 
         GOTO 100 
      ENDIF 
                                                                        
!  PARSE THE STRING                                                     
!  ----------------                                                     
                                                                        
      CALL STRING (STR, LUN, I1, 0) 
                                                                        
!  EXPAND THE TEMPLATE FOR THIS SUBSET AS LITTLE AS POSSIBLE            
!  ---------------------------------------------------------            
                                                                        
      N = 1 
      NBIT (N) = 0 
      MBIT (N) = MBYT (LUN) * 8 + 16 
      CALL USRTPL (LUN, N, N) 
                                                                        
   10 DO N = N + 1, NVAL (LUN) 
      NODE = INV (N, LUN) 
      NBIT (N) = IBT (NODE) 
      MBIT (N) = MBIT (N - 1) + NBIT (N - 1) 
      IF (NODE.EQ.NODS (NNOD) ) THEN 
         NVAL (LUN) = N 
         GOTO 20 
      ELSEIF (ITP (NODE) .EQ.1) THEN 
         CALL UPBB (IVAL, NBIT (N), MBIT (N), MBAY (1, LUN) ) 
         CALL USRTPL (LUN, N, IVAL) 
         GOTO 10 
      ENDIF 
      ENDDO 
   20 CONTINUE 
                                                                        
!  UNPACK ONLY THE NODES FOUND IN THE STRING                            
!  -----------------------------------------                            
                                                                        
      DO I = 1, NNOD 
      NODE = NODS (I) 
      INVN = INVWIN (NODE, LUN, 1, NVAL (LUN) ) 
      IF (INVN.GT.0) THEN 
         CALL UPBB (IVAL, NBIT (INVN), MBIT (INVN), MBAY (1, LUN) ) 
         IF (ITP (NODE) .EQ.1) THEN 
            TAB (I) = IVAL 
         ELSEIF (ITP (NODE) .EQ.2) THEN 
            IF (IVAL.LT.MPS (NODE) ) TAB (I) = UPS (NODE) 
         ELSEIF (ITP (NODE) .EQ.3) THEN 
            CVAL = ' ' 
            KBIT = MBIT (INVN) 
            CALL UPC (CVAL, NBIT (INVN) / 8, MBAY (1, LUN), KBIT) 
            TAB (I) = RVAL 
         ENDIF 
      ELSE 
         TAB (I) = BMISS 
      ENDIF 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFBGET - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: UFBGET - INPUT BUFR FILE IS OPEN FOR OUTPUT'/&
     &/', IT MUST BE OPEN FOR INPUT')                                   
  902 CALL BORT ('BUFRLIB: UFBGET - A MESSAGE MUST BE OPEN IN INPUT '// &
      'BUFR FILE, NONE ARE')                                            
      END SUBROUTINE UFBGET                         

      SUBROUTINE MAKESTAB 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MAKESTAB                                               
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CONSTRUCTS AN INTERNAL JUMP/LINK TABLE      
!  WITHIN COMMON BLOCK /TABLES/, USING THE INFORMATION WITHIN THE       
!  INTERNAL BUFR TABLE ARRAYS (WITHIN COMMON BLOCK /TABABD/) FOR ALL OF 
!  THE LUN (I.E., I/O STREAM INDEX) VALUES THAT ARE CURRENTLY DEFINED TO
!  THE BUFR ARCHIVE LIBRARY SOFTWARE.  NOTE THAT THE ENTIRE JUMP/LINK   
!  TABLE WILL ALWAYS BE COMPLETELY RECONSTRUCTED FROM SCRATCH, EVEN IF  
!  SOME OF THE INFORMATION WITHIN THE INTERNAL BUFR TABLE ARRAYS        
!  ALREADY EXISTED THERE AT THE TIME OF THE PREVIOUS CALL TO THIS       
!  SUBROUTINE, BECAUSE THERE MAY HAVE BEEN OTHER EVENTS THAT HAVE TAKEN 
!  PLACE SINCE THE PREVIOUS CALL TO THIS SUBROUTINE THAT HAVE NOT YET   
!  BEEN REFLECTED WITHIN THE INTERNAL JUMP/LINK TABLE, SUCH AS, E.G.    
!  THE UNLINKING OF AN LUN VALUE FROM THE BUFR ARCHIVE LIBRARY SOFTWARE 
!  VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE CLOSBF.                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE   
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES      
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED HISTORY DOCUMENTATION; OUTPUTS   
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY; NOW ALLOWS FOR THE   
!                           POSSIBILITY THAT A CONNECTED FILE MAY NOT   
!                           CONTAIN ANY DICTIONARY TABLE INFO (E.G.,    
!                           AN EMPTY FILE), SUBSEQUENT CONNECTED FILES  
!                           WHICH ARE NOT EMPTY WILL NO LONGER GET      
!                           TRIPPED UP BY THIS (THIS AVOIDS THE NEED    
!                           FOR AN APPLICATION PROGRAM TO DISCONNECT    
!                           ANY EMPTY FILES VIA A CALL TO CLOSBF)       
!                                                                       
! USAGE:    CALL MAKESTAB                                               
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CHEKSTAB STRCLN   TABSUB       
!    THIS ROUTINE IS CALLED BY: RDBFDX   RDUSDX                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / QUIET / IPRT 
      COMMON / STBFR / IOLUN (NFILES), IOMSG (NFILES) 
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(128) BORT_STR 
!     CHARACTER(10) TAG 
      CHARACTER(8) NEMO 
!     CHARACTER(3) TYP 
      DIMENSION LUS (NFILES) 
      LOGICAL EXPAND 
!     REAL(8) VAL 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  RESET POINTER TABLE AND STRING CACHE                                 
!  ------------------------------------                                 
                                                                        
      NTAB = 0 
      CALL STRCLN 
                                                                        
!  FIGURE OUT WHICH UNITS SHARE TABLES                                  
!  -----------------------------------                                  
                                                                        
!     First, determine how many LUN values are currently being used and,
!     for each such one, whether it uses the same dictionary table      
!     information as any other LUN values that we have examined so far. 
!     If so, then set LUS(LUN) to a nonzero value.                      
                                                                        
!     Note that, for each LUN value, the MTAB(*,LUN) array contains     
!     pointer indices into the internal jump/link table for each of the 
!     Table A mnemonics that is currently defined for that LUN value.   
!     Thus, the code within the following DO loop is simply checking    
!     whether the first Table A mnemonic is the same for two different  
!     LUN values as the determination of whether those LUN values indeed
!     share the same dictionary tables.                                 
                                                                        
      DO LUN = 1, NFILES 
      LUS (LUN) = 0 
      IF (IOLUN (LUN) .NE.0) THEN 
      IF (LUN.GT.1) THEN 
      DO LUM = 1, LUN - 1 
!ccccccc IF(MTAB(1,LUN).EQ.MTAB(1,LUM)) LUS(LUN) = LUM                  
      IF (MTAB (1, LUN) .EQ.MTAB (1, LUM) .AND.MTAB (1, LUM) .NE.0) LUS &
      (LUN) = LUM                                                       
      ENDDO 
      ENDIF 
      ENDIF 
      ENDDO 
                                                                        
!  INITIALIZE JUMP/LINK TABLES WITH SUBSETS/SEQUENCES/ELEMENTS          
!  -----------------------------------------------------------          
                                                                        
      DO LUN = 1, NFILES 
                                                                        
!cccc IF(IOLUN(LUN).NE.0) THEN                                          
      IF (IOLUN (LUN) .NE.0.AND.NTBA (LUN) .GT.0) THEN 
                                                                        
!  RESET ANY EXISTING INVENTORY POINTERS                                
!  -------------------------------------                                
                                                                        
      IF (IOMSG (LUN) .NE.0) THEN 
      IF (LUS (LUN) .EQ.0) INC = (NTAB + 1) - MTAB (1, LUN) 
      IF (LUS (LUN) .NE.0) INC = MTAB (1, LUS (LUN) ) - MTAB (1, LUN) 
      DO N = 1, NVAL (LUN) 
      INV (N, LUN) = INV (N, LUN) + INC 
      ENDDO 
      ENDIF 
                                                                        
!  CREATE NEW TABLE ENTRIES IF THIS UNIT DOESN'T SHARE EXISTING ONES    
!  -----------------------------------------------------------------    
                                                                        
      IF (LUS (LUN) .EQ.0) THEN 
                                                                        
!     The dictionary table information corresponding to this LUN        
!     has not yet been written into the internal jump/link table,       
!     so add it in now.                                                 
                                                                        
      CALL CHEKSTAB (LUN) 
      DO ITBA = 1, NTBA (LUN) 
      INOD = NTAB + 1 
      NEMO = TABA (ITBA, LUN) (4:11) 
      CALL TABSUB (LUN, NEMO) 
      MTAB (ITBA, LUN) = INOD 
      ISC (INOD) = NTAB 
                                                                        
!**** note that the following lines are commented out****               
!ccc        DO N1=INOD,ISC(INOD)-1                                      
!ccc        DO N2=N1+1,ISC(INOD)                                        
!ccc        IF(TAG(N1).EQ.TAG(N2)) GOTO 900                             
!ccc        ENDDO                                                       
!ccc        ENDDO                                                       
!********************************************************               
                                                                        
      ENDDO 
      ENDIF 
                                                                        
      ENDIF 
      ENDDO 
                                                                        
!  STORE TYPES AND INITIAL VALUES AND COUNTS                            
!  -----------------------------------------                            
                                                                        
      DO NODE = 1, NTAB 
      IF (TYP (NODE) .EQ.'SUB') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 1 
      ITP (NODE) = 0 
      ELSEIF (TYP (NODE) .EQ.'SEQ') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 1 
      ITP (NODE) = 0 
      ELSEIF (TYP (NODE) .EQ.'RPC') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 0 
      ITP (NODE) = 0 
      ELSEIF (TYP (NODE) .EQ.'RPS') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 0 
      ITP (NODE) = 0 
      ELSEIF (TYP (NODE) .EQ.'REP') THEN 
!     VALI (NODE) = BMISS 
      VALI (NODE) = 2147483647
      KNTI (NODE) = IRF (NODE) 
      ITP (NODE) = 0 
      ELSEIF (TYP (NODE) .EQ.'DRS') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 1 
      ITP (NODE) = 1 
      ELSEIF (TYP (NODE) .EQ.'DRP') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 1 
      ITP (NODE) = 1 
      ELSEIF (TYP (NODE) .EQ.'DRB') THEN 
      VALI (NODE) = 0 
      KNTI (NODE) = 0 
      ITP (NODE) = 1 
      ELSEIF (TYP (NODE) .EQ.'NUM') THEN 
!     VALI (NODE) = BMISS 
      VALI (NODE) = 2147483647 
      KNTI (NODE) = 1 
      ITP (NODE) = 2 
      ELSEIF (TYP (NODE) .EQ.'CHR') THEN 
!     VALI (NODE) = BMISS 
      VALI (NODE) = 2147483647 
      KNTI (NODE) = 1 
      ITP (NODE) = 3 
      ELSE 
      GOTO 901 
      ENDIF 
      ENDDO 
                                                                        
!  SET UP EXPANSION SEGMENTS FOR TYPE 'SUB', 'DRP', AND 'DRS' NODES     
!  ----------------------------------------------------------------     
                                                                        
      NEWN = 0 
                                                                        
      DO N = 1, NTAB 
      ISEQ (N, 1) = 0 
      ISEQ (N, 2) = 0 
      EXPAND = TYP (N) .EQ.'SUB'.OR.TYP (N) .EQ.'DRP'.OR.TYP (N)        &
      .EQ.'DRS'.OR.TYP (N) .EQ.'REP'.OR.TYP (N) .EQ.'DRB'               
      IF (EXPAND) THEN 
      ISEQ (N, 1) = NEWN + 1 
      NODA = N 
      NODE = N + 1 
      DO K = 1, MAXJL 
      KNT (K) = 0 
      ENDDO 
      IF (TYP (NODA) .EQ.'REP') KNT (NODE) = KNTI (NODA) 
      IF (TYP (NODA) .NE.'REP') KNT (NODE) = 1 
                                                                        
    1 NEWN = NEWN + 1 
      IF (NEWN.GT.MAXJL) GOTO 902 
      JSEQ (NEWN) = NODE 
      KNT (NODE) = MAX (KNTI (NODE), KNT (NODE) ) 
    2 IF (JUMP (NODE) * KNT (NODE) .GT.0) THEN 
      NODE = JUMP (NODE) 
      GOTO 1 
      ELSEIF (LINK (NODE) .GT.0) THEN 
      NODE = LINK (NODE) 
      GOTO 1 
      ELSE 
      NODE = JMPB (NODE) 
      IF (NODE.EQ.NODA) GOTO 3 
      IF (NODE.EQ.0) GOTO 903 
      KNT (NODE) = MAX (KNT (NODE) - 1, 0) 
      GOTO 2 
      ENDIF 
    3 ISEQ (N, 2) = NEWN 
      ENDIF 
      ENDDO 
                                                                        
!  PRINT THE SEQUENCE TABLES                                            
!  ------------------------                                             
                                                                        
      IF (IPRT.GE.2) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      DO I = 1, NTAB 
      PRINT 99, I, TAG (I), TYP (I), JMPB (I), JUMP (I), LINK (I),      &
      IBT (I), IRF (I), ISC (I)                                         
      ENDDO 
      PRINT * 
   99 FORMAT   ('BUFRLIB: MAKESTAB ',I5,2X,A10,A5,6I8) 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: MAKESTAB - MNEMONIC ",A," IS '//     &
      'DUPLICATED IN SUBSET: ",A)') NEMO, TAG (N1)                      
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: MAKESTAB - UNKNOWN TYPE ",A)') TYP ( &
      NODE)                                                             
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: MAKESTAB - NUMBER OF JSEQ ENTRIES IN'&
     &//' JUMP/LINK TABLE EXCEEDS THE LIMIT (",I6,")")') MAXJL          
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: MAKESTAB - NODE IS ZERO, FAILED TO '/&
     &/'CIRCULATE (TAG IS ",A,")")') TAG (N)                            
      CALL BORT (BORT_STR) 
      END SUBROUTINE MAKESTAB                       

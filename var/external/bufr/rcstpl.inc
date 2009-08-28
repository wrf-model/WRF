      SUBROUTINE RCSTPL (LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RCSTPL                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE STORES THE SUBSET TEMPLATE INTO INTERNAL    
!   SUBSET ARRAYS IN COMMON BLOCKS /USRINT/ AND /USRBIT/.  THIS IS IN   
!   PREPARATION FOR THE ACTUAL UNPACKING OF THE SUBSET IN BUFR ARCHIVE  
!   LIBRARY SUBROUTINE RDTREE.                                          
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
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); MAXRCR (MAXIMUM      
!                           NUMBER OF RECURSION LEVELS) INCREASED FROM  
!                           50 TO 100  (WAS IN VERIFICATION VERSION);   
!                           UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY; COMMENTED OUT        
!                           HARDWIRE OF VTMP TO "BMISS" (10E10) WHEN IT 
!                           IS > 10E9 (CAUSED PROBLEMS ON SOME FOREIGN  
!                           MACHINES)                                   
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL RCSTPL (LUN)                                           
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     UPBB                           
!    THIS ROUTINE IS CALLED BY: RDTREE                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      PARAMETER (MAXRCR = 100) 
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRBIT / NBIT (MAXJL), MBIT (MAXJL) 
!     COMMON / USRTMP / ITMP (MAXJL, MAXRCR), VTMP (MAXJL, MAXRCR) 
                                                                        
      CHARACTER(128) BORT_STR 
!     CHARACTER(10) TAG 
!     CHARACTER(3) TYP 
      DIMENSION NBMP (2, MAXRCR), NEWN (2, MAXRCR) 
      DIMENSION KNX (MAXRCR) 
!     REAL(8) VTMP !, VAL
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      IF ( .NOT. ALLOCATED (NVAL) ) ALLOCATE ( NVAL(NFILES) )
      IF ( .NOT. ALLOCATED (INV) ) ALLOCATE ( INV(MAXJL,NFILES) )
      IF ( .NOT. ALLOCATED (VAL) ) ALLOCATE ( VAL(MAXJL,NFILES) )

      IF ( .NOT. ALLOCATED (ITMP) ) ALLOCATE (ITMP (MAXJL, MAXRCR))
      IF ( .NOT. ALLOCATED (VTMP) ) ALLOCATE (VTMP (MAXJL, MAXRCR))
                                                                        
!  SET THE INITIAL VALUES FOR THE TEMPLATE                              
!  ---------------------------------------                              
                                                                        
!  .... Positional index of Table A mnem.                               
      INV (1, LUN) = INODE (LUN) 
      VAL (1, LUN) = 0 
      NBMP (1, 1) = 1 
      NBMP (2, 1) = 1 
      NODI = INODE (LUN) 
      NODE = INODE (LUN) 
      MBMP = 1 
      KNVN = 1 
      NR = 0 
                                                                        
      DO I = 1, MAXRCR 
      KNX (I) = 0 
      ENDDO 
                                                                        
!  SET UP THE PARAMETERS FOR A LEVEL OF RECURSION                       
!  ----------------------------------------------                       
                                                                        
   10 CONTINUE 
                                                                        
      NR = NR + 1 
      IF (NR.GT.MAXRCR) GOTO 900 
      NBMP (1, NR) = 1 
      NBMP (2, NR) = MBMP 
                                                                        
      N1 = ISEQ (NODE, 1) 
      N2 = ISEQ (NODE, 2) 
      IF (N1.EQ.0) GOTO 901 
      IF (N2 - N1 + 1.GT.MAXJL) GOTO 902 
      NEWN (1, NR) = 1 
      NEWN (2, NR) = N2 - N1 + 1 
                                                                        
      DO N = 1, NEWN (2, NR) 
      NN = JSEQ (N + N1 - 1) 
      ITMP (N, NR) = NN 
      VTMP (N, NR) = VALI (NN) 
      ENDDO 
                                                                        
!  STORE NODES AT SOME RECURSION LEVEL                                  
!  -----------------------------------                                  
                                                                        
   20 DO I = NBMP (1, NR), NBMP (2, NR) 
      IF (KNX (NR) .EQ.0000) KNX (NR) = KNVN 
      IF (I.GT.NBMP (1, NR) ) NEWN (1, NR) = 1 
      DO J = NEWN (1, NR), NEWN (2, NR) 
      KNVN = KNVN + 1 
      NODE = ITMP (J, NR) 
!  .... INV is positional index in internal jump/link table for packed  
!       subset element KNVN in MBAY                                     
      INV (KNVN, LUN) = NODE 
!  .... Actual unpacked subset values (VAL) are init. here (numbers as  
!      10E10, msg)                                                      
      VAL (KNVN, LUN) = VTMP (J, NR) 
!  .... MBIT is the bit in MBAY pointing to where the packed subset     
!       element KNVN begins                                             
      MBIT (KNVN) = MBIT (KNVN - 1) + NBIT (KNVN - 1) 
!  .... NBIT is the number of bits in MBAY occupied by packed subset    
!       element KNVN                                                    
      NBIT (KNVN) = IBT (NODE) 
      IF (ITP (NODE) .EQ.1) THEN 
      CALL UPBB (MBMP, NBIT (KNVN), MBIT (KNVN), MBAY (1, LUN) ) 
      NEWN (1, NR) = J + 1 
      NBMP (1, NR) = I 
      GOTO 10 
      ENDIF 
      ENDDO 
      NEW = KNVN - KNX (NR) 
      VAL (KNX (NR) + 1, LUN) = VAL (KNX (NR) + 1, LUN) + NEW 
      KNX (NR) = 0 
      ENDDO 
                                                                        
!  CONTINUE AT ONE RECURSION LEVEL BACK                                 
!  ------------------------------------                                 
                                                                        
      IF (NR - 1.NE.0) THEN 
      NR = NR - 1 
      GOTO 20 
      ENDIF 
                                                                        
!  FINALLY STORE THE LENGTH OF (NUMBER OF ELEMENTS IN) SUBSET TEMPLATE  
!  -------------------------------------------------------------------  
                                                                        
      NVAL (LUN) = KNVN 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: RCSTPL - THE NUMBER OF RECURSION '// &
      'LEVELS EXCEEDS THE LIMIT (",I3,")")') MAXRCR                     
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: RCSTPL - UNSET EXPANSION SEGMENT ",A)&
     &') TAG (NODI)                                                     
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: RCSTPL - TEMPLATE ARRAY OVERFLOW, '//&
      'EXCEEDS THE LIMIT (",I6,") (",A,")")') MAXJL, TAG (NODI)         
      CALL BORT (BORT_STR) 
      END SUBROUTINE RCSTPL                         

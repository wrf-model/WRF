      SUBROUTINE NEMTBD (LUN, ITAB, NSEQ, NEMS, IRPS, KNTS) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEMTBD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS A LIST OF THE MNEMONICS (I.E.,      
!   "CHILD" MNEMONICS) CONTAINED WITHIN A TABLE D SEQUENCE MNEMONIC     
!   (I.E., A "PARENT MNEMONIC").  THIS INFORMATION SHOULD HAVE BEEN     
!   PACKED INTO THE INTERNAL BUFR TABLE D ENTRY FOR THE PARENT MNEMONIC 
!   (IN COMMON BLOCK /TABABD/) VIA PREVIOUS CALLS TO BUFR ARCHIVE       
!   LIBRARY SUBROUTINE PKTDD.  NOTE THAT NEMTBD DOES NOT RECURSIVELY    
!   RESOLVE CHILD MNEMONICS WHICH ARE THEMSELVES TABLE D SEQUENCE       
!   MNEMONICS; RATHER, SUCH RESOLUTION MUST BE DONE VIA SEPARATE        
!   SUBSEQUENT CALLS TO THIS SUBROUTINE.                                
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
! 2000-09-19  J. WOOLLEN -- MUST NOW CHECK FOR TABLE C (OPERATOR        
!                           DESCRIPTOR) MNEMONICS SINCE THE CAPABILITY  
!                           HAS NOW BEEN ADDED TO ENCODE AND DECODE     
!                           THESE                                       
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    CALL NEMTBD (LUN, ITAB, NSEQ, NEMS, IRPS, KNTS)             
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     ITAB     - INTEGER: POSITIONAL INDEX OF PARENT MNEMONIC WITHIN    
!                INTERNAL BUFR TABLE D ARRAY TABD(*,*)                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NSEQ     - INTEGER: TOTAL NUMBER OF CHILD MNEMONICS FOR THE       
!                PARENT MNEMONIC GIVEN BY TABD(ITAB,LUN)                
!     NEMS     - CHARACTER*8: (NSEQ)-WORD ARRAY OF CHILD MNEMONICS      
!     IRPS     - INTEGER: (NSEQ)-WORD RETURN VALUE ARRAY (SEE REMARKS)  
!     KNTS     - INTEGER: (NSEQ)-WORD RETURN VALUE ARRAY (SEE REMARKS)  
!                                                                       
! REMARKS:                                                              
!    VALUE FOR OUTPUT ARGUMENT IRPS:                                    
!       The interpretation of the return value IRPS(I) depends upon the 
!       type of descriptor corresponding to NEMS(I), as follows:        
!                                                                       
!       IF ( NEMS(I) corresponds to an F=1 regular (i.e. non-delayed)   
!            replication descriptor ) THEN                              
!          IRPS(I) = 1                                                  
!       ELSE IF ( NEMS(I) corresponds to a delayed replicator or        
!                 replication factor descriptor )  THEN                 
!          IRPS(I) = positional index of corresponding descriptor       
!                    within internal replication array IDNR(*,*)        
!       ELSE                                                            
!          IRPS(I) = 0                                                  
!       END IF                                                          
!                                                                       
!                                                                       
!    VALUE FOR OUTPUT ARGUMENT KNTS:                                    
!       The interpretation of the return value KNTS(I) depends upon the 
!       type of descriptor corresponding to NEMS(I), as follows:        
!                                                                       
!       IF ( NEMS(I) corresponds to an F=1 regular (i.e. non-delayed)   
!            replication descriptor ) THEN                              
!          KNTS(I) = number of replications                             
!       ELSE                                                            
!          KNTS(I) = 0                                                  
!       END IF                                                          
!                                                                       
!                                                                       
!    THIS ROUTINE CALLS:        BORT     IFXY     NUMTAB   RSVFVM       
!                               UPTDD                                   
!    THIS ROUTINE IS CALLED BY: CHEKSTAB DXDUMP   GETABDB  TABSUB       
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / TABABD / NTBA (0:NFILES), NTBB (0:NFILES), NTBD (0:      &
      NFILES), MTAB (MAXTBA, NFILES), IDNA (MAXTBA, NFILES, 2), IDNB (  &
      MAXTBB, NFILES), IDND (MAXTBD, NFILES), TABA (MAXTBA, NFILES),    &
      TABB (MAXTBB, NFILES), TABD (MAXTBD, NFILES)                      
                                                                        
      CHARACTER(600) TABD 
      CHARACTER(128) TABB 
      CHARACTER(128) TABA 
      CHARACTER(128) BORT_STR 
      CHARACTER(8) NEMO, NEMS, NEMT, NEMF 
      CHARACTER(1) TAB 
      DIMENSION NEMS (MAXCD), IRPS (MAXCD), KNTS (MAXCD) 
      LOGICAL REP 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (ITAB.LE.0.OR.ITAB.GT.NTBD (LUN) ) GOTO 900 
                                                                        
      REP = .FALSE. 
                                                                        
!  CLEAR THE RETURN VALUES                                              
!  -----------------------                                              
                                                                        
      NSEQ = 0 
                                                                        
      DO I = 1, MAXCD 
      NEMS (I) = ' ' 
      IRPS (I) = 0 
      KNTS (I) = 0 
      ENDDO 
                                                                        
!  PARSE THE TABLE D ENTRY                                              
!  -----------------------                                              
                                                                        
      NEMO = TABD (ITAB, LUN) (7:14) 
      IDSC = IDND (ITAB, LUN) 
      CALL UPTDD (ITAB, LUN, 0, NDSC) 
                                                                        
      IF (IDSC.LT.IFXY ('300000') ) GOTO 901 
      IF (IDSC.GT.IFXY ('363255') ) GOTO 901 
!ccc  IF(NDSC.LE.0             ) GOTO 902                               
                                                                        
!     Loop through each child mnemonic.                                 
                                                                        
!  .... DK: What happens here if NDSC=0 ?                               
      DO J = 1, NDSC 
      IF (NSEQ + 1.GT.MAXCD) GOTO 903 
      CALL UPTDD (ITAB, LUN, J, IDSC) 
!  .... get NEMT from IDSC                                              
      CALL NUMTAB (LUN, IDSC, NEMT, TAB, IRET) 
      IF (TAB.EQ.'R') THEN 
      IF (REP) GOTO 904 
      REP = .TRUE. 
      IF (IRET.LT.0) THEN 
                                                                        
!           F=1 regular (i.e. non-delayed) replication.                 
                                                                        
      IRPS (NSEQ + 1) = 1 
      KNTS (NSEQ + 1) = ABS (IRET) 
      ELSEIF (IRET.GT.0) THEN 
                                                                        
!           Delayed replication.                                        
                                                                        
      IRPS (NSEQ + 1) = IRET 
      ENDIF 
      ELSEIF (TAB.EQ.'F') THEN 
                                                                        
!            Replication factor.                                        
                                                                        
      IF (.NOT.REP) GOTO 904 
      IRPS (NSEQ + 1) = IRET 
      REP = .FALSE. 
      ELSEIF (TAB.EQ.'D'.OR.TAB.EQ.'C') THEN 
      REP = .FALSE. 
      NSEQ = NSEQ + 1 
      NEMS (NSEQ) = NEMT 
      ELSEIF (TAB.EQ.'B') THEN 
      REP = .FALSE. 
      NSEQ = NSEQ + 1 
      IF (NEMT (1:1) .EQ.'.') THEN 
                                                                        
!            This is a "following value" mnemonic.                      
                                                                        
      CALL UPTDD (ITAB, LUN, J + 1, IDSC) 
!  .... get NEMF from IDSC                                              
      CALL NUMTAB (LUN, IDSC, NEMF, TAB, IRET) 
      CALL RSVFVM (NEMT, NEMF) 
      IF (TAB.NE.'B') GOTO 906 
      ENDIF 
      NEMS (NSEQ) = NEMT 
      ELSE 
      GOTO 905 
      ENDIF 
      ENDDO 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - ITAB (",I7,") NOT FOUND IN '&
     &//'TABLE D")') ITAB                                               
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - INTEGER REPRESENTATION OF '/&
     &/'DESCRIPTOR FOR TABLE D MNEMONIC ",A," (",I7,") IS OUTSIDE '//'RA&
     &NGE 0-65535 (65535 -> 3-63-255)")') NEMO, IDSC                    
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - TABLE D MNEMONIC ",A," IS A'&
     &//' ZERO LENGTH SEQUENCE")') NEMO                                 
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - THERE ARE MORE THAN '//'(",I&
     &4,") DESCRIPTORS (THE LIMIT) IN TABLE D SEQUENCE '//'MNEMONIC ",A)&
     &') MAXCD, NEMO                                                    
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - REPLICATOR IS OUT OF ORDER '&
     &//'IN TABLE D SEQUENCE MNEMONIC ",A)') NEMO                       
      CALL BORT (BORT_STR) 
  905 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - UNRECONGIZED DESCRIPTOR '// &
      '(INTEGER=",I7,") IN TABLE D SEQUENCE MNEMONIC ",A)') IDSC, NEMO  
      CALL BORT (BORT_STR) 
  906 WRITE (BORT_STR, '("BUFRLIB: NEMTBD - A ''FOLLOWING VALUE'' '//   &
      'MNEMONIC (",A,") IS FROM TABLE ",A,", IT MUST BE FROM TABLE B'// &
      '")') NEMF, TAB                                                   
      CALL BORT (BORT_STR) 
      END SUBROUTINE NEMTBD                         

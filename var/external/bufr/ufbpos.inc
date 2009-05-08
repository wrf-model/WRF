      SUBROUTINE UFBPOS (LUNIT, IREC, ISUB, SUBSET, JDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBPOS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1995-11-22           
!                                                                       
! ABSTRACT: THIS SUBROUTINE SHOULD ONLY BE CALLED WHEN LOGICAL UNIT     
!   LUNIT HAS BEEN OPENED FOR INPUT OPERATIONS.  IT POSITIONS THE       
!   MESSAGE POINTER TO A USER-SPECIFIED BUFR MESSAGE NUMBER IN THE FILE 
!   CONNECTED TO LUNIT AND THEN CALLS BUFR ARCHIVE LIBRARY SUBROUTINE   
!   READMG TO READ THIS BUFR MESSAGE INTO A MESSAGE BUFFER (ARRAY MBAY  
!   IN COMMON BLOCK /BITBUF/).  IT THEN POSITIONS THE SUBSET POINTER TO 
!   A USER-SPECIFIED SUBSET NUMBER WITHIN THE BUFR MESSAGE AND CALLS    
!   BUFR ARCHIVE LIBRARY SUBROUTINE READSB TO READ THIS SUBSET INTO     
!   INTERNAL SUBSET ARRAYS.  THE BUFR MESSAGE HERE MAY BE EITHER        
!   COMPRESSED OR UNCOMPRESSED.  THE USER-SPECIFIED MESSAGE NUMBER DOES 
!   NOT INCLUDE  ANY DICTIONARY MESSAGES THAT MAY BE AT THE TOP OF THE   
!   FILE).                                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1995-11-22  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN-LINED IN PROGRAM    
!                           NAM_STNMLIST)                               
! 2005-03-04  D. KEYSER  -- ADDED TO BUFR ARCHIVE LIBRARY; ADDED        
!                           DOCUMENTATION                               
! 2005-11-29  J. ATOR    -- USE IUPBS01 AND RDMSGW                      
! 2006-04-14  J. ATOR    -- REMOVE UNNECESSARY MOIN INITIALIZATION      
!                                                                       
! USAGE:    CALL UFBPOS( LUNIT, IREC, ISUB, SUBSET, JDATE )             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     IREC     - INTEGER: POINTER TO BUFR MESSAGE NUMBER (RECORD) IN    
!                FILE (DOES NOT INCLUDE  ANY DICTIONARY MESSSAGES THAT   
!                MAY BE AT THE TOP OF THE FILE)                         
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR TYPE OF BUFR MESSAGE 
!                BEING READ                                             
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING READ, IN FORMAT OF EITHER YYMMDDHH OR    
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPBS01  NMSUB    RDMSGW       
!                               READMG   READSB   STATUS   UFBCNT       
!                               UPB                                     
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
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
      DIMENSION MOIN (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!---------------------------------------------------------------------- 
                                                                        
!  MAKE SURE A FILE IS OPEN FOR INPUT                                   
!  ----------------------------------                                   
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.GT.0) GOTO 901 
                                                                        
      IF (IREC.LE.0) GOTO 902 
      IF (ISUB.LE.0) GOTO 903 
                                                                        
!  SEE WHERE POINTERS ARE CURRENTLY LOCATED                             
!  ----------------------------------------                             
                                                                        
      CALL UFBCNT (LUNIT, JREC, JSUB) 
                                                                        
!  POSSIBLY REWIND AND POSITION AFTER THE DICTIONARY                    
!   (IF REQUESTED POINTERS ARE BEHIND CURRENT POINTERS)                 
!  ----------------------------------------------------                 
                                                                        
      IF (IREC.LT.JREC.OR. (IREC.EQ.JREC.AND.ISUB.LT.JSUB) ) THEN 
         IDEX = 0 
         MSGT = 11 
         REWIND LUNIT 
         DO WHILE (MSGT.EQ.11) 
         CALL RDMSGW (LUNIT, MOIN, IER) 
         MSGT = IUPBS01 (MOIN, 'MTYP') 
         IDEX = IDEX + 1 
         ENDDO 
         REWIND LUNIT 
         DO NDX = 1, IDEX - 1 
         CALL RDMSGW (LUNIT, MOIN, IER) 
         ENDDO 
         NMSG (LUN) = 0 
         NSUB (LUN) = 0 
         CALL UFBCNT (LUNIT, JREC, JSUB) 
      ENDIF 
                                                                        
!  READ SUBSET #ISUB FROM MESSAGE #IREC FROM FILE                       
!  ----------------------------------------------                       
                                                                        
      DO WHILE (IREC.GT.JREC) 
      CALL READMG (LUNIT, SUBSET, JDATE, IRET) 
      IF (IRET.NE.0) GOTO 904 
      CALL UFBCNT (LUNIT, JREC, JSUB) 
      ENDDO 
                                                                        
      KSUB = NMSUB (LUNIT) 
      IF (ISUB.GT.KSUB) GOTO 905 
                                                                        
      DO WHILE (ISUB - 1.GT.JSUB) 
      IBIT = MBYT (LUN) * 8 
      CALL UPB (NBYT, 16, MBAY (1, LUN), IBIT) 
      MBYT (LUN) = MBYT (LUN) + NBYT 
      NSUB (LUN) = NSUB (LUN) + 1 
      CALL UFBCNT (LUNIT, JREC, JSUB) 
      ENDDO 
                                                                        
      CALL READSB (LUNIT, IRET) 
      IF (IRET.NE.0) GOTO 905 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UFBPOS - INPUT BUFR FILE IS CLOSED, IT MUST'/&
     &/' BE OPEN FOR INPUT')                                            
  901 CALL BORT ('BUFRLIB: UFBPOS - INPUT BUFR FILE IS OPEN FOR OUTPUT'/&
     &/', IT MUST BE OPEN FOR INPUT')                                   
  902 WRITE (BORT_STR, '("BUFRLIB: UFBPOS - REQUESTED MESSAGE NUMBER '//&
      'TO READ IN (",I5,") IS NOT VALID")') IREC                        
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: UFBPOS - REQUESTED SUBSET NUMBER '// &
      'TO READ IN (",I5,") IS NOT VALID")') ISUB                        
      CALL BORT (BORT_STR) 
  904 WRITE (BORT_STR, '("BUFRLIB: UFBPOS - REQUESTED MESSAGE NUMBER '//&
      'TO READ IN (",I5,") EXCEEDS THE NUMBER OF MESSAGES IN THE '//    &
      'FILE (",I5,")")') IREC, JREC                                     
      CALL BORT (BORT_STR) 
  905 WRITE (BORT_STR, '("BUFRLIB: UFBPOS - REQ. SUBSET NUMBER TO READ'/&
     &/' IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") IN THE '//'R&
     &EQ. MESSAGE (",I5,")")') ISUB, KSUB, IREC                         
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBPOS                         

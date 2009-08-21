      SUBROUTINE UFBOVR (LUNIT, USR, I1, I2, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBOVR                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE WRITES OVER SPECIFIED VALUES WHICH EXIST    
!   IN CURRENT INTERNAL BUFR SUBSET ARRAYS IN A FILE OPEN FOR OUTPUT.   
!   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A         
!   DELAYED-REPLICATION SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION  
!   AT ALL.  EITHER BUFR ARCHIVE LIBRARY SUBROUTINE OPENMG OR OPENMB    
!   MUST HAVE BEEN PREVIOUSLY CALLED TO OPEN AND INITIALIZE A BUFR      
!   MESSAGE WITHIN MEMORY FOR THIS LUNIT.  IN ADDITION, BUFR ARCHIVE    
!   LIBRARY SUBROUTINE WRITSB OR INVMRG MUST HAVE BEEN CALLED TO STORE  
!   DATA IN THE INTERNAL OUTPUT SUBSET ARRAYS.                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR  
!                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM    
!                           BORT TO BORT2 IN SOME CASES                 
! 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS    
!                                                                       
! USAGE:    CALL UFBOVR (LUNIT, USR, I1, I2, IRET, STR)                 
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     USR      - REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES        
!                WRITTEN TO DATA SUBSET                                 
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE AT LEAST AS LARGE AS LATTER)                   
!     I2       - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES TO BE       
!                WRITTEN TO DATA SUBSET                                 
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF USR                                       
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES WRITTEN TO  
!                DATA SUBSET (SHOULD BE SAME AS I2)                     
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING       
!                               TRYBUMP                                 
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
                                                                        
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER ( * ) STR 
      REAL(8) USR (I1, I2)!, VAL 
                                                                        
      DATA IFIRST1 / 0 /, IFIRST2 / 0 / 
                                                                        
      SAVE IFIRST1, IFIRST2 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IRET = 0 
                                                                        
!  CHECK THE FILE STATUS AND I-NODE                                     
!  --------------------------------                                     
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.LT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
      IF (INODE (LUN) .NE.INV (1, LUN) ) GOTO 903 
                                                                        
!  .... DK: Why check, isn't IO always 1 here?                          
      IO = MIN (MAX (0, IL), 1) 
                                                                        
      IF (I1.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBOVR - THIRD ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH FIFTH ARGUMENT (IRET) = 0'                         
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I2.LE.0) THEN 
         IF (IPRT.EQ. - 1) IFIRST1 = 1 
         IF (IO.EQ.0.OR.IFIRST1.EQ.0.OR.IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBOVR - FOURTH ARGUMENT (INPUT) IS .LE.', ' 0&
     & -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'                        
            PRINT * , 'STR = ', STR 
            IF (IPRT.EQ.0.AND.IO.EQ.1) PRINT 101 
  101 FORMAT('Note: Only the first occurrence of this WARNING message ',&
     & 'is printed, there may be more.  To output'/6X,'ALL WARNING ',   &
     & 'messages, modify your application program to add ',             &
     & '"CALL OPENBF(0,''QUIET'',1)" prior'/6X,'to the first call to a',&
     & ' BUFRLIB routine.')                                             
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
            IFIRST1 = 1 
         ENDIF 
         GOTO 100 
      ENDIF 
                                                                        
!  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES                 
!  ----------------------------------------------------                 
                                                                        
      CALL STRING (STR, LUN, I1, IO) 
      CALL TRYBUMP (LUNIT, LUN, USR, I1, I2, IO, IRET) 
                                                                        
      IF (IO.EQ.1.AND.IRET.NE.I2) GOTO 904 
                                                                        
      IF (IRET.EQ.0) THEN 
         IF (IPRT.EQ. - 1) IFIRST2 = 1 
         IF (IFIRST2.EQ.0.OR.IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBOVR - NO SPECIFIED VALUES WRITTEN OUT', ' -&
     &  RETURN WITH FIFTH ARGUMENT (IRET) = 0'                          
            PRINT * , 'STR = ', STR, ' MAY NOT BE IN THE BUFR TABLE(?)' 
            IF (IPRT.EQ.0) PRINT 101 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
            IFIRST2 = 1 
         ENDIF 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  901 CALL BORT ('BUFRLIB: UFBOVR - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  902 CALL BORT ('BUFRLIB: UFBOVR - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
  903 CALL BORT ('BUFRLIB: UFBOVR - LOCATION OF INTERNAL TABLE FOR '//  &
      'OUTPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//    &
      'INTERNAL SUBSET ARRAY')                                          
  904 WRITE (BORT_STR1, '("BUFRLIB: UFBOVR - MNEMONIC STRING READ IN IS'&
     &//': ",A)') STR                                                   
      WRITE (BORT_STR2, '(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//    &
      'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'// &
      ' - INCOMPLETE WRITE")') IRET, I2                                 
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END SUBROUTINE UFBOVR                         

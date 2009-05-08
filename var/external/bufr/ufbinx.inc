      SUBROUTINE UFBINX (LUNIT, IMSG, ISUB, USR, I1, I2, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBINX                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE EITHER OPENS A BUFR FILE CONNECTED TO       
!   LOGICAL UNIT LUNIT FOR INPUT OPERATIONS (IF IT IS NOT ALREADY       
!   OPENED AS SUCH), OR SAVES ITS POSITION AND REWINDS IT TO THE FIRST  
!   DATA MESSAGE (IF BUFR FILE ALREADY OPENED), THEN (VIA A CALL TO     
!   BUFR ARCHIVE LIBRARY SUBROUTINE UFBINT) READS SPECIFIED VALUES FROM 
!   INTERNAL SUBSET ARRAYS ASSOCIATED WITH A PARTICULAR SUBSET FROM A   
!   PARTICULAR BUFR MESSAGE IN A MESSAGE BUFFER.  THE PARTICULAR SUBSET 
!   AND BUFR MESSAGE ARE BASED BASED ON THE SUBSET NUMBER IN THE        
!   MESSAGE AND THE MESSAGE NUMBER IN THE BUFR FILE.  FINALLY, THIS     
!   SUBROUTINE EITHER CLOSES THE BUFR FILE IN LUNIT (IF IS WAS OPENED   
!   HERE) OR RESTORES IT TO ITS PREVIOUS READ/WRITE STATUS AND POSITION 
!   (IF IT WAS NOT OPENED HERE).  SEE UFBINT FOR MORE INFORMATION ON    
!   THE READING OF VALUES OUT OF A BUFR MESSAGE SUBSET.  NOTE: THE      
!   MESSAGE NUMBER HERE DOES NOT INCLUDE  THE DICTIONARY MESSAGES AT THE 
!   BEGINNING OF THE FILE.                                              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION BUT MAY HAVE BEEN IN THE PRODUCTION 
!                           VERSION AT ONE TIME AND THEN REMOVED)       
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL UFBINX (LUNIT, IMSG, ISUB, USR, I1, I2, IRET, STR)     
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER TO READ IN     
!                BUFR FILE                                              
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE AT LEAST AS LARGE AS LATTER)                   
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF USR {THIS CAN ALSO BE A SINGLE TABLE D    
!                (SEQUENCE) MNEMONIC WITH EITHER 8- OR 16-BIT DELAYED   
!                REPLICATION (SEE REMARKS 1 IN UFBINT DOCBLOCK)}        
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES READ   
!                FROM DATA SUBSET                                       
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES READ FROM   
!                DATA SUBSET (MUST BE NO LARGER THAN I2)                
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CLOSBF   OPENBF   READMG       
!                               READSB   REWNBF   STATUS   UFBINT       
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
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
      LOGICAL OPENIT 
      REAL(8) USR (I1, I2) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      OPENIT = IL.EQ.0 
                                                                        
      IF (OPENIT) THEN 
                                                                        
!  OPEN BUFR FILE CONNECTED TO UNIT LUNIT IF IT IS NOT ALREADY OPEN     
!  ----------------------------------------------------------------     
                                                                        
         CALL OPENBF (LUNIT, 'IN', LUNIT) 
      ELSE 
                                                                        
!  IF BUFR FILE ALREADY OPENED, SAVE POSITION & REWIND TO FIRST DATA MSG
!  ---------------------------------------------------------------------
                                                                        
         CALL REWNBF (LUNIT, 0) 
      ENDIF 
                                                                        
!  SKIP TO MESSAGE # IMSG                                               
!  ----------------------                                               
                                                                        
      DO I = 1, IMSG - 1 
      READ (LUNIT, ERR = 900, END = 901) 
      ENDDO 
                                                                        
      CALL READMG (LUNIT, SUBSET, JDATE, JRET) 
      IF (JRET.NE.0) GOTO 901 
                                                                        
!  POSITION AT SUBSET # ISUB                                            
!  -------------------------                                            
                                                                        
      DO I = 1, ISUB - 1 
      IF (NSUB (LUN) .GT.MSUB (LUN) ) GOTO 902 
      IBIT = MBYT (LUN) * 8 
      CALL UPB (NBYT, 16, MBAY (1, LUN), IBIT) 
      MBYT (LUN) = MBYT (LUN) + NBYT 
      NSUB (LUN) = NSUB (LUN) + 1 
      ENDDO 
                                                                        
      CALL READSB (LUNIT, JRET) 
      IF (JRET.NE.0) GOTO 902 
                                                                        
      CALL UFBINT (LUNIT, USR, I1, I2, IRET, STR) 
                                                                        
      IF (OPENIT) THEN 
                                                                        
!  CLOSE BUFR FILE IF IT WAS OPENED HERE                                
!  -------------------------------------                                
                                                                        
         CALL CLOSBF (LUNIT) 
      ELSE 
                                                                        
                                                                        
!  RESTORE BUFR FILE TO PREV. STATUS & POSITION IF NOT ORIG. OPENED HERE
!  ---------------------------------------------------------------------
                                                                        
         CALL REWNBF (LUNIT, 1) 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UFBINX - ERROR READING MESSAGE '//'(R&
     &ECORD) NUMBER",I5," IN INPUT BUFR FILE CONNECTED TO UNIT",'//'I4)'&
     &) I, LUNIT                                                        
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: UFBINX - HIT END OF FILE BEFORE '//  &
      'READING REQUESTED MESSAGE NO.",I5," IN BUFR FILE CONNECTED TO'// &
      ' UNIT",I4)') IMSG, LUNIT                                         
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: UFBINX - ALL SUBSETS READ BEFORE '// &
      'READING REQ. SUBSET NO.",I3," IN REQ. MSG NO.",I5," IN BUFR '//  &
      'FILE CONNECTED TO UNIT",I4)') ISUB, IMSG, LUNIT                  
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBINX                         

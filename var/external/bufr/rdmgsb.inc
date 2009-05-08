      SUBROUTINE RDMGSB (LUNIT, IMSG, ISUB) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMGSB                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE OPENS A BUFR FILE IN LOGICAL UNIT LUNIT FOR 
!   INPUT OPERATIONS, THEN READS A PARTICULAR SUBSET INTO INTERNAL      
!   SUBSET ARRAYS FROM A PARTICULAR BUFR MESSAGE IN A MESSAGE BUFFER.   
!   THIS IS BASED ON THE SUBSET NUMBER IN THE MESSAGE AND THE MESSAGE   
!   NUMBER IN THE BUFR FILE.  THE MESSAGE NUMBER DOES NOT INCLUDE  THE   
!   DICTIONARY MESSAGES AT THE BEGINNING OF THE FILE.                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION BUT MAY HAVE BEEN IN THE PRODUCTION 
!                           VERSION AT ONE TIME AND THEN REMOVED)       
! 2003-11-04  D. KEYSER  -- INCORPORATED INTO "UNIFIED" BUFR ARCHIVE    
!                           LIBRARY; UNIFIED/PORTABLE FOR WRF; ADDED    
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL RDMGSB (LUNIT, IMSG, ISUB)                             
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER TO READ IN     
!                BUFR FILE                                              
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     OPENBF   READMG   READSB       
!                               STATUS   UPB                            
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
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  OPEN THE FILE AND SKIP TO MESSAGE # IMSG                             
!  ----------------------------------------                             
                                                                        
      CALL OPENBF (LUNIT, 'IN', LUNIT) 
      CALL STATUS (LUNIT, LUN, IL, IM) 
                                                                        
      DO I = 1, IMSG - 1 
      READ (LUNIT, ERR = 900, END = 901) 
      ENDDO 
                                                                        
      CALL READMG (LUNIT, SUBSET, JDATE, IRET) 
      IF (IRET.NE.0) GOTO 901 
                                                                        
!  POSITION AT SUBSET # ISUB                                            
!  -------------------------                                            
                                                                        
      DO I = 1, ISUB - 1 
      IF (NSUB (LUN) .GT.MSUB (LUN) ) GOTO 902 
      IBIT = MBYT (LUN) * 8 
      CALL UPB (NBYT, 16, MBAY (1, LUN), IBIT) 
      MBYT (LUN) = MBYT (LUN) + NBYT 
      NSUB (LUN) = NSUB (LUN) + 1 
      ENDDO 
                                                                        
      CALL READSB (LUNIT, IRET) 
      IF (IRET.NE.0) GOTO 902 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: RDMGSB - ERROR READING MESSAGE '//'(R&
     &ECORD) NUMBER",I5," IN INPUT BUFR FILE CONNECTED TO UNIT",'//'I4)'&
     &) I, LUNIT                                                        
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: RDMGSB - HIT END OF FILE BEFORE '//  &
      'READING REQUESTED MESSAGE NO.",I5," IN BUFR FILE CONNECTED TO'// &
      ' UNIT",I4)') IMSG, LUNIT                                         
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: RDMGSB - ALL SUBSETS READ BEFORE '// &
      'READING REQ. SUBSET NO.",I3," IN REQ. MSG NO.",I5," IN BUFR '//  &
      'FILE CONNECTED TO UNIT",I4)') ISUB, IMSG, LUNIT                  
      CALL BORT (BORT_STR) 
      END SUBROUTINE RDMGSB                         

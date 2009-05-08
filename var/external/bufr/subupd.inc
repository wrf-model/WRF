      SUBROUTINE SUBUPD (LUNIT, LUN, IBYT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SUBUPD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PACKS UP THE CURRENT SUBSET WITHIN MEMORY   
!  (ARRAY IBAY IN COMMON BLOCK /BITBUF/) AND THEN TRIES TO ADD IT TO    
!  THE BUFR MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR LUNIT      
!  (ARRAY MBAY IN COMMON BlOCK /BITBUF/).  IF THE SUBSET WILL NOT FIT   
!  INTO THE CURRENTLY OPEN MESSAGE, THEN THAT MESSAGE IS FLUSHED TO     
!  LUNIT AND A NEW ONE IS CREATED IN ORDER TO HOLD THE CURRENT SUBSET.  
!  IF THE SUBSET IS LARGER THAN AN EMPTY MESSAGE, THE SUBSET IS         
!  DISCARDED ANDA DIAGNOSTIC IS PRINTED.  THIS SUBROUTINE IS IDENTICAL  
!  TO BUFR ARCHIVE LIBRARY SUBROUTINE MSGUPD EXCEPT SUBUPD DOES NOT PAD 
!  THE PACKED SUBSET.                                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR (WAS IN VERIFICATION        
!                           VERSION)                                    
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION                               
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL SUBUPD (LUNIT, LUN, IBYT)                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT) 
!     IBYT     - INTEGER: NUMBER OF BYTES WITHIN IBAY CONTAINING PACKED 
!                DATA                                                   
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IUPB     MSGINI   MSGWRT   MVB          
!                               PKB      USRTPL                         
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Perhaps a verification application      
!                               program calls it (if not, remove it?)   
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / MSGPTR / NBY0, NBY1, NBY2, NBY3, NBY4, NBY5 
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
      COMMON / QUIET / IPRT 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  SEE IF THE NEW SUBSET FITS                                           
!  --------------------------                                           
                                                                        
      IF (MBYT (LUN) + IBYT + 8.GT.MAXBYT) THEN 
!  .... NO it does not fit                                              
         CALL MSGWRT (LUNIT, MBAY (1, LUN), MBYT (LUN) ) 
         CALL MSGINI (LUN) 
      ENDIF 
                                                                        
      IF (MBYT (LUN) + IBYT + 8.GT.MAXBYT) GOTO 900 
                                                                        
!  SET A BYTE COUNT AND TRANSFER THE SUBSET BUFFER INTO THE MESSAGE     
!  ----------------------------------------------------------------     
                                                                        
      LBIT = 0 
      CALL PKB (IBYT, 16, IBAY, LBIT) 
!  .... DK: Why the -3 in "MBYT(LUN)-3" ??                              
      CALL MVB (IBAY, 1, MBAY (1, LUN), MBYT (LUN) - 3, IBYT) 
                                                                        
!  UPDATE THE SUBSET AND BYTE COUNTERS                                  
!  --------------------------------------                               
                                                                        
      MBYT (LUN) = MBYT (LUN) + IBYT 
      NSUB (LUN) = NSUB (LUN) + 1 
                                                                        
      LBIT = (NBY0 + NBY1 + NBY2 + 4) * 8 
      CALL PKB (NSUB (LUN), 16, MBAY (1, LUN), LBIT) 
                                                                        
      LBYT = NBY0 + NBY1 + NBY2 + NBY3 
      NBYT = IUPB (MBAY (1, LUN), LBYT + 1, 24) 
      LBIT = LBYT * 8 
      CALL PKB (NBYT + IBYT, 24, MBAY (1, LUN), LBIT) 
                                                                        
!  RESET THE USER ARRAYS AND EXIT NORMALLY                              
!  ---------------------------------------                              
                                                                        
      CALL USRTPL (LUN, 1, 1) 
      GOTO 100 
                                                                        
!  ON ENCOUTERING OVERLARGE SUBSETS, EXIT GRACEFULLY (SUBSET DISCARDED) 
!  -------------------------------------------------------------------- 
                                                                        
  900 IF (IPRT.GE.0) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: SUBUPD - SUBSET LONGER THAN ANY POSSIBLE ', 'M&
     &ESSAGE {SUBSET LENGTH= ', MBYT (LUN)  + IBYT + 8, ', MESSAGE LENGT&
     &H= ', MAXBYT, '}'                                                 
      PRINT * , '>>>>>>>OVERLARGE SUBSET DISCARDED FROM FILE<<<<<<<<' 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * 
      ENDIF 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE SUBUPD                         

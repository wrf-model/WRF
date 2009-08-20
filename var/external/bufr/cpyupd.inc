      SUBROUTINE CPYUPD (LUNIT, LIN, LUN, IBYT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CPYUPD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A SUBSET FROM ONE MESSAGE BUFFER     
!   (ARRAY MBAY IN COMMON BLOCK /BITBUF/) TO ANOTHER AND/OR RESETS THE  
!   POINTERS.                                                           
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
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL CPYUPD (LUNIT, LIN, LUN, IBYT)                         
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     LIN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                FOR INPUT MESSAGE LOCATION                             
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                FOR OUTPUT MESSAGE LOCATION                            
!     IBYT     - INTEGER: NUMBER OF BYTES OCCUPIED BY THIS SUBSET       
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IUPB     MSGINI   MSGWRT       
!                               MVB      PKB                            
!    THIS ROUTINE IS CALLED BY: COPYSB                                  
!                               Normally not called by any application  
!                               programs.                               
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
                                                                        
      CHARACTER(128) BORT_STR 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  SEE IF THE NEW SUBSET FITS                                           
!  --------------------------                                           
                                                                        
      IF (MBYT (LUN) + IBYT + 8.GT.MAXBYT) THEN 
         CALL MSGWRT (LUNIT, MBAY (1, LUN), MBYT (LUN) ) 
         CALL MSGINI (LUN) 
      ENDIF 
                                                                        
      IF (MBYT (LUN) + IBYT + 8.GT.MAXBYT) GOTO 900 
                                                                        
!  TRANSFER SUBSET FROM ONE MESSAGE TO THE OTHER                        
!  ---------------------------------------------                        
                                                                        
!  .... DK: Why the -3 in "MBYT(LUN)-3" ??                              
      CALL MVB (MBAY (1, LIN), MBYT (LIN) + 1, MBAY (1, LUN), MBYT (LUN)&
      - 3, IBYT)                                                        
                                                                        
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
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: CPYUPD - THE LENGTH OF THIS SUBSET '/&
     &/'(",I6," EXCEEDS THE MAXIMUM MESSAGE LENGTH (",I6,")")') MBYT (LU&
     &N)  + IBYT + 8, MAXBYT                                            
      CALL BORT (BORT_STR) 
      END SUBROUTINE CPYUPD                         

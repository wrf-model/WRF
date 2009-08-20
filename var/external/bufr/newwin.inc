      SUBROUTINE NEWWIN (LUN, IWIN, JWIN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEWWIN (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE ....                                        
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
!                           HISTORY) (INCOMPLETE); OUTPUTS MORE         
!                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE       
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    CALL NEWWIN (LUN, IWIN, JWIN)                               
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     IWIN     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     JWIN     - INTEGER: ....                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     LSTRPC                         
!    THIS ROUTINE IS CALLED BY: CONWIN   DRSTPL   UFBRW                 
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
      NFILES)                                                           
                                                                        
      CHARACTER(128) BORT_STR 
      REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (IWIN.EQ.1) THEN 
      JWIN = NVAL (LUN) 
      GOTO 100 
      ENDIF 
                                                                        
!  REFIND THE JWIN BOUNDARY FROM IWIN                                   
!  ----------------------------------                                   
                                                                        
      NODE = INV (IWIN, LUN) 
      IF (LSTRPC (NODE, LUN) .NE.NODE) GOTO 900 
      JWIN = IWIN + VAL (IWIN, LUN) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NEWWIN - LISTRPC FOR NODE",I6,'//'" (&
     &LISTRPC=",I5,") DOES NOT EQUAL VALUE OF NODE, NOT RPC '//'(IWIN ="&
     &,I8,")")') NODE, LSTRPC (NODE, LUN) , IWIN                        
      CALL BORT (BORT_STR) 
      END SUBROUTINE NEWWIN                         

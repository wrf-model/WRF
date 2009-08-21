      SUBROUTINE GETWIN (NODE, LUN, IWIN, JWIN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GETWIN (docblock incomplete)                           
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
! USAGE:    CALL GETWIN (NODE, LUN, IWIN, JWIN)                         
!   INPUT ARGUMENT LIST:                                                
!     NODE     - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     IWIN     - INTEGER: ....                                          
!     JWIN     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IWIN     - INTEGER: ....                                          
!     JWIN     - INTEGER: ....                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     INVWIN   LSTRPC                
!    THIS ROUTINE IS CALLED BY: CONWIN   UFBEVN   UFBIN3   UFBRW        
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
      CHARACTER(128) BORT_STR 
!     REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IRPC = LSTRPC (NODE, LUN) 
                                                                        
      IF (IRPC.EQ.0) THEN 
      IWIN = INVWIN (NODE, LUN, JWIN, NVAL (LUN) ) 
      IF (IWIN.EQ.0.and.jwin.gt.1) GOTO 100 
      IWIN = 1 
      JWIN = NVAL (LUN) 
      GOTO 100 
      ELSE 
      IWIN = INVWIN (IRPC, LUN, JWIN, NVAL (LUN) ) 
      IF (IWIN.EQ.0) THEN 
      GOTO 100 
      ELSEIF (VAL (IWIN, LUN) .EQ.0.) THEN 
      IWIN = 0 
      GOTO 100 
      ENDIF 
      ENDIF 
                                                                        
      JWIN = INVWIN (IRPC, LUN, IWIN + 1, NVAL (LUN) ) 
      IF (JWIN.EQ.0) GOTO 900 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: GETWIN - SEARCHED BETWEEN",I5," AND"'&
     &//',I5,", MISSING BRACKET")') IWIN + 1, NVAL (LUN)                
      CALL BORT (BORT_STR) 
      END SUBROUTINE GETWIN                         

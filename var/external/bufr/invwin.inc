      FUNCTION INVWIN (NODE, LUN, INV1, INV2) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    INVWIN (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION ....                                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE); OUTPUTS MORE         
!                           COMPLETE DIAGNOSTIC INFO WHEN UNUSUAL       
!                           THINGS HAPPEN                               
!                                                                       
! USAGE:    INVWIN (NODE, LUN, INV1, INV2)                              
!   INPUT ARGUMENT LIST:                                                
!     NODE     - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INV1     - INTEGER: ....                                          
!     INV2     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     INVWIN   - INTEGER: ....                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: CONWIN   DRSTPL   GETWIN   NEVN         
!                               TRYBUMP  UFBGET   UFBRW    UFBSEQ       
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
      COMMON / QUIET / IPRT 
                                                                        
      REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      INVWIN = 0 
      IF (NODE.EQ.0) GOTO 200 
                                                                        
!  SEARCH BETWEEN INV1 AND INV2                                         
!  ----------------------------                                         
                                                                        
   10 DO INVWIN = INV1, INV2 
      IF (INV (INVWIN, LUN) .EQ.NODE) GOTO 100 
      ENDDO 
                                                                        
      INVWIN = 0 
                                                                        
  200 IF (IPRT.GE.2) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: INVWIN - INVWIN RETURNING WITH VALUE OF 0' 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION INVWIN                           

      FUNCTION NVNWIN (NODE, LUN, INV1, INV2, INVN, NMAX) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NVNWIN (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION ....                                          
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
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE); OUTPUTS MORE         
!                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE       
!                           TERMINATES ABNORMALLY OR UNUSUAL THINGS     
!                           HAPPEN                                      
!                                                                       
! USAGE:    NVNWIN (NODE, LUN, INV1, INV2, INVN, NMAX)                  
!   INPUT ARGUMENT LIST:                                                
!     NODE     - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INV1     - INTEGER: ....                                          
!     INV2     - INTEGER: ....                                          
!     NMAX     - INTEGER: LENGTH OF INVN                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     INVN     - INTEGER: NMAX-WORD ARRAY ....                          
!     NVNWIN   - INTEGER: ....                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT                                    
!    THIS ROUTINE IS CALLED BY: UFBEVN                                  
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
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(128) BORT_STR 
      DIMENSION INVN (NMAX) 
!     REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (NODE.EQ.0) THEN 
         IF (IPRT.GE.1) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * , 'BUFRLIB: NVNWIN - NODE=0, IMMEDIATE RETURN' 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ENDIF 
                                                                        
!  .... DK: Shouldn't this be before RETURN above?                      
      NVNWIN = 0 
                                                                        
      DO I = 1, NMAX 
!     INVN (I) = BMISS 
      INVN (I) = -9999999 ! for gfortran compiler- xz
      ENDDO 
                                                                        
!  SEARCH BETWEEN INV1 AND INV2                                         
!  ----------------------------                                         
                                                                        
      DO N = INV1, INV2 
      IF (INV (N, LUN) .EQ.NODE) THEN 
         NVNWIN = NVNWIN + 1 
         INVN (NVNWIN) = N 
      ENDIF 
      ENDDO 
                                                                        
!  .... DK: Shouldn't this check be moved into do loop above to         
!            prevent array overflow in INVN ????                        
      IF (NVNWIN.GT.NMAX) GOTO 900 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: NVNWIN - THE NUMBER OF EVENTS, '//   &
      'NVNWIN (",I5,") EXCEEDS THE LIMIT, NMAX (",I5,")")') NVNWIN,     &
      NMAX                                                              
      CALL BORT (BORT_STR) 
      END FUNCTION NVNWIN                           

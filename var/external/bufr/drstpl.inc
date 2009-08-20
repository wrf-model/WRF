      SUBROUTINE DRSTPL (INOD, LUN, INV1, INV2, INVN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    DRSTPL (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE ....                                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT" (LATER REMOVED, UNKNOWN      
!                           WHEN)                                       
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES        
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE)                       
!                                                                       
! USAGE:    CALL DRSTPL (INOD, LUN, INV1, INV2, INVN)                   
!   INPUT ARGUMENT LIST:                                                
!     INOD     - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INV1     - INTEGER: ....                                          
!     INV2     - INTEGER: ....                                          
!     INVN     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     INVN     - INTEGER: ....                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        INVWIN   NEWWIN   USRTPL                
!    THIS ROUTINE IS CALLED BY: UFBRW                                   
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
      MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
      IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
      ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      CHARACTER(10) TAG 
      CHARACTER(3) TYP 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
    1 NODE = INOD 
    2 NODE = JMPB (NODE) 
      IF (NODE.EQ.0) GOTO 100 
      IF (TYP (NODE) .EQ.'DRS'.OR.TYP (NODE) .EQ.'DRB') THEN 
      INVN = INVWIN (NODE, LUN, INV1, INV2) 
      IF (INVN.GT.0) THEN 
      CALL USRTPL (LUN, INVN, 1) 
      CALL NEWWIN (LUN, INV1, INV2) 
      INVN = INVWIN (INOD, LUN, INVN, INV2) 
      IF (INVN.GT.0) GOTO 100 
      GOTO 1 
      ENDIF 
      ENDIF 
      GOTO 2 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE DRSTPL                         

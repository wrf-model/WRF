      SUBROUTINE MRGINV 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MRGINV                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PRINTS A SUMMARY OF MERGE ACTIVITY.         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN INVMRG)     
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS              
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    CALL MRGINV                                                 
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / MRGCOM / NRPL, NMRG, NAMB, NTOT 
      COMMON / QUIET / IPRT 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (IPRT.GE.0) THEN 
      PRINT * , '+++++++++++++++++++++++BUFRLIB+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , '-------------------------------------------------------&
     &'                                                                 
      PRINT * , 'INVENTORY FROM MERGE PROCESS IN BUFRLIB ROUTINE INVMRG &
     &'                                                                 
      PRINT * , '-------------------------------------------------------&
     &'                                                                 
      PRINT * , 'NUMBER OF DRB EXPANSIONS  = ', NRPL 
      PRINT * , 'NUMBER OF MERGES          = ', NMRG 
         PRINT * , 'NUMBER THAT ARE AMBIGUOUS = ', NAMB 
      PRINT * , '-------------------------------------------------------&
     &'                                                                 
      PRINT * , 'TOTAL NUMBER OF VISITS    = ', NTOT 
      PRINT * , '-------------------------------------------------------&
     &'                                                                 
      PRINT * , '+++++++++++++++++++++++BUFRLIB+++++++++++++++++++++++++&
     &'                                                                 
      ENDIF 
                                                                        
      RETURN 
      END SUBROUTINE MRGINV                         

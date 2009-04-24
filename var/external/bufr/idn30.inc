      FUNCTION IDN30 (ADN30, L30) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IDN30                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CONVERTS A DESCRIPTOR FROM ITS FIVE OR SIX    
!   CHARACTER ASCII REPRESENTATION TO ITS BIT-WISE (INTEGER)            
!   REPRESENTATION.                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
!                                                                       
! USAGE:    IDN30 (ADN30, L30)                                          
!   INPUT ARGUMENT LIST:                                                
!     ADN30    - CHARACTER*(*): CHARACTER FORM OF DESCRIPTOR (FXY       
!                VALUE)                                                 
!     L30      - INTEGER: LENGTH OF ADN30 (NUMBER OF CHARACTERS, 5 OR   
!                6)                                                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IDN      - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)   
!                VALUE                                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ADN30    BORT     IFXY                  
!    THIS ROUTINE IS CALLED BY: RDBFDX                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / HRDWRD / NBYTW, NBITW, NREV, IORD (8) 
                                                                        
      CHARACTER ( * ) ADN30 
      CHARACTER(128) BORT_STR 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (LEN (ADN30) .LT.L30) GOTO 900 
      IF (L30.EQ.5) THEN 
      READ (ADN30, '(I5)') IDN30 
      IF (IDN30.LT.0.OR.IDN30.GT.65535) GOTO 901 
      ELSEIF (L30.EQ.6) THEN 
      IDN30 = IFXY (ADN30) 
      ELSE 
      GOTO 902 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A,'//&
      '" CHARACTER LENGTH (",I4,") IS TOO SHORT (< L30,",I5)') ADN30,   &
      LEN (ADN30) , L30                                                 
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: IDN30 - DESCRIPTOR INTEGER '//'REPRES&
     &ENTATION, IDN30 (",I8,"), IS OUTSIDE 16-BIT RANGE '//'(0-65535)")'&
     &) IDN30                                                           
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A,'//&
      '" CHARACTER LENGTH (",I4,") MUST BE EITHER 5 OR 6")') ADN30, L30 
      CALL BORT (BORT_STR) 
      END FUNCTION IDN30                            

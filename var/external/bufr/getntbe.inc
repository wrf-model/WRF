      SUBROUTINE GETNTBE (LUNT, IFXYN, LINE, IRET) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GETNTBE                                                
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE GETS THE FIRST LINE OF THE NEXT ENTRY IN   
!   THE SPECIFIED ASCII MASTER TABLE B OR MASTER TABLE D FILE.  THIS    
!   LINE CONTAINS, AMONG OTHER THINGS, THE FXY NUMBER CORRESPONDING TO  
!   THIS ENTRY.                                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL GETNTBE ( LUNT, IFXYN, LINE, IRET )                    
!   INPUT ARGUMENT LIST:                                                
!     LUNT     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING MASTER TABLE B OR MASTER TABLE D INFORMATION
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR     
!                NEXT TABLE ENTRY                                       
!     LINE     - CHARACTER*(*): FIRST LINE OF NEXT TABLE ENTRY          
!     IRET     - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = end-of-file encountered while reading       
!                           from LUNT                                   
!                      -2 = I/O error encountered while reading         
!                           from LUNT                                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT2    IGETNTBL IGETFXY  IFXY         
!                               PARSTR                                  
!    THIS ROUTINE IS CALLED BY: RDMTBB   RDMTBD                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) LINE 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(20) TAGS (4) 
      CHARACTER(6) ADSC 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!       Get the first line of the next entry in the file.               
                                                                        
      IRET = IGETNTBL (LUNT, LINE) 
      IF (IRET.EQ.0) THEN 
                                                                        
!           The first field within this line should contain the         
!           FXY number.                                                 
                                                                        
         CALL PARSTR (LINE (1:20) , TAGS, 4, NTAG, '|', .FALSE.) 
         IF (NTAG.LT.1) GOTO 900 
         IF (IGETFXY (TAGS (1), ADSC) .NE.0) GOTO 900 
                                                                        
!           Store the bit-wise representation of the FXY number.        
                                                                        
         IFXYN = IFXY (ADSC) 
      ENDIF 
                                                                        
      RETURN 
                                                                        
  900 BORT_STR1 = 'BUFRLIB: GETNTBE - CARD BEGINNING WITH: '//LINE (1:  &
      20)                                                               
      BORT_STR2 = '                  HAS BAD OR MISSING FXY NUMBER' 
      CALL BORT2 (BORT_STR1, BORT_STR2) 
                                                                        
      END SUBROUTINE GETNTBE                        

      SUBROUTINE GETTBH (LUNS, LUNL, TAB, IMT, IMTV, IOGCE, ILTV) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GETTBH                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE READS AND PARSES THE HEADER LINES FROM TWO 
!   SEPARATE (I.E. ONE STANDARD AND ONE LOCAL) ASCII FILES CONTAINING   
!   EITHER MASTER TABLE B OR MASTER TABLE D INFORMATION.                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL GETTBH ( LUNS, LUNL, TAB, IMT, IMTV, IOGCE, ILTV )     
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     LUNS     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING STANDARD TABLE INFORMATION                  
!     LUNL     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING LOCAL TABLE INFORMATION                     
!     TAB      - CHARACTER*1: TABLE TYPE ('B' OR 'D')                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IMT      - INTEGER: MASTER TABLE, READ FROM EACH ASCII FILE       
!                (NOTE: THESE VALUES MUST BE THE SAME IN EACH FILE!)    
!     IMTV     - INTEGER: VERSION NUMBER OF MASTER TABLE, READ FROM     
!                STANDARD ASCII FILE                                    
!     IOGCE    - INTEGER: ORIGINATING CENTER, READ FROM LOCAL ASCII FILE
!     ILTV     - INTEGER: VERSION NUMBER OF LOCAL TABLE, READ FROM      
!                LOCAL ASCII FILE                                       
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IGETNTBL PARSTR   VALX         
!    THIS ROUTINE IS CALLED BY: RDMTBB   RDMTBD                         
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(40) HEADER 
      CHARACTER(30) TAGS (5), LABEL 
      CHARACTER(3) CFTYP 
      CHARACTER(2) CTTYP 
      CHARACTER(1) TAB 
                                                                        
      LOGICAL BADLABEL 
                                                                        
!-----------------------------------------------------------------------
!       Statement function to check for bad header line label:          
                                                                        
      BADLABEL (LABEL) = ( (INDEX (LABEL, CTTYP) .EQ.0) .OR. (INDEX (   &
      LABEL, CFTYP) .EQ.0) )                                            
!-----------------------------------------------------------------------
                                                                        
      CTTYP = TAB//' ' 
                                                                        
!       Read and parse the header line of the standard file.            
                                                                        
      CFTYP = 'STD' 
      IF (IGETNTBL (LUNS, HEADER) .NE.0) GOTO 900 
      CALL PARSTR (HEADER, TAGS, 5, NTAG, '|', .FALSE.) 
      IF (NTAG.LT.3) GOTO 900 
      IF (BADLABEL (TAGS (1) ) ) GOTO 900 
      IMT = VALX (TAGS (2) ) 
      IMTV = VALX (TAGS (3) ) 
                                                                        
!       Read and parse the header line of the local file.               
                                                                        
      CFTYP = 'LOC' 
      IF (IGETNTBL (LUNL, HEADER) .NE.0) GOTO 900 
      CALL PARSTR (HEADER, TAGS, 5, NTAG, '|', .FALSE.) 
      IF (NTAG.LT.4) GOTO 900 
      IF (BADLABEL (TAGS (1) ) ) GOTO 900 
      IMT2 = VALX (TAGS (2) ) 
      IOGCE = VALX (TAGS (3) ) 
      ILTV = VALX (TAGS (4) ) 
                                                                        
!       Verify that both files are for the same master table.           
                                                                        
      IF (IMT.NE.IMT2) GOTO 901 
                                                                        
      RETURN 
                                                                        
  900 WRITE (BORT_STR, '("BUFRLIB: GETTBH - BAD OR MISSING HEADER '//   &
      'WITHIN ",A," TABLE ",A)') CFTYP, TAB                             
      CALL BORT (BORT_STR) 
  901 WRITE (BORT_STR, '("BUFRLIB: GETTBH - MASTER TABLE NUMBER '//     &
      'MISMATCH BETWEEN STD AND LOC TABLE ",A)') TAB                    
      CALL BORT (BORT_STR) 
      END SUBROUTINE GETTBH                         

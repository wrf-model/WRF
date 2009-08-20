      SUBROUTINE UFBQCD (LUNIT, NEMO, QCD) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBQCD                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS IN A MNEMONIC KNOWN TO BE IN THE BUFR 
!   TABLE ASSOCIATED WITH THE BUFR FILE IN LOGICAL UNIT LUNIT, AND      
!   RETURNS THE DESCRIPTOR ENTRY (Y) ASSOCIATED WITH IT WHEN THE FXY    
!   DESCRIPTOR IS A SEQUENCE DESCRIPTOR (F=3) WITH TABLE D CATEGORY 63  
!   (X=63).  THIS ROUTINE WILL NOT WORK FOR ANY OTHER TYPE OF           
!   DESCRIPTOR OR ANY OTHER SEQUENCE DESCRIPTOR TABLE D CATEGORY.       
!   LUNIT MUST ALREADY BE OPENED FOR INPUT OR OUTPUT VIA A CALL TO      
!   OPENBF.  THIS ROUTINE IS ESPECIALLY USEFUL WHEN THE CALLING PROGRAM 
!   IS WRITING "EVENTS" TO AN OUTPUT BUFR FILE (USUALLY THE "PREPBUFR"  
!   FILE) USING THE SAME BUFR TABLE SINCE THE DESCRIPTOR ENTRY (Y) HERE 
!   DEFINES THE EVENT PROGRAM CODE.  THUS, THE CALLING PROGRAM CAN PASS 
!   THE PROGRAM CODE INTO VARIOUS EVENTS WITHOUT ACTUALLY KNOWING ITS   
!   VALUE AS LONG AS IT KNOWS THE MNEMONIC NAME ASSOCIATED WITH IT.     
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
! USAGE:    CALL UFBQCD (LUNIT, NEMO, QCD)                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                (ASSOCIATED BUFR TABLE MAY BE INTERNAL OR EXTERNAL)    
!     NEMO     - CHARACTER*(*): MNEMONIC                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     QCD      - REAL: SEQUENCE DESCRIPTOR ENTRY (I.E., EVENT PROGRAM   
!                CODE) IN BUFR TABLE ASSOCIATED WITH NEMO (Y IN FXY     
!                DESCRIPTOR, WHERE F=3 AND X=63)                        
!                                                                       
! REMARKS:                                                              
!    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE     
!    UFBQCP.                                                            
!                                                                       
!    THIS ROUTINE CALLS:        ADN30    BORT     NEMTAB   STATUS       
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) NEMO 
      CHARACTER(128) BORT_STR 
      CHARACTER(6) FXY, ADN30 
      CHARACTER(1) TAB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
                                                                        
      CALL NEMTAB (LUN, NEMO, IDN, TAB, IRET) 
      IF (TAB.NE.'D') GOTO 901 
                                                                        
      FXY = ADN30 (IDN, 6) 
      IF (FXY (2:3) .NE.'63') GOTO 902 
      READ (FXY (4:6) , '(F3.0)', ERR = 903) QCD 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UFBQCD - BUFR FILE IS CLOSED, IT MUST BE'//' &
     &OPEN')                                                            
  901 WRITE (BORT_STR, '("BUFRLIB: UFBQCD - INPUT MNEMONIC ",A," NOT '//&
      'DEFINED AS A SEQUENCE DESCRIPTOR IN BUFR TABLE")') NEMO          
      CALL BORT (BORT_STR) 
  902 WRITE (BORT_STR, '("BUFRLIB: UFBQCD - BUFR TABLE SEQ. DESCRIPTOR '&
     &//'ASSOC. WITH INPUT MNEMONIC ",A," HAS INVALID CATEGORY ",A," -'/&
     &/' CATEGORY MUST BE 63")') NEMO, FXY (2:3)                        
      CALL BORT (BORT_STR) 
  903 WRITE (BORT_STR, '("BUFRLIB: UFBQCD - ERROR READING ENTRY '//'(PRO&
     &GRAM CODE) FROM BUFR TBL SEQ. DESCRIPTOR ",A," ASSOC. '//'WITH INP&
     &UT MNEM. ",A)') FXY, NEMO                                         
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBQCD                         

      SUBROUTINE UFBQCP (LUNIT, QCP, NEMO) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBQCP                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS IN A FXY DESCRIPTOR ENTRY (Y) FOR A   
!   SEQUENCE DESCRIPTOR (F=3) WITH TABLE D CATEGORY 63 (X=63) WHEN THE  
!   DESCRIPTOR IS KNOWN TO BE IN THE BUFR TABLE IN LOGICAL UNIT LUNIT,  
!   AND RETURNS THE MNEMONIC ASSOCIATED WITH IT.  THIS ROUTINE WILL NOT 
!   WORK FOR ANY OTHER TYPE OF DESCRIPTOR OR ANY OTHER SEQUENCE         
!   DESCRIPTOR TABLE D CATEGORY.  LUNIT MUST ALREADY BE OPENED FOR      
!   INPUT OR OUTPUT VIA A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE       
!   OPENBF.  THIS ROUTINE IS ESPECIALLY USEFUL WHEN THE CALLING PROGRAM 
!   IS READING "EVENTS" FROM AN INPUT BUFR FILE IN LUNIT (USUALLY THE   
!   "PREPBUFR" FILE) SINCE THE DESCRIPTOR ENTRY (Y) HERE DEFINES THE    
!   EVENT PROGRAM CODE.  THUS, THE CALLING PROGRAM CAN OBTAIN THE       
!   MNEMONIC NAME ASSOCIATED WITH AN EVENT PROGRAM CODE.                
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
! USAGE:    CALL UFBQCP (LUNIT, QCP, NEMO)                              
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                (ASSOCIATED BUFR TABLE MAY BE INTERNAL OR EXTERNAL)    
!     QCP      - REAL: SEQUENCE DESCRIPTOR ENTRY (I.E., EVENT PROGRAM   
!                CODE) (Y IN FXY DESCRIPTOR)                            
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NEMO     - CHARACTER*(*): MNEMONIC IN BUFR TABLE ASSOCIATED WITH  
!                SEQUENCE DESCRIPTOR FXY WHERE F=3 AND X=63 AND         
!                Y=INT(QCP)                                             
!                                                                       
! REMARKS:                                                              
!    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE     
!    UFBQCD.                                                            
!                                                                       
!    THIS ROUTINE CALLS:        BORT     IFXY     NUMTAB   STATUS       
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
      CHARACTER(1) TAB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
                                                                        
      IDN = IFXY ('363000') + IFIX (QCP) 
!  .... get NEMO from IDN                                               
      CALL NUMTAB (LUN, IDN, NEMO, TAB, IRET) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: UFBQCP - BUFR FILE IS CLOSED, IT MUST BE'//' &
     &OPEN')                                                            
      END SUBROUTINE UFBQCP                         

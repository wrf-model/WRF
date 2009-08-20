      SUBROUTINE MINIMG (LUNIT, MINI) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MINIMG                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE PACKS THE VALUE OF MINI INTO SECTION 1 OF   
!   THE BUFR MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR LUNIT,    
!   SO THAT THIS VALUE THEN BECOMES THE MINUTES COMPONENT OF THE        
!   SECTION 1 DATE-TIME FOR THE MESSAGE.  THIS SUBROUTINE SHOULD ONLY   
!   BE CALLED WHEN LOGICAL UNIT LUNIT HAS BEEN OPENED FOR OUTPUT        
!   OPERATIONS.                                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN MSGINI)     
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT" (IN PARENT ROUTINE MSGINI)   
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI) (IN PARENT        
!                           ROUTINE MSGINI)                             
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES (IN PARENT ROUTINE   
!                           MSGINI)                                     
! 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE     
!                           PORTABILITY TO OTHER PLATFORMS              
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE PKBS1                                   
!                                                                       
! USAGE:    CALL MINIMG (LUNIT, MINI)                                   
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     MINI     - INTEGER: MINUTES VALUE TO BE PACKED                    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     PKBS1    STATUS                
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.LT.0) GOTO 901 
      IF (IM.EQ.0) GOTO 902 
                                                                        
      CALL PKBS1 (MINI, MBAY (1, LUN) , 'MINU') 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  901 CALL BORT ('BUFRLIB: MINIMG - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
  902 CALL BORT ('BUFRLIB: MINIMG - A MESSAGE MUST BE OPEN IN OUTPUT '//&
      'BUFR FILE, NONE ARE')                                            
      END SUBROUTINE MINIMG                         

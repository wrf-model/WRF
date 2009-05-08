      SUBROUTINE COPYBF (LUNIN, LUNOT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    COPYBF                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES AN ENTIRE BUFR FILE FROM LOGICAL     
!   UNIT LUNIN TO LOGICAL UNIT LUNOT.                                   
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE RDMSGW AND NMWRD                        
!                                                                       
! USAGE:    CALL COPYBF (LUNIN, LUNOT)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUNOT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIN" - BUFR FILE                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT "LUNOT" - BUFR FILE                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     NMWRD    RDMSGW   STATUS       
!                               WRDLEN                                  
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      DIMENSION MBAY (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION      
!  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)         
!  ---------------------------------------------------------------      
                                                                        
      CALL WRDLEN 
                                                                        
!  CHECK BUFR FILE STATUSES                                             
!  ------------------------                                             
                                                                        
      CALL STATUS (LUNIN, LUN, IL, IM) 
      IF (IL.NE.0) GOTO 900 
      CALL STATUS (LUNOT, LUN, IL, IM) 
      IF (IL.NE.0) GOTO 901 
                                                                        
      REWIND (LUNIN) 
      REWIND (LUNOT) 
                                                                        
!  READ AND COPY A BUFR FILE ON UNIT LUNIN TO UNIT LUNOT                
!  -----------------------------------------------------                
                                                                        
    1 CALL RDMSGW (LUNIN, MBAY, IER) 
      IF (IER.EQ. - 1) GOTO 2 
      IF (IER.EQ. - 2) GOTO 902 
      WRITE (LUNOT, ERR = 903) (MBAY (I), I = 1, NMWRD (MBAY) ) 
      GOTO 1 
                                                                        
    2 CLOSE (LUNIN) 
      CLOSE (LUNOT) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: COPYBF - INPUT BUFR FILE IS OPEN, IT MUST BE &
     &CLOSED')                                                          
  901 CALL BORT ('BUFRLIB: COPYBF - OUTPUT BUFR FILE IS OPEN, IT MUST BE&
     & CLOSED')                                                         
  902 CALL BORT ('BUFRLIB: COPYBF - ERROR READING BUFR FILE') 
  903 CALL BORT ('BUFRLIB: COPYBF - ERROR WRITING BUFR FILE') 
      END SUBROUTINE COPYBF                         

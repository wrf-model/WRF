      SUBROUTINE STRSUC (STR1, STR2, LENS) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    STRSUC                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE REMOVES LEADING AND TRAILING BLANKS FROM A  
!   STRING.                                                             
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; ADDED MORE COMPLETE          
!                           DIAGNOSTIC INFO WHEN UNUSUAL THINGS HAPPEN  
!                                                                       
! USAGE:    CALL STRSUC (STR1, STR2, LENS)                              
!   INPUT ARGUMENT LIST:                                                
!     STR1     - CHARACTER*(*): STRING                                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR2     - CHARACTER*(*): COPY OF STR1 WITH LEADING AND TRAILING  
!                BLANKS REMOVED                                         
!     LENS     - INTEGER: LENGTH OF STR2:                               
!                      -1 = STR1 contained embedded blanks              
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: DXDUMP   STRNUM                         
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR1, STR2 
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      LENS = 0 
      LSTR = LEN (STR1) 
                                                                        
!  FIND THE FIRST NON-BLANK IN THE INPUT STRING                         
!  --------------------------------------------                         
                                                                        
      DO I = 1, LSTR 
      IF (STR1 (I:I) .NE.' ') GOTO 2 
      ENDDO 
      GOTO 100 
                                                                        
!     Now, starting with the first non-blank in the input string,       
!     copy characters from the input string into the output string      
!     until reaching the next blank in the input string.                
                                                                        
    2 DO J = I, LSTR 
      IF (STR1 (J:J) .EQ.' ') GOTO 3 
      LENS = LENS + 1 
      STR2 (LENS:LENS) = STR1 (J:J) 
      ENDDO 
      GOTO 100 
                                                                        
!     Now, continuing on within the input string, make sure that        
!     there are no more non-blank characters.  If there are, then       
!     the blank at which we stopped copying from the input string       
!     into the output string was an embedded blank.                     
                                                                        
    3 DO I = J, LSTR 
      IF (STR1 (I:I) .NE.' ') LENS = - 1 
      ENDDO 
                                                                        
      IF (LENS.EQ. - 1.AND.IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: STRSUC - INPUT STRING ', STR1, ' CONTAINS ', '&
     &1 OR MORE EMBEDDED BLANKS -  RETURN WITH LENS = -1'               
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE STRSUC                         

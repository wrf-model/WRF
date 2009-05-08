      SUBROUTINE DATEBF (LUNIT, MEAR, MMON, MDAY, MOUR, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    DATEBF                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS THE SECTION 1 DATE IN THE FIRST     
!   NON-DICTIONARY BUFR MESSAGE IN LOGICAL UNIT LUNIT, REGARDLESS OF    
!   THE NUMBER OF SUBSETS IN THE MESSAGE.  LUNIT SHOULD NOT BE          
!   PREVIOUSLY OPENED TO THE BUFR INTERFACE.                            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT                                   
! 1998-08-31  J. WOOLLEN -- MODIFIED TO CORRECT AN ERROR WHICH LEAD TO  
!                           THE YEAR BEING RETURNED IN "MEAR" AS 2-     
!                           DIGIT YEAR WHEN A 4-DIGIT YEAR WAS          
!                           REQUESTED VIA A PRIOR CALL TO DATELEN (THE  
!                           CENTER DATE RETURNED IN "IDATE", IN THE     
!                           FORM YYYYMMDDHH, WAS CORRECT IN THE         
!                           PREVIOUS VERSION OF THIS ROUTINE            
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRCT PROBLEMS CAUSED BY IN-   
!                           LINING CODE WITH FPP DIRECTIVES             
! 2003-05-19  M. SHIREY  -- REPLACED CALLS TO FORTRAN INSRINSIC         
!                           FUNCTION ICHAR WITH THE NCEP W3LIB C-       
!                           FUNCTION MOVA2I BECAUSE ICHAR DOES NOT WORK 
!                           PROPERLY ON SOME MACHINES (E.G., IBM FROST/ 
!                           SNOW) (NOTE: ON 2003-??-??, MOVA2I WAS      
!                           ADDED TO THE BUFRLIB AS A FORTRAN FUNCTION) 
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MODIFIED DATE CALCULATIONS TO NO LONGER     
!                           USE FLOATING POINT ARITHMETIC SINCE THIS    
!                           CAN LEAD TO ROUND OFF ERROR AND AN IMPROPER 
!                           RESULTING DATE ON SOME MACHINES (E.G., NCEP 
!                           IBM FROST/SNOW), INCREASES PORTABILITY;     
!                           UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY OR UNUSUAL THINGS     
!                           HAPPEN                                      
! 2004-08-18  J. ATOR    -- MODIFIED 'BUFR' STRING TEST FOR PORTABILITY 
!                           TO EBCDIC MACHINES                          
! 2004-12-20  D. KEYSER  -- CALLS WRDLEN TO INITIALIZE LOCAL MACHINE    
!                           INFORMATION (IN CASE IT HAS NOT YET BEEN    
!                           CALLED), THIS ROUTINE DOES NOT REQUIRE IT   
!                           BUT 2004-08-18 CHANGE CALLS OTHER ROUTINES  
!                           THAT DO REQUIRE IT                          
! 2005-11-29  J. ATOR    -- USE IGETDATE, IUPBS01 AND RDMSGW            
!                                                                       
! USAGE:    CALL  DATEBF (LUNIT, MEAR, MMON, MDAY, MOUR, IDATE)         
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MEAR     - INTEGER: SECTION 1 YEAR (YYYY OR YY, DEPENDING ON      
!                DATELEN() VALUE                                        
!     MMON     - INTEGER: SECTION 1 MONTH MM                            
!     MDAY     - INTEGER: SECTION 1 DAY DD                              
!     MOUR     - INTEGER: SECTION 1 HOUR HH                             
!     IDATE    - INTEGER: DATE-TIME FROM SECTION 1 OF BUFR MESSAGE IN   
!                FORMAT OF EITHER YYMMDDHH OR YYYYMMDDHH, DEPENDING ON  
!                DATELEN() VALUE; OR -1 IF SECTION 1 DATE COULD NOT BE  
!                LOCATED                                                
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IGETDATE  IUPBS01  RDMSGW      
!                               STATUS   WRDLEN                         
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
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      DIMENSION MBAY (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION      
!  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)         
!  ---------------------------------------------------------------      
                                                                        
      CALL WRDLEN 
                                                                        
      IDATE = - 1 
                                                                        
!  SEE IF THE FILE IS ALREADY OPEN TO BUFR INTERFACE (A NO-NO)          
!  -----------------------------------------------------------          
                                                                        
      CALL STATUS (LUNIT, LUN, JL, JM) 
      IF (JL.NE.0) GOTO 900 
                                                                        
!  READ TO A DATA MESSAGE AND PICK OUT THE DATE                         
!  --------------------------------------------                         
                                                                        
      REWIND LUNIT 
                                                                        
    1 CALL RDMSGW (LUNIT, MBAY, IER) 
      IF (IER.LT.0) GOTO 100 
      IF (IUPBS01 (MBAY, 'MTYP') .EQ.11) GOTO 1 
                                                                        
      IDATE = IGETDATE (MBAY, MEAR, MMON, MDAY, MOUR) 
                                                                        
  100 IF (IPRT.GE.1.AND.IDATE.EQ. - 1) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * , 'BUFRLIB: DATEBF - SECTION 1 DATE COULD NOT BE ',    &
         'LOCATED - RETURN WITH IDATE = -1'                             
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: DATEBF - INPUT BUFR FILE IS OPEN, IT MUST BE &
     &CLOSED')                                                          
      END SUBROUTINE DATEBF                         

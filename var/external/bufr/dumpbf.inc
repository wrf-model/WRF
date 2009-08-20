      SUBROUTINE DUMPBF (LUNIT, JDATE, JDUMP) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    DUMPBF                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-12-11           
!                                                                       
! ABSTRACT: THIS SUBROUTINE RETURNS THE SECTION 1 DATE IN THE FIRST     
!   TWO NON-DICTIONARY BUFR MESSAGES IN LOGICAL UNIT LUNIT WHICH        
!   CONTAIN ZERO SUBSETS.  NORMALLY, THESE "DUMMY" MESSAGES APPEAR      
!   ONLY IN DATA DUMP FILES AND ARE IMMEDIATELY AFTER THE DICTIONARY    
!   MESSAGES.  THEY CONTAIN A DUMP "CENTER TIME" AND A DUMP FILE        
!   "PROCESSING TIME", RESPECTIVELY.  LUNIT SHOULD NOT BE PREVIOUSLY    
!   OPENED TO THE BUFR INTERFACE.                                       
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1996-12-11  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1996-12-17  J. WOOLLEN -- CORRECTED ERROR IN DUMP DATE READER         
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT                                   
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
! 2005-11-29  J. ATOR    -- USE IUPBS01, IGETDATE, GETLENS AND RDMSGW   
!                                                                       
! USAGE:    CALL DUMPBF (LUNIT, JDATE, JDUMP)                           
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     JDATE    - INTEGER: 5-WORD ARRAY CONTAINING THE YEAR              
!                (YYYY OR YY, DEPENDING ON DATELEN() VALUE),            
!                MONTH, DAY, HOUR AND MINUTE FROM SECTION 1 OF THE      
!                FIRST NON-DICTIONARY BUFR MESSAGE WITH ZERO SUBSETS    
!                (NORMALLY THE DATA DUMP CENTER TIME IN A DATA DUMP     
!                FILE); OR 5*-1 IF THIS COULD NOT BE LOCATED            
!     JDUMP    - INTEGER: 5-WORD ARRAY CONTAINING THE YEAR              
!                (YYYY OR YY, DEPENDING ON DATELEN() VALUE),            
!                MONTH, DAY, HOUR AND MINUTE FROM SECTION 1 OF THE      
!                SECOND NON-DICTIONARY BUFR MESSAGE WITH ZERO SUBSETS   
!                (NORMALLY THE FILE PROCESSING TIME IN A DATA DUMP      
!                FILE); OR 5*-1 IF THIS COULD NOT BE LOCATED            
!                                                                       
!   INPUT FILES:                                                        
!     UNIT "LUNIT" - BUFR FILE                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     GETLENS  IGETDATE IUPB         
!                               IUPBS01  RDMSGW   STATUS   WRDLEN       
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
      DIMENSION JDATE (5), JDUMP (5) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION      
!  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)         
!  ---------------------------------------------------------------      
                                                                        
      CALL WRDLEN 
                                                                        
      DO I = 1, 5 
      JDATE (I) = - 1 
      JDUMP (I) = - 1 
      ENDDO 
                                                                        
!  SEE IF THE FILE IS ALREADY OPEN TO BUFR INTERFACE (A NO-NO)          
!  -----------------------------------------------------------          
                                                                        
      CALL STATUS (LUNIT, LUN, JL, JM) 
      IF (JL.NE.0) GOTO 900 
                                                                        
!  READ PAST ANY DICTIONARY MESSAGES                                    
!  ---------------------------------                                    
                                                                        
      REWIND LUNIT 
                                                                        
    1 CALL RDMSGW (LUNIT, MBAY, IER) 
      IF (IER.LT.0) GOTO 200 
      IF (IUPBS01 (MBAY, 'MTYP') .EQ.11) GOTO 1 
                                                                        
!  DUMP CENTER YY,MM,DD,HH,MM IS IN THE FIRST EMPTY MESSAGE             
!  --------------------------------------------------------             
!  i.e. the first message containing zero subsets                       
                                                                        
      CALL GETLENS (MBAY, 2, LEN0, LEN1, LEN2, L3, L4, L5) 
      IPT = LEN0 + LEN1 + LEN2 + 5 
      IF (IUPB (MBAY, IPT, 16) .NE.0) GOTO 200 
                                                                        
      IGD = IGETDATE (MBAY, JDATE (1), JDATE (2), JDATE (3), JDATE (4) ) 
      JDATE (5) = IUPBS01 (MBAY, 'MINU') 
                                                                        
!  DUMP CLOCK YY,MM,DD,HH,MM IS IN THE SECOND EMPTY MESSAGE             
!  --------------------------------------------------------             
!  i.e. the second message containing zero subsets                      
                                                                        
      CALL RDMSGW (LUNIT, MBAY, IER) 
      IF (IER.LT.0) GOTO 200 
                                                                        
      CALL GETLENS (MBAY, 2, LEN0, LEN1, LEN2, L3, L4, L5) 
      IPT = LEN0 + LEN1 + LEN2 + 5 
      IF (IUPB (MBAY, IPT, 16) .NE.0) GOTO 200 
                                                                        
      IGD = IGETDATE (MBAY, JDUMP (1), JDUMP (2), JDUMP (3), JDUMP (4) ) 
      JDUMP (5) = IUPBS01 (MBAY, 'MINU') 
                                                                        
      GOTO 100 
                                                                        
  200 IF (IPRT.GE.1.AND. (JDATE (1) .EQ. - 1.OR.JDUMP (1) .EQ. - 1) )   &
      THEN                                                              
         PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      IF (JDATE (1) .EQ. - 1) PRINT  * , 'BUFRLIB: DATEBF - FIRST  EMPTY&
     & ', 'BUFR MESSAGE SECTION 1 DATE COULD NOT BE LOCATED - RETURN ', &
     &'WITH JDATE = 4*-1'                                               
      IF (JDUMP (1) .EQ. - 1) PRINT  * , 'BUFRLIB: DATEBF - SECOND EMPTY&
     & ', 'BUFR MESSAGE SECTION 1 DATE COULD NOT BE LOCATED - RETURN ', &
     &'WITH JDUMP = 4*-1'                                               
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
         PRINT * 
      ENDIF 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 CALL BORT ('BUFRLIB: DUMPBF - INPUT BUFR FILE IS OPEN, IT MUST BE &
     &CLOSED')                                                          
      END SUBROUTINE DUMPBF                         

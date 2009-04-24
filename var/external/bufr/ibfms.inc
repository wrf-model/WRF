      INTEGER FUNCTION IBFMS (R8VAL) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IBFMS                                                  
!   PRGMMR: J. ATOR          ORG: NP12       DATE: 2007-01-19           
!                                                                       
! ABSTRACT: THIS FUNCTION TESTS WHETHER THE INPUT VALUE IS EQUIVALENT   
!   TO THE BUFR ARCHIVE LIBRARY "MISSING" VALUE.  THE COMPARISON IS     
!   MADE USING A FUZZINESS THRESHOLD TO ENSURE THAT REAL NUMBERS ARE    
!   NOT TESTED FOR ACTUAL EQUALITY TO EACH OTHER.  THE USE OF INTEGER   
!   RETURN CODES ALLOWS THIS FUNCTION TO BE CALLED IN A LOGICAL         
!   CONTEXT FROM A CALLING PROGRAM WRITTEN IN C.                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    IBFMS ( R8VAL )                                             
!   INPUT ARGUMENT LIST:                                                
!     R8VAL    - REAL*8: VALUE TO BE TESTED FOR EQUIVALENCE TO          
!                BUFR ARCHIVE LIBRARY "MISSING" VALUE                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IBFMS    - INTEGER: RETURN CODE:                                  
!                0 - R8VAL IS NOT EQUIVALENT TO "MISSING"               
!                1 - R8VAL IS EQUIVALENT TO "MISSING"                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: INVMRG   UFBDMP   UFBRW    UFDUMP       
!                               WRTREE                                  
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      REAL(8) R8VAL 
                                                                        
!       The following parameter defines the fuzziness threshold to use  
!       when testing for BUFR "missing".                                
                                                                        
      REAL(8) BDIFD 
      PARAMETER (BDIFD = 100.) 
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF (ABS (R8VAL - BMISS) .LT.BDIFD) THEN 
      IBFMS = 1 
      ELSE 
      IBFMS = 0 
      ENDIF 
                                                                        
      RETURN 
      END FUNCTION IBFMS                            

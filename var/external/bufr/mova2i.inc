      FUNCTION MOVA2I (A) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MOVA2I                                                 
!   PRGMMR: GILBERT          ORG: NP11       DATE: 2003-11-04           
!                                                                       
! ABSTRACT: THIS FUNCTION COPIES A BIT STRING FROM A CHARACTER*1        
!   VARIABLE TO AN INTEGER VARIABLE.  IT IS INTENDED TO REPLACE THE     
!   FORTRAN INTRINSIC FUNCTION ICHAR, WHICH ONLY SUPPORTS VALUES OF     
!   ICHAR(A) BETWEEN ZERO AND 127, INCLUSIVE ON SOME PLATFORMS (E.G.,   
!   THE NCEP IBM SP).  IF "A" IS GREATER THAN 127 IN THE COLLATING      
!   SEQUENCE, ICHAR(A) DOES NOT RETURN THE EXPECTED BIT VALUE.  THIS    
!   FUNCTION CAN BE USED FOR ALL VALUES OF ICHAR(A) BETWEEN ZERO AND    
!   255.  SINCE THIS FUNCTION IS PRESENT IN THE W3 LIBRARIES (IN C      
!   LANGUAGE), AND SINCE IT IS NO LONGER CALLED BY ANY BUFR ARCHIVE     
!   LIBRARY ROUTINES, IT MAY BE REMOVED FROM THE BUFR ARCHIVE LIBRARY   
!   IN A FUTURE VERSION.  USERS SHOULD MIGRATE TO MOVA2I IN THE W3      
!   LIBRARIES WHICH IS USED IN THE SAME WAY AS MOVA2I IS USED HERE.     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  S. GILBERT -- ORIGINAL AUTHOR                             
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF                    
! 2005-11-29  D. KEYSER  -- MARKED AS OBSOLETE AND ADDED PRINT          
!                           NOTIFICATION                                
!                                                                       
! USAGE:    CALL MOVA2I (A)                                             
!   INPUT ARGUMENT LIST:                                                
!     A        - CHARACTER*1: VARIABLE THAT HOLDS THE BITSTRING TO      
!                EXTRACT                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MOVA2I   - INTEGER: VALUE OF THE BITSTRING IN CHARACTER A         
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Possibly called by application          
!                               programs?                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(1) A 
      CHARACTER(4) CTEMP 
      INTEGER ITEMP, FF 
                                                                        
      EQUIVALENCE (CTEMP, ITEMP) 
                                                                        
      DATA IFIRST / 0 / 
                                                                        
      SAVE IFIRST 
                                                                        
      IF (IFIRST.EQ.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT 101 
  101 FORMAT(' BUFRLIB: MOVA2I - THIS SUBROUTINE IS NOW OBSOLETE; ',    &
     & 'USE IDENTICAL SUBROUTINE MOVA2I IN W3 LIBRARIES INSTEAD')       
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
            PRINT * 
         ENDIF 
         IFIRST = 1 
      ENDIF 
                                                                        
      FF = 255 
      CTEMP = A//A//A//A 
      MOVA2I = IAND (ITEMP, FF) 
      RETURN 
      END FUNCTION MOVA2I                           

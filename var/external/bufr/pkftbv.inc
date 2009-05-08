      REAL(8) FUNCTION PKFTBV (NBITS, IBIT) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    PKFTBV                                                 
!   PRGMMR: JATOR            ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT:  THIS FUNCTION COMPUTES AND RETURNS THE VALUE EQUIVALENT    
!   TO THE SETTING OF BIT# IBIT WITHIN A FLAG TABLE OF NBITS BITS.      
!   IF THE COMPUTATION FAILS FOR ANY REASON, THEN THE VALUE BMISS       
!   (10E10) IS RETURNED.  NOTE THAT THIS SUBROUTINE IS THE LOGICAL      
!   INVERSE OF BUFRLIB SUBROUTINE UPFTBV.                               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ORIGINAL VERSION                            
!                                                                       
! USAGE:    PKFTBV (NBITS,IBIT)                                         
!   INPUT ARGUMENT LIST:                                                
!     NBITS    - INTEGER: NUMBER OF BITS IN FLAG TABLE                  
!     IBIT     - INTEGER: NUMBER OF BIT TO BE SET WITHIN FLAG TABLE     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     PKFTBV   - REAL*8: VALUE EQUIVALENT TO THE SETTING OF BIT# IBIT   
!                WITHIN A FLAG TABLE OF NBITS BITS.                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
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
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
      IF ( (NBITS.LE.0) .OR. (IBIT.LE.0) .OR. (IBIT.GT.NBITS) ) THEN 
         PKFTBV = BMISS 
      ELSE 
         PKFTBV = (2.) ** (NBITS - IBIT) 
      ENDIF 
                                                                        
      RETURN 
      END FUNCTION PKFTBV                           

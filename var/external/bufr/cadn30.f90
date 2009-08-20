      SUBROUTINE CADN30 (IDN, ADN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CADN30                                                 
!   PRGMMR: ATOR             ORG: NP12       DATE: 2004-08-18           
!                                                                       
! ABSTRACT:  GIVEN THE BIT-WISE REPRESENTATION OF THE FXY VALUE         
!   FOR A DESCRIPTOR, THIS ROUTINE CALLS FUNCTION ADN30 AND STORES      
!   ITS RETURN VALUE (I.E. THE ASCII-EQUIVALENT FXY VALUE) AS THE       
!   ROUTINE OUTPUT VALUE.  THIS MECHANISM (I.E. A FORTRAN SUBROUTINE    
!   WRAPPER RETURNING ADN AS A CALL PARAMETER, RATHER THAN DIRECTLY     
!   CALLING THE FORTRAN FUNCTION ADN30 FROM WITHIN A C ROUTINE)         
!   ALLOWS SAFE AND PORTABLE (ALBEIT INDIRECT) ACCESS TO THE ADN30      
!   FUNCTION LOGIC FROM WITHIN A C ROUTINE SUCH AS RESTD.               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2004-08-18  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CADN30( IDN, ADN )                                          
!   INPUT ARGUMENT LIST:                                                
!     IDN      - INTEGER: BIT-WISE REPRESENTATION OF FXY VALUE          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     ADN      - CHARACTER*(*): ASCII-CHARACTER FORM OF IDN             
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ADN30                                   
!    THIS ROUTINE IS CALLED BY: RESTD                                   
!                               Normally not called by application      
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) ADN 
      CHARACTER(6) ADN30 
                                                                        
      ADN = ADN30 (IDN, 6) 
                                                                        
      RETURN 
      END SUBROUTINE CADN30                         

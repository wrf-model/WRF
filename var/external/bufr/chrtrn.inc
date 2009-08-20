      SUBROUTINE CHRTRN (STR, CHR, N) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CHRTRN                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A SPECIFIED NUMBER OF CHARACTERS     
!   FROM A CHARACTER ARRAY INTO A CHARACTER STRING.                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION                               
!                                                                       
! USAGE:    CALL CHRTRN (STR, CHR, N)                                   
!   INPUT ARGUMENT LIST:                                                
!     CHR      - CHARACTER*1: N-WORD CHARACTER ARRAY                    
!     N        - INTEGER: NUMBER OF CHARACTERS TO COPY                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): CHARACTER STRING                        
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: RDBFDX                                  
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(1) CHR (N) 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
      DO I = 1, N 
      STR (I:I) = CHR (I) 
      ENDDO 
      RETURN 
      END SUBROUTINE CHRTRN                         

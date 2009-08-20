      SUBROUTINE CHRTRNA (STR, CHR, N) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CHRTRNA                                                
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE COPIES A SPECIFIED NUMBER OF CHARACTERS     
!   FROM A CHARACTER ARRAY INTO A CHARACTER STRING.  THE DIFFERENCE     
!   BETWEEN THIS SUBROUTINE AND BUFR ARCHIVE LIBRARY SUBROUTINE CHRTRN  
!   IS THAT, IN THIS SUBROUTINE, THE INPUT CHARACTER ARRAY IS ASSUMED   
!   TO BE IN ASCII; THUS, FOR CASES WHERE THE NATIVE MACHINE IS EBCDIC, 
!   AN ASCII TO EBCDIC TRANSLATION IS DONE ON THE FINAL STRING BEFORE   
!   IT IS OUTPUT.                                                       
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION                               
!                                                                       
! USAGE:    CALL CHRTRNA (STR, CHR, N)                                  
!   INPUT ARGUMENT LIST:                                                
!     CHR      - CHARACTER*1: N-WORD CHARACTER ARRAY IN ASCII           
!     N        - INTEGER: NUMBER OF CHARACTERS TO COPY                  
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): CHARACTER STRING IN ASCII OR EBCDIC,    
!                DEPENDING ON NATIVE MACHINE                            
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IPKM     IUPM                           
!    THIS ROUTINE IS CALLED BY: ICHKSTR  RDBFDX                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / CHARAC / IASCII, IATOE (0:255), IETOA (0:255) 
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(1) CHR (N) 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!     Loop on N characters of CHR                                       
                                                                        
      DO I = 1, N 
      STR (I:I) = CHR (I) 
                                                                        
!     If this is an EBCDIC machine, then translate the character        
!     from ASCII -> EBCDIC.                                             
                                                                        
      IF (IASCII.EQ.0) CALL IPKM (STR (I:I), 1, IATOE (IUPM (STR (I:I), &
      8) ) )                                                            
      ENDDO 
      RETURN 
      END SUBROUTINE CHRTRNA                        

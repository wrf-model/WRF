      FUNCTION IGETFXY (STR, CFXY) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IGETFXY                                                
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS FUNCTION LOOKS FOR AND RETURNS A VALID FXY NUMBER     
!   FROM WITHIN THE GIVEN INPUT STRING.  THE FXY NUMBER MAY BE IN       
!   FORMAT OF EITHER FXXYYY OR F-XX-YYY WITHIN THE INPUT STRING, BUT    
!   IT IS ALWAYS RETURNED IN FORMAT FXXYYY UPON OUTPUT.                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    IGETFXY ( STR, CFXY )                                       
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): INPUT STRING                            
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     CFXY     - CHARACTER*6: FXY NUMBER IN FORMAT FXXYYY               
!     IGETFXY  - INTEGER: RETURN CODE:                                  
!                       0 = normal return                               
!                      -1 = could not find a valid FXY number in STR    
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        JSTCHR   NUMBCK                         
!    THIS ROUTINE IS CALLED BY: GETNTBE  SNTBDE                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(6) CFXY 
                                                                        
      PARAMETER (LSTR2 = 120) 
      CHARACTER * (LSTR2) STR2 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IGETFXY = - 1 
                                                                        
      LSTR = LEN (STR) 
      IF (LSTR.LT.6) RETURN 
                                                                        
!       Left-justify a copy of the input string.                        
                                                                        
      IF (LSTR.GT.LSTR2) THEN 
         STR2 (1:LSTR2) = STR (1:LSTR2) 
      ELSE 
         STR2 = STR 
      ENDIF 
      CALL JSTCHR (STR2, IRET) 
      IF (IRET.NE.0) RETURN 
                                                                        
!       Look for an FXY number.                                         
                                                                        
      IF (INDEX (STR2, '-') .NE.0) THEN 
!           Format of field is F-XX-YYY.                                
         CFXY (1:1) = STR2 (1:1) 
         CFXY (2:3) = STR2 (3:4) 
         CFXY (4:6) = STR2 (6:8) 
      ELSE 
!           Format of field is FXXYYY.                                  
         CFXY = STR2 (1:6) 
      ENDIF 
                                                                        
!       Check that the FXY number is valid.                             
                                                                        
      IF (NUMBCK (CFXY) .EQ.0) IGETFXY = 0 
                                                                        
      RETURN 
      END FUNCTION IGETFXY                          

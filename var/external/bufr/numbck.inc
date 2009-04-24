      FUNCTION NUMBCK (NUMB) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NUMBCK                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION CHECKS THE INPUT CHARACTER STRING TO DETERMINE
!   WHETHER IT CONTAINS A VALID FXY (DESCRIPTOR) VALUE.                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- SPLIT NON-ZERO RETURN INTO -1 FOR INVALID   
!                           CHARACTER IN POSITION 1, -2 FOR INVALID     
!                           CHARACTERS IN POSITIONS 2 THROUGH 6, -3 FOR 
!                           INVALID CHARACTERS IN POSITIONS 2 AND 3 DUE 
!                           TO BEING OUT OF RANGE, AND -4 FOR INVALID   
!                           CHARACTERS IN POSITIONS 4 THROUGH 6 DUE TO  
!                           BEING OUT OF RANGE (RETURN ONLY -1 BEFORE   
!                           FOR ALL PROBLEMATIC CASES); UNIFIED/        
!                           PORTABLE FOR WRF; ADDED HISTORY             
!                           DOCUMENTATION                               
! 2007-01-19  J. ATOR    -- CLEANED UP AND SIMPLIFIED LOGIC             
!                                                                       
! USAGE:   NUMBCK (NUMB)                                                
!   INPUT ARGUMENT LIST:                                                
!     NUMB     - CHARACTER*6: FXY VALUE TO BE CHECKED                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NUMBCK   - INTEGER: INDICATOR AS TO WHETHER NUMB IS VALID:        
!                       0 = YES                                         
!                      -1 = NO - first character ("F" value) is not '0',
!                           '1', '2' OR '3'                             
!                      -2 = NO - remaining characters (2-6) ("X" and "Y"
!                           values) are not all numeric                 
!                      -3 = NO - characters 2-3 ("X" value) are not     
!                           between '00' and '63'                       
!                      -4 = NO - characters 4-6 ("Y" value) are not     
!                           between '000' and '255'                     
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        DIGIT                                   
!    THIS ROUTINE IS CALLED BY: IGETFXY  RDUSDX                         
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(6) NUMB 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FIRST CHARACTER OF NUMB                                    
!  ---------------------------------                                    
                                                                        
      IF (LLT (NUMB (1:1) , '0') .OR.LGT (NUMB (1:1) , '3') ) THEN 
      NUMBCK = - 1 
      RETURN 
      ENDIF 
                                                                        
!  CHECK FOR A VALID DESCRIPTOR                                         
!  ----------------------------                                         
                                                                        
      IF (DIGIT (NUMB (2:6) ) ) THEN 
      READ (NUMB, '(1X,I2,I3)') IX, IY 
      ELSE 
      NUMBCK = - 2 
      RETURN 
      ENDIF 
                                                                        
      IF (IX.LT.0.OR.IX.GT.63) THEN 
      NUMBCK = - 3 
      RETURN 
      ELSEIF (IY.LT.0.OR.IY.GT.255) THEN 
      NUMBCK = - 4 
      RETURN 
      ENDIF 
                                                                        
      NUMBCK = 0 
                                                                        
      RETURN 
      END FUNCTION NUMBCK                           

      SUBROUTINE CAPIT (STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CAPIT                                                  
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT: THIS SUBROUTINE CAPITALIZES A STRING OF CHARACTERS. THIS    
!   ENABLES THE USE OF MIXED CASE IN THE UNIT SECTION OF THE ASCII      
!   BUFR TABLES.  AN EXAMPLE: A PROGRAM WHICH GENERATES AN ASCII BUFR   
!   TABLE FROM THE "MASTER TABLE B" MIGHT END UP COPYING SOME UNITS     
!   FIELDS IN MIXED OR LOWER CASE.  IF THE UNITS ARE 'CODE TABLE' OR    
!   'FLAG TABLE' OR CERTAIN OTHER UNIT DESIGNATIONS, THE TABLE WILL BE  
!   PARSED INCORRECTLY, AND THE DATA READ OR INCORRECTLY AS A RESULT.   
!   THIS MAKES SURE ALL UNIT DESIGNATIONS ARE SEEN BY THE PARSER IN     
!   UPPER CASE TO AVOID THESE TYPES OF PROBLEMS.                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY)           
!                                                                       
! USAGE:    CALL CAPIT (STR)                                            
!   INPUT ARGUMENT LIST:                                                
!     STR      - CHARACTER*(*): STRING POSSIBLY CONTAINING MIXED UPPER- 
!                AND LOWER-CASE CHARACTERS                              
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     STR      - CHARACTER*(*): SAME STRING AS INPUT BUT NOW CONTAINING 
!                ALL UPPER-CASE CHARACTERS                              
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: CMPMSG   ELEMDX   RDBFDX   STDMSG       
!                               Normally not called by any application  
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(26) UPS, LOS 
      DATA UPS / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' / 
      DATA LOS / 'abcdefghijklmnopqrstuvwxyz' / 
                                                                        
      DO 20 I = 1, LEN (STR) 
      DO 10 J = 1, 26 
      IF (STR (I:I) .EQ.LOS (J:J) ) THEN 
      STR (I:I) = UPS (J:J) 
      GOTO 20 
      ENDIF 
   10 END DO 
   20 END DO 
                                                                        
      RETURN 
      END SUBROUTINE CAPIT                          

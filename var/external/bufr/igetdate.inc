      FUNCTION IGETDATE (MBAY, IYR, IMO, IDY, IHR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    IGETDATE                                               
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29           
!                                                                       
! ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS THE SECTION 1 DATE-TIME   
!   FROM THE BUFR MESSAGE STORED IN ARRAY MBAY.  IT WILL WORK ON ANY    
!   MESSAGE ENCODED USING BUFR EDITION 2, 3 OR 4.  THE START OF THE     
!   BUFR MESSAGE, (I.E. THE STRING "BUFR") MUST BE ALIGNED ON THE FIRST 
!   FOUR BYTES OF MBAY.                                                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    IGETDATE (MBAY, IYR, IMO, IDY, IHR)                         
!   INPUT ARGUMENT LIST:                                                
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING         
!                BUFR MESSAGE                                           
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IYR      - INTEGER: SECTION 1 YEAR (YYYY OR YY, DEPENDING ON      
!                DATELEN() VALUE)                                       
!     IMO      - INTEGER: SECTION 1 MONTH (MM)                          
!     IDY      - INTEGER: SECTION 1 DAY (DD)                            
!     IHR      - INTEGER: SECTION 1 HOUR (HH)                           
!     IGETDATE - INTEGER: SECTION 1 DATE-TIME (YYYYMMDDHH OR YYMMDDHH,  
!                DEPENDING ON DATELEN() VALUE)                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        IUPBS01                                 
!    THIS ROUTINE IS CALLED BY: CKTABA   DATEBF   DUMPBF                
!                               Normally not called by application      
!                               programs but it could be.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / DATELN / LENDAT 
                                                                        
      DIMENSION MBAY ( * ) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IYR = IUPBS01 (MBAY, 'YEAR') 
      IMO = IUPBS01 (MBAY, 'MNTH') 
      IDY = IUPBS01 (MBAY, 'DAYS') 
      IHR = IUPBS01 (MBAY, 'HOUR') 
      IF (LENDAT.NE.10) THEN 
      IYR = MOD (IYR, 100) 
      ENDIF 
      IGETDATE = (IYR * 1000000) + (IMO * 10000) + (IDY * 100) + IHR 
                                                                        
      RETURN 
      END FUNCTION IGETDATE                         

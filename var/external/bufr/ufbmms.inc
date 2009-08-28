      SUBROUTINE UFBMMS (IMSG, ISUB, SUBSET, JDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBMMS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS A PARTICULAR SUBSET INTO INTERNAL     
!   SUBSET ARRAYS FROM A PARTICULAR BUFR MESSAGE IN INTERNAL MEMORY     
!   BASED ON THE SUBSET NUMBER IN THE MESSAGE AND THE MESSAGE NUMBER IN 
!   INTERNAL MEMORY.  THIS SUBROUTINE IS ACTUALLY A COMBINATION OF      
!   BUFR ARCHIVE LIBRARY SUBROUTINES RDMEMM AND RDMEMS.                 
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE MAXIMUM NUMBER OF BYTES REQUIRED TO     
!                           STORE ALL MESSAGES INTERNALLY WAS INCREASED 
!                           FROM 4 MBYTES TO 8 MBYTES                   
! 2001-08-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 8 MBYTES TO  
!                           16 MBYTES                                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- PARAMETER MAXMSG (THE MAXIMUM NUMBER OF     
!                           BUFR MESSAGES WHICH CAN BE STORED           
!                           INTERNALLY) INCREASED FROM 50000 TO 200000; 
!                           UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE  
!                           TERMINATES ABNORMALLY                       
! 2004-11-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 16 MBYTES TO 
!                           50 MBYTES                                   
!                                                                       
! USAGE:    CALL UFBMMS (IMSG, ISUB, SUBSET, JDATE)                     
!   INPUT ARGUMENT LIST:                                                
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER (RECORD) IN    
!                STORAGE                                                
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR BUFR MESSAGE         
!                CONTAINING SUBSET                                      
!     JDATE    - INTEGER: DATE-TIME FROM SECTION 1 OF BUFR MESSAGE      
!                CONTAINING SUBSET, IN FORMAT OF EITHER YYMMDDHH OR     
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM IS CALLED PRIOR TO THIS TO STORE THE BUFR         
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     RDMEMM   RDMEMS   STATUS       
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
                                                                        
!     COMMON / MSGMEM / MUNIT, MLAST, MSGP (0:MAXMSG), MSGS (MAXMEM) 
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  READ SUBSET #ISUB FROM MEMORY MESSAGE #IMSG                          
!  -------------------------------------------                          
                                                                        
      CALL RDMEMM (IMSG, SUBSET, JDATE, IRET) 
      IF (IRET.NE.0) GOTO 900 
      CALL RDMEMS (ISUB, IRET) 
      IF (IRET.NE.0) GOTO 901 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 IF (IMSG.GT.0) THEN 
      WRITE (BORT_STR, '("BUFRLIB: UFBMMS - REQUESTED MEMORY MESSAGE '//&
     &'NUMBER TO READ IN (",I5,") EXCEEDS THE NUMBER OF MESSAGES IN '//'&
     &MEMORY (",I5,")")') IMSG, MSGP (0)                                
      ELSE 
      WRITE (BORT_STR, '("BUFRLIB: UFBMMS - REQUESTED MEMORY MESSAGE '//&
     &'NUMBER TO READ IN IS ZERO - THIS IS NOT VALID")')                
      ENDIF 
      CALL BORT (BORT_STR) 
  901 CALL STATUS (MUNIT, LUN, IL, IM) 
      WRITE (BORT_STR, '("BUFRLIB: UFBMMS - REQ. SUBSET NUMBER TO READ '&
     &//'IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") IN THE '//'R&
     &EG. MEMORY MESSAGE (",I5,")")') ISUB, MSUB (LUN) , IMSG           
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBMMS                         

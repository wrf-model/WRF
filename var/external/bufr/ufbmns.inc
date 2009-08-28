      SUBROUTINE UFBMNS (IREP, SUBSET, IDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBMNS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS A PARTICULAR SUBSET INTO INTERNAL     
!   SUBSET ARRAYS FROM A COLLECTION OF BUFR MESSAGES IN INTERNAL MEMORY 
!   BASED ON THE SUBSET NUMBER RELATIVE TO THE TOTAL NUMBER OF SUBSETS  
!   IN THE COLLECTION.  THE SUBROUTINE DOES NOT RETURN ANY INFORMATION  
!   ABOUT WHICH MESSAGE NUMBER CONTAINED THE DESIRED SUBSET.  IF THE    
!   REQUESTED SUBSET IS LARGER THAN THE TOTAL NUMBER OF SUBSETS IN      
!   MEMORY, THEN AN APPROPRIATE CALL IS MADE TO BUFR ARCHIVE LIBRARY    
!   SUBROUTINE BORT.                                                    
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
! USAGE:    CALL UFBMNS (IREP, SUBSET, IDATE)                           
!   INPUT ARGUMENT LIST:                                                
!     IREP     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN           
!                COLLECTION OF MESSAGES                                 
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     SUBSET   - CHARACTER*8: TABLE A MNEMONIC FOR BUFR MESSAGE         
!                CONTAINING SUBSET                                      
!     IDATE    - INTEGER: DATE-TIME FROM SECTION 1 OF BUFR MESSAGE      
!                CONTAINING SUBSET, IN FORMAT OF EITHER YYMMDDHH OR     
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM IS CALLED PRIOR TO THIS TO STORE THE BUFR         
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     NMSUB    RDMEMM   RDMEMS       
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
                                                                        
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
      JREP = 0 
      IMSG = 1 
                                                                        
!  READ SUBSET #ISUB FROM MEMORY MESSAGE #IMSG                          
!  -------------------------------------------                          
                                                                        
      DO WHILE (IRET.EQ.0) 
      CALL RDMEMM (IMSG, SUBSET, IDATE, IRET) 
      IF (IRET.NE.0) GOTO 900 
      IF (JREP + NMSUB (MUNIT) .GE.IREP) THEN 
         CALL RDMEMS (IREP - JREP, IRET) 
!  .... DK: I don't think the below error could ever happen(??)         
         IF (IRET.NE.0) GOTO 900 
         GOTO 100 
      ELSE 
         JREP = JREP + NMSUB (MUNIT) 
         IMSG = IMSG + 1 
      ENDIF 
      ENDDO 
!  .... DK: I don't think the below error could ever happen(??)         
      GOTO 900 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: UFBMNS - REQ. SUBSET NO. TO READ IN '&
     &//'(",I5,") EXCEEDS TOTAL NO. OF SUBSETS IN THE COLLECTION OF '//'&
     &MEMORY MESSAGES (",I5,")")') IREP, JREP                           
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBMNS                         

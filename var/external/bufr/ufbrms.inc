      SUBROUTINE UFBRMS (IMSG, ISUB, USR, I1, I2, IRET, STR) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    UFBRMS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE READS SPECIFIED VALUES OUT OF A PARTICULAR  
!   SUBSET WHICH HAS BEEN READ INTO INTERNAL SUBSET ARRAYS FROM A       
!   PARTICULAR BUFR MESSAGE IN INTERNAL MEMORY.  THE DATA VALUES        
!   CORRESPOND TO MNEMONICS WHICH ARE PART OF A DELAYED-REPLICATION     
!   SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION AT ALL. THE SUBSET   
!   READ IN IS BASED ON THE SUBSET NUMBER IN THE MESSAGE AND THE        
!   MESSAGE READ IN IS BASED ON THE MESSAGE NUMBER IN INTERNAL MEMORY.  
!   THIS SUBROUTINE IS ACTUALLY A COMBINATION OF BUFR ARCHIVE LIBRARY   
!   SUBROUTINES RDMEMM, RDMEMS AND UFBINT.                              
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
!                           TERMINATES ABNORMALLY OR UNUSUAL THINGS     
!                           HAPPEN                                      
! 2004-11-15  D. KEYSER  -- PARAMETER MAXMEM (THE MAXIMUM NUMBER OF     
!                           BYTES REQUIRED TO STORE ALL MESSAGES        
!                           INTERNALLY) WAS INCREASED FROM 16 MBYTES TO 
!                           50 MBYTES                                   
!                                                                       
! USAGE:    CALL UFBRMS (IMSG, ISUB, USR, I1, I2, IRET, STR)            
!   INPUT ARGUMENT LIST:                                                
!     IMSG     - INTEGER: POINTER TO BUFR MESSAGE NUMBER (RECORD) IN    
!                STORAGE                                                
!     ISUB     - INTEGER: POINTER TO SUBSET NUMBER TO READ IN BUFR      
!                MESSAGE                                                
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE       
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER     
!                MUST BE AT LEAST AS LARGE AS LATTER)                   
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR             
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B       
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST      
!                DIMENSION OF USR {THIS CAN ALSO BE A SINGLE TABLE D    
!                (SEQUENCE) MNEMONIC WITH EITHER 8- OR 16-BIT DELAYED   
!                REPLICATION (SEE REMARKS 1 IN UFBINT DOCBLOCK)}        
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     USR      - REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES READ   
!                FROM DATA SUBSET                                       
!     IRET     - INTEGER: NUMBER OF "LEVELS" OF DATA VALUES READ FROM   
!                DATA SUBSET (MUST BE NO LARGER THAN I2)                
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    NOTE THAT UFBMEM IS CALLED PRIOR TO THIS TO STORE THE BUFR         
!    MESSAGES INTO INTERNAL MEMORY.                                     
!                                                                       
!    THIS ROUTINE CALLS:        BORT     RDMEMM   RDMEMS   STATUS       
!                               UFBINT                                  
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
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER ( * ) STR 
      CHARACTER(128) BORT_STR 
      CHARACTER(8) SUBSET 
      REAL(8) USR (I1, I2) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IRET = 0 
      IF (I1.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBRMS - FOURTH ARGUMENT (INPUT) IS .LE. 0', '&
     & -  RETURN WITH SIXTH ARGUMENT (IRET) = 0'                        
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ELSEIF (I2.LE.0) THEN 
         IF (IPRT.GE.0) THEN 
            PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: UFBRMS - FIFTH ARGUMENT (INPUT) IS .LE. 0', ' &
     &-  RETURN WITH SIXTH ARGUMENT (IRET) = 0'                         
            PRINT * , 'STR = ', STR 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
            PRINT * 
         ENDIF 
         GOTO 100 
      ENDIF 
                                                                        
!  UFBINT SUBSET #ISUB FROM MEMORY MESSAGE #IMSG                        
!  ---------------------------------------------                        
                                                                        
      CALL RDMEMM (IMSG, SUBSET, JDATE, IRET) 
      IF (IRET.NE.0) GOTO 900 
      CALL RDMEMS (ISUB, IRET) 
      IF (IRET.NE.0) GOTO 901 
                                                                        
      CALL UFBINT (MUNIT, USR, I1, I2, IRET, STR) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
  100 RETURN 
  900 IF (IMSG.GT.0) THEN 
      WRITE (BORT_STR, '("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE '//&
     &'NUMBER TO READ IN (",I5,") EXCEEDS THE NUMBER OF MESSAGES IN '//'&
     &MEMORY (",I5,")")') IMSG, MSGP (0)                                
      ELSE 
      WRITE (BORT_STR, '("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE '//&
     &'NUMBER TO READ IN IS ZERO - THIS IS NOT VALID")')                
      ENDIF 
      CALL BORT (BORT_STR) 
  901 CALL STATUS (MUNIT, LUN, IL, IM) 
      WRITE (BORT_STR, '("BUFRLIB: UFBRMS - REQ. SUBSET NUMBER TO READ '&
     &//'IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") IN THE '//'R&
     &EQ. MEMORY MESSAGE (",I5,")")') ISUB, MSUB (LUN) , IMSG           
      CALL BORT (BORT_STR) 
      END SUBROUTINE UFBRMS                         

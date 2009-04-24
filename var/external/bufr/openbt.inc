      SUBROUTINE OPENBT (LUNDX, MTYP) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    OPENBT                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1998-07-08           
!                                                                       
! ABSTRACT: THIS IS A DUMMY SUBROUTINE WHICH ALWAYS RETURNS LUNDX = 0.  
!   OPENBT MUST BE PRESENT BECAUSE IT IS CALLED BY BUFR ARCHIVE LIBRARY 
!   SUBROUTINE CKTABA AS A LAST RESORT TO TRY AND FIND AN EXTERNAL      
!   USER-SUPPLIED BUFR DICTIONARY TABLE FILE IN CHARACTER FORMAT FROM   
!   WHICH A TABLE A MNEMONIC CAN BE LOCATED.  IF THE APPLICATION        
!   PROGRAM DOES NOT HAVE AN IN-LINE VERSION OF OPENBT (OVERRIDING THIS 
!   ONE), THEN THE RETURNED LUNDX = 0 WILL RESULT IN CKTABA RETURNING   
!   WITHOUT FINDING A TABLE A MNEMONIC BECAUSE THERE IS NO LINK TO ANY  
!   EXTERNAL BUFR TABLES.  NORMALLY, IT IS EXPECTED THAT AN IN-LINE     
!   VERSION OF THIS SUBROUTINE WILL ACTUALLY FIND THE APPROPRIATE       
!   EXTERNAL BUFR TABLE.                                                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1998-07-08  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); ADDED    
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN UNUSUAL  
!                           THINGS HAPPEN                               
!                                                                       
! USAGE:    CALL OPENBT (LUNDX, MTYP)                                   
!   INPUT ARGUMENT LIST:                                                
!     MTYP     - INTEGER: DUMMY {IN AN APPLICATION PROGRAM (IN-LINE)    
!                THIS WOULD BE THE BUFR MESSAGE TYPE}                   
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     LUNDX    - INTEGER: DUMMY, ALWAYS RETURNED AS ZERO {IN AN         
!                APPLICATION PROGRAM (IN-LINE) THIS WOULD BE THE        
!                FORTRAN LOGICAL UNIT NUMBER CONNECTED TO THE FILE      
!                CONTAINING THE EXTERNAL BUFR TABLE}                    
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE (IN BUFR                                              
!    ARCHIVE LIBRARY):          Called by CKTABA only to allow the      
!                               BUFR ARCHIVE LIBRARY to compile, CKTABA 
!                               and any application programs should     
!                               always call a version of OPENBT in-line 
!                               in the application program.             
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      COMMON / QUIET / IPRT 
                                                                        
      IF (IPRT.GE.0) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * , 'BUFRLIB: OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE ',   &
      'CALLED BY CKTABA OR APPL. PGM; OPENBT SHOULD BE INCL. IN-LINE ', &
      'IN APPL. PGM'                                                    
      PRINT * , '+++++++++++++++++++++++WARNING+++++++++++++++++++++++++&
     &'                                                                 
      PRINT * 
      ENDIF 
      LUNDX = 0 
      RETURN 
      END SUBROUTINE OPENBT                         

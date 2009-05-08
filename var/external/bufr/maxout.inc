      SUBROUTINE MAXOUT (MAXO) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MAXOUT                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14           
!                                                                       
! ABSTRACT: THIS SUBROUTINE ALLOWS AN APPLICATION PROGRAM TO SET THE    
!  RECORD LENGTH OF NEWLY CREATED BUFR MESSAGES, OVERRIDING THE VALUE   
!  SET IN BUFR ARCHIVE LIBRARY SUBROUTINE BFRINI.  THIS MUST BE CALLED  
!  AFTER THE INITIAL CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF     
!  SINCE OPENBF CALLS BFRINI.  THE RECORD LENGTH WILL REMAIN MAX0       
!  UNLESS THIS SUBROUTINE IS CALLED AGAIN WITH A NEW MAX0.              
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED             
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS  
!                           MORE COMPLETE DIAGNOSTIC INFO FOR           
!                           INFORMATIONAL PURPOSES                      
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2006-04-14  J. ATOR    -- ADDED MAX0=0 OPTION AND OVERFLOW CHECK      
!                                                                       
! USAGE:    CALL MAXOUT (MAXO)                                          
!   INPUT ARGUMENT LIST:                                                
!     MAXO     - INTEGER: DESIRED MESSAGE LENGTH (BYTES):               
!                       0 = SET RECORD LENGTH TO THE MAXIMUM ALLOWABLE  
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
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
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
      COMMON / MAXCMP / MAXCMB, MAXROW, MAXCOL, NCMSGS, NCSUBS, NCBYTS 
      COMMON / DXTAB / MAXDX, IDXV, NXSTR (10), LDXA (10), LDXB (10),   &
      LDXD (10)                                                         
      COMMON / DXTAB / LD30 (10), DXSTR (10) 
      COMMON / QUIET / IPRT 
                                                                        
      CHARACTER(56) DXSTR 
                                                                        
      IF ( (MAXO.EQ.0) .OR. (MAXO.GT.MXMSGL) ) THEN 
         NEWSIZ = MXMSGL 
      ELSE 
         NEWSIZ = MAXO 
      ENDIF 
                                                                        
      IF (IPRT.GE.0) THEN 
         PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
         PRINT 101, MAXBYT, NEWSIZ 
  101 FORMAT(' BUFRLIB: MAXOUT - THE RECORD LENGTH OF ALL BUFR ',       &
     & 'MESSAGES CREATED FROM THIS POINT ON IS BEING CHANGED FROM ',I7, &
     & ' TO ',I7)                                                       
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
         PRINT * 
      ENDIF 
                                                                        
      MAXBYT = NEWSIZ 
      MAXCMB = NEWSIZ 
      MAXDX = NEWSIZ 
                                                                        
      RETURN 
      END SUBROUTINE MAXOUT                         

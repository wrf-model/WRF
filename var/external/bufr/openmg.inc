      SUBROUTINE OPENMG (LUNIT, SUBSET, JDATE) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    OPENMG                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE OPENS AND INITIALIZES A NEW BUFR MESSAGE    
!   WITHIN MEMORY.  IT SHOULD ONLY BE CALLED WHEN LOGICAL UNIT LUNIT    
!   HAS BEEN OPENED FOR OUTPUT OPERATIONS.  IT IS SIMILAR TO BUFR       
!   ARCHIVE LIBRARY SUBROUTINE OPENMB, HOWEVER UNLIKE OPENMB, IT WILL   
!   ALWAYS OPEN A NEW MESSAGE REGARDLESS OF THE VALUES OF SUBSET AND    
!   JDATE.  IF THERE IS ALREADY A BUFR MESSAGE OPEN WITHIN MEMORY FOR   
!   THIS LUNIT, THEN THAT MESSAGE WILL BE CLOSED AND FLUSHED TO LUNIT   
!   BEFORE OPENING THE NEW ONE.                                         
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"; MODIFIED TO MAKE Y2K        
!                           COMPLIANT                                   
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY     
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE        
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES     
!                           ABNORMALLY                                  
!                                                                       
! USAGE:    CALL OPENMG (LUNIT, SUBSET, JDATE)                          
!   INPUT ARGUMENT LIST:                                                
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE     
!     SUBSET   - CHARACTER*(*): TABLE A MNEMONIC FOR TYPE OF BUFR MESSAG
!                BEING OPENED                                           
!     JDATE    - INTEGER: DATE-TIME STORED WITHIN SECTION 1 OF BUFR     
!                MESSAGE BEING OPENED, IN FORMAT OF EITHER YYMMDDHH OR  
!                YYYYMMDDHH, DEPENDING ON DATELEN() VALUE               
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     CLOSMG   I4DY     MSGINI       
!                               NEMTBA   STATUS   USRTPL   WTSTAT       
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
                                                                        
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
                                                                        
      CHARACTER ( * ) SUBSET 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!  CHECK THE FILE STATUS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUNIT, LUN, IL, IM) 
      IF (IL.EQ.0) GOTO 900 
      IF (IL.LT.0) GOTO 901 
      IF (IM.NE.0) CALL CLOSMG (LUNIT) 
      CALL WTSTAT (LUNIT, LUN, IL, 1) 
                                                                        
!  GET SOME SUBSET PARTICULARS                                          
!  ---------------------------                                          
                                                                        
!  .... Given SUBSET, returns MTYP,MSTB,INOD                            
      CALL NEMTBA (LUN, SUBSET, MTYP, MSTB, INOD) 
!  .... Set pos. index for new Tbl A mnem.                              
      INODE (LUN) = INOD 
!  .... Set date for new message                                        
      IDATE (LUN) = I4DY (JDATE) 
                                                                        
!  INITIALIZE THE OPEN MESSAGE                                          
!  ---------------------------                                          
                                                                        
      CALL MSGINI (LUN) 
      CALL USRTPL (LUN, 1, 1) 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS CLOSED, IT '//  &
      'MUST BE OPEN FOR OUTPUT')                                        
  901 CALL BORT ('BUFRLIB: OPENMG - OUTPUT BUFR FILE IS OPEN FOR '//    &
      'INPUT, IT MUST BE OPEN FOR OUTPUT')                              
      END SUBROUTINE OPENMG                         

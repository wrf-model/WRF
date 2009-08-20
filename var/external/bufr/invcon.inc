      FUNCTION INVCON (NC, LUN, INV1, INV2) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    INVCON (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS FUNCTION ....                                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY                
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY) (INCOMPLETE);  OUTPUTS MORE        
!                           COMPLETE DIAGNOSTIC INFO WHEN UNUSUAL       
!                           THINGS HAPPEN                               
!                                                                       
! USAGE:    INVCON (NC, LUN, INV1, INV2)                                
!   INPUT ARGUMENT LIST:                                                
!     NC       - INTEGER: ....                                          
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INV1     - INTEGER: ....                                          
!     INV2     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     INVCON   - INTEGER: ....                                          
!                                                                       
!   OUTPUT FILES:                                                       
!     UNIT 06  - STANDARD OUTPUT PRINT                                  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        None                                    
!    THIS ROUTINE IS CALLED BY: CONWIN                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
      NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
      COMMON / QUIET / IPRT 
                                                                        
      REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!  CHECK THE INVENTORY INTERVAL                                         
!  ----------------------------                                         
                                                                        
      IF (INV1.LE.0.OR.INV1.GT.NVAL (LUN) ) GOTO 99 
      IF (INV2.LE.0.OR.INV2.GT.NVAL (LUN) ) GOTO 99 
                                                                        
!  FIND AN OCCURANCE OF NODE IN THE WINDOW MEETING THIS CONDITION       
!  --------------------------------------------------------------       
                                                                        
      DO INVCON = INV1, INV2 
      IF (INV (INVCON, LUN) .EQ.NODC (NC) ) THEN 
      IF (KONS (NC) .EQ.1.AND.VAL (INVCON, LUN) .EQ.IVLS (NC) ) GOTO    &
      100                                                               
      IF (KONS (NC) .EQ.2.AND.VAL (INVCON, LUN) .NE.IVLS (NC) ) GOTO    &
      100                                                               
      IF (KONS (NC) .EQ.3.AND.VAL (INVCON, LUN) .LT.IVLS (NC) ) GOTO    &
      100                                                               
      IF (KONS (NC) .EQ.4.AND.VAL (INVCON, LUN) .GT.IVLS (NC) ) GOTO    &
      100                                                               
      ENDIF 
      ENDDO 
                                                                        
   99 INVCON = 0 
      IF (IPRT.GE.2) THEN 
      PRINT * 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * , 'BUFRLIB: INVCON - INVCON RETURNING WITH VALUE OF 0' 
      PRINT * , '+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++&
     &++'                                                               
      PRINT * 
      ENDIF 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END FUNCTION INVCON                           

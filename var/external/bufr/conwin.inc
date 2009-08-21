      SUBROUTINE CONWIN (LUN, INC1, INC2, NBMP) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CONWIN (docblock incomplete)                           
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE ....                                        
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
!                           HISTORY) (INCOMPLETE)                       
!                                                                       
! USAGE:    CALL CONWIN (LUN, INC1, INC2, NBMP)                         
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!     INC1     - INTEGER: ....                                          
!     INC2     - INTEGER: ....                                          
!     NBMP     - INTEGER: ....                                          
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     INC1     - INTEGER: ....                                          
!     INC2     - INTEGER: ....                                          
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        GETWIN   INVCON   INVWIN   NEWWIN       
!                               NXTWIN   USRTPL                         
!    THIS ROUTINE IS CALLED BY: UFBEVN   UFBIN3   UFBRW                 
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRSTR / NNOD, NCON, NODS (20), NODC (10), IVLS (10),    &
      KONS (10)                                                         
                                                                        
!     REAL(8) VAL 
                                                                        
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
                                                                        
!  SPECIAL CASES                                                        
!  -------------                                                        
                                                                        
!  .... There are no condition nodes in string                          
      IF (NCON.EQ.0) THEN 
      INC1 = 1 
      INC2 = NVAL (LUN) 
      GOTO 100 
      ENDIF 
                                                                        
!  .... BUMP node                                                       
      IF (INC1.GT.1.AND.KONS (NCON) .EQ.5) THEN 
      CALL NXTWIN (LUN, INC1, INC2) 
      GOTO 100 
      ENDIF 
                                                                        
!  EVALUATE CONDITIONS TO SEE IF ANY MORE CASES                         
!  --------------------------------------------                         
                                                                        
   10 DO NC = 1, NCON 
      IF (KONS (NC) .EQ.5) THEN 
      INC1 = INVWIN (NODC (NC), LUN, INC1, NVAL (LUN) ) 
      CALL USRTPL (LUN, INC1 - 1, NBMP) 
      CALL NEWWIN (LUN, INC1, INC2) 
      ELSE 
   15 CALL GETWIN (NODC (NC), LUN, INC1, INC2) 
      IF (INC1.EQ.0.AND.NC.EQ.1) GOTO 100 
      IF (INC1.EQ.0) GOTO 10 
      ICON = INVCON (NC, LUN, INC1, INC2) 
      IF (ICON.EQ.0) GOTO 15 
      ENDIF 
      ENDDO 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE CONWIN                         

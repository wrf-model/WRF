      SUBROUTINE INVMRG (LUBFI, LUBFJ) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    INVMRG                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1996-10-09           
!                                                                       
! ABSTRACT: THIS SUBROUTINE MERGES "PARTS" OF SUBSETS WHICH HAVE        
!   DUPLICATE SPACE AND TIME COORDINATES BUT DIFFERENT OR UNIQUE        
!   OBSERVATIONAL DATA.  IT CANNOT MERGE REPLICATED DATA.               
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1996-10-09  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1996-11-25  J. WOOLLEN -- MODIFIED FOR RADIOSONDE CALL SIGNS          
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE       
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB   
!                           ROUTINE "BORT"                              
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES;       
!                           REMOVED ENTRY POINT MRGINV (IT BECAME A     
!                           SEPARATE ROUTINE IN THE BUFRLIB TO          
!                           INCREASE PORTABILITY TO OTHER PLATFORMS)    
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC  
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY     
! 2007-01-19  J. ATOR    -- USE FUNCTION IBFMS AND SIMPLIFY LOGIC       
!                                                                       
! USAGE:    CALL INVMRG (LUBFI, LUBFJ)                                  
!   INPUT ARGUMENT LIST:                                                
!     LUBFI    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR INPUT BUFR    
!                FILE                                                   
!     LUBFJ    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR OUTPUT BUFR   
!                FILE                                                   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     IBFMS    NWORDS   STATUS       
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
                                                                        
      COMMON / MRGCOM / NRPL, NMRG, NAMB, NTOT 
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
                                                                        
      CHARACTER(128) BORT_STR 
!     CHARACTER(10) TAG 
!     CHARACTER(3) TYP 
      LOGICAL HEREI, HEREJ, MISSI, MISSJ, SAMEI 
!     REAL(8) VAL 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IS = 1 
      JS = 1 
                                                                        
!  GET THE UNIT POINTERS                                                
!  ---------------------                                                
                                                                        
      CALL STATUS (LUBFI, LUNI, IL, IM) 
      CALL STATUS (LUBFJ, LUNJ, JL, JM) 
                                                                        
!  STEP THROUGH THE BUFFERS COMPARING THE INVENTORY AND MERGING DATA    
!  -----------------------------------------------------------------    
                                                                        
      DO WHILE (IS.LE.NVAL (LUNI) ) 
                                                                        
!  CHECK TO SEE WE ARE AT THE SAME NODE IN EACH BUFFER                  
!  ---------------------------------------------------                  
                                                                        
      NODE = INV (IS, LUNI) 
      NODJ = INV (JS, LUNJ) 
      IF (NODE.NE.NODJ) GOTO 900 
                                                                        
      ITYP = ITP (NODE) 
                                                                        
!  FOR TYPE 1 NODES DO AN ENTIRE SEQUENCE REPLACEMENT                   
!  --------------------------------------------------                   
                                                                        
      IF (ITYP.EQ.1) THEN 
         IF (TYP (NODE) .EQ.'DRB') IOFF = 0 
         IF (TYP (NODE) .NE.'DRB') IOFF = 1 
         IWRDS = NWORDS (IS, LUNI) + IOFF 
         JWRDS = NWORDS (JS, LUNJ) + IOFF 
         IF (IWRDS.GT.IOFF.AND.JWRDS.EQ.IOFF) THEN 
            DO N = NVAL (LUNJ), JS + 1, - 1 
            INV (N + IWRDS - JWRDS, LUNJ) = INV (N, LUNJ) 
            VAL (N + IWRDS - JWRDS, LUNJ) = VAL (N, LUNJ) 
            ENDDO 
            DO N = 0, IWRDS 
            INV (JS + N, LUNJ) = INV (IS + N, LUNI) 
            VAL (JS + N, LUNJ) = VAL (IS + N, LUNI) 
            ENDDO 
            NVAL (LUNJ) = NVAL (LUNJ) + IWRDS - JWRDS 
            JWRDS = IWRDS 
            NRPL = NRPL + 1 
         ENDIF 
         IS = IS + IWRDS 
         JS = JS + JWRDS 
      ENDIF 
                                                                        
!  FOR TYPES 2 AND 3 FILL MISSINGS                                      
!  -------------------------------                                      
                                                                        
      IF ( (ITYP.EQ.2) .OR. (ITYP.EQ.3) ) THEN 
         HEREI = IBFMS (VAL (IS, LUNI) ) .EQ.0 
         HEREJ = IBFMS (VAL (JS, LUNJ) ) .EQ.0 
         MISSI = .NOT. (HEREI) 
         MISSJ = .NOT. (HEREJ) 
         SAMEI = VAL (IS, LUNI) .EQ.VAL (JS, LUNJ) 
         IF (HEREI.AND.MISSJ) THEN 
            VAL (JS, LUNJ) = VAL (IS, LUNI) 
            NMRG = NMRG + 1 
         ELSEIF (HEREI.AND.HEREJ.AND..NOT.SAMEI) THEN 
            NAMB = NAMB + 1 
         ENDIF 
      ENDIF 
                                                                        
!  BUMP THE COUNTERS AND GO CHECK THE NEXT PAIR                         
!  --------------------------------------------                         
                                                                        
      IS = IS + 1 
      JS = JS + 1 
      ENDDO 
                                                                        
      NTOT = NTOT + 1 
                                                                        
!  EXITS                                                                
!  -----                                                                
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: INVMRG - NODE FROM INPUT BUFR FILE '/&
     &/'(",I7,") DOES NOT EQUAL NODE FROM OUTPUT BUFR FILE (",I7,"), '//&
     &'TABULAR MISMATCH")') NODE, NODJ                                  
      CALL BORT (BORT_STR) 
      END SUBROUTINE INVMRG                         

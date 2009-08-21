      SUBROUTINE RDTREE (LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDTREE                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06           
!                                                                       
! ABSTRACT: THIS SUBROUTINE UNPACKS THE NEXT SUBSET FROM THE INTERNAL   
!   UNCOMPRESSED MESSAGE BUFFER (ARRAY MBAY IN COMMON BLOCK /BITBUF/)   
!   AND STORES THE UNPACKED SUBSET WITHIN THE INTERNAL ARRAY VAL(*,LUN) 
!   IN COMMON BLOCK /USRINT/.                                           
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-  
!                           LINING CODE WITH FPP DIRECTIVES             
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE       
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32  
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE     
!                           BUFR FILES UNDER THE MPI)                   
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           10,000 TO 20,000 BYTES                      
! 2003-11-04  J. WOOLLEN -- FIXED A BUG WHICH COULD ONLY OCCUR WHEN     
!                           THE LAST ELEMENT IN A SUBSET IS A CHARACTER 
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED DOCUMENTATION (INCLUDING         
!                           HISTORY)                                    
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2007-01-19  J. ATOR    -- PREVENT OVERFLOW OF CVAL FOR STRINGS LONGER 
!                           THAN 8 CHARACTERS                           
!                                                                       
! USAGE:    CALL RDTREE (LUN)                                           
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        RCSTPL   UPBB     UPC                   
!    THIS ROUTINE IS CALLED BY: READSB                                  
!                               Normally not called by any application  
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
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
      COMMON / USRBIT / NBIT (MAXJL), MBIT (MAXJL) 
                                                                        
!     CHARACTER(10) TAG 
      CHARACTER(8) CVAL 
!     CHARACTER(3) TYP 
      DIMENSION IVAL (MAXJL) 
      EQUIVALENCE (CVAL, RVAL) 
      REAL(8) RVAL, UPS, TEN !, VAL
                                                                        
      DATA TEN / 10. / 
                                                                        
!-----------------------------------------------------------------------
!     Statement function to compute BUFR "missing value" for field      
!     of length IBT(NODE)) bits (all bits "on"):                        
                                                                        
      MPS (NODE) = 2** (IBT (NODE) ) - 1 
                                                                        
!     Statement function to decode the unpacked BUFR value IVAL accordin
!     to the scale and reference values that are stored within index NOD
!     of the internal arrays ISC(*) and IRF(*):                         
                                                                        
      UPS (NODE) = (IVAL (N) + IRF (NODE) ) * TEN** ( - ISC (NODE) ) 
!-----------------------------------------------------------------------
                                                                        
!  CYCLE THROUGH A SUBSET SETTING UP THE TEMPLATE                       
!  ----------------------------------------------                       
                                                                        
      MBIT (1) = IBIT 
      NBIT (1) = 0 
      CALL RCSTPL (LUN) 
                                                                        
!  UNPACK A SUBSET INTO THE USER ARRAY IVAL                             
!  ----------------------------------------                             
                                                                        
      DO N = 1, NVAL (LUN) 
      CALL UPBB (IVAL (N), NBIT (N), MBIT (N), MBAY (1, LUN) ) 
      ENDDO 
                                                                        
!  LOOP THROUGH EACH ELEMENT OF THE SUBSET, CONVERTING THE UNPACKED     
!  VALUES TO THE PROPER TYPES                                           
!  ----------------------------------------------------------------     
                                                                        
      DO N = 1, NVAL (LUN) 
      NODE = INV (N, LUN) 
      IF (ITP (NODE) .EQ.1) THEN 
                                                                        
!        The unpacked value is a delayed descriptor replication factor. 
                                                                        
      VAL (N, LUN) = IVAL (N) 
      ELSEIF (ITP (NODE) .EQ.2) THEN 
                                                                        
!        The unpacked value is a real.                                  
                                                                        
      IF (IVAL (N) .LT.MPS (NODE) ) VAL (N, LUN) = UPS (NODE) 
      ELSEIF (ITP (NODE) .EQ.3) THEN 
                                                                        
!        The unpacked value is a character string.  If the string is    
!        longer than 8 characters, then only the first 8 will be        
!        unpacked by this routine, and a separate subsequent call to    
!        BUFR archive library subroutine READLC will be required to     
!        unpack the remainder of the string.                            
                                                                        
      CVAL = ' ' 
      KBIT = MBIT (N) 
      NBT = MIN (8, NBIT (N) / 8) 
      CALL UPC (CVAL, NBT, MBAY (1, LUN), KBIT) 
      VAL (N, LUN) = RVAL 
      ENDIF 
      ENDDO 
                                                                        
      IBIT = NBIT (NVAL (LUN) ) + MBIT (NVAL (LUN) ) 
                                                                        
      RETURN 
      END SUBROUTINE RDTREE                         

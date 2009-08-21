      SUBROUTINE RDCMPS (LUN) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDCMPS                                                 
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2000-09-19           
!                                                                       
! ABSTRACT: THIS SUBROUTINE UNCOMPRESSES AND UNPACKS THE NEXT SUBSET    
!   FROM THE INTERNAL COMPRESSED MESSAGE BUFFER (ARRAY MBAY IN COMMON   
!   BLOCK /BITBUF/) AND STORES THE UNPACKED SUBSET WITHIN THE INTERNAL  
!   ARRAY VAL(*,LUN) IN COMMON BLOCK /USRINT/.                          
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR                             
! 2002-05-14  J. WOOLLEN -- IMPROVED GENERALITY, PREVIOUSLY RDCMPS      
!                           WOULD NOT RECOGNIZE COMPRESSED DELAYED      
!                           REPLICATION AS A LEGITIMATE DATA STRUCTURE  
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION                         
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE               
!                           INTERDEPENDENCIES                           
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES) 
!                           INCREASED FROM 15000 TO 16000 (WAS IN       
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR 
!                           WRF; ADDED HISTORY DOCUMENTATION            
! 2004-08-18  J. ATOR    -- INITIALIZE CVAL TO EMPTY BEFORE CALLING UPC;
!                           CORRECT LOGIC FOR WHEN A CHARACTER VALUE IS 
!                           THE SAME FOR ALL SUBSETS IN A MESSAGE;      
!                           MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
!                                                                       
! USAGE:    CALL RDCMPS (LUN)                                           
!   INPUT ARGUMENT LIST:                                                
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS  
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        UPB      UPC      USRTPL                
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
      COMMON / MSGCWD / NMSG (NFILES), NSUB (NFILES), MSUB (NFILES),    &
      INODE (NFILES), IDATE (NFILES)                                    
!     COMMON / TABLES / MAXTAB, NTAB, TAG (MAXJL), TYP (MAXJL), KNT (   &
!     MAXJL), JUMP (MAXJL), LINK (MAXJL), JMPB (MAXJL), IBT (MAXJL),    &
!     IRF (MAXJL), ISC (MAXJL), ITP (MAXJL), VALI (MAXJL), KNTI (MAXJL),&
!     ISEQ (MAXJL, 2), JSEQ (MAXJL)                                     
!     COMMON / USRINT / NVAL (NFILES), INV (MAXJL, NFILES), VAL (MAXJL, &
!     NFILES)                                                           
                                                                        
!     CHARACTER(10) TAG 
      CHARACTER(8) CREF, CVAL 
!     CHARACTER(3) TYP 
      EQUIVALENCE (CVAL, RVAL) 
      REAL(8) RVAL, UPS, TEN !, VAL
                                                                        
      DATA TEN / 10 / 
                                                                        
!-----------------------------------------------------------------------
!     Statement function to compute BUFR "missing value" for field      
!     of length LBIT bits (all bits "on"):                              
                                                                        
      LPS (LBIT) = MAX (2** (LBIT) - 1, 1) 
                                                                        
!     Statement function to decode the encoded BUFR value IVAL according
!     to the scale and reference values that are stored within index NOD
!     of the internal arrays ISC(*) and IRF(*):                         
                                                                        
      UPS (NODE) = (IVAL + IRF (NODE) ) * TEN** ( - ISC (NODE) ) 
!-----------------------------------------------------------------------
                                                                        
!  SETUP THE SUBSET TEMPLATE                                            
!  -------------------------                                            
                                                                        
      CALL USRTPL (LUN, 1, 1) 
                                                                        
!  UNCOMPRESS A SUBSET INTO THE VAL ARRAY ACCORDING TO TABLE B          
!  -----------------------------------------------------------          
                                                                        
      NSBS = NSUB (LUN) 
                                                                        
!     Note that we are going to unpack the (NSBS)th subset from within  
!     the current BUFR message.                                         
                                                                        
      IBIT = MBYT (LUN) 
                                                                        
!     Loop through each element of the subset.                          
                                                                        
      N = 0 
                                                                        
    1 DO N = N + 1, NVAL (LUN) 
      NODE = INV (N, LUN) 
      NBIT = IBT (NODE) 
      ITYP = ITP (NODE) 
                                                                        
!     In each of the following code blocks, the "local reference value" 
!     for the element is determined first, followed by the 6-bit value  
!     which indicates how many bits are used to store the increment     
!     (i.e. offset) from this "local reference value".  Then, we jump   
!     ahead to where this increment is stored for this particular subset
!     unpack it, and add it to the "local reference value" to determine 
!     the final uncompressed value for this element from this subset.   
                                                                        
!     Note that, if an element has the same final uncompressed value    
!     for each subset in the message, then the encoding rules for BUFR  
!     compression dictate that the "local reference value" will be equal
!     to this value, the 6-bit increment length indicator will have     
!     a value of zero, and the actual increments themselves will be     
!     omitted from the message.                                         
                                                                        
      IF (ITYP.EQ.1.OR.ITYP.EQ.2) THEN 
                                                                        
!        This is a numeric element.                                     
                                                                        
      CALL UPB (LREF, NBIT, MBAY (1, LUN), IBIT) 
      CALL UPB (LINC, 6, MBAY (1, LUN), IBIT) 
      JBIT = IBIT + LINC * (NSBS - 1) 
      CALL UPB (NINC, LINC, MBAY (1, LUN), JBIT) 
      IF (NINC.EQ.LPS (LINC) ) NINC = LPS (NBIT) 
      IVAL = LREF + NINC 
      IF (ITYP.EQ.1) THEN 
      CALL USRTPL (LUN, N, IVAL) 
      GOTO 1 
      ENDIF 
      IF (IVAL.LT.LPS (NBIT) ) VAL (N, LUN) = UPS (NODE) 
      IBIT = IBIT + LINC * MSUB (LUN) 
      ELSEIF (ITYP.EQ.3) THEN 
                                                                        
!        This is a character element.                                   
                                                                        
      CREF = ' ' 
      CALL UPC (CREF, NBIT / 8, MBAY (1, LUN), IBIT) 
      CALL UPB (LINC, 6, MBAY (1, LUN), IBIT) 
      IF (LINC.EQ.0) THEN 
      CVAL = CREF 
      ELSE 
      JBIT = IBIT + LINC * (NSBS - 1) * 8 
      CVAL = ' ' 
      CALL UPC (CVAL, LINC, MBAY (1, LUN), JBIT) 
      ENDIF 
      VAL (N, LUN) = RVAL 
      IBIT = IBIT + 8 * LINC * MSUB (LUN) 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
      END SUBROUTINE RDCMPS                         

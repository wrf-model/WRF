      SUBROUTINE SNTBBE (IFXYN, LINE, MXMTBB, NMTBB, IMFXYN, CMSCL,     &
      CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)                      
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SNTBBE                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE PARSES AN ENTRY THAT WAS PREVIOUSLY READ   
!   FROM AN ASCII MASTER TABLE B FILE AND THEN STORES THE OUTPUT INTO   
!   THE MERGED ARRAYS.                                                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL SNTBBE ( IFXYN, LINE, MXMTBB,                          
!                         NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,           
!                         CMUNIT, CMMNEM, CMDSC, CMELEM )               
!   INPUT ARGUMENT LIST:                                                
!     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR     
!                TABLE ENTRY; THIS FXY NUMBER IS THE ELEMENT DESCRIPTOR 
!     LINE     - CHARACTER*(*): TABLE ENTRY                             
!     MXMTBB   - INTEGER: MAXIMUM NUMBER OF ENTRIES TO BE STORED IN     
!                MERGED MASTER TABLE B ARRAYS; THIS SHOULD BE THE SAME  
!                NUMBER AS WAS USED TO DIMENSION THE OUTPUT ARRAYS IN   
!                THE CALLING PROGRAM, AND IT IS USED BY THIS SUBROUTINE 
!                TO ENSURE THAT IT DOESN'T OVERFLOW THESE ARRAYS        
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NMTBB    - INTEGER: NUMBER OF ENTRIES IN MERGED MASTER TABLE B    
!                ARRAYS                                                 
!     IMFXYN(*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE              
!                REPRESENTATIONS OF FXY NUMBERS (I.E. ELEMENT           
!                DESCRIPTORS)                                           
!     CMSCL(*) - CHARACTER*4: MERGED ARRAY CONTAINING SCALE FACTORS     
!     CMSREF(*)- CHARACTER*12: MERGED ARRAY CONTAINING REFERENCE VALUES 
!     CMBW(*)  - CHARACTER*4: MERGED ARRAY CONTAINING BIT WIDTHS        
!     CMUNIT(*)- CHARACTER*14: MERGED ARRAY CONTAINING UNITS            
!     CMMNEM(*)- CHARACTER*8: MERGED ARRAY CONTAINING MNEMONICS         
!     CMDSC(*) - CHARACTER*4: MERGED ARRAY CONTAINING DESCRIPTOR CODES  
!     CMELEM(*)- CHARACTER*120: MERGED ARRAY CONTAINING ELEMENT NAMES   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        BORT     BORT2    JSTCHR   NEMOCK       
!                               PARSTR   RJUST                          
!    THIS ROUTINE IS CALLED BY: RDMTBB                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) LINE 
      CHARACTER(200) TAGS (10), WKTAG 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(120) CMELEM ( * ) 
      CHARACTER(14) CMUNIT ( * ) 
      CHARACTER(12) CMSREF ( * ) 
      CHARACTER(8) CMMNEM ( * ) 
      CHARACTER(4) CMSCL ( * ), CMBW ( * ), CMDSC ( * ) 
                                                                        
      INTEGER IMFXYN ( * ) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (NMTBB.GE.MXMTBB) GOTO 900 
      NMTBB = NMTBB + 1 
                                                                        
!       Store the FXY number.  This is the element descriptor.          
                                                                        
      IMFXYN (NMTBB) = IFXYN 
                                                                        
!       Parse the table entry.                                          
                                                                        
      CALL PARSTR (LINE, TAGS, 10, NTAG, '|', .FALSE.) 
      IF (NTAG.LT.4) THEN 
      BORT_STR2 = '                  HAS TOO FEW FIELDS' 
         GOTO 901 
      ENDIF 
                                                                        
!       Scale factor.                                                   
                                                                        
      CALL JSTCHR (TAGS (2), IRET) 
      IF (IRET.NE.0) THEN 
      BORT_STR2 = '                  HAS MISSING SCALE FACTOR' 
         GOTO 901 
      ENDIF 
      CMSCL (NMTBB) = TAGS (2) (1:4) 
      RJ = RJUST (CMSCL (NMTBB) ) 
                                                                        
!       Reference value.                                                
                                                                        
      CALL JSTCHR (TAGS (3), IRET) 
      IF (IRET.NE.0) THEN 
      BORT_STR2 = '                  HAS MISSING REFERENCE VALUE' 
         GOTO 901 
      ENDIF 
      CMSREF (NMTBB) = TAGS (3) (1:12) 
      RJ = RJUST (CMSREF (NMTBB) ) 
                                                                        
!       Bit width.                                                      
                                                                        
      CALL JSTCHR (TAGS (4), IRET) 
      IF (IRET.NE.0) THEN 
      BORT_STR2 = '                  HAS MISSING BIT WIDTH' 
         GOTO 901 
      ENDIF 
      CMBW (NMTBB) = TAGS (4) (1:4) 
      RJ = RJUST (CMBW (NMTBB) ) 
                                                                        
!       Units.  Note that this field is allowed to be blank.            
                                                                        
      IF (NTAG.GT.4) THEN 
         CALL JSTCHR (TAGS (5), IRET) 
         CMUNIT (NMTBB) = TAGS (5) (1:14) 
      ELSE 
         CMUNIT (NMTBB) = ' ' 
      ENDIF 
                                                                        
!       Comment (additional) fields.  Any of these fields may be blank. 
                                                                        
      CMMNEM (NMTBB) = ' ' 
      CMDSC (NMTBB) = ' ' 
      CMELEM (NMTBB) = ' ' 
      IF (NTAG.GT.5) THEN 
         WKTAG = TAGS (6) 
         CALL PARSTR (WKTAG, TAGS, 10, NTAG, ';', .FALSE.) 
         IF (NTAG.GT.0) THEN 
!               The first additional field contains the mnemonic.       
            CALL JSTCHR (TAGS (1), IRET) 
!               If there is a mnemonic, then make sure it's legal.      
            IF ( (IRET.EQ.0) .AND. (NEMOCK (TAGS (1) ) .NE.0) ) THEN 
      BORT_STR2 = '                  HAS ILLEGAL MNEMONIC' 
               GOTO 901 
            ENDIF 
            CMMNEM (NMTBB) = TAGS (1) (1:8) 
         ENDIF 
         IF (NTAG.GT.1) THEN 
!               The second additional field contains descriptor codes.  
            CALL JSTCHR (TAGS (2), IRET) 
            CMDSC (NMTBB) = TAGS (2) (1:4) 
         ENDIF 
         IF (NTAG.GT.2) THEN 
!               The third additional field contains the element name.   
            CALL JSTCHR (TAGS (3), IRET) 
            CMELEM (NMTBB) = TAGS (3) (1:120) 
         ENDIF 
      ENDIF 
                                                                        
      RETURN 
  900 CALL BORT ('BUFRLIB: SNTBBE - OVERFLOW OF MERGED ARRAYS') 
  901 BORT_STR1 = 'BUFRLIB: SNTBBE - CARD BEGINNING WITH: '//LINE (1:20) 
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END SUBROUTINE SNTBBE                         

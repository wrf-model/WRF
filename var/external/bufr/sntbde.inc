      SUBROUTINE SNTBDE (LUNT, IFXYN, LINE, MXMTBD, MXELEM, NMTBD,      &
      IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, CEELEM)             
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SNTBDE                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE PARSES THE FIRST LINE OF AN ENTRY THAT WAS 
!   PREVIOUSLY READ FROM AN ASCII MASTER TABLE D FILE AND STORES THE    
!   OUTPUT INTO THE MERGED ARRAYS.  IT THEN READS AND PARSES ALL        
!   REMAINING LINES FOR THAT SAME ENTRY AND THEN LIKEWISE STORES THAT   
!   OUTPUT INTO THE MERGED ARRAYS.  THE RESULT IS THAT, UPON OUTPUT,    
!   THE MERGED ARRAYS NOW CONTAIN ALL OF THE INFORMATION FOR THE        
!   CURRENT TABLE ENTRY.                                                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL SNTBDE ( LUNT, IFXYN, LINE, MXMTBD, MXELEM,            
!                         NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,          
!                         NMELEM, IEFXYN, CEELEM )                      
!   INPUT ARGUMENT LIST:                                                
!     LUNT     - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING MASTER TABLE D INFORMATION                  
!     IFXYN    - INTEGER: BIT-WISE REPRESENTATION OF FXY NUMBER FOR     
!                TABLE ENTRY; THIS FXY NUMBER IS THE SEQUENCE DESCRIPTOR
!     LINE     - CHARACTER*(*): FIRST LINE OF TABLE ENTRY               
!     MXMTBD   - INTEGER: MAXIMUM NUMBER OF ENTRIES TO BE STORED IN     
!                MERGED MASTER TABLE D ARRAYS; THIS SHOULD BE THE SAME  
!                NUMBER AS WAS USED TO DIMENSION THE OUTPUT ARRAYS IN   
!                THE CALLING PROGRAM, AND IT IS USED BY THIS SUBROUTINE 
!                TO ENSURE THAT IT DOESN'T OVERFLOW THESE ARRAYS        
!     MXELEM   - INTEGER: MAXIMUM NUMBER OF ELEMENTS TO BE STORED PER   
!                ENTRY WITHIN THE MERGED MASTER TABLE D ARRAYS; THIS    
!                SHOULD BE THE SAME NUMBER AS WAS USED TO DIMENSION THE 
!                OUTPUT ARRAYS IN THE CALLING PROGRAM, AND IT IS USED   
!                BY THIS SUBROUTINE TO ENSURE THAT IT DOESN'T OVERFLOW  
!                THESE ARRAYS                                           
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     NMTBD    - INTEGER: NUMBER OF ENTRIES IN MERGED MASTER TABLE D    
!                ARRAYS                                                 
!     IMFXYN(*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE              
!                REPRESENTATIONS OF FXY NUMBERS (I.E. SEQUENCE          
!                DESCRIPTORS)                                           
!     CMMNEM(*)- CHARACTER*8: MERGED ARRAY CONTAINING MNEMONICS         
!     CMDSC(*) - CHARACTER*4: MERGED ARRAY CONTAINING DESCRIPTOR CODES  
!     CMSEQ(*) - CHARACTER*120: MERGED ARRAY CONTAINING SEQUENCE NAMES  
!     NMELEM(*)- INTEGER: MERGED ARRAY CONTAINING NUMBER OF ELEMENTS    
!                STORED FOR EACH ENTRY                                  
!   IEFXYN(*,*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE              
!                REPRESENTATIONS OF ELEMENT FXY NUMBERS                 
!   CEELEM(*,*)- CHARACTER*120: MERGED ARRAY CONTAINING ELEMENT NAMES   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ADN30    BORT     BORT2    IFXY         
!                               IGETFXY  IGETNTBL JSTCHR   NEMOCK       
!                               PARSTR                                  
!    THIS ROUTINE IS CALLED BY: RDMTBD                                  
!                               Normally not called by any application  
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER ( * ) LINE 
      CHARACTER(200) TAGS (10), CLINE 
      CHARACTER(128) BORT_STR1, BORT_STR2 
      CHARACTER(120) CMSEQ ( * ), CEELEM (MXMTBD, MXELEM) 
      CHARACTER(8) CMMNEM ( * ) 
      CHARACTER(6) ADN30, ADSC, CLEMON 
      CHARACTER(4) CMDSC ( * ) 
                                                                        
      INTEGER IMFXYN ( * ), NMELEM ( * ), IEFXYN (MXMTBD, MXELEM) 
                                                                        
      LOGICAL DONE 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      IF (NMTBD.GE.MXMTBD) GOTO 900 
      NMTBD = NMTBD+1 
                                                                        
!       Store the FXY number.  This is the sequence descriptor.         
                                                                        
      IMFXYN (NMTBD) = IFXYN 
                                                                        
!       Is there any other information within the first line of the     
!       table entry?  If so, it follows a "|" separator.                
                                                                        
      CMMNEM (NMTBD) = ' ' 
      CMDSC (NMTBD) = ' ' 
      CMSEQ (NMTBD) = ' ' 
      IPT = INDEX (LINE, '|') 
      IF (IPT.NE.0) THEN 
                                                                        
!           Parse the rest of the line.  Any of the fields may be blank.
                                                                        
         CALL PARSTR (LINE (IPT + 1:) , TAGS, 10, NTAG, ';', .FALSE.) 
         IF (NTAG.GT.0) THEN 
!               The first additional field contains the mnemonic.       
            CALL JSTCHR (TAGS (1), IRET) 
!               If there is a mnemonic, then make sure it's legal.      
            IF ( (IRET.EQ.0) .AND. (NEMOCK (TAGS (1) ) .NE.0) ) THEN 
      BORT_STR2 = '                  HAS ILLEGAL MNEMONIC' 
               GOTO 901 
            ENDIF 
            CMMNEM (NMTBD) = TAGS (1) (1:8) 
         ENDIF 
         IF (NTAG.GT.1) THEN 
!               The second additional field contains descriptor codes.  
            CALL JSTCHR (TAGS (2), IRET) 
            CMDSC (NMTBD) = TAGS (2) (1:4) 
         ENDIF 
         IF (NTAG.GT.2) THEN 
!               The third additional field contains the sequence name.  
            CALL JSTCHR (TAGS (3), IRET) 
            CMSEQ (NMTBD) = TAGS (3) (1:120) 
         ENDIF 
      ENDIF 
                                                                        
!       Now, read and parse all remaining lines from this table entry.  
!       Each line should contain an element descriptor for the sequence 
!       represented by the current sequence descriptor.                 
                                                                        
      NELEM = 0 
      DONE = .FALSE. 
      DO WHILE (.NOT.DONE) 
      IF (IGETNTBL (LUNT, CLINE) .NE.0) THEN 
      BORT_STR2 = '                  IS INCOMPLETE' 
         GOTO 901 
      ENDIF 
      CALL PARSTR (CLINE, TAGS, 10, NTAG, '|', .FALSE.) 
      IF (NTAG.LT.2) THEN 
      BORT_STR2 = '                  HAS BAD ELEMENT CARD' 
         GOTO 901 
      ENDIF 
                                                                        
!           The second field contains the FXY number for this element.  
                                                                        
      IF (IGETFXY (TAGS (2), ADSC) .NE.0) THEN 
      BORT_STR2 = '                  HAS BAD OR MISSING'//' ELEMENT FXY &
     &NUMBER'                                                           
         GOTO 901 
      ENDIF 
      IF (NELEM.GE.MXELEM) GOTO 900 
      NELEM = NELEM + 1 
      IEFXYN (NMTBD, NELEM) = IFXY (ADSC) 
                                                                        
!           The third field (if it exists) contains the element name.   
                                                                        
      IF (NTAG.GT.2) THEN 
         CALL JSTCHR (TAGS (3), IRET) 
         CEELEM (NMTBD, NELEM) = TAGS (3) (1:120) 
      ELSE 
         CEELEM (NMTBD, NELEM) = ' ' 
      ENDIF 
                                                                        
!           Is this the last line for this table entry?                 
                                                                        
      IF (INDEX (TAGS (2) , ' >') .EQ.0) DONE = .TRUE. 
      ENDDO 
      NMELEM (NMTBD) = NELEM 
                                                                        
      RETURN 
                                                                        
  900 CALL BORT ('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS') 
  901 CLEMON = ADN30 (IFXYN, 6) 
      WRITE (BORT_STR1, '("BUFRLIB: SNTBDE - TABLE D ENTRY FOR'//' SEQUE&
     &NCE DESCRIPTOR: ",5A)') CLEMON (1:1) , '-', CLEMON (2:3) , '-', CL&
     &EMON (4:6)                                                        
      CALL BORT2 (BORT_STR1, BORT_STR2) 
      END SUBROUTINE SNTBDE                         

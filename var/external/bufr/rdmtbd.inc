      SUBROUTINE RDMTBD (LUNSTD, LUNLTD, MXMTBD, MXELEM, IMT, IMTV,     &
      IOGCE, ILTV, NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, &
      CEELEM)                                                           
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMTBD                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE READS MASTER TABLE D INFORMATION FROM TWO  
!   SEPARATE (I.E. ONE STANDARD AND ONE LOCAL) ASCII FILES AND THEN     
!   MERGES IT INTO A UNIFIED SET OF MASTER TABLE D ARRAYS FOR OUTPUT.   
!   EACH OF THE TWO INPUT FILES MUST ALREADY BE INDIVIDUALLY SORTED IN  
!   ASCENDING ORDER WITH RESPECT TO THE FXY NUMBERS.                    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL RDMTBD ( LUNSTD, LUNLTD, MXMTBD, MXELEM,               
!                         IMT, IMTV, IOGCE, ILTV,                       
!                         NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,          
!                         NMELEM, IEFXYN, CEELEM )                      
!   INPUT ARGUMENT LIST:                                                
!     LUNSTD   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING STANDARD TABLE D INFORMATION                
!     LUNLTD   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING LOCAL TABLE D INFORMATION                   
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
!     IMT      - INTEGER: MASTER TABLE, READ FROM EACH ASCII FILE       
!                (NOTE: THESE VALUES MUST BE THE SAME IN EACH FILE!)    
!     IMTV     - INTEGER: VERSION NUMBER OF MASTER TABLE, READ FROM     
!                STANDARD ASCII FILE                                    
!     IOGCE    - INTEGER: ORIGINATING CENTER, READ FROM LOCAL ASCII FILE
!     ILTV     - INTEGER: VERSION NUMBER OF LOCAL TABLE, READ FROM      
!                LOCAL ASCII FILE                                       
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
!    THIS ROUTINE CALLS:        ADN30    BORT     GETNTBE  GETTBH       
!                               SNTBDE   WRDLEN                         
!    THIS ROUTINE IS CALLED BY: None                                    
!                               Normally called only by application     
!                               programs.                               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      CHARACTER(200) STLINE, LTLINE 
      CHARACTER(128) BORT_STR 
      CHARACTER(120) CMSEQ ( * ), CEELEM (MXMTBD, MXELEM) 
      CHARACTER(8) CMMNEM ( * ) 
      CHARACTER(6) CMATCH, ADN30 
      CHARACTER(4) CMDSC ( * ) 
                                                                        
      INTEGER IMFXYN ( * ), NMELEM ( * ), IEFXYN (MXMTBD, MXELEM) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!       Call WRDLEN to initialize some important information about the  
!       local machine, just in case it hasn't already been called.      
                                                                        
      CALL WRDLEN 
                                                                        
!       Read and parse the header lines of both files.                  
                                                                        
      CALL GETTBH (LUNSTD, LUNLTD, 'D', IMT, IMTV, IOGCE, ILTV) 
                                                                        
!       Read through the remainder of both files, merging the           
!       contents into a unified set of master Table D arrays.           
                                                                        
      NMTBD = 0 
      CALL GETNTBE (LUNSTD, ISFXYN, STLINE, IERS) 
      CALL GETNTBE (LUNLTD, ILFXYN, LTLINE, IERL) 
      DO WHILE ( (IERS.EQ.0) .OR. (IERL.EQ.0) ) 
      IF ( (IERS.EQ.0) .AND. (IERL.EQ.0) ) THEN 
         IF (ISFXYN.EQ.ILFXYN) THEN 
            CMATCH = ADN30 (ISFXYN, 6) 
            GOTO 900 
         ELSEIF (ISFXYN.LT.ILFXYN) THEN 
            CALL SNTBDE (LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM, NMTBD, &
            IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, CEELEM)       
            CALL GETNTBE (LUNSTD, ISFXYN, STLINE, IERS) 
         ELSE 
            CALL SNTBDE (LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM, NMTBD, &
            IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, CEELEM)       
            CALL GETNTBE (LUNLTD, ILFXYN, LTLINE, IERL) 
         ENDIF 
      ELSEIF (IERS.EQ.0) THEN 
         CALL SNTBDE (LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM, NMTBD,    &
         IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, CEELEM)          
         CALL GETNTBE (LUNSTD, ISFXYN, STLINE, IERS) 
      ELSEIF (IERL.EQ.0) THEN 
         CALL SNTBDE (LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM, NMTBD,    &
         IMFXYN, CMMNEM, CMDSC, CMSEQ, NMELEM, IEFXYN, CEELEM)          
         CALL GETNTBE (LUNLTD, ILFXYN, LTLINE, IERL) 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: RDMTBD - STANDARD AND LOCAL'//' TABLE&
     & D FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') CMATCH (1:1) , '-',&
     & CMATCH (2:3) , '-', CMATCH (4:6)                                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE RDMTBD                         

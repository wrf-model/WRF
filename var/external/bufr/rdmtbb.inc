      SUBROUTINE RDMTBB (LUNSTB, LUNLTB, MXMTBB, IMT, IMTV, IOGCE, ILTV,&
      NMTBB, IMFXYN, CMSCL, CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RDMTBB                                                 
!   PRGMMR: ATOR            ORG: NP12       DATE: 2007-01-19            
!                                                                       
! ABSTRACT:  THIS SUBROUTINE READS MASTER TABLE B INFORMATION FROM TWO  
!   SEPARATE (I.E. ONE STANDARD AND ONE LOCAL) ASCII FILES AND THEN     
!   MERGES IT INTO A UNIFIED SET OF MASTER TABLE B ARRAYS FOR OUTPUT.   
!   EACH OF THE TWO INPUT FILES MUST ALREADY BE INDIVIDUALLY SORTED IN  
!   ASCENDING ORDER WITH RESPECT TO THE FXY NUMBERS.                    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2007-01-19  J. ATOR    -- ORIGINAL AUTHOR                             
!                                                                       
! USAGE:    CALL RDMTBB ( LUNSTB, LUNLTB, MXMTBB, IMT, IMTV, IOGCE,     
!                         ILTV, NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,     
!                         CMUNIT, CMMNEM, CMDSC, CMELEM )               
!   INPUT ARGUMENT LIST:                                                
!     LUNSTB   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING STANDARD TABLE B INFORMATION                
!     LUNLTB   - INTEGER: FORTRAN LOGICAL UNIT NUMBER OF ASCII FILE     
!                CONTAINING LOCAL TABLE B INFORMATION                   
!     MXMTBB   - INTEGER: MAXIMUM NUMBER OF ENTRIES TO BE STORED IN     
!                MERGED MASTER TABLE B ARRAYS; THIS SHOULD BE THE SAME  
!                NUMBER AS WAS USED TO DIMENSION THE OUTPUT ARRAYS IN   
!                THE CALLING PROGRAM, AND IT IS USED BY THIS SUBROUTINE 
!                TO ENSURE THAT IT DOESN'T OVERFLOW THESE ARRAYS        
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     IMT      - INTEGER: MASTER TABLE, READ FROM EACH ASCII FILE       
!                (NOTE: THESE VALUES MUST BE THE SAME IN EACH FILE!)    
!     IMTV     - INTEGER: VERSION NUMBER OF MASTER TABLE, READ FROM     
!                STANDARD ASCII FILE                                    
!     IOGCE    - INTEGER: ORIGINATING CENTER, READ FROM LOCAL ASCII FILE
!     ILTV     - INTEGER: VERSION NUMBER OF LOCAL TABLE, READ FROM      
!                LOCAL ASCII FILE                                       
!     NMTBB    - INTEGER: NUMBER OF ENTRIES IN MERGED MASTER TABLE B    
!                ARRAYS                                                 
!     IMFXYN(*)- INTEGER: MERGED ARRAY CONTAINING BIT-WISE              
!                REPRESENTATIONS OF FXY NUMBERS                         
!     CMSCL(*) - CHARACTER*4: MERGED ARRAY CONTAINING SCALE FACTORS     
!     CMSREF(*)- CHARACTER*12: MERGED ARRAY CONTAINING REFERENCE VALUES 
!     CMBW(*)  - CHARACTER*4: MERGED ARRAY CONTAINING BIT WIDTHS        
!     CMUNIT(*)- CHARACTER*14: MERGED ARRAY CONTAINING UNITS            
!     CMMNEM(*)- CHARACTER*8: MERGED ARRAY CONTAINING MNEMONICS         
!     CMDSC(*) - CHARACTER*4: MERGED ARRAY CONTAINING DESCRIPTOR CODES  
!     CMELEM(*)- CHARACTER*120: MERGED ARRAY CONTAINING ELEMENT NAMES   
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        ADN30    BORT     GETNTBE  GETTBH       
!                               SNTBBE   WRDLEN                         
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
      CHARACTER(120) CMELEM ( * ) 
      CHARACTER(14) CMUNIT ( * ) 
      CHARACTER(12) CMSREF ( * ) 
      CHARACTER(8) CMMNEM ( * ) 
      CHARACTER(6) CMATCH, ADN30 
      CHARACTER(4) CMSCL ( * ), CMBW ( * ), CMDSC ( * ) 
                                                                        
      INTEGER IMFXYN ( * ) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
!       Call WRDLEN to initialize some important information about the  
!       local machine, just in case it hasn't already been called.      
                                                                        
      CALL WRDLEN 
                                                                        
!       Read and parse the header lines of both files.                  
                                                                        
      CALL GETTBH (LUNSTB, LUNLTB, 'B', IMT, IMTV, IOGCE, ILTV) 
                                                                        
!       Read through the remainder of both files, merging the           
!       contents into a unified set of master Table B arrays.           
                                                                        
      NMTBB = 0 
      CALL GETNTBE (LUNSTB, ISFXYN, STLINE, IERS) 
      CALL GETNTBE (LUNLTB, ILFXYN, LTLINE, IERL) 
      DO WHILE ( (IERS.EQ.0) .OR. (IERL.EQ.0) ) 
      IF ( (IERS.EQ.0) .AND. (IERL.EQ.0) ) THEN 
         IF (ISFXYN.EQ.ILFXYN) THEN 
            CMATCH = ADN30 (ISFXYN, 6) 
            GOTO 900 
         ELSEIF (ISFXYN.LT.ILFXYN) THEN 
            CALL SNTBBE (ISFXYN, STLINE, MXMTBB, NMTBB, IMFXYN, CMSCL,  &
            CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)                
            CALL GETNTBE (LUNSTB, ISFXYN, STLINE, IERS) 
         ELSE 
            CALL SNTBBE (ILFXYN, LTLINE, MXMTBB, NMTBB, IMFXYN, CMSCL,  &
            CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)                
            CALL GETNTBE (LUNLTB, ILFXYN, LTLINE, IERL) 
         ENDIF 
      ELSEIF (IERS.EQ.0) THEN 
         CALL SNTBBE (ISFXYN, STLINE, MXMTBB, NMTBB, IMFXYN, CMSCL,     &
         CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)                   
         CALL GETNTBE (LUNSTB, ISFXYN, STLINE, IERS) 
      ELSEIF (IERL.EQ.0) THEN 
         CALL SNTBBE (ILFXYN, LTLINE, MXMTBB, NMTBB, IMFXYN, CMSCL,     &
         CMSREF, CMBW, CMUNIT, CMMNEM, CMDSC, CMELEM)                   
         CALL GETNTBE (LUNLTB, ILFXYN, LTLINE, IERL) 
      ENDIF 
      ENDDO 
                                                                        
      RETURN 
  900 WRITE (BORT_STR, '("BUFRLIB: RDMTBB - STANDARD AND LOCAL'//' TABLE&
     & B FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') CMATCH (1:1) , '-',&
     & CMATCH (2:3) , '-', CMATCH (4:6)                                 
      CALL BORT (BORT_STR) 
      END SUBROUTINE RDMTBB                         

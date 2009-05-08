      SUBROUTINE MESGBC (LUNIN, MESGTYP, ICOMP) 
                                                                        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    MESGBC                                                 
!   PRGMMR: KEYSER           ORG: NP22       DATE: 2004-06-29           
!                                                                       
! ABSTRACT: THIS SUBROUTINE EXAMINES A BUFR MESSAGE AND RETURNS BOTH    
!  THE MESSAGE TYPE FROM SECTION 1 AND A MESSAGE COMPRESSION INDICATOR  
!  UNPACKED FROM SECTION 3.  IT OBTAINS THE BUFR MESSAGE VIA TWO        
!  DIFFERENT METHODS, BASED UPON THE SIGN OF LUNIN.                     
!     IF LUNIN IS GREATER THAN ZERO, THIS SUBROUTINE READS AND EXAMINES 
!  SECTION 1 OF MESSAGES IN A BUFR FILE IN SEQUENCE UNTIL IT FINDS THE  
!  FIRST MESSAGE THAT ACTUALLY CONTAINS REPORT DATA {I.E., BEYOND THE   
!  BUFR TABLE (DICTIONARY) MESSAGES AT THE TOP AND, FOR DUMP FILES,     
!  BEYOND THE TWO DUMMY MESSAGES CONTAINING THE CENTER TIME AND THE     
!  DUMP TIME}.  IT THEN RETURNS THE MESSAGE TYPE AND COMPRESSION        
!  INDICATOR FOR THIS FIRST DATA MESSAGE.  IN THIS CASE, THE BUFR FILE  
!  SHOULD NOT BE OPENED VIA BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF      
!  PRIOR TO CALLING THIS SUBROUTINE.  HOWEVER, THE BUFR FILE MUST BE    
!  CONNECTED TO UNIT ABS(LUNIN).  WHEN USED THIS WAY, THIS SUBROUTINE   
!  IS IDENTICAL TO BUFR ARCHIVE LIBRARY SUBROUTINE MESGBF EXCEPT MESGBF 
!  DOES NOT RETURN ANY INFORMATION ABOUT COMPRESSION AND MESGBF READS   
!  UNTIL IT FINDS THE FIRST NON-DICTIONARY MESSAGE REGARDLESS OF        
!  WHETHER OR NOT IT CONTAINS ANY REPORTS (I.E., IT WOULD STOP AT THE   
!  DUMMY MESSAGE CONTAINING THE CENTER TIME FOR DUMP FILES).            
!     THE SECOND METHOD IN WHICH THIS SUBROUTINE CAN BE USED OCCURS     
!  WHEN LUNIN IS PASSED IN WITH A VALUE LESS THAN ZERO.  IN THIS CASE,  
!  IT SIMPLY RETURNS THE MESSAGE TYPE AND COMPRESSION INDICATOR FOR THE 
!  BUFR MESSAGE CURRENTLY STORED IN THE INTERNAL MESSAGE BUFFER (ARRAY  
!  MBAY IN COMMON BLOCK /BITBUF/).  IN THIS CASE, THE BUFR FILE         
!  CONNECTED TO ABS(LUNIN) MUST HAVE BEEN PREVIOUSLY OPENED FOR INPUT   
!  OPERATIONS BY BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF, AND THE BUFR   
!  MESSAGE MUST HAVE BEEN READ INTO MEMORY BY BUFR ARCHIVE LIBRARY      
!  ROUTINE READMG OR EQUIVALENT.                                        
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2003-11-04  D. KEYSER  -- ORIGINAL AUTHOR                             
! 2004-06-29  D. KEYSER  -- ADDED NEW OPTION TO RETURN MESSAGE TYPE AND 
!                           COMPRESSION INDICATOR FOR BUFR MESSAGE      
!                           CURRENTLY STORED IN MEMORY (TRIGGERED BY    
!                           INPUT ARGUMENT LUNIN LESS THAN ZERO)        
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM       
!                           20,000 TO 50,000 BYTES                      
! 2005-11-29  J. ATOR    -- USE IUPBS01, GETLENS AND RDMSGW             
!                                                                       
! USAGE:    CALL MESGBC (LUNIN, MESGTYP, ICOMP)                         
!   INPUT ARGUMENT LIST:                                                
!     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER 
!                FOR BUFR FILE                                          
!                  - IF LUNIN IS GREATER THAN ZERO, THIS SUBROUTINE     
!                    READS THROUGH ALL BUFR MESSAGES FROM BEGINNING OF  
!                    FILE UNTIL IT FINDS THE FIRST MESSAGE CONTAINING   
!                    REPORT DATA                                        
!                  - IF LUNIN IS LESS THAN ZERO, THIS SUBROUTINE        
!                    OPERATES ON THE BUFR MESSAGE CURRENTLY STORED IN   
!                    MEMORY                                             
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     MESGTYP  - INTEGER: BUFR MESSAGE TYPE FOR EITHER THE FIRST        
!                MESSAGE IN FILE CONTAINING REPORT DATA (IF LUNIN > 0), 
!                OR FOR THE MESSAGE CURRENTLY IN MEMORY (IF LUNIN < 0)  
!                    -256 = for LUNIN > 0 case only: no messages read   
!                           or error reading file                       
!                     < 0 = for LUNIN > 0 case only: none of the        
!                           messages read contain reports; this is the  
!                           negative of the message type the last       
!                           message read (i.e., -11 indicates the BUFR  
!                           file contains only BUFR table messages)     
!     ICOMP    - INTEGER: BUFR MESSAGE COMPRESSION SWITCH:              
!                      -3 = for LUNIN > 0 case only: BUFR file does not 
!                           exist                                       
!                      -2 = for LUNIN > 0 case only: BUFR file does not 
!                           contain any report messages                 
!                      -1 = for LUNIN > 0 case only: cannot determine   
!                           if first BUFR message containing report     
!                           data is compressed due to error reading     
!                           file                                        
!                       0 = BUFR message (either first containing       
!                           report data if LUNIN > 0, or that currently 
!                           in memory if LUNIN < 0) is NOT compressed   
!                       1 = BUFR message (either first containing       
!                           report data if LUNIN > 0, or that currently 
!                           in memory if LUNIN < 0) IS compressed       
!                                                                       
!   INPUT FILES:                                                        
!     UNIT ABS(LUNIN) - BUFR FILE                                       
!                                                                       
! REMARKS:                                                              
!    THIS ROUTINE CALLS:        GETLENS  IUPB     IUPBS01  RDMSGW       
!                               STATUS   WRDLEN                         
!    THIS ROUTINE IS CALLED BY: COPYSB   UFBTAB                         
!                               Also called by application programs.    
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN 77                                                
!   MACHINE:  PORTABLE TO ALL PLATFORMS                                 
!                                                                       
!$$$                                                                    
                                                                        
      INCLUDE 'bufrlib.prm' 
                                                                        
      COMMON / BITBUF / MAXBYT, IBIT, IBAY (MXMSGLD4), MBYT (NFILES),   &
      MBAY (MXMSGLD4, NFILES)                                           
                                                                        
      DIMENSION MSGS (MXMSGLD4) 
                                                                        
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                                                                        
      LUNIT = ABS (LUNIN) 
                                                                        
!  DETERMINE METHOD OF OPERATION BASED ON SIGN OF LUNIN                 
!   LUNIN > 0 - REWIND AND LOOK FOR FIRST DATA MESSAGE (ITYPE = 0)      
!   LUNIN < 0 - LOOK AT MESSAGE CURRENLY IN MEMORY     (ITYPE = 1)      
!  ---------------------------------------------------------------      
                                                                        
      ITYPE = 0 
      IF (LUNIT.NE.LUNIN) ITYPE = 1 
                                                                        
      ICOMP = - 1 
      MESGTYP = - 256 
                                                                        
      IF (ITYPE.EQ.0) THEN 
                                                                        
         IREC = 0 
         NSUB = 0 
                                                                        
!  SINCE OPENBF HAS NOT YET BEEN CALLED, MUST CALL WRDLEN TO GET        
!  MACHINE INFO NEEDED LATER                                            
!  -------------------------------------------------------------        
                                                                        
         CALL WRDLEN 
                                                                        
!  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND    
!  -----------------------------------------------------------------    
                                                                        
         REWIND LUNIT 
                                                                        
    1    CALL RDMSGW (LUNIT, MSGS, IER) 
         IF (IER.EQ. - 1) GOTO 900 
         IF (IER.EQ. - 2) GOTO 901 
                                                                        
         IREC = IREC + 1 
                                                                        
         MESGTYP = IUPBS01 (MSGS, 'MTYP') 
                                                                        
         CALL GETLENS (MSGS, 2, LEN0, LEN1, LEN2, L3, L4, L5) 
         IPT = LEN0 + LEN1 + LEN2 
                                                                        
         NSUB = IUPB (MSGS, IPT + 5, 16) 
                                                                        
         IF (MESGTYP.EQ.11.OR.NSUB.EQ.0) GOTO 1 
                                                                        
      ELSE 
                                                                        
!  RETURN MESSAGE TYPE FOR MESSAGE CURRENTLY STORED IN MEMORY           
!  ----------------------------------------------------------           
                                                                        
         CALL STATUS (LUNIT, LUN, IL, IM) 
                                                                        
         DO I = 1, 12 
         MSGS (I) = MBAY (I, LUN) 
         ENDDO 
                                                                        
         MESGTYP = IUPBS01 (MSGS, 'MTYP') 
                                                                        
         CALL GETLENS (MSGS, 2, LEN0, LEN1, LEN2, L3, L4, L5) 
         IPT = LEN0 + LEN1 + LEN2 
                                                                        
      ENDIF 
                                                                        
      ICOMP = 0 
      NCMP = IUPB (MSGS, IPT + 7, 8) 
                                                                        
!  BUFR MESSAGES ARE COMPRESSED IF BELOW IS TRUE                        
!  ---------------------------------------------                        
                                                                        
      IF (IAND (NCMP, 64) .GT.0) ICOMP = 1 
      IF (itype.eq.0) REWIND LUNIT 
      GOTO 100 
                                                                        
!  CAN ONLY GET TO STATEMENTS 900 OR 901 WHEN ITYPE = 0                 
!  ----------------------------------------------------                 
                                                                        
  900 IF (IREC.EQ.0) THEN 
         MESGTYP = - 256 
         ICOMP = - 3 
      ELSE 
         IF (MESGTYP.GE.0) MESGTYP = - MESGTYP 
         ICOMP = - 2 
         REWIND LUNIT 
      ENDIF 
      GOTO 100 
                                                                        
  901 MESGTYP = - 256 
      ICOMP = - 1 
      REWIND LUNIT 
                                                                        
!  EXIT                                                                 
!  ----                                                                 
                                                                        
  100 RETURN 
      END SUBROUTINE MESGBC                         

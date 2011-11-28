      SUBROUTINE MESGBC(LUNIN,MESGTYP,ICOMP)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MESGBC
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2004-06-29
C
C ABSTRACT: THIS SUBROUTINE EXAMINES A BUFR MESSAGE AND RETURNS BOTH
C  THE MESSAGE TYPE FROM SECTION 1 AND A MESSAGE COMPRESSION INDICATOR
C  UNPACKED FROM SECTION 3.  IT OBTAINS THE BUFR MESSAGE VIA TWO
C  DIFFERENT METHODS, BASED UPON THE SIGN OF LUNIN.
C     IF LUNIN IS GREATER THAN ZERO, THIS SUBROUTINE READS AND EXAMINES
C  SECTION 1 OF MESSAGES IN A BUFR FILE IN SEQUENCE UNTIL IT FINDS THE
C  FIRST MESSAGE THAT ACTUALLY CONTAINS REPORT DATA {I.E., BEYOND THE
C  BUFR TABLE (DICTIONARY) MESSAGES AT THE TOP AND, FOR DUMP FILES,
C  BEYOND THE TWO DUMMY MESSAGES CONTAINING THE CENTER TIME AND THE
C  DUMP TIME}.  IT THEN RETURNS THE MESSAGE TYPE AND COMPRESSION
C  INDICATOR FOR THIS FIRST DATA MESSAGE.  IN THIS CASE, THE BUFR FILE
C  SHOULD NOT BE OPENED VIA BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF
C  PRIOR TO CALLING THIS SUBROUTINE.  HOWEVER, THE BUFR FILE MUST BE
C  CONNECTED TO UNIT ABS(LUNIN).  WHEN USED THIS WAY, THIS SUBROUTINE
C  IS IDENTICAL TO BUFR ARCHIVE LIBRARY SUBROUTINE MESGBF EXCEPT MESGBF
C  DOES NOT RETURN ANY INFORMATION ABOUT COMPRESSION AND MESGBF READS
C  UNTIL IT FINDS THE FIRST NON-DICTIONARY MESSAGE REGARDLESS OF
C  WHETHER OR NOT IT CONTAINS ANY REPORTS (I.E., IT WOULD STOP AT THE
C  DUMMY MESSAGE CONTAINING THE CENTER TIME FOR DUMP FILES).
C     THE SECOND METHOD IN WHICH THIS SUBROUTINE CAN BE USED OCCURS
C  WHEN LUNIN IS PASSED IN WITH A VALUE LESS THAN ZERO.  IN THIS CASE,
C  IT SIMPLY RETURNS THE MESSAGE TYPE AND COMPRESSION INDICATOR FOR THE
C  BUFR MESSAGE CURRENTLY STORED IN THE INTERNAL MESSAGE BUFFER (ARRAY
C  MBAY IN COMMON BLOCK /BITBUF/).  IN THIS CASE, THE BUFR FILE
C  CONNECTED TO ABS(LUNIN) MUST HAVE BEEN PREVIOUSLY OPENED FOR INPUT
C  OPERATIONS BY BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF, AND THE BUFR
C  MESSAGE MUST HAVE BEEN READ INTO MEMORY BY BUFR ARCHIVE LIBRARY
C  ROUTINE READMG OR EQUIVALENT.
C
C PROGRAM HISTORY LOG:
C 2003-11-04  D. KEYSER  -- ORIGINAL AUTHOR
C 2004-06-29  D. KEYSER  -- ADDED NEW OPTION TO RETURN MESSAGE TYPE AND
C                           COMPRESSION INDICATOR FOR BUFR MESSAGE
C                           CURRENTLY STORED IN MEMORY (TRIGGERED BY
C                           INPUT ARGUMENT LUNIN LESS THAN ZERO)
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- USE IUPBS01, GETLENS AND RDMSGW
C
C USAGE:    CALL MESGBC (LUNIN, MESGTYP, ICOMP)
C   INPUT ARGUMENT LIST:
C     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
C                FOR BUFR FILE
C                  - IF LUNIN IS GREATER THAN ZERO, THIS SUBROUTINE
C                    READS THROUGH ALL BUFR MESSAGES FROM BEGINNING OF
C                    FILE UNTIL IT FINDS THE FIRST MESSAGE CONTAINING
C                    REPORT DATA
C                  - IF LUNIN IS LESS THAN ZERO, THIS SUBROUTINE
C                    OPERATES ON THE BUFR MESSAGE CURRENTLY STORED IN
C                    MEMORY
C
C   OUTPUT ARGUMENT LIST:
C     MESGTYP  - INTEGER: BUFR MESSAGE TYPE FOR EITHER THE FIRST
C                MESSAGE IN FILE CONTAINING REPORT DATA (IF LUNIN > 0),
C                OR FOR THE MESSAGE CURRENTLY IN MEMORY (IF LUNIN < 0)
C                    -256 = for LUNIN > 0 case only: no messages read
C                           or error reading file
C                     < 0 = for LUNIN > 0 case only: none of the
C                           messages read contain reports; this is the
C                           negative of the message type the last
C                           message read (i.e., -11 indicates the BUFR
C                           file contains only BUFR table messages)
C     ICOMP    - INTEGER: BUFR MESSAGE COMPRESSION SWITCH:
C                      -3 = for LUNIN > 0 case only: BUFR file does not
C                           exist
C                      -2 = for LUNIN > 0 case only: BUFR file does not
C                           contain any report messages
C                      -1 = for LUNIN > 0 case only: cannot determine
C                           if first BUFR message containing report
C                           data is compressed due to error reading
C                           file
C                       0 = BUFR message (either first containing
C                           report data if LUNIN > 0, or that currently
C                           in memory if LUNIN < 0) is NOT compressed
C                       1 = BUFR message (either first containing
C                           report data if LUNIN > 0, or that currently
C                           in memory if LUNIN < 0) IS compressed
C
C   INPUT FILES:
C     UNIT ABS(LUNIN) - BUFR FILE
C
C REMARKS:
C    THIS ROUTINE CALLS:        GETLENS  IUPB     IUPBS01  RDMSGW
C                               STATUS   WRDLEN
C    THIS ROUTINE IS CALLED BY: COPYSB   UFBTAB
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .                MBAY(MXMSGLD4,NFILES)

      DIMENSION    MSGS(MXMSGLD4)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LUNIT = ABS(LUNIN)

C  DETERMINE METHOD OF OPERATION BASED ON SIGN OF LUNIN
C   LUNIN > 0 - REWIND AND LOOK FOR FIRST DATA MESSAGE (ITYPE = 0)
C   LUNIN < 0 - LOOK AT MESSAGE CURRENLY IN MEMORY     (ITYPE = 1)
C  ---------------------------------------------------------------

      ITYPE = 0
      IF(LUNIT.NE.LUNIN) ITYPE = 1

      ICOMP   =   -1
      MESGTYP = -256

      IF(ITYPE.EQ.0) THEN

         IREC    =    0
         NSUB    =    0

C  SINCE OPENBF HAS NOT YET BEEN CALLED, MUST CALL WRDLEN TO GET
C  MACHINE INFO NEEDED LATER
C  -------------------------------------------------------------

         CALL WRDLEN

C  READ PAST ANY BUFR TABLES AND RETURN THE FIRST MESSAGE TYPE FOUND
C  -----------------------------------------------------------------

         REWIND LUNIT

1        CALL RDMSGW(LUNIT,MSGS,IER)
         IF(IER.EQ.-1) GOTO 900
         IF(IER.EQ.-2) GOTO 901

         IREC = IREC + 1

         MESGTYP = IUPBS01(MSGS,'MTYP') 

         CALL GETLENS(MSGS,2,LEN0,LEN1,LEN2,L3,L4,L5)
         IPT = LEN0+LEN1+LEN2

         NSUB = IUPB(MSGS,IPT+5,16)

         IF(MESGTYP.EQ.11 .OR. NSUB.EQ.0) GO TO 1

      ELSE

C  RETURN MESSAGE TYPE FOR MESSAGE CURRENTLY STORED IN MEMORY
C  ----------------------------------------------------------

         CALL STATUS(LUNIT,LUN,IL,IM)

         DO I=1,12
           MSGS(I) = MBAY(I,LUN)
         ENDDO

         MESGTYP = IUPBS01(MSGS,'MTYP') 

         CALL GETLENS(MSGS,2,LEN0,LEN1,LEN2,L3,L4,L5)
         IPT = LEN0+LEN1+LEN2

      END IF

      ICOMP = 0
      NCMP = IUPB(MSGS,IPT+7,8)

C  BUFR MESSAGES ARE COMPRESSED IF BELOW IS TRUE
C  ---------------------------------------------

      IF(IAND(NCMP,64).GT.0) ICOMP = 1
      if(itype.eq.0)  REWIND LUNIT
      GOTO 100

C  CAN ONLY GET TO STATEMENTS 900 OR 901 WHEN ITYPE = 0
C  ----------------------------------------------------

900   IF(IREC.EQ.0) THEN
         MESGTYP = -256
         ICOMP =     -3
      ELSE
         IF(MESGTYP.GE.0) MESGTYP = -MESGTYP
         ICOMP  = -2
         REWIND LUNIT
      ENDIF
      GOTO 100

901   MESGTYP = -256
      ICOMP =     -1
      REWIND LUNIT

C  EXIT
C  ----

100   RETURN
      END

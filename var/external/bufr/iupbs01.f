      FUNCTION IUPBS01(MBAY,S01MNEM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IUPBS01
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS A SPECIFIED INTEGER VALUE
C   FROM SECTION 0 OR SECTION 1 OF THE BUFR MESSAGE STORED IN ARRAY
C   MBAY.  IT WILL WORK ON ANY MESSAGE ENCODED USING BUFR EDITION 2, 3
C   OR 4.  THE START OF THE BUFR MESSAGE (I.E. THE STRING "BUFR") MUST
C   BE ALIGNED ON THE FIRST FOUR BYTES OF MBAY, AND THE VALUE TO BE
C   UNPACKED IS SPECIFIED VIA THE MNEMONIC S01MNEM, AS EXPLAINED IN
C   FURTHER DETAIL BELOW.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C 2006-04-14  J. ATOR    -- ADDED OPTIONS FOR 'YCEN' AND 'CENT';
C                           RESTRUCTURED LOGIC
C
C USAGE:    IUPBS01 (MBAY, S01MNEM)
C   INPUT ARGUMENT LIST:
C     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
C                BUFR MESSAGE
C     S01MNEM  - CHARACTER*(*): MNEMONIC SPECIFYING VALUE TO BE
C                UNPACKED FROM SECTION 0 OR SECTION 1 OF BUFR MESSAGE:
C                  'LENM'  = LENGTH (IN BYTES) OF BUFR MESSAGE
C                  'LEN0'  = LENGTH (IN BYTES) OF SECTION 0
C                  'BEN'   = BUFR EDITION NUMBER 
C                  'LEN1'  = LENGTH (IN BYTES) OF SECTION 1
C                  'BMT'   = BUFR MASTER TABLE 
C                  'OGCE'  = ORIGINATING CENTER
C                  'GSES'  = ORIGINATING SUBCENTER
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 3 OR 4 MESSAGES!)
C                  'USN'   = UPDATE SEQUENCE NUMBER 
C                  'ISC2'  = FLAG INDICATING ABSENCE/PRESENCE OF
C                            (OPTIONAL) SECTION 2 IN BUFR MESSAGE:
C                              0 = SECTION 2 ABSENT
C                              1 = SECTION 2 PRESENT
C                  'MTYP'  = DATA CATEGORY 
C                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
C                  'MTV'   = VERSION NUMBER OF MASTER TABLE
C                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
C                  'YCEN'  = YEAR OF CENTURY (1-100)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 2 AND 3 MESSAGES!)
C                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
C                                           21 FOR YEARS 2001-2100)
C                              (NOTE: THIS VALUE *MAY* BE PRESENT IN
C                                     BUFR EDITION 2 AND 3 MESSAGES,
C                                     BUT IT IS NEVER PRESENT IN ANY
C                                     BUFR EDITION 4 MESSAGES!)
C                  'YEAR'  = YEAR (4-DIGIT)
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES.  FOR
C                                     BUFR EDITION 2 AND 3 MESSAGES
C                                     IT WILL BE CALCULATED USING THE
C                                     VALUES FOR 'YCEN' AND 'CENT',
C                                     EXCEPT WHEN THE LATTER IS NOT
C                                     PRESENT AND IN WHICH CASE A
C                                     "WINDOWING" TECHNIQUE WILL BE
C                                     USED INSTEAD!)
C                  'MNTH'  = MONTH
C                  'DAYS'  = DAY
C                  'HOUR'  = HOUR
C                  'MINU'  = MINUTE
C                  'SECO'  = SECOND
C                              (NOTE: THIS VALUE IS PRESENT ONLY IN
C                                     BUFR EDITION 4 MESSAGES!)
C
C   OUTPUT ARGUMENT LIST:
C     IUPBS01  - INTEGER: UNPACKED INTEGER VALUE
C                  -1 = THE INPUT S01MNEM MNEMONIC WAS INVALID FOR
C                       THE EDITION OF BUFR MESSAGE IN MBAY
C
C REMARKS:
C    THIS ROUTINE CALLS:        GETS1LOC I4DY     IUPB     WRDLEN
C    THIS ROUTINE IS CALLED BY: ATRCPT   CKTABA   CNVED4   COPYBF
C                               COPYMG   CPYMEM   CRBMG    CRDBUFR
C                               DUMPBF   GETLENS  IDXMSG   IGETDATE
C                               IUPVS01  MESGBC   MESGBF   MSGWRT
C                               NMWRD    PADMSG   PKBS1    RDMSGB
C                               READS3   RTRCPT   STBFDX   STNDRD
C                               UFBMEX   WRCMPS
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S01MNEM

	LOGICAL		OK4CENT

C-----------------------------------------------------------------------
C	This statement function checks whether its input value contains
C       a valid century value.

	OK4CENT(IVAL) = ((IVAL.GE.19).AND.(IVAL.LE.21))
C-----------------------------------------------------------------------

C	Call subroutine WRDLEN to initialize some important information
C	about the local machine, just in case subroutine OPENBF hasn't
C	been called yet.

	CALL WRDLEN

C	Handle some simple requests that do not depend on the BUFR
C       edition number.

	IF(S01MNEM.EQ.'LENM') THEN
	    IUPBS01 = IUPB(MBAY,5,24)
	    RETURN
	ENDIF

	LEN0 = 8
	IF(S01MNEM.EQ.'LEN0') THEN
	    IUPBS01 = LEN0
	    RETURN
	ENDIF

C	Get the BUFR edition number.

	IBEN = IUPB(MBAY,8,8)
	IF(S01MNEM.EQ.'BEN') THEN
	    IUPBS01 = IBEN
	    RETURN
	ENDIF

C	Use the BUFR edition number to handle any other requests.

	CALL GETS1LOC(S01MNEM,IBEN,ISBYT,IWID,IRET)
	IF(IRET.EQ.0) THEN
	    IUPBS01 = IUPB(MBAY,LEN0+ISBYT,IWID)
	    IF(S01MNEM.EQ.'CENT') THEN

C		Test whether the returned value was a valid
C		century value.

		IF(.NOT.OK4CENT(IUPBS01)) IUPBS01 = -1
            ENDIF
        ELSE IF( (S01MNEM.EQ.'YEAR') .AND. (IBEN.LT.4) ) THEN

C	    Calculate the 4-digit year.

	    IYOC = IUPB(MBAY,21,8)
	    ICEN = IUPB(MBAY,26,8)

C	    Does ICEN contain a valid century value?

	    IF(OK4CENT(ICEN)) THEN

C               YES, so use it to calculate the 4-digit year. Note that,
C               by international convention, the year 2000 was the 100th
C               year of the 20th century, and the year 2001 was the 1st
C               year of the 21st century

		IUPBS01 = (ICEN-1)*100 + IYOC
	    ELSE

C               NO, so use a windowing technique to determine the
C               4-digit year from the year of the century.

		IUPBS01 = I4DY(MOD(IYOC,100)*1000000)/10**6
	    ENDIF
	ELSE
	    IUPBS01 = -1
	ENDIF

	RETURN
	END

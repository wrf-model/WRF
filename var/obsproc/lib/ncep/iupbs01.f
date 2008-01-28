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
C                              (NOTE: THIS VALUE EXISTS ONLY IN
C                              BUFR EDITION 3 OR 4 MESSAGES!)
C                  'USN'   = UPDATE SEQUENCE NUMBER 
C                  'ISC2'  = FLAG INDICATING ABSENCE/PRESENCE OF
C                            (OPTIONAL) SECTION 2 IN BUFR MESSAGE:
C                              0 = SECTION 2 ABSENT
C                              1 = SECTION 2 PRESENT
C                  'MTYP'  = DATA CATEGORY 
C                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
C                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
C                              (NOTE: THIS VALUE EXISTS ONLY IN
C                              BUFR EDITION 4 MESSAGES!)
C                  'MTV'   = VERSION NUMBER OF MASTER TABLE
C                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
C                  'YEAR'  = YEAR (4-DIGIT)
C                  'MNTH'  = MONTH
C                  'DAYS'  = DAY
C                  'HOUR'  = HOUR
C                  'MINU'  = MINUTE
C                  'SECO'  = SECOND
C                              (NOTE: THIS VALUE EXISTS ONLY IN
C                              BUFR EDITION 4 MESSAGES!)
C
C   OUTPUT ARGUMENT LIST:
C     IUPBS01  - INTEGER: UNPACKED INTEGER VALUE
C                  -1 = THE INPUT S01MNEM MNEMONIC WAS INVALID FOR
C                       THE EDITION OF BUFR MESSAGE IN MBAY
C
C REMARKS:
C    THIS ROUTINE CALLS:        GETS1LOC I4DY     IUPB     WRDLEN
C    THIS ROUTINE IS CALLED BY: CKTABA   CNVED4   COPYMG   CPYMEM
C                               CRBMG    DATEBF   DUMPBF   GETLENS
C                               IGETDATE IUPVS01  MESGBC   MESGBF
C                               MSGWRT   NMWRD    PADMSG   PKBS1
C                               RDBFDX   RDMSGB   STNDRD   UFBPOS
C                               WRCMPS
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S01MNEM

C-----------------------------------------------------------------------
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
	IF(IRET.LT.0) THEN
	    IUPBS01 = -1
	    RETURN
	ENDIF
	IUPBS01 = IUPB(MBAY,LEN0+ISBYT,IWID)

	IF( (S01MNEM.EQ.'YEAR') .AND. (IBEN.LT.4) ) THEN

C	    The value returned was actually the year of the century
C           rather than the full 4-digit year, so we need to compute
C	    the latter.

	    IYOC = IUPBS01
	    ICEN = IUPB(MBAY,26,8)

C	    Does ICEN contain a century value?

	    IF((ICEN.GE.19).AND.(ICEN.LE.21)) THEN

C		YES, so use it to calculate the 4-digit year. Note that,
C		by international convention, the year 2000 was the 100th
C		year of the 20th century, and the year 2001 was the 1st
C       	year of the 21st century

		IUPBS01 = (ICEN-1)*100 + IYOC
	    ELSE

C		NO, so use a windowing technique to determine the
C		4-digit year from the year of the century.

		IUPBS01 = I4DY(MOD(IYOC,100)*1000000)/10**6
	    ENDIF
	ENDIF

	RETURN
	END

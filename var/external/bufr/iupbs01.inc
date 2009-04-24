      FUNCTION IUPBS01(MBAY,S01MNEM)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    IUPBS01
!   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
!
! ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS A SPECIFIED INTEGER VALUE
!   FROM SECTION 0 OR SECTION 1 OF THE BUFR MESSAGE STORED IN ARRAY
!   MBAY.  IT WILL WORK ON ANY MESSAGE ENCODED USING BUFR EDITION 2, 3
!   OR 4.  THE START OF THE BUFR MESSAGE (I.E. THE STRING "BUFR") MUST
!   BE ALIGNED ON THE FIRST FOUR BYTES OF MBAY, AND THE VALUE TO BE
!   UNPACKED IS SPECIFIED VIA THE MNEMONIC S01MNEM, AS EXPLAINED IN
!   FURTHER DETAIL BELOW.
!
! PROGRAM HISTORY LOG:
! 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
! 2006-04-14  J. ATOR    -- ADDED OPTIONS FOR 'YCEN' AND 'CENT';
!                           RESTRUCTURED LOGIC
!
! USAGE:    IUPBS01 (MBAY, S01MNEM)
!   INPUT ARGUMENT LIST:
!     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
!                BUFR MESSAGE
!     S01MNEM  - CHARACTER*(*): MNEMONIC SPECIFYING VALUE TO BE
!                UNPACKED FROM SECTION 0 OR SECTION 1 OF BUFR MESSAGE:
!                  'LENM'  = LENGTH (IN BYTES) OF BUFR MESSAGE
!                  'LEN0'  = LENGTH (IN BYTES) OF SECTION 0
!                  'BEN'   = BUFR EDITION NUMBER 
!                  'LEN1'  = LENGTH (IN BYTES) OF SECTION 1
!                  'BMT'   = BUFR MASTER TABLE 
!                  'OGCE'  = ORIGINATING CENTER
!                  'GSES'  = ORIGINATING SUBCENTER
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 3 OR 4 MESSAGES!)
!                  'USN'   = UPDATE SEQUENCE NUMBER 
!                  'ISC2'  = FLAG INDICATING ABSENCE/PRESENCE OF
!                            (OPTIONAL) SECTION 2 IN BUFR MESSAGE:
!                              0 = SECTION 2 ABSENT
!                              1 = SECTION 2 PRESENT
!                  'MTYP'  = DATA CATEGORY 
!                  'MSBTI' = DATA SUBCATEGORY (INTERNATIONAL)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!                  'MSBT'  = DATA SUBCATEGORY (LOCAL)
!                  'MTV'   = VERSION NUMBER OF MASTER TABLE
!                  'MTVL'  = VERSION NUMBER OF LOCAL TABLES
!                  'YCEN'  = YEAR OF CENTURY (1-100)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 2 AND 3 MESSAGES!)
!                  'CENT'  = CENTURY (I.E., 20 FOR YEARS 1901-2000,
!                                           21 FOR YEARS 2001-2100)
!                              (NOTE: THIS VALUE *MAY* BE PRESENT IN
!                                     BUFR EDITION 2 AND 3 MESSAGES,
!                                     BUT IT IS NEVER PRESENT IN ANY
!                                     BUFR EDITION 4 MESSAGES!)
!                  'YEAR'  = YEAR (4-DIGIT)
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES.  FOR
!                                     BUFR EDITION 2 AND 3 MESSAGES
!                                     IT WILL BE CALCULATED USING THE
!                                     VALUES FOR 'YCEN' AND 'CENT',
!                                     EXCEPT WHEN THE LATTER IS NOT
!                                     PRESENT AND IN WHICH CASE A
!                                     "WINDOWING" TECHNIQUE WILL BE
!                                     USED INSTEAD!)
!                  'MNTH'  = MONTH
!                  'DAYS'  = DAY
!                  'HOUR'  = HOUR
!                  'MINU'  = MINUTE
!                  'SECO'  = SECOND
!                              (NOTE: THIS VALUE IS PRESENT ONLY IN
!                                     BUFR EDITION 4 MESSAGES!)
!
!   OUTPUT ARGUMENT LIST:
!     IUPBS01  - INTEGER: UNPACKED INTEGER VALUE
!                  -1 = THE INPUT S01MNEM MNEMONIC WAS INVALID FOR
!                       THE EDITION OF BUFR MESSAGE IN MBAY
!
! REMARKS:
!    THIS ROUTINE CALLS:        GETS1LOC I4DY     IUPB     WRDLEN
!    THIS ROUTINE IS CALLED BY: CKTABA   CNVED4   COPYMG   CPYMEM
!                               CRBMG    DATEBF   DUMPBF   GETLENS
!                               IGETDATE IUPVS01  MESGBC   MESGBF
!                               MSGWRT   NMWRD    PADMSG   PKBS1
!                               RDBFDX   RDMSGB   STNDRD   UFBPOS
!                               WRCMPS
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

	DIMENSION	MBAY(*)

	CHARACTER*(*)	S01MNEM

	LOGICAL		OK4CENT

!-----------------------------------------------------------------------
!	This statement function checks whether its input value contains
!       a valid century value.

	OK4CENT(IVAL) = ((IVAL.GE.19).AND.(IVAL.LE.21))
!-----------------------------------------------------------------------

!	Call subroutine WRDLEN to initialize some important information
!	about the local machine, just in case subroutine OPENBF hasn't
!	been called yet.

	CALL WRDLEN

!	Handle some simple requests that do not depend on the BUFR
!       edition number.

	IF(S01MNEM.EQ.'LENM') THEN
	    IUPBS01 = IUPB(MBAY,5,24)
	    RETURN
	ENDIF

	LEN0 = 8
	IF(S01MNEM.EQ.'LEN0') THEN
	    IUPBS01 = LEN0
	    RETURN
	ENDIF

!	Get the BUFR edition number.

	IBEN = IUPB(MBAY,8,8)
	IF(S01MNEM.EQ.'BEN') THEN
	    IUPBS01 = IBEN
	    RETURN
	ENDIF

!	Use the BUFR edition number to handle any other requests.

	CALL GETS1LOC(S01MNEM,IBEN,ISBYT,IWID,IRET)
	IF(IRET.EQ.0) THEN
	    IUPBS01 = IUPB(MBAY,LEN0+ISBYT,IWID)
	    IF(S01MNEM.EQ.'CENT') THEN

!		Test whether the returned value was a valid
!		century value.

		IF(.NOT.OK4CENT(IUPBS01)) IUPBS01 = -1
            ENDIF
        ELSE IF( (S01MNEM.EQ.'YEAR') .AND. (IBEN.LT.4) ) THEN

!	    Calculate the 4-digit year.

	    IYOC = IUPB(MBAY,21,8)
	    ICEN = IUPB(MBAY,26,8)

!	    Does ICEN contain a valid century value?

	    IF(OK4CENT(ICEN)) THEN

!               YES, so use it to calculate the 4-digit year. Note that,
!               by international convention, the year 2000 was the 100th
!               year of the 20th century, and the year 2001 was the 1st
!               year of the 21st century

		IUPBS01 = (ICEN-1)*100 + IYOC
	    ELSE

!               NO, so use a windowing technique to determine the
!               4-digit year from the year of the century.

		IUPBS01 = I4DY(MOD(IYOC,100)*1000000)/10**6
	    ENDIF
	ELSE
	    IUPBS01 = -1
	ENDIF

	RETURN
	END FUNCTION IUPBS01

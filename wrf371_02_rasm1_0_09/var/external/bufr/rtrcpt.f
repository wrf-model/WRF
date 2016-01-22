      SUBROUTINE RTRCPT(LUNIT,IYR,IMO,IDY,IHR,IMI,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RTRCPT
C   PRGMMR: ATOR            ORG: NP12       DATE: 2009-03-23
C
C ABSTRACT: THIS SUBROUTINE RETURNS THE TANK RECEIPT TIME STORED WITHIN
C   SECTION 1 OF THE BUFR MESSAGE OPEN FOR INPUT VIA A PREVIOUS CALL TO
C   BUFR ARCHIVE LIBRARY SUBROUTINE READMG, READMM OR EQUIVALENT.
C
C PROGRAM HISTORY LOG:
C 2009-03-23  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    CALL RTRCPT (LUNIT,IYR,IMO,IDY,IHR,IMI,IRET) 
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     IYR      - INTEGER: TANK RECEIPT YEAR
C     IMO      - INTEGER: TANK RECEIPT MONTH
C     IDY      - INTEGER: TANK RECEIPT DAY
C     IHR      - INTEGER: TANK RECEIPT HOUR
C     IMI      - INTEGER: TANK RECEIPT MINUTE
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = no tank receipt time was present within the
C                           BUFR message currently open for input
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     IUPB     IUPBS01   STATUS
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .                MBAY(MXMSGLD4,NFILES)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = -1

C     Check the file status.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C     Check whether the message contains a tank receipt time.

      IF(IUPBS01(MBAY(1,LUN),'BEN').EQ.4) THEN
	IS1BYT = 23
      ELSE
	IS1BYT = 19
      ENDIF
      IF( (IS1BYT+5) .GT. IUPBS01(MBAY(1,LUN),'LEN1') ) RETURN

C     Unpack the tank receipt time.

C     Note that IS1BYT is a starting byte number relative to the
C     beginning of Section 1, so we still need to account for
C     Section 0 when specifying the actual byte numbers to unpack
C     within the overall message.

      IMGBYT = IS1BYT + IUPBS01(MBAY(1,LUN),'LEN0')

      IYR = IUPB(MBAY(1,LUN),IMGBYT,16)
      IMO = IUPB(MBAY(1,LUN),IMGBYT+2,8)
      IDY = IUPB(MBAY(1,LUN),IMGBYT+3,8)
      IHR = IUPB(MBAY(1,LUN),IMGBYT+4,8)
      IMI = IUPB(MBAY(1,LUN),IMGBYT+5,8)

      IRET = 0
	
C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS CLOSED; IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RTRCPT - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT; IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: RTRCPT - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE; NONE ARE')
      END

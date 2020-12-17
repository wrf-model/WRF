      INTEGER FUNCTION IOK2CPY(LUI,LUO)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IOK2CPY
C   PRGMMR: J. ATOR          ORG: NP20       DATE: 2009-06-26
C
C ABSTRACT: THIS FUNCTION DETERMINES WHETHER A MESSAGE, OR A SUBSET
C   FROM A MESSAGE, CAN BE COPIED FROM LOGICAL UNIT IOLUN(LUI) TO
C   LOGICAL UNIT IOLUN(LUO).  THE DECISION IS BASED ON WHETHER THE
C   EXACT SAME DEFINITION FOR THE GIVEN MESSAGE TYPE APPEARS WITHIN
C   THE DICTIONARY TABLE INFORMATION FOR BOTH LOGICAL UNITS.  NOTE THAT
C   IT IS POSSIBLE FOR A MESSAGE TYPE TO BE IDENTICALLY DEFINED FOR TWO
C   DIFFERENT LOGICAL UNITS EVEN IF THE UNITS THEMSELVES DON'T SHARE
C   THE EXACT SAME FULL SET OF DICTIONARY TABLES.
C
C PROGRAM HISTORY LOG:
C 2009-06-26  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    IOK2CPY (LUI, LUO)
C   INPUT ARGUMENT LIST:
C     LUI      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR LOGICAL UNIT TO COPY FROM
C     LUO      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR LOGICAL UNIT TO COPY TO
C
C   OUTPUT ARGUMENT LIST:
C     IOK2CPY  - INTEGER: RETURN CODE INDICATING WHETHER IT IS OKAY TO
C                COPY FROM IOLUN(LUI) TO IOLUN(LUO)
C                  0 - NO
C                  1 - YES
C
C REMARKS:
C    THIS ROUTINE CALLS:        ICMPDX   NEMTBAX
C    THIS ROUTINE IS CALLED BY: COPYSB   COPYMG   CPYMEM   UFBCPY
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /BTABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)

      CHARACTER*10 TAG
      CHARACTER*8  SUBSET
      CHARACTER*3  TYP

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IOK2CPY = 0

C     Do both logical units have the same internal table information?

      IF ( ICMPDX(LUI,LUO) .EQ. 1 ) THEN
	IOK2CPY = 1
	RETURN
      ENDIF
 
C     No, so get the Table A mnemonic from the message to be copied,
C     then check whether that mnemonic is defined within the dictionary
C     tables for the logical unit to be copied to.

      SUBSET = TAG(INODE(LUI))
      CALL NEMTBAX(LUO,SUBSET,MTYP,MSBT,INOD)
      IF ( INOD .EQ. 0 ) RETURN

C     The Table A mnemonic is defined within the dictionary tables for
C     both units, so now make sure the definitions are identical.

      NTEI = ISC(INODE(LUI))-INODE(LUI)
      NTEO = ISC(INOD)-INOD
      IF ( NTEI .NE. NTEO ) RETURN

      DO I = 1, NTEI
        IF ( TAG(INODE(LUI)+I) .NE. TAG(INOD+I) ) RETURN
        IF ( TYP(INODE(LUI)+I) .NE. TYP(INOD+I) ) RETURN
        IF ( ISC(INODE(LUI)+I) .NE. ISC(INOD+I) ) RETURN
        IF ( IRF(INODE(LUI)+I) .NE. IRF(INOD+I) ) RETURN
        IF ( IBT(INODE(LUI)+I) .NE. IBT(INOD+I) ) RETURN
      ENDDO

      IOK2CPY = 1

      RETURN
      END

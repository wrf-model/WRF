C-----------------------------------------------------------------------
      SUBROUTINE ERRMSG(CMSG)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: ERRMSG         WRITE A MESSAGE TO STDERR
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 95-10-31
C
C ABSTRACT: WRITE A MESSAGE TO STDERR.
C
C PROGRAM HISTORY LOG:
C   95-10-31  IREDELL
C
C USAGE:    CALL ERRMSG(CMSG)
C   INPUT ARGUMENTS:
C     CMSG         CHARACTER*(*) MESSAGE TO WRITE
C
C REMARKS: THIS IS A MACHINE-DEPENDENT SUBPROGRAM.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY
C
C$$$
      CHARACTER*(*) CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(0,'(A)') CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END

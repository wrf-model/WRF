!-----------------------------------------------------------------------
      subroutine w3doxdat(idat,jdow,jdoy,jday)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3DOXDAT       RETURN WEEK DAY, YEAR DAY, AND JULIAN DAY
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE INTEGER DAY OF WEEK, THE DAY
!   OF YEAR, AND JULIAN DAY GIVEN AN NCEP ABSOLUTE DATE AND TIME.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3DOXDAT(IDAT,JDOW,JDOY,JDAY)
!
!   INPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDOW       INTEGER DAY OF WEEK (1-7, WHERE 1 IS SUNDAY)
!     JDOY       INTEGER DAY OF YEAR (1-366, WHERE 1 IS JANUARY 1)
!     JDAY       INTEGER JULIAN DAY (DAY NUMBER FROM JAN. 1,4713 B.C.)
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get julian day and then get day of week and day of year
      jday=iw3jdn(idat(1),idat(2),idat(3))
      call w3fs26(jday,jy,jm,jd,jdow,jdoy)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

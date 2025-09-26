!-----------------------------------------------------------------------
      subroutine w3difdat(jdat,idat,it,rinc)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3DIFDAT       RETURN A TIME INTERVAL BETWEEN TWO DATES
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE ELAPSED TIME INTERVAL FROM
!   AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE SECOND ARGUMENT UNTIL
!   AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE FIRST ARGUMENT.
!   THE OUTPUT TIME INTERVAL IS IN ONE OF SEVEN CANONICAL FORMS
!   OF THE NCEP RELATIVE TIME INTERVAL DATA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3DIFDAT(JDAT,IDAT,IT,RINC)
!
!   INPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!     IT         INTEGER RELATIVE TIME INTERVAL FORMAT TYPE
!                (-1 FOR FIRST REDUCED TYPE (HOURS ALWAYS POSITIVE),
!                 0 FOR SECOND REDUCED TYPE (HOURS CAN BE NEGATIVE),
!                 1 FOR DAYS ONLY, 2 FOR HOURS ONLY, 3 FOR MINUTES ONLY,
!                 4 FOR SECONDS ONLY, 5 FOR MILLISECONDS ONLY)
!
!   OUTPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!                (TIME INTERVAL IS POSITIVE IF JDAT IS LATER THAN IDAT.)
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer jdat(8),idat(8)
      real rinc(5)
      real rinc1(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  difference the days and time and put into canonical form
      rinc1(1)=iw3jdn(jdat(1),jdat(2),jdat(3))-
     &         iw3jdn(idat(1),idat(2),idat(3))
      rinc1(2:5)=jdat(5:8)-idat(5:8)
      call w3reddat(it,rinc1,rinc)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

!-----------------------------------------------------------------------
      subroutine w3movdat(rinc,idat,jdat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3MOVDAT       RETURN A DATE FROM A TIME INTERVAL AND DATE
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE DATE AND TIME THAT IS A GIVEN
!   NCEP RELATIVE TIME INTERVAL FROM AN NCEP ABSOLUTE DATE AND TIME.
!   THE OUTPUT IS IN THE NCEP ABSOLUTE DATE AND TIME DATA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3MOVDAT(RINC,IDAT,JDAT)
!
!   INPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!                (JDAT IS LATER THAN IDAT IF TIME INTERVAL IS POSITIVE.)
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      real rinc(5)
      integer idat(8),jdat(8)
      real rinc1(5),rinc2(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  add the interval to the input time of day and put into reduced form
!  and then compute new date using julian day arithmetic.
      rinc1(1)=rinc(1)
      rinc1(2:5)=rinc(2:5)+idat(5:8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
      jdat(4)=idat(4)
      jdat(5:8)=nint(rinc2(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

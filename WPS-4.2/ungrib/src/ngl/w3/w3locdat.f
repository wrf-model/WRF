!-----------------------------------------------------------------------
      subroutine w3locdat(idat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3LOCDAT       RETURN THE LOCAL DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE LOCAL DATE AND TIME
!   IN THE NCEP ABSOLUTE DATE AND TIME DATA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
! 1999-04-28  Gilbert         - added a patch to check for the proper
!                               UTC offset.  Needed until the IBM bug
!                               in date_and_time is fixed.  The patch
!                               can then be removed.  See comments in
!                               the section blocked with "&&&&&&&&&&&".
! 1999-08-12  Gilbert         - Changed so that czone variable is saved
!                               and the system call is only done for
!                               first invocation of this routine.
!
! USAGE:  CALL W3LOCDAT(IDAT)
!
!   OUTPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
! SUBPROGRAMS CALLED:
!     DATE_AND_TIME  FORTRAN 90 SYSTEM DATE INTRINSIC
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

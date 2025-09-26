!-----------------------------------------------------------------------
      subroutine w3utcdat(idat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3UTCDAT       RETURN THE UTC DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE UTC (GREENWICH) DATE AND TIME
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
! USAGE:  CALL W3UTCDAT(IDAT)
!
!   OUTPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
! SUBPROGRAMS CALLED:
!     DATE_AND_TIME  FORTRAN 90 SYSTEM DATE INTRINSIC
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
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
!  convert to hours and minutes to UTC time
!  and possibly adjust the date as well
      idat(6)=idat(6)-mod(idat(4),100)
      idat(5)=idat(5)-idat(4)/100
      idat(4)=0
      if(idat(6).lt.00) then
        idat(6)=idat(6)+60
        idat(5)=idat(5)-1
      elseif(idat(6).ge.60) then
        idat(6)=idat(6)-60
        idat(5)=idat(5)+1
      endif
      if(idat(5).lt.00) then
        idat(5)=idat(5)+24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))-1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      elseif(idat(5).ge.24) then
        idat(5)=idat(5)-24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))+1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

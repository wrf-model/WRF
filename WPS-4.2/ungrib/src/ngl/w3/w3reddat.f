      subroutine w3reddat(it,rinc,dinc)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM REDUCES AN NCEP RELATIVE TIME INTERVAL
!   INTO ONE OF SEVEN CANONICAL FORMS, DEPENDING ON THE INPUT IT VALUE.
!
!   First reduced format type (IT=-1):
!        RINC(1) is an arbitrary integer.
!        RINC(2) is an integer between 00 and 23, inclusive.
!        RINC(3) is an integer between 00 and 59, inclusive.
!        RINC(4) is an integer between 00 and 59, inclusive.
!        RINC(5) is an integer between 000 and 999, inclusive.
!      If RINC(1) is negative, then the time interval is negative.
!    
!   Second reduced format type (IT=0):
!      If the time interval is not negative, then the format is:
!        RINC(1) is zero or a positive integer. 
!        RINC(2) is an integer between 00 and 23, inclusive.
!        RINC(3) is an integer between 00 and 59, inclusive.
!        RINC(4) is an integer between 00 and 59, inclusive.
!        RINC(5) is an integer between 000 and 999, inclusive.
!      Otherwise if the time interval is negative, then the format is:
!        RINC(1) is zero or a negative integer. 
!        RINC(2) is an integer between 00 and -23, inclusive.
!        RINC(3) is an integer between 00 and -59, inclusive.
!        RINC(4) is an integer between 00 and -59, inclusive.
!        RINC(5) is an integer between 000 and -999, inclusive.
!    
!   Days format type (IT=1):
!        RINC(1) is arbitrary.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is zero.
!    
!   Hours format type (IT=2):
!        RINC(1) is zero.
!        RINC(2) is arbitrary.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than 300 years.)
!    
!   Minutes format type (IT=3):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is arbitrary.
!        RINC(4) is zero.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than five years.)
!    
!   Seconds format type (IT=4):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is arbitrary.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than one month.)
!    
!   Milliseconds format type (IT=5):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is arbitrary.
!     (This format should not express time intervals longer than one hour.)
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3REDDAT(IT,RINC,DINC)
!
!   INPUT VARIABLES:
!     IT         INTEGER RELATIVE TIME INTERVAL FORMAT TYPE
!                (-1 FOR FIRST REDUCED TYPE (HOURS ALWAYS POSITIVE),
!                 0 FOR SECOND REDUCED TYPE (HOURS CAN BE NEGATIVE),
!                 1 FOR DAYS ONLY, 2 FOR HOURS ONLY, 3 FOR MINUTES ONLY,
!                 4 FOR SECONDS ONLY, 5 FOR MILLISECONDS ONLY)
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!
!   OUTPUT VARIABLES:
!     DINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!
! SUBPROGRAMS CALLED:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      real rinc(5),dinc(5)
!  parameters for number of units in a day
!  and number of milliseconds in a unit
!  and number of next smaller units in a unit, respectively
      integer,dimension(5),parameter:: itd=(/1,24,1440,86400,86400000/),
     &                                 itm=itd(5)/itd
      integer,dimension(4),parameter:: itn=itd(2:5)/itd(1:4)
      integer,parameter:: np=16
      integer iinc(4),jinc(5),kinc(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  first reduce to the first reduced form
      iinc=floor(rinc(1:4))
!  convert all positive fractional parts to milliseconds
!  and determine canonical milliseconds
      jinc(5)=nint(dot_product(rinc(1:4)-iinc,real(itm(1:4)))+rinc(5))
      kinc(5)=modulo(jinc(5),itn(4))
!  convert remainder to seconds and determine canonical seconds
      jinc(4)=iinc(4)+(jinc(5)-kinc(5))/itn(4)
      kinc(4)=modulo(jinc(4),itn(3))
!  convert remainder to minutes and determine canonical minutes
      jinc(3)=iinc(3)+(jinc(4)-kinc(4))/itn(3)
      kinc(3)=modulo(jinc(3),itn(2))
!  convert remainder to hours and determine canonical hours
      jinc(2)=iinc(2)+(jinc(3)-kinc(3))/itn(2)
      kinc(2)=modulo(jinc(2),itn(1))
!  convert remainder to days and compute milliseconds of the day
      kinc(1)=iinc(1)+(jinc(2)-kinc(2))/itn(1)
      ms=dot_product(kinc(2:5),itm(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  next reduce to either single value canonical form
!  or to one of the two reduced forms
      if(it.ge.1.and.it.le.5) then
!  ensure that exact multiples of 1./np are expressed exactly
!  (other fractions may have precision errors)
        rp=(np*ms)/itm(it)+mod(np*ms,itm(it))/real(itm(it))
        dinc=0
        dinc(it)=real(kinc(1))*itd(it)+rp/np
      else
!  the reduced form is done except the second reduced form is modified
!  for negative time intervals with fractional days
        dinc=kinc
        if(it.eq.0.and.kinc(1).lt.0.and.ms.gt.0) then
          dinc(1)=dinc(1)+1
          dinc(2:5)=mod(ms-itm(1),itm(1:4))/itm(2:5)
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end

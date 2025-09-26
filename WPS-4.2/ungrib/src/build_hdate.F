      subroutine build_hdate(hdate, iyr, imo, idy, ihr, imi, isc)

! PURPOSE: 
!      From the Year, Month, Day, Hour, Minute, and Second values, 
!      creates a 19-character string representing the date, in the 
!      format:  "YYYY-MM-DD hh:mm:ss"

! INPUT:
      integer iyr     ! year (e.g., 1997, 2001)
      integer imo     ! month (01 - 12)
      integer idy     ! day of the month (01 - 31)
      integer ihr     ! hour (00-23)
      integer imi     ! minute (00-59)
      integer isc     ! second (00-59)
! OUTPUT:
      character*(*) hdate ! 'YYYY-MM-DD hh:mm:ss'

! LOCAL:
      integer i  ! Loop counter.
      integer hlen ! Length of hdate string

      hlen = len(hdate)

      if (hlen.eq.19) then
         write(hdate,19) iyr, imo, idy, ihr, imi, isc
 19      format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

      elseif (hlen.eq.16) then
         write(hdate,16) iyr, imo, idy, ihr, imi
 16      format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

      elseif (hlen.eq.13) then
         write(hdate,13) iyr, imo, idy, ihr
 13      format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

      elseif (hlen.eq.10) then
         write(hdate,10) iyr, imo, idy
 10      format(i4,'-',i2.2,'-',i2.2)
      endif

      return
      end

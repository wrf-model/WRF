subroutine da_mspps_ts(tb, nchan, satzen, ts)
 
! http://www.star.nesdis.noaa.gov/corp/scsb/mspps/algorithms.html
! land algorithm for surface temperature
 
   use gsi_constants, only: deg2rad

   implicit none

   integer,                intent(in)  :: nchan
   real, dimension(nchan), intent(in)  :: tb
   real,                   intent(in)  :: satzen
   real,                   intent(out) :: ts

   real, parameter :: rmiss = -999.0
   real, parameter :: tbmin = 50.0
   real, parameter :: tbmax = 550.0

   real :: cza

   ts = rmiss  ! initialize

   if ( tb(1) > tbmin .and. tb(1) < tbmax .and.   &
        tb(2) > tbmin .and. tb(2) < tbmax .and.   &
        tb(3) > tbmin .and. tb(3) < tbmax .and.   &
        satzen >= 0.0 .and. satzen <= 90.0 ) then
      cza = COS(satzen*deg2rad)
      ts = 2.9079E2-(8.5059E-1-1.9821E-3*tb(1))*tb(1)+         &
           (6.1433E-1-2.3579E-3*tb(2))*tb(2)-                  &
           (1.1493-5.4709E-3*tb(3))*tb(3)-1.50E1*(cza-5.40E-1)
   end if

end subroutine da_mspps_ts


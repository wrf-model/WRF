subroutine da_mspps_emis(tb, nchan, em)

! http://www.star.nesdis.noaa.gov/corp/scsb/mspps/algorithms.html
! land algorithms for emissivities at three AMSU channels (23.8, 31.4, 50.3 GHz)

   implicit none

   integer,                intent(in)  :: nchan
   real, dimension(nchan), intent(in)  :: tb
   real, dimension(nchan), intent(out) :: em

   real, dimension(3), parameter :: b0 = (/ -2.5404E-1,-2.2606E-1, 8.9494E-2 /)
   real, dimension(3), parameter :: b1 = (/  1.1326E-2, 3.4481E-3,-3.6615E-3 /)
   real, dimension(3), parameter :: b2 = (/ -1.9479E-5,-9.7185E-6,-4.2390E-7 /)
   real, dimension(3), parameter :: b3 = (/ -4.5763E-3, 4.3299E-3, 1.0636E-2 /)
   real, dimension(3), parameter :: b4 = (/  1.7833E-5, 5.3281E-6,-6.4559E-6 /)
   real, dimension(3), parameter :: b5 = (/  3.2324E-3, 1.8668E-3,-4.2449E-4 /)
   real, dimension(3), parameter :: b6 = (/ -1.9056E-5,-1.5369E-5,-6.6878E-6 /)
   real, parameter :: f1  = 23.8
   real, parameter :: f2  = 31.4
   real, parameter :: f3  = 50.3
   real, parameter :: f4  = 52.8
   real, parameter :: f5  = 53.596
   real, parameter :: f15 = 89.0
   real, parameter :: rmiss =   0.0
   real, parameter :: tbmin =  50.0
   real, parameter :: tbmax = 550.0

   integer :: k

   em = rmiss  ! initialize

   if ( tb(1) > tbmin .and. tb(1) < tbmax .and.  &
        tb(2) > tbmin .and. tb(2) < tbmax .and.  &
        tb(3) > tbmin .and. tb(3) < tbmax ) then
      do k = 1, 2
      ! do k = 1, 3
         em(k) = b0(k)+b1(k)*tb(1)+b2(k)*tb(1)**2+b3(k)*tb(2)+b4(k)*tb(2)**2 &
                +b5(k)*tb(3)+b6(k)*tb(3)**2
      end do
      em(3)=em(2)+(f3-f2)*(em(2)-em(1))/(f2-f1)   !linear interpolated
      em(4)=em(3)+(f4-f3)*(em(3)-em(2))/(f3-f2)   !linear interpolated
      em(5)=em(4)+(f5-f4)*(em(4)-em(3))/(f4-f3)
      em(15)=em(4)+(f15-f4)*(em(4)-em(3))/(f4-f3)
   end if

end subroutine da_mspps_emis


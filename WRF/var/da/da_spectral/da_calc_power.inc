subroutine da_calc_power(max_wavenumber, r_cvsize, rcv, power)

   !----------------------------------------------------------------------
   ! Purpose: Performs spectral to gridpoint transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wavenumber ! Smallest scale required (ni/2 - 1)
   integer, intent(in) :: r_cvsize               ! Size of rcv vector.
   real, intent(in)    :: rcv(1:r_cvsize)        ! Spectral modes.
   real, intent(out)   :: power(0:max_wavenumber)! Power spectrum.

   integer             :: n, m                   ! Loop counters.
   integer             :: index, indexx          ! Position markers in cv.
   integer             :: cv_size                ! Complex cv size.
   complex, allocatable:: ccv(:)                 ! Complex control variable.

   ! Create complex array:
   cv_size = r_cvsize / 2
   allocate(ccv(1:cv_size))
   do index = 1, cv_size
      indexx = 2 * index - 1
      ccv(index) = cmplx(rcv(indexx), rcv(indexx+1))
   end do

   power(:) = 0.0

   ! Calculate power spectrum from input 1D spectral mode vector:

   do n = 0, max_wavenumber

      ! First consider m=0:
      m = 0
      index = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 + n - m
      power(n) = real(ccv(index))**2

      ! Now add m>0 terms:
      do m = 1, n
         index = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 + n - m
         power(n) = power(n) + 2.0 *(real(ccv(index))**2 + aimag(ccv(index))**2)
      end do

   end do

   deallocate(ccv)
      
end subroutine da_calc_power



subroutine da_apply_power (power, max_wavenumber, ccv, c_cvsize) 

   !-------------------------------------------------------------------------
   ! Purpose: Adjust power spectrum for the control variable
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)   :: max_wavenumber ! Smallest scale required (ni/2 - 1).
   integer, intent(in)   :: c_cvsize                ! Size of complex cv-array
   real*8,  intent(in)   :: power(0:max_wavenumber) ! Power Spectrum
   complex, intent(inout):: ccv(1:c_cvsize)         ! complex control  array   
  
   integer             :: m, n, index

   ! Adjust Power spectrum

   do n =0, max_wavenumber
      
      ccv(n+1) = ccv(n+1) * sqrt (power(n)/(2*n+1))

      do m=1, n
         index = m*(max_wavenumber+1- m) + m*(m+1)/2+n-m + 1
         ccv(index) = ccv(index) * sqrt (power(n)/(2*n+1))
      end do
   end do

end subroutine da_apply_power           



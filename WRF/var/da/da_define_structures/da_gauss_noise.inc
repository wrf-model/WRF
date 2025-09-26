subroutine da_gauss_noise( z)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
      
   implicit none
   
   real,intent(out)     :: z
   real                 :: x, y, r, coeff

   if (trace_use) call da_trace_entry("da_gauss_noise")

   ! [2.1] Get two uniform variate random numbers in range 0 to 1:

   do
      call random_number( x)
      call random_number( y)

      ! [2.2] Transform to range -1 to 1 and calculate sum of squares:

      x = 2.0 * x - 1.0
      y = 2.0 * y - 1.0
      r = x * x + y * y
      
      if (r > 0.0 .and. r < 1.0) exit        
   end do

   ! [2.3] use Box-Muller transformation to get normal deviates:

   coeff = sqrt( -2.0 * log(r) / r)         
   z = coeff * x

   if (trace_use) call da_trace_exit("da_gauss_noise")
      
end subroutine da_gauss_noise



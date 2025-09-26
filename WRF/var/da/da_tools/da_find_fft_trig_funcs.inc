subroutine da_find_fft_trig_funcs(n, trig_functs)

   !---------------------------------------------------------------------------
   ! Purpose: Set up constants required for Fourier, sine and cosine transforms
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n
   real,    intent(out) :: trig_functs(:)

   integer :: k, nil, nhl
   real    :: del, angle

   ! in da_control
   ! if (trace_use) call da_trace_entry("da_find_fft_trig_funcs")

   !---------------------------------------------------------------------------
   ! [1.0] Trig functions for real periodic transform:
   !---------------------------------------------------------------------------

   trig_functs(:) = 0.0

   del=4.0*(pi/2.0)/float(n)
   nil=0
   nhl=(n/2)-1

   do k=nil,nhl
      angle=float(k)*del
      trig_functs(2*k+1)=cos(angle)
      trig_functs(2*k+2)=sin(angle)
   end do

   ! [1.1] extra trig functions for cosine transform:

   del=0.5*del
   do k=1,nhl
      angle=float(k)*del
      trig_functs(2*n+k)=sin(angle)
   end do
  
   ! [1.2] extra trig functions for shifted cosine transform:

   del=0.5*del
   do k=1,n
      angle=float(k)*del
      trig_functs(n+k)=sin(angle)
   end do

   !if (trace_use) call da_trace_exit("da_find_fft_trig_funcs")

end subroutine da_find_fft_trig_funcs



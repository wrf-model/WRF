subroutine da_find_fft_factors(n, n_ok, fft_factors)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates prime factors of input number.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n
   logical, intent(out) :: n_ok
   integer, intent(out) :: fft_factors(:)

   integer :: i, k, l
   integer :: nfax, nu, ifac
   integer :: jfax(num_fft_factors)
   integer :: lfax(7)

   data lfax /6,8,5,4,3,2,1/

   ! in da_control
   !if (trace_use) call da_trace_entry("da_find_fft_factors")

   !---------------------------------------------------------------------------
   ! [1.0] Find factors of vector size (8,6,5,4,3,2; only one 8 allowed):
   !---------------------------------------------------------------------------

   n_ok = .false.
   fft_factors(:) = 0

   ! look for sixes first, store factors in descending order
   nu=n
   ifac=6
   k=0
   l=1

20 continue

   if (mod(nu,ifac).ne.0) goto 30
   
   ! 6 is a factor:
   k=k+1
   jfax(k)=ifac
   if (ifac.ne.8) goto 25
   if (k.eq.1) goto 25
   jfax(1)=8
   jfax(k)=6

25 continue
   nu=nu/ifac
   if (nu.eq.1) goto 50
   if (ifac.ne.8) goto 20

30 continue
   l=l+1
   ifac=lfax(l)
   if (ifac .gt. 1) goto 20

   ! illegal factors:
   ! write (unit=message(1),fmt='(a,i4,a)') 'n = ', n, ' contains illegal factors.'
   ! call da_warning(__file__,__line__,message(1:1))
   
   goto 9

   ! now reverse order of factors
50 continue
   nfax=k
   fft_factors(1)=nfax
   do i=1,nfax
      fft_factors(nfax+2-i)=jfax(i)
   end do
   
   n_ok = .true.
      
9  continue

   !if (trace_use) call da_trace_exit("da_find_fft_factors")

end subroutine da_find_fft_factors



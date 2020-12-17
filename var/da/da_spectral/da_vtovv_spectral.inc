subroutine da_vtovv_spectral(max_wave, sizec, lenr, lenwrk, lensav, inc, &
                                alp_size, alp, wsave, power, rcv, field)

   !-------------------------------------------------------------------------
   ! Purpose: Performs spectral to gridpoint transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wave                 ! Max total wavenumber.
   integer, intent(in) :: sizec                    ! Size of packed spectral array.
   integer, intent(in) :: lenr                     ! FFT info.
   integer, intent(in) :: lenwrk                   ! FFT info.
   integer, intent(in) :: lensav                   ! FFT info.
   integer, intent(in) :: inc                      ! FFT info.
   integer, intent(in) :: alp_size                 ! Size of alp array.
   real, intent(in)    :: alp(1:alp_size)          ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)          ! Primes for FFT.
   real*8, intent(in)  :: power(0:max_wave)        ! Power spectrum 
   real, intent(in)    :: rcv(1:2*sizec)           ! Spectral modes.
   real, intent(out)   :: field(its:ite,jts:jte)   ! Gridpoint field.

   integer :: j, l,m, n                ! Loop counters.
   integer :: index_start              ! Position markers in cv.
   integer :: index_r, index_c         ! Array index for complex v_fou.

   real    :: r_fou(1:lenr)            ! FFT array.
   complex :: v_fou(its:ite,0:max_wave)! Intermediate Fourier state.
   complex :: ccv(1:sizec)             ! Spectral modes.  
   integer              :: index_m, index_j
   integer              :: jc, js, je, iequator
   complex              :: sum_legtra          ! Summation scalars.

#ifdef FFTPACK
   real    :: work(1:lenwrk)           ! FFT work array. 
#endif


   if (trace_use) call da_trace_entry("da_vtovv_spectral")

   !----------------------------------------------------------------------------
   ! [1] Create complex array from read array:
   !----------------------------------------------------------------------------

   v_fou = 0.0
   do n = 1, sizec
      ccv(n)  = CMPLX(rcv(2*n-1), rcv(2*n))
   end do

   !----------------------------------------------------------------------------
   ! [2]  Apply power spectrum
   !----------------------------------------------------------------------------

   if (.not. test_transforms) call da_apply_power(power, max_wave, ccv, sizec)

   !----------------------------------------------------------------------------
   ! [3] Perform inverse Legendre decomposition in N-S direction:
   !----------------------------------------------------------------------------

   do m = 0, max_wave
      index_start = m * (max_wave + 1 - m) + m * (m + 1) / 2 + 1

      index_m = m * (max_wave + 1 - m) + m * (m + 1) / 2 + 1 - m

      jc = (jde-jds+1)/2

      iequator = mod(jde-jds+1, 2)

      je = min(jc+iequator, jte)

      do j = jts, je
         index_j = (j - 1) * (max_wave + 1) * (max_wave + 2) / 2

         v_fou(j,m) = sum(ccv(index_start:index_start-m+max_wave) * &
            alp(index_j+index_m+m:index_j+index_m+max_wave))
      end do

      js = max(jts, jc+iequator+1)

      do j = js, jte
         index_j = (jds+jde - j - 1) * (max_wave + 1) * (max_wave + 2) / 2

         sum_legtra = da_zero_complex
         do l = m, max_wave
            ! Calculate second quadrant values:
            if(mod(l+m,2) == 1) then
               sum_legtra = sum_legtra - ccv(index_start-m+l) * alp(index_j + index_m + l)
            else
               sum_legtra = sum_legtra + ccv(index_start-m+l) * alp(index_j + index_m + l)
            end if
         end do
         v_fou(j,m) = sum_legtra
      end do
   end do

   !----------------------------------------------------------------------------
   ! [4] Perform inverse Fourier decomposition in E-W direction:
   !----------------------------------------------------------------------------

   do j = jts, jte
      r_fou(its) =  real(v_fou(j,0))  ! R(m=0) is real.
      ! r_fou(ite) = aimag(v_fou(j,0)) ! R(m=NI/2) is real, but packed in imag m = 0)
      ! make r_fou(ide) zero as there is no power computed corresponding to this wavenumber
      r_fou(ite) = 0.0      

      do m = 1, max_wave
         index_r = 2 * m
         index_c = 2 * m + 1
         r_fou(index_r) = real(v_fou(j,m))
         r_fou(index_c) = aimag(v_fou(j,m))
      end do
 
#ifdef FFTPACK
      call rfft1b(ide, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ierr)
#else
      call da_error(__FILE__,__LINE__,(/"Must compile with FFTPACK"/))
#endif
      field(its:ite,j) = r_fou(its:ite)
   end do

   if (trace_use) call da_trace_exit("da_vtovv_spectral")
   
end subroutine da_vtovv_spectral



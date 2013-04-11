subroutine da_vtovv_spectral_adj(max_wavenumber, sizec, lenr, lenwrk, lensav, inc, &
                                    alp_size, alp, wsave, power, rcv, field)

   !----------------------------------------------------------------------
   ! Purpose: Performs Adjoint of spectral to grid transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wavenumber             ! Max total wavenumber.
   integer, intent(in) :: sizec                  ! Size of packed spectral array.
   integer, intent(in) :: lenr                       ! FFT info.
   integer, intent(in) :: lenwrk                     ! FFT info.
   integer, intent(in) :: lensav                     ! FFT info.
   integer, intent(in) :: inc                        ! FFT info.
   integer, intent(in) :: alp_size                   ! Size of alp array.
   real, intent(in)    :: alp(1:alp_size)            ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)            ! Primes for FFT.
   real*8, intent(in)    :: power(0:max_wavenumber)    ! Power spectrum
   real, intent(out)   :: rcv(1:2*sizec)             ! Spectral modes.
   real, intent(in)    :: field(its:ite,jts:jte)     ! Gridpoint field.

   integer             :: j, m, n                    ! Loop counters.
   integer             :: index_start                ! Position markers in cv.
#ifdef DM_PARALLEL
   integer             :: index_end                  ! Position markers in cv.
#endif
   integer             :: index_r, index_c           ! Array index for complex v_fou.

   real                :: r_fou(1:lenr)              ! FFT array.
   complex             :: v_fou(its:ite,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: ccv(1:sizec)               ! Spectral modes.
   complex             :: ccv_local(1:sizec)         ! Spectral modes.

   integer              :: l, js, je           ! Loop counters.
   integer              :: index_m, index_j    ! Markers.
   complex              :: sum_legtra          ! Summation scalars.

   integer              :: jc, iequator, temp

#ifdef FFTPACK
   real                :: work(1:lenwrk)             ! FFT work array.
#endif

   if (trace_use) call da_trace_entry("da_vtovv_spectral_adj")

   !----------------------------------------------------------------------
   ! [1] Perform Adjoint of inverse Fourier decomposition in E-W direction:
   !----------------------------------------------------------------------

   v_fou = 0.0
   do j = jts, jte
      r_fou(its:ite) = field(its:ite,j) 
#ifdef FFTPACK
      call rfft1f(ide, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ierr)
#else
      call da_error(__FILE__,__LINE__,(/"Must compile with FFTPACK"/))
#endif

      !----------------------------------------------------------------------
      ! Adjust the output for adjoint test
      !----------------------------------------------------------------------
      r_fou      =  real(ite)/2.0 * r_fou
      r_fou(its) =  r_fou(its)   * 2.0       

      ! if(.not. odd_longitudes) r_fou(ite) = 2.0*r_fou(ite)   
      ! make r_fou(ide) zero as there is no power computed for this wavenumber
      r_fou(ite) = 0.0

      v_fou(j,0) = CMPLX(r_fou(its), r_fou(ite))

      do m = 1, max_wavenumber
         index_r = 2 * m
         index_c = 2 * m + 1
         v_fou(j,m)  = v_fou(j,m) + cmplx(r_fou(index_r),r_fou(index_c))
      end do
   end do

   !----------------------------------------------------------------------
   ! [2] Perform adjoint of inverse Legendre decomposition in N-S direction:
   !----------------------------------------------------------------------

   ccv_local(:) = 0.0

   do m = 0, max_wavenumber
      index_start = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 +1

      index_m     = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

      jc = (jde-jds+1)/2

      iequator = mod(jde-jds+1, 2)

      js = max(jts, jc+iequator+1)
      je = min(jc+iequator, jte)

      temp = (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      do l = m, max_wavenumber
         sum_legtra = da_zero_complex

         if (mod(l+m,2) == 1) then
            do j = js, jte
               index_j = (jds+jde - j - 1) * temp
               sum_legtra = sum_legtra - v_fou(j,m) * alp(index_j + index_m + l)
            end do
         else
            do j = js, jte
               index_j = (jds+jde - j - 1) * temp
               sum_legtra = sum_legtra + v_fou(j,m) * alp(index_j + index_m + l)
            end do
         end if

         do j = jts, je
            index_j = (j - 1) * temp
            sum_legtra = sum_legtra + v_fou(j,m) * alp(index_j + index_m + l) 
         end do
   
         ccv_local(index_start+l-m) = sum_legtra
      end do
   end do

#ifdef DM_PARALLEL
   index_start = 1
   index_end   = max_wavenumber + &
      max_wavenumber * (max_wavenumber + 1) / 2 + 1

   n = index_end - index_start + 1
   call mpi_allreduce(ccv_local(index_start:index_end), &
                      ccv(index_start:index_end), n, true_mpi_complex, mpi_sum, &
                      comm, ierr)
#else
   ccv(:) = ccv_local(:)                      
#endif

   !----------------------------------------------------------------------
   ! [2] Apply Power spectrum
   !-------------------------------------------------------------------------

   if (.not. test_transforms) call da_apply_power(power, max_wavenumber, ccv, sizec)

   do n = 1, sizec
      rcv(2*n - 1) = real (ccv(n))
      rcv(2*n    ) = aimag(ccv(n))
   end do 

   if (trace_use) call da_trace_exit("da_vtovv_spectral_adj")

end subroutine da_vtovv_spectral_adj



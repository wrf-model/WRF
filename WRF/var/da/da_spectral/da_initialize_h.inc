subroutine da_initialize_h(ni, nj, max_wavenumber, lensav, alp_size, wsave, lon, sinlon, coslon, &
   lat, sinlat, coslat, int_wgts, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ni                         ! Number of longitudes.
   integer, intent(in)  :: nj                         ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in)  :: lensav                     ! Size of FFTs wsave array.
   integer, intent(in)  :: alp_size                   ! Size of ALP array.
   real,    intent(out) :: wsave(1:lensav)            ! Primes for FFT.
   real,    intent(out) :: lon(1:ni)                  ! Longitude (radians).
   real,    intent(out) :: sinlon(1:ni)               ! sine(longitude).
   real,    intent(out) :: coslon(1:ni)               ! cosine(longitude).
   real,    intent(out) :: lat(1:nj)                  ! Latitude (radians, from south).
   real,    intent(out) :: sinlat(1:nj)               ! sine(latitude).
   real,    intent(out) :: coslat(1:nj)               ! cosine(latitude).
   real,    intent(out) :: int_wgts(1:nj)             ! Legendre integration weights.
   real,    intent(out) :: alp(1:alp_size)            ! Associated Legendre Polynomial.

   integer :: i                          ! Loop counters.

   if (trace_use) call da_trace_entry("da_initialize_h")

   !----------------------------------------------------------------------------
   ! [1] Initialize FFT coefficients.'
   !----------------------------------------------------------------------------

   wsave(:) = 0.0
#ifdef FFTPACK
   call rfft1i(ni, wsave, lensav, ierr)
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with FFTPACK"/))
#endif

   if (ierr /= 0) then
     write(unit=message(1),fmt='(A,I4)') &
        "Fourier initialization failed. ierr = ", ierr
     call da_error(__FILE__,__LINE__,message(1:1))
   end if

   !----------------------------------------------------------------------------
   ! [2] Calculate latitudes, and their sines/cosines.'
   !---------------------------------------------------------------------------
 
   if (gaussian_lats) then
      call da_get_gausslats(nj, lat, int_wgts, sinlat, coslat)
   else
      call da_get_reglats(nj, lat, sinlat, coslat, int_wgts)
   end if

   do i = 1, ni
      lon(i) = 2.0 * pi / real(ni) * real(i - 1)
      sinlon(i) = sin(lon(i))
      coslon(i) = cos(lon(i))
   end do

   !----------------------------------------------------------------------------
   ! [3] Initialize Legendre coefficients.'
   !----------------------------------------------------------------------------

   call da_setlegpol(nj, max_wavenumber, alp_size, sinlat, coslat, alp)

   if (trace_use) call da_trace_exit("da_initialize_h")

end subroutine da_initialize_h



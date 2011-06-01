subroutine da_calc_power_spectrum(max_wave, power)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate power spectrum
   !-----------------------------------------------------------------------
    
   implicit none
    
   integer, parameter  :: nj = 200          ! #Gaussian lats (even).

   integer, intent(in) :: max_wave          ! Smallest wavenumber for grid.
   real*8, intent(out) :: power(0:max_wave) ! Power spectrum.

   real                :: glats(1:nj)       ! Gaussian latitudes.
   real                :: gwgts(1:nj)       ! Gaussian weights.
   real                :: sinlat(1:nj)      ! sine.
   real                :: coslat(1:nj)      ! cosine.

   integer             :: max_wave_nj       ! Maximum wavenumber.
   integer             :: i,j, l            ! Loop counters.
   real                :: corr_scale        ! Correlation scale.
   real                :: corr_scale_inv    ! 1/corr_scale
   real                :: variance          ! Variance = sum(power).
   real                :: d(1:nj)           ! Temp array.
   real                :: corr(1:nj)        ! Correlation function.
   logical             :: odd               ! true if odd.
   real, allocatable   :: alp(:,:)          ! Associated Legendre Polynomials.
   real, allocatable   :: power_nj(:)       ! Power spectrum.
   character (len=filename_len) :: filename
   
   do i=1,num_alpha_corr_types
      call da_get_unit(alpha_corr_unit1(i))
      call da_get_unit(alpha_corr_unit2(i))
      write (unit=filename,fmt='(A,I1)') "alpha_corr1_",i
      open(unit=alpha_corr_unit1(i),file=filename,status="replace")
      write (unit=filename,fmt='(A,I1)') "alpha_corr2_",i
      open(unit=alpha_corr_unit2(i),file=filename,status="replace")
   end do

   !-----------------------------------------------------------------------------
   ! [1] Switch lats from -pi/2 to pi/2 to 0 to pi:
   !-----------------------------------------------------------------------------

   max_wave_nj = nj / 2 - 1
   allocate(alp(1:nj,0:max_wave_nj))
   allocate(power_nj(0:max_wave_nj))

   call da_get_gausslats(nj, glats, gwgts, sinlat, coslat)
   glats = glats + 0.5 * pi

   ! Get m=0 Associated Legendre Polynomials:

   do l = 0, max_wave_nj
      odd = .false.
      if (real(int(0.5 * real(l))) /= 0.5 * l) odd = .true.

      do j = 1, nj
         call da_asslegpol(l, 0, sinlat(j), coslat(j), alp(j,l))
         ! Reverse order of alps to account for latitude/angle difference:
         if (odd) alp(j,l) = -alp(j,l)
      end do
   end do

   !-----------------------------------------------------------------------------
   ! [2] Define correlation function:
   !-----------------------------------------------------------------------------

   corr_scale = alpha_corr_scale / earth_radius 
   corr_scale_inv = 1.0 / corr_scale

   do j = 1, nj
      ! d(j) = 0.5 * glats(j) * corr_scale_inv
      d(j) = glats(j) * corr_scale_inv

      if (alpha_corr_type == alpha_corr_type_exp) then ! Exponential.
         corr(j) = exp(-d(j))
      else if (alpha_corr_type == alpha_corr_type_soar) then ! SOAR
         d(j) = 2.0 * d(j)
         corr(j) = (1.0 + d(j)) * exp(-d(j))
      else if (alpha_corr_type == alpha_corr_type_gaussian) then ! Gaussian
         corr(j) = exp(-d(j) * d(j))
      end if
   end do

   do j = 1, nj
      write(unit=alpha_corr_unit1(alpha_corr_type),fmt='(i4,2f12.4)') &
        j, earth_radius * glats(j), corr(j)
   end do

   !--------------------------------------------------------------------------
   ! [3] Calculate power spectra:
   !--------------------------------------------------------------------------

   ! Calculate power spectrum (and truncate if has -ve values).
   ! Power spectrum at this stage is is the Dl=sqrt(2l+1)*Bl of Boer(1983).
   ! This ensures the total variance = sum(Dl).

   power_nj(:) = 0.0
   do l = 0, max_wave_nj
      power_nj(l) = 0.5 * sqrt(2.0 * real(l) + 1.0) * &
                    sum(gwgts(1:nj) * corr(1:nj) * alp(1:nj,l))

      if (power_nj(l) < 0.0) power_nj(l) = 0.0
   end do
   write(unit=stdout,fmt='(a,2f12.5)')' Total, unscale variance = ', &
                sum(power_nj(0:max_wave_nj))

   ! Rescale so variance = 1 (take out later?):
   variance = sum(power_nj(0:max_wave_nj))
   power_nj(0:max_wave_nj) = power_nj(0:max_wave_nj) / variance

   do l = 0, max_wave_nj
      write(unit=alpha_corr_unit2(alpha_corr_type),fmt='(i4,2f12.4)') &
        l, power_nj(l), sum(power_nj(0:l))
   end do

   write(unit=stdout,fmt='(a,2i6)')' Total, truncated wave_max = ', &
                     max_wave_nj, max_wave
   write(unit=stdout,fmt='(a,2f12.5)')' Total, truncated variance = ', &
                sum(power_nj(0:max_wave_nj)), sum(power_nj(0:max_wave))

   power(0:max_wave) = power_nj(0:max_wave)

   ! Add compactly supported correlation from calc_globalspectral later?

   deallocate(alp)
   deallocate(power_nj)
   
   do i=1,num_alpha_corr_types
      close (alpha_corr_unit1(i))
      close (alpha_corr_unit2(i))
      call da_free_unit (alpha_corr_unit1(i))
      call da_free_unit (alpha_corr_unit2(i))
   end do

end subroutine da_calc_power_spectrum



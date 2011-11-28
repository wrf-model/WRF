subroutine da_legtra_inv(jds, jde, jts, jte, max_wavenumber, alp_size, m, &
   alp, v, r_leg)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: jds, jde            ! Number of latitudes.
   integer, intent(in)  :: jts, jte            ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber      ! Maximum wavenumber.
   integer, intent(in)  :: alp_size            ! Dimension of ALPs.
   integer, intent(in)  :: m                   ! Zonal wavenumber.
   real,    intent(in)  :: alp(1:alp_size)     ! Associated Legendre Polynomials

   complex, intent(in)  :: v(m:max_wavenumber) ! Output spectral coefficient.
   complex, intent(out) :: r_leg(jts:jte)         ! Field to transform.

   integer              :: l, j, js, je        ! Loop counters.
   integer              :: index_m, index_j
   complex              :: sum_legtra          ! Summation scalars.

   integer              :: jc, iequator

   if (trace_use) call da_trace_entry("da_legtra_inv")

   index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

   jc = (jde-jds+1)/2

   iequator = mod(jde-jds+1, 2)

   je = min(jc+iequator, jte)

   do j = jts, je
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      r_leg(j) = sum(v(m:max_wavenumber) * &
         alp(index_j+index_m+m:index_j+index_m+max_wavenumber))
   end do

   js = max(jts, jc+iequator+1)

   do j = js, jte
      index_j = (jds+jde - j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      sum_legtra = da_zero_complex
      do l = m, max_wavenumber
         ! Calculate second quadrant values:
         if(mod(l+m,2) == 1) then
            sum_legtra = sum_legtra - v(l) * alp(index_j + index_m + l)
         else
            sum_legtra = sum_legtra + v(l) * alp(index_j + index_m + l)
         end if
      end do
      r_leg(j) = sum_legtra
   end do

   if (trace_use) call da_trace_exit("da_legtra_inv")

end subroutine da_legtra_inv



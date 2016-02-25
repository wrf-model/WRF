subroutine da_legtra (nj, max_wavenumber, alp_size, m, int_wgts, alp, r_leg, v)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj                      ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber          ! Maximum wavenumber.
   integer, intent(in)  :: alp_size                ! Dimension of ALPs.
   integer, intent(in)  :: m                       ! Zonal wavenumber.
   real,    intent(in)  :: int_wgts(1:nj)          ! Integration weights.
   real,    intent(in)  :: alp(1:alp_size)         ! Associated Legendre Polynomials.
   complex, intent(in)  :: r_leg(1:nj)             ! Field to transform.
   complex, intent(out) :: v(m:max_wavenumber)     ! Output spectral coefficient.

   integer              :: l, j, j1                ! Loop counters.
   integer              :: index_m, index_j, index ! Markers.
   integer              :: sign_switch             ! make use of symmetry of ALPs.
   real                 :: eq_coeff                ! 1 if equator point, 0 otherwise.
   complex              :: sum_legtra              ! Summation scalar.
   complex              :: eq_term                 ! Summation scalar.

   if (trace_use) call da_trace_entry("da_legtra")

   index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m

   if ((nj+1) / 2 == nj/2 + 1) then
      eq_coeff = 1.0 ! Odd latitudes
   else
      eq_coeff = 0.0 ! Even latitudes
      eq_term  = 0.0
   end if

   do l = m, max_wavenumber

      sign_switch = (-1)**(l + m)
      sum_legtra = da_zero_complex

      do j = 1, nj / 2
         index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2
         index = index_j + index_m + l

         ! Sum first quadrant:
         sum_legtra = sum_legtra + int_wgts(j) * r_leg(j) * alp(index)

         ! Add second quadrant (use symmetry ALP(-mu)=(-1)^{n+|m|}ALP(mu)):
         j1 = nj + 1 - j
         sum_legtra = sum_legtra + sign_switch * int_wgts(j1) * r_leg(j1) * &
            alp(index)
      end do
     
      if (eq_coeff > 0.0) then
         ! Skip this step for Even lats    ! Syed RH Rizvi
         ! Add equator term (wrong if even nj, but then eq_coeff = 0.0 so OK):
         j = nj/2 + 1
         index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber+2) / 2
         index = index_j + index_m + l
         eq_term = int_wgts(j) * r_leg(j) * alp(index)
      end if

      v(l) = 0.5 * (sum_legtra + eq_coeff * eq_term)
   end do

   if (trace_use) call da_trace_exit("da_legtra")

end subroutine da_legtra



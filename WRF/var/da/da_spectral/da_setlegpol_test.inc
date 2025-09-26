subroutine da_setlegpol_test (nj, max_wavenumber, alp_size, int_wgts, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nj              ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber  ! Maximum wavenumber.
   integer, intent(in)  :: alp_size        ! Dimension of ALPs.
   real,    intent(in)  :: int_wgts(1:nj)  ! Integration weights.
   real,    intent(in)  :: alp(1:alp_size) ! Associated Legendre Polynomials.

   real, parameter :: tolerance = 1.0e-6  ! warn if normalization error exceeds

   integer :: m, l1, l2, j, j1    ! Loop counters.
   integer :: index_m, index_j    ! Markers.
   integer :: index1, index2      ! Markers.
   integer :: sign_switch1        ! Defined to make use of symmetry of ALPs.
   integer :: sign_switch2        ! Defined to make use of symmetry of ALPs.
   real    :: eq_coeff            ! 1 if equator point, 0 otherwise.
   real    :: alp_norm_test       ! Summation scalar.
   real    :: eq_term             ! Summation scalar.
   integer :: spec_unit

   if (trace_use) call da_trace_entry("da_setlegpol_test")

   call da_get_unit(spec_unit)
   open(unit=spec_unit,file="spec_pol",status="replace")

   if ((nj+1) / 2 == nj/2 + 1) then
      eq_coeff = 1.0 ! Odd latitudes
   else
      eq_coeff = 0.0 ! Even latitudes
      eq_term  = 0.0
   end if

   ! Test 0.5 * integral_-1^1 alp(j,l1,m) * alp(j,l2,m) = 1 if l1=l2, 
   ! 0 otherwise:

   do m = 0, max_wavenumber
      index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
      do l1 = m, max_wavenumber
         do l2 = m, max_wavenumber

            sign_switch1 = (-1)**(l1 + m)
            sign_switch2 = (-1)**(l2 + m)

            alp_norm_test = 0.0
            do j = 1, nj / 2
               index_j = (j - 1) * (max_wavenumber+1) * (max_wavenumber+2) /2
               index1 = index_j + index_m + l1
               index2 = index_j + index_m + l2

               ! Sum first quadrant:
               alp_norm_test = alp_norm_test + int_wgts(j) * alp(index1) &
                  * alp(index2)

               ! Add second quadrant (use symmetry ALP(-mu)=(-1)^{n+|m|}ALP(mu)):
               j1 = nj + 1 - j
               alp_norm_test = alp_norm_test + int_wgts(j1) * &
                  sign_switch1 * alp(index1) * sign_switch2 * alp(index2)
            end do

            if (eq_coeff > 0.0) then   
               ! Skip this step for even lats       R! Syed RH Rizvi! S
               ! Add equator term (wrong if even nj, but then eq_coeff = 0.0 
               ! so OK):
               j = nj/2 + 1
               index_j = (j - 1) * (max_wavenumber+1) * (max_wavenumber+2) /2
               index1 = index_j + index_m + l1
               index2 = index_j + index_m + l2

               eq_term = int_wgts(j) * alp(index1) * alp(index2)
            end if
            alp_norm_test = 0.5 * (alp_norm_test + eq_coeff * eq_term)

            ! if (l1 /= l2 .and. abs(alp_norm_test) >= tolerance) then
            !    write(unit=stdout,fmt='(a,3i6,f15.10,a,f15.10)')
            !      ' warning: ALP normalization error (m, l1, l2) = ', !&
            !                                      m, l1, l2, alp_norm_test, &
            !                                      ', > tolerance = ', tolerance
            !            end if
            if (l1 == l2 .and. abs(alp_norm_test-1.0) >= tolerance) then
               write(unit=spec_unit,fmt='(a,3i6,f15.10,a,f15.10)') &
                 ' warning: ALP normalization error (m, l1, l2) = ', &
                 m, l1, l2, alp_norm_test - 1.0, &
                 ', > tolerance = ', tolerance

            end if
         end do
      end do
   end do

   close(spec_unit)
   call da_free_unit(spec_unit)

   if (trace_use) call da_trace_exit("da_setlegpol_test")

end subroutine da_setlegpol_test



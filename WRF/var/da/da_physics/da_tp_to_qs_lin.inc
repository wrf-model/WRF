subroutine da_tp_to_qs_lin(grid, qs_prime_over_qs )

   !---------------------------------------------------------------------------
   !  Purpose: Convert es/p/es_prime to saturation specific humidity increment.
   !
   !  Method: qs~ = qs * ( p es'/es - p' ) / ( p - (1-rd_over_rv) es ).
   !          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).

   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid
   real,          intent(out)   :: qs_prime_over_qs(its:ite,jts:jte,kts:kte)

   integer :: i, j, k      ! Loop counters.
   real    :: temp         ! Temporary array.
   real    :: es_prime_over_es ! Sat Vap pressure ratio.

   if (trace_use_dull) call da_trace_entry("da_tp_to_qs_lin")

   do k = kts, kte
      do j = jts, jte
         do i = its, ite
            temp = grid%xb % t(i,j,k) + es_gammakelvin
            !-----------------------------------------------------------------
            ! [2.0] Calculate saturation vapour pressure increment:
            !-----------------------------------------------------------------

            es_prime_over_es = es_gammabeta * grid%xa % t(i,j,k) / ( temp * temp )

            !-----------------------------------------------------------------
            ! [3.0] Calculate saturation specific humidity increment:
            !-----------------------------------------------------------------

            qs_prime_over_qs(i,j,k) = ( grid%xb % p(i,j,k) * es_prime_over_es - &
                                        grid%xa % p(i,j,k) ) / &
                                      ( grid%xb % p(i,j,k) - rd_over_rv1 * &
                                        grid%xb % es(i,j,k) )
         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_tp_to_qs_lin")

end subroutine da_tp_to_qs_lin



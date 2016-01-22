subroutine da_tp_to_qs1 (grid, es, qs)

   !---------------------------------------------------------------------------
   !  Purpose: Convert T/p to saturation specific humidity.
   !
   !  Method: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
   !          use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
   !
   !  This da_tp_to_qs1 was added and called by the corrected subroutine
   !       da_tpq_to_rh_lin.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)  :: grid
   real,          intent(out) :: es(its:ite,jts:jte,kts:kte)
   real,          intent(out) :: qs(its:ite,jts:jte,kts:kte)

   integer :: i, j, k      ! Loop counters.
   real    :: t_c          ! Working variable.

   if (trace_use_dull) call da_trace_entry("da_tp_to_qs1")

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------
    

   do k = kts, kte
      do j = jts, jte
         do i = its, ite

            !------------------------------------------------------------------
            ! [1.0] initialise:
            !------------------------------------------------------------------

            t_c = grid%xb % t(i,j,k) - t_kelvin
   
            !------------------------------------------------------------------
            ! [2.0] Calculate saturation vapour pressure:
            !------------------------------------------------------------------

            es(i,j,k) = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
   
            !------------------------------------------------------------------
            ! [3.0] Calculate saturation specific humidity:
            !------------------------------------------------------------------

            qs(i,j,k) = rd_over_rv * es(i,j,k) / &
                     (grid%xb % p(i,j,k) - rd_over_rv1 * es(i,j,k))

         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_tp_to_qs1")

end subroutine da_tp_to_qs1



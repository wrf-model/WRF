subroutine da_check_max_iv_radar(iv, it, irv, irf, irvf, irff)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it    
   integer,       intent(inout) :: irv, irf, irvf, irff    

   integer :: k,n
   logical :: failed 

   if (trace_use) call da_trace_entry("da_check_max_iv_radar")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(a)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for radar_rv   = ',max_error_rv, &
         'Error max test ratio for radar_rf   = ',max_error_rf
   end if

   !------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !------------------------------------------------------------------------

   failed = .false.

   do n = iv%info(radar)%n1,iv%info(radar)%n2
      do k = 1, iv%info(radar)%levels(n)
         if (iv%radar(n)%height_qc(k) /= far_below_model_surface .and. &
              iv%radar(n)%height_qc(k) /= above_model_lid) then
            ! rv
            if (use_radar_rv) then
               call da_max_error_qc_radar(it, iv%info(radar), n, iv%radar(n)%rv(k), max_error_rv, irv, irvf, &
                  check_max_iv_unit, 'rv   ', failed, check_max_iv_print)
            end if

            ! rf
            if (use_radar_rf) then
               call da_max_error_qc_radar(it, iv%info(radar), n, iv%radar(n)%rf(k), max_error_rf, irf, irff, &
                  check_max_iv_unit, 'rf   ', failed, check_max_iv_print)
            end if
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_radar")

end subroutine da_check_max_iv_radar

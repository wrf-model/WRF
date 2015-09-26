subroutine da_check_xtoy_adjoint_radar(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint_radar")

   do n=iv%info(radar)%n1, iv%info(radar)%n2
      if (iv%info(radar)%proc_domain(1,n)) then
         do k=1, iv%info(radar)%levels(n)
            adjtest_lhs = adjtest_lhs + &
               (y%radar(n)%rv(k)/typical_rv_rms)**2   + (y%radar(n)%rf(k)/typical_rf_rms)**2   + &
               (y%radar(n)%rrn(k)/typical_qrn_rms)**2 + (y%radar(n)%rsn(k)/typical_qsn_rms)**2 + &
               (y%radar(n)%rgr(k)/typical_qgr_rms)**2 + (y%radar(n)%rqv(k)/typical_q_rms)**2 
         end do
      end if

      do k=1, iv%info(radar)%levels(n)
         pertile_lhs = pertile_lhs + &
            (y%radar(n)%rv(k)/typical_rv_rms)**2   + (y%radar(n)%rf(k)/typical_rf_rms)**2 + &
            (y%radar(n)%rrn(k)/typical_qrn_rms)**2 + (y%radar(n)%rsn(k)/typical_qsn_rms)**2 + &
            (y%radar(n)%rgr(k)/typical_qgr_rms)**2 + (y%radar(n)%rqv(k)/typical_q_rms)**2

         y%radar(n)%rv(k)= y%radar(n)%rv(k) / typical_rv_rms ** 2
         y%radar(n)%rf(k)= y%radar(n)%rf(k) / typical_rf_rms ** 2
         y%radar(n)%rrn(k)= y%radar(n)%rrn(k) / typical_qrn_rms ** 2
         y%radar(n)%rsn(k)= y%radar(n)%rsn(k) / typical_qsn_rms ** 2
         y%radar(n)%rgr(k)= y%radar(n)%rgr(k) / typical_qgr_rms ** 2
         y%radar(n)%rqv(k)= y%radar(n)%rqv(k) / typical_q_rms ** 2
      end do
   end do

end subroutine da_check_xtoy_adjoint_radar



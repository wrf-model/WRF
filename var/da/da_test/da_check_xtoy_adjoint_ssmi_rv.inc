subroutine da_check_xtoy_adjoint_ssmi_rv(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! loop counter.
   real    :: var

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint_ssmi_rv")

   if (iv%info(ssmi_rv)%nlocal > 0) then
      do n=iv%info(ssmi_rv)%n1, iv%info(ssmi_rv)%n2
         y%ssmi_rv(n)%speed = y%ssmi_rv(n)%speed/typical_speed_rms
         y%ssmi_rv(n)%tpw   = y%ssmi_rv(n)%tpw/typical_tpw_rms

         var = (y%ssmi_rv(n)%speed) ** 2 + (y%ssmi_rv(n)%tpw) ** 2

         pertile_lhs = pertile_lhs + var

         if (iv%info(ssmi_rv)%proc_domain(1,n)) then
            adjtest_lhs = adjtest_lhs + var
         end if

         y%ssmi_rv(n)%speed = y%ssmi_rv(n)%speed/typical_speed_rms
         y%ssmi_rv(n)%tpw   = y%ssmi_rv(n)%tpw/typical_tpw_rms
      end do
   end if

   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint_ssmi_rv")

end subroutine da_check_xtoy_adjoint_ssmi_rv



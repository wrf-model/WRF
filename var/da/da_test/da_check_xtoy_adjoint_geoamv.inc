subroutine da_check_xtoy_adjoint_geoamv (iv, y, adjtest_lhs, pertile_lhs)

   !-------------------------------------------------------------------------
   ! Purpose:  Adjoint Test for Geo. AMVs Obs
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type(y_type),  intent(inout)  :: y             ! y = h (xa)
   real,          intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_geoamv")

   do n=iv%info(geoamv)%n1, iv%info(geoamv)%n2
      if (iv%info(geoamv)%proc_domain(1,n)) then
         do k=1, iv%info(geoamv)%levels(n)
            adjtest_lhs = adjtest_lhs + &
                        (y%geoamv(n)%u(k)/typical_u_rms)**2 + &
                        (y%geoamv(n)%v(k)/typical_v_rms)**2
         end do
      end if

      do k=1, iv%info(geoamv)%levels(n)
         pertile_lhs = pertile_lhs + &
                     (y%geoamv(n)%u(k)/typical_u_rms)**2 + &
                     (y%geoamv(n)%v(k)/typical_v_rms)**2

         y%geoamv(n)%u(k)= y%geoamv(n)%u(k) / typical_u_rms ** 2
         y%geoamv(n)%v(k)= y%geoamv(n)%v(k) / typical_v_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_geoamv")

end subroutine da_check_xtoy_adjoint_geoamv


